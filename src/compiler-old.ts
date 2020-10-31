/*

Problems:
	Basic types can be represented in two different ways atm
	IntType and TypeName("Int") for example
	
	IrValue is used to represent lvalues, a bit unclear.
	
	Functions should probably always be blocks, not enforced atm. could error atm.
	
	Decide on a convention for addr / address
	
	Maybe fix the stack frame stuff
	
	Refactor expression parsing
	
	Lots of duplicate code in IrGenerator for things like arithmetic
	and moving values around
	
	Can't increment pointers

Ideas:
	Globals, including strings will go at the top of memory.
	
	But it will probably require a hidden function to be called before
	main that initializes all the globals.
	
	Optimize call stack of consecutive function calls

*/

//const { TokenType, Token, Lexer } = require("./lexer.js");
import { TokenType, Token, Lexer } from "./lexer";
import {
	Type,
	FunctionType,
	NamedArgument,
	PointerType,
	ArrayType,
	Types, TypeAliases
} from "./types";
import {
	AstNode,
	Program,
	
	ExternDeclaration,
	FunctionDefinition,
	//StructDefinition,
	
	Statement,
	BlockStatement,
	DeclarationStatement,
	IfStatement,
	ForStatement,
	WhileStatement,
	DoWhileStatement,
	BreakStatement,
	ContinueStatement,
	ReturnStatement,
	LabelStatement,
	GotoStatement,
	HaltStatement,
	ExpressionStatement,
	EmptyStatement,
	
	Expression,
	UnaryExpression,
	BinaryExpression,
	CallExpression,
	SubscriptExpression,
	
	Identifier,
	NumberLiteral,
	CharLiteral,
	StringLiteral,
	
	Parser
} from "./parser";

export function compile(code: string): number[] {
	let lexer = new Lexer(code);
	let tokens = [];
	while (true) {
		let token = lexer.nextToken();
		if (token.type === TokenType.EOF) break;
		tokens.push(token);
	}
	
	let parser = new Parser(tokens);
	let program = parser.parseProgram();
	
	let highIrGenerator = new HighIrGenerator();
	let highIr = highIrGenerator.generate(program);
	
	let irGenerator = new IrGenerator();
	let {ir, data} = irGenerator.generate(program);
	
	console.log("========== IR ==========");
	console.log(ir.join("\n"));
	console.log(data);
	
	let asmGenerator = new AsmGenerator(ir, data);
	let asm = asmGenerator.generate();
	
	//console.log(asm);
	
	return asm;
}

// TODO: names record should not contain any
class Scope {
	parent: Scope | null;
	names: Record<string, IrValue | IrLabel>;
	labels: Record<string, IrLabel>;
	stackOffset: number;
	
	constructor() {
		this.parent = null;
		this.names = {};
		this.labels = {};
		this.stackOffset = 0;
	}
	
	resolve(name: string): any | null {
		if (name in this.names) return this.names[name];
		if (this.parent !== null) return this.parent.resolve(name);
		return null;
	}
	add(name: string, value: IrValue | IrLabel) {
		if (name in this.names) throw "This name already exists in the current scope";
		this.names[name] = value;
	}
	
	resolveLabel(name: string): IrLabel | null {
		if (name in this.labels) return this.labels[name];
		if (this.parent !== null) return this.parent.resolveLabel(name);
		return null;
	}
	addLabel(name: string, label: IrLabel) {
		if (name in this.labels) throw "Label already exists in the current scope";
		
		if (this.parent) {
			this.parent.addLabel(name, label);
		} else {
			this.labels[name] = label;
		}
	}
	
	child(): Scope {
		let child = new Scope();
		child.parent = this;
		child.stackOffset = this.stackOffset;
		return child;
	}
	pop(): Scope {
		if (this.parent === null) throw "Tried to pop root scope";
		return this.parent;
	}
}

/*

Proposed ir tree stage:

x := arr[x][y];
x := *(&*(&arr + x * sizeof(*arr)) + y * sizeof(**arr));
x := *((&arr + x * 2) + y);
x := *(sp + arr_pos + 5);
x := *(sp + z);

x := ptr[x][y];
x := *(*(ptr + x) + y);

x := ++y;
x := y = y + 1;

x := y++;
x := tmp(y, y + 1);

*/

class ScopedValue {
	name: string;
	type: Type;
	
	constructor(name: string, type: Type) {
		this.name = name;
		this.type = type;
	}
}

class HighIrDelayedAssign {
	
}

class HighIrGenerator {
	scope = new Scope();
	
	initFunction(fun: FunctionDefinition): FunctionDefinition {
		this.scope = this.scope.child();
		
		fun.type.args.forEach(arg => {
			if (!(arg instanceof NamedArgument)) throw "Expected named arguments in function definition";
			
			let name = arg.name;
			let type = arg.type;
			
			//this.scope.add(arg.name, new ScopedValue(name, type));
		});
		
		fun.body = this.initialPass(fun.body) as BlockStatement;
		
		this.scope = this.scope.pop();
		
		return fun;
	}
	
	initialPass(node: AstNode) {
		if (node instanceof FunctionDefinition) {
			node = this.initFunction(node);
		} else if (node instanceof BlockStatement) {
			this.scope = this.scope.child();
			for (let i=0; i<node.body.length; i++) {
				node.body[i] = this.initialPass(node.body[i]);
			}
			this.scope = this.scope.pop();
		} else if (node instanceof DeclarationStatement) {
			
		}
		
		return node;
	}
	
	generate(program: Program) {
		
	}
}

/*==============*/
//      IR
/*==============*/
class IrValue {
	type: Type;
	address: IrAddr | IrLabel;
	
	constructor(type: Type, address: IrAddr | IrLabel) {
		this.type = type;
		this.address = address;
	}
	
	equals(type: Type): boolean {
		return this.type.equals(type);
	}
	
	memSize(): number {
		return this.type.memSize();
	}
	
	toString(): string {
		return `& ${this.type}`;
	}
}
// used for postfix increment
class IrTmpValue extends IrValue {
	tmp: IrAddr;
	
	constructor(type: Type, address: IrAddr, tmp: IrAddr) {
		super(type, address);
		this.tmp = tmp;
	}
	
	toString(): string {
		return `'& ${this.type}`;
	}
}

class IrAddr {
	base: number;
	offset: number;
	
	constructor(base: number, offset: number) {
		this.base = base;
		this.offset = offset;
	}
	
	static absolute(addr: number) {
		return new IrAddr(-1, addr);
	}
	static stack(addr: number) {
		return new IrAddr(0, addr);
	}
	static data(addr: number) {
		return new IrAddr(-1, -addr - 1);
	}
	
	static stackPointer() { return IrAddr.absolute(0); }
	static reg1() { return IrAddr.absolute(1); }
	static reg2() { return IrAddr.absolute(2); }
	static zero() { return IrAddr.data(0); }
	static one() { return IrAddr.data(1); }
	static reg1Ref() { return new IrAddr(1, 0); }
	static reg2Ref() { return new IrAddr(2, 0); }
	
	eq(base: number, offset: number): boolean {
		return this.base === base && this.offset === offset;
	}
	isReg1() {
		return this.eq(-1, 1);
	}
	isReg1Ref() { return this.eq(1, 0); }
	isReg2Ref() { return this.eq(2, 0); }
	
	toString(): string {
		return `[${this.base}:${this.offset}]`;
	}
}

abstract class IrInstr {
	abstract toString(): string;
}
let labelInc = 0;
class IrLabel extends IrInstr {
	id: number;
	
	constructor() {
		super();
		this.id = labelInc++;
	}
	
	toString(): string {
		return `:lab_${this.id}:`;
	}
}
class IrNamedLabel extends IrInstr {
	name: string;
	
	constructor(name: string) {
		super();
		this.name = name;
	}
	
	toString(): string {
		return `:${this.name}:`;
	}
}
class IrMove extends IrInstr {
	value: IrAddr | IrLabel | number;
	addr: IrAddr;
	
	constructor(value: IrAddr | IrLabel | number, addr: IrAddr) {
		super();
		this.value = value;
		this.addr = addr;
	}
	
	toString(): string {
		return `\tMOV\t${this.value} ${this.addr}`;
	}
}
abstract class IrBinop extends IrInstr {
	left: IrAddr;
	right: IrAddr;
	dest: IrAddr;
	
	abstract mnemonic: string;
	
	constructor(left: IrAddr, right: IrAddr, dest: IrAddr) {
		super();
		this.left = left;
		this.right = right;
		this.dest = dest;
	}
	
	toString(): string {
		return `\t${this.mnemonic}\t${this.left} ${this.right} ${this.dest}`;
	}
}
class IrAdd extends IrBinop { mnemonic = "ADD"; }
class IrSub extends IrBinop { mnemonic = "SUB"; }
class IrMul extends IrBinop { mnemonic = "MUL"; }
class IrDiv extends IrBinop { mnemonic = "DIV"; }
class IrMod extends IrBinop { mnemonic = "MOD"; }
class IrLt extends IrBinop { mnemonic = "LT"; }
class IrLeq extends IrBinop { mnemonic = "LEQ"; }
class IrEq extends IrBinop { mnemonic = "EQ"; }
class IrLea extends IrInstr {
	addr: IrAddr;
	dest: IrAddr;
	
	constructor(addr: IrAddr, dest: IrAddr) {
		super();
		this.addr = addr;
		this.dest = dest;
	}
	
	toString(): string {
		return `\tLEA\t${this.addr} ${this.dest}`;
	}
}
class IrJump extends IrInstr {
	address: IrAddr | IrLabel | IrNamedLabel;
	
	constructor(address: IrAddr | IrLabel | IrNamedLabel) {
		super();
		this.address = address;
	}
	
	toString(): string {
		return `\tJMP\t${this.address}`;
	}
}
class IrJumpZero extends IrInstr {
	address: IrAddr | IrLabel;
	test: IrAddr;
	
	constructor(address: IrAddr | IrLabel, test: IrAddr) {
		super();
		this.address = address;
		this.test = test;
	}
	
	toString(): string {
		return `\tJZ\t${this.test} ${this.address}`;
	}
}
class IrCall extends IrInstr {
	stackSize: number;
	retLabel: IrLabel;
	
	constructor(stackSize: number, retLabel: IrLabel) {
		super();
		this.stackSize = stackSize;
		this.retLabel = retLabel;
	}
	
	toString(): string {
		return `\tCALL\t${this.stackSize} ${this.retLabel}`;
	}
}
class IrExtern extends IrInstr {
	name: string;
	
	constructor(name: string) {
		super();
		this.name = name;
	}
	
	toString(): string {
		return `\tEXTERN\t${this.name}`;
	}
}
class IrHalt extends IrInstr {
	toString(): string {
		return `\tHALT`;
	}
}

class IrGenerator {
	scope: Scope = new Scope();
	code: IrInstr[] = [];
	data: number[] = [0, 1];
	
	addIr(ir: IrInstr) {
		this.code.push(ir);
	}
	lastIr() {
		return this.code[this.code.length - 1];
	}
	addString(str: string): number {
		let values = str.split("").map(a => a.charCodeAt(0));
		values.push(0);
		values.reverse();
		
		for (let i=0; i<values.length; i++) {
			this.data.push(values[i]);
		}
		
		//return IrAddr.data(this.data.length - 1);
		return -this.data.length;
	}
	getConstant(value: number): IrAddr {
		if (!this.data.includes(value)) {
			this.data.push(value);
		}
		return new IrAddr(-1, -this.data.indexOf(value) - 1);
	}
	
	generate(program: Program) {
		let funs = program.body.filter((a): a is FunctionDefinition => a instanceof FunctionDefinition);
		
		funs.forEach(fun => {
			let value = new IrValue(fun.type, new IrLabel);
			this.scope.add(fun.name, value);
		});
		
		// Entry procedure
		this.addIr(new IrMove(3 + 1, IrAddr.stackPointer()));
		let retLabel = new IrLabel;
		this.addIr(new IrMove(retLabel, IrAddr.stack(-1)));
		this.addIr(new IrJump(this.scope.resolve("main").address));
		this.addIr(retLabel);
		this.addIr(new IrHalt());
		
		let externs = program.body.filter((a): a is ExternDeclaration => a instanceof ExternDeclaration);
		externs.forEach(extern => {
			let name = extern.name;
			let thunk = new IrLabel;
			let value = new IrValue(extern.type, thunk);
			this.scope.add(name, value);
			
			let stackOffset = extern.type.ret.memSize();
			extern.type.args.forEach(arg => {
				if (arg instanceof NamedArgument) arg = arg.type;
				stackOffset += arg.memSize();
			});
			stackOffset += 1;
			
			this.addIr(thunk);
			this.addIr(new IrExtern(name));
			this.addIr(new IrJump(IrAddr.stack(-stackOffset)));
		});
		
		funs.forEach(fun => {
			this.generateFunction(fun);
		});
		
		return {ir: this.code, data: this.data};
	}
	
	generateFunction(fun: FunctionDefinition) {
		// Label
		this.addIr(this.scope.resolve(fun.name).address);
		
		this.scope = this.scope.child();
		
		let stackOffset = 0;
		
		// Add arguments to scope
		fun.type.args.forEach(arg => {
			if (!(arg instanceof NamedArgument)) throw "Expected named arguments in function definition";
			
			stackOffset += arg.type.memSize();
			this.scope.add(arg.name, new IrValue(arg.type, IrAddr.stack(-stackOffset)));
		});
		
		// Return address
		stackOffset += 1;
		this.scope.add("__return__", new IrValue(Types.Void, IrAddr.stack(-stackOffset)));
		
		// Return value
		if (!fun.type.ret.equals(Types.Void)) {
			stackOffset += fun.type.ret.memSize();
			this.scope.add("__value__", new IrValue(fun.type.ret, IrAddr.stack(-stackOffset)));
		}
		
		//console.log(this.scope.names);
		
		//console.log(fun.body.body);
		
		if (
			fun.type.ret.equals(Types.Void) &&
			!(fun.body.body[fun.body.body.length - 1] instanceof ReturnStatement)
		) {
			fun.body.body.push(new ReturnStatement(null));
		}
		
		// Error if no final return statement
		
		fun.body.body.forEach(stmt => {
			this.generateStatement(stmt);
		});
		
		this.scope = this.scope.pop();
	}
	
	generateStatement(stmt: Statement) {
		if (stmt instanceof BlockStatement) {
			this.scope = this.scope.child();
			stmt.body.forEach(stmt => {
				this.generateStatement(stmt);
			});
			this.scope = this.scope.pop();
		} else if (stmt instanceof DeclarationStatement) {
			let name = stmt.name;
			if (name in this.scope.names) {
				throw "\"" + name + "\" already declared in this scope";
			}
			
			let addr = IrAddr.stack(this.scope.stackOffset);
			
			let type = stmt.type;
			if (stmt.init) {
				let inferredType = this.generateRvalExpr(stmt.init);
				if (!type) type = inferredType;
				
				//if (!type.equals(inferredType)) throw "Declaration type mismatch";
			}
			if (type === null) throw "Declaration type is fucked";
			
			this.scope.stackOffset = addr.offset + type.memSize();
			
			let value = new IrValue(type, addr);
			
			this.scope.add(name, value);
		} else if (stmt instanceof ExpressionStatement) {
			let stack = this.scope.stackOffset;
			this.generateExpression(stmt.expression);
			this.scope.stackOffset = stack;
		} else if (stmt instanceof IfStatement) {
			let altLabel = new IrLabel;
			let endLabel = new IrLabel; // Unnecessary init because of typescript
			
			let addr = IrAddr.stack(this.scope.stackOffset);
			this.generateRvalExpr(stmt.test);
			this.addIr(new IrJumpZero(altLabel, addr));
			
			this.generateStatement(stmt.consequent);
			if (stmt.alternate) {
				this.addIr(new IrJump(endLabel));
			}
				
			this.addIr(altLabel);
			
			if (stmt.alternate)	{
				this.generateStatement(stmt.alternate);
				this.addIr(endLabel);
			}
		} else if (stmt instanceof ForStatement) {
			this.scope = this.scope.child();
			if (stmt.init) {
				this.generateStatement(stmt.init);
			}
			
			let loopLabel = new IrLabel;
			let contLabel = new IrLabel;
			let endLabel = new IrLabel;
			this.scope.add("__continue__", contLabel);
			this.scope.add("__break__", endLabel);
			
			this.addIr(loopLabel);
			if (!stmt.inc) {
				this.addIr(contLabel);
			}
			
			let addr = IrAddr.stack(this.scope.stackOffset);
			if (stmt.test) {
				this.generateRvalExpr(stmt.test);
				this.addIr(new IrJumpZero(endLabel, addr));
			}
			
			this.generateStatement(stmt.body);
			
			if (stmt.inc) {
				this.addIr(contLabel);
				this.generateExpression(stmt.inc);
			}
			this.addIr(new IrJump(loopLabel));
			
			this.addIr(endLabel)
			
			this.scope = this.scope.pop();
		} else if (stmt instanceof WhileStatement) {
			this.scope = this.scope.child();
			
			let loopLabel = new IrLabel;
			let endLabel = new IrLabel;
			this.scope.add("__continue__", loopLabel);
			this.scope.add("__break__", endLabel);
			this.addIr(loopLabel);
			
			let addr = IrAddr.stack(this.scope.stackOffset);
			this.generateRvalExpr(stmt.test);
			this.addIr(new IrJumpZero(endLabel, addr));
			
			this.generateStatement(stmt.body);
			this.addIr(new IrJump(loopLabel));
			
			this.addIr(endLabel);
			
			this.scope = this.scope.pop();
		} else if (stmt instanceof DoWhileStatement) {
			this.scope = this.scope.child();
			
			let loopLabel = new IrLabel;
			let endLabel = new IrLabel;
			this.scope.add("__continue__", loopLabel);
			this.scope.add("__break__", endLabel);
			
			this.addIr(loopLabel);
			this.generateStatement(stmt.body);
			
			let addr = IrAddr.stack(this.scope.stackOffset);
			this.generateRvalExpr(stmt.test);
			this.addIr(new IrJumpZero(endLabel, addr));
			this.addIr(new IrJump(loopLabel));
			
			this.addIr(endLabel);
			
			this.scope = this.scope.pop();
		} else if (stmt instanceof BreakStatement) {
			let breakLabel = this.scope.resolve("__break__");
			if (!breakLabel) throw "No loop to break out of";
			
			this.addIr(new IrJump(breakLabel));
		} else if (stmt instanceof ContinueStatement) {
			let contLabel = this.scope.resolve("__continue__");
			if (!contLabel) throw "No loop to continue";
			
			this.addIr(new IrJump(contLabel));
		} else if (stmt instanceof ReturnStatement) {
			let returnType = this.scope.resolve("__value__");
			
			if (returnType) {
				if (!stmt.value) throw "Return in function with return type has to return a value";
				let stackOffset = this.scope.stackOffset;
				let returnValue = this.generateRvalExpr(stmt.value);
				this.scope.stackOffset = stackOffset;
				if (!returnType.equals(returnValue)) throw "Returned value doesn't match return type";
				
				this.addIr(new IrMove(IrAddr.stack(stackOffset), returnType.address));
			} else {
				if (stmt.value) throw "Return in Void function can't return a value";
			}
			
			this.addIr(new IrJump(this.scope.resolve("__return__").address));
		} else if (stmt instanceof LabelStatement) {
			let name = stmt.name;
			if (name in this.scope.names) {
				throw "\"" + name + "\" already declared in this scope";
			}
			
			let label = new IrLabel;
			this.addIr(label);
			this.scope.addLabel(name, label);
		} else if (stmt instanceof GotoStatement) {
			this.addIr(new IrJump(new IrNamedLabel(stmt.label)));
		} else if (stmt instanceof HaltStatement) {
			this.addIr(new IrHalt());
		} else if (stmt instanceof EmptyStatement) {
			
		} else {
			throw "Unknown statement " + stmt.constructor.name;
		}
	}
	
	addressOf(operand: IrValue) {
		if (!(operand.address instanceof IrAddr)) throw "error";
		// OPTIMIZATION
		if (operand.address.isReg1Ref()) {
			this.addIr(new IrMove(IrAddr.reg1(), IrAddr.stack(this.scope.stackOffset)));
		} else if (operand.address.isReg2Ref()) {
			this.addIr(new IrMove(IrAddr.reg2(), IrAddr.stack(this.scope.stackOffset)));
		} else {
			if (operand.address.base !== 0) throw "Unexpected base " + operand.address;
			let addr = IrAddr.stack(this.scope.stackOffset);
			//this.addIr(new IrMove(operand.address.offset, addr))
			//this.addIr(new IrAdd(this.getConstant(operand.address.offset), IrAddr.absolute(operand.address.base), addr));
			this.addIr(new IrLea(operand.address, addr));
		}
		this.scope.stackOffset += 1;
		
		return new PointerType(operand.type);
	}
	
	generateExpression(expr: Expression, secondaryReg: boolean = false): Type | IrValue {
		if (expr instanceof NumberLiteral) {
			this.addIr(new IrMove(expr.value, IrAddr.stack(this.scope.stackOffset)));
			this.scope.stackOffset += Types.Int.memSize();
			return Types.Int;
		} else if (expr instanceof CharLiteral) {
			this.addIr(new IrMove(expr.value.charCodeAt(0), IrAddr.stack(this.scope.stackOffset)));
			this.scope.stackOffset += Types.Char.memSize();
			return Types.Char;
		} else if (expr instanceof StringLiteral) {
			let type = TypeAliases.String;
			
			let addr = this.addString(expr.value);
			this.addIr(new IrMove(addr, IrAddr.stack(this.scope.stackOffset)));
			this.scope.stackOffset += type.memSize();
			
			return type;
			//return new IrValue(TypeAliases.String, addr);
		} else if (expr instanceof Identifier) {
			let name = expr.name;
			let value = this.scope.resolve(name);
			if (!value) throw "Undeclared variable \"" + name + "\"";
			
			return value;
		} else if (expr instanceof UnaryExpression) {
			if (expr.prefix) {
				switch (expr.operator.type) {
					case TokenType.Minus:
						if (
							expr.operand instanceof NumberLiteral
						) {
							this.addIr(new IrMove(-expr.operand.value, IrAddr.stack(this.scope.stackOffset)));
							this.scope.stackOffset += Types.Int.memSize();
							return Types.Int;
						} else {
							throw "Negate unsuported";
						}
					case TokenType.Star: {
						let operand: IrAddr = IrAddr.stack(this.scope.stackOffset);
						let operandType = this.generateRvalExpr(expr.operand);
						if (
							this.lastIr() instanceof IrMove &&
							(this.lastIr() as IrMove).value instanceof IrAddr
						) {
							operand = (this.code.pop() as IrMove).value as IrAddr;
						}
						
						let outReg = secondaryReg ? IrAddr.reg1() : IrAddr.reg2();
						let outRef = secondaryReg ? IrAddr.reg1Ref() : IrAddr.reg2Ref();
						
						if (
							this.lastIr() instanceof IrMove &&
							(this.lastIr() as IrMove).addr.eq(operand.base, operand.offset)
						) {
							operand = (this.code.pop() as IrMove).value as IrAddr;
						}
						
						if (!(operandType instanceof PointerType)) throw "Indirection of non-pointer type " + operandType;
						
						this.addIr(new IrMove(operand, outReg));
						
						return new IrValue(operandType.type, outRef);
					}
					case TokenType.Amp: {
						return this.addressOf(this.generateExpression(expr.operand) as IrValue);
					}
					case TokenType.PlusPlus: {
						// TODO: Replace with x += 1
						let operand = this.generateExpression(expr.operand);
						if (!(operand instanceof IrValue)) throw "Tried to increment non-lvalue";
						if (!operand.equals(Types.Int)) throw "Tried to increment " + operand;
						this.addIr(new IrAdd(operand.address as IrAddr, IrAddr.one(), operand.address as IrAddr));
						this.addIr(new IrMove(operand.address, IrAddr.stack(this.scope.stackOffset++)));
						return Types.Int;
					}
					default:
						throw "Unknown unary prefix operator " + expr.operator;
				}
			} else {
				switch (expr.operator.type) {
					case TokenType.PlusPlus: {
						let operand = this.generateExpression(expr.operand);
						if (!(operand instanceof IrValue)) throw "Tried to increment non-lvalue";
						if (!operand.equals(Types.Int)) throw "Tried to increment " + operand;
						this.addIr(new IrMove(operand.address, IrAddr.reg1()));
						this.addIr(new IrAdd(operand.address as IrAddr, IrAddr.one(), operand.address as IrAddr))
						return new IrTmpValue(Types.Int, operand.address as IrAddr, IrAddr.reg1());
					}
					case TokenType.MinusMinus: {
						let operand = this.generateExpression(expr.operand);
						if (!(operand instanceof IrValue)) throw "Tried to decrement non-lvalue";
						if (!operand.equals(Types.Int)) throw "Tried to decrement " + operand;
						this.addIr(new IrMove(operand.address, IrAddr.reg1()));
						this.addIr(new IrSub(operand.address as IrAddr, IrAddr.one(), operand.address as IrAddr))
						return new IrTmpValue(Types.Int, operand.address as IrAddr, IrAddr.reg1());
					}
					default:
						throw "Unknown unary postfix operator " + expr.operator;
				}
			}
		} else if (expr instanceof BinaryExpression) {
			switch (expr.operator.type) {
				case TokenType.Plus:
				case TokenType.Minus:
				case TokenType.Star:
				case TokenType.Slash:
				case TokenType.Percent:
					return this.generateArithmetic(expr);
				case TokenType.Lt:
				case TokenType.Gt:
				case TokenType.LtEq:
				case TokenType.GtEq:
				case TokenType.EqEq:
				case TokenType.BangEq:
					return this.generateComparison(expr);
				case TokenType.Eq:
					return this.generateAssignment(expr);
				case TokenType.PlusEq:
				case TokenType.MinusEq:
				case TokenType.StarEq:
				case TokenType.SlashEq:
				case TokenType.PercentEq:
					return this.generateCompundAssignment(expr);
				default:
					throw "Unknown binary operator " + expr.operator;
			}
		} else if (expr instanceof CallExpression) {
			let prelType = this.getExpressionType(expr.callee);
			if (!(prelType instanceof FunctionType)) throw "Tried to call " + prelType;
			
			// Return value
			this.scope.stackOffset += prelType.ret.memSize();
			let stackRestore = this.scope.stackOffset;
			
			// Return address
			let retLabel = new IrLabel;
			this.addIr(new IrMove(retLabel, IrAddr.stack(this.scope.stackOffset)));
			this.scope.stackOffset++;
			
			// Arguments
			let args: {addr: IrAddr, type: Type}[] = [];
			expr.args.slice().reverse().forEach(arg => {
				args.push({
					addr: IrAddr.stack(this.scope.stackOffset),
					type: this.generateRvalExpr(arg)
				});
			});
			args.reverse();
			
			let stackSize = this.scope.stackOffset;
			
			let callee = this.generateRvalExpr(expr.callee);
			if (!(callee instanceof FunctionType)) throw "Tried to call " + callee;
			
			let typeMatch = callee.args.length === args.length;
			if (typeMatch) {
				for (let i=0; i<args.length; i++) {
					if (!args[i].type.equals(callee.args[i])) {
						typeMatch = false;
						break;
					}
				}
			}
			if (!typeMatch) {
				let givenType = `(${args.map(a => a.type).join(", ")})`;
				throw `Called function type ${callee} does not match given arguments ${givenType}`;
			}
			
			this.addIr(new IrCall(stackSize, retLabel));
			
			this.scope.stackOffset = stackRestore;
			
			return callee.ret;
		} else if (expr instanceof SubscriptExpression) {
			return this.generateExpression(new UnaryExpression(new Token(TokenType.Star, "*"), new BinaryExpression(
				expr.primary,
				new Token(TokenType.Plus, "+"),
				expr.secondary
			), true));
		} else {
			throw "Unknown expression " + expr.constructor.name;
		}
	}
	generateRvalExpr(expr: Expression, secondaryReg: boolean = false): Type {
		let addr = this.scope.stackOffset;
		let value = this.generateExpression(expr, secondaryReg);
		
		//console.log(value);
		
		if (value instanceof IrValue) {
			if (value.type instanceof ArrayType) {
				this.addressOf(value);
				return new PointerType(value.type.type);
			} else if (value instanceof IrTmpValue) {
				this.addIr(new IrMove(value.tmp, IrAddr.stack(addr)));
				this.scope.stackOffset = addr + 1;
				
				return value.type;
			} else {
				this.addIr(new IrMove(value.address, IrAddr.stack(addr)));
				this.scope.stackOffset = addr + 1;
				
				return value.type;
			}
		}
		
		return value;
	}
	
	generateArithmetic(expr: BinaryExpression): Type {
		let start = this.scope.stackOffset;
		let left = IrAddr.stack(this.scope.stackOffset);
		let dest = left;
		let leftType = this.generateRvalExpr(expr.left, false);
		if (
			this.lastIr() instanceof IrMove &&
			(this.lastIr() as IrMove).value instanceof IrAddr
		) {
			//dest = left;
			//left = this.code.pop().value;
			//this.scope.stackOffset = left.offset;
		}
		
		let right = IrAddr.stack(this.scope.stackOffset);
		let rightType = this.generateRvalExpr(expr.right, true);
		if (
			this.lastIr() instanceof IrMove &&
			(this.lastIr() as IrMove).value instanceof IrAddr
		) {
			//right = this.code.pop().value;
			//this.scope.stackOffset = right.offset;
		}
		
		if (
			expr.operator.type === TokenType.Plus &&
			(
				leftType instanceof PointerType && rightType.equals(Types.Int)// ||
				//leftType.equals(Types.Int) && rightType instanceof PointerType
			)
		) {
			// If rhs is a constant
			// TODO: this is an ugly way to check
			if (
				this.lastIr() instanceof IrMove &&
				typeof (this.lastIr() as IrMove).value === "number" &&
				(this.lastIr() as IrMove).value === 0
			) {
				let value = (this.code.pop() as IrMove).value;
				if (value === 0) return leftType;
			}
			if (leftType.type.memSize() !== 1) {
				//this.addIr(new IrMove(leftType.type.memSize(), IrAddr.reg1()));
				this.addIr(new IrMul(right, this.getConstant(leftType.type.memSize()), right));
			}
			this.addIr(new IrAdd(left, right, left));
			this.scope.stackOffset = start + 1;
			return leftType;
			//return leftType instanceof PointerType ? leftType : rightType;
		} else if (
			expr.operator.type === TokenType.Minus &&
			leftType instanceof PointerType && rightType.equals(Types.Int)
		) {
			this.addIr(new IrSub(left, right, left));
			this.scope.stackOffset = start + 1;
			return leftType;
		}
		
		if (!leftType.equals(Types.Int) || !rightType.equals(Types.Int)) {
			throw `Invalid operands ${leftType} and ${rightType} to arithmetic operator`;
		}
		
		switch (expr.operator.type) {
			case TokenType.Plus: this.addIr(new IrAdd(left, right, dest)); break;
			case TokenType.Minus: this.addIr(new IrSub(left, right, dest)); break;
			case TokenType.Star: this.addIr(new IrMul(left, right, dest)); break;
			case TokenType.Slash: this.addIr(new IrDiv(left, right, dest)); break;
			case TokenType.Percent: this.addIr(new IrMod(left, right, dest)); break;
		}
		
		this.scope.stackOffset = start + 1;
		
		return Types.Int;
	}
	generateComparison(expr: BinaryExpression): Type {
		let start = this.scope.stackOffset;
		let left = IrAddr.stack(this.scope.stackOffset);
		let leftType = this.generateRvalExpr(expr.left);
		let right = IrAddr.stack(this.scope.stackOffset);
		let rightType = this.generateRvalExpr(expr.right);
		
		if (!leftType.equals(Types.Int) || !rightType.equals(Types.Int)) {
			throw `Invalid operands ${leftType} and ${rightType} to comparison operator`;
		}
		
		switch (expr.operator.type) {
			case TokenType.Lt: this.addIr(new IrLt(left, right, left)); break;
			case TokenType.Gt: this.addIr(new IrLt(right, left, left)); break;
			case TokenType.LtEq: this.addIr(new IrLeq(left, right, left)); break;
			case TokenType.GtEq: this.addIr(new IrLeq(right, left, left)); break;
			case TokenType.EqEq: this.addIr(new IrEq(left, right, left)); break;
			case TokenType.BangEq: throw "Unimplemented";
		}
		
		this.scope.stackOffset = start + 1;
		
		return Types.Bool;
	}
	
	generateAssignment(expr: BinaryExpression): Type {
		let start = this.scope.stackOffset;
		let right = IrAddr.stack(this.scope.stackOffset);
		let rightType = this.generateRvalExpr(expr.right);
		
		let leftType = this.generateExpression(expr.left);
		
		if (!(leftType instanceof IrValue)) throw "Left-hand side of assignment is not an lvalue";
		
		if (!leftType.equals(rightType)) throw `Tried to assign ${rightType} to ${leftType}`;
		
		if (!(leftType.address instanceof IrAddr)) throw "error";
		
		this.addIr(new IrMove(right, leftType.address));
		
		this.scope.stackOffset = start + 1;
		return leftType;
	}
	generateCompundAssignment(expr: BinaryExpression): Type {
		let operator: Token;
		switch (expr.operator.type) {
			case TokenType.PlusEq: operator = new Token(TokenType.Plus, "+"); break;
			case TokenType.MinusEq: operator = new Token(TokenType.Minus, "-"); break;
			case TokenType.StarEq: operator = new Token(TokenType.Star, "*"); break;
			case TokenType.SlashEq: operator = new Token(TokenType.Slash, "/"); break;
			case TokenType.PercentEq: operator = new Token(TokenType.Percent, "%"); break;
			default:
				throw "Invalid compund operator";
		}
		
		return this.generateAssignment(new BinaryExpression(
			expr.left,
			new Token(TokenType.Eq, "="),
			new BinaryExpression(
				expr.left,
				operator,
				expr.right
			))
		);
	}
	
	getExpressionType(expr: Expression): Type {
		if (expr instanceof NumberLiteral) {
			return Types.Int;
		} else if (expr instanceof CharLiteral) {
			return Types.Char;
		} else if (expr instanceof Identifier) {
			let name = expr.name;
			let value = this.scope.resolve(name);
			if (!value) throw "Undeclared variable \"" + name + "\"";
			
			if (value instanceof IrValue) value = value.type;
			
			return value;
		} else if (expr instanceof UnaryExpression) {
			if (expr.prefix) {
				switch (expr.operator.type) {
					case TokenType.Star: {
						let operand = this.getExpressionType(expr.operand);
						if (!(operand instanceof PointerType)) throw "Indirection of non-pointer type " + operand;
						return operand.type;
					}
					case TokenType.Amp: {
						let operand = this.getExpressionType(expr.operand);
						return new PointerType(operand);
					}
					default:
						throw "Unknown unary prefix operator " + expr.operator;
				}
			} else {
				switch (expr.operator.type) {
					default:
						throw "Unknown unary postfix operator " + expr.operator;
				}
			}
		} else if (expr instanceof BinaryExpression) {
			switch (expr.operator.type) {
				case TokenType.Plus:
				case TokenType.Minus:
				case TokenType.Star:
				case TokenType.Slash:
				case TokenType.Percent:
					return Types.Int;
				case TokenType.Eq:
					return this.getExpressionType(expr.left);
				default:
					throw "Unknown binary operator " + expr.operator;
			}
		} else if (expr instanceof CallExpression) {
			let callee = this.getExpressionType(expr.callee);
			if (!(callee instanceof FunctionType)) throw "Tried to call " + callee;
			
			return callee.ret;
		}
		throw "error";
	}
}

enum Op {
	BINDEF = 0x00,
	TEXT = 0x01,
	DATA = 0x02,
	MOV = 0x03,
	CONST = 0x04,
	ADD = 0x05,
	MUL = 0x06,
	SUB = 0x07,
	DIV = 0x08,
	LT = 0x09,
	JMP = 0x0A,
	EXTERN = 0x0B,
	HALT = 0x0C,
	EQ = 0x0D,
	MOD = 0x0E,
	LEQ = 0x0F,
	JMPADR = 0x10
};

class AsmGenerator {
	ir: IrInstr[];
	data: number[];
	file: number[] = [];
	
	constructor(ir: IrInstr[], data: number[]) {
		this.ir = ir;
		this.data = data;
	}
	
	getConstant(value: number): IrAddr {
		if (!this.data.includes(value)) {
			this.data.push(value);
		}
		return new IrAddr(-1, -this.data.indexOf(value) - 1);
	}
	
	optimize() {
		// TODO:
		// * Replace REG_1 constants
		// * Optimize direct function calls
		// * Optimize direct extern calls
		
		
	}
	
	writeString(str: string) {
		if (str.length >= 256) throw "String too long";
		this.file.push(str.length);
		for (let i=0; i<str.length; i++) {
			this.file.push(str.charCodeAt(i));
		}
	}
	writeInt(value: number) {
		this.file.push((value >> 24) & 0xff);
		this.file.push((value >> 16) & 0xff);
		this.file.push((value >>  8) & 0xff);
		this.file.push((value >>  0) & 0xff);
	}
	writeAddr(addr: IrAddr) {
		this.writeInt(addr.base);
		this.writeInt(addr.offset);
	}
	writeIns(op: Op, args: (number | string | IrAddr | IrLabel | IrNamedLabel)[]) {
		this.file.push(op);
		for (let i=0; i<args.length; i++) {
			let arg = args[i];
			if (typeof arg === "number") {
				this.writeInt(arg);
			} else if (typeof arg === "string") {
				this.writeString(arg);
			} else if (arg instanceof IrAddr) {
				this.writeAddr(arg);
			} else {
				throw "Unknown argument";
			}
		}
	}
	
	replaceLabels() {
		let labels = new Map();
		for (let i=0; i<this.ir.length; i++) {
			let ins = this.ir[i];
			
			if (ins instanceof IrLabel) {
				labels.set(ins, i);
				this.ir.splice(i, 1);
				i--;
			}
		}
		
		function replace(input: any) {
			if (input instanceof IrLabel) {
				if (!labels.get(input)) throw "No such label " + input.id;
				return labels.get(input);
			}
			return input;
		}
		
		for (let i=0; i<this.ir.length; i++) {
			let ins = this.ir[i];
			
			if (ins instanceof IrMove) {
				ins.value = replace(ins.value);
				ins.addr = replace(ins.addr);
			} else if (ins instanceof IrBinop) {
				ins.left = replace(ins.left);
				ins.right = replace(ins.right);
				ins.dest = replace(ins.dest);
			} else if (ins instanceof IrJump) {
				ins.address = replace(ins.address);
			} else if (ins instanceof IrJumpZero) {
				ins.address = replace(ins.address);
				ins.test = replace(ins.test);
			}
		}
	}
	
	generate() {
		// Expand high-level instructions
		let asm = [];
		for (let i=0; i<this.ir.length; i++) {
			let ins = this.ir[i];
			if (ins instanceof IrCall) {
				let addr: IrAddr | IrLabel = IrAddr.stack(0);
				if (
					this.ir[i - 1] instanceof IrMove &&
					(this.ir[i - 1] as IrMove).value instanceof IrLabel
				) {
					addr = (asm.pop() as IrMove).value as IrLabel;
				}
				
				asm.push(new IrAdd(IrAddr.stackPointer(), this.getConstant(ins.stackSize), IrAddr.stackPointer()));
				asm.push(new IrJump(addr));
				
				asm.push(ins.retLabel);
				asm.push(new IrSub(IrAddr.stackPointer(), this.getConstant(ins.stackSize), IrAddr.stackPointer()));
			} else {
				asm.push(ins);
			}
		}
		this.ir = asm;
		
		this.replaceLabels();
		
		this.writeIns(Op.BINDEF, ["spooky++"]);
		//this.writeIns(Asm.BINDEF, ["scarrot"]);
		this.writeIns(Op.TEXT, []);
		
		// Write bytecode
		for (let i=0; i<this.ir.length; i++) {
			let ins = this.ir[i];
			
			if (ins instanceof IrMove) {
				if (typeof ins.value === "number") {
					this.writeIns(Op.CONST, [ins.value, ins.addr]);
				} else {
					this.writeIns(Op.MOV, [ins.value, ins.addr]);
				}
			} else if (ins instanceof IrBinop) {
				let operator;
				if (ins instanceof IrAdd) operator = Op.ADD;
				else if (ins instanceof IrSub) operator = Op.SUB;
				else if (ins instanceof IrMul) operator = Op.MUL;
				else if (ins instanceof IrDiv) operator = Op.DIV;
				else if (ins instanceof IrMod) operator = Op.MOD;
				else if (ins instanceof IrLt) operator = Op.LT;
				else if (ins instanceof IrLeq) operator = Op.LEQ;
				else if (ins instanceof IrEq) operator = Op.EQ;
				else throw "Unknown binary operator";
				
				this.writeIns(operator, [ins.left, ins.right, ins.dest]);
			} else if (ins instanceof IrLea) {
				this.writeIns(Op.ADD, [this.getConstant(ins.addr.offset), IrAddr.absolute(ins.addr.base), ins.dest]);
			} else if (ins instanceof IrJump) {
				if (typeof ins.address === "number") {
					this.writeIns(Op.JMP, [IrAddr.zero(), ins.address]);
				} else {
					this.writeIns(Op.JMPADR, [ins.address]);
				}
			} else if (ins instanceof IrJumpZero) {
				this.writeIns(Op.JMP, [ins.test, ins.address]);
			} else if (ins instanceof IrExtern) {
				this.writeIns(Op.EXTERN, [ins.name]);
			} else if (ins instanceof IrHalt) {
				this.writeIns(Op.HALT, []);
			} else {
				throw "Unexpected IR " + ins.constructor.name;
			}
		}
		
		this.writeIns(Op.DATA, this.data);
		
		return this.file;
	}
}
