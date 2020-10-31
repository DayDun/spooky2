/*

Problems:
	Functions should probably always be blocks, not enforced atm. could error atm.
	
	Decide on a convention for addr / address
	
	Maybe fix the stack frame stuff
	
	Refactor expression parsing
	
	Global initialization is probably volatilbe, possible to overwrite stuff

Ideas:
	Globals, including strings will go at the top of memory.
	
	But it will probably require a hidden function to be called before
	main that initializes all the globals.
	
	Optimize call stack of consecutive function calls

Test cases:
	Compare array with pointer

*/

/*

Stages:
	# AST
		The AST stage is a literal tree representation of
		the program containing no type information.
	# HighIr (HIR)
		The AST is converted into a typed tree structure

*/

// https://en.wikipedia.org/wiki/Visitor_pattern
// Add globals pronto

//const { TokenType, Token, Lexer } = require("./lexer.js");
import { TokenType, Token, Lexer } from "./lexer";
import {
	Type,
	TypeName,
	FunctionType,
	NamedArgument,
	PointerType,
	ArrayType,
	StructType,
	Types, TypeAliases,
	typeCmp, typeSize
} from "./types";
import {
	AstNode,
	Program,
	
	ExternDeclaration,
	FunctionDefinition,
	StructDefinition,
	
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
	ArrayLiteral,
	
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
	
	console.log(highIr);
	
	highIr.funs.forEach(fun => console.log(fun.body.toString()));
	
	let irGenerator = new IrGenerator(highIr.scope);
	let {ir, data} = irGenerator.generate(highIr.globals, highIr.externs, highIr.funs);
	
	console.log("========== IR ==========");
	console.log(ir.join("\n"));
	console.log(data);
	
	let asmGenerator = new AsmGenerator(ir, data);
	let asm = asmGenerator.generate();
	
	//console.log(asm);
	
	return asm;
}

class Scope {
	parent: Scope | null = null;
	names: Record<string, ScopedValue> = {};
	labels: Record<string, IrLabel> = {};
	stackOffset: number = 0;
	types: Record<string, Type> = {};
	
	resolve(name: string): ScopedValue | null {
		if (name in this.names) return this.names[name];
		if (this.parent !== null) return this.parent.resolve(name);
		return null;
	}
	add(value: ScopedValue) {
		if (value.name in this.names) throw "This name already exists in the current scope";
		this.names[value.name] = value;
	}
	
	resolveType(name: string): Type | null {
		if (name in this.types) return this.types[name];
		if (this.parent !== null) return this.parent.resolveType(name);
		return null;
	}
	addType(name: string, type: Type) {
		if (name in this.types) throw "This type already exists in the current scope";
		this.types[name] = type;
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
class ScopedValue {
	name: string;
	value: Expression;
	constant: boolean;
	
	constructor(name: string, value: Expression, constant: boolean = false) {
		this.name = name;
		this.value = value;
		this.constant = constant;
	}
}

class StackPointer extends Expression {
	type = Types.Int;
	
	toString() { return "SP"; }
}
class LabelValue extends Expression {}

enum Operation {
	Add, Sub, Mul, Div, Mod,
	Lt, LtEq, Gt, GtEq,
	Eq, NotEq,
	
	Int, Neg,
	Not,
	
	Deref, Ref
}

class HighIrOperation extends Expression {
	operation: Operation;
	args: Expression[];
	
	constructor(operation: Operation, args: Expression[]) {
		super();
		this.operation = operation;
		this.args = args;
	}
	
	toString(): string {
		switch (this.operation) {
			case Operation.Deref: return `(*${this.args[0]})`;
			case Operation.Ref: return `(&${this.args[0]})`;
			case Operation.Add: return `(${this.args[0]} + ${this.args[1]})`;
			case Operation.Sub: return `(${this.args[0]} - ${this.args[1]})`;
			case Operation.Mul: return `(${this.args[0]} * ${this.args[1]})`;
			case Operation.Gt: return `(${this.args[0]} > ${this.args[1]})`;
			case Operation.GtEq: return `(${this.args[0]} >= ${this.args[1]})`;
			default: return `${Operation[this.operation]}(${this.args.join(", ")})`;
		}
	}
}
class HighIrAssign extends Expression {
	dst: Expression;
	compound: Operation | null;
	val: Expression;
	post: boolean;
	
	constructor(dst: Expression, compound: Operation | null, val: Expression, post: boolean) {
		super();
		this.dst = dst;
		this.compound = compound;
		this.val = val;
		this.post = post;
	}
	
	toString() {
		if (this.compound === null) return `${this.dst} = ${this.val}`;
		return `(${this.dst} ${Operation[this.compound]}=${this.post ? "post" : ""} ${this.val})`;
	}
}

class HighIrGenerator {
	scope = new Scope();
	
	initType(type: Type): Type {
		if (type instanceof TypeName) {
			if (type.name in Types) {
				return Types[type.name];
			} else if (type.name in TypeAliases) {
				return TypeAliases[type.name];
			} else {
				let resType = this.scope.resolveType(type.name);
				if (resType) return resType;
				throw "Unknown type " + type.name;
			}
		} else if (type instanceof PointerType) {
			type.type = this.initType(type.type);
		} else if (type instanceof ArrayType) {
			type.type = this.initType(type.type);
		} else if (type instanceof StructType) {
			type.records = type.records.map(r => this.initType(r) as NamedArgument);
		} else if (type instanceof FunctionType) {
			type.args = type.args.map(a => this.initType(a));
			type.ret = this.initType(type.ret);
		} else if (type instanceof NamedArgument) {
			type.type = this.initType(type.type);
		}
		return type;
	}
	
	initFunction(fun: FunctionDefinition): FunctionDefinition {
		this.scope = this.scope.child();
		this.scope.stackOffset = 0;
		
		let offset = 0;
		fun.type.args.slice().reverse().forEach(arg => {
			if (!(arg instanceof NamedArgument)) throw "Expected named arguments in function definition";
			
			let name = arg.name;
			let type = arg.type;
			offset += typeSize(type);
			let value = new HighIrOperation(Operation.Deref, [
				new HighIrOperation(Operation.Sub, [
					new StackPointer,
					new NumberLiteral(offset)
				])
			]);
			value.type = type;
			
			this.scope.add(new ScopedValue(name, value));
		});
		
		// Add return statement to void functions
		if (
			typeCmp(fun.type.ret, Types.Void) &&
			!(fun.body.body[fun.body.body.length - 1] instanceof ReturnStatement)
		) {
			fun.body.body.push(new ReturnStatement(null));
		}
		
		fun.body = this.initStatement(fun.body) as BlockStatement;
		
		this.scope = this.scope.pop();
		
		return fun;
	}
	
	initStatement(stmt: Statement): Statement {
		if (stmt instanceof BlockStatement) {
			this.scope = this.scope.child();
			stmt.body = stmt.body.map(a => this.initStatement(a));
			this.scope = this.scope.pop();
		} else if (stmt instanceof DeclarationStatement) {
			stmt = this.initDeclaration(stmt);
		} else if (stmt instanceof ExpressionStatement) {
			stmt.expression = this.initExpression(stmt.expression);
		} else if (stmt instanceof IfStatement) {
			stmt.test = this.initExpression(stmt.test);
			stmt.consequent = this.initStatement(stmt.consequent);
			if (stmt.alternate) {
				stmt.alternate = this.initStatement(stmt.alternate);
			}
		} else if (stmt instanceof ForStatement) {
			this.scope = this.scope.child();
			if (stmt.init) stmt.init = this.initStatement(stmt.init);
			if (stmt.test) stmt.test = this.initExpression(stmt.test);
			if (stmt.inc) stmt.inc = this.initExpression(stmt.inc);
			
			stmt.body = this.initStatement(stmt.body);
			this.scope = this.scope.pop();
		} else if (stmt instanceof WhileStatement) {
			stmt.test = this.initExpression(stmt.test);
			stmt.body = this.initStatement(stmt.body);
		} else if (stmt instanceof DoWhileStatement) {
			stmt.body = this.initStatement(stmt.body);
			stmt.test = this.initExpression(stmt.test);
		} else if (stmt instanceof BreakStatement) {
			
		} else if (stmt instanceof ContinueStatement) {
			
		} else if (stmt instanceof ReturnStatement) {
			if (stmt.value) stmt.value = this.initExpression(stmt.value);
		} else if (stmt instanceof EmptyStatement) {
			
		} else {
			throw "Unknown statement " + stmt.constructor.name;
		}
		return stmt;
	}
	
	initDeclaration(stmt: DeclarationStatement, global = false): Statement {
		let name = stmt.name;
		if (name in this.scope.names) {
			throw "Variable \"" + name + "\" already declared in the same scope";
		}
		
		let type = stmt.type;
		if (stmt.init) {
			let expr = this.initExpression(stmt.init);
			let inferredType = expr.type as Type;
			if (expr instanceof ArrayLiteral && stmt.constant) {
				inferredType = new PointerType((expr.type as ArrayType).type);
			} else {
				expr = this.getExprVal(expr);
			}
			stmt.init = expr;
			if (!type) type = inferredType;
			
			//if (!type.equals(inferredType)) throw "Declaration type mismatch";
		}
		if (type === null) throw "Declaration type is fucked";
		type = this.initType(type);
		stmt.type = type;
		
		let value: Expression;
		if (global) {
			let offset = new NumberLiteral(this.scope.stackOffset + 1);
			offset.type = Types.Int;
			value = new HighIrOperation(Operation.Deref, [
				offset
			]);
		} else {
			let offset = new NumberLiteral(this.scope.stackOffset);
			offset.type = Types.Int;
			let sp = new StackPointer;
			sp.type = new PointerType(type);
			value = new HighIrOperation(Operation.Deref, [
				new HighIrOperation(Operation.Add, [
					sp,
					offset
				])
			]);
		}
		this.scope.stackOffset += typeSize(type);
		value.type = type;
		
		this.scope.add(new ScopedValue(name, value));
		
		//if (!stmt.init) return new EmptyStatement;
		//return new ExpressionStatement(new HighIrAssign(value, null, stmt.init, false));
		return stmt;
	}
	
	initExpression(expr: Expression): Expression {
		if (expr instanceof NumberLiteral) {
			expr.type = Types.Int;
			return expr;
		} else if (expr instanceof CharLiteral) {
			let out = new NumberLiteral(expr.value.charCodeAt(0));
			out.type = Types.Char;
			return out;
		} else if (expr instanceof StringLiteral) {
			let out: Expression = new ArrayLiteral(
				(expr.value + "\0").split("").map(a => new CharLiteral(a))
			);
			out = this.initExpression(out);
			return out;
		} else if (expr instanceof ArrayLiteral) {
			if (expr.value.length === 0) throw "Array literal cannot have length 0";
			
			expr.value = expr.value.map(a => this.initExpression(a));
			
			let type = expr.value[0].type as Type;
			for (let i=1; i<expr.value.length; i++) {
				if (typeCmp(type, expr.value[i].type as Type)) continue;
				
				throw "Array literal cannot contain multiple types";
			}
			
			expr.type = new ArrayType(type, expr.value.length);
		} else if (expr instanceof Identifier) {
			let value = this.scope.resolve(expr.name);
			if (value === null) throw "Undeclared identifier \"" + expr.name + "\"";
			
			return value.value;
		} else if (expr instanceof BinaryExpression) {
			if (
				expr.operator.type === TokenType.PlusEq ||
				expr.operator.type === TokenType.MinusEq ||
				expr.operator.type === TokenType.StarEq ||
				expr.operator.type === TokenType.SlashEq ||
				expr.operator.type === TokenType.PercentEq
			) {
				let op;
				switch (expr.operator.type) {
					case TokenType.PlusEq: op = new Token(TokenType.Plus, "+"); break;
					case TokenType.MinusEq: op = new Token(TokenType.Minus, "-"); break;
					case TokenType.StarEq: op = new Token(TokenType.Star, "*"); break;
					case TokenType.SlashEq: op = new Token(TokenType.Slash, "/"); break;
					case TokenType.PercentEq: op = new Token(TokenType.Percent, "%"); break;
				}
				
				let binex = this.initExpression(new BinaryExpression(
					expr.left,
					op,
					expr.right
				)) as HighIrOperation;
				
				//console.log(binex);
				
				let dest = this.getExprMetaVal(binex.args[0]);
				
				if (!(
					dest instanceof HighIrOperation &&
					dest.operation === Operation.Deref
				)) throw "Can't assign to non-reference (compound)";
				
				let out = new HighIrAssign(
					binex.args[0],
					binex.operation,
					binex.args[1],
					false
				);
				out.type = binex.type;
				return out;
			} else if (expr.operator.type === TokenType.Dot) {
				let lhs = this.initExpression(expr.left);
				if (!(lhs.type instanceof StructType)) throw "Can't member select " + lhs.type;
				if (!(expr.right instanceof Identifier)) throw "Can't member select with " + expr.right;
				if (!(
					lhs instanceof HighIrOperation &&
					lhs.operation === Operation.Deref
				)) throw "Can't member select non-reference";
				
				let offset = 0;
				let record;
				for (let i = 0; i<lhs.type.records.length; i++) {
					let rec = lhs.type.records[i];
					if (rec.name === expr.right.name) {
						record = rec;
						break;
					}
					offset += typeSize(rec);
				}
				if (!record) throw "Struct " + lhs.type + " does not contain field " + expr.right.name;
				
				let off = new NumberLiteral(offset);
				off.type = Types.Int;
				let out = new HighIrOperation(Operation.Deref, [
					new HighIrOperation(Operation.Add, [
						lhs.args[0],
						off
					])
				]);
				out.type = record.type;
				return out;
			} else if (expr.operator.type === TokenType.Arrow) {
				return this.initExpression(new BinaryExpression(
					new UnaryExpression(new Token(TokenType.Star, "*"), expr.left, true),
					new Token(TokenType.Dot, "."),
					expr.right
				));
			}
			
			let lhs = this.initExpression(expr.left);
			let rhs = this.initExpression(expr.right);
			
			switch (expr.operator.type) {
				case TokenType.Plus:
					if (
						typeCmp(lhs.type as Type, Types.Int) &&
						(rhs.type instanceof PointerType || rhs.type instanceof ArrayType)
					) {
						[lhs, rhs] = [rhs, lhs];
					}
					// Fallthrough
				case TokenType.Minus:
					if (
						(lhs.type instanceof PointerType || lhs.type instanceof ArrayType) &&
						typeCmp(rhs.type as Type, Types.Int)
					) {
						lhs = this.getExprVal(lhs);
						let op = Operation.Add;
						if (expr.operator.type === TokenType.Minus) op = Operation.Sub;
						
						let size = new NumberLiteral(typeSize((lhs.type as PointerType | ArrayType).type));
						size.type = Types.Int;
						let shift = new HighIrOperation(Operation.Mul, [
							rhs,
							size
						]);
						shift.type = Types.Int;
						let out = new HighIrOperation(op, [
							lhs,
							shift
						]);
						out.type = lhs.type;
						return out;
					}
					// Fallthrough
				case TokenType.Star:
				case TokenType.Slash:
				case TokenType.Percent: {
					if (!(typeCmp(lhs.type as Type, Types.Int) && typeCmp(rhs.type as Type, Types.Int))) {
						throw "Can't perform arithmetic with " + lhs.type + " and " + rhs.type;
					}
					let op;
					switch (expr.operator.type) {
						case TokenType.Plus: op = Operation.Add; break;
						case TokenType.Minus: op = Operation.Sub; break;
						case TokenType.Star: op = Operation.Mul; break;
						case TokenType.Slash: op = Operation.Div; break;
						case TokenType.Percent: op = Operation.Mod; break;
					}
					let out = new HighIrOperation(op, [lhs, rhs]);
					out.type = Types.Int;
					return out;
				}
				case TokenType.Lt:
				case TokenType.LtEq:
				case TokenType.Gt:
				case TokenType.GtEq:
				case TokenType.EqEq:
				case TokenType.BangEq: {
					if (!(
						(typeCmp(lhs.type as Type, Types.Int) || lhs.type instanceof PointerType) &&
						(typeCmp(rhs.type as Type, Types.Int) || rhs.type instanceof PointerType)
					)) {
						throw "Can't compare " + lhs.type + " and " + rhs.type;
					}
					let op;
					switch (expr.operator.type) {
						case TokenType.Lt: op = Operation.Lt; break;
						case TokenType.LtEq: op = Operation.LtEq; break;
						case TokenType.Gt: op = Operation.Gt; break;
						case TokenType.GtEq: op = Operation.GtEq; break;
						case TokenType.EqEq: op = Operation.Eq; break;
						case TokenType.BangEq: op = Operation.NotEq; break;
					}
					let out = new HighIrOperation(op, [lhs, rhs]);
					out.type = Types.Bool;
					return out;
				}
				case TokenType.Eq: {
					if (!(
						lhs instanceof HighIrOperation &&
						lhs.operation === Operation.Deref
					)) {
						//console.log(lhs);
						throw "Can't assign to non-reference (regular)";
					}
					if (!typeCmp(lhs.type as Type, rhs.type as Type)) throw "Can't assign " + rhs.type + " to " + lhs.type;
					
					let out = new HighIrAssign(lhs, null, rhs, false);
					out.type = lhs.type;
					return out;
				}
				default:
					throw "Unknown binary operator " + expr.operator;
			}
		} else if (expr instanceof UnaryExpression) {
			if (
				expr.operator.type === TokenType.PlusPlus ||
				expr.operator.type === TokenType.MinusMinus
			) {
				let op;
				if (expr.operator.type === TokenType.PlusPlus) op = new Token(TokenType.Plus, "+");
				else op = new Token(TokenType.Minus, "-");
				
				let binex = this.initExpression(new BinaryExpression(
					expr.operand,
					op,
					new NumberLiteral(1)
				)) as HighIrOperation;
				
				let dest = this.getExprMetaVal(binex.args[0]);
				
				if (!(
					dest instanceof HighIrOperation &&
					dest.operation === Operation.Deref
				)) throw "Can't assign to non-reference";
				
				let out = new HighIrAssign(
					binex.args[0],
					binex.operation,
					binex.args[1],
					!expr.prefix
				);
				out.type = binex.args[0].type;
				return out;
			}
			
			if (!expr.prefix) {
				throw "Unknown postfix operator " + expr.operator;
			}
			
			let operand = this.initExpression(expr.operand);
			
			switch (expr.operator.type) {
				case TokenType.Plus:
				case TokenType.Minus: {
					if (!typeCmp(operand.type as Type, Types.Int)) {
						throw "Can't perform arithmetic with " + operand;
					}
					let op = Operation.Int;
					if (expr.operator.type) op = Operation.Neg;
					
					let out = new HighIrOperation(op, [operand]);
					out.type = Types.Int;
					return out;
				}
				case TokenType.Bang: {
					if (!typeCmp(operand.type as Type, Types.Bool)) {
						throw "Can't not " + operand.type;
					}
					let out = new HighIrOperation(Operation.Not, [operand]);
					out.type = Types.Bool;
					return out;
				}
				case TokenType.Star: {
					if (!(
						operand.type instanceof PointerType ||
						operand.type instanceof ArrayType
					)) {
						throw "Can't dereference " + operand.type;
					}
					operand = this.getExprVal(operand);
					let out = new HighIrOperation(Operation.Deref, [operand]);
					out.type = (operand.type as PointerType | ArrayType).type;
					return out;
				}
				case TokenType.Amp: {
					if (!(
						operand instanceof HighIrOperation &&
						operand.operation === Operation.Deref
					)) {
						throw "Can't reference non-reference";
					}
					let out = operand.args[0];
					out.type = new PointerType(operand.type as Type);
					return out;
				}
				default:
					throw "Unknown prefix operator " + expr.operator;
			}
		} else if (expr instanceof CallExpression) {
			let callee = this.initExpression(expr.callee);
			if (!(callee.type instanceof FunctionType)) {
				throw "Can't call " + callee.type;
			}
			let args = expr.args.map(arg => {
				arg = this.initExpression(arg);
				arg = this.getExprVal(arg);
				return arg;
			});
			
			let typeMatch = callee.type.args.length === args.length;
			if (typeMatch) {
				for (let i=0; i<args.length; i++) {
					if (typeCmp(args[i].type as Type, callee.type.args[i])) continue;
					
					typeMatch = false;
					break;
				}
			}
			if (!typeMatch) {
				let givenType = `(${args.map(a => a.type).join(", ")})`;
				throw `Called function type ${callee.type} does not match given arguments ${givenType}`;
			}
			
			let out = new CallExpression(callee, args)
			out.type = callee.type.ret;
			return out;
		} else if (expr instanceof SubscriptExpression) {
			let primary = this.initExpression(expr.primary);
			let secondary = this.initExpression(expr.secondary);
			
			if (
				primary.type instanceof PointerType ||
				primary.type instanceof ArrayType
			) {
				return this.initExpression(new UnaryExpression(
					new Token(TokenType.Star, "*"),
					new BinaryExpression(
						primary,
						new Token(TokenType.Plus, "+"),
						secondary
					),
					true
				));
			} else {
				throw "Can't index " + primary.type;
			}
		} else if (expr instanceof HighIrOperation) {
			
		} else if (expr instanceof StackPointer) {
			
		} else {
			throw "Unknown expression " + expr.constructor.name;
		}
		return expr;
	}
	getExprVal(expr: Expression): Expression {
		if (expr.type instanceof ArrayType) {
			let out = new HighIrOperation(Operation.Ref, [expr]);
			out.type = new PointerType((expr.type as ArrayType).type);
			return out;
		}
		return expr;
	}
	getExprMetaVal(expr: Expression): Expression {
		if (expr instanceof ArrayType) {
			let out = new HighIrOperation(Operation.Ref, [expr]);
			out.type = new PointerType((expr.type as ArrayType).type);
			return out;
		} else if (expr instanceof HighIrAssign) {
			if (!expr.post) {
				return this.getExprMetaVal(expr.dst);
			}
		}
		return expr;
	}
	
	optimizeStatement(stmt: Statement): Statement {
		if (stmt instanceof BlockStatement) {
			stmt.body = stmt.body.map(a => this.optimizeStatement(a));
		} else if (stmt instanceof DeclarationStatement) {
			if (stmt.init) stmt.init = this.optimizeExpression(stmt.init);
		} else if (stmt instanceof ExpressionStatement) {
			stmt.expression = this.optimizeExpression(stmt.expression);
		} else if (stmt instanceof IfStatement) {
			stmt.test = this.optimizeExpression(stmt.test);
			stmt.consequent = this.optimizeStatement(stmt.consequent);
			if (stmt.alternate) {
				stmt.alternate = this.optimizeStatement(stmt.alternate);
			}
		} else if (stmt instanceof ForStatement) {
			if (stmt.init) stmt.init = this.optimizeStatement(stmt.init);
			if (stmt.test) stmt.test = this.optimizeExpression(stmt.test);
			if (stmt.inc) stmt.inc = this.optimizeExpression(stmt.inc);
			
			stmt.body = this.optimizeStatement(stmt.body);
		} else if (stmt instanceof WhileStatement) {
			stmt.test = this.optimizeExpression(stmt.test);
			stmt.body = this.optimizeStatement(stmt.body);
		} else if (stmt instanceof DoWhileStatement) {
			stmt.body = this.optimizeStatement(stmt.body);
			stmt.test = this.optimizeExpression(stmt.test);
		} else if (stmt instanceof BreakStatement) {
			
		} else if (stmt instanceof ContinueStatement) {
			
		} else if (stmt instanceof ReturnStatement) {
			if (stmt.value) stmt.value = this.optimizeExpression(stmt.value);
		} else if (stmt instanceof EmptyStatement) {
			
		} else {
			throw "Unknown statement " + stmt.constructor.name;
		}
		return stmt;
	}
	optimizeExpression(expr: Expression): Expression {
		if (expr instanceof NumberLiteral) {
			
		} else if (expr instanceof ArrayLiteral) {
			expr.value = expr.value.map(a => this.optimizeExpression(a));
		} else if (expr instanceof StackPointer) {
			
		} else if (expr instanceof HighIrOperation) {
			switch (expr.operation) {
				case Operation.Add:
				case Operation.Sub:
				case Operation.Mul:
				case Operation.Div:
				case Operation.Mod:
					expr.args[0] = this.optimizeExpression(expr.args[0]);
					expr.args[1] = this.optimizeExpression(expr.args[1]);
					if (
						expr.args[0] instanceof NumberLiteral &&
						expr.args[1] instanceof NumberLiteral
					) {
						let left = (expr.args[0] as NumberLiteral).value;
						let right = (expr.args[1] as NumberLiteral).value;
						let res;
						switch (expr.operation) {
							case Operation.Add: res = left + right; break;
							case Operation.Sub: res = left - right; break;
							case Operation.Mul: res = left * right; break;
							case Operation.Div: res = left / right; break;
							case Operation.Mod: res = left % right; break;
						}
						return new NumberLiteral(res | 0);
					} else if (
						expr.args[0] instanceof NumberLiteral ||
						expr.args[1] instanceof NumberLiteral
					) {
						let num: NumberLiteral;
						let other: Expression;
						if (expr.args[0] instanceof NumberLiteral) {
							num = expr.args[0] as NumberLiteral;
							other = expr.args[1];
						} else {
							num = expr.args[1] as NumberLiteral;
							other = expr.args[0];
						}
						
						if (
							expr.operation === Operation.Add &&
							num.value === 0
						) {
							return other;
						} else if (
							expr.operation === Operation.Mul &&
							num.value === 1
						) {
							return other;
						}
					}
					break;
				case Operation.Lt:
				case Operation.LtEq:
				case Operation.Gt:
				case Operation.GtEq:
				case Operation.Eq:
					expr.args[0] = this.optimizeExpression(expr.args[0]);
					expr.args[1] = this.optimizeExpression(expr.args[1]);
					break;
				case Operation.Int:
					return expr.args[0];
				case Operation.Neg: {
					return this.optimizeExpression(new HighIrOperation(Operation.Mul, [
						expr.args[0],
						new NumberLiteral(-1)
					]));
				}
				case Operation.Ref: {
					if (!(
						expr.args[0] instanceof HighIrOperation &&
						(expr.args[0] as HighIrOperation).operation === Operation.Deref
					)) throw "Can't take address of non-reference";
					let out = (expr.args[0] as HighIrOperation).args[0];
					out.type = expr.type;
					return this.optimizeExpression(out);
				}
				case Operation.Deref:
					expr.args[0] = this.optimizeExpression(expr.args[0]);
					break;
				default:
					throw "Unknown operation " + Operation[expr.operation];
			}
		} else if (expr instanceof HighIrAssign) {
			expr.dst = this.optimizeExpression(expr.dst);
			expr.val = this.optimizeExpression(expr.val);
		} else if (expr instanceof CallExpression) {
			expr.callee = this.optimizeExpression(expr.callee);
			expr.args = expr.args.map(arg => this.optimizeExpression(arg));
		} else if (expr instanceof LabelValue) {
			
		} else {
			throw "Unknown expression in optimizing pass " + expr.constructor.name;
		}
		return expr;
	}
	
	generate(program: Program) {
		// Structs
		let structs = program.body.filter((a): a is StructDefinition => a instanceof StructDefinition);
		structs.forEach(struct => {
			this.scope.addType(struct.name, new StructType(struct.fields.map(record => {
				if (record.init) throw "Struct defaults unsupported";
				return new NamedArgument(record.name, record.type as Type);
			})));
		});
		console.log(this.scope.types);
		structs.forEach(struct => {
			let type = this.scope.resolveType(struct.name) as StructType;
			type.records = type.records.map(r => this.initType(r) as NamedArgument);
		});
		
		// Function declarations
		let funs = program.body.filter((a): a is FunctionDefinition => a instanceof FunctionDefinition);
		funs.forEach(fun => {
			let value = new LabelValue;
			value.type = this.initType(fun.type);
			this.scope.add(new ScopedValue(fun.name, value, true));
		});
		
		if (this.scope.resolve("main") === null) throw "Program needs a main function";
		
		// Externs
		let externs = program.body.filter((a): a is ExternDeclaration => a instanceof ExternDeclaration);
		externs.forEach(extern => {
			let value = new LabelValue;
			value.type = this.initType(extern.type);
			this.scope.add(new ScopedValue(extern.name, value, true));
		});
		
		// Globals
		let globals = program.body.filter((a): a is DeclarationStatement => a instanceof DeclarationStatement);
		globals.forEach(global => {
			let dec = this.initDeclaration(global, true);
			dec = this.optimizeStatement(dec);
			//this.scope.add(new ScopedValue(dec.name, dec.init as Expression, dec.constant));
		});
		
		// Function definitions
		funs.forEach(fun => {
			fun = this.initFunction(fun);
			//console.log(fun.body.toString());
			fun.body = this.optimizeStatement(fun.body) as BlockStatement;
		});
		
		return {
			scope: this.scope,
			globals,
			externs,
			funs
		};
	}
}

/*==============*/
//      IR
/*==============*/
/*class IrValue {
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
}*/
// used for postfix increment
/*class IrTmpValue extends IrValue {
	tmp: IrAddr;
	
	constructor(type: Type, address: IrAddr, tmp: IrAddr) {
		super(type, address);
		this.tmp = tmp;
	}
	
	toString(): string {
		return `'& ${this.type}`;
	}
}*/

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
	static reg(n: number) { return IrAddr.absolute(1 + n); }
	static zero() { return IrAddr.data(0); }
	static one() { return IrAddr.data(1); }
	static regRef(n: number) { return new IrAddr(1 + n, 0); }
	
	eq(base: number, offset: number): boolean {
		return this.base === base && this.offset === offset;
	}
	/*isReg1() {
		return this.eq(-1, 1);
	}
	isReg1Ref() { return this.eq(1, 0); }
	isReg2Ref() { return this.eq(2, 0); }*/
	
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
	callee: IrAddr;
	stackSize: number;
	retLabel: IrLabel;
	
	constructor(callee: IrAddr, stackSize: number, retLabel: IrLabel) {
		super();
		this.callee = callee;
		this.stackSize = stackSize;
		this.retLabel = retLabel;
	}
	
	toString(): string {
		return `\tCALL\t${this.callee} ${this.stackSize} ${this.retLabel}`;
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
	code: IrInstr[] = [];
	data: number[] = [0, 1];
	regsUsed: number = 0;
	maxRegs: number = 0;
	
	scope: Scope;
	labels: Map<LabelValue, IrLabel> = new Map();
	
	constructor(scope: Scope) {
		this.scope = scope;
	}
	
	addIr(ir: IrInstr) {
		this.code.push(ir);
	}
	useReg() {
		this.regsUsed++;
		this.maxRegs = Math.max(this.maxRegs, this.regsUsed);
		return this.regsUsed - 1;
	}
	
	resolveLabel(label: LabelValue): IrLabel {
		if (!this.labels.has(label)) {
			this.labels.set(label, new IrLabel);
		}
		return this.labels.get(label) as IrLabel;
	}
	
	generateFunction(fun: FunctionDefinition) {
		this.addIr(this.resolveLabel((this.scope.resolve(fun.name) as ScopedValue).value));
		
		this.scope = this.scope.child();
		
		let stackOffset = 0;
		fun.type.args.forEach(arg => {
			stackOffset += typeSize(arg);
		});
		
		// Return address
		stackOffset += 1;
		this.scope.add(new ScopedValue("__return__", new HighIrOperation(Operation.Deref, [
			new HighIrOperation(Operation.Sub, [
				new StackPointer,
				new NumberLiteral(stackOffset)
			])
		])));
		
		// Return value
		if (!typeCmp(fun.type.ret, Types.Void)) {
			stackOffset += typeSize(fun.type.ret);
			this.scope.add(new ScopedValue("__value__",new HighIrOperation(Operation.Deref, [
				new HighIrOperation(Operation.Sub, [
					new StackPointer,
					new NumberLiteral(stackOffset)
				])
			]) ));
		}
		
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
			let start = this.scope.stackOffset;
			if (stmt.init) {
				if (stmt.init instanceof ArrayLiteral) {
					if (stmt.constant) {
						stmt.init.value.slice().reverse().forEach(val => {
							if (!(val instanceof NumberLiteral)) {
								throw "Value in constant array is not constant: " + val;
							}
							
							this.data.push(val.value);
						});
						this.generateExpression(new HighIrAssign(
							new HighIrOperation(Operation.Deref, [
								new HighIrOperation(Operation.Add, [
									new StackPointer,
									new NumberLiteral(start)
								])
							]),
							null,
							new NumberLiteral(-this.data.length),
							false
						));
					} else {
						throw "TODO";
					}
				} else {
					this.generateExpression(new HighIrAssign(
						new HighIrOperation(Operation.Deref, [
							new HighIrOperation(Operation.Add, [
								new StackPointer,
								new NumberLiteral(start)
							])
						]),
						null,
						stmt.init,
						false
					));
				}
			}
			this.scope.stackOffset = start + typeSize(stmt.type as Type);
		} else if (stmt instanceof ExpressionStatement) {
			let stack = this.scope.stackOffset;
			let regs = this.regsUsed;
			this.generateExpression(stmt.expression);
			this.regsUsed = regs;
			//if (this.scope.stackOffset !== stack) throw "Stack leak";
			this.scope.stackOffset = stack;
		} else if (stmt instanceof IfStatement) {
			let altLabel = new IrLabel;
			let endLabel;
			
			let addr = IrAddr.stack(this.scope.stackOffset);
			let regs = this.regsUsed;
			this.generateExpression(stmt.test);
			this.regsUsed = regs;
			this.addIr(new IrJumpZero(altLabel, addr));
			
			this.generateStatement(stmt.consequent);
			if (stmt.alternate) {
				endLabel = new IrLabel;
				this.addIr(new IrJump(endLabel));
			}
				
			this.addIr(altLabel);
			
			if (stmt.alternate)	{
				this.generateStatement(stmt.alternate);
				this.addIr(endLabel as IrLabel);
			}
		} else if (stmt instanceof ForStatement) {
			this.scope = this.scope.child();
			if (stmt.init) {
				this.generateStatement(stmt.init);
			}
			
			let loopLabel = new LabelValue;
			let contLabel = new LabelValue;
			let endLabel = new LabelValue;
			this.scope.add(new ScopedValue("__continue__", contLabel));
			this.scope.add(new ScopedValue("__break__", endLabel));
			
			this.addIr(this.resolveLabel(loopLabel));
			if (!stmt.inc) {
				this.addIr(this.resolveLabel(contLabel));
			}
			
			if (stmt.test) {
				let stack = this.scope.stackOffset;
				let regs = this.regsUsed;
				let addr = this.generateExpression(stmt.test);
				this.regsUsed = regs;
				this.addIr(new IrJumpZero(this.resolveLabel(endLabel), addr));
				this.scope.stackOffset = stack;
			}
			
			this.generateStatement(stmt.body);
			
			if (stmt.inc) {
				let stack = this.scope.stackOffset;
				this.addIr(this.resolveLabel(contLabel));
				let regs = this.regsUsed;
				this.generateExpression(stmt.inc);
				this.regsUsed = regs;
				this.scope.stackOffset = stack;
			}
			this.addIr(new IrJump(this.resolveLabel(loopLabel)));
			
			this.addIr(this.resolveLabel(endLabel));
			this.scope = this.scope.pop();
		} else if (stmt instanceof WhileStatement) {
			this.scope = this.scope.child();
			let loopLabel = new LabelValue;
			let endLabel = new LabelValue;
			this.scope.add(new ScopedValue("__continue__", loopLabel));
			this.scope.add(new ScopedValue("__break__", endLabel));
			this.addIr(this.resolveLabel(loopLabel));
			
			let regs = this.regsUsed;
			let addr = this.generateExpression(stmt.test);
			this.regsUsed = regs;
			this.addIr(new IrJumpZero(this.resolveLabel(endLabel), addr));
			
			this.generateStatement(stmt.body);
			this.addIr(new IrJump(this.resolveLabel(loopLabel)));
			
			this.addIr(this.resolveLabel(endLabel));
			this.scope = this.scope.pop();
		} else if (stmt instanceof DoWhileStatement) {
			this.scope = this.scope.child();
			let loopLabel = new LabelValue;
			let endLabel = new LabelValue;
			this.scope.add(new ScopedValue("__continue__", loopLabel));
			this.scope.add(new ScopedValue("__break__", endLabel));
			this.addIr(this.resolveLabel(loopLabel));
			
			this.generateStatement(stmt.body);
			
			let regs = this.regsUsed;
			let addr = this.generateExpression(stmt.test);
			this.regsUsed = regs;
			this.addIr(new IrJumpZero(this.resolveLabel(endLabel), addr));
			
			this.addIr(new IrJump(this.resolveLabel(loopLabel)));
			
			this.addIr(this.resolveLabel(endLabel));
			this.scope = this.scope.pop();
		} else if (stmt instanceof BreakStatement) {
			let breakLabel = this.scope.resolve("__break__");
			if (breakLabel === null) throw "No loop to break out of";
			
			this.addIr(new IrJump(this.resolveLabel(breakLabel.value)));
		} else if (stmt instanceof ContinueStatement) {
			let contLabel = this.scope.resolve("__continue__");
			if (contLabel === null) throw "No loop to continue";
			
			this.addIr(new IrJump(this.resolveLabel(contLabel.value)));
		} else if (stmt instanceof ReturnStatement) {
			let returnValDest = this.scope.resolve("__value__");
			
			if (returnValDest) {
				if (!stmt.value) throw "Return in function with return type has to return a value";
				
				let regs = this.regsUsed;
				this.generateExpression(new HighIrAssign(returnValDest.value, null, stmt.value, false));
				this.regsUsed = regs;
			} else {
				if (stmt.value) throw "Return in Void function can't return a value";
			}
			
			let retAddr = ((((this.scope.resolve("__return__") as ScopedValue).value as HighIrOperation).args[0] as HighIrOperation).args[1] as NumberLiteral).value;
			this.addIr(new IrJump(IrAddr.stack(-retAddr)));
		} else if (stmt instanceof EmptyStatement) {
			
		} else {
			throw "Unknown statement " + stmt.constructor.name;
		}
	}
	
	generateExpression(expr: Expression): IrAddr {
		if (expr instanceof NumberLiteral) {
			let dest = IrAddr.stack(this.scope.stackOffset);
			this.addIr(new IrMove(expr.value, dest));
			this.scope.stackOffset += typeSize(Types.Int);
			return dest;
		} else if (expr instanceof HighIrOperation) {
			switch (expr.operation) {
				case Operation.Add:
				case Operation.Sub:
				case Operation.Mul:
				case Operation.Div:
				case Operation.Mod: {
					let start = this.scope.stackOffset;
					let dest = IrAddr.stack(this.scope.stackOffset);
					
					let regs = this.regsUsed;
					let left = this.generateExpression(expr.args[0]);
					let right = this.generateExpression(expr.args[1]);
					this.regsUsed = regs;
					
					switch (expr.operation) {
						case Operation.Add: this.addIr(new IrAdd(left, right, dest)); break;
						case Operation.Sub: this.addIr(new IrSub(left, right, dest)); break;
						case Operation.Mul: this.addIr(new IrMul(left, right, dest)); break;
						case Operation.Div: this.addIr(new IrDiv(left, right, dest)); break;
						case Operation.Mod: this.addIr(new IrMod(left, right, dest)); break;
					}
					
					this.scope.stackOffset = start + typeSize(Types.Int);
					return dest;
				}
				case Operation.Lt:
				case Operation.LtEq:
				case Operation.Gt:
				case Operation.GtEq:
				case Operation.Eq: {
					let start = this.scope.stackOffset;
					let dest = IrAddr.stack(this.scope.stackOffset);
					
					let regs = this.regsUsed;
					let left = this.generateExpression(expr.args[0]);
					let right = this.generateExpression(expr.args[1]);
					this.regsUsed = regs;
					
					switch (expr.operation) {
						case Operation.Lt: this.addIr(new IrLt(left, right, dest)); break;
						case Operation.LtEq: this.addIr(new IrLeq(left, right, dest)); break;
						case Operation.Gt: this.addIr(new IrLt(right, left, dest)); break;
						case Operation.GtEq: this.addIr(new IrLeq(right, left, dest)); break;
						case Operation.Eq: this.addIr(new IrEq(left, right, dest)); break;
					}
					
					this.scope.stackOffset = start + typeSize(Types.Bool);
					return dest;
				}
				case Operation.Deref: {
					// Check if stack value
					let arg0 = expr.args[0];
					if (
						arg0 instanceof HighIrOperation &&
						(arg0.operation === Operation.Add || arg0.operation === Operation.Sub) &&
						arg0.args[0] instanceof StackPointer &&
						arg0.args[1] instanceof NumberLiteral
					) {
						let sign = 1;
						if (arg0.operation === Operation.Sub) sign = -1;
						return IrAddr.stack(sign * (arg0.args[1] as NumberLiteral).value);
					} else if (
						arg0 instanceof StackPointer
					) {
						return IrAddr.stack(0);
					} else if (
						arg0 instanceof NumberLiteral
					) {
						return IrAddr.absolute(arg0.value);
					}
					
					let regs = this.regsUsed;
					let value = this.generateExpression(expr.args[0]);
					this.regsUsed = regs;
					
					let reg = this.useReg();
					this.addIr(new IrMove(value, IrAddr.reg(reg)));
					return IrAddr.regRef(reg);
				}
				default:
					throw "Unknown operation " + Operation[expr.operation];
			}
		} else if (expr instanceof HighIrAssign) {
			let regs = this.regsUsed;
			let dest = this.generateExpression(expr.dst);
			let value = this.generateExpression(expr.val);
			this.regsUsed = regs;
			let out = dest;
			
			if (expr.post) {
				out = IrAddr.stack(this.scope.stackOffset);
				this.scope.stackOffset += typeSize(Types.Int);
				this.addIr(new IrMove(dest, out));
			}
			
			if (expr.compound === null) {
				this.addIr(new IrMove(value, dest));
			} else {
				switch (expr.compound) {
					case Operation.Add: this.addIr(new IrAdd(dest, value, dest)); break;
					case Operation.Sub: this.addIr(new IrSub(dest, value, dest)); break;
					case Operation.Mul: this.addIr(new IrMul(dest, value, dest)); break;
					case Operation.Div: this.addIr(new IrDiv(dest, value, dest)); break;
					case Operation.Mod: this.addIr(new IrMod(dest, value, dest)); break;
				}
			}
			
			return out;
		} else if (expr instanceof CallExpression) {
			// Return value
			let retVal = IrAddr.stack(this.scope.stackOffset);
			this.scope.stackOffset += typeSize((expr.callee.type as FunctionType).ret);
			let stackRestore = this.scope.stackOffset;
			
			// Return address
			let retLab = new IrLabel;
			this.addIr(new IrMove(retLab, IrAddr.stack(this.scope.stackOffset++)));
			
			// Arguments
			expr.args.forEach(arg => {
				//console.log("arg", arg);
				let stack = this.scope.stackOffset;
				let dest = IrAddr.stack(stack);
				let regs = this.regsUsed;
				this.addIr(new IrMove(this.generateExpression(arg), dest));
				this.regsUsed = regs;
				this.scope.stackOffset = stack + typeSize(arg.type as Type);
			});
			
			let stackSize = this.scope.stackOffset;
			
			let regs = this.regsUsed;
			let callee = this.generateExpression(expr.callee);
			this.regsUsed = regs;
			
			this.addIr(new IrCall(callee, stackSize, retLab));
			
			this.scope.stackOffset = stackRestore;
			return retVal;
		} else if (expr instanceof LabelValue) {
			let dest = IrAddr.stack(this.scope.stackOffset);
			this.addIr(new IrMove(this.resolveLabel(expr), dest));
			this.scope.stackOffset += typeSize(Types.Int);
			return dest;
		} else if (expr instanceof StackPointer) {
			return IrAddr.absolute(0);
		} else {
			throw "Unknown expression " + expr.constructor.name;
		}
	}
	
	generate(globals: DeclarationStatement[], externs: ExternDeclaration[], funs: FunctionDefinition[]) {
		// Entry procedure
		// Stack pointer is inited later on
		
		let globSize = 0;
		globals.forEach(global => {
			if (global.init) {
				let reg = this.generateExpression(global.init);
				this.addIr(new IrMove(reg, IrAddr.absolute(globSize + 1)));
			}
			globSize += typeSize(global.type as Type);
		});
		this.regsUsed = globSize;
		this.maxRegs = globSize;
		
		let retLabel = new IrLabel;
		this.addIr(new IrMove(retLabel, IrAddr.stack(-1)));
		this.addIr(new IrJump(this.resolveLabel((this.scope.resolve("main") as ScopedValue).value)));
		this.addIr(retLabel);
		this.addIr(new IrHalt);
		
		// Externs
		externs.forEach(extern => {
			let stackOffset = typeSize(extern.type.ret);
			extern.type.args.forEach(arg => {
				if (arg instanceof NamedArgument) arg = arg.type;
				stackOffset += typeSize(arg);
			});
			stackOffset += 1;
			
			this.addIr(this.resolveLabel((this.scope.resolve(extern.name) as ScopedValue).value));
			this.addIr(new IrExtern(extern.name));
			this.addIr(new IrJump(IrAddr.stack(-stackOffset)));
		});
		
		funs.forEach(fun => {
			this.generateFunction(fun);
		});
		
		// Stack pointer
		this.code.unshift(new IrMove(1 + this.maxRegs + 1, IrAddr.stackPointer()));
		
		return {ir: this.code, data: this.data};
	}
}

/*class IrGenerator2 {
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
}*/

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
				let addr: IrAddr | IrLabel = ins.callee;
				if (addr.base === 0) addr.offset -= ins.stackSize;
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
