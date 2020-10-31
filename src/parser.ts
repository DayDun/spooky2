import {
	Type,
	TypeName,
	FunctionType,
	NamedArgument,
	PointerType,
	ArrayType,
	Types,
} from "./types";
import {
	TokenType, Token, EOF_TOKEN
} from "./lexer";

export abstract class AstNode {}

export class Program extends AstNode {
	body: AstNode[];
	
	constructor(body: AstNode[]) {
		super();
		this.body = body;
	}
}

export class ExternDeclaration extends AstNode {
	name: string;
	type: FunctionType;
	
	constructor(name: string, type: FunctionType) {
		super();
		this.name = name;
		this.type = type;
	}
}
export class FunctionDefinition extends AstNode {
	name: string;
	type: FunctionType;
	body: BlockStatement;
	
	constructor(name: string, type: FunctionType, body: BlockStatement) {
		super();
		this.name = name;
		this.type = type;
		this.body = body;
	}
}
export class StructDefinition extends AstNode {
	name: string;
	fields: DeclarationStatement[];
	
	constructor(name: string, fields: DeclarationStatement[]) {
		super();
		this.name = name;
		this.fields = fields;
	}
}

export abstract class Statement extends AstNode {
	abstract toString(indent: number): string;
}
export class EmptyStatement extends Statement {
	toString() { return ""; }
} // ;
export class LabelStatement extends Statement {
	name: string;
	
	constructor(name: string) {
		super();
		this.name = name;
	}
	
	toString() {
		return `:${this.name}:`;
	}
}

export class BlockStatement extends Statement {
	body: Statement[];
	
	constructor(body: Statement[]) {
		super();
		this.body = body;
	}
	
	toString(indent: number = 0) {
		let out = "{\n";
		this.body.forEach(stmt => {
			out += "\t".repeat(indent + 1) + `${stmt.toString(indent + 1)}\n`;
		});
		out += "\t".repeat(indent) + "}";
		return out;
	}
}
export class IfStatement extends Statement {
	test: Expression;
	consequent: Statement;
	alternate: Statement | null;
	
	constructor(test: Expression, consequent: Statement, alternate: Statement | null) {
		super();
		this.test = test;
		this.consequent = consequent;
		this.alternate = alternate;
	}
	
	toString(indent: number) {
		let out = `if (${this.test}) ${this.consequent.toString(indent)}`;
		if (this.alternate) out += ` else ${this.alternate.toString(indent)}`
		return out;
	}
}
export class WhileStatement extends Statement {
	test: Expression;
	body: Statement;
	
	constructor(test: Expression, body: Statement) {
		super();
		this.test = test;
		this.body = body;
	}
	
	toString(indent: number): string {
		return `while (${this.test}) ${this.body.toString(indent)}`;
	}
}
export class DoWhileStatement extends Statement {
	body: Statement;
	test: Expression;
	
	constructor(body: Statement, test: Expression) {
		super();
		this.body = body;
		this.test = test;
	}
	
	toString(indent: number) {
		return `do ${this.body.toString(indent)} while (${this.test});`;
	}
}
export class ForStatement extends Statement {
	//init: DeclarationStatement | ExpressionStatement | null;
	init: Statement | null;
	test: Expression | null;
	inc: Expression | null;
	body: Statement;
	
	constructor(
		init: Statement | null,
		test: Expression | null,
		inc: Expression | null,
		body: Statement
	) {
		super();
		this.init = init;
		this.test = test;
		this.inc = inc;
		this.body = body;
	}
	
	toString(indent: number) {
		return `for (${this.init}${this.test || ""};${this.inc || ""}) ${this.body.toString(indent)}`;
	}
}
export class BreakStatement extends Statement {
	label: string | null;
	
	constructor(label: string | null) {
		super();
		this.label = label;
	}
	
	toString() {
		if (!this.label) return `break;`;
		return `break ${this.label};`;
	}
}
export class ContinueStatement extends Statement {
	label: string | null;
	
	constructor(label: string | null) {
		super();
		this.label = label;
	}
	
	toString() {
		if (!this.label) return `continue;`;
		return `continue ${this.label};`;
	}
}
export class GotoStatement extends Statement {
	label: string;
	
	constructor(label: string) {
		super();
		this.label = label;
	}
	
	toString() {
		return `goto ${this.label};`;
	}
}
export class ReturnStatement extends Statement {
	value: Expression | null;
	
	constructor(value: Expression | null) {
		super();
		this.value = value;
	}
	
	toString() {
		if (!this.value) return "return;"
		return `return ${this.value};`;
	}
}
export class HaltStatement extends Statement {
	toString() { return "halt;"; }
}
export class DeclarationStatement extends Statement {
	name: string;
	type: Type | null;
	init: Expression | null;
	constant: boolean;
	
	constructor(name: string, type: Type | null, init: Expression | null, constant: boolean) {
		super();
		this.name = name;
		this.type = type;
		this.init = init;
		this.constant = constant;
	}
	
	toString() {
		if (!this.type) return `${this.name} := ${this.init};`;
		return `${this.name}: ${this.type} = ${this.init};`;
	}
}
export class ExpressionStatement extends Statement {
	expression: Expression;
	
	constructor(expression: Expression) {
		super();
		this.expression = expression;
	}
	
	toString() {
		return `${this.expression};`;
	}
}

/*export enum Operator {
	PostInc,
	PostDec,
	PreInc,
	PreDec
}*/

export abstract class Expression extends AstNode {
	type: Type | null = null;
}
// TODO: perfix is ugly, operator should maybe be a new enum
export class UnaryExpression extends Expression {
	operator: Token;
	operand: Expression;
	prefix: boolean;
	
	constructor(operator: Token, operand: Expression, prefix: boolean) {
		super();
		this.operator = operator;
		this.operand = operand;
		this.prefix = prefix;
	}
	
	toString() {
		if (this.prefix) return `(${this.operator.value}${this.operand})`;
		return `(${this.operand}${this.operator.value})`;
	}
}
export class BinaryExpression extends Expression {
	left: Expression;
	operator: Token;
	right: Expression;
	
	constructor(left: Expression, operator: Token, right: Expression) {
		super();
		this.left = left;
		this.operator = operator;
		this.right = right;
	}
	
	toString() {
		return `(${this.left} ${this.operator.value} ${this.right})`;
	}
}
export class CallExpression extends Expression {
	callee: Expression;
	args: Expression[];
	
	constructor(callee: Expression, args: Expression[]) {
		super();
		this.callee = callee;
		this.args = args;
	}
	
	toString() {
		return `${this.callee}(${this.args.join(", ")})`;
	}
}
export class SubscriptExpression extends Expression {
	primary: Expression;
	secondary: Expression;
	
	constructor(primary: Expression, secondary: Expression) {
		super();
		this.primary = primary;
		this.secondary = secondary;
	}
	
	toString() {
		return `${this.primary}[${this.secondary}]`;
	}
}

export class Identifier extends Expression {
	name: string;
	
	constructor(name: string) {
		super();
		this.name = name;
	}
	
	toString() {
		return this.name;
	}
}
export class NumberLiteral extends Expression {
	value: number;
	
	constructor(value: number) {
		super();
		this.value = value;
	}
	
	toString() {
		return this.value.toString();
	}
}
export class CharLiteral extends Expression {
	value: string;
	
	constructor(value: string) {
		super();
		this.value = value;
	}
	
	toString() {
		return `'${this.value}'`;
	}
}
export class StringLiteral extends Expression {
	value: string;
	
	constructor(value: string) {
		super();
		this.value = value;
	}
	
	toString() {
		return JSON.stringify(this.value);
	}
}
export class ArrayLiteral extends Expression {
	value: Expression[];
	
	constructor(value: Expression[]) {
		super();
		this.value = value;
	}
	
	toString() {
		return `[${this.value.join(", ")}]`;
	}
}

// Precedence & Associativity
function getPrefixPrecedence(operator: TokenType): number | null {
	switch (operator) {
		case TokenType.Plus:
		case TokenType.Minus:
		case TokenType.PlusPlus:
		case TokenType.MinusMinus:
		case TokenType.Tilde:
		case TokenType.Bang:
		case TokenType.Star:
		case TokenType.Amp:
			return 13;
		default:
			return null;
	}
}
function getPostfixPrecedence(operator: TokenType): number | null {
	switch (operator) {
		case TokenType.PlusPlus:
		case TokenType.MinusMinus:
		case TokenType.ParenL:
		case TokenType.BracketL:
			return 14;
		default:
			return null;
	}
}
function getInfixPrecedence(operator: TokenType): number | null {
	switch (operator) {
		case TokenType.Dot:
		case TokenType.Arrow:
			return 15;
		case TokenType.Star:
		case TokenType.Slash:
		case TokenType.Percent:
			return 12;
		case TokenType.Plus:
		case TokenType.Minus:
			return 11;
		case TokenType.LtLt:
		case TokenType.GtGt:
			return 10;
		case TokenType.Lt:
		case TokenType.Gt:
		case TokenType.LtEq:
		case TokenType.GtEq:
			return 9;
		case TokenType.EqEq:
		case TokenType.BangEq:
			return 8;
		case TokenType.Amp:
			return 7;
		case TokenType.Caret:
			return 6;
		case TokenType.Pipe:
			return 5;
		case TokenType.AmpAmp:
			return 4;
		case TokenType.PipePipe:
			return 3;
		case TokenType.Question:
		case TokenType.Colon:
			return 2;
		case TokenType.Eq:
		case TokenType.PlusEq:
		case TokenType.MinusEq:
		case TokenType.StarEq:
		case TokenType.SlashEq:
		case TokenType.PercentEq:
		case TokenType.LtLtEq:
		case TokenType.GtGtEq:
		case TokenType.PipeEq:
		case TokenType.AmpEq:
		case TokenType.CaretEq:
			return 1;
		default:
			return null;
	}
}
enum Associativity {
	Left,
	Right
}
function getAssociativity(operator: TokenType): Associativity | null {
	switch (operator) {
		case TokenType.Dot:
		case TokenType.Arrow:
		case TokenType.Star:
		case TokenType.Slash:
		case TokenType.Percent:
		case TokenType.Plus:
		case TokenType.Minus:
		case TokenType.LtLt:
		case TokenType.GtGt:
		case TokenType.Lt:
		case TokenType.Gt:
		case TokenType.LtEq:
		case TokenType.GtEq:
		case TokenType.EqEq:
		case TokenType.BangEq:
		case TokenType.Amp:
		case TokenType.Caret:
		case TokenType.Pipe:
		case TokenType.AmpAmp:
		case TokenType.PipePipe:
			return Associativity.Left;
		case TokenType.Question:
		case TokenType.Colon:
		case TokenType.Eq:
		case TokenType.PlusEq:
		case TokenType.MinusEq:
		case TokenType.StarEq:
		case TokenType.SlashEq:
		case TokenType.PercentEq:
		case TokenType.LtLtEq:
		case TokenType.GtGtEq:
		case TokenType.PipeEq:
		case TokenType.AmpEq:
		case TokenType.CaretEq:
			return Associativity.Right;
		default:
			return null;
	}
}

export class Parser {
	tokens: Token[];
	pos: number = 0;
	
	constructor(tokens: Token[]) {
		this.tokens = tokens;
	}
	
	peekToken(offset: number = 0): Token {
		let token = this.tokens[this.pos + offset];
		return token || EOF_TOKEN;
	}
	eatToken(): Token {
		return this.tokens[this.pos++];
	}
	expect(tokenType: TokenType): Token | never {
		let token = this.eatToken();
		if (token.type !== tokenType) {
			throw "Expected " + TokenType[tokenType] + ", got " + TokenType[token.type];
		}
		return token;
	}
	
	parseType(): Type {
		let t = this.peekToken().type;
		
		if (t === TokenType.ParenL) {
			return this.parseFunctionType();
		} else if (t === TokenType.Star) {
			this.pos++;
			return new PointerType(this.parseType());
		} else if (t === TokenType.BracketL) {
			return this.parseArrayType();
		}
		
		let name = this.expect(TokenType.Identifier).value as string;
		//return resolveTypeName(name);
		return new TypeName(name);
	}
	parseArrayType(): ArrayType {
		this.expect(TokenType.BracketL);
		let length = this.expect(TokenType.Number).value as number;
		this.expect(TokenType.BracketR);
		return new ArrayType(this.parseType(), length);
	}
	parseFunctionType(): FunctionType {
		this.expect(TokenType.ParenL);
		let args = [];
		while (this.pos < this.tokens.length) {
			if (this.peekToken().type === TokenType.ParenR) break;
			
			if (this.peekToken(1).type === TokenType.Colon) {
				let name = this.expect(TokenType.Identifier).value as string;
				this.expect(TokenType.Colon);
				let type = this.parseType();
				
				args.push(new NamedArgument(name, type));
			} else {
				let type = this.parseType();
				args.push(type);
			}
			
			if (this.peekToken().type !== TokenType.Comma) break;
			this.expect(TokenType.Comma);
		}
		this.expect(TokenType.ParenR);
		
		let ret = Types.Void;
		if (this.peekToken().type === TokenType.Arrow) {
			this.pos++;
			ret = this.parseType();
		}
		
		return new FunctionType(args, ret);
	}
	
	// Quite big, ugly and hacky
	parseExpression(parentPrecedence: number = 0): Expression {
		let left;
		
		let prefixPrecedence = getPrefixPrecedence(this.peekToken().type);
		if (prefixPrecedence !== null && parentPrecedence <= prefixPrecedence) {
			let operator = this.eatToken();
			let operand = this.parseExpression(prefixPrecedence);
			left = new UnaryExpression(operator, operand, true);
		} else {
			switch (this.peekToken().type) {
				case TokenType.ParenL:
					this.expect(TokenType.ParenL);
					left = this.parseExpression();
					this.expect(TokenType.ParenR);
					break;
				case TokenType.Identifier:
					left = new Identifier(this.expect(TokenType.Identifier).value as string);
					break;
				case TokenType.Number:
					left = new NumberLiteral(this.expect(TokenType.Number).value as number);
					break;
				case TokenType.Char:
					left = new CharLiteral(this.expect(TokenType.Char).value as string);
					break;
				case TokenType.String:
					left = new StringLiteral(this.expect(TokenType.String).value as string);
					break;
				default:
					throw "Not an expression";
			}
		}
		
		while (true) {
			while (true) {
				let infixPrecedence = getInfixPrecedence(this.peekToken().type);
				let associativity = getAssociativity(this.peekToken().type);
				if (infixPrecedence === null) break;
				if (
					associativity === Associativity.Left && infixPrecedence <= parentPrecedence ||
					associativity === Associativity.Right && infixPrecedence < parentPrecedence
				) break;
				
				let operator = this.eatToken();
				let right = this.parseExpression(infixPrecedence);
				left = new BinaryExpression(left, operator, right);
			}
			
			while (true) {
				let postfixPrecedence = getPostfixPrecedence(this.peekToken().type);
				if (postfixPrecedence === null || parentPrecedence >= postfixPrecedence) break;
				
				let operator = this.eatToken();
				
				if (operator.type === TokenType.ParenL) {
					//this.expect(TokenType.ParenL);
					let args = [];
					while (true) {
						if (this.peekToken().type === TokenType.ParenR) break;
						args.push(this.parseExpression());
						if (this.peekToken().type !== TokenType.Comma) break;
						this.expect(TokenType.Comma);
					}
					this.expect(TokenType.ParenR);
					left = new CallExpression(left, args);
				} else if (operator.type === TokenType.BracketL) {
					//this.expect(TokenType.BracketL);
					let secondary = this.parseExpression();
					this.expect(TokenType.BracketR);
					left = new SubscriptExpression(left, secondary);
				} else {
					left = new UnaryExpression(operator, left, false);
				}
			}
			
			let infixPrecedence = getInfixPrecedence(this.peekToken().type);
			let associativity = getAssociativity(this.peekToken().type);
			if (infixPrecedence === null) break;
			if (
				associativity === Associativity.Left && infixPrecedence <= parentPrecedence ||
				associativity === Associativity.Right && infixPrecedence < parentPrecedence
			) break;
		}
		
		return left;
	}
	
	parseExtern(): ExternDeclaration {
		this.expect(TokenType.Keyword_Extern);
		let name = this.expect(TokenType.Identifier).value as string;
		let type = this.parseFunctionType();
		return new ExternDeclaration(name, type);
	}
	parseFunctionDefinition(): FunctionDefinition {
		this.expect(TokenType.Keyword_Func);
		let name = this.expect(TokenType.Identifier).value as string;
		let type = this.parseFunctionType();
		let body = this.parseBlock();
		return new FunctionDefinition(name, type, body);
	}
	parseStructDefinition(): StructDefinition {
		this.expect(TokenType.Keyword_Struct);
		let name = this.expect(TokenType.Identifier).value as string;
		this.expect(TokenType.BraceL);
		let fields = [];
		while (this.pos < this.tokens.length) {
			if (this.peekToken().type === TokenType.BraceR) break;
			
			fields.push(this.parseDeclarationStatement());
		}
		this.expect(TokenType.BraceR);
		
		return new StructDefinition(name, fields);
	}
	
	parseBlock(): BlockStatement {
		this.expect(TokenType.BraceL);
		let body = [];
		while (this.pos < this.tokens.length) {
			if (this.peekToken().type === TokenType.BraceR) break;
			
			body.push(this.parseStatement());
		}
		this.expect(TokenType.BraceR);
		
		return new BlockStatement(body);
	}
	parseLabel(): LabelStatement {
		//this.expect(TokenType.At);
		this.expect(TokenType.Colon);
		let name = this.expect(TokenType.Identifier).value as string;
		this.expect(TokenType.Colon);
		
		return new LabelStatement(name);
	}
	
	parseStatement(): Statement {
		switch (this.peekToken().type) {
			case TokenType.Semi:
				this.pos++;
				return new EmptyStatement();
			case TokenType.BraceL:
				return this.parseBlock();
			case TokenType.Keyword_If:
				return this.parseIfStatement();
			case TokenType.Keyword_While:
				return this.parseWhileStatement();
			case TokenType.Keyword_Do:
				return this.parseDoWhileStatement();
			case TokenType.Keyword_For:
				return this.parseForStatement();
			case TokenType.Keyword_Break:
				return this.parseBreakStatement();
			case TokenType.Keyword_Continue:
				return this.parseContinueStatement();
			case TokenType.Keyword_Goto:
				return this.parseGotoStatement();
			case TokenType.Keyword_Return:
				return this.parseReturnStatement();
			case TokenType.Keyword_Halt:
				return this.parseHaltStatement();
			//case TokenType.At:
			case TokenType.Colon:
				return this.parseLabel();
			case TokenType.Identifier:
				if (this.peekToken(1).type === TokenType.Colon) {
					return this.parseDeclarationStatement();
				}
			default:
				return this.parseExpressionStatement();
		}
	}
	parseIfStatement(): IfStatement {
		this.expect(TokenType.Keyword_If);
		let test = this.parseExpression();
		let consequent = this.parseStatement();
		let alternate = null;
		if (this.peekToken().type === TokenType.Keyword_Else) {
			this.expect(TokenType.Keyword_Else);
			alternate = this.parseStatement();
		}
		
		return new IfStatement(test, consequent, alternate);
	}
	parseWhileStatement(): WhileStatement {
		this.expect(TokenType.Keyword_While);
		let test = this.parseExpression();
		let body = this.parseStatement();
		return new WhileStatement(test, body);
	}
	parseDoWhileStatement(): DoWhileStatement {
		this.expect(TokenType.Keyword_Do);
		let body = this.parseStatement();
		this.expect(TokenType.Keyword_While);
		let test = this.parseExpression();
		this.expect(TokenType.Semi);
		return new DoWhileStatement(body, test);
	}
	parseForStatement(): ForStatement {
		this.expect(TokenType.Keyword_For);
		this.expect(TokenType.ParenL);
		let init = this.parseStatement(); // TODO: Kinda retarded
		let test = null;
		if (this.peekToken().type !== TokenType.Semi) {
			test = this.parseExpression();
		}
		this.expect(TokenType.Semi);
		let inc = null;
		if (this.peekToken().type !== TokenType.ParenR) {
			inc = this.parseExpression();
		}
		this.expect(TokenType.ParenR);
		
		let body = this.parseStatement();
		
		return new ForStatement(init, test, inc, body);
	}
	parseBreakStatement(): BreakStatement {
		this.expect(TokenType.Keyword_Break);
		let label = null;
		if (this.peekToken().type === TokenType.Identifier) {
			label = this.expect(TokenType.Identifier).value as string;
		}
		this.expect(TokenType.Semi);
		
		return new BreakStatement(label);
	}
	parseContinueStatement(): ContinueStatement {
		this.expect(TokenType.Keyword_Continue);
		let label = null;
		if (this.peekToken().type === TokenType.Identifier) {
			label = this.expect(TokenType.Identifier).value as string;
		}
		this.expect(TokenType.Semi);
		
		return new ContinueStatement(label);
	}
	parseGotoStatement(): GotoStatement {
		this.expect(TokenType.Keyword_Goto);
		let label = this.expect(TokenType.Identifier).value as string;
		this.expect(TokenType.Semi);
		
		return new GotoStatement(label);
	}
	parseReturnStatement(): ReturnStatement {
		this.expect(TokenType.Keyword_Return);
		let value = null;
		if (this.peekToken().type !== TokenType.Semi) {
			value = this.parseExpression();
		}
		this.expect(TokenType.Semi);
		
		return new ReturnStatement(value);
	}
	parseHaltStatement(): HaltStatement {
		this.expect(TokenType.Keyword_Halt);
		this.expect(TokenType.Semi);
		return new HaltStatement();
	}
	parseDeclarationStatement(): DeclarationStatement {
		let name = this.expect(TokenType.Identifier).value as string;
		this.expect(TokenType.Colon);
		let type = null;
		if (![TokenType.Eq, TokenType.Colon].includes(this.peekToken().type)) {
			type = this.parseType();
		}
		let init = null;
		let constant = false;
		if (this.peekToken().type === TokenType.Eq) {
			this.expect(TokenType.Eq);
			init = this.parseExpression();
		} else if (this.peekToken().type === TokenType.Colon) {
			constant = true;
			this.expect(TokenType.Colon);
			init = this.parseExpression();
		}
		this.expect(TokenType.Semi);
		
		return new DeclarationStatement(name, type, init, constant);
	}
	parseExpressionStatement(): ExpressionStatement {
		let expression = this.parseExpression();
		this.expect(TokenType.Semi);
		
		return new ExpressionStatement(expression);
	}
	
	parseProgramStatement(): AstNode {
		switch (this.peekToken().type) {
			case TokenType.Keyword_Extern:
				return this.parseExtern();
			case TokenType.Keyword_Func:
				return this.parseFunctionDefinition();
			case TokenType.Keyword_Struct:
				return this.parseStructDefinition();
			case TokenType.Identifier:
				return this.parseDeclarationStatement();
			default:
				throw "Unexpected token " + this.peekToken();
		}
	}
	
	parseProgram(): Program {
		let body = [];
		while (this.pos < this.tokens.length) {
			body.push(this.parseProgramStatement());
		}
		
		return new Program(body);
	}
}
