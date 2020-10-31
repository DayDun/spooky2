export enum TokenType {
	EOF,
	
	Dot,      // .
	Comma,    // ,
	Semi,     // ;
	Colon,    // :
	Question, // ?
	Arrow,    // ->
	At,       // @
	
	ParenL,   // (
	ParenR,   // )
	BracketL, // [
	BracketR, // ]
	BraceL,   // {
	BraceR,   // }
	
	Plus,    // +
	Minus,   // -
	Star,    // *
	Slash,   // /
	Percent, // %
	Pipe,    // |
	Amp,     // &
	Caret,   // ^
	Tilde,   // ~
	Bang,    // !
	Lt,      // <
	Gt,      // >
	
	LtLt,       // <<
	GtGt,       // >>
	PipePipe,   // ||
	AmpAmp,     // &&
	PlusPlus,   // ++
	MinusMinus, // --
	
	Eq,        // =
	PlusEq,    // +=
	MinusEq,   // -=
	StarEq,    // *=
	SlashEq,   // /=
	PercentEq, // %=
	PipeEq,    // |=
	AmpEq,     // &=
	CaretEq,   // ^=
	LtLtEq,    // <<=
	GtGtEq,    // >>=
	
	EqEq,   // ==
	BangEq, // !=
	LtEq,   // <=
	GtEq,   // >=
	
	Identifier,
	Number,
	Char,
	String,
	
	Keyword_If,
	Keyword_Else,
	Keyword_For,
	Keyword_While,
	Keyword_Do,
	Keyword_Break,
	Keyword_Continue,
	Keyword_Goto,
	Keyword_Return,
	
	Keyword_Extern,
	Keyword_Func,
	Keyword_Struct,
	Keyword_Halt
};

export class Token {
	type: TokenType;
	value: string | number;
	
	constructor(type: TokenType, value: string | number) {
		this.type = type;
		this.value = value;
	}
	
	toString(): string {
		if (this.type === TokenType.Identifier) return `<Identifier ${this.value}>`;
		if (this.type === TokenType.Number) return `<Number ${this.value}>`;
		return `<Token ${this.value} >`
	}
}

export const EOF_TOKEN = new Token(TokenType.EOF, "<EOF>");

export class Lexer {
	text: string;
	pos: number = 0;
	
	constructor(text: string) {
		this.text = text;
	}
	
	peekChar(offset: number = 0): string {
		return this.text.charAt(this.pos + offset);
	}
	
	eatChar(): string {
		return this.text.charAt(this.pos++);
	}
	
	skipSpace(): void {
		while (this.pos < this.text.length) {
			if (
				this.peekChar() === " " ||
				this.peekChar() === "\t" ||
				this.peekChar() === "\n" ||
				this.peekChar() === "\r"
			) {
				this.pos++;
			} else if (this.peekChar() === "/") {
				if (this.peekChar(1) === "/") {
					this.skipLineComment();
				} else if (this.peekChar(1) === "*") {
					this.skipBlockComment();
				} else {
					break;
				}
			} else {
				break;
			}
		}
	}
	skipLineComment(): void {
		this.pos += 2;
		while (this.pos < this.text.length) {
			if (this.peekChar() === "\n" || this.peekChar() === "\r") break;
			this.pos++;
		}
	}
	skipBlockComment(): void {
		this.pos += 2;
		while (this.pos < this.text.length) {
			if (this.eatChar() === "*") {
				if (this.eatChar() === "/") break;
			}
		}
	}
	
	readInt(radix: number, length: number = Infinity): number {
		let num = 0;
		
		for (let i=0; i<length; i++) {
			let digit = this.peekChar().charCodeAt(0);
			if (digit >= "a".charCodeAt(0) && digit <= "z".charCodeAt(0)) {
				digit = digit - "a".charCodeAt(0) + 10;
			} else if (digit >= "A".charCodeAt(0) && digit <= "Z".charCodeAt(0)) {
				digit = digit - "A".charCodeAt(0) + 10;
			} else if (digit >= "0".charCodeAt(0) && digit <= "9".charCodeAt(0)) {
				digit = digit - "0".charCodeAt(0);
			} else {
				digit = Infinity;
			}
			
			if (digit >= radix) break;
			
			num = num * radix + digit;
			this.pos++;
		}
		return num;
	}
	readEscapeSequence(): string {
		let c = this.eatChar();
		
		if (c === "a") return "\x07";
		if (c === "b") return "\b";
		if (c === "t") return "\t";
		if (c === "n") return "\n";
		if (c === "v") return "\v";
		if (c === "f") return "\f";
		if (c === "r") return "\r";
		if (c === "e") return "\x1b";
		if (c === "x") return String.fromCharCode(this.readInt(16, 2));
		if (c === "u") return String.fromCodePoint(this.readInt(16, 4));
		if (c.charCodeAt(0) >= "0".charCodeAt(0) && c.charCodeAt(0) < "9".charCodeAt(0)) {
			return String.fromCharCode(this.readInt(8, 3));
		}
		return c;
	}
	
	readNumber(): Token {
		if (this.peekChar() === "0") {
			if (this.peekChar(1) === "x" || this.peekChar(1) === "X") {
				this.pos += 2;
				return new Token(TokenType.Number, this.readInt(16));
			} else if (this.peekChar(1) === "o" || this.peekChar(1) === "O") {
				this.pos += 2;
				return new Token(TokenType.Number, this.readInt(8));
			} else if (this.peekChar(1) === "b" || this.peekChar(1) === "B") {
				this.pos += 2;
				return new Token(TokenType.Number, this.readInt(2));
			}
		}
		
		return new Token(TokenType.Number, this.readInt(10));
	}
	readChar(): Token {
		if (this.eatChar() !== "'") throw "Invalid char start";
		
		let value;
		let c = this.eatChar();
		if (c === "\\") {
			value = this.readEscapeSequence();
		} else {
			value = c;
		}
		
		if (this.eatChar() !== "'") throw "Invalid char end";
		
		return new Token(TokenType.Char, value);
	}
	readString(): Token {
		if (this.eatChar() !== "\"") throw "Invalid string start";
		
		let value = "";
		while (true) {
			if (this.pos >= this.text.length) throw "Unterminated string literal";
			
			let c = this.eatChar();
			if (c === "\"") break;
			if (c === "\\") {
				value += this.readEscapeSequence();
			} else {
				value += c;
			}
		}
		
		return new Token(TokenType.String, value);
	}
	readIdentifier(): Token {
		let name = "";
		while (this.pos < this.text.length) {
			let c = this.peekChar().charCodeAt(0);
			
			if (
				(c >= "0".charCodeAt(0) && c <= "9".charCodeAt(0)) ||
				(c >= "A".charCodeAt(0) && c <= "Z".charCodeAt(0)) ||
				(c >= "a".charCodeAt(0) && c <= "z".charCodeAt(0)) ||
				c === "_".charCodeAt(0)
			) {
				name += this.eatChar();
			} else {
				break;
			}
		}
		
		switch (name) {
			case "if": return new Token(TokenType.Keyword_If, "if");
			case "else": return new Token(TokenType.Keyword_Else, "else");
			case "for": return new Token(TokenType.Keyword_For, "for");
			case "while": return new Token(TokenType.Keyword_While, "while");
			case "do": return new Token(TokenType.Keyword_Do, "do");
			case "break": return new Token(TokenType.Keyword_Break, "break");
			case "continue": return new Token(TokenType.Keyword_Continue, "continue");
			case "goto": return new Token(TokenType.Keyword_Goto, "goto");
			case "return": return new Token(TokenType.Keyword_Return, "return");
			case "extern": return new Token(TokenType.Keyword_Extern, "extern");
			case "func": return new Token(TokenType.Keyword_Func, "func");
			case "struct": return new Token(TokenType.Keyword_Struct, "struct");
			case "halt": return new Token(TokenType.Keyword_Halt, "halt");
			default: return new Token(TokenType.Identifier, name);
		}
	}
	
	nextToken(): Token {
		this.skipSpace();
		
		if (this.pos >= this.text.length) {
			return EOF_TOKEN;
		}
		
		let c = this.peekChar();
		
		// String
		if (c === "\"") {
			return this.readString();
		}
		
		// Char
		if (c === "'") {
			return this.readChar();
		}
		
		if (c === ".") return this.pos++, new Token(TokenType.Dot, ".");
		if (c === ",") return this.pos++, new Token(TokenType.Comma, ",");
		if (c === ";") return this.pos++, new Token(TokenType.Semi, ";");
		if (c === ":") return this.pos++, new Token(TokenType.Colon, ":");
		if (c === "?") return this.pos++, new Token(TokenType.Question, "?");
		if (c === "@") return this.pos++, new Token(TokenType.At, "@");
			
		if (c === "(") return this.pos++, new Token(TokenType.ParenL, "(");
		if (c === ")") return this.pos++, new Token(TokenType.ParenR, ")");
		if (c === "[") return this.pos++, new Token(TokenType.BracketL, "[");
		if (c === "]") return this.pos++, new Token(TokenType.BracketR, "]");
		if (c === "{") return this.pos++, new Token(TokenType.BraceL, "{");
		if (c === "}") return this.pos++, new Token(TokenType.BraceR, "}");
			
		if (c === "~") return this.pos++, new Token(TokenType.Tilde, "~");
		
		if (c === "+") {
			c = this.text.charAt(++this.pos);
			if (c === "+") return this.pos++, new Token(TokenType.PlusPlus, "++");
			if (c === "=") return this.pos++, new Token(TokenType.PlusEq, "+=");
			return new Token(TokenType.Plus, "+");
		} else if (c === "-") {
			c = this.text.charAt(++this.pos);
			if (c === "-") return this.pos++, new Token(TokenType.MinusMinus, "--");
			if (c === "=") return this.pos++, new Token(TokenType.MinusEq, "-=");
			if (c === ">") return this.pos++, new Token(TokenType.Arrow, "->");
			return new Token(TokenType.Minus, "-");
		} else if (c === "&") {
			c = this.text.charAt(++this.pos);
			if (c === "&") return this.pos++, new Token(TokenType.AmpAmp, "&&");
			if (c === "=") return this.pos++, new Token(TokenType.AmpEq, "&=");
			return new Token(TokenType.Amp, "&");
		} else if (c === "|") {
			c = this.text.charAt(++this.pos);
			if (c === "|") return this.pos++, new Token(TokenType.PipePipe, "||");
			if (c === "=") return this.pos++, new Token(TokenType.PipeEq, "|=");
			return new Token(TokenType.Pipe, "|");
		} else if (c === "*") {
			c = this.text.charAt(++this.pos);
			if (c === "=") return this.pos++, new Token(TokenType.StarEq, "*=");
			return new Token(TokenType.Star, "*");
		} else if (c === "/") {
			c = this.text.charAt(++this.pos);
			if (c === "=") return this.pos++, new Token(TokenType.SlashEq, "/=");
			return new Token(TokenType.Slash, "/");
		} else if (c === "%") {
			c = this.text.charAt(++this.pos);
			if (c === "=") return this.pos++, new Token(TokenType.PercentEq, "%=");
			return new Token(TokenType.Percent, "%");
		} else if (c === "^") {
			c = this.text.charAt(++this.pos);
			if (c === "=") return this.pos++, new Token(TokenType.CaretEq, "^=");
			return new Token(TokenType.Caret, "^");
		} else if (c === "=") {
			c = this.text.charAt(++this.pos);
			if (c === "=") return this.pos++, new Token(TokenType.EqEq, "==");
			return new Token(TokenType.Eq, "=");
		} else if (c === "!") {
			c = this.text.charAt(++this.pos);
			if (c === "=") return this.pos++, new Token(TokenType.BangEq, "!=");
			return new Token(TokenType.Bang, "!");
		} else if (c === "<") {
			c = this.text.charAt(++this.pos);
			if (c === "<") {
				c = this.text.charAt(++this.pos);
				if (c === "=") return this.pos++, new Token(TokenType.LtLtEq, "<<=");
				return new Token(TokenType.LtLt, "<<");
			}
			if (c === "=") return this.pos++, new Token(TokenType.LtEq, "<=");
			return new Token(TokenType.Lt, "<");
		} else if (c === ">") {
			c = this.text.charAt(++this.pos);
			if (c === ">") {
				c = this.text.charAt(++this.pos);
				if (c === "=") return this.pos++, new Token(TokenType.GtGtEq, ">>=");
				return new Token(TokenType.GtGt, ">>");
			}
			if (c === "=") return this.pos++, new Token(TokenType.GtEq, ">=");
			return new Token(TokenType.Gt, ">");
		}
		
		let cc = c.charCodeAt(0);
		
		// Number
		if (cc >= "0".charCodeAt(0) && cc <= "9".charCodeAt(0)) {
			return this.readNumber();
		}
		
		if (
			(cc >= "A".charCodeAt(0) && cc <= "Z".charCodeAt(0)) ||
			(cc >= "a".charCodeAt(0) && cc <= "z".charCodeAt(0)) ||
			cc === "_".charCodeAt(0)
		) {
			return this.readIdentifier();
		}
		
		throw "Lexer error";
	}
}
