export abstract class Type {
	abstract toString(): string;
}
export class TypeName extends Type {
	name: string;
	
	constructor(name: string) {
		super();
		this.name = name;
	}
	
	toString(): string {
		return this.name;
	}
}
export class PointerType extends Type {
	type: Type;
	
	constructor(type: Type) {
		super();
		this.type = type;
	}
	toString(): string {
		return `*${this.type}`;
	}
}
export class ArrayType extends Type {
	type: Type;
	length: number;
	
	constructor(type: Type, length: number) {
		super();
		this.type = type;
		this.length = length;
	}
	
	toString(): string {
		return `[${this.length}]${this.type}`;
	}
}
export class StructType extends Type {
	records: NamedArgument[];
	
	constructor(records: NamedArgument[]) {
		super();
		this.records = records;
	}
	
	toString(): string {
		return `{${this.records.join("; ")}}`;
	}
}
export class FunctionType extends Type {
	args: Type[];
	ret: Type;
	
	constructor(args: Type[], ret: Type = Types.Void) {
		super();
		this.args = args;
		this.ret = ret;
	}
	
	toString(): string {
		return `(${this.args.join(", ")}) -> ${this.ret}`;
	}
}
export class NamedArgument extends Type {
	name: string;
	type: Type;
	
	constructor(name: string, type: Type) {
		super();
		this.name = name;
		this.type = type;
	}
	
	toString(): string {
		return `${this.name}: ${this.type}`;
	}
}
export class VoidType extends Type {
	toString(): string {
		return "Void";
	}
}
export class IntType extends Type {
	toString(): string {
		return "Int";
	}
}
export class CharType extends Type {
	toString(): string {
		return "Char";
	}
}
export class BoolType extends Type {
	toString(): string {
		return "Bool";
	}
}

export const Types: Record<string, Type> = {
	Void: new VoidType,
	Int: new IntType,
	Char: new CharType,
	Bool: new BoolType
};
export const TypeAliases: Record<string, Type> = {
	Boolean: Types.Bool,
	String: new PointerType(Types.Char)
};

export function typeCmp(a: Type, b: Type): boolean {
	if (a instanceof NamedArgument) return typeCmp(a.type, b);
	if (b instanceof NamedArgument) return typeCmp(a, b.type);
	
	if (a instanceof VoidType) {
		return b instanceof VoidType;
	} else if (a instanceof IntType) {
		return b instanceof IntType ||
			b instanceof CharType;
	} else if (a instanceof CharType) {
		return b instanceof CharType ||
			b instanceof IntType;
	} else if (a instanceof BoolType) {
		return b instanceof BoolType;
	} else if (a instanceof PointerType) {
		return b instanceof PointerType &&
			typeCmp(a.type, b.type);
	} else if (a instanceof ArrayType) {
		return b instanceof ArrayType &&
			a.length === b.length &&
			typeCmp(a.type, b.type);
	} else if (a instanceof StructType) {
		return b instanceof StructType &&
			a.records.length === b.records.length &&
			a.records.every((r, i) =>
				r.name === b.records[i].name &&
				typeCmp(r.type, b.records[i].type)
			);
	} else if (a instanceof FunctionType) {
		return b instanceof FunctionType &&
			a.args.length === b.args.length &&
			a.args.every((a, i) => typeCmp(a, b.args[i])) &&
			typeCmp(a.ret, b.ret);
	} else {
		throw "typeCmp: Unknown type " + a;
	}
}
export function typeSize(type: Type): number {
	if (type instanceof VoidType) {
		return 0;
	} else if (type instanceof IntType) {
		return 1;
	} else if (type instanceof CharType) {
		return 1;
	} else if (type instanceof BoolType) {
		return 1;
	} else if (type instanceof PointerType) {
		return 1;
	} else if (type instanceof ArrayType) {
		return typeSize(type.type) * type.length;
	} else if (type instanceof StructType) {
		return type.records.map(r => typeSize(r)).reduce((a, b) => a + b);
	} else if (type instanceof FunctionType) {
		return 1;
	} else if (type instanceof NamedArgument) {
		return typeSize(type.type);
	} else {
		throw "typeSize: Unknown type " + type;
	}
}
