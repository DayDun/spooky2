const stream = require("stream");

const Op = {
	BINDEF: 0x00,
	TEXT: 0x01,
	DATA: 0x02,
	MOV: 0x03,
	CONST: 0x04,
	ADD: 0x05,
	MUL: 0x06,
	SUB: 0x07,
	DIV: 0x08,
	LT: 0x09,
	JMP: 0x0A,
	EXTERN: 0x0B,
	HALT: 0x0C,
	EQ: 0x0D,
	MOD: 0x0E,
	LEQ: 0x0F,
	JMPADR: 0x10
};

class Address {
	constructor(base, offset) {
		this.base = base;
		this.offset = offset;
	}
}

class MovIns {
	constructor(src, dst) {
		this.src = src;
		this.dst = dst;
	}
}
class ConstIns {
	constructor(val, dst) {
		this.val = val;
		this.dst = dst;
	}
}
class BinopIns {
	constructor(lhs, rhs, dst) {
		this.lhs = lhs;
		this.rhs = rhs;
		this.dst = dst;
	}
}
class AddIns extends BinopIns {}
class SubIns extends BinopIns {}
class MulIns extends BinopIns {}
class DivIns extends BinopIns {}
class ModIns extends BinopIns {}
class LtIns extends BinopIns {}
class EqIns extends BinopIns {}
class LeqIns extends BinopIns {}
class JmpIns {
	constructor(test, dst) {
		this.test = test;
		this.dst = dst;
	}
}
class ExternIns {
	constructor(name) {
		this.name = name;
	}
}
class HaltIns {}
class JmpadrIns {
	constructor(dst) {
		this.dst = dst;
	}
}

class Parser {
	parse(file) {
		this.file = file;
		this.pos = 0;
		
		if (this.readByte() !== Op.BINDEF) throw "Expected BINDEF";
		let name = this.readString();
		
		if (this.readByte() !== Op.TEXT) throw "Expected TEXT";
		let text = [];
		
		loop:
		while (this.pos < this.file.length) {
			switch (this.readByte()) {
				case Op.DATA: break loop;
				case Op.MOV: text.push(new MovIns(this.readAddr(), this.readAddr())); break;
				case Op.CONST: text.push(new ConstIns(this.readInt(), this.readAddr())); break;
				case Op.ADD: text.push(new AddIns(this.readAddr(), this.readAddr(), this.readAddr())); break;
				case Op.SUB: text.push(new SubIns(this.readAddr(), this.readAddr(), this.readAddr())); break;
				case Op.MUL: text.push(new MulIns(this.readAddr(), this.readAddr(), this.readAddr())); break;
				case Op.DIV: text.push(new DivIns(this.readAddr(), this.readAddr(), this.readAddr())); break;
				case Op.MOD: text.push(new ModIns(this.readAddr(), this.readAddr(), this.readAddr())); break;
				case Op.LT: text.push(new LtIns(this.readAddr(), this.readAddr(), this.readAddr())); break;
				case Op.EQ: text.push(new EqIns(this.readAddr(), this.readAddr(), this.readAddr())); break;
				case Op.LEQ: text.push(new LeqIns(this.readAddr(), this.readAddr(), this.readAddr())); break;
				case Op.JMP: text.push(new JmpIns(this.readAddr(), this.readInt())); break;
				case Op.EXTERN: text.push(new ExternIns(this.readString())); break;
				case Op.HALT: text.push(new HaltIns()); break;
				case Op.JMPADR: text.push(new JmpadrIns(this.readAddr())); break;
			}
		}
		
		let data = [];
		while (this.pos + 4 <= this.file.length) {
			data.push(this.readInt());
		}
		
		if (this.pos !== this.file.length) throw "Did not reach file end";
		
		return {
			name: name,
			text: text,
			data: data
		};
	}
	
	readByte() {
		return this.file.readUInt8(this.pos++);
	}
	readInt() {
		let value = this.file.readInt32BE(this.pos);
		this.pos += 4;
		return value;
	}
	readAddr() {
		return new Address(this.readInt(), this.readInt());
	}
	readString() {
		let len = this.readByte();
		return this.file.slice(this.pos, this.pos += len).toString();
	}
}
class Vm {
	constructor({ text, data }, memory, out) {
		this.text = text;
		this.data = data;
		this.memory = new Int32Array(memory);
		this.ip = 0;
		this.out = out;
	}
	
	step() {
		if (this.ip < 0 || this.ip >= this.text.length) {
			throw "Instruction pointer out-of-bounds";
		}
		
		let ins = this.text[this.ip++];
		if (ins instanceof MovIns) {
			this.setM(ins.dst, this.getM(ins.src));
		} else if (ins instanceof ConstIns) {
			this.setM(ins.dst, ins.val);
		} else if (ins instanceof AddIns) {
			this.setM(ins.dst, this.getM(ins.lhs) + this.getM(ins.rhs));
		} else if (ins instanceof SubIns) {
			this.setM(ins.dst, this.getM(ins.lhs) - this.getM(ins.rhs));
		} else if (ins instanceof MulIns) {
			this.setM(ins.dst, this.getM(ins.lhs) * this.getM(ins.rhs));
		} else if (ins instanceof DivIns) {
			let rhs = this.getM(ins.rhs);
			if (rhs === 0) throw "Division by zero";
			
			this.setM(ins.dst, this.getM(ins.lhs) / rhs);
		} else if (ins instanceof ModIns) {
			let rhs = this.getM(ins.rhs);
			if (rhs === 0) throw "Division by zero";
			
			this.setM(ins.dst, this.getM(ins.lhs) % rhs);
		} else if (ins instanceof LtIns) {
			this.setM(ins.dst, this.getM(ins.lhs) < this.getM(ins.rhs) ? 1 : 0);
		} else if (ins instanceof LeqIns) {
			this.setM(ins.dst, this.getM(ins.lhs) <= this.getM(ins.rhs) ? 1 : 0);
		} else if (ins instanceof EqIns) {
			this.setM(ins.dst, this.getM(ins.lhs) === this.getM(ins.rhs) ? 1 : 0);
		} else if (ins instanceof JmpIns) {
			if (this.getM(ins.test) === 0) {
				this.ip = ins.dst;
			}
		} else if (ins instanceof JmpadrIns) {
			this.ip = this.getM(ins.dst);
		} else if (ins instanceof ExternIns) {
			this.callExtern(ins.name);
		} else if (ins instanceof HaltIns) {
			return false;
		} else {
			throw "Invalid instruction";
		}
		
		return true;
	}
	
	getM(pos) {
		if (pos instanceof Address) pos = this.resolveAddr(pos);
		
		if (0 <= pos && pos < this.memory.length) return this.memory[pos];
		if (-this.data.length <= pos && pos < 0) return this.data[-(pos + 1)];
		throw "Memory position " + pos + " is out of bounds";
	}
	setM(pos, value) {
		if (pos instanceof Address) pos = this.resolveAddr(pos);
		if (pos < 0 || pos >= this.memory.length) {
			throw "Memory position " + pos + " is out of bounds";
		}
		
		this.memory[pos] = value;
	}
	resolveAddr(addr) {
		return this.getM(addr.base) + addr.offset;
	}
	
	getArg(offset) {
		return this.getM(this.memory[0] - offset);
	}
	setReturn(argSize, value) {
		this.setM(this.memory[0] - argSize - 1, value);
	}
	callExtern(name) {
		switch (name) {
			case "print": this.externPrint(); break;
			case "printInt": this.externPrintInt(); break;
			case "random": this.externRandom(); break;
			default:
				throw "Attempted to call non-existent extern " + name;
		}
	}

	externPrint() {
		//process.stdout.write(String.fromCharCode(this.getArg(1)));
		this.out.write(String.fromCharCode(this.getArg(1)));
	}
	externPrintInt() {
		//process.stdout.write(this.getArg(1).toString());
		this.out.write(this.getArg(1).toString());
	}
	externRandom() {
		this.setReturn(0, Math.floor(Math.random() * 2**32));
	}
	
	run(maxRun) {
		for (let i=0; i<maxRun; i++) {
			if (!this.step()) return i;
		}
		throw "Reached execution limit";
	}
}

class OutStream extends stream.Writable {
	out = "";
	
	_write(chunk, encoding, callback) {
		this.out += chunk.toString();
		callback();
	}
}

function emulate(file) {
	let parser = new Parser();
	let stream = new OutStream();
	let vm = new Vm(parser.parse(file), 1000, stream);
	
	//console.log(vm.text);
	
	let instr = vm.run(1000000);
	
	return {
		out: stream.out,
		instr: instr
	};
}

module.exports = emulate;

if (process.argv[2]) {
	const fs = require("fs");
	let file = fs.readFileSync(process.argv[2]);
	let parser = new Parser();
	let vm = new Vm(parser.parse(file), 1000, process.stdout);
	
	return vm.run(Infinity);
}
