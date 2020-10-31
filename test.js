require("typescript-require");

const fs = require("fs");
const { execSync } = require("child_process");
const { compile } = require("./src/compiler.ts");
const emulate = require("./emulator/main.js");


//execSync("npx tsc");

let testGroups = fs.readdirSync("./tests").filter(a => a.endsWith(".spooky2"));

for (let i=0; i<testGroups.length; i++) {
	let name = testGroups[i].split(".")[0];
	console.log("\x1b[0m# " + name + ":");
	let testGroup = fs.readFileSync("./tests/" + testGroups[i]).toString();
	let testData = testGroup.split("=".repeat(16));
	testData.shift();
	
	let maxTitleLen = 0;
	let tests = [];
	for (let j=0; j<testData.length; j++) {
		let title = testData[j++].trim();
		let data = testData[j].split("-".repeat(16));
		
		let input = data[0];
		let output = data[1].trim().replace(/\r\n/g, "\n");
		
		tests.push({
			title: title,
			input: input,
			output: output
		});
		
		maxTitleLen = Math.max(maxTitleLen, title.length);
	}
	
	let caseNo = 1;
	for (let j=0; j<tests.length; j++) {
		let { title, input, output } = tests[j];
		
		if (title.startsWith("//")) {
			console.log("\x1b[97;43m SKIP \x1b[m " + title.slice(2).trim());
			continue;
		}
		
		title = title + " ".repeat(maxTitleLen - title.length);
		
		let startBuild = process.hrtime.bigint();
		let file = Buffer.from(compile(input));
		//fs.writeFileSync("./tests/test.spook", file);
		/*execSync("node build/main.js ./tests/test.spooky ./tests/test.spook", {
			timeout: 2000
		});*/
		
		let startRun = process.hrtime.bigint();
		/*let out = execSync("java --enable-preview -jar ../spooky.jar run ./tests/test.spook", {
			timeout: 2000
		});*/
		let { out, instr } = emulate(file);
		let endRun = process.hrtime.bigint();
		
		out = out.toString().trim();
		
		if (out !== output) {
			console.log("\x1b[97;101m FAIL \x1b[m " + title);
			console.log("\x1b[90m" + input.trim().split("\n").map(a => "\t" + a).join("\n"));
			console.log("\n\t\x1b[mExpected:");
			console.log("\t\t\x1b[92m" + output);
			console.log("\t\x1b[mGot:");
			console.log("\t\t\x1b[91m" + out);
			console.log();
		} else {
			let buildTime = (startRun - startBuild) / 1000n / 1000n + "ms";
			let runTime = (endRun - startRun) / 1000n / 1000n + "ms";
			console.log("\x1b[97;102m PASS \x1b[m " + title + " \x1b[90m " + buildTime + " build  " + runTime + " run  " + instr + " cycles\x1b[0m");
		}
		caseNo++;
	}
	
	console.log();
}
