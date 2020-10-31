import fs from "fs";
import { compile } from "./compiler";

let file = fs.readFileSync(process.argv[2]).toString();

fs.writeFileSync(process.argv[3], Buffer.from(compile(file)));
