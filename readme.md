# Spooky++
Spooky with pointers.

## How do I program this?
Read the test cases in the `tests` directory for some example code. Or read the source code of the compiler for a more in-depth look.

The language is largely unfinished and has a couple of known bugs.

I do not plan on maintaining this project.

## Installing
`npm install`

## Compiling
`ts-node src/main.ts <source.spooky2> <out.spook>`

Spooky++ outputs regular spook binaries. They can be run with the regular spooky vm.

## Testing
`node test.js`

## Syntax highlighting
The `language-spooky` directory contains an Atom package for syntax highlighting. It is mainly intended for the Atom editor, but could be adapted for other editors supporting tree sitter grammars as well.