#!/usr/bin/env node
'use strict';


const processor = require('./processor.js')
var jl = require('jsonld');


const { Command } = require('commander');
const program = new Command();

program
	.command('jsonld <source>')
	.description('json-ld from cars rdf')
	.action(async (source) => {
		let s = await processor.load_n3(source);
		console.debug('loaded')
		let framed = await processor.frame(s, {
		    "@id": "http://ex.com/request",
		    "cars:car_sheet_rows":{

			}
		});
		console.log(framed);
});

program.parse(process.argv);
