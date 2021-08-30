#!/usr/bin/env node
'use strict';


const processor = require('./processor.js')
var jl = require('jsonld');


const { Command } = require('commander');
const program = new Command();


async function cars_framed(source)
{
	let s = await processor.load_n3(source);
	console.debug('loaded')
	let framed = await processor.frame(s, {
		"@context":
			{
				"excel": "https://rdf.lodgeit.net.au/v1/excel#",
				"cars": "https://rdf.lodgeit.net.au/v1/cars#",
				"rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",

			},
		"@embed": "@once",
		"@requireAll": true,
		"excel:sheet_type": {"@id": "cars:car_sheet"},
		"rdf:value":
			{
				"@list": [
					{
						"@embed": "@always"
					}
				]
			}
	});
	return framed;
}

program
	.command('jsonld <source>')
	.description('json-ld from cars rdf')
	.action(async (source) => {
		console.log(JSON.stringify(await cars_framed(source), null, 2))
});
program
	.command('simplified <source>')
	.description('simplified json from cars rdf')
	.action(async (source) => {
		let f = await cars_framed(source);
		let items = f['@graph'][0]['rdf:value']['@list'];
		items.forEach(i => {
			for (const [key, value] of Object.entries(i))
			{
				let v = value['rdf:value'];
				if (v !== undefined)
					i[key] = v;
			}
		});
		console.log(JSON.stringify(await items, null, 2))
});

program.parse(process.argv);
