#!/usr/bin/env node
'use strict';


const processor = require('./processor.js')
var jl = require('jsonld');
const fs = require('fs');
const os = require('os');


const { Command } = require('commander');
const program = new Command();


const ctx = {
//	"@base": "https://rdf.lodgeit.net.au/v1/",

	"xsd": "http://www.w3.org/2001/XMLSchema#",
	"rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
	"rdfs": "http://www.w3.org/2000/01/rdf-schema#",
	"excel": "https://rdf.lodgeit.net.au/v1/excel#",

	"depr": "https://rdf.lodgeit.net.au/v1/calcs/depr#",
	"depr_ui": "https://rdf.lodgeit.net.au/v1/calcs/depr/ui#",
	"smsf": "https://rdf.lodgeit.net.au/v1/calcs/smsf#",
	"smsf_ui": "https://rdf.lodgeit.net.au/v1/calcs/smsf/ui#",
	"smsf_distribution": "https://rdf.lodgeit.net.au/v1/calcs/smsf/distribution#",
	"smsf_distribution_ui": "https://rdf.lodgeit.net.au/v1/calcs/smsf/distribution_ui#",

	"excel:optional":{"@type":"xsd:boolean"},
	"excel:cardinality":{"@type":"@id"},
	"excel:type":{"@type":"@id"},
	"excel:sheets":{"@type":"@id"},
	"excel:has_sheet":{"@type":"@id"},
	"excel:multiple_sheets_allowed":{"@type":"xsd:boolean"},
	"excel:is_horizontal":{"@type":"xsd:boolean"},

	"is_type_of": {"@reverse": "rdf:type2", "@container": "@set"},
	"is_range_of": {"@reverse": "rdfs:range"},

};



const frame = {
  "@context": ctx,

  //"@type":"excel:example_sheet_set",
  //"@type":"excel:sheet_set",
  //"@type":"excel:Request",
	"@id":"https://rdf.lodgeit.net.au/v1/excel_request#request",
	"rdf:value":
		{
			"@list": [
				{
					"@embed": "@always",
				    "@omitDefault": true

				}
			]
		}

};


async function do_frame(data, frame)
{
	const framed = await jl.frame(data, frame, {
		base: "http://ex.com/",
		processingMode: "json-ld-1.1",
		omitGraph: true,
		embed: '@once',
		ordered: true
	})
	return framed
}


async function simplify(frame)
{
	let f = await cars_framed(source);
	let items = f['@graph'][0]['rdf:value']['@list'];
	items.forEach(i => {
		for (const [key, value] of Object.entries(i)) {
			let v = value['rdf:value'];
			if (v !== undefined)
				i[key] = v;
		}
	});
	return items;
}

function clean(data) {
    //console.log(data);
    var del = [];
    for (var key in data) {
        var value = data[key];
        //console.error(key);
        
        if (key === "excel:sheet_instance_has_sheet_type") {
            data[key] = value['@id'];
        }
        
        if (
            (key === "rdf:value" && value === null) ||
            key === "excel:col" || 
            key === "excel:row" || 
            key === "excel:title" || 
            key === "excel:position" || 
            key === "excel:has_sheet_name" || 
            key === "excel:template" ||
            key === "excel:sheet_instance_has_sheet_name" ||
            key === "excel:sheet_type"
             
            
            
            ) {
            //console.log('deleting ' + key + '...');
            del.push(key);
        }
        else if ((typeof value) === 'object') {
            //console.log(value);
            if (value != null)
                clean(value);
        }
    }
    for (var k of del) {
        console.error('deleting ' + k + '...');
        delete data[k];
    }
}

program
	.command('frameAndCleanRequest <source>')
	.action(async (source) => {

		var doc = await processor.load_n3(source, false);
		var r = await do_frame(doc, frame);
		//r = await jl.compact(r, ctx);
		//console.log(r);
		clean(r);
		delete r['@context'];
		
		
		//r = await simplify(r);
		console.log(JSON.stringify(r, null, 4))
});

program
	.command('request_jsonld_to_n3 <source>')
	.action(async (source) => {

		const r = await jl.toRDF(
			await JSON.parse(fs.readFileSync(source, {encoding: 'utf-8'})),
			{format: 'application/n-quads'}
		);
		// print out the N-Quads
		process.stdout.write(r);

});




program.parse(process.argv);
