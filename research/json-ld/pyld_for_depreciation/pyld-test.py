#!/usr/bin/env python3

import rdflib
from pyld import jsonld
import json


frame = {
	"@context": {
		"@base": "https://rdf.lodgeit.net.au/v1/",
		"xsd": "http://www.w3.org/2001/XMLSchema#",
		"rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
		"rdfs": "http://www.w3.org/2000/01/rdf-schema#",
		"l": "https://rdf.lodgeit.net.au/v1/request#",
		"av": "https://rdf.lodgeit.net.au/v1/action_verbs#",
		"excel": "https://rdf.lodgeit.net.au/v1/excel#",
		"kb": "https://rdf.lodgeit.net.au/v1/kb#",
		"depr": "https://rdf.lodgeit.net.au/v1/calcs/depr#",
		"depr_ui": "https://rdf.lodgeit.net.au/v1/calcs/depr/ui#",
	},
	"@type":"https://rdf.lodgeit.net.au/v1/excel_request#request"
}


g = rdflib.ConjunctiveGraph()
g.parse("lodgeitrequest.n3", format="n3")
nq = g.serialize(format='nquads')
doc = jsonld.JsonLdProcessor.parse_nquads(nq)
#doc = jsonld.from_rdf(nq)
framed = jsonld.frame(doc, frame, {'omitGraph':False})
print(json.dumps(framed, indent=True))

