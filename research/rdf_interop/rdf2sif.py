#!/usr/bin/env python3

import rdflib
import click


@click.command()
@click.argument('rdf_file', type=click.File('r'))
def rdf2sif(rdf_file):
	"""Converts an RDF graph to a SIF file."""

	rdf = rdflib.Graph()
	rdf.parse(rdf_file)
	
	with open('sif.sif', 'w') as f:
		for s, p, o in rdf:
		
			# use loaded namespaces to shorten URIs
			if isinstance(s, rdflib.term.URIRef):
				s = rdf.namespace_manager.normalizeUri(s)
			if isinstance(p, rdflib.term.URIRef):
				p = rdf.namespace_manager.normalizeUri(p)
			if isinstance(o, rdflib.term.URIRef):
				o = rdf.namespace_manager.normalizeUri(o)
		
			f.write('%s\t%s\t%s\n' % (s, p, o))


if __name__ == '__main__':
	rdf2sif()
	