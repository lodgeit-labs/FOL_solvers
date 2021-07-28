#!/usr/bin/env python3

import click,sys
from rdflib import Dataset
from rdflib.namespace import RDF

@click.command()
@click.argument('source_trig_file')

def run(source_trig_file):
    g = Dataset()
    g.parse(source_trig_file, format='trig')

    g2 = Dataset()
    g2.namespace_manager = g.namespace_manager
    for t in g.quads((None, None, None, 'https://rdf.lodgeit.net.au/v1/request#request_graph')):
        #print(s, p, o)
        #print(g)
        g2.add(t)

    sys.stdout.buffer.write(g2.serialize(format='trig'))




if __name__ == '__main__':
    run()

