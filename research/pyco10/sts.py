from rdflib import URIRef
from typing import Collection

def st_to_txs(verbs: Collection, st: URIRef, txs: URIRef):
	"""transform a statement transaction into a list of GL transactions"""
	