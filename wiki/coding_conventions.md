## code style

nothing's set in stone, this is mainly an explanation of some inconsistencies currently found in the codebase. Some of the style elements i'm migrating towards are:

```

 xbrl_bank_account_names(Xbrl_bank_account_names) :- 
	bla,
	bla,
	(	condition
	->	then
	;	else).
	
```
or:
```

 'xbrl bank account names'(Xbrl_bank_account_names) :- 
	bla,
	bla,
	(	condition
	->	then
	;	else).
	
```
not:
```

 xbrlBankAccountNames(XBRLBankAccountNames) :-
	...	
	
```


* Underscores everywhere, no camelCase.
* Variable_names - only first letter capitalized
* Rule definition is preceded by one space. Coupled with otherwise indentation by tabs, this allows jumping to declarations by simply fulltext-searching relatively painlessly.
* It's my personal convention to not capitalize abbreviations, so: Xbrl_instance, not XBRL_instance.

## module system
swipl's module system is really just a namespacing system, and a bad one imo. We had an awful lot of DRY violation when we tried to modularize everything (all the explicit exports and imports) (and CHR also didn't work with it iirc), so now there's pretty much only one 'utils' module and one main 'lib' module. 

