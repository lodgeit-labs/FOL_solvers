# robust-specific testcases for pyco


```

?- pack_install('https://github.com/koo5/fnotation.git').
Remove old installation in '/home/koom/lib/swipl/pack/fnotation' Y/n? 
% Cloning into '/home/koom/lib/swipl/pack/fnotation'...
Verify package status (anonymously)
	at "http://www.swi-prolog.org/pack/query" Y/n? 
Package:                fnotation
Title:                  Function notation for Prolog
Installed version:      0.0.3
Author:                 Vitaliy Akimov <vitaliy.akimov@gmail.com>
Maintainer:             Vitaliy Akimov <vitaliy.akimov@gmail.com>
Packager:               Vitaliy Akimov <vitaliy.akimov@gmail.com>
Home page:              https://github.com/awto/fnotation
Download URL:           https://github.com/awto/fnotation/archive/v0.0.3.zip
Activate pack "fnotation" Y/n? 
true.

?- ^D
% halt
[33cd2f4] [19:43:54] koom@L440 /home/koom/repos/koo5/accounts-assessor/0/accounts-assessor/sources/public_lib/lodgeit_solvers/research/robust/pyco2 ((33cd2f49…)) 
 swipl -O -s tests/pyco2_test2b.pl -g "test(q5(_,_)),halt" 2>&1 | tee logs/q5
Warning: pyco_proof: no matching debug topic (yet)
Warning: pyco_ep: no matching debug topic (yet)
Warning: pyco_run: no matching debug topic (yet)
% 3412 ? r repeating.
% 4488 ? r repeating.
% 5055 ? r repeating.
% 5522 ? r stabilized.
% 5522 ? r ok...
% 5522 ? r ok...

```
```
reset;echo -e "\e[3J";   swipl -O -s tests/pyco2_test2b.pl -g "test(q5(_,_)),halt" 2>&1 | tee logs/q5
```

