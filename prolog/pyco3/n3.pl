:- module(_, [parse_n3_file/2, parse_n3_stream/3]).


version_info('EYE v20.0910.0008 josd').

license_info('MIT License

Copyright (c) 2009 Jos De Roo

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.').


:- dynamic(pfx/2).
:- dynamic(flag/1).
:- dynamic(flag/2).
:- dynamic(base_uri/1).
:- dynamic(qevar/3).
:- dynamic(query/2).
:- dynamic(quvar/3).
:- dynamic(rule_uvar/1).
:- dynamic(evar/3).
:- dynamic(ns/2).



prolog_sym(abolish, abolish, rel).

so_uri('http://').
so_uri('https://').
so_uri('ftp://').
so_uri('file://').


:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#biconditional>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#call>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#conditional>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#finalize>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#relabel>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#tactic>'/2).
:- dynamic('<http://eulersharp.sourceforge.net/2003/03swap/log-rules#transaction>'/2).
:- dynamic('<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'/2).
:- dynamic('<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'/2).
:- dynamic('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'/2).
:- dynamic('<http://www.w3.org/2000/01/rdf-schema#subClassOf>'/2).
:- dynamic('<http://www.w3.org/2000/10/swap/log#implies>'/2).
:- dynamic('<http://www.w3.org/2000/10/swap/log#outputString>'/2).



parse_n3_file(FilePath,Triples):-
	open(FilePath, read, In, [encoding(utf8)]),
    parse_n3_stream(FilePath, In, Triples).

parse_n3_stream(Base_uri, In, Triples) :-
	nb_setval(line_number, 1),
	nb_setval(fdepth, 0),
    nb_setval(smod, true),
	retractall(base_uri(_)),
	assertz(base_uri(Base_uri)),
	gtrace,
	tokens(In, Tokens),
	phrase(document(Triples), Tokens, Rest),
	(   Rest = []
	->  writeq(Triples)
	;   nb_getval(line_number, Ln),
		throw(invalid_document(after_line(Ln),remaining(Rest)))
	).




%
% N3 parser
%
% according to http://www.w3.org/2000/10/swap/grammar/n3-ietf.txt
% inspired by http://code.google.com/p/km-rdf/wiki/Henry
%

barename(BareName) -->
    [name(BareName)].

barename_csl([BareName|Tail]) -->
    barename(BareName),
    !,
    barename_csl_tail(Tail).
barename_csl([]) -->
    [].

barename_csl_tail([BareName|Tail]) -->
    [','],
    !,
    barename(BareName),
    barename_csl_tail(Tail).
barename_csl_tail([]) -->
    [].

boolean(true) -->
    [name('true')],
    !.
boolean(false) -->
    [name('false')],
    !.
boolean(Boolean) -->
    literal(Atom, type(T)),
    {   T = '\'<http://www.w3.org/2001/XMLSchema#boolean>\'',
        (   memberchk([Boolean, Atom], [[true, '\'true\''], [true, true], [true, '\'1\''], [false, '\'false\''], [false, false], [false, '\'0\'']])
        ->  true
        ;   (   flag('parse-only')
            ->  true
            ;   nb_getval(line_number, Ln),
                throw(invalid_boolean_literal(Atom, after_line(Ln)))
            )
        )
    }.

declaration -->
    [atname(base)],
    !,
    explicituri(U),
    {   base_uri(V),
        resolve_uri(U, V, URI),
        retractall(base_uri(_)),
        assertz(base_uri(URI))
    }.
declaration -->
    [name(Name)],
    {   downcase_atom(Name, 'base')
    },
    !,
    explicituri(U),
    {   base_uri(V),
        resolve_uri(U, V, URI),
        retractall(base_uri(_)),
        assertz(base_uri(URI))
    },
    withoutdot.
declaration -->
    [atname(prefix)],
    !,
    prefix(Prefix),
    explicituri(U),
    {   base_uri(V),
        resolve_uri(U, V, URI),
        retractall(ns(Prefix, _)),
        assertz(ns(Prefix, URI)),
        put_pfx(Prefix, URI)
    }.
declaration -->
    [name(Name)],
    {   downcase_atom(Name, 'prefix')
    },
    prefix(Prefix),
    explicituri(U),
    {   base_uri(V),
        resolve_uri(U, V, URI),
        retractall(ns(Prefix, _)),
        assertz(ns(Prefix, URI)),
        put_pfx(Prefix, URI)
    },
    withoutdot.

document(Triples) -->
    statements_optional(Triples).

dtlang(lang(Langcode)) -->
    [atname(Name)],
    {   Name \= 'is',
        Name \= 'has'
    },
    !,
    {   atomic_list_concat(['\'', Name, '\''], Langcode)
    }.
dtlang(type(Datatype)) -->
    [caret_caret],
    !,
    uri(Datatype).
dtlang(type(T)) -->
    {   T = '\'<http://www.w3.org/2001/XMLSchema#string>\''
    },
    [].

existential -->
    [atname(forSome)],
    !,
    symbol_csl(Symbols),
    {   nb_getval(fdepth, D),
        forall(
            (   member(S, Symbols)
            ),
            (   gensym('qe_', Q),
                asserta(qevar(S, Q, D))
            )
        )
    }.

explicituri(ExplicitURI) -->
    [relative_uri(ExplicitURI)].

expression(Node, T) -->
    pathitem(N1, T1),
    pathtail(N1, Node, T2),
    {   append(T1, T2, T)
    }.

formulacontent(Formula) -->
    statementlist(List),
    {   conj_list(Formula, List)
    }.

literal(Atom, DtLang) -->
    string(Codes),
    dtlang(DtLang),
    {   escape_string(Codes, B),
        escape_string(B, C),
        atom_codes(A, C),
        (   sub_atom(A, _, 1, _, '\'')
        ->  escape_squote(C, D),
            atom_codes(E, D)
        ;   E = A
        ),
        atomic_list_concat(['\'', E, '\''], Atom)
    }.

numericliteral(Number) -->
    [numeric(_, NumB)],
    {   numeral(NumB, NumC),
        number_codes(Number, NumC)
    }.

object(Node, Triples) -->
    expression(Node, Triples).

objecttail(Subject, Verb, [Triple|T]) -->
    [','],
    !,
    object(Object, Triples),
    objecttail(Subject, Verb, Tail),
    {   append(Triples, Tail, T),
        (   Verb = isof(V)
        ->  (   atom(V),
                \+sub_atom(V, 0, 1, _, '_')
            ->  Triple =.. [V, Object, Subject]
            ;   Triple = exopred(V, Object, Subject)
            )
        ;   (   atom(Verb),
                \+sub_atom(Verb, 0, 1, _, '_')
            ->  Triple =.. [Verb, Subject, Object]
            ;   Triple = exopred(Verb, Subject, Object)
            )
        )
    }.
objecttail(_, _, []) -->
    [].

pathitem(Name, []) -->
    symbol(S),
    !,
    {   (   qevar(S, N, D),
            \+quvar(S, _, _)
        ->  (   D >= 1,
                nb_getval(fdepth, FD),
                FD >= D,
                \+flag('pass-all-ground')
            ->  atom_concat('_', N, Name),
                nb_setval(smod, false)
            ;   nb_getval(var_ns, Vns),
                atomic_list_concat(['\'<', Vns, N, '>\''], Name)
            )
        ;   (   quvar(S, N, D)
            ->  (   (   D = 1,
                        nb_getval(fdepth, FD),
                        FD >= 1
                    ;   flag('pass-all-ground')
                    )
                ->  nb_getval(var_ns, Vns),
                    atomic_list_concat(['\'<', Vns, N, '>\''], Name)
                ;   atom_concat('_', N, Name),
                    nb_setval(smod, false)
                )
            ;   Name = S
            )
        ),
        (   quvar(S, _, _)
        ->  nb_setval(smod, false)
        ;   true
        )
    }.
pathitem(VarID, []) -->
    [uvar(Var)],
    !,
    {   atom_codes(Var, VarCodes),
        subst([[[0'-], [0'_, 0'M, 0'I, 0'N, 0'U, 0'S, 0'_]], [[0'.], [0'_, 0'D, 0'O, 0'T, 0'_]]], VarCodes, VarTidy),
        atom_codes(VarAtom, [0'_|VarTidy]),
        (   flag('pass-all-ground')
        ->  nb_getval(var_ns, Vns),
            atom_codes(VarFrag, VarTidy),
            atomic_list_concat(['\'<', Vns, VarFrag, '>\''], VarID)
        ;   VarID = VarAtom
        ),
        nb_setval(smod, false)
    }.
pathitem(Number, []) -->
    numericliteral(Number),
    !.
pathitem(Boolean, []) -->
    boolean(Boolean),
    !.
pathitem(Atom, []) -->
    literal(A, type(T)),
    {   T = '\'<http://eulersharp.sourceforge.net/2003/03swap/prolog#atom>\''
    },
    !,
    {   atom_codes(A, B),
        escape_string(C, B),
        atom_codes(Atom, C)
    }.
pathitem(Number, []) -->
    {   \+flag('parse-only')
    },
    literal(Atom, type(Type)),
    {   memberchk(Type, ['\'<http://www.w3.org/2001/XMLSchema#integer>\'', '\'<http://www.w3.org/2001/XMLSchema#decimal>\'', '\'<http://www.w3.org/2001/XMLSchema#double>\'']),
        sub_atom(Atom, 1, _, 1, A),
        atom_codes(A, NumB),
        numeral(NumB, NumC),
        atom_codes(NumA, NumC),
        (   NumA = 'INF'
        ->  Number = inf
        ;   (   NumA = '-INF'
            ->  Number = -inf
            ;   (   NumA = 'NaN'
                ->  Number = nan
                ;   number_codes(Number, NumC)
                )
            )
        )
    },
    !.
pathitem(literal(Atom, DtLang), []) -->
    literal(Atom, DtLang),
    !.
pathitem(BNode, Triples) -->
    ['['],
    !,
    {   gensym('bn_', S),
        (   (   nb_getval(fdepth, FD),
                FD =\= 1
            ;   flag('pass-all-ground')
            )
        ->  nb_getval(var_ns, Vns),
            atomic_list_concat(['\'<', Vns, S, '>\''], BN)
        ;   atom_concat('_', S, BN),
            nb_setval(smod, false)
        )
    },
    propertylist(BN, T),
    {   (   memberchk('\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>\''(X, Head), T),
            memberchk('\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>\''(X, Tail), T),
            del(T, '\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>\''(X, '\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#List>\''), U),
            del(U, '\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>\''(X, Head), V),
            del(V, '\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>\''(X, Tail), W)
        ->  BNode = [Head|Tail],
            Triples = W
        ;   BNode = BN,
            Triples = T
        )
    },
    [']'].
pathitem(set(Distinct), Triples) -->
    ['(', '$'],
    !,
    pathlist(List, Triples),
    {   (   nb_getval(smod, true)
        ->  sort(List, Distinct)
        ;   distinct(List, Distinct)
        )
    },
    ['$', ')'].
pathitem(List, Triples) -->
    ['('],
    !,
    pathlist(List, Triples),
    [')'].
pathitem(triple(S, P, O), Triples) -->
    [lt_lt],
    !,
    pathlist(List, Triples),
    {   (   List = [S, P, O]
        ->  true
        ;   nb_getval(line_number, Ln),
            throw('invalid_n3*_triple'(List, after_line(Ln)))
        )
    },
    [gt_gt].
pathitem(Node, []) -->
    ['{'],
    {   nb_getval(fdepth, I),
        J is I+1,
        nb_setval(fdepth, J),
        nb_setval(smod, true)
    },
    formulacontent(Node),
    {   retractall(quvar(_, _, J)),
        retractall(qevar(_, _, J)),
        retractall(evar(_, _, J)),
        nb_setval(fdepth, I),
        nb_setval(smod, false)
    },
    ['}'].

pathlist([Node|Rest], Triples) -->
    expression(Node, T),
    !,
    pathlist(Rest, Tail),
    {   append(T, Tail, Triples)
    }.
pathlist([], []) -->
    [].

pathtail(Node, PNode, [Triple|Triples]) -->
    ['!'],
    !,
    pathitem(Item, Triples2),
    {   maybe_prolog_verb(Item, Verb),
        gensym('bn_', S),
        (   (   nb_getval(fdepth, 0)
            ;   flag('pass-all-ground')
            )
        ->  nb_getval(var_ns, Vns),
            atomic_list_concat(['\'<', Vns, S, '>\''], BNode)
        ;   atom_concat('_', S, BNode),
            nb_setval(smod, false)
        ),
        (   Verb = isof(V)
        ->  (   atom(V),
                \+sub_atom(V, 0, 1, _, '_')
            ->  Triple =.. [V, BNode, Node]
            ;   Triple = exopred(V, BNode, Node)
            )
        ;   (   Verb = prolog:Pred
            ->  (   BNode = true
                ->  Triple =.. [Pred|Node]
                ;   (   BNode = false
                    ->  T =.. [Pred|Node],
                        Triple = \+(T)
                    ;   (   prolog_sym(_, Pred, func)
                        ->  T =.. [Pred|Node],
                            Triple = is(BNode, T)
                        ;   Triple =.. [Pred, Node, BNode]
                        )
                    )
                )
            ;   (   atom(Verb),
                    \+sub_atom(Verb, 0, 1, _, '_')
                ->  Triple =.. [Verb, Node, BNode]
                ;   Triple = exopred(Verb, Node, BNode)
                )
            )
        )
    },
    pathtail(BNode, PNode, Tail),
    {   append(Triples2, Tail, Triples)
    }.
pathtail(Node, PNode, [Triple|Triples]) -->
    ['^'],
    !,
    pathitem(Item, Triples2),
    {   maybe_prolog_verb(Item, Verb),
        gensym('bn_', S),
        (   (   nb_getval(fdepth, 0)
            ;   flag('pass-all-ground')
            )
        ->  nb_getval(var_ns, Vns),
            atomic_list_concat(['\'<', Vns, S, '>\''], BNode)
        ;   atom_concat('_', S, BNode),
            nb_setval(smod, false)
        ),
        (   Verb = isof(V)
        ->  (   atom(V),
                \+sub_atom(V, 0, 1, _, '_')
            ->  Triple =.. [V, Node, BNode]
            ;   Triple = exopred(V, Node, BNode)
            )
        ;   (   Verb = prolog:Pred
            ->  (   Node = true
                ->  Triple =.. [Pred|BNode]
                ;   (   Node = false
                    ->  T =.. [Pred|BNode],
                        Triple = \+(T)
                    ;   (   prolog_sym(_, Pred, func)
                        ->  T =.. [Pred|BNode],
                            Triple = is(Node, T)
                        ;   Triple =.. [Pred, BNode, Node]
                        )
                    )
                )
            ;   (   atom(Verb),
                    \+sub_atom(Verb, 0, 1, _, '_')
                ->  Triple =.. [Verb, BNode, Node]
                ;   Triple = exopred(Verb, BNode, Node)
                )
            )
        )
    },
    pathtail(BNode, PNode, Tail),
    {   append(Triples2, Tail, Triples)
    }.
pathtail(Node, Node, []) -->
    [].

prefix(Prefix) -->
    [Prefix:''].

propertylist(Subject, [Triple|Triples]) -->
    verb(Item, Triples1),
    {   maybe_prolog_verb(Item, Verb)
    },
    !,
    object(Object, Triples2),
    objecttail(Subject, Verb, Triples3),
    propertylisttail(Subject, Triples4),
    {   append(Triples1, Triples2, Triples12),
        append(Triples12, Triples3, Triples123),
        append(Triples123, Triples4, Triples),
        (   Verb = isof(V)
        ->  (   atom(V),
                \+sub_atom(V, 0, 1, _, '_')
            ->  Triple =.. [V, Object, Subject]
            ;   Triple = exopred(V, Object, Subject)
            )
        ;   (   Verb = prolog:Pred
            ->  (   Object = true
                ->  Triple =.. [Pred|Subject]
                ;   (   Object = false
                    ->  T =.. [Pred|Subject],
                        Triple = \+(T)
                    ;   (   prolog_sym(_, Pred, func)
                        ->  T =.. [Pred|Subject],
                            Triple = is(Object, T)
                        ;   Triple =.. [Pred, Subject, Object]
                        )
                    )
                )
            ;   (   atom(Verb),
                    \+sub_atom(Verb, 0, 1, _, '_')
                ->  Triple =.. [Verb, Subject, Object]
                ;   Triple = exopred(Verb, Subject, Object)
                )
            )
        )
    }.
propertylist(_, []) -->
    [].

propertylisttail(Subject, Triples) -->
    [';'],
    !,
    propertylisttailsemis,
    propertylist(Subject, Triples).
propertylisttail(_, []) -->
    [].

propertylisttailsemis -->
    [';'],
    !,
    propertylisttailsemis.
propertylisttailsemis -->
    [].

qname(URI) -->
    [NS:Name],
    {   (   ns(NS, Base)
        ->  atomic_list_concat([Base, Name], Name1),
            (   sub_atom(Name1, _, 1, _, '\'')
            ->  atom_codes(Name1, Codes1),
                escape_squote(Codes1, Codes2),
                atom_codes(Name2, Codes2)
            ;   Name2 = Name1
            ),
            atomic_list_concat(['\'<', Name2, '>\''], URI)
        ;   nb_getval(line_number, Ln),
            throw(no_prefix_directive(NS, after_line(Ln)))
        )
    },
    !.

simpleStatement(Triples) -->
    subject(Subject, Triples1),
    (   {   Subject = (D1;D2)
        }
    ->  {   Triples = [(D1;D2)]
        }
    ;   propertylist(Subject, Triples2),
        {   append(Triples1, Triples2, Triples)
        }
    ).

statement([]) -->
    declaration,
    !.
statement([]) -->
    universal,
    !.
statement([]) -->
    existential,
    !.
statement(Statement) -->
    simpleStatement(Statement).

statementlist(Triples) -->
    statement(Tr),
    !,
    statementtail(T),
    {   append(Tr, T, Triples)
    }.
statementlist([]) -->
    [].

statements_optional(Triples) -->
    statement(Tr),
    [dot(Ln)],
    !,
    {   nb_setval(line_number, Ln)
    },
    statements_optional(T),
    {   append(Tr, T, Triples)
    }.
statements_optional([]) -->
    [].

statementtail(T) -->
    [dot(Ln)],
    !,
    {   nb_setval(line_number, Ln)
    },
    statementlist(T).
statementtail([]) -->
    [].

string(Codes) -->
    [literal(Codes)].

subject(Node, Triples) -->
    expression(Node, Triples).

symbol(Name) -->
    uri(Name),
    !.
symbol(Name) -->
    [name(N)],
    !,
    {   (   memberchk(N, [true, false])
        ->  Name = N
        ;   nb_getval(line_number, Ln),
            throw(invalid_keyword(N, after_line(Ln)))
        )
    }.
symbol(Name) -->
    [bnode(Label)],
    {   nb_getval(fdepth, D),
        (   D =:= 0
        ->  N = Label
        ;   atom_codes(Label, LabelCodes),
            subst([[[0'-], [0'_, 0'M, 0'I, 0'N, 0'U, 0'S, 0'_]], [[0'.], [0'_, 0'D, 0'O, 0'T, 0'_]]], LabelCodes, LabelTidy),
            atom_codes(N, LabelTidy)
        ),
        (   evar(N, S, D)
        ->  true
        ;   atom_concat(N, '_', M),
            gensym(M, S),
            assertz(evar(N, S, D))
        ),
        (   (   nb_getval(fdepth, FD),
                FD =\= 1
            ;   flag('pass-all-ground')
            )
        ->  nb_getval(var_ns, Vns),
            (   flag('pass-all-ground')
            ->  atomic_list_concat(['\'<', Vns, N, '>\''], Name)
            ;   atomic_list_concat(['\'<', Vns, 'e_', S, '>\''], Name)
            )
        ;   atom_concat('_e_', S, Name),
            nb_setval(smod, false)
        )
    }.

symbol_csl([Symbol|Tail]) -->
    symbol(Symbol),
    !,
    symbol_csl_tail(Tail).
symbol_csl([]) -->
    [].

symbol_csl_tail([Symbol|T]) -->
    [','],
    !,
    symbol(Symbol),
    symbol_csl_tail(T).
symbol_csl_tail([]) -->
    [].

universal -->
    [atname(forAll)],
    !,
    symbol_csl(Symbols),
    {   nb_getval(fdepth, D),
        (   \+flag(traditional),
            D > 0
        ->  throw(not_supported_keyword('@forAll', at_formula_depth(D)))
        ;   true
        ),
        forall(
            (   member(S, Symbols)
            ),
            (   gensym('qu_', Q),
                asserta(quvar(S, Q, D))
            )
        )
    }.

uri(Name) -->
    explicituri(U),
    !,
    {   base_uri(V),
        resolve_uri(U, V, W),
        (   sub_atom(W, _, 1, _, '\'')
        ->  atom_codes(W, X),
            escape_squote(X, Y),
            atom_codes(Z, Y)
        ;   Z = W
        ),
        atomic_list_concat(['\'<', Z, '>\''], Name)
    }.
uri(Name) -->
    qname(Name).

verb('\'<http://www.w3.org/2000/10/swap/log#implies>\'', []) -->
    ['=', '>'],
    !.
verb('\'<http://www.w3.org/2002/07/owl#sameAs>\'', []) -->
    ['='],
    !.
verb(':-', []) -->
    [lt_eq],
    !.
verb('\'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>\'', []) -->
    [name(a)],
    !.
verb(Node, Triples) -->
    [name(has)],
    !,
    expression(Node, Triples).
verb(isof(Node), Triples) -->
    [name(is)],
    !,
    expression(Node, Triples),
    [name(of)].
verb(isof(Node), Triples) -->
    [lt_dash],
    !,
    expression(Node, Triples).
verb(Node, Triples) -->
    expression(Node, Triples).

withoutdot, [dot(Ln)] -->
    [dot(Ln)],
    !,
    {   throw(unexpected_dot(after_line(Ln)))
    }.
withoutdot, [dot(Ln)] -->
    [],
    {   nb_getval(line_number, Ln)
    }.

%
% N3 tokenizer
%

tokens(In, List) :-
    get_code(In, C0),
    (   token(C0, In, C1, Tok1)
    ->  true
    ;   nb_getval(line_number, Ln),
        char_code(Char, C0),
        throw(illegal_token(char_code(Char, C0), line(Ln)))
    ),
    (   Tok1 == end_of_file
    ->  List = []
    ;   List = [Tok1|Tokens],
        tokens(C1, In, Tokens)
    ).

tokens(C0, In, List) :-
    (   token(C0, In, C1, H)
    ->  true
    ;   nb_getval(line_number, Ln),
        char_code(Char, C0),
        throw(illegal_token(char_code(Char, C0), line(Ln)))
    ),
    (   H == end_of_file
    ->  List = []
    ;   List = [H|T],
        tokens(C1, In, T)
    ).

token(-1, _, -1, end_of_file) :-
    !.
token(0'., In, C, Token) :-
    (   peek_code(In, C0),
        (   e(C0)
        ->  T1 = [0'0|T2],
            get_code(In, CN1)
        ;   0'0 =< C0,
            C0 =< 0'9,
            get_code(In, C1),
            integer_codes(C1, In, CN1, T1, T2)
        )
    ->  (   exponent(CN1, In, C, T2)
        ->  Type = double
        ;   C = CN1,
            T2 = [],
            Type = decimal
        ),
        Token = numeric(Type, [0'0, 0'.|T1])
    ;   nb_getval(line_number, Ln),
        get_code(In, C),
        !,
        Token = dot(Ln)
    ).
token(0'#, In, C, Token) :-
    !,
    get_code(In, C1),
    skip_line(C1, In, C2),
    token(C2, In, C, Token).
token(C0, In, C, Token) :-
    white_space(C0),
    !,
    get_code(In, C1),
    token(C1, In, C, Token).
token(C0, In, C, Number) :-
    0'0 =< C0,
    C0 =< 0'9,
    !,
    number_n(C0, In, C, Number).
token(0'-, In, C, Number) :-
    !,
    number_n(0'-, In, C, Number).
token(0'+, In, C, Number) :-
    !,
    number_n(0'+, In, C, Number).
token(0'", In, C, literal(Codes)) :-
    !,
    (   peek_code(In, 0'")
    ->  get_code(In, 0'"),
        (   peek_code(In, 0'")
        ->  get_code(In, 0'"),
            get_code(In, C1),
            dq_string(C1, In, C, Codes)
        ;   get_code(In, C),
            Codes = []
        )
    ;   get_code(In, C1),
        string_dq(C1, In, C, Codes)
    ).
token(0'', In, C, literal(Codes)) :-
    !,
    (   peek_code(In, 0'')
    ->  get_code(In, 0''),
        (   peek_code(In, 0'')
        ->  get_code(In, 0''),
            get_code(In, C1),
            sq_string(C1, In, C, Codes)
        ;   get_code(In, C),
            Codes = []
        )
    ;   get_code(In, C1),
        string_sq(C1, In, C, Codes)
    ).
token(0'?, In, C, uvar(Name)) :-
    !,
    get_code(In, C0),
    (   name(C0, In, C, Name)
    ->  true
    ;   C = C0,
        nb_getval(line_number, Ln),
        throw(empty_quickvar_name(line(Ln)))
    ).
token(0'_, In, C, bnode(Name)) :-
    peek_code(In, 0':),
    !,
    get_code(In, _),
    get_code(In, C0),
    (   name(C0, In, C, Name)
    ->  true
    ;   C = C0,
        Name = ''
    ).
token(0'<, In, C, lt_lt) :-
    peek_code(In, 0'<),
    !,
    get_code(In, _),
    get_code(In, C).
token(0'<, In, C, lt_eq) :-
    peek_string(In, 2, D),
    string_codes(D, [0'=, E]),
    (   white_space(E)
    ;   punctuation(E, _)
    ),
    !,
    get_code(In, _),
    get_code(In, C).
token(0'<, In, C, lt_dash) :-
    peek_string(In, 2, D),
    string_codes(D, [0'-, E]),
    (   white_space(E)
    ;   punctuation(E, _)
    ),
    !,
    get_code(In, _),
    get_code(In, C).
token(0'<, In, C, relative_uri(URI)) :-
    peek_code(In, C1),
    !,
    get_code(In, C1),
    iri_chars(C1, In, C, Codes),
    D = Codes,
    atom_codes(URI, D).
token(0'>, In, C, gt_gt) :-
    peek_code(In, 0'>),
    !,
    get_code(In, _),
    get_code(In, C).
token(0':, In, C, Token) :-
    !,
    get_code(In, C0),
    (   local_name(C0, In, C, Name)
    ->  Token = '':Name
    ;   Token = '':'',
        C = C0
    ).
token(0'@, In, C, atname(Name)) :-
    get_code(In, C0),
    token(C0, In, C, name(Name)),
    !.
token(0'^, In, C, caret_caret) :-
    peek_code(In, 0'^),
    !,
    get_code(In, _),
    get_code(In, C).
token(C0, In, C, Token) :-
    name(C0, In, C1, Name),
    !,
    (   C1 == 0':
    ->  get_code(In, C2),
        (   local_name(C2, In, C, Name2)
        ->  Token = (Name:Name2)
        ;   Token = (Name:''),
            C = C2
        )
    ;   Token = name(Name),
        C = C1
    ).
token(C0, In, C, P) :-
    punctuation(C0, P),
    !,
    get_code(In, C).

number_n(0'-, In, CN, numeric(T, [0'-|Codes])) :-
    !,
    get_code(In, C0),
    number_nn(C0, In, CN, numeric(T, Codes)).
number_n(0'+, In, CN, numeric(T, [0'+|Codes])) :-
    !,
    get_code(In, C0),
    number_nn(C0, In, CN, numeric(T, Codes)).
number_n(C0, In, CN, Value) :-
    number_nn(C0, In, CN, Value).

number_nn(C, In, CN, numeric(Type, Codes)) :-
    integer_codes(C, In, CN0, Codes, T0),
    (   CN0 == 0'.,
        peek_code(In, C0),
        (   e(C0)
        ->  T1 = [0'0|T2],
            get_code(In, CN1)
        ;   0'0 =< C0,
            C0 =< 0'9,
            get_code(In, C1),
            integer_codes(C1, In, CN1, T1, T2)
        ),
        T0 = [0'.|T1]
    ->  (   exponent(CN1, In, CN, T2)
        ->  Type = double
        ;   CN = CN1,
            T2 = [],
            Type = decimal
        )
    ;   (   exponent(CN0, In, CN, T0)
        ->  Type = double
        ;   T0 = [],
            CN = CN0,
            Type = integer
        )
    ).

integer_codes(C0, In, CN, [C0|T0], T) :-
    0'0 =< C0,
    C0 =< 0'9,
    !,
    get_code(In, C1),
    integer_codes(C1, In, CN, T0, T).
integer_codes(CN, _, CN, T, T).

exponent(C0, In, CN, [C0|T0]) :-
    e(C0),
    !,
    get_code(In, C1),
    optional_sign(C1, In, CN0, T0, T1),
    integer_codes(CN0, In, CN, T1, []),
    (   T1 = []
    ->  nb_getval(line_number, Ln),
        throw(invalid_exponent(line(Ln)))
    ;   true
    ).

optional_sign(C0, In, CN, [C0|T], T) :-
    sign(C0),
    !,
    get_code(In, CN).
optional_sign(CN, _, CN, T, T).

e(0'e).
e(0'E).

sign(0'-).
sign(0'+).

dq_string(-1, _, _, []) :-
    !,
    nb_getval(line_number, Ln),
    throw(unexpected_end_of_input(line(Ln))).
dq_string(0'", In, C, []) :-
    (   retract(got_dq)
    ->  true
    ;   peek_code(In, 0'"),
        get_code(In, _)
    ),
    (   retract(got_dq)
    ->  assertz(got_dq)
    ;   assertz(got_dq),
        peek_code(In, 0'"),
        get_code(In, _),
        assertz(got_dq)
    ),
    !,
    (   peek_code(In, 0'")
    ->  nb_getval(line_number, Ln),
        throw(unexpected_double_quote(line(Ln)))
    ;   true
    ),
    retractall(got_dq),
    get_code(In, C).
dq_string(0'", In, C, [0'"|T]) :-
    !,
    (   retract(got_dq)
    ->  C1 = 0'"
    ;   get_code(In, C1)
    ),
    dq_string(C1, In, C, T).
dq_string(0'\\, In, C, [H|T]) :-
    (   retract(got_dq)
    ->  C1 = 0'"
    ;   get_code(In, C1)
    ),
    !,
    string_escape(C1, In, C2, H),
    dq_string(C2, In, C, T).
dq_string(C0, In, C, [C0|T]) :-
    (   retract(got_dq)
    ->  C1 = 0'"
    ;   get_code(In, C1)
    ),
    dq_string(C1, In, C, T).

sq_string(-1, _, _, []) :-
    !,
    nb_getval(line_number, Ln),
    throw(unexpected_end_of_input(line(Ln))).
sq_string(0'', In, C, []) :-
    (   retract(got_sq)
    ->  true
    ;   peek_code(In, 0''),
        get_code(In, _)
    ),
    (   retract(got_sq)
    ->  assertz(got_sq)
    ;   assertz(got_sq),
        peek_code(In, 0''),
        get_code(In, _),
        assertz(got_sq)
    ),
    !,
    (   peek_code(In, 0'')
    ->  nb_getval(line_number, Ln),
        throw(unexpected_single_quote(line(Ln)))
    ;   true
    ),
    retractall(got_sq),
    get_code(In, C).
sq_string(0'', In, C, [0''|T]) :-
    !,
    (   retract(got_sq)
    ->  C1 = 0''
    ;   get_code(In, C1)
    ),
    sq_string(C1, In, C, T).
sq_string(0'\\, In, C, [H|T]) :-
    (   retract(got_sq)
    ->  C1 = 0''
    ;   get_code(In, C1)
    ),
    !,
    string_escape(C1, In, C2, H),
    sq_string(C2, In, C, T).
sq_string(C0, In, C, [C0|T]) :-
    (   retract(got_sq)
    ->  C1 = 0''
    ;   get_code(In, C1)
    ),
    sq_string(C1, In, C, T).

string_dq(-1, _, _, []) :-
    !,
    nb_getval(line_number, Ln),
    throw(unexpected_end_of_input(line(Ln))).
string_dq(0'\n, _, _, []) :-
    !,
    nb_getval(line_number, Ln),
    throw(unexpected_end_of_line(line(Ln))).
string_dq(0'", In, C, []) :-
    !,
    get_code(In, C).
string_dq(0'\\, In, C, D) :-
    get_code(In, C1),
    !,
    string_escape(C1, In, C2, H),
    (   current_prolog_flag(windows, true),
        H > 0xFFFF
    ->  E is (H-0x10000)>>10+0xD800,
        F is (H-0x10000) mod 0x400+0xDC00,
        D = [E, F|T]
    ;   D = [H|T]
    ),
    string_dq(C2, In, C, T).
string_dq(C0, In, C, D) :-
    (   current_prolog_flag(windows, true),
        C0 > 0xFFFF
    ->  E is (C0-0x10000)>>10+0xD800,
        F is (C0-0x10000) mod 0x400+0xDC00,
        D = [E, F|T]
    ;   D = [C0|T]
    ),
    get_code(In, C1),
    string_dq(C1, In, C, T).

string_sq(-1, _, _, []) :-
    !,
    nb_getval(line_number, Ln),
    throw(unexpected_end_of_input(line(Ln))).
string_sq(0'', In, C, []) :-
    !,
    get_code(In, C).
string_sq(0'\\, In, C, D) :-
    get_code(In, C1),
    !,
    string_escape(C1, In, C2, H),
    (   current_prolog_flag(windows, true),
        H > 0xFFFF
    ->  E is (H-0x10000)>>10+0xD800,
        F is (H-0x10000) mod 0x400+0xDC00,
        D = [E, F|T]
    ;   D = [H|T]
    ),
    string_sq(C2, In, C, T).
string_sq(C0, In, C, D) :-
    (   current_prolog_flag(windows, true),
        C0 > 0xFFFF
    ->  E is (C0-0x10000)>>10+0xD800,
        F is (C0-0x10000) mod 0x400+0xDC00,
        D = [E, F|T]
    ;   D = [C0|T]
    ),
    get_code(In, C1),
    string_sq(C1, In, C, T).

string_escape(0't, In, C, 0'\t) :-
    !,
    get_code(In, C).
string_escape(0'b, In, C, 0'\b) :-
    !,
    get_code(In, C).
string_escape(0'n, In, C, 0'\n) :-
    !,
    get_code(In, C).
string_escape(0'r, In, C, 0'\r) :-
    !,
    get_code(In, C).
string_escape(0'f, In, C, 0'\f) :-
    !,
    get_code(In, C).
string_escape(0'", In, C, 0'") :-
    !,
    get_code(In, C).
string_escape(0'', In, C, 0'') :-
    !,
    get_code(In, C).
string_escape(0'\\, In, C, 0'\\) :-
    !,
    get_code(In, C).
string_escape(0'u, In, C, Code) :-
    !,
    get_hhhh(In, A),
    (   0xD800 =< A,
        A =< 0xDBFF
    ->  get_code(In, 0'\\),
        get_code(In, 0'u),
        get_hhhh(In, B),
        Code is 0x10000+(A-0xD800)*0x400+(B-0xDC00)
    ;   Code is A
    ),
    get_code(In, C).
string_escape(0'U, In, C, Code) :-
    !,
    get_hhhh(In, Code0),
    get_hhhh(In, Code1),
    Code is Code0 << 16 + Code1,
    get_code(In, C).
string_escape(C, _, _, _) :-
    nb_getval(line_number, Ln),
    atom_codes(A, [0'\\, C]),
    throw(illegal_string_escape_sequence(A, line(Ln))).

get_hhhh(In, Code) :-
    get_code(In, C1),
    code_type(C1, xdigit(D1)),
    get_code(In, C2),
    code_type(C2, xdigit(D2)),
    get_code(In, C3),
    code_type(C3, xdigit(D3)),
    get_code(In, C4),
    code_type(C4, xdigit(D4)),
    Code is D1<<12+D2<<8+D3<<4+D4.

language(C0, In, C, [C0|Codes]) :-
    code_type(C0, lower),
    get_code(In, C1),
    lwr_word(C1, In, C2, Codes, Tail),
    sub_langs(C2, In, C, Tail, []).

lwr_word(C0, In, C, [C0|T0], T) :-
    code_type(C0, lower),
    !,
    get_code(In, C1),
    lwr_word(C1, In, C, T0, T).
lwr_word(C, _, C, T, T).

sub_langs(0'-, In, C, [0'-, C1|Codes], T) :-
    get_code(In, C1),
    lwrdig(C1),
    !,
    get_code(In, C2),
    lwrdigs(C2, In, C3, Codes, Tail),
    sub_langs(C3, In, C, Tail, T).
sub_langs(C, _, C, T, T).

lwrdig(C) :-
    code_type(C, lower),
    !.
lwrdig(C) :-
    code_type(C, digit).

lwrdigs(C0, In, C, [C0|T0], T) :-
    lwrdig(C0),
    !,
    get_code(In, C1),
    lwr_word(C1, In, C, T0, T).
lwrdigs(C, _, C, T, T).

iri_chars(0'>, In, C, []) :-
    !,
    get_code(In, C).
iri_chars(0'\\, In, C, D) :-
    !,
    get_code(In, C1),
    iri_escape(C1, In, C2, H),
    \+non_iri_char(H),
    (   current_prolog_flag(windows, true),
        H > 0xFFFF
    ->  E is (H-0x10000)>>10+0xD800,
        F is (H-0x10000) mod 0x400+0xDC00,
        D = [E, F|T]
    ;   D = [H|T]
    ),
    iri_chars(C2, In, C, T).
iri_chars(0'%, In, C, [0'%, C1, C2|T]) :-
    !,
    get_code(In, C1),
    code_type(C1, xdigit(_)),
    get_code(In, C2),
    code_type(C2, xdigit(_)),
    get_code(In, C3),
    iri_chars(C3, In, C, T).
iri_chars(-1, _, _, _) :-
    !,
    fail.
iri_chars(C0, In, C, D) :-
    \+non_iri_char(C0),
    (   current_prolog_flag(windows, true),
        C0 > 0xFFFF
    ->  E is (C0-0x10000)>>10+0xD800,
        F is (C0-0x10000) mod 0x400+0xDC00,
        D = [E, F|T]
    ;   D = [C0|T]
    ),
    get_code(In, C1),
    iri_chars(C1, In, C, T).

iri_escape(0'u, In, C, Code) :-
    !,
    get_hhhh(In, A),
    (   0xD800 =< A,
        A =< 0xDBFF
    ->  get_code(In, 0'\\),
        get_code(In, 0'u),
        get_hhhh(In, B),
        Code is 0x10000+(A-0xD800)*0x400+(B-0xDC00)
    ;   Code is A
    ),
    get_code(In, C).
iri_escape(0'U, In, C, Code) :-
    !,
    get_hhhh(In, Code0),
    get_hhhh(In, Code1),
    Code is Code0 << 16 + Code1,
    get_code(In, C).
iri_escape(C, _, _, _) :-
    nb_getval(line_number, Ln),
    atom_codes(A, [0'\\, C]),
    throw(illegal_iri_escape_sequence(A, line(Ln))).

non_iri_char(C) :-
    0x00 =< C,
    C =< 0x20,
    !.
non_iri_char(0'<).
non_iri_char(0'>).
non_iri_char(0'").
non_iri_char(0'{).
non_iri_char(0'}).
non_iri_char(0'|).
non_iri_char(0'^).
non_iri_char(0'`).
non_iri_char(0'\\).

name(C0, In, C, Atom) :-
    name_start_char(C0),
    get_code(In, C1),
    name_chars(C1, In, C, T),
    atom_codes(Atom, [C0|T]).

name_start_char(C) :-
    pn_chars_base(C),
    !.
name_start_char(0'_).
name_start_char(C) :-
    code_type(C, digit).

name_chars(0'., In, C, [0'.|T]) :-
    peek_code(In, C1),
    pn_chars(C1),
    !,
    get_code(In, C1),
    name_chars(C1, In, C, T).
name_chars(C0, In, C, [C0|T]) :-
    pn_chars(C0),
    !,
    get_code(In, C1),
    name_chars(C1, In, C, T).
name_chars(C, _, C, []).

pn_chars_base(C) :-
    code_type(C, alpha),
    !.
pn_chars_base(C) :-
    0xC0 =< C,
    C =< 0xD6,
    !.
pn_chars_base(C) :-
    0xD8 =< C,
    C =< 0xF6,
    !.
pn_chars_base(C) :-
    0xF8 =< C,
    C =< 0x2FF,
    !.
pn_chars_base(C) :-
    0x370 =< C,
    C =< 0x37D,
    !.
pn_chars_base(C) :-
    0x37F =< C,
    C =< 0x1FFF,
    !.
pn_chars_base(C) :-
    0x200C =< C,
    C =< 0x200D,
    !.
pn_chars_base(C) :-
    0x2070 =< C,
    C =< 0x218F,
    !.
pn_chars_base(C) :-
    0x2C00 =< C,
    C =< 0x2FEF,
    !.
pn_chars_base(C) :-
    0x3001 =< C,
    C =< 0xD7FF,
    !.
pn_chars_base(C) :-
    0xF900 =< C,
    C =< 0xFDCF,
    !.
pn_chars_base(C) :-
    0xFDF0 =< C,
    C =< 0xFFFD,
    !.
pn_chars_base(C) :-
    0x10000 =< C,
    C =< 0xEFFFF.

pn_chars(C) :-
    code_type(C, csym),
    !.
pn_chars(C) :-
    pn_chars_base(C),
    !.
pn_chars(0'-) :-
    !.
pn_chars(0xB7) :-
    !.
pn_chars(C) :-
    0x0300 =< C,
    C =< 0x036F,
    !.
pn_chars(C) :-
    0x203F =< C,
    C =< 0x2040.

local_name(0'\\, In, C, Atom) :-
    !,
    get_code(In, C0),
    reserved_char_escapes(C0),
    get_code(In, C1),
    local_name_chars(C1, In, C, T),
    atom_codes(Atom, [C0|T]).
local_name(0'%, In, C, Atom) :-
    !,
    get_code(In, C0),
    code_type(C0, xdigit(_)),
    get_code(In, C1),
    code_type(C1, xdigit(_)),
    get_code(In, C2),
    local_name_chars(C2, In, C, T),
    atom_codes(Atom, [0'%, C0, C1|T]).
local_name(C0, In, C, Atom) :-
    local_name_start_char(C0),
    get_code(In, C1),
    local_name_chars(C1, In, C, T),
    atom_codes(Atom, [C0|T]).

local_name_chars(0'\\, In, C, [C0|T]) :-
    !,
    get_code(In, C0),
    reserved_char_escapes(C0),
    get_code(In, C1),
    local_name_chars(C1, In, C, T).
local_name_chars(0'%, In, C, [0'%, C0, C1|T]) :-
    !,
    get_code(In, C0),
    code_type(C0, xdigit(_)),
    get_code(In, C1),
    code_type(C1, xdigit(_)),
    get_code(In, C2),
    local_name_chars(C2, In, C, T).
local_name_chars(0'., In, C, [0'.|T]) :-
    peek_code(In, C1),
    (   local_name_char(C1)
    ;   C1 = 0'.
    ),
    !,
    get_code(In, C1),
    local_name_chars(C1, In, C, T).
local_name_chars(C0, In, C, [C0|T]) :-
    local_name_char(C0),
    !,
    get_code(In, C1),
    local_name_chars(C1, In, C, T).
local_name_chars(C, _, C, []).

local_name_start_char(C) :-
    name_start_char(C),
    !.
local_name_start_char(0':).
local_name_start_char(0'%).
local_name_start_char(0'\\).

local_name_char(C) :-
    pn_chars(C),
    !.
local_name_char(0':).
local_name_char(0'%).
local_name_char(0'\\).

reserved_char_escapes(0'~).
reserved_char_escapes(0'.).
reserved_char_escapes(0'-).
reserved_char_escapes(0'!).
reserved_char_escapes(0'$).
reserved_char_escapes(0'&).
reserved_char_escapes(0'').
reserved_char_escapes(0'().
reserved_char_escapes(0')).
reserved_char_escapes(0'*).
reserved_char_escapes(0'+).
reserved_char_escapes(0',).
reserved_char_escapes(0';).
reserved_char_escapes(0'=).
reserved_char_escapes(0'/).
reserved_char_escapes(0'?).
reserved_char_escapes(0'#).
reserved_char_escapes(0'@).
reserved_char_escapes(0'%).
reserved_char_escapes(0'_).

punctuation(0'(, '(').
punctuation(0'), ')').
punctuation(0'[, '[').
punctuation(0'], ']').
punctuation(0',, ',').
punctuation(0':, ':').
punctuation(0';, ';').
punctuation(0'{, '{').
punctuation(0'}, '}').
punctuation(0'?, '?').
punctuation(0'!, '!').
punctuation(0'^, '^').
punctuation(0'=, '=').
punctuation(0'<, '<').
punctuation(0'>, '>').
punctuation(0'$, '$').

skip_line(-1, _, -1) :-
    !.
skip_line(0xA, In, C) :-
    !,
    cnt(line_number),
    get_code(In, C).
skip_line(0xD, In, C) :-
    !,
    get_code(In, C).
skip_line(_, In, C) :-
    get_code(In, C1),
    skip_line(C1, In, C).

white_space(0x9).
white_space(0xA) :-
    cnt(line_number).
white_space(0xD).
white_space(0x20).



% utils




def_pfx('math:', '<http://www.w3.org/2000/10/swap/math#>').
def_pfx('e:', '<http://eulersharp.sourceforge.net/2003/03swap/log-rules#>').
def_pfx('list:', '<http://www.w3.org/2000/10/swap/list#>').
def_pfx('xsd:', '<http://www.w3.org/2001/XMLSchema#>').
def_pfx('log:', '<http://www.w3.org/2000/10/swap/log#>').
def_pfx('r:', '<http://www.w3.org/2000/10/swap/reason#>').
def_pfx('rdfs:', '<http://www.w3.org/2000/01/rdf-schema#>').
def_pfx('time:', '<http://www.w3.org/2000/10/swap/time#>').
def_pfx('rdf:', '<http://www.w3.org/1999/02/22-rdf-syntax-ns#>').
def_pfx('string:', '<http://www.w3.org/2000/10/swap/string#>').
def_pfx('owl:', '<http://www.w3.org/2002/07/owl#>').
def_pfx('n3:', '<http://www.w3.org/2004/06/rei#>').

put_pfx(_, URI) :-
    atomic_list_concat(['<', URI, '>'], U),
    pfx(_, U),
    !.
put_pfx(_, URI) :-
    atomic_list_concat(['<', URI, '>'], U),
    def_pfx(Pf, U),
    \+pfx(Pf, _),
    !,
    assertz(pfx(Pf, U)).
put_pfx(Pf, URI) :-
    atomic_list_concat(['<', URI, '>'], U),
    fresh_pf(Pf, Pff),
    assertz(pfx(Pff, U)).

fresh_pf(Pf, Pfx) :-
    atom_concat(Pf, ':', Pfx),
    \+pfx(Pfx, _),
    !.
fresh_pf(_, Pfx) :-
    gensym(ns, Pfn),
    fresh_pf(Pfn, Pfx).



cnt(A) :-
    nb_getval(A, B),
    C is B+1,
    nb_setval(A, C),
    (   flag('debug-cnt'),
        C mod 10000 =:= 0
    ->  format(user_error, '~w = ~w~n', [A, C]),
        flush_output(user_error)
    ;   true
    ).

cnt(A, I) :-
    nb_getval(A, B),
    C is B+I,
    nb_setval(A, C),
    (   flag('debug-cnt'),
        C mod 10000 =:= 0
    ->  format(user_error, '~w = ~w~n', [A, C]),
        flush_output(user_error)
    ;   true
    ).


absolute_uri('-', '-') :-
    !.
absolute_uri(A, B) :-
    (   is_absolute_url(A)
    ->  B = A
    ;   absolute_file_name(A, C),
        prolog_to_os_filename(D, C),
        atom_codes(D, E),
        subst([[[0x20], [0'%, 0'2, 0'0]]], E, F),
        atom_codes(G, F),
        (   current_prolog_flag(windows, true)
        ->  atomic_list_concat(['file:///', G], B)
        ;   atomic_list_concat(['file://', G], B)
        )
    ).

:- if(current_predicate(uri_resolve/3)).
resolve_uri(A, _, A) :-
    uri_is_global(A),
    !.
resolve_uri('', A, A) :-
    !.
resolve_uri(A, B, C) :-
    sub_atom(A, 0, 1, _, '?'),
    (   sub_atom(B, I, 1, _, '?')
    ->  true
    ;   atom_length(B, I)
    ),
    sub_atom(B, 0, I, _, D),
    atomic_list_concat([D, A], C),
    !.
resolve_uri(A, B, C) :-
    sub_atom(A, 0, 1, _, '#'),
    (   sub_atom(B, I, 1, _, '#')
    ->  true
    ;   atom_length(B, I)
    ),
    sub_atom(B, 0, I, _, D),
    atomic_list_concat([D, A], C),
    !.
resolve_uri(A, B, C) :-
    uri_resolve(A, B, C).
:-else.
resolve_uri(A, _, A) :-
    sub_atom(A, _, 1, _, ':'),
    !.
resolve_uri('', A, A) :-
    !.
resolve_uri('#', A, B) :-
    !,
    atomic_list_concat([A, '#'], B).
resolve_uri(A, B, A) :-
    \+sub_atom(B, _, 1, _, ':'),
    !.
resolve_uri(A, B, C) :-
    so_uri(U),
    atom_length(U, V),
    sub_atom(A, 0, 1, _, '#'),
    sub_atom(B, 0, V, _, U),
    !,
    atomic_list_concat([B, A], C).
resolve_uri(A, B, C) :-
    sub_atom(A, 0, 2, _, './'),
    !,
    sub_atom(A, 2, _, 0, R),
    resolve_uri(R, B, C).
resolve_uri(A, B, C) :-
    sub_atom(A, 0, 3, _, '../'),
    !,
    sub_atom(A, 3, _, 0, R),
    so_uri(U),
    atom_length(U, V),
    sub_atom(B, 0, V, D, U),
    sub_atom(B, V, D, _, E),
    (   sub_atom(E, F, 1, G, '/'),
        sub_atom(E, _, G, 0, H),
        \+sub_atom(H, _, _, _, '/'),
        K is V+F
    ->  sub_atom(B, 0, K, _, S)
    ;   S = B
    ),
    resolve_uri(R, S, C).
resolve_uri(A, B, C) :-
    so_uri(U),
    atom_length(U, V),
    sub_atom(A, 0, 1, _, '/'),
    sub_atom(B, 0, V, D, U),
    sub_atom(B, V, D, _, E),
    (   sub_atom(E, F, 1, _, '/')
    ->  sub_atom(E, 0, F, _, G)
    ;   G = E
    ),
    !,
    atomic_list_concat([U, G, A], C).
resolve_uri(A, B, C) :-
    so_uri(U),
    atom_length(U, V),
    sub_atom(B, 0, V, D, U),
    sub_atom(B, V, D, _, E),
    (   sub_atom(E, F, 1, G, '/'),
        sub_atom(E, _, G, 0, H),
        \+sub_atom(H, _, _, _, '/')
    ->  sub_atom(E, 0, F, _, I)
    ;   I = E
    ),
    !,
    atomic_list_concat([U, I, '/', A], C).
resolve_uri(A, _, _) :-
    nb_getval(line_number, Ln),
    throw(unresolvable_relative_uri(A, after_line(Ln))).
:- endif.

conj_list(true, []) :-
    !.
conj_list(A, [A]) :-
    A \= (_, _),
    A \= false,
    !.
conj_list((A, B), [A|C]) :-
    conj_list(B, C).

maybe_prolog_verb(S, Name) :-
    (   atom(S),
        atom_concat('\'<http://eulersharp.sourceforge.net/2003/03swap/prolog#', A, S),
        atom_concat(B, '>\'', A)
    ->  (   B = conjunction
        ->  Pred = '\',\''
        ;   (   B = disjunction
            ->  Pred = '\';\''
            ;   (   prolog_sym(B, Pred, _)
                ->  true
                ;   nb_getval(line_number, Ln),
                    throw(invalid_prolog_builtin(B, after_line(Ln)))
                )
            )
        ),
        Name = prolog:Pred
    ;   Name = S
    ).

numeral([0'-, 0'.|A], [0'-, 0'0, 0'.|A]) :-
    !.
numeral([0'+, 0'.|A], [0'+, 0'0, 0'.|A]) :-
    !.
numeral([0'.|A], [0'0, 0'.|A]) :-
    !.
numeral(A, B) :-
    append([C, [0'., 0'e], D], A),
    append([C, [0'., 0'0, 0'e], D], B),
    !.
numeral(A, B) :-
    append([C, [0'., 0'E], D], A),
    append([C, [0'., 0'0, 0'E], D], B),
    !.
numeral(A, B) :-
    last(A, 0'.),
    append(A, [0'0], B),
    !.
numeral(A, A).


del([], _, []).
del([A|B], C, D) :-
    copy_term_nat(A, Ac),
    copy_term_nat(C, Cc),
    unify(Ac, Cc),
    !,
    del(B, C, D).
del([A|B], C, [A|D]) :-
    del(B, C, D).

escape_squote([], []) :-
    !.
escape_squote([0''|A], [0'\\, 0''|B]) :-
    !,
    escape_squote(A, B).
escape_squote([A|B], [A|C]) :-
    escape_squote(B, C).


escape_string([], []) :-
    !.
escape_string([0'\t|A], [0'\\, 0't|B]) :-
    !,
    escape_string(A, B).
escape_string([0'\b|A], [0'\\, 0'b|B]) :-
    !,
    escape_string(A, B).
escape_string([0'\n|A], [0'\\, 0'n|B]) :-
    !,
    escape_string(A, B).
escape_string([0'\r|A], [0'\\, 0'r|B]) :-
    !,
    escape_string(A, B).
escape_string([0'\f|A], [0'\\, 0'f|B]) :-
    !,
    escape_string(A, B).
escape_string([0'"|A], [0'\\, 0'"|B]) :-
    !,
    escape_string(A, B).
escape_string([0'\\|A], [0'\\, 0'\\|B]) :-
    !,
    escape_string(A, B).
escape_string([A|B], [A|C]) :-
    escape_string(B, C).

subst(_, [], []).
subst(A, B, C) :-
    member([D, E], A),
    append(D, F, B),
    !,
    append(E, G, C),
    subst(A, F, G).
subst(A, [B|C], [B|D]) :-
    subst(A, C, D).

unify(A, B) :-
    nonvar(A),
    A = exopred(P, S, O),
    (   (   nonvar(B)
        ;   nonvar(P)
        )
    ->  (   nonvar(P)
        ->  atom(P)
        ;   true
        ),
        B =.. [P, T, R],
        atom(P),
        unify(S, T),
        unify(O, R)
    ;   B = exopred(P, T, R),
        unify(S, T),
        unify(O, R)
    ),
    !.
unify(A, B) :-
    nonvar(B),
    B = exopred(P, S, O),
    (   (   nonvar(A)
        ;   nonvar(P)
        )
    ->  (   nonvar(P)
        ->  atom(P)
        ;   true
        ),
        A =.. [P, T, R],
        atom(P),
        unify(S, T),
        unify(O, R)
    ;   A = exopred(P, T, R),
        unify(S, T),
        unify(O, R)
    ),
    !.
unify(A, B) :-
    is_list(A),
    !,
    getlist(B, C),
    C = A.
unify(A, B) :-
    is_list(B),
    !,
    getlist(A, C),
    C = B.
unify(A, B) :-
    nonvar(A),
    nonvar(B),
    (   A = (_, _)
    ;   B = (_, _)
    ),
    !,
    conj_list(A, C),
    conj_list(B, D),
    includes(C, D),
    includes(D, C).
unify(A, B) :-
    nonvar(A),
    nonvar(B),
    A =.. [P, S, O],
    B =.. [P, T, R],
    !,
    unify(S, T),
    unify(O, R).
unify(A, A).


getlist(A, A) :-
    var(A),
    !.
getlist(set(A), A) :-
    !.
getlist('<http://www.w3.org/1999/02/22-rdf-syntax-ns#nil>', []) :-
    !.
getlist([], []) :-
    !.
getlist([A|B], [C|D]) :-
    getlist(A, C),
    !,
    getlist(B, D).
getlist([A|B], [A|D]) :-
    !,
    getlist(B, D).
getlist(A, [B|C]) :-
    '<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'(A, B),
    '<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'(A, D),
    getlist(D, C).

includes(_, []) :-
    !.
includes(X, [Y|Z]) :-
    member(U, X),
    unify(U, Y),
    includes(X, Z).

