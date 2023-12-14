%:- module(_, []).

:- b_setval(context, []).


:- use_module(library(record)).
:- use_module(library(semweb/rdf11),except(['{}'/1])).
:- use_module(library(semweb/turtle)).
:- use_module(library(http/json)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(yall)).
:- use_module(library(debug)).


% https://github.com/SWI-Prolog/swipl-devel/issues/715#issuecomment-731019516
%:- use_module(library(clpq), [{}/1]).
%:- use_module(library(clpq), []).
:- use_module(library(clpr), []).
:- use_module(library(clpq)).


:- use_module(library(fnotation)).
:- fnotation_ops($>,<$).
:- op(900,fx,<$).




/* comment out a goal, structurally. */
:- op(920,fx,##).
##_Goal.



%%:- use_module(envvars).
:- [exceptions].
:- [envvars].
:- [checks].



:- multifile determinancy_checker_throw_error/1.
 determinancy_checker_throw_error(X) :- throw_string(X),!.
%:- use_module('../determinancy_checker/determinancy_checker_main.pl').
:- ['../determinancy_checker/determinancy_checker_main.pl'].
%:- asserta((determinancy_checker_throw_error(X) :- throw_string(X),!)).



:- multifile user:goal_expansion/2.
:- dynamic user:goal_expansion/2.



:- [checklist].
:- [compare_xml].
:- [compile_with_variable_names_preserved].
:- [days].
:- [dict_vars].
:- [doc].
:- [execution_context].
:- [files].
:- [globals].
:- [higher_order].
:- [json].
:- [magic_formula].
:- [numbers].
:- [pacioli].
:- [request_files].
:- [services].
:- [string_manipulation].
:- [structured_xml].
:- [structures].
:- [term_dict].
:- [term_output].
:- [vector_string].
:- [xml].
