:- b_setval(context, []).


:- use_module(library(semweb/rdf11),except(['{}'/1])).
:- use_module(library(semweb/turtle)).
:- use_module(library(http/json)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(yall)).
:- use_module(library(debug)).

:- use_module(library(fnotation)).
:- fnotation_ops($>,<$).
:- op(900,fx,<$).

:- use_module('../determinancy_checker/determinancy_checker_main.pl').
:- assert(user:determinancy_checker_thrower(throw_string)).

:- multifile user:goal_expansion/2.
:- dynamic user:goal_expansion/2.

:- [compare_xml].
:- [compile_with_variable_names_preserved].
:- [dict_vars].
:- [doc].
:- [exceptions].
:- [execution_context].
:- [files].
:- [higher_order].
:- [json].
:- [magic_formula].
:- [numbers].
:- [request_files].
:- [shell].
:- [string_manipulation].
:- [structured_xml].
:- [structures].
:- [term_output].
:- [xml].
