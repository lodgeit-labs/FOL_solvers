/* spawns a server for debugging "doc"..*/


:- use_module(library(http/json)).
:- use_module(library(http/http_host)).
:- use_module(library(http/mimetype), []).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_error)).

:- http_handler(root(.), root, []).

 root(_) :-
	thread_signal(main, doc_dump),
	reply_json(_{'msg':'doc dump requested from main thread. If main thread is stopped in trace, make one step to allow it to run.'}).

:- dynamic(doc_dump_server_is_inited/0).

 init_doc_dump_server :-
 	(	doc_dump_server_is_inited
 	->	true
 	;	(
			assert(doc_dump_server_is_inited),
			format(user_error, 'starting doc-dump server..', []),
			thread_create(
				http_server(
					http_dispatch,
					[
						interactive(true),
						port(1234),
						ip(localhost)
					]
				),
				_
			)
		)
	).
