
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).


 services_server(S) :-
	!getenv('SERVICES_URL',S),
	%!current_prolog_flag(services_server, S),
	debug(d, 'services_server = ~q', [S]).

 url_parts(Url, Parts) :-
	parse_url(Url, X),
	member(path('/'), X),
	member(protocol(S), X),
	member(host(H), X),
	member(port(P), X),
	[scheme(S), host(H), port(P)] = Parts,
	true.

 json_post(Url, Payload, Response) :-
	json_post(Url, Payload, Response, 5).

 json_post(Url, Payload, Response, Max_retries) :-
	json_post(Url, Payload, Response, Max_retries, Max_retries).

 json_post(Url, Payload, Response, Max_retries, Retries) :-
	Options = [content_type('application/json'), json_object(dict)],
	%format(user_error, '~q~n', [http_post(Url, json(Payload), Response, Options),
	catch(
		http_post(Url, json(Payload), Response, Options),
		E,
		(
			(	(
					E = error(socket_error(eai_again,Msg),_),
					Retries > 0
				)
			->	(
					%debug(shell, '~q', Msg),
					format(user_error, '~q', Msg),
					Sleep is Max_retries - Retries,
					sleep(Sleep),
					Next_retries is Retries - 1,
					json_post(Url, Payload, Response, Max_retries, Next_retries)
				)
			;	throw(
					during(
						E,
						http_post(Url, json(Payload), Response, Options)
					)
				)
			)
		)
	).



 services_server_shell_cmd(Cmd) :-
	format(string(Url), '~w/shell', [$>services_server(<$)]),
	debug(shell, 'POST: ~w', Url),
	json_post(Url, _{cmd:Cmd}, _).



/*shell4: to be used probably everywhere instead of shell2 or swipl shell.
swipl shell has a bug making it stuck for long time */

 shell4(Cmd_In, Exit_Status) :-
	%format(user_error, 'shell4: ~q ...\n', [Cmd_In]),
	services_server_shell_cmd(Cmd_In),Exit_Status=0,
	%format(user_error, 'shell4: done\n', []),
	true.



 shell2(Cmd) :-
	shell2(Cmd, _).

 shell2(Cmd_In, Exit_Status) :-
	shell3(Cmd_In, [exit_status(Exit_Status)]).

 shell3(Cmd_In, Options) :-
	flatten([Cmd_In], Cmd_Flat),
	atomic_list_concat(Cmd_Flat, " ", Cmd),

	(	memberchk(print_command(true), Options)
	->	format(user_error, '~w\n\n', [Cmd])
	;	true),

	shell(Cmd, Exit_Status),

	(	memberchk(exit_status(E), Options)
	->	E = Exit_Status
	;	true),

	/* this option allows to pass the exact command string used back to the calling predicate */
	(	memberchk(command(C), Options)
	->	C = Cmd
	;	true).



/* for gnome-terminal and ..? */
 print_clickable_link(Url, Title) :-
	/* todo replace this with write */
	atomics_to_string([">&2 printf '\e]8;;", Url,"\e\\   ", Title, "   \e]8;;\e\\\n'"],  S),
	shell2(S,_).
