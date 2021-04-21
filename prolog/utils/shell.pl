
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).


services_server(S) :-
	current_prolog_flag(services_server, S).

json_post(Url, Payload, Response) :-
	%format(user_error, '~q~n', [http_post(Url, json(Payload), Response, [content_type('application/json'), json_object(dict)])]),
	catch(
		http_post(
			Url,
			json(Payload),
			Response,
			[content_type('application/json'), json_object(dict)]
		),
		E,
		(
			(	E = error(socket_error(eai_again,Msg),_)
			->	(
					%debug(shell, '~q', Msg),
					format(user_error, '~q', Msg),
					sleep(1),
					json_post(Url, Payload, Response)
				)
			;	throw(
					during(
						E,
						http_post(Url, json(Payload), Response, [content_type('application/json'), json_object(dict)])
					)
				)
			)
		)
	).

services_server_shell_cmd(Cmd) :-
	format(string(Url), '~w/shell/rpc/', [$>services_server(<$)]),
	debug(shell, 'POST: ~w', Url),
	json_post(Url, _{cmd:Cmd,quiet_success:true}, _).

/* shell4: to be used probably everywhere instead of shell2 or swipl shell.
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
