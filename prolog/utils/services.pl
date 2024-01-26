
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).


 services_server(S) :-
	!flag('SERVICES_URL',S),
	debug(d, 'services_server = ~q', [S]).

 download_bastion_server(S) :-
	!flag('DOWNLOAD_BASTION_URL',S),
	debug(d, 'download_bastion_server = ~q', [S]).

 url_parts(Url, Parts) :-
	parse_url(Url, X),
	member(path('/'), X),
	member(protocol(S), X),
	member(host(H), X),
	member(port(P), X),
	[scheme(S), host(H), port(P)] = Parts,
	true.






 json_post_result(Url, Payload, Result) :-
 	json_post(Url, Payload, Response),
 	%format(user_error, 'json_post_result: ~q\n', [Response]),
	(	Error = Response.get(error)
	->	throw_string(Error)
	;	Result = Response.get(result)).



 json_post(Url, Payload, Response) :-
	json_post(Url, Payload, Response, 10).



 json_post(Url, Payload, Response, Max_retries) :-
	json_post(Url, Payload, Response, Max_retries, Max_retries).



 json_post(Url0, Payload, Response, Max_retries, Retries_left) :-
	(
		string(Url0)
	->	Url = Url0
	;	atomic_list_concat(Url0, Url)
	),

	Options = [content_type('application/json'), json_object(dict), connection('Keep-Alive'), timeout(600)],
	%format(user_error, '~q~n', [http_post(Url, json(Payload), Response, Options),
	catch(
		http_post(Url, json(Payload), Response, Options),
		E,
		(
			(	(
					%E = error(socket_error(_/*econnreset,eai_again,..*/,Msg),_),
					E = error(_,context(_, Msg)),

					Retries_left > 0
				)
			->	(
					%debug(shell, '~q', Msg),
					format(user_error, '~q', Msg),
					Sleep is Max_retries - Retries_left,
					sleep(Sleep),
					Next_retries_left is Retries_left - 1,
					json_post(Url, Payload, Response, Max_retries, Next_retries_left)
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
	json_post(['http://localhost:1111/shell'], _{cmd:Cmd}, _).



 services_post_result(Path, Params, Result) :-
	json_post_result([$>services_server, '/', Path], Params, Result).




 shell4(Cmd_In, Exit_Status) :-

	/*
	shell4: to be used probably everywhere instead of shell2 or swipl shell.
	swipl shell has a bug making it stuck for long time
	*/

	%format(user_error, 'shell4: ~q ...\n', [Cmd_In]),
	services_server_shell_cmd(Cmd_In),
	Exit_Status=0,
	%format(user_error, 'shell4: done\n', []),
	true.



 get_file_from_url_into_dir(loc(absolute_url, Url), loc(absolute_path, Path), Filepath) :-
	json_post_result([$>download_bastion_server(<$), '/get_file_from_url_into_dir'], _{url: Url, dir: Path}, Result),
	_{filepath: Filepath} :< Result.
