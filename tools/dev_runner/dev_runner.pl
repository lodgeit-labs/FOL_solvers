#!/usr/bin/env swipl

:- multifile user:message_hook/3.
user:message_hook(initialization_error(_,X,_),Kind,_) :- print_message(Kind,X),halt(1).
user:message_hook(string(S),_,_) :- format(user_error,'ERROR: ~w~n', [S]).

/*
running this takes a while because it first does just a load to find compile errors, and then runs swipl again to actually execute goal. Maybe next version will consult Script directly, but idk how to eval() a goal, except by parsing it first..
*/

shell2(Cmd) :-
	shell2(Cmd, _).

shell2(Cmd_In, Exit_Status) :-
	flatten([Cmd_In], Cmd_Flat),
	atomic_list_concat(Cmd_Flat, Cmd),
	format(user_error, '~w\n\n', [Cmd]),
	shell(Cmd, Exit_Status).

/* call halt_on_problems if halt_on_problems(true) */
maybe_halt_on_problems :-
	opts(Opts),
	memberchk(halt_on_problems(Halt), Opts),
	(	Halt = true
	->	halt_on_problems
	;	true).

/* halt if err_file contains lines other than found in problem_lines_whitelist */
halt_on_problems :-
	get_flag(err_file, Err_File),
	opts(Opts),
	(	(memberchk(problem_lines_whitelist(Whitelist_File), Opts), nonvar(Whitelist_File))
	->	Err_Grep = ['grep -E -i "Warn|err" ', Err_File, ' | grep -q -v -x -F -f ', Whitelist_File]
	;	Err_Grep = ['grep -q -E -i "Warn|err" ', Err_File]
	),
	(	shell2(Err_Grep, 0)
	 ->	(
			format(user_error, "that's an error, halting.\n", []),
	 		halt(1)
		)
	;	true).

maybe_clean_terminal :-
	opts(Opts),
	memberchk(clear_terminal(Clear), Opts),
	(
		Clear = true
	->
		(
			shell2('timeout 0.1 reset'),
			shell2('echo "\e[3J" 1>&2'),
			shell2('timeout 0.1 reset')
		)
	;
		true
	).


make_temp_err_file :-
	tmp_file_stream(text, Err_File, Stream),
	set_flag(err_file, Err_File),
	close(Stream).

main :-
	format(user_error, 'dev_runner: starting...\n', []),
	Spec = [
		[opt(script), type(atom), shortflags([s]), longflags([script]),
			help('the .pl file you wish to run')]
		,[opt(goal), type(atom), shortflags([g]), longflags([goal]),
			help('the -g you wish to pass to your .pl file')]
		,[opt(debug), type(boolean), default(true), shortflags([d]), longflags([debug]),
			help('run swipl with -O or without?')]
		,[opt(compile), type(boolean), default(false), shortflags([c]), longflags([compile]),
			help('compile first?')]
		,[opt(toplevel), type(boolean), default(true), shortflags([t]), longflags([toplevel]),
			help('pass goal interactively into toplevel instead of with -g? Allows gtrace to run after exception')]
		,[opt(halt_on_problems), type(boolean), default(true), shortflags([h]), longflags([halt_on_problems])]
		,[opt(problem_lines_whitelist), type(atom), longflags([problem_lines_whitelist])]
		,[opt(viewer), type(atom), shortflags([v]), longflags([viewer]),
			help('invoke a program on the stdout output of your .pl file')]
		,[opt(clear_terminal), type(boolean), default(false), longflags([clear_terminal])]
	],
	opt_arguments(Spec, Opts, Args),
	(Args = [] -> true ; throw(string('no positional arguments accepted'))),
	assert(opts(Opts)),
	memberchk(debug(Debug), Opts),
	memberchk(viewer(Viewer), Opts),
	memberchk(script(Script), Opts),
	(nonvar(Script)->true;throw(string('--script needed'))),
	maybe_clean_terminal,
	optimization_flag(Debug, Optimization),
	check_syntax(Optimization, Script),

	(	memberchk(compile(true), Opts)
	->	(
			run_with_compilation(Optimization, Script, Viewer)
		)
	;	not_implemented).

/*
			shell2([Load_Cmd, ' -g "', Goal, '." ', Redirection]),

			(	nonvar(Viewer)
			->	(maybe_halt_on_problems, shell2([Viewer, ' arrr.xml']))
			;	true)

		)
	;	(
			format(user_error, 'dev_runner: running without -g...\n', [])
			shell2([Load_Cmd],Exit)
		)
	)
*/


optimization_flag(Debug, Optimization) :-
	(	Debug = true
	->	(
			Optimization = ' ',
			format(user_error, 'dev_runner: debug is true, no -O...\n', [])
		)
	;	(
			Optimization = ' -O ',
			format(user_error, 'dev_runner: debug is false, using -O...\n', [])
		)
	).



check_syntax(Optimization, Script) :-
	/* make forces compilation of dcg's or something. Ideally, we would have two steps: 1)compile 2) run the compiled file. But for this i'd like to review what kind of dcg declaration errors 'make' reported that just loading the prolog file didnt, because we would lose that reporting.  */
	make_temp_err_file,
	atomic_list_concat(['swipl ', Optimization, ' -s ', Script], Load_Cmd),
	get_flag(err_file, Err_File),
	format(user_error, 'dev_runner: checking syntax...\n', []),
	shell2([Load_Cmd, ' -g "make,halt."  2>&1  |  tee ', Err_File, ' | head -n 150 1>&2']),
	maybe_halt_on_problems,
	format(user_error, 'dev_runner: syntax seems ok...\n', []).



run_with_compilation(Optimization, Script, Viewer) :-
	opts(Opts),
	format(user_error, 'dev_runner: compiling...\n', []),
	memberchk(goal(Goal), Opts),

	% if goal is passed and toplevel(true), compile without goal, otherwise compile Goal in
	(	nonvar(Goal)
	->	(	memberchk(toplevel(true), Opts)
		->	Compilation_goal = ' '
		;	atomic_list_concat([' -g "', Goal, '." '], Compilation_goal))
	;	(
			Compilation_goal = '',
			format(user_error, 'dev_runner: no goal specified...\n', [])
		)
	),

	shell2(["swipl", Optimization,  Compilation_goal, ' -o a.out -c ', Script], Compilation_exit_status),
	(	Compilation_exit_status = 0
	->	true
	;	(format(user_error, 'dev_runner: compilation failed\n', []), halt(Compilation_exit_status))),

	(	memberchk(toplevel(true), Opts)
	->	(format(user_error, 'dev_runner: not implemented\n', []), halt(1))
	;	(
			% run the compiled file

			(	nonvar(Viewer)
			->	Redirection = [' 2>&1  1> arrr.xml']
			;	Redirection = ''),

			shell2(['./a.out', Redirection], Execution_exit_status),
			(	Execution_exit_status = 0
			->	true
			;	(format(user_error, 'dev_runner: script exited with non-zero status\n', []), halt(Execution_exit_status)))
		)
	),
	halt.




:- initialization(main).


% todo for python rewrite:  --tty=true -q? pipe goal (not rpc message) to swipl. get gtrace working. (gtrace is only invoked from the toplevel)
