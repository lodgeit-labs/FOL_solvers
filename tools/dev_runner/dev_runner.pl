#!/usr/bin/env swipl

%:- ['../../prolog/determinancy_checker/determinancy_checker_main.pl'].
:- ['../../prolog/utils/utils'].

/*
not all permutations of compilation and toplevel options are implemented. Compilation would be useful if swipl reported all errors, including DCG errors, upon loading, without a need to call 'make'. Right now, it is useless, and it gives no speed improvement.
*/

:- multifile user:message_hook/3.
user:message_hook(initialization_error(_,X,_),Kind,_) :- print_message(Kind,X),halt(1).
user:message_hook(string(S),_,_) :- format(user_error,'ERROR: ~w~n', [S]).

/*
running this takes a while because it first does just a load to find compile errors, and then runs swipl again to actually execute goal. Maybe next version will consult Script directly, but idk how to eval() a goal, except by parsing it first..
*/

shell2(Cmd) :-
	shell2(Cmd, _).

:- debug(dev_runner).

shell2(Cmd_In, Exit_Status) :-
	flatten([Cmd_In], Cmd_Flat),
	atomic_list_concat(Cmd_Flat, Cmd),
	debug(dev_runner, '~w\n\n', [Cmd]),
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
	debug(dev_runner, 'dev_runner: starting...\n', []),
	Spec = [
		[opt(script), type(atom), shortflags([s]), longflags([script]),
			help('the .pl file you wish to run')]
		,[opt(goal), type(atom), shortflags([g]), longflags([goal]),
			help('the -g you wish to pass to swipl')]
		%,[opt(args), type(atom), default(''), shortflags([a]), longflags([args]),
		%	help('the command-line arguments you wish to pass to your script')]
		,[opt(debug), type(boolean), default(true), shortflags([d]), longflags([debug]),
			help('run swipl with -O?')]
		,[opt(compile), type(boolean), default(false), shortflags([c]), longflags([compile]),
			help('compile first?')]
		,[opt(toplevel), type(boolean), default(false), shortflags([t]), longflags([toplevel]),
			help('pass goal interactively into toplevel instead of with -g? Allows guitracer to run after exception.')]
		,[opt(halt_on_problems), type(boolean), default(true), shortflags([h]), longflags([halt_on_problems])]
		,[opt(problem_lines_whitelist), type(atom), longflags([problem_lines_whitelist])]
		,[opt(viewer), type(atom), shortflags([v]), longflags([viewer]),
			help('invoke a program on the stdout output of your .pl file')]
		,[opt(clear_terminal), type(boolean), default(false), longflags([clear_terminal])]
	],
	% accept positional arguments after a '--'. These will be the arguments passed to the script.

	current_prolog_flag(argv, Args),
	!split_list_by_last_occurence_of(Args,'--',Args2, ScriptArgs),
	opt_parse(Spec, Args2, Opts, []),

	assert(opts(Opts)),
	assert(scriptargs(ScriptArgs)),
	memberchk(debug(Debug), Opts),
	memberchk(viewer(Viewer), Opts),
	memberchk(script(Script), Opts),
	(nonvar(Script)->true;throw(string('--script needed'))),
	maybe_clean_terminal,
	optimization_flag(Debug, Optimization),
	check_syntax(Optimization, Script),

	(	memberchk(compile(true), Opts)
	->	run_with_compilation(Optimization, Script, Viewer)
	;	run_without_compilation(Debug, Optimization, Script, Viewer)).

/*


		)
	;	(
			format(user_error, 'dev_runner: running without -g...\n', [])
			shell2([Load_Cmd],Exit)
		)
	)
*/


split_list_by_last_occurence_of(In,Separator,Before,After) :-
	reverse(In, In2),
	split_list_by_last_occurence_of2(In2,Separator,Before0, After0),
	reverse(Before0, After),
	reverse(After0, Before).

split_list_by_last_occurence_of2([Separator|L1t],Separator,L1t,[]) :- !.

split_list_by_last_occurence_of2([H|L1t],Separator,Before,[H|L2t]) :-
	dif(H,Separator),
	split_list_by_last_occurence_of2(L1t,Separator,Before,L2t),
	!.

split_list_by_last_occurence_of2([],_,[],[]) :- !.


optimization_flag(Debug, Optimization) :-
	(	Debug = true
	->	(
			Optimization = ' --debug=true ',
			debug(dev_runner, 'dev_runner: debug is true, no -O...\n', [])
		)
	;	(
			Optimization = ' -O ',
			debug(dev_runner, 'dev_runner: debug is false, using -O...\n', [])
		)
	).



check_syntax(Optimization, Script) :-
	/* make forces compilation of dcg's or something. Ideally, we would have two steps: 1)compile 2) run the compiled file. But for this i'd like to review what kind of dcg declaration errors 'make' reported that just loading the prolog file didnt, because we would lose that reporting.  */
	make_temp_err_file,
	atomic_list_concat(['swipl ', Optimization, ' -s ', Script], Load_Cmd),
	get_flag(err_file, Err_File),
	debug(dev_runner, 'dev_runner: checking syntax...\n', []),

	opts(Opts),
	(	(memberchk(problem_lines_whitelist(Whitelist_File), Opts), nonvar(Whitelist_File))
	->	Err_Grep = ['| grep -v -x -F -f ', Whitelist_File]
	;	Err_Grep = ''),

	%shell2([Load_Cmd, ' -g "make,halt."  2>&1  |  tee ', Err_File, Err_Grep, ' | head -n 150 1>&2']),
	shell2([Load_Cmd, ' -g "halt."  2>&1  |  tee ', Err_File, Err_Grep, ' | head -n 150 1>&2']),
	maybe_halt_on_problems,
	debug(dev_runner, 'dev_runner: syntax seems ok...\n', []).



run_with_compilation(Optimization, Script, Viewer) :-
	opts(Opts),
	debug(dev_runner, 'dev_runner: compiling...\n', []),
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

			%shell2(['/usr/bin/time --f "user time :%U secs, max ram:%M KB" ./a.out', Redirection], Execution_exit_status),
			shell2(['/usr/bin/time -v ./a.out', Redirection], Execution_exit_status),

			(	Execution_exit_status = 0
			->	true
			;	(format(user_error, 'dev_runner: script exited with non-zero status\n', []), halt(Execution_exit_status))),

			(	nonvar(Viewer)
			->	(maybe_halt_on_problems, shell2([Viewer, ' arrr.xml']))
			;	true)

		)
	),
	halt.


run_without_compilation(Debug, Optimization, Script, Viewer) :-
	opts(Opts),
	debug(dev_runner, 'dev_runner: running script...\n', []),
	memberchk(goal(Goal), Opts),

	% if goal is passed and toplevel(true), compile without goal, otherwise compile Goal in
	(	nonvar(Goal)
	->	(	memberchk(toplevel(true), Opts)
		->	Execution_goal = ' '
		;	atomic_list_concat([' -g "', Goal, '." '], Execution_goal))
	;	(
			Execution_goal = '',
			debug(dev_runner, 'dev_runner: running without goal...\n', [])
		)
	),

	(	memberchk(toplevel(true), Opts)
	->	run_with_toplevel(Debug, Goal, Script, Opts)
	;	(
			(	nonvar(Viewer)
			->	Redirection = [' 2>&1  1> arrr.xml']
			;	Redirection = ''),

			(	(memberchk(problem_lines_whitelist(Whitelist_File), Opts), nonvar(Whitelist_File))
			%	http://burgerbum.com/stderr_pipe.html
			->	Err_Grep = [' 3>&1 1>&2 2>&3 | grep -v -x -F -f ', Whitelist_File, ' ) 3>&1 1>&2 2>&3']
			;	Err_Grep = ')'),

			(	getenv('MPROF_OUTPUT_PATH', MPROF_OUTPUT_PATH)
			->	true
			;	MPROF_OUTPUT_PATH = '/app/server_root/tmp/mem_prof'),

			% shell2(["(/usr/bin/time -v mprof run --nopython -C -E -o ", MPROF_OUTPUT_PATH, " swipl --stack_limit=100G ", Optimization, Execution_goal, ' -s ', Script, Redirection, Err_Grep], Exit_status),
			% ^ not sure how this worked, mprof prints to stdout, even in older versions, let's for it and change the two lines
			shell2(["(/usr/bin/time -v swipl --stack_limit=100G ", Optimization, Execution_goal, ' -s ', Script, Redirection, Err_Grep], Exit_status),

			(	Exit_status = 0
			->	true
			;	(format(user_error, 'dev_runner: script failed\n', []), halt(Exit_status))),

			(	nonvar(Viewer)
			->	(maybe_halt_on_problems, shell2([Viewer, ' arrr.xml']))
			;	true)
		)
	),
	halt.



optimization_flag2(Debug, Optimization) :-
	(	Debug = true
	->	(
			Optimization = ['--debug=true', '--debug-on-interrupt'],
			debug(dev_runner, 'dev_runner: debug is true, no -O...\n', [])
		)
	;	(
			Optimization = ['--debug=false', '-O'],
			debug(dev_runner, 'dev_runner: debug is false, using -O...\n', [])
		)
	).

run_with_toplevel(Debug, Goal, Script, _Opts) :-
	optimization_flag2(Debug, Optimization),
	scriptargs(ScriptArgs),

	(	Debug = true
	->	Environment = environment(['SWIPL_NODEBUG'=false])
	;	Environment = environment(['SWIPL_NODEBUG'=true])),

	(	getenv('MPROF_OUTPUT_PATH', MPROF_OUTPUT_PATH)
	->	true
	;	MPROF_OUTPUT_PATH = '/app/server_root/tmp/mem_prof'),

	Args0 = ['-v',

		'mprof', 'run', '--nopython', '-C', '-E' , '-o', MPROF_OUTPUT_PATH,

	'swipl', '--stack_limit=100G', Optimization, '-s', Script, '--', ScriptArgs],

	flatten(Args0, Args),
	debug(dev_runner, 'dev_runner: will run with toplevel with args: ~q\n', [Args]),
	atomics_to_string(['(',Goal,',halt(0));halt(1).\n'], Goal2),
	debug(dev_runner, 'dev_runner: will pipe goal: ~w\n', [Goal2]),
	%process_create(path(swipl), Args, [process(Pid), stdin(pipe(Stdin))]),
	process_create(path(time), Args, [process(Pid), stdin(pipe(Stdin)), Environment]),
	%write(Stdin, writeln('script output starts below'),
	%write(Stdin, "current_prolog_flag(debug,Debug),format(user_error,'debug=~q~n',[Debug]).\n\n"),
	write(Stdin, Goal2),
	% "a" to cause an abort after an exception. "." to pretend that the "a" was a query, in case tracer isnt on.
	% maybe use -t halt or -t goal instead?
	write(Stdin, 'a. '),
	write(Stdin, 'halt(1).\n'),
	flush_output(Stdin),
	process_wait(Pid,Status),
	(	Status = exit(Exit_status)
	->	halt(Exit_status)
	;	halt(1)).


:- initialization(main).


% todo for python rewrite:  --tty=true -q? pipe goal (not rpc message) to swipl. get g trace working. (g trace is only invoked from the toplevel)

/*
--debug-on-interrupt
    Enable debugging on an interrupt signal (Control-C, SIGINT) immediately. Normally debugging on interrupt is enabled when entering the interactive toplevel. This flag can be used to start the debugger on an interrupt while executing goals from -g or initialization/[1,2]. See also the Prolog flag debug_on_interrupt.
    */
