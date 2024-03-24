:- thread_local user:my_request_tmp_dir/1.
:- thread_local asserted_server_public_url/1.

 set_unique_tmp_directory_name(Name) :-
	retractall(my_request_tmp_dir(_)),
	asserta(my_request_tmp_dir(Name)).

 create_tmp_directory(Dir_Name) :-
	Dir_Name = loc(tmp_directory_name, Dir_Name_Value),
	resolve_specifier(loc(specifier, my_tmp(Dir_Name_Value)), Path),
	%Path = loc(absolute_path, Path_Value),
	ensure_directory_exists(Path),
	symlink_last_to_current(Path).

 symlink_last_to_current(loc(absolute_path, Path)) :-
	resolve_specifier(loc(specifier, my_tmp('last')), loc(_, Last)),
	shell4(['rm', '-f', Last], _),
	shell4(['ln', '-s', Path, Last], _).


/*
  to be used instead of absolute_file_name for request-specific tmp files
*/
 absolute_tmp_path(loc(file_name, File_Name), Absolute_File_Name) :-
	my_tmp_file_path(loc(file_name, File_Name), loc(path_relative_to_tmp, File_Path_Relative_To_Tmp)),
	resolve_specifier(loc(specifier,my_tmp(File_Path_Relative_To_Tmp)), Absolute_File_Name).


% make_zip :-
%% todo replace with https://docs.python.org/3/library/shutil.html#archiving-example
%
%	resolve_specifier(loc(specifier, my_tmp('')), loc(absolute_path, Tmp)),
%	my_request_tmp_dir_path(Tmp_Dir_Path),
%
%	!doc($>request_data, l:request_tmp_directory_name, Request_Files_Tmp_Directory_Name),
%	resolve_specifier(loc(specifier, my_tmp(Request_Files_Tmp_Directory_Name)), loc(absolute_path, Request_Files_Tmp_Directory_Path)),
%
%	atomic_list_concat([Tmp_Dir_Path, '.zip'], Zip_Fn),
%	atomic_list_concat([Tmp_Dir_Path, '/'], Tmp_Dir_With_Slash),
%	atomic_list_concat([Request_Files_Tmp_Directory_Path, '/'], Request_Files_Tmp_Directory_Path_With_Slash),
%	archive_create(Zip_Fn, [Tmp_Dir_With_Slash, Request_Files_Tmp_Directory_Path_With_Slash], [format(zip), directory(Tmp)]),
%	shell4(['mv', Zip_Fn, Tmp_Dir_With_Slash], _).

 copy_request_files_to_tmp(Paths, Names) :-
	maplist(copy_request_file_to_tmp, Paths, Names).

 copy_request_file_to_tmp(Path, Name) :-
	exclude_file_location_from_filename(Path, Name),
	absolute_tmp_path(Name, Tmp_Request_File_Path),
	copy_file_loc(Path, Tmp_Request_File_Path).

 replace_request_with_response(Atom, Response) :-
	atom_string(Atom, String),
	(
		(
			re_replace('request', 'response', String, Response);
			re_replace('Request', 'Response', String, Response);
			re_replace('REQUEST', 'RESPONSE', String, Response)
		),
		String \= Response
	).

/*write_tmp_file(Name, Text) :-
	absolute_tmp_path(Name, Path),
	write_file(Path, Text).

write_tmp_json_file(Name, Json) :-
	dict_json_text(Json, Text),
	write_tmp_file(Name, Text).
*/




 /* this numbering is useful for running "quick" series of reports for debugging, finding an offending transaction.
 But in future it could also be useful for generic nondeterministic calculations, ie, calculators that yield multiple results.*/
  

 nondet_report_fn_key(Fn, Key) :-
	atomic_list_concat(['report_fn_', Fn], Key).

 next_nondet_report_fn(Base, Fn) :-
	next_nondet_report_fn_id(Base, Next_id),
	format(string(Fn), '~|~`0t~w~6|_~w', [Next_id, Base]).
	%atomics_to_string([Next_id,'_',Base], Fn).

 next_nondet_report_fn_id(Base, Next_id) :-
	nondet_report_fn_key(Base, Key),
	(	b_current_num(Key, Id)
	->	Next_id is Id + 1
	;	Next_id is 0).

 bump_nondet_report_fn_id(Base) :-
	next_nondet_report_fn_id(Base, Next_id),
	nondet_report_fn_key(Base, Key),
	nb_setval(Key, Next_id).

 grab_nondet_report_fn(Base, Fn) :-
	next_nondet_report_fn(Base, Fn),
	bump_nondet_report_fn_id(Base).

 report_file_path(Fn0, Url, Path) :-
	report_file_path(Fn0, Url, Path, _).

 report_file_path(
	/* input */
	loc(file_name, Fn0),
	/* output */
	Url,
	Path,
	Fn
) :-
	grab_nondet_report_fn(Fn0, Fn),
	report_file_path__singleton(loc(file_name, Fn), Url, Path).

% report_file_path__singleton(Fn, Url, Path, Final_fn) :-

 report_file_path__singleton(
	/* input */
	loc(file_name, Fn),
	/* output */
	loc(absolute_url,Url),
	Path
) :-
	my_request_tmp_dir(loc(tmp_directory_name, Tmp_Dir_Value)),
	debug(tmp_files, "report_file_path:my_request_tmp_dir(loc(tmp_directory_name, ~w))~n", [Tmp_Dir_Value]),
	server_public_url(Server_Public_Url),
	debug(tmp_files, "report_file_path:server_public_url(~w)~n",[Server_Public_Url]),
	atomic_list_concat([Server_Public_Url, '/tmp/', Tmp_Dir_Value, '/', Fn], Url),
	absolute_tmp_path(loc(file_name, Fn), Path).


 server_public_url(Url) :-
	asserted_server_public_url(Url).

 set_server_public_url(Url) :-
	asserted_server_public_url(Url), !.

 set_server_public_url(Url) :-
	(	asserted_server_public_url(Old)
	->	format(user_error, 'old Server_Public_Url: ~qw\n', [Old])
	;	true),
	%format(user_error, 'Server_Public_Url: ~q\n', [Url]),
	retractall(asserted_server_public_url(_)),
	assert(asserted_server_public_url(Url)).

 my_tmp_file_path(loc(file_name,File_Name), loc(path_relative_to_tmp, File_Path_Relative_To_Tmp)) :-
	my_request_tmp_dir(loc(tmp_directory_name,Tmp_Dir)),
	atomic_list_concat([Tmp_Dir, '/', File_Name], File_Path_Relative_To_Tmp).

 tmp_file_url(File_Name, Url) :-
	server_public_url(Server),
	my_tmp_file_path(File_Name, loc(path_relative_to_tmp, File_Path_Relative_To_Tmp)),
	atomic_list_concat([Server, '/tmp/', File_Path_Relative_To_Tmp], Url).

 tmp_file_path_from_something(FileName, Path) :-
	exclude_file_location_from_filename(FileName, FileName2),
	FileName2 = loc(file_name, FileName2_Value),
	http_safe_file(FileName2_Value, []),
	absolute_tmp_path(FileName2, Path).

 tmp_file_path_to_url(Path, Url) :-
	exclude_file_location_from_filename(Path, Fn),
	debug(tmp_files, "tmp_file_path_to_url:exclude_file_location_from_filename(~w, ~w)~n", [Path,Fn]),
	report_file_path(Fn, Url, _),
	debug(tmp_files, "tmp_file_path_to_url:report_file_path(~w, ~w, ~w)~n", [Fn, Url, _]).

 tmp_file_path_to_url__singleton(Path, Url) :-
	exclude_file_location_from_filename(Path, Fn),
	debug(tmp_files, "tmp_file_path_to_url:exclude_file_location_from_filename(~w, ~w)~n", [Path,Fn]),
	report_file_path__singleton(Fn, Url, _),
	debug(tmp_files, "tmp_file_path_to_url:report_file_path(~w, ~w, ~w)~n", [Fn, Url, _]).

% if loc was a dict X (or represented as triples with `.` access notation) then you could just do `icase_endswith(X.fn, Suffix)`
 loc_icase_endswith(loc(_, Fn), Suffix) :-
	icase_endswith(Fn, Suffix).

 add_xml_result(Result_XML) :-
	add_xml_report('response', 'response', Result_XML).

 add_xml_report(Key, Title, XML) :-
	atomics_to_string([Key, '.xml'], Fn_value),
	Fn = loc(file_name, Fn_value),
	report_file_path(Fn, Url, loc(absolute_path, Path)),
	setup_call_cleanup(
		open(Path, write, Stream),
		sane_xml_write(Stream, XML),
		close(Stream)
	),
	add_report_file(_Report_Uri, -10,Key, Title, Url). % (_{name:Name,format:'xml'}

 add_report_file(Uri, Priority, Key, Title, loc(absolute_url, Url)) :-
	result(R),
	doc_new_uri(report_file, Uri),
	doc_add(R, l:report, Uri, files),
	doc_add(Uri, l:priority, Priority, files),
	doc_add(Uri, l:key, $>atom_string(Key), files),
	doc_add(Uri, l:title, $>atom_string(Title), files),
	doc_add(Uri, l:url, Url, files).

 get_report_file(Priority, Title, Key, Url) :-
	result(R),
	*doc(R, l:report, Uri, files),
	(	doc(Uri, l:priority, Priority, files)
	->	true
	;	Priority = 0),
	doc(Uri, l:key, Key, files),
	doc(Uri, l:title, Title, files),
	doc(Uri, l:url, Url, files).

 add_result_file_by_filename(Name) :-
	report_file_path(Name, Url, _),
	add_report_file(_Report_Uri, -1,'result', 'result', Url).

 add_result_file_by_path(Path) :-
	tmp_file_path_to_url(Path, Url),
	add_report_file(_Report_Uri, -1,'result', 'result', Url).

 my_request_tmp_dir_path(Tmp_Dir_Path) :-
	my_request_tmp_dir(loc(tmp_directory_name,Tmp_Dir)),
    resolve_specifier(loc(specifier, my_tmp(Tmp_Dir)), loc(absolute_path, Tmp_Dir_Path)).
