

% this gets the children of an element with ElementXPath
 inner_xml(Dom, Element_XPath, Children) :-
	xpath(Dom, Element_XPath, element(_,_,Children)).

 inner_xml_throw(Dom, Element_XPath, Children) :-
	(
		xpath(Dom, Element_XPath, element(_,_,Children))
	->
		true
	;
		(
			pretty_term_string(Element_XPath, Element_XPath_Str),
			throw_string(['element missing:', Element_XPath_Str])
		)
	).

 trimmed_field(Dom, Element_XPath, Value) :-
	xpath(Dom, Element_XPath, element(_,_,[Child_Atom])),
	trim_atom(Child_Atom, Value).

 trim_atom(Atom, Trimmed_Atom) :-
	atom_string(Atom, Atom_String),
	trim_string(Atom_String, Trimmed_String),
	%split_string(Atom_String, "", "\s\t\n", [Trimmed_String]),
	atom_string(Trimmed_Atom, Trimmed_String).

 trim_string(String, Trimmed_String) :-
	split_string(String, "", "\s\t\n", [Trimmed_String]).

 write_tag(Tag_Name_Input,Tag_Value) :-
	flatten([Tag_Name_Input], Tag_Name_List),
	atomic_list_concat(Tag_Name_List, Tag_Name),
	string_concat("<",Tag_Name,Open_Tag_Tmp),
	string_concat(Open_Tag_Tmp,">",Open_Tag),
	string_concat("</",Tag_Name,Closing_Tag_Tmp),
	string_concat(Closing_Tag_Tmp,">",Closing_Tag),
	write(Open_Tag),
	write(Tag_Value),
	writeln(Closing_Tag).

 open_tag(Name) :-
	flatten(['<', Name, '>'], L),
	atomic_list_concat(L, S),
	write(S).

 close_tag(Name) :-
	flatten(['</', Name, '>\n'], L),
	atomic_list_concat(L, S),
	write(S).


 numeric_field(Dom, Name_String, Value) :-
	trimmed_field(Dom, //Name_String, Value_Atom),
	atom_number(Value_Atom, Value).


/* take a list of field names and variables that the contents extracted from xml are bound to
a (variable, default value) tuple can also be passed */
 fields(Dom, [Name_String, Value_And_Default|Rest]) :-
	nonvar(Value_And_Default),
	!,
	(Value, Default_Value) = Value_And_Default,
	(
		(
			trimmed_field(Dom, //Name_String, Value),
			!
		);
			Value = Default_Value
	),
	fields(Dom, Rest).

/* if default is not passed, throw error if field's tag is not found*/
 fields(Dom, [Name_String, Value|Rest]) :-
	(
		(
			trimmed_field(Dom, //Name_String, Value),
			!
		);
		(
			pretty_term_string(Dom, Dom_String),
			pretty_term_string(Name_String, Pretty_Name_String),
			atomic_list_concat([Pretty_Name_String, " field missing in ", Dom_String], Error),
			throw(Error)
		)
	),
	fields(Dom, Rest).

 fields(_, []).


/* try to extract a field, possibly fail*/
 field_nothrow(Dom, [Name_String, Value]) :-
	trimmed_field(Dom, //Name_String, Value).

/* extract fields and convert to numbers*/
 numeric_fields(Dom, [Name_String, Value_And_Default|Rest]) :-
	nonvar(Value_And_Default),
	!,
	(Value, Default_Value) = Value_And_Default,
	(
		(
			numeric_field(Dom, Name_String, Value),
			!
		);
		Value = Default_Value
	),
	numeric_fields(Dom, Rest).

 numeric_fields(Dom, [Name_String, Value|Rest]) :-
	(
		(
			numeric_field(Dom, Name_String, Value),
			!
		);
		(
			string_concat(Name_String, " field missing", Error),
			throw(Error)
		)
	),
	numeric_fields(Dom, Rest).

 numeric_fields(_, []).





 xml_write_file(loc(absolute_path,File_Name), Term, Options) :-
	setup_call_cleanup(
		open(File_Name, write, Stream),
		xml_write(Stream, Term, Options),
		close(Stream)).

 xml_from_url(Url, Dom) :-
	/*fixme: throw something more descriptive here and produce a human-level error message at output*/
	setup_call_cleanup(
        http_open(Url, In, []),
		load_structure(In, Dom, [dialect(xml),space(remove)]),
        close(In)).

 xml_from_path(File_Path, Dom) :-
	%http_safe_file(File_Path, []),
	nb_setval(xml_from_path__file_path,File_Path),
	load_xml(File_Path, Dom, [space(remove), call(error, xml_loader_error_callback)]),
	nb_delete(xml_from_path__file_path).

 xml_loader_error_callback(X,Y,Z) :-
	nb_getval(xml_from_path__file_path,File_Path),
	throw_string(['XML parsing error: "',File_Path, '": ', X,': ', Y,' (',Z,')']).

 xml_from_path_or_url(Url, Dom) :-
		is_url(Url)
	->	xml_from_url(Url, Dom)
	;	xml_from_path(Url, Dom).



/*
Validates an XML instance against an XSD schema by calling an external Python script
*/
 validate_xml(loc(absolute_path,Instance_File), loc(absolute_path,Schema_File), Schema_Errors) :-
	!uri_encoded(query_value,Instance_File,Instance_File_Encoded),
	!uri_encoded(query_value,Schema_File,Schema_File_Encoded),
	setup_call_cleanup(
		!http_open(
		    $>append($>!url_parts($>services_server),
		    [
                path('/xml_xsd_validator'),
                search([
                    xml=Instance_File_Encoded,
                    xsd=Schema_File_Encoded
                ])
            ]),
            In,
            [
				request_header('Accept'='application/json'),
				method(post)
			]
        ),
		!json_read_dict(In, Response_JSON),
		/* todo is this correct, or can In be unbound here? */
		(var(In) -> true ; close(In))
    ),
	%format("Result: ~w~n", [Response_JSON.result]),
	(
		Response_JSON.result = "ok"
	->
		Schema_Errors = []
	;
		atomic_list_concat(['XML validation error', Response_JSON.error_message], ": ", Schema_Error),
		Schema_Errors = [Schema_Error]
	).

 validate_xml2(Xml, Xsd) :-
	!resolve_specifier(loc(specifier, my_schemas(Xsd)), Schema_File),
	!validate_xml(Xml, Schema_File, Schema_Errors),
	(	Schema_Errors = []
	->	true
	;	(
			!maplist(add_alert(error), Schema_Errors),
			throw_string(['aborted.'])
		)
	).
