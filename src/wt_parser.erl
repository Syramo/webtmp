-module(wt_parser).
-export([parse_file/1]).


-spec parse_file (File) -> {ok, Mtag, Cache, Doc} | {error, Reason} when
	File :: string(),
	Mtag :: integer(),
	Cache :: none | transient | permanent,
	Doc :: binary(),
	Reason :: string().

parse_file (File) ->
	Ext = filename:extension(File),
	start_parser(File,Ext).




-spec start_parser (File,Ext) -> {ok, Mtag, Cache, Doc} | {error, Reason} when
	File :: string(),
	Ext :: string(),
	Mtag :: integer(),
	Cache :: none | transient | permanent,
	Doc :: binary(),
	Reason :: string().
	
start_parser (File,[]) ->
	{error, "Can't determine the document type for: " ++ File};
	
start_parser (File,[_|Ext]) ->
	Ps = #{},
	case list_to_atom(string:lowercase(Ext)) of
		config -> parser_config(Ps);
		_ -> {error, "No parser for type '" ++ Ext ++ "' implemented"}
	end.
	
	
	
%%=======parsers================================================================
	
parser_config (Ps) ->
	{error, "not implemented"}.
