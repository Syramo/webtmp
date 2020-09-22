-module(wt_parser).
-export([parse_file/1]).

-include_lib("kernel/include/file.hrl").
-include_lib("webtmp/include/webtmp.hrl").


-record (fdscr, {handle,
				 mtag :: #mtag{}, 
			     line = 0 :: integer()}).
			     
-type cache_type() :: none | transient | permanent.

-type replacements() :: #{string() => any()}.
			     
-record (pstate, {queue = [] :: [#fdscr{}],
				  mtags = [] :: [#mtag{}],
				  cache = none :: cache_type(), 
				  vars = #{} :: replacements(), 
				  doc = [] :: string()}).


-spec parse_file (File) -> {ok, Mtags, Cache, Vars, Doc} | {error, Reason} when
	File :: string(),
	Vars :: replacements(),
	Mtags :: [#mtag{}],
	Cache :: cache_type(),
	Doc :: string(),
	Reason :: string().

parse_file (File) ->
	case filelib:is_regular(File) of
		true ->	start_parser(File);
		false -> {error, "File '" ++ File ++ "' does not exist"}
	end.




-spec start_parser (File) -> {ok, Mtags, Cache, Vars, Doc} | {error, Reason} when
	File :: string(),
	Vars :: replacements(),
	Mtags ::[#mtag{}],
	Cache :: cache_type(),
	Doc :: string(),
	Reason :: string().

	
start_parser (File) ->
	case parse (File,#pstate{}) of
		{ok, S} ->
			Doc = lists:reverse(S#pstate.doc),
			{ok, S#pstate.mtags, S#pstate.cache, S#pstate.vars, lists:flatten(Doc)};
		{error, R} ->
			wt_events:emmit({error, "parsing failed with: " ++ R}),
			{error, R}
	end.
	
	
parse (File, S) ->
	case wt_tools:open_with_mtag(File) of
		{ok, Fh, Mtag} ->
			Fdsc = #fdscr{handle = Fh, mtag = Mtag},
			{ok, Mreg} = re:compile("^\\s*(<--|//)#ERL(?<P>{([a-z_-]+),.+}\\.?)",[no_auto_capture]),
			parse_loop(S#pstate{queue = [Fdsc | S#pstate.queue]},Mreg);
		{error, R} ->
			{error, R}	
	end.
	
	
	
%%=======internals================================================================
	
parse_loop (#pstate{queue = []} = Ps, _) ->
	{ok, Ps};
	
parse_loop (#pstate{queue = [Fd|T]} = Ps, Mreg) when is_record(Ps,pstate) ->
	case io:get_line(Fd#fdscr.handle, "") of
		eof ->
			S = Ps#pstate{queue=T, mtags=[Fd#fdscr.mtag|Ps#pstate.mtags]},
			file:close(Fd#fdscr.handle),
			parse_loop(S,Mreg);
		Line -> 
			case parse_line(Line,Ps,Mreg) of
				{ok, S} -> 
					parse_loop(S,Mreg);
				{error, _S} ->    % we should unwind the whole queue and close everything on an error
					file:close(Fd#fdscr.handle),
					R = "error in '" ++ Fd#fdscr.mtag#mtag.path ++ "' line: " ++ integer_to_list(Fd#fdscr.line),
					{error, R}
			end
	end.
	
	

parse_line (Line, #pstate{queue = [Fd|T]} = Ps, Mreg) ->
	Fd2 = Fd#fdscr{line = Fd#fdscr.line + 1},
	S = Ps#pstate{queue=[Fd2|T]},
	case eval_line(Line, Mreg) of
		{add_vars, Vars} -> 
			{ok, S#pstate{vars=maps:merge(S#pstate.vars,Vars)}};
			
		{append, Ln} -> 
			{ok, S#pstate{doc=[Ln | S#pstate.doc]}};
			
		{include, File} -> % need to create a new path for the file
			parse(File,S);
			
		{error, R} ->
			wt_events:emmit({warning, R ++ "(" ++ Fd2#fdscr.mtag#mtag.path ++ " / " ++ integer_to_list(Fd2#fdscr.line) ++ ")"}),
			{ok, S} 
	end.
	
	
	
eval_line (Line, Mreg) ->
	case re:run(Line,Mreg,[{capture,all_names,list}]) of
		{match, [Match|_]} -> 
			eval_cmd(Match);
		_ ->
			{append, Line}
	end.
	
	
	
eval_cmd (Cmd) ->
	Cmd2 = string:trim(Cmd,trailing,"\n.") ++ ".",
	case wt_tools:str_to_term (Cmd) of
		{vars, L} -> 
			{add_vars, maps:from_list(L)};
		
		{include, Path} ->
			{include, Path};
		
		_ ->
			{error, "unknown command: " ++ Cmd}
	end.

