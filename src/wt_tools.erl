-module (wt_tools).

-export ([get_mtag/1, open_with_mtag/1, str_to_term/1]).

-include_lib ("webtmp/include/webtmp.hrl").
-include_lib ("kernel/include/file.hrl").



-spec get_mtag (File) -> {ok, Mtag} | {error, Reason} when
	File :: string(),
	Mtag :: #mtag{},
	Reason :: string().
	
get_mtag (File) ->
	case file:read_file_info(File,[{time, posix}]) of
		{ok, Fi} ->
			Mtag = #mtag{path = File, mod = Fi#file_info.mtime, size = Fi#file_info.size},
			{ok, Mtag};
		{error, R} ->
			Err = "accessing '" ++ File ++ "' (" ++ atom_to_list(R) ++  ")",
			wt_events:emmit({error, Err}),
			{error, Err}
	end.
	
	


-spec open_with_mtag (File) -> {ok, Fh, Mtag} | {error, Reason} when
	File :: string(),
	Fh :: any(),
	Mtag :: #mtag{},
	Reason :: string().
	
	
open_with_mtag (File) ->
	case get_mtag(File) of
		{ok, Mtag} ->
			{ok, Fh} = file:open(File,[read]),
			{ok, Fh, Mtag};
		{error, R} ->
			{error, R}
	end.
	
	
	
	
-spec str_to_term (Str :: string()) -> any().

str_to_term (Str) ->
	{ok, Tokens, _} = erl_scan:string(Str),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs(Parsed, [],
                                        {value, fun (_,_) -> nope end},
                                        {value, fun (_,_) -> nope end}),
    Result.
		