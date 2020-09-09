-module(wt_dds).
-behaviour(gen_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2]).


-include_lib("kernel/include/file.hrl").


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
stop() ->
	gen_server:cast(?MODULE, stop).
	
	
init(Args) ->
    {ok, Args}.
    
    
terminate(_Reason, _State) ->
    ok.
    

%%------------------------------------------------------------------------------
%% synchronous calls

handle_call({get_doc,Path,Opt}, _From, State) when is_map(Opt) ->
	Key = maps:get(key,Opt,"***"),
	Repl = prep_doc(Path, 
					Key, 
					wt_cache:doc_state(Path,Key), 
					maps:get(rebuild,Opt,false)),
	{reply, Repl, State}.
	


%%------------------------------------------------------------------------------
%% asynchronous calls

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Msg, State) ->
    {noreply, State}.
    

%%%-----------internals---------------------------------------------------------

prep_doc (Path, Key, _, true) ->
	build_doc(Path, Key);
	
prep_doc (Path, Key, {error, na}, _) ->
	build_doc(Path, Key);
	
prep_doc (Path, Key, {mtag, N}, _) when is_integer(N) ->
	case file:read_file_info(Path,[{time, posix}]) of
		{error, Reason} -> {error, Reason};
		{ok, FileInfo} -> 
			if 
				FileInfo#file_info.mtime > N -> build_doc(Path, Key);
				true -> wt_cache:get_doc(Path, Key)
			end
	end.

	
build_doc (Path, Key) ->
	case wt_parser:parse_file(Path) of
		{ok, MTag, Cache, Doc} -> 
			wt_cache:add_doc(Path,Doc,#{key => Key, mtag => MTag, cache => Cache}),
			{ok, Doc};
		{error, Reason} ->
			{error, Reason}
	end.