-module(webtmp).

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).

%% API
-export([get_doc/1, get_doc/2, build_doc/1, clear_cache/0, prune_cache/1]).


-include("webtmp.hrl").


start(_StartType, _StartArgs) ->
    {ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, []),
    {ok, Pid}.

stop(_State) ->
    ok.
    
init(_) ->
    ChildSpecList = [child(wt_dds),child(wt_cache),child(wt_events)],
    {ok,{{rest_for_one, 2, 3600}, ChildSpecList}}.

child(Module) ->
    {Module, {Module, start_link, []},permanent, 2000, worker, [Module]}.
    
    
%%------------------------------------------------------------------------------    
%% the simple API to use


-spec get_doc(string(),doc_options()) -> {ok, Htdoc :: string()} | {error, Reason :: string()}.
get_doc(Path, Opt) ->
	gen_server:call(wt_dds,{get_doc,Path,Opt}).
	
get_doc(Path) ->
	get_doc(Path,#{}).
	
build_doc(Path) ->
	get_doc(Path,#{rebuild => true}).
	

-spec clear_cache() -> ok.
clear_cache() ->
	prune_cache(#{what => all}).
	
	
-spec prune_cache (purge_options()) -> ok.	
prune_cache(#{what := _What} = Opt) ->
	wt_cache:prune(Opt).	

