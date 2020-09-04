%%%-------------------------------------------------------------------
%% @doc webtmp public API
%% @end
%%%-------------------------------------------------------------------

-module(webtmp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    webtmp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
