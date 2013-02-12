%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc Application to work with UMB devices (lufft.com).
-module(umb_app).
-created('Date: 12/02/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    application:start(sl),
    case application:get_env(log) of
        undefined -> ok;
        {ok, FileName} -> sl:open(FileName)
    end,
    umb_sup:start_link().

stop(_State) ->
    ok.
