%% ----------------------------------------------------------------------------
%%
%% giallo: A small and flexible web framework
%%
%% Copyright (c) 2013 KIVRA
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.
%%
%% ----------------------------------------------------------------------------

-module(giallo).

-export([start/1]).
-export([start/2]).
-export([stop/0]).

%% API ------------------------------------------------------------------------

start(Dispatch) ->
    start(Dispatch, []).

start(Dispatch, Env) ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    application:load(giallo),

    CompiledDispatch = cowboy_router:compile(Dispatch),
    {ok, Acceptors} = get_env(acceptors, Env),
    {ok, Port} = get_env(port, Env),
    cowboy:start_http(giallo_http_listener, Acceptors, [{port, Port}], [
            {env, [{dispatch, CompiledDispatch}]},
            {middlewares, [cowboy_router, giallo_middleware,
                           cowboy_handler]}
            ]),
    ok = application:start(giallo).

stop() ->
    application:stop(giallo),
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(crypto).

%% Private --------------------------------------------------------------------

get_env(Key, Env) ->
    case lists:keyfind(Key, 1, Env) of
        {Key, Val} -> {ok, Val};
        false      -> application:get_env(giallo, Key)
    end.
