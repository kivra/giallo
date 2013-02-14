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
-export([post_param/2]).
-export([post_param/3]).
-export([query_param/2]).
-export([query_param/3]).

%% API ------------------------------------------------------------------------

start(Dispatch) ->
    start(Dispatch, []).

start(Dispatch, Env) ->
    CompiledDispatch = cowboy_router:compile(Dispatch),
    {ok, Acceptors} = get_env(acceptors, Env),
    {ok, Port} = get_env(port, Env),
    cowboy:start_http(giallo_http_listener, Acceptors, [{port, Port}], [
            {env, [{dispatch, CompiledDispatch}]},
            {middlewares, [cowboy_router, giallo_middleware,
                           cowboy_handler]}
            ]).

stop() ->
    application:stop(giallo).

%% @equiv post_param(Key, Req0, undefined)
-spec post_param(Key, Req0) -> binary() | undefined | true when
    Key     :: binary(),
    Req0    :: cowboy_req:req().
post_param(Key, Req0) ->
    post_param(Key, Req0, undefined).

-spec post_param(Key, Req0, Default) -> binary() | Default | true when
    Key     :: binary(),
    Req0    :: cowboy_req:req(),
    Default :: any().
post_param(Key, Req0, Default) ->
    case cowboy_req:body_qs(Req0) of
        {error, _Reason} = E -> E;
        {ok, ValueList, _Req1}      ->
            case lists:keyfind(Key, 1, ValueList) of
                {Key, Value} -> Value;
                false -> Default
            end
    end.

%% @equiv query_param(Key, Req0, undefined)
-spec query_param(Key, Req0) -> binary() | undefined | true when
    Key     :: binary(),
    Req0    :: cowboy_req:req().
query_param(Key, Req0) ->
    query_param(Key, Req0, undefined).

-spec query_param(Key, Req0, Default) -> binary() | Default | true when
    Key     :: binary(),
    Req0    :: cowboy_req:req(),
    Default :: any().
query_param(Key, Req0, Default) ->
    {Value, _Req1} = cowboy_req:qs_val(Key, Req0, Default),
    Value.

%% Private --------------------------------------------------------------------

get_env(Key, Env) ->
    case lists:keyfind(Key, 1, Env) of
        {Key, Val} -> {ok, Val};
        false      -> application:get_env(giallo, Key)
    end.
