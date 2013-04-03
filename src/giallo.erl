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

%% @doc API for stoping and starting Giallo and convenience function.
%%
%% Giallo uses standard Cowboy features and makes it easy to mix and match
%% conventient Giallo modules with the full power of Cowboy, REST-handlers,
%% etc.
%%
%% This module provides functions for starting and stopping Giallo as well as
%% some convenience functions for working with headers, parameters and
%% multipart data.
%% @end

-module(giallo).

-export([start/1]).
-export([start/2]).
-export([stop/0]).

-export([header/2]).
-export([header/3]).
-export([post_param/2]).
-export([post_param/3]).
-export([query_param/2]).
-export([query_param/3]).
-export([multipart_file/2]).
-export([multipart_param/2]).
-export([multipart_param/3]).
-export([multipart_stream/4]).

%% API ------------------------------------------------------------------------

-spec start(Dispatch) -> ok | {error, Reason} when
    Dispatch :: cowboy_router:routes(),
    Reason   :: term().
start(Dispatch) ->
    start(Dispatch, []).

-spec start(Dispatch, Env) -> ok | {error, Reason} when
    Dispatch :: cowboy_router:routes(),
    Env      :: proplists:proplist(),
    Reason   :: term().
start(Dispatch, Env) ->
    CompiledDispatch = cowboy_router:compile(Dispatch),
    {ok, Acceptors} = get_env(acceptors, Env),
    {ok, Port} = get_env(port, Env),
    cowboy:start_http(giallo_http_listener, Acceptors, [{port, Port}], [
            {env, [{dispatch, CompiledDispatch}]},
            {middlewares, [cowboy_router, giallo_middleware,
                           cowboy_handler]}
            ]).

-spec stop() -> ok | {error, Reason} when
    Reason :: term().
stop() ->
    application:stop(giallo).

%% @equiv post_param(Key, Req0, undefined)
-spec post_param(Key, Req0) -> binary() | undefined | true when
    Key     :: binary(),
    Req0    :: cowboy_req:req().
post_param(Key, Req0) ->
    post_param(Key, Req0, undefined).

%% @doc
%% Return a named parameter from a HTTP POST or <em>Default</em> if not found,
%% see <em>query_param/2</em> for query parameter retrieving.
%% @end
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

%% @doc
%% Return a named parameter from the querystring or <em>Default</em>
%% if not found, see <em>post_param/2</em> for HTTP POST parameter retrieving.
%% @end
-spec query_param(Key, Req0, Default) -> binary() | Default | true when
    Key     :: binary(),
    Req0    :: cowboy_req:req(),
    Default :: any().
query_param(Key, Req0, Default) ->
    {Value, _Req1} = cowboy_req:qs_val(Key, Req0, Default),
    Value.

%% @equiv header(Key, Req0, undefined)
-spec header(Key, Req0) -> binary() | undefined when
    Key     :: binary(),
    Req0    :: cowboy_req:req().
header(Key, Req0) ->
    header(Key, Req0, undefined).

%% @doc
%% Return a named HTTP Header from the Request or <em>Default</em>
%% if not found.
%% @end
-spec header(Key, Req0, Default) -> binary() | Default when
    Key     :: binary(),
    Req0    :: cowboy_req:req(),
    Default :: any().
header(Key, Req0, Default) ->
    {Value, _Req1} = cowboy_req:header(Key, Req0, Default),
    Value.

%% @equiv multipart_param(Key, Req0, undefined)
-spec multipart_param(Key, Req0) -> binary() | undefined when
    Key     :: binary(),
    Req0    :: cowboy_req:req().
multipart_param(Key, Req0) ->
    multipart_param(Key, Req0, undefined).

%% @doc
%% Returns the value of a multipart request, or Default if not found.
-spec multipart_param(Key, Req0, Default) -> binary() | Default when
    Key     :: binary(),
    Req0    :: cowboy_req:req(),
    Default :: any().
multipart_param(Key, Req0, Default) ->
    case giallo_multipart:param(Key, Req0) of
        undefined -> Default;
        Value     -> Value
    end.

%% @doc
%% Locates a multipart field named Param, assumed to contain a file.
%% Returns {Filename, Body}, where Filename is the result of decoding
%% the "filename" part of the Content-Disposition header.
%% @end
-spec multipart_file(Key, Req0) -> {binary(), binary()} | undefined when
    Key     :: binary(),
    Req0    :: cowboy_req:req().
multipart_file(Key, Req0) ->
    giallo_multipart:file(Key, Req0).

%% @doc
%% Streams fragments of a multipart part by repeatedly calling
%% <em>Fun(Fragment, Meta, State)</em> where Fragment is a binary containing
%% a part of the body, Meta contains the header fields of the part,
%% and State is a user-specified updated on each call to Fun.
%% When the end of the part is reached, Fun is called with Fragment
%% set to the atom "eof".
%% @end
-spec multipart_stream(Key, Fun, State, Req0) ->
                                {binary(), binary()} | undefined when
    Key     :: binary(),
    Fun     :: fun(),
    State   :: any(),
    Req0    :: cowboy_req:req().
multipart_stream(Key, Fun, State, Req0) ->
    giallo_multipart:stream_param(Key, Fun, State, Req0).

%% Private --------------------------------------------------------------------

get_env(Key, Env) ->
    case lists:keyfind(Key, 1, Env) of
        {Key, Val} -> {ok, Val};
        false      -> application:get_env(giallo, Key)
    end.
