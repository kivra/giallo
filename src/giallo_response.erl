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

-module(giallo_response).

-export([eval/3]).
-export([try_fun/6]).

-define(DEFAULT_CT, [{<<"content-type">>, <<"text/html">>}]).
-define(JSON_CT, [{<<"content-type">>, <<"application/json">>}]).
-define(JS_CT, [{<<"content-type">>, <<"application/javascript">>}]).

%% API ------------------------------------------------------------------------

eval({Action, X, Req1}, _Req0, Env) when is_tuple(Req1) ->
    eval({Action, X}, Req1, Env);
eval({Action, X, Y, Req1}, _Req0, Env) when is_tuple(Req1) ->
    eval({Action, X, Y}, Req1, Env);
eval({Action, X, Y, Z, Req1}, _Req0, Env) when is_tuple(Req1) ->
    eval({Action, X, Y, Z}, Req1, Env);
eval(ok, Req0, Env) ->
    eval({ok, []}, Req0, Env);
eval({ok, Variables}, Req0, Env) ->
    eval({ok, Variables, []}, Req0, Env);
eval({ok, Variables, Headers}, Req0, Env) ->
    Response = render_template(Req0, get_value(handler, Env),
                               Variables, Headers, Env),
    eval(Response, Req0, Env);
eval({redirect, Location}, Req0, Env) when is_binary(Location) ->
    eval({redirect, Location, []}, Req0, Env);
eval({redirect, Location}, Req0, Env) ->
    render_other(Location, [], Req0, Env);
eval({redirect, Location, Headers}, Req0, _Env) ->
    redirect_or_move(302, Location, Headers, Req0);
eval({moved, Location}, Req0, Env) ->
    eval({moved, Location, []}, Req0, Env);
eval({moved, Location, Headers}, Req0, _Env) ->
    redirect_or_move(301, Location, Headers, Req0);
eval({render_other, Location}, Req0, _Env) when is_binary(Location) ->
    redirect_or_move(302, Location, [], Req0);
eval({render_other, Location}, Req0, Env) ->
    eval({render_other, Location, []}, Req0, Env);
eval({render_other, Location, Variables}, Req0, Env) ->
    render_other(Location, Variables, Req0, Env);
eval({stream, Fun, Acc0}, Req0, Env) ->
    eval({stream, Fun, Acc0, []}, Req0, Env);
eval({stream, _Fun, _Acc0, _Headers}, _Req0, _Env) ->
    % TODO: Implement
    implement;
eval({json, Data}, Req0, Env) ->
    eval({json, Data, []}, Req0, Env);
eval({json, Data, []}, Req0, Env) ->
    eval({json, Data, ?JSON_CT}, Req0, Env);
eval({json, Data, Headers}, Req0, Env) ->
    F = fun() ->
            {output, jsx:encode(Data), Headers}
    end,
    eval(try_fun(F, Req0, jsx, encode, 1, Env), Req0, Env);
eval({jsonp, Callback, Data}, Req0, Env) ->
    eval({jsonp, Callback, Data, []}, Req0, Env);
eval({jsonp, Callback, Data, []}, Req0, Env) ->
    eval({jsonp, Callback, Data, ?JS_CT}, Req0, Env);
eval({jsonp, Callback, Data, Headers}, Req0, Env) ->
    F = fun() ->
            JsonData = jsx:encode(Data),
            Payload = <<Callback/binary, "(", JsonData/binary, ");">>,
            {output, Payload, Headers}
    end,
    eval(try_fun(F, Req0, jsx, encode, 1, Env), Req0, Env);
eval({output, Output}, Req0, Env) ->
    eval({output, Output, []}, Req0, Env);
eval({output, Output, []}, Req0, Env) ->
    eval({output, Output, ?DEFAULT_CT}, Req0, Env);
eval({output, Output, Headers}, Req0, _Env) ->
    {ok, Req1} = cowboy_req:reply(200, Headers, Output, Req0),
    {halt, Req1};
eval(not_found, Req0, _Env) ->
    {ok, Req1} = cowboy_req:reply(404, Req0),
    {halt, Req1};
eval({error, Status}, Req0, _Env) ->
    {error, Status, Req0};
eval(continue, Req, Env) ->
    {ok, Req, Env}.

try_fun(Fun, Req, Handler, Action, Arity, Env) ->
    try Fun()
    catch Class:Reason ->
            error_logger:error_msg(
            "** Giallo handler ~p terminating in ~p/~p~n"
            "   for the reason ~p:~p~n"
            "** Handler state was ~p~n"
            "** Request was ~p~n"
            "** Stacktrace: ~p~n~n",
            [Handler, Action, Arity, Class, Reason, Env,
                cowboy_req:to_list(Req), erlang:get_stacktrace()]),
            {error, 500}
    end.

%% Private --------------------------------------------------------------------

render_other(Location, Variables, Req, Env) ->
    Action = get_value(action, Location),
    Handler = get_value(controller, Location, get_value(handler, Env)),
    giallo_middleware:execute_handler(Handler, Action, Variables, Req, Env).

redirect_or_move(Status, Location, Headers, Req0) ->
    {ok, Req1} =
        cowboy_req:reply(Status, [{<<"location">>, Location}], Headers, Req0),
    {halt, Req1}.

render_template(Req0, Handler, Variables, Headers, Env) ->
    F = fun() ->
            {PathInfo, _Req1} = cowboy_req:path_info(Req0),
            Action = get_function_name(PathInfo),
            Prefix = atom_to_list(Handler),
            Template = list_to_existing_atom(Prefix ++ "_" ++ Action ++ "_dtl"),
            {ok, Response} = apply(Template, render, [Variables]),
            {output, Response, Headers}
    end,
    try_fun(F, Req0, erlydtl, render, 1, Env).

get_function_name(undefined) ->
    "index_";
get_function_name([]) ->
    "index_";
get_function_name(PathInfo) ->
    binary_to_list(hd(PathInfo)).

get_value(Key, List) ->
    get_value(Key, List, undefined).

get_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false      -> Default;
        {Key, Val} -> Val
    end.
