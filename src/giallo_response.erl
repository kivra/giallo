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

-export([eval/5]).

-include("giallo.hrl").

-type eval_action() :: ok | redirect | moved | render_other | stream | json |
                       jsonp | output | not_found | error.
-type eval() :: ok | continue | {eval_action(), any(), any(), any()} |
                {eval_action(), any()} | {eval_action(), any(), any()} |
                {eval_action(), any(), any(), any()}.

-define(DEFAULT_CT, [{<<"content-type">>, <<"text/html">>}]).
-define(JSON_CT, [{<<"content-type">>, <<"application/json">>}]).
-define(JS_CT, [{<<"content-type">>, <<"application/javascript">>}]).

%% API ------------------------------------------------------------------------

-spec eval(Eval, Req0, Env, Handler, Action) ->
    {halt, Req1} | {error, Status, Req1} | {ok, Req1, Env} when
    Eval    :: eval(),
    Req0    :: cowboy_req:req(),
    Req1    :: cowboy_req:req(),
    Env     :: cowboy_middleware:env(),
    Handler :: module(),
    Action  :: atom(),
    Status  :: non_neg_integer().
eval({EvalAction, X, Req1}, _, Env, Handler, Action) when is_tuple(Req1) ->
    eval({EvalAction, X}, Req1, Env, Handler, Action);
eval({EvalAction, X, Y, Req1}, _, Env, Handler, Action) when is_tuple(Req1) ->
    eval({EvalAction, X, Y}, Req1, Env, Handler, Action);
eval({EvalAction, X, Y, Z, Req1}, _, Env, Handler, Action)
                                                        when is_tuple(Req1) ->
    eval({EvalAction, X, Y, Z}, Req1, Env, Handler, Action);
eval(ok, Req0, Env, Handler, Action) ->
    eval({ok, []}, Req0, Env, Handler, Action);
eval({ok, Variables}, Req0, Env, Handler, Action) ->
    eval({ok, Variables, []}, Req0, Env, Handler, Action);
eval({ok, Variables, Headers}, Req0, Env, Handler, Action) ->
    Response = render_template(Req0, Handler, Action,
                               Variables, Headers, Env),
    eval(Response, Req0, Env, Handler, Action);
eval({redirect, Location}, Req0, Env, Handler, Action) ->
    eval({redirect, Location, []}, Req0, Env, Handler, Action);
eval({redirect, Location, Headers}, Req0, _, _, _) ->
    redirect_or_move(302, Location, Headers, Req0);
eval({moved, Location}, Req0, Env, Handler, Action) ->
    eval({moved, Location, []}, Req0, Env, Handler, Action);
eval({moved, Location, Headers}, Req0, _, _, _) ->
    redirect_or_move(301, Location, Headers, Req0);
eval({action_other, Location}, Req0, Env, Handler, Action) ->
    eval(action_other(Location, Handler, [], Req0, Env),
         Req0, Env, Handler, Action);
eval({render_other, Location}, Req0, Env, Handler, Action) ->
    eval({render_other, Location, []}, Req0, Env, Handler, Action);
eval({render_other, Location, Variables}, Req0, Env, Handler, Action) ->
    eval(render_other(Location, Handler, Variables, Req0, Env),
         Req0, Env, Handler, Action);
eval({stream, Fun, Acc0}, Req0, Env, Handler, Action) ->
    eval({stream, Fun, Acc0, []}, Req0, Env, Handler, Action);
eval({stream, _Fun, _Acc0, _Headers}, _Req0, _En, _, _v) ->
    % TODO: Implement
    implement;
eval({json, Data}, Req0, Env, Handler, Action) ->
    eval({json, Data, []}, Req0, Env, Handler, Action);
eval({json, Data, []}, Req0, Env, Handler, Action) ->
    eval({json, Data, ?JSON_CT}, Req0, Env, Handler, Action);
eval({json, Data, Headers}, Req0, Env, Handler, Action) ->
    F = ?lazy({output, jsx:encode(Data), Headers}),
    eval(?do_or_error(F, Req0, jsx, encode, 1, Env),
         Req0, Env, Handler, Action);
eval({jsonp, Callback, Data}, Req0, Env, Handler, Action) ->
    eval({jsonp, Callback, Data, []}, Req0, Env, Handler, Action);
eval({jsonp, Callback, Data, []}, Req0, Env, Handler, Action) ->
    eval({jsonp, Callback, Data, ?JS_CT}, Req0, Env, Handler, Action);
eval({jsonp, Callback, Data, Headers}, Req0, Env, Handler, Action) ->
    F = fun() ->
            JsonData = jsx:encode(Data),
            Payload = <<Callback/binary, "(", JsonData/binary, ");">>,
            {output, Payload, Headers}
    end,
    eval(?do_or_error(F, Req0, jsx, encode, 1, Env),
         Req0, Env, Handler, Action);
eval({output, Output}, Req0, Env, Handler, Action) ->
    eval({output, Output, []}, Req0, Env, Handler, Action);
eval({output, Output, []}, Req0, Env, Handler, Action) ->
    eval({output, Output, ?DEFAULT_CT}, Req0, Env, Handler, Action);
eval({output, Output, Headers}, Req0, _, _, _) ->
    {ok, Req1} = cowboy_req:reply(200, Headers, Output, Req0),
    {halt, Req1};
eval(not_found, Req0, _, _, _) ->
    {ok, Req1} = cowboy_req:reply(404, Req0),
    {halt, Req1};
eval({error, Status}, Req0, _, _, _) ->
    {error, Status, Req0};
eval(continue, Req, Env, _, _) ->
    {ok, Req, Env}.

%% Private --------------------------------------------------------------------

action_other(Location, DefaultHandler, Variables, Req, Env) ->
    Action = get_value(action, Location),
    Handler = get_value(controller, Location, DefaultHandler),
    giallo_middleware:execute_handler(Handler, Action, Variables, Req, Env).

render_other(Location, DefaultHandler, Variables, Req, Env) ->
    Action = get_value(action, Location),
    Handler = get_value(controller, Location, DefaultHandler),
    render_template(Req, Handler, Action, Variables, [], Env).

redirect_or_move(Status, Location, Headers, Req0) ->
    {ok, Req1} =
        cowboy_req:reply(Status, [{<<"location">>, Location}], Headers, Req0),
    {halt, cowboy_req:set([{connection, close}, {resp_state, done}], Req1)}.

render_template(Req0, Handler, Action0, Variables, Headers, Env) ->
    F = fun() ->
            Action1 = atom_to_list(Action0),
            Prefix = atom_to_list(Handler),
            Template = list_to_existing_atom(Prefix++"_"++Action1++"_dtl"),
            case code:ensure_loaded(Template) of
                {module, Template} ->
                    {ok, Response} = apply(Template, render, [Variables]),
                    {output, Response, Headers};
                {error, _Reason} ->
                    continue
            end
    end,
    ?do_or_error(F, Req0, erlydtl, render, 1, Env).

get_value(Key, List) ->
    get_value(Key, List, undefined).

get_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false      -> Default;
        {Key, Val} -> Val
    end.

