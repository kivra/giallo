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

-module(giallo_middleware).
-behaviour(cowboy_middleware).

-include("giallo.hrl").

-export([execute/2]).
-export([execute_handler/5]).

%% API ------------------------------------------------------------------------

-spec execute(Req0, Env) ->
    {ok, Req, Env} | {error, 500, Req} | {halt, Req} when
    Req0 :: cowboy_req:req()
    ,Req :: cowboy_req:req()
    ,Env :: cowboy_middleware:env().
execute(Req0, Env) ->
    {handler, Handler} = lists:keyfind(handler, 1, Env),
    {handler_opts, Arguments} = lists:keyfind(handler_opts, 1, Env),
    {Req1, Action} = get_action(Handler, Req0),
    execute_handler(Handler, Action, Arguments, Req1, Env).


-spec execute_handler(Handler, Action, Arguments, Req0, Env) ->
    {ok, Req, Env} | {error, 500, Req} | {halt, Req} when
    Handler    :: module()
    ,Action    :: atom()
    ,Arguments :: proplists:proplist()
    ,Req0      :: cowboy_req:req()
    ,Req       :: cowboy_req:req()
    ,Env       :: cowboy_middleware:env().
execute_handler(Handler, ActionName, Arguments, Req0, Env) ->
    {PathInfo, Req1} = cowboy_req:path_info(Req0),
    Extra = get_extra(PathInfo),
    Action = do_get_action(Handler, [ActionName]),
    giallo_response:eval(unmarshal_before(handler_handle(Handler, Action,
                                                         Extra, Arguments,
                                                         Req1, Env)),
                         Req1, Env, Handler, Action).

%% Private --------------------------------------------------------------------

handler_handle(_Handler, undefined, _PathInfo, _Arguments, _Req0, _Env) ->
    continue;
handler_handle(Handler, Action, PathInfo, Arguments, Req0, Env) ->
    case maybe_do_before(Handler, Action, Req0, Env) of
        {redirect, _Location} = Redirect ->
            Redirect;
        {error, 500} = Error ->
            Error;
        {ok, BeforeArgs} ->
            F = fun() ->
                    Parameters = Arguments ++ BeforeArgs,
                    {Method, Req1} = cowboy_req:method(Req0),
                    {before, BeforeArgs, apply(Handler,
                                               Action,
                                               [Method,
                                                PathInfo,
                                                Parameters,
                                                Req1])}
            end,
            ?exported_or_else({Handler, Action, 4},
                              ?lazy(?do_or_error(F, Req0, Handler, Action,
                                                 4, Env)),
                              ?lazy(continue))
    end.

get_action(Handler, Req0) ->
    {PathInfo, Req1} = cowboy_req:path_info(Req0),
    {Req1, do_get_action(Handler, PathInfo)}.

do_get_action(Handler, undefined) ->
    do_get_action(Handler, [<<"index_">>]);
do_get_action(Handler, []) ->
    do_get_action(Handler, [<<"index_">>]);
do_get_action(Handler, [ActionName | _]) ->
    F = fun() ->
            case code:ensure_loaded(Handler) of
                {module, Handler}  -> ?any_to_existing_atom(ActionName);
                {error, _Reason}   -> undefined
            end
    end,
    ?do_or_else(F, fun(_) -> undefined end).

unmarshal_before({before, [], Eval}) ->
    Eval;
unmarshal_before({before, Args, ok}) ->
    {ok, [{"_before", Args}]};
unmarshal_before({before, Args, {ok, Var}}) ->
    {ok, [{"_before", Args} | Var]};
unmarshal_before({before, Args, {ok, Var, Headers}}) ->
    {ok, [{"_before", Args} | Var], Headers};
unmarshal_before({before, Args, {render_other, Location, Var}}) ->
    {render_other, Location, [{"_before", Args} | Var]};
unmarshal_before({before, _, Eval}) ->
    Eval;
unmarshal_before(Eval) ->
    Eval.

maybe_do_before(Handler, Action, Req0, Env) ->
    F = ?lazy(apply(Handler, before_, [Action, Req0])),
    ?exported_or_else({Handler, before_, 2},
                      ?lazy(?do_or_error(F, Req0, Handler, before_, 2, Env)),
                      ?lazy({ok, []})).

get_extra([]) ->
    [];
get_extra(undefined) ->
    [];
get_extra(PathInfo) ->
    tl(PathInfo).
