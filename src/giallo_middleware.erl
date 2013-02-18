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
    case get_action(Handler, Req0) of
        continue       -> {ok, Req0, Env};
        {Req1, Action} ->
           execute_handler(Handler, Action, Arguments, Req1, Env)
    end.


-spec execute_handler(Handler, Action, Arguments, Req0, Env) ->
    {ok, Req, Env} | {error, 500, Req} | {halt, Req} when
    Handler    :: module()
    ,Action    :: atom()
    ,Arguments :: proplists:proplist()
    ,Req0      :: cowboy_req:req()
    ,Req       :: cowboy_req:req()
    ,Env       :: cowboy_middleware:env().
execute_handler(Handler, Action, Arguments, Req0, Env) ->
    {PathInfo, Req1} = cowboy_req:path_info(Req0),
    Extra = get_extra(PathInfo),
    giallo_response:eval(unmarshal_before(handler_handle(Handler, Action,
                                                         Extra, Arguments,
                                                         Req1, Env)),
                                          Req1, Env).

%% Private --------------------------------------------------------------------

unmarshal_before({before, [], Eval}) ->
    Eval;
unmarshal_before({before, Args, ok}) ->
    {ok, [{before, Args}]};
unmarshal_before({before, Args, {ok, Var}}) ->
    {ok, [{before, Args} | Var]};
unmarshal_before({before, Args, {ok, Var, Headers}}) ->
    {ok, [{before, Args} | Var], Headers};
unmarshal_before(Eval) ->
    Eval.

get_action(Handler, Req0) ->
    {PathInfo, Req1} = cowboy_req:path_info(Req0),
    case get_function_name(Handler, PathInfo) of
        undefined -> continue;
        Action    -> {Req1, Action}
    end.

handler_handle(Handler, Action, PathInfo, Arguments, Req0, Env) ->
    case code:ensure_loaded(Handler) of
        {module, Handler} ->
            case maybe_do_before(Handler, Action, Req0, Env) of
                {redirect, _Location} = Redirect ->
                    Redirect;
                {error, 500} = Error ->
                    Error;
                {ok, BeforeArgs} ->
                    case erlang:function_exported(Handler, Action, 4) of
                        true ->
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
                            ?do_or_error(F, Req0, Handler, 4, Action, Env);
                        false -> ok
                    end
            end;
        {error, _Reason} -> continue
    end.

maybe_do_before(Handler, Action, Req0, Env) ->
    case erlang:function_exported(Handler, before_, 2) of
        true ->
            F = ?lazy(apply(Handler, before_, [Action, Req0])),
            ?do_or_error(F, Req0, Handler, 2, before_, Env);
        false -> {ok, []}
    end.

get_function_name(_Handler, undefined) ->
    index_;
get_function_name(Handler, []) ->
    ensure_action(Handler, "index_");
get_function_name(Handler, PathInfo) ->
    Function = binary_to_list(hd(PathInfo)),
    ?do_or_else(?lazy(list_to_existing_atom(Function)),
                fun(_) -> ensure_action(Handler, Function) end).

ensure_action(Handler, Function) ->
    Prefix = atom_to_list(Handler),
    %% TODO: Possible atom leakage
    Template = list_to_atom(Prefix ++ "_" ++ Function ++ "_dtl"),
    case code:ensure_loaded(Template) of
        {module, Template} -> list_to_existing_atom(Function);
        {error, _Reason}   -> undefined
    end.

get_extra([]) ->
    [];
get_extra(undefined) ->
    [];
get_extra(PathInfo) ->
    tl(PathInfo).
