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

%% @doc Giallo middleware.
%%
%% This module is implemented as a Cowboy middleware and checks if a given
%% request should be handled by a Giallo handler. If so it executes that
%% handler and evaluates the response. If Giallo determines that it shouldn't
%% execute the handler it will hand it off to Cowboy or return 404 depending
%% on if the handler implements the correct Cowboy behavior or not.
%% @end

-module(giallo_middleware).
-behaviour(cowboy_middleware).

-include("giallo.hrl").

-export([execute/2]).
-export([execute_handler/5]).

%% API ------------------------------------------------------------------------

%% @doc This function is called from Cowboy for every request. For every call
%%      it will check to see if it should execute a Giallo handler, or
%%      transparently pass on to Cowboy. The flow of control is:
%%
%%      <pre>
%%      Cowboy
%%        |
%%      Giallo ({@link giallo_middleware:execute/2})
%%        |
%%      Is handler valid?
%%        |  \ no
%%        |   - Hand over the control to Cowboy to 404 it
%%        |
%%        | yes
%%      Is action valid?
%%        |  \ no
%%        |   \
%%        |    - Is the index_/4 action valid?
%%        |    | \ no
%%        |    |  \
%%        |    |   - Is the handler a valid Cowboy handler?
%%        |    |   |  \ no
%%        |    |   |   \
%%        |    |   |    - Return a 404
%%        |    |   |
%%        |    | Hand over control to Cowboy to continue processing
%%        |    |
%%        |  Execute(*) the index_/4 action and render the result
%%        |
%%      Execute(*) the action and render the result
%%
%%      (*) Look to see if before_/4 should be executed
%%      </pre>
-spec execute(Req0, Env) ->
    {ok, Req, Env} | {error, 500, Req} | {halt, Req} when
    Req0 :: cowboy_req:req()
    ,Req :: cowboy_req:req()
    ,Env :: cowboy_middleware:env().
execute(Req0, Env) ->
    {handler, Handler}        = lists:keyfind(handler, 1, Env),
    {handler_opts, Arguments} = lists:keyfind(handler_opts, 1, Env),
    {Extra, Req1}             = giallo_util:get_extra(Req0),
    {Action, Req2}            = giallo_util:get_action(Req1),

    {ValidAction, Eval} =
        case {valid_handler(Handler)
             , valid_action(Handler, Action)
             , valid_action(Handler, index_)} of
            {true, true, _}      -> run(Handler, Action, Extra, Arguments
                                        , Req2, Env);
            {true, false, true}  -> run(Handler, index_, Extra, Arguments
                                        , Req2, Env);
            {true, false, false} -> {non_existent_action
                                     , maybe_cowboy_continue(Handler)};
            {false, _, _}        -> {non_existent_action, continue}
        end,
    giallo_response:eval(Eval, Req2, Env, Handler, ValidAction).

-spec execute_handler(Handler, Action, Arguments, Req0, Env) ->
    {ok, Req, Env} | {error, 500, Req} | {halt, Req} when
    Handler    :: module()
    ,Action    :: atom()
    ,Arguments :: proplists:proplist()
    ,Req0      :: cowboy_req:req()
    ,Req       :: cowboy_req:req()
    ,Env       :: cowboy_middleware:env().
execute_handler(Handler, Action, Arguments, Req0, Env) ->
    {Extra, Req1} = giallo_util:get_extra(Req0),

    {ValidAction, Eval} =
        case {valid_handler(Handler), valid_action(Handler, Action)} of
            {true, true}  -> run(Handler, Action, Extra, Arguments, Req1, Env);
            {true, false} -> {non_existent_action, {error, 404}};
            {false, _}    -> {non_existent_action, continue}
        end,
    giallo_response:eval(Eval, Req1, Env, Handler, ValidAction).

%% Private --------------------------------------------------------------------

run(Handler, Action0, Extra, Arguments, Req0, Env) ->
    Action1 = ?any2ea(Action0),
    Eval =
        case maybe_do_before(Handler, [Action1, Req0], Req0, Env) of
            {redirect, _Location} = Redirect ->
                Redirect;
            {error, 500} = Error ->
                Error;
            {ok, BeforeArgs} ->
                %% Everything seems Ok, execute the handler
                Parameters = Arguments ++ BeforeArgs,
                {Method, Req1} = cowboy_req:method(Req0),
                HandlerReturn = handler_handle(Handler, Action1, [Method,
                                                Extra,
                                                Parameters,
                                                Req1], Req1, Env),
                {before, BeforeArgs, HandlerReturn}
        end,
    {Action1, unmarshal_before(Eval)}.

valid_handler(Handler) ->
    case code:ensure_loaded(Handler) of
        {module, Handler}  -> true;
        {error, _Reason}   -> false
    end.

valid_action(_Handler, non_existent_action) ->
    false;
valid_action(Handler, Action) ->
    try
        %% if there exist an exported function in the running VM with the name
        %% <em>Action</em> that would get converted to an atom. If not an
        %% erlang bad arg would get thrown and we need to catch that not to
        %% crash the current Cowboy process.
        erlang:function_exported(Handler, ?any2ea(Action), 4)
    catch _:_ ->
        false
    end.

%% @doc Validate if the Handler is indeed a Cowboy handler, if so pass over
%% control to Cowboy, else return 404.
maybe_cowboy_continue(Handler) ->
    case erlang:function_exported(Handler, init, 3) of
        true  -> continue;
        false -> {error, 404}
    end.

handler_handle(Handler, Action, Arguments, Req0, Env) ->
    try apply(Handler, Action, Arguments)
    catch Class:Reason ->
        giallo_util:error(Handler, Action, erlang:length(Arguments),
                          Class, Reason, Env, Req0, erlang:get_stacktrace())
    end.

unmarshal_before({before, [], Eval}) ->
    Eval;
unmarshal_before({before, Args, ok}) ->
    {ok, [{<<"_before">>, Args}]};
unmarshal_before({before, Args, {ok, Var}}) ->
    {ok, [{<<"_before">>, Args} | Var]};
unmarshal_before({before, Args, {ok, Var, Headers}}) ->
    {ok, [{<<"_before">>, Args} | Var], Headers};
unmarshal_before({before, Args, {render_other, Location, Var}}) ->
    {render_other, Location, [{<<"_before">>, Args} | Var]};
unmarshal_before({before, _, Eval}) ->
    Eval;
unmarshal_before(Eval) ->
    Eval.

maybe_do_before(Handler, Arguments, Req0, Env) ->
    case erlang:function_exported(Handler, before_, 2) of
        true  -> handler_handle(Handler, before_, Arguments, Req0, Env);
        false -> {ok, []}
    end.
