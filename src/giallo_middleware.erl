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
-export([execute_handler/1]).

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
    {Method, Req3}            = cowboy_req:method(Req2),
    GialloReq                 = #g{ req=Req3, env=Env, handler=Handler
                                  , action=Action, extra=Extra
                                  , args=Arguments, method=Method },

    giallo_response:eval(
        case {valid_handler(GialloReq)
             , valid_action(GialloReq)
             , valid_action(GialloReq#g{action=index_})} of
            {true, true, _}      -> run(GialloReq);
            {true, false, true}  -> run(GialloReq#g{action=index_});
            {true, false, false} -> maybe_continue(GialloReq);
            {false, _, _}        -> continue(GialloReq)
        end).

-spec execute_handler(GialloReq) ->
    {ok, Req, Env} | {error, 500, Req} | {halt, Req} when
    GialloReq :: giallo:giallo_req()
    ,Req      :: cowboy_req:req()
    ,Env      :: cowboy_middleware:env().
execute_handler(#g{ req=Req0 } = GialloReq0) ->
    {Extra, Req1} = giallo_util:get_extra(Req0),
    GialloReq1    = GialloReq0#g{ extra=Extra, req=Req1 },

    giallo_response:eval(
        case {valid_handler(GialloReq1), valid_action(GialloReq1)} of
            {true, true}  -> run(GialloReq1);
            {true, false} -> notfound(GialloReq1);
            {false, _}    -> continue(GialloReq1)
        end).

%% Private --------------------------------------------------------------------

-spec run(giallo:giallo_req()) -> giallo:giallo_req().
run(#g{ args=Args, action=Action, method=Method
                                , req=Req, extra=Extra }=GialloReq0 ) ->
    GialloReq1 = GialloReq0#g{ action=giallo_util:any2ea(Action) },
    case maybe_do_before(GialloReq1) of
        {ok, BeforeArgs} ->
            GialloReq1#g{ before_args=BeforeArgs
                        , resp={before, handler_handle(GialloReq1#g{
                                                        args=[ Method
                                                             , Extra
                                                             , Args
                                                             ++ BeforeArgs
                                                             , Req] })} };
        BeforeResponse -> GialloReq1#g{ resp=BeforeResponse }
    end.

-spec valid_handler(giallo:giallo_req()) -> boolean().
valid_handler(#g{ handler = Handler }) ->
    case code:ensure_loaded(Handler) of
        {module, Handler}  -> true;
        {error, _Reason}   -> false
    end.

-spec valid_action(giallo:giallo_req()) -> boolean().
valid_action(#g{ action=non_existent_action }) ->
    false;
valid_action(#g{ handler=Handler, action=Action }) ->
    try
        %% if there exist an exported function in the running VM with the name
        %% <em>Action</em> that would get converted to an atom. If not an
        %% erlang bad arg would get thrown and we need to catch that not to
        %% crash the current Cowboy process.
        erlang:function_exported(Handler, giallo_util:any2ea(Action), 4)
    catch _:_ ->
        false
    end.

%% @doc Validate if the Handler is indeed a Cowboy handler, if so pass over
%% control to Cowboy, else return 404.
-spec maybe_continue(giallo:giallo_req()) -> giallo:giallo_req().
maybe_continue(#g{ handler=Handler }=GialloReq) ->
    case erlang:function_exported(Handler, init, 3) of
        true  -> continue(GialloReq);
        false -> notfound(GialloReq)
    end.

-spec notfound(giallo:giallo_req()) -> giallo:giallo_req().
notfound(GialloReq) ->
    GialloReq#g{ resp={error, 404} }.

-spec continue(giallo:giallo_req()) -> giallo:giallo_req().
continue(GialloReq) ->
    GialloReq#g{ action=non_existent_action, resp=continue }.

handler_handle(#g{ handler=Handler, action=Action, args=Args }=GialloReq) ->
    try apply(Handler, Action, Args)
    catch Class:Reason ->
        giallo_util:error(GialloReq, erlang:length(Args), Class
                         , Reason, erlang:get_stacktrace())
    end.

maybe_do_before(#g{ handler=Handler }=GialloReq) ->
    case erlang:function_exported(Handler, before_, 2) of
        true  ->
            handler_handle(GialloReq#g{ action=before_
                                      , args=[ GialloReq#g.action
                                             , GialloReq#g.req ] });
        false -> {ok, []}
    end.
