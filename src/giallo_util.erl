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


%% @doc Giallo utility function.
%%
%% This module is contains various utility functions internal to Giallo
%% @end

-module(giallo_util).

-include("giallo.hrl").

-export([error/5]).
-export([get_extra/1]).
-export([get_action/1]).
-export([any2ea/1]).

%% API ------------------------------------------------------------------------

%% @doc Print a Giallo error using error_logger
-spec error(GialloReq, Arity, Class, Reason, Stack) -> Error when
    GialloReq   :: giallo:giallo_req()
    ,Arity      :: non_neg_integer()
    ,Class      :: term()
    ,Reason     :: term()
    ,Stack      :: term()
    ,Error      :: {error, 500}.
error(#g{handler=H, action=A, req=R, env=Env}, Arity, Class, Reason, Stack) ->
    error_logger:error_msg(
                "** Giallo handler ~p terminating in ~p/~p~n"
                "   for the reason ~p:~p~n"
                "** Handler state was ~p~n"
                "** Request was ~p~n"
                "** Stacktrace: ~p~n~n",
                [H, A, Arity, Class, Reason, Env,
                    cowboy_req:to_list(R), Stack]),
    {error, 500}.

%% @doc Tries to extract the current action from a Cowboy Req-object.
%%      Action is the first part of the URI.
%%      i.e. URI /action/extra/extra/extra would for a Giallo controller
%%      mapped to / return {@type @{cowboy_req:req(), Action::binary()@}}.
-spec get_action(Req0) -> Action when
    Req0    :: cowboy_req:req()
    ,Action :: {binary(), cowboy_req:req()} |
               {non_existent_action, cowboy_req:req()}.
get_action(Req0) ->
    case cowboy_req:path_info(Req0) of
        {[Action | _], Req1} -> {Action, Req1};
        {_, Req1}            -> {non_existent_action, Req1}
    end.

%% @doc Tries to extract the current extra from a Cowboy Req-object.
%%      Extra is the remainding parts of the URI when action is extracted.
%%      i.e. URI /action/extra/extra/extra would for a Giallo controller
%%      return a list of path segments (as binaries) {@type list(binary())}.
-spec get_extra(Req0) -> Extra when
    Req0   :: cowboy_req:req()
    ,Extra :: {list(binary()), cowboy_req:req()} | {[], cowboy_req:req()}.
get_extra(Req0) ->
    {PathInfo, Req1} = cowboy_req:path_info(Req0),
    {do_get_extra(PathInfo), Req1}.

-spec any2ea(Any) -> AnyAtom when
    Any      :: list() | binary() | atom()
    ,AnyAtom :: atom().
any2ea(A) when is_atom(A)   -> A;
any2ea(L) when is_list(L)   -> any2ea(list_to_existing_atom(L));
any2ea(B) when is_binary(B) -> any2ea(binary_to_list(B)).

%% Private --------------------------------------------------------------------

do_get_extra([]) ->
    [];
do_get_extra(undefined) ->
    [];
do_get_extra([_ | PathInfo]) ->
    PathInfo.
