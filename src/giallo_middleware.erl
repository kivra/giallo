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

-export([execute/2]).
-export([execute_handler/5]).

%% API ------------------------------------------------------------------------

-spec execute(Req, Env)
	-> {ok, Req, Env} | {error, 500, Req}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env) ->
    {handler, Handler} = lists:keyfind(handler, 1, Env),
    giallo_response:eval(maybe_do_handler(Handler, Req, Env), Req, Env).

execute_handler(Handler, Action, Arguments, Req0, Env) ->
    {PathInfo, Req1} = cowboy_req:path_info(Req0),
    Extra = get_extra(PathInfo),
    giallo_response:eval(handler_handle(Handler, Action, Extra, Arguments,
                                         Req1, Env), Req1, Env).

%% Private --------------------------------------------------------------------

maybe_do_handler(Handler, Req0, Env) ->
    {PathInfo, Req1} = cowboy_req:path_info(Req0),
    {handler_opts, Arguments} = lists:keyfind(handler_opts, 1, Env),
    Extra = get_extra(PathInfo),
    case get_function_name(PathInfo) of
        undefined -> continue;
        Action    ->
            handler_handle(Handler, Action, Extra, Arguments, Req1, Env)
    end.

handler_handle(Handler, Action, PathInfo, Arguments, Req0, Env) ->
    case erlang:function_exported(Handler, Action, 4) of
        true ->
            F = fun() ->
                    {Method, Req1} = cowboy_req:method(Req0),
                    apply(Handler, Action, [Method, PathInfo, Arguments, Req1])
            end,
            giallo_response:try_fun(F, Req0, Handler, 4, Action, Env);
        false -> continue
    end.

get_function_name(PathInfo) ->
    try list_to_existing_atom(binary_to_list(hd(PathInfo)))
    catch _:_ ->
            undefined
    end.

get_extra([]) ->
    [];
get_extra(PathInfo) ->
    tl(PathInfo).
