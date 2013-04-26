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

%% @doc Handle Giallo's returntypes.
%%
%% This module provides one function <em>eval/5</em> which evaluates any
%% return value from a Giallo handler.
%% @end

-module(giallo_response).

-export([eval/1]).

-include("giallo.hrl").

-define(DEFAULT_CT, [{<<"content-type">>, <<"text/html">>}]).
-define(JSON_CT,    [{<<"content-type">>, <<"application/json">>}]).
-define(JS_CT,      [{<<"content-type">>, <<"application/javascript">>}]).

%% API ------------------------------------------------------------------------

-spec eval(GialloReq) ->
    {halt, Req} | {error, Status, Req} | {ok, Req, Env} when
    GialloReq :: giallo:giallo_req(),
    Req       :: cowboy_req:req(),
    Env       :: cowboy_middleware:env(),
    Status    :: non_neg_integer().
eval(GialloReq) ->
    do_eval(unmarshal_req(unmarshal_before(GialloReq))).

%% Private --------------------------------------------------------------------

unmarshal_before(#g{ resp={before, ok}, before_args=Args }=GialloReq) ->
    GialloReq#g{ resp={ok, [{<<"_before">>, Args}]} };
unmarshal_before(#g{ resp={before, {ok, Var}}, before_args=Args }=GialloReq) ->
    GialloReq#g{ resp={ok, [{<<"_before">>, Args} | Var]} };
unmarshal_before(#g{ resp={before, {ok, Var, Headers}}
                , before_args=Args }=GialloReq) ->
    GialloReq#g{ resp={ok, [{<<"_before">>, Args} | Var], Headers} };
unmarshal_before(#g{ resp={before, {render_other, Props, Var}}
                , before_args=Args }=GialloReq) ->
    GialloReq#g{ resp={render_other, Props, [{<<"_before">>, Args} | Var]} };
unmarshal_before(#g{ resp={before, Eval} }=GialloReq) ->
    GialloReq#g{ resp=Eval };
unmarshal_before(GialloReq) ->
    GialloReq.

unmarshal_req(#g{ resp={Eval, X, Req} }=GialloReq) when is_tuple(Req) ->
    GialloReq#g{ resp={Eval, X}, req=Req };
unmarshal_req(#g{ resp={Eval, X, Y, Req} }=GialloReq) when is_tuple(Req) ->
    GialloReq#g{ resp={Eval, X, Y}, req=Req };
unmarshal_req(#g{ resp={Eval, X, Y, Z, Req} }=GialloReq) when is_tuple(Req) ->
    GialloReq#g{ resp={Eval, X, Y, Z}, req=Req };
unmarshal_req(GialloReq) ->
    GialloReq.

do_eval(#g{ resp=ok }=GialloReq) ->
    do_eval(GialloReq#g{ resp={ok, []} });
do_eval(#g{ resp={ok, Var} }=GialloReq) ->
    do_eval(GialloReq#g{ resp={ok, Var, []} });
do_eval(#g{ resp={ok, Var, Headers} }=GialloReq) ->
    do_eval(GialloReq#g{ resp=render_template(GialloReq, Var, Headers) });
do_eval(#g{ resp={redirect, Location} }=GialloReq) when is_binary(Location) ->
    do_eval(GialloReq#g{ resp={redirect, Location, []} });
do_eval(#g{ resp={redirect, Location, Headers}, req=Req })
                                                   when is_binary(Location) ->
    redirect_or_move(302, Location, Headers, Req);
do_eval(#g{ resp={moved, Location} }=GialloReq)    when is_binary(Location) ->
    do_eval(GialloReq#g{ resp={moved, Location, []} });
do_eval(#g{ resp={moved, Location, Headers}, req=Req })
                                                   when is_binary(Location) ->
    redirect_or_move(301, Location, Headers, Req);
do_eval(#g{ resp={action_other, Props} }=GialloReq) ->
    do_eval((GialloReq#g{ resp=action_other(GialloReq, Props) }));
do_eval(#g{ resp={action_other, Props, Args} }=GialloReq) ->
    do_eval((GialloReq#g{ resp=action_other(GialloReq, Props), args=Args }));
do_eval(#g{ resp={render_other, Props} }=GialloReq) ->
    do_eval(GialloReq#g{ resp={render_other, Props, []} });
do_eval(#g{ resp={render_other, Props, Args} }=GialloReq) ->
    do_eval(GialloReq#g{ resp=render_other(GialloReq, Props, Args) });
do_eval(#g{ resp={stream, Fun, Acc} }=GialloReq) ->
    do_eval(GialloReq#g{ resp={stream, Fun, Acc, []} });
do_eval(#g{ resp={stream, Fun, Acc, Headers}, req=Req0 }=GialloReq) ->
    {ok, Req1} = cowboy_req:chunked_reply(200, Req0),
    ok = stream(GialloReq#g{ req=Req1 }, Fun, Acc),
    {halt, cowboy_req:set([ {resp_headers, Headers} ], Req1)};
do_eval(#g{ resp={json, Data} }=GialloReq) ->
    do_eval(GialloReq#g{ resp={json, Data, []} });
do_eval(#g{ resp={json, Data, []} }=GialloReq) ->
    do_eval(GialloReq#g{ resp={json, Data, ?JSON_CT} });
do_eval(#g{ resp={json, Data, Headers} }=GialloReq) ->
    do_eval(GialloReq#g{ resp=encode_json(GialloReq, Data, Headers) });
do_eval(#g{ resp={jsonp, Callback, Data} }=GialloReq) ->
    do_eval(GialloReq#g{ resp={jsonp, Callback, Data, []} });
do_eval(#g{ resp={jsonp, Callback, Data, []} }=GialloReq) ->
    do_eval(GialloReq#g{ resp={jsonp, Callback, Data, ?JS_CT} });
do_eval(#g{ resp={jsonp, Callback, Data, Headers} }=GialloReq) ->
    case encode_json(GialloReq, Data, Headers) of
        {output, Json, Headers} ->
            do_eval(GialloReq#g{ resp={output
                               , <<Callback/binary,"(",Json/binary,");">>
                               , Headers} });
        Error                   -> do_eval(GialloReq#g{ resp=Error })
    end;
do_eval(#g{ resp={output, Output} }=GialloReq) ->
    do_eval(GialloReq#g{ resp={output, Output, []} });
do_eval(#g{ resp={output, Output, []} }=GialloReq) ->
    do_eval(GialloReq#g{ resp={output, Output, ?DEFAULT_CT} });
do_eval(#g{ resp={output, Output, Headers}, req=Req0 }) ->
    {ok, Req1} = cowboy_req:reply(200, Headers, Output, Req0),
    {halt, Req1};
do_eval(#g{ resp=not_found, req=Req0 }) ->
    {ok, Req1} = cowboy_req:reply(404, Req0),
    {halt, Req1};
do_eval(#g{ resp={error, Status}, req=Req }) ->
    {error, Status, Req};
do_eval(#g{ resp=continue, req=Req, env=Env }) ->
    {ok, Req, Env}.

stream(#g{ req=Req }=GialloReq, Fun, Acc0) ->
    case Fun(Acc0) of
        {output, Data, Acc1} ->
            ok = cowboy_req:chunk(Data, Req),
            stream(GialloReq, Fun, Acc1);
        done ->
            ok
    end.

encode_json(GialloReq, Data, Headers) ->
    try {output, jsx:encode(Data), Headers}
    catch C:R ->
        giallo_util:error(GialloReq#g{ action=jsx, handler=encode }
                         , 1, C, R, erlang:get_stacktrace())
    end.

action_other(#g{ handler=Handler }=GialloReq, Props) ->
    giallo_middleware:execute_handler(
        GialloReq#g{
            handler=giallo_util:any2ea(get_value(controller, Props, Handler))
            , action=get_value(action, Props) }).

render_other(#g{ handler=Handler }=GialloReq, Props, Args) ->
    render_template(GialloReq#g{ handler=get_value(controller, Props, Handler)
                   , action=get_value(action, Props) }, Args, []).

redirect_or_move(Status, Location, Headers, Req0) ->
    {halt, cowboy_req:set([{connection, close}, {resp_state, done}]
    , unwrap(cowboy_req:reply(Status, [{<<"location">>, Location}]
    , Headers, Req0)))}.

render_template(#g{ handler=Handler, action=Action }=GReq, Args, Headers) ->
    try
        case code:ensure_loaded(giallo_util:any2ea(atom_to_list(Handler)
                                ++"_"++atom_to_list(Action)++"_dtl"))
        of
            {module, Template} ->
                {output,
                    unwrap(apply(Template, render, [Args])), Headers};
            {error, _Reason}   -> continue
        end
    catch C:R ->
       giallo_util:error(GReq, erlang:length(Args), C, R
                        , erlang:get_stacktrace())
    end.

get_value(Key, List) -> get_value(Key, List, undefined).

get_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false      -> Default;
        {Key, Val} -> Val
    end.

unwrap({ok, Val}) -> Val;
unwrap(Val)       -> Val.
