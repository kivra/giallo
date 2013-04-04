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

-module(giallo_multipart).

-include("giallo.hrl").

-export([param/2]).
-export([file/2]).
-export([stream_param/4]).

%% API ------------------------------------------------------------------------

%% @doc Returns the value of a multipart request, or undefined if not found.
-spec param(Param, Req) -> undefined | binary() when
    Param :: binary(),
    Req   :: cowboy_req:req().
param(Param, Req) ->
    stream_param(Param,
                     fun(eof, _Meta, Acc) ->
                             iolist_to_binary(lists:reverse(Acc));
                        (Frag, _Meta, Acc) ->
                             [Frag|Acc]
                     end,
                     [],
                     Req).

%% @doc
%% Streams fragments of a multipart part by repeatedly calling
%% Fun(Fragment, Meta, State) where Fragment is a binary containing
%% a part of the body, Meta contains the header fields of the part,
%% and State is a user-specified updated on each call to Fun.
%% When the end of the part is reached, Fun is called with Fragment
%% set to the atom "eof".
%% @end
-spec stream_param(Name, Fun, StateTy, Req) -> undefined | StateTy when
    Name   :: binary(),
    Fun    :: fun(),
    State0 :: any(),
    Req    :: cowboy_req:req(),
    State0 :: any().
stream_param(Name, Fun, State0, Req) ->
    case find_multipart(Name, cowboy_req:multipart_data(Req)) of
        {ok, {Meta, Req2}} ->
            read_multipart(Fun,
                           State0,
                           Meta,
                           cowboy_req:multipart_data(Req2));
        undefined ->
            undefined
    end.

%% @doc
%% Locates a multipart field named Param, assumed to contain a file.
%% Returns {Filename, Body}, where Filename is the result of decoding
%% the "filename" part of the Content-Disposition header.
%% @end
-spec file(Param, Req) -> {Filename, Body} when
    Param    :: binary(),
    Req      :: cowboy_req:req(),
    Filename :: binary(),
    Body     :: binary().
file(Param, Req) ->
    stream_param(Param,
                 fun(eof, Meta, Acc) ->
                        {<<"filename">>, Filename} =
                                    lists:keyfind(<<"filename">>, 1, Meta),
                         {Filename, iolist_to_binary(lists:reverse(Acc))};
                    (Frag, _Meta, Acc) ->
                         [Frag|Acc]
                 end,
                 [],
                 Req).

%% Private --------------------------------------------------------------------

%% @doc
%% Tries to locate a named parameter in a multipart-encoded request.
%% If found, a tuple of the part header and a request object for which
%% calling cowboy_req:multipart_data will start generating
%% body data is returned. Otherwise, undefined is returned.
%% @end
%% @private
-spec find_multipart(Target, Part) -> Result when
      Target :: binary(),
      Part   :: {{headers, [tuple()]}, cowboy_req:req()}
              | {eof, cowboy_req:req()},
      Result :: undefined | {[tuple()], cowboy_req:req()}.
find_multipart(Target, {headers, Headers, Req}) ->
    {<<"content-disposition">>, ContentDisposition} =
        lists:keyfind(<<"content-disposition">>, 1, Headers),
    case ContentDisposition of
        <<"form-data", Rest/binary>> ->
            Meta = cowboy_http:params(Rest, fun(_, Params) -> Params end),
            case lists:keyfind(<<"name">>, 1, Meta) of
                {<<"name">>, Target} ->
                    {ok, {Meta, Req}};
                _ ->
                    {ok, Req2} = cowboy_req:multipart_skip(Req),
                    find_multipart(Target, cowboy_req:multipart_data(Req2))
            end;
        _ ->
            {ok, Req2} = cowboy_req:multipart_skip(Req),
            find_multipart(Target, cowboy_req:multipart_data(Req2))
    end;
find_multipart(_Target, {eof, _Req}) ->
    undefined.

%% @doc
%% Reads the body of a multipart request part, calling a function for every
%% fragment received, returning the result of the last function invocation.
%% @end
%% @private
-spec read_multipart(Fun, State, Meta, Part) -> any() when
    Fun   :: fun(),
    State :: any(),
    Meta  :: [tuple()],
    Part  :: {headers, cowboy_http:headers(), cowboy_req:req()}
           | {body, binary(), cowboy_req:req()}
           | {end_of_part | eof, cowboy_req:req()}.
read_multipart(Fun, State, Meta, {body, Body, Req}) ->
    State2 = Fun(Body, Meta, State),
    read_multipart(
        Fun,
        State2,
        Meta,
        cowboy_req:multipart_data(Req));
read_multipart(Fun, State, Meta, {end_of_part, _Req}) ->
    Fun(eof, Meta, State).
