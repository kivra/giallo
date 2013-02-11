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

-module(giallo_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests
-export([minimal/1]).
-export([moved/1]).
-export([redirect/1]).
-export([hi_world/1]).
-export([hi_json/1]).
-export([hi_jsonp/1]).
-export([subpath_hi_world/1]).
-export([hello_world/1]).
-export([hello_world_template/1]).
-export([hello_world_template_var/1]).
-export([not_found/1]).
-export([error_500/1]).
-export([render_other/1]).

%% CT Setup -------------------------------------------------------------------

all() ->
    [
        {group, minimal},
        {group, default}
    ].

groups() ->
    Tests = [
            hi_json,
            hi_jsonp,
            hi_world,
            moved,
            redirect,
            hello_world,
            hello_world_template,
            hello_world_template_var,
            subpath_hi_world,
            not_found,
            render_other,
            error_500
    ],
    [
        {minimal, [], [minimal]},
        {default, [], Tests}
    ].

init_per_suite(Config) ->
    application:start(inets),
    Config.

end_per_suite(_Config) ->
    inets:stop(),
    ok.

init_per_group(minimal, Config) ->
    Dispatch = [
                {'_', [
                        {"/[...]", minimal_handler, []}
                      ]
                }
               ],
    giallo:start(Dispatch),
    Config;
init_per_group(default, Config) ->
    Dispatch = [
                {'_', [
                        {"/[...]", default_handler, []},
                        {"/subpath/[...]", default_handler, []}
                      ]
                }
               ],
    giallo:start(Dispatch),
    Config.

end_per_group(_, _Config) ->
    giallo:stop().

%% Tests ----------------------------------------------------------------------

subpath_hi_world(Config) ->
    Url = base_url(Config),
    {ok, {Status, Headers, Body}} = httpc:request(Url ++ "subpath/hi/you"),
    {"HTTP/1.1", 200, "OK"} = Status,
    "Ohai!" = Body,
    {"content-type", "text/html"} = lists:keyfind("content-type", 1, Headers).

hi_world(Config) ->
    Url = base_url(Config),
    {ok, {Status, Headers, Body}} = httpc:request(Url ++ "hi/you"),
    {"HTTP/1.1", 200, "OK"} = Status,
    "Ohai!" = Body,
    {"content-type", "text/html"} = lists:keyfind("content-type", 1, Headers).

hi_json(Config) ->
    Url = base_url(Config),
    {ok, {Status, Headers, Body}} = httpc:request(Url ++ "hi/json"),
    {"HTTP/1.1", 200, "OK"} = Status,
    "{\"jason\":\"Ohai!\"}" = Body,
    {"content-type", "application/json"} =
                                    lists:keyfind("content-type", 1, Headers).

hi_jsonp(Config) ->
    Url = base_url(Config),
    {ok, {Status, Headers, Body}} = httpc:request(Url ++ "hi/jsonp"),
    {"HTTP/1.1", 200, "OK"} = Status,
    "callback({\"jason\":\"Ohai!\"});" = Body,
    {"content-type", "application/javascript"} =
                                    lists:keyfind("content-type", 1, Headers).

moved(Config) ->
    Url = base_url(Config),
    {ok, {Status, _Headers, _Body}} = httpc:request(get, {Url ++ "moved", []},
                                                  [{autoredirect, false}],
                                                  []),
    {"HTTP/1.1",301,"Moved Permanently"} = Status.

render_other(Config) ->
    Url = base_url(Config),
    {ok, {Status, Headers, Body}} = httpc:request(Url ++ "render_other"),
    {"HTTP/1.1", 200, "OK"} = Status,
    "You got rendered!" = Body,
    {"content-type", "text/html"} = lists:keyfind("content-type", 1, Headers).

redirect(Config) ->
    Url = base_url(Config),
    {ok, {Status, _Headers, _Body}} = httpc:request(get, {Url ++ "redirect",
                                                          []},
                                                  [{autoredirect, false}],
                                                  []),
    {"HTTP/1.1",302,"Found"} = Status.

hello_world(Config) ->
    Url = base_url(Config),
    {ok, {Status, _Headers, Body}} = httpc:request(Url),
    {"HTTP/1.1", 200, "OK"} = Status,
    "Hello World!" = Body.

hello_world_template(Config) ->
    Url = base_url(Config),
    {ok, {Status, Headers, Body}} = httpc:request(Url ++
                                                   "hello_world_template"),
    {"HTTP/1.1", 200, "OK"} = Status,
    "Hello World!\n" = Body,
    {"content-type", "text/html"} = lists:keyfind("content-type", 1, Headers).

hello_world_template_var(Config) ->
    Url = base_url(Config),
    {ok, {Status, Headers, Body}} = httpc:request(Url ++
                                                   "hello_world_template_var"),
    {"HTTP/1.1", 200, "OK"} = Status,
    "Hello World!\n" = Body,
    {"content-type", "text/html"} = lists:keyfind("content-type", 1, Headers).

not_found(Config) ->
    Url = base_url(Config),
    {ok, {Status, _Headers, _Body}} = httpc:request(Url ++ "not_found"),
    {"HTTP/1.1", 404, "Not Found"} = Status.

error_500(Config) ->
    Url = base_url(Config),
    {ok, {Status, _Headers, _Body}} = httpc:request(Url ++ "error_500"),
    {"HTTP/1.1", 500, "Internal Server Error"} = Status.

minimal(Config) ->
    Url = base_url(Config),
    {ok, {Status, Headers, Body}} = httpc:request(Url ++ "hi/you"),
    {"HTTP/1.1", 200, "OK"} = Status,
    "Ohai!" = Body,
    {"content-type", "text/html"} = lists:keyfind("content-type", 1, Headers).

%% Private --------------------------------------------------------------------

base_url(Config) ->
    Port = get_config(port, Config, 8080),
    "http://localhost:" ++ integer_to_list(Port) ++ "/".

get_config(Key, Config, Default) ->
    case lists:keyfind(Key, 1, Config) of
        {Key, Val} -> Val;
        false      -> Default
    end.
