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
-export([query_param/1]).
-export([post_param/1]).
-export([hi_world/1]).
-export([hi_json/1]).
-export([hi_jsonp/1]).
-export([before_template/1]).
-export([subpath_hi_world/1]).
-export([hello_world/1]).
-export([hello_world_template/1]).
-export([hello_world_template_var/1]).
-export([action_other/1]).
-export([extra_req_return/1]).
-export([not_found/1]).
-export([error_500/1]).
-export([render_other/1]).

%% CT Setup -------------------------------------------------------------------

all() ->
    [
        {group, minimal},
        {group, subpath},
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
            extra_req_return,
            before_template,
            not_found,
            render_other,
            post_param,
            query_param,
            action_other,
            error_500
    ],
    [
        {minimal, [], [minimal]},
        {subpath, [], [subpath_hi_world]},
        {default, [], Tests}
    ].

init_per_suite(Config) ->
    application:start(crypto),
    application:start(inets),
    Config.

end_per_suite(_Config) ->
    inets:stop(),
    ok.

giallo_init() ->
    application:start(ranch),
    application:start(cowboy),
    application:start(giallo).

init_per_group(minimal, Config) ->
    giallo_init(),
    Dispatch = [
                {'_', [
                        {"/", minimal_handler, []}
                      ]
                }
               ],
    giallo:start(Dispatch),
    Config;
init_per_group(default, Config) ->
    giallo_init(),
    Dispatch = [
                {'_', [
                        {"/[...]", default_handler, []}
                      ]
                }
               ],
    giallo:start(Dispatch),
    Config;
init_per_group(subpath, Config) ->
    giallo_init(),
    Dispatch = [
                {'_', [
                        {"/subpath/[...]", default_handler, []}
                      ]
                }
               ],
    giallo:start(Dispatch),
    Config.

end_per_group(_, _Config) ->
    application:stop(giallo),
    application:stop(cowboy),
    application:stop(ranch).

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

query_param(Config) ->
    Url = base_url(Config),
    {ok, {Status, Headers, Body}} = httpc:request(Url ++ "query_param?a=b&c=d"),
    {"HTTP/1.1", 200, "OK"} = Status,
    "Ok!" = Body,
    {"content-type", "text/html"} = lists:keyfind("content-type", 1, Headers).

post_param(Config) ->
    Url = base_url(Config),
    {ok, {Status, Headers, Body}} = httpc:request(post,
                            {Url ++ "post_param", [],
                             "application/x-www-form-urlencoded",
                             "a=b&c=d"}, [], []),
    {"HTTP/1.1", 200, "OK"} = Status,
    "Ok!" = Body,
    {"content-type", "text/html"} = lists:keyfind("content-type", 1, Headers).

extra_req_return(Config) ->
    Url = base_url(Config),
    {ok, {Status, Headers, Body}} = httpc:request(Url ++ "extra_req_return"),
    {"HTTP/1.1", 200, "OK"} = Status,
    "Hello World!" = Body,
    {"extra-pextra", "C.R.E.A.M"} =
                                    lists:keyfind("extra-pextra", 1, Headers).

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

action_other(Config) ->
    Url = base_url(Config),
    {ok, {Status, Headers, Body}} = httpc:request(Url ++ "action_other"),
    {"HTTP/1.1", 200, "OK"} = Status,
    "You got rendered!" = Body,
    {"content-type", "text/html"} = lists:keyfind("content-type", 1, Headers).

render_other(Config) ->
    Url = base_url(Config),
    {ok, {Status, Headers, Body}} = httpc:request(Url ++ "render_other"),
    {"HTTP/1.1", 200, "OK"} = Status,
    "Hello World!\n" = Body,
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

before_template(Config) ->
    Url = base_url(Config),
    {ok, {Status, Headers, Body}} = httpc:request(Url ++
                                                   "before_template"),
    {"HTTP/1.1", 200, "OK"} = Status,
    "Before!\n" = Body,
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
    {ok, {Status, Headers, Body}} = httpc:request(Url),
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
