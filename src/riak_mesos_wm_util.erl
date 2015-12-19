%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Basho Technologies Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(riak_mesos_wm_util).

-export([
  dispatch/2,
  build_routes/1,
  build_routes/2]).

-include("riak_mesos.hrl").

%%%===================================================================
%%% API
%%%===================================================================

dispatch(Ip, Port) ->
    Resources = [
        riak_mesos_wm_resource:dispatch()
    ],
    [
        {ip, Ip},
        {port, Port},
        {nodelay, true},
        {log_dir, "log"},
        {dispatch, lists:flatten(Resources)}
    ].

build_routes(Routes) ->
    build_routes([
        [?RIAK_MESOS_BASE_ROUTE, ?RIAK_MESOS_API_VERSION]
    ], Routes, []).

build_routes(Prefixes, Routes) ->
    build_routes(Prefixes, Routes, []).

%%%===================================================================
%%% Private
%%%===================================================================

build_routes([], _, Acc) ->
    Acc;
build_routes([P|Prefixes], Routes, Acc) ->
    PRoutes = build_prefixed_routes(P, Routes, []),
    build_routes(Prefixes, Routes, Acc ++ PRoutes).

build_prefixed_routes(_, [], Acc) ->
    lists:reverse(Acc);
build_prefixed_routes(Prefix, [R|Routes], Acc) ->
    R0 = Prefix ++ R,
    build_prefixed_routes(Prefix, Routes, [R0|Acc]).
