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

-module(riak_mesos_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("riak_mesos.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start(_Type, _StartArgs) ->
    application:ensure_started(erl_mesos),
    {ok, Pid} = riak_mesos_sup:start_link(),
    riak_mesos_cli:load_schema(),
    riak_mesos_cli:register(),
    {ok, Pid}.

stop(_State) ->
    ok.
