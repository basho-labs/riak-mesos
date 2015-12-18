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

-module(riak_mesos_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-include("riak_mesos.hrl").


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
    Ref = {riak_mesos, scheduler},
    Scheduler = riak_mesos_scheduler,
    SchedulerOptions = [
        {user, <<"root">>},
        {name, <<"riak">>},
        {id, undefined},
        {failover_timeout, undefined},
        {checkpoint, undefined},
        {role, <<"riak">>},
        {hostname, undefined},
        {principal, <<"riak">>},
        {webui_url, undefined},
        {capabilities, undefined},
        {labels, undefined}
    ],
    Options = [{master_hosts, [<<"vagrant-ubuntu-trusty-64.local:5050">>]}],
    erl_mesos:start_scheduler(Ref, Scheduler, SchedulerOptions, Options),

    WebConfig = riak_mesos_wm_util:dispatch(),

    RIAK_MESOS_SERVER = {riak_mesos_server,
          {riak_mesos_server, start_link, [[]]},
          permanent, 5000, worker, [riak_mesos_server]},
    WEB = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    Processes = [RIAK_MESOS_SERVER, WEB],

    {ok, { {one_for_one, 10, 10}, Processes} }.
