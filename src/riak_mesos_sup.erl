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
        {user, riak_mesos_config:get_value(user, <<"root">>, binary)},
        {name, riak_mesos_config:get_value(name, <<"riak">>, binary)},
        {role, riak_mesos_config:get_value(role, <<"riak">>, binary)},
        {hostname, riak_mesos_config:get_value(hostname, undefined, binary)},
        {principal, riak_mesos_config:get_value(principal, <<"riak">>, binary)},
        {checkpoint, undefined},
        {id, undefined}, %% TODO: Will need to check ZK for this for reregistration
        {failover_timeout, undefined},
        {webui_url, undefined},
        {capabilities, undefined},
        {labels, undefined}
    ],
    Options = [{master_hosts, [riak_mesos_config:get_value(master, <<"master.mesos:5050">>, binary)]}],
    erl_mesos:start_scheduler(Ref, Scheduler, SchedulerOptions, Options),

    Ip = riak_mesos_config:get_value(ip, "0.0.0.0"),
    Port = riak_mesos_config:get_value(port, 9090, integer), %% TODO: Will need to get this dynamically... somehow
    WebConfig = riak_mesos_wm_util:dispatch(Ip, Port),

    RIAK_MESOS_SERVER = {riak_mesos_server,
          {riak_mesos_server, start_link, [[]]},
          permanent, 5000, worker, [riak_mesos_server]},
    WEB = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    Processes = [RIAK_MESOS_SERVER, WEB],

    {ok, { {one_for_one, 10, 10}, Processes} }.
