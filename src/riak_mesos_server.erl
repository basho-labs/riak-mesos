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

-module(riak_mesos_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([get_status/0, set_status/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("riak_mesos.hrl").

-record(state, {status}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

get_status() ->
    gen_server:call(?MODULE, {get_status}).

set_status(Status) ->
    gen_server:cast(?MODULE, {set_status, Status}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_Args) ->
    process_flag(trap_exit, true),
    InitialData = [{status, <<"Initial Status">>}],
    {ok, #state{status=InitialData}}.

handle_call({get_status}, _From, State) ->
    {reply, State#state.status, State};
handle_call(Message, _From, State) ->
    lager:error("Received undefined message in call: ~p", [Message]),
    {reply, {error, undefined_message}, State}.

handle_cast({set_status, Status}, State) ->
    {noreply, State#state{status=Status}};
handle_cast(Message, State) ->
    lager:error("Received undefined message in cast: ~p", [Message]),
    {noreply, State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};
handle_info(Message, State) ->
    lager:warning("Received unknown message: ~p", [Message]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    lager:error("riak_mesos_server terminated, reason: ~p", [Reason]),
    ok.

%%%===================================================================
%%% Private
%%%===================================================================
