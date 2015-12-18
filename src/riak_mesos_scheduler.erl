-module(riak_mesos_scheduler).

-behaviour(erl_mesos_scheduler).

-include_lib("erl_mesos/include/erl_mesos.hrl").

-export([init/1,
         registered/3,
         reregistered/2,
         disconnected/2,
         resource_offers/3,
         offer_rescinded/3,
         error/3,
         handle_info/3,
         terminate/3]).

-record(state, {callback,
                test_pid}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(Options) ->
    FrameworkInfo = framework_info(Options),
    TestPid = proplists:get_value(test_pid, Options),
    {ok, FrameworkInfo, true, #state{callback = init,
                                     test_pid = TestPid}}.

registered(SchedulerInfo, EventSubscribed,
           #state{test_pid = TestPid} = State) ->
    reply(TestPid, {registered, self(), SchedulerInfo, EventSubscribed}),
    {ok, State#state{callback = registered}}.

disconnected(SchedulerInfo, #state{test_pid = TestPid} = State) ->
    reply(TestPid, {disconnected, self(), SchedulerInfo}),
    {ok, State#state{callback = disconnected}}.

reregistered(SchedulerInfo, #state{test_pid = TestPid} = State) ->
    reply(TestPid, {reregistered, self(), SchedulerInfo}),
    {ok, State#state{callback = reregistered}}.

resource_offers(SchedulerInfo, EventOffers,
                #state{test_pid = TestPid} = State) ->
    reply(TestPid, {resource_offers, self(), SchedulerInfo, EventOffers}),
    {ok, State#state{callback = resource_offers}}.

offer_rescinded(SchedulerInfo, EventRescind,
                #state{test_pid = TestPid} = State) ->
    reply(TestPid, {offer_rescinded, self(), SchedulerInfo, EventRescind}),
    {ok, State#state{callback = offer_rescinded}}.

error(SchedulerInfo, EventError, #state{test_pid = TestPid} = State) ->
    reply(TestPid, {error, self(), SchedulerInfo, EventError}),
    {stop, State#state{callback = error}}.

handle_info(_SchedulerInfo, stop, State) ->
    {stop, State};
handle_info(_SchedulerInfo, _Info, State) ->
    {ok, State}.

terminate(SchedulerInfo, Reason, #state{test_pid = TestPid} = State) ->
    reply(TestPid, {terminate, self(), SchedulerInfo, Reason, State}).

%% ====================================================================
%% Private
%% ====================================================================

framework_info(Options) ->
    User = proplists:get_value(user, Options, <<>>),
    Name = proplists:get_value(name, Options, <<>>),
    Id = proplists:get_value(id, Options, undefined),
    FailoverTimeout = proplists:get_value(failover_timeout, Options, undefined),
    Checkpoint = proplists:get_value(checkpoint, Options, undefined),
    Role = proplists:get_value(role, Options, undefined),
    Hostname = proplists:get_value(hostname, Options, undefined),
    Principal = proplists:get_value(principal, Options, undefined),
    WebuiUrl = proplists:get_value(webui_url, Options, undefined),
    Capabilities = proplists:get_value(capabilities, Options, undefined),
    Labels = proplists:get_value(labels, Options, undefined),
    #framework_info{user = User,
                    name = Name,
                    id = Id,
                    failover_timeout = FailoverTimeout,
                    checkpoint = Checkpoint,
                    role = Role,
                    hostname = Hostname,
                    principal = Principal,
                    webui_url = WebuiUrl,
                    capabilities = Capabilities,
                    labels = Labels}.

reply(undefined, _Message) ->
    undefined;
reply(TestPid, Message) ->
    TestPid ! Message.
