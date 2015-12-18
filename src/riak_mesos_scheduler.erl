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

-record(state, {callback}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(Options) ->
    FrameworkInfo = framework_info(Options),
    lager:info("~n~p~n~p", [Options, FrameworkInfo]),
    {ok, FrameworkInfo, true, #state{callback = init}}.

registered(_SchedulerInfo, EventSubscribed, State) ->
    lager:info("~p", [EventSubscribed]),
    {ok, State#state{callback = registered}}.

disconnected(_SchedulerInfo, State) ->
    lager:info("disconnected", []),
    {ok, State#state{callback = disconnected}}.

reregistered(_SchedulerInfo, State) ->
    lager:info("reregistered", []),
    {ok, State#state{callback = reregistered}}.

resource_offers(_SchedulerInfo, EventOffers,State) ->
    lager:info("~p", [EventOffers]),
    {ok, State#state{callback = resource_offers}}.

offer_rescinded(_SchedulerInfo, EventRescind, State) ->
    lager:info("~p", [EventRescind]),
    {ok, State#state{callback = offer_rescinded}}.

error(_SchedulerInfo, EventError, State) ->
    lager:error("~p", [EventError]),
    {stop, State#state{callback = error}}.

handle_info(_SchedulerInfo, stop, State) ->
    lager:info("stopped", []),
    {stop, State};
handle_info(_SchedulerInfo, Info, State) ->
    lager:warn("~p", [Info]),
    {ok, State}.

terminate(_SchedulerInfo, Reason, _State) ->
    lager:warning("~p", [Reason]),
    ok.

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
