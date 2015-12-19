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

-module(riak_mesos_config).

-export([
  get_value/2,
  get_value/3
]).

-include("riak_mesos.hrl").

%%%===================================================================
%%% API
%%%===================================================================

get_value(Key, Default) ->
    case get_env_value(Key) of
        false ->
            application:get_env(riak_mesos, Key, Default);
        Value -> Value
    end.

get_value(Key, Default, Type) ->
    case get_value(Key, Default) of
        Default -> Default;
        V -> translate_value(V, Type)
    end.

%% ====================================================================
%% Private
%% ====================================================================

translate_value(Value, integer) when is_list(Value) -> list_to_integer(Value);
translate_value(Value, boolean) when is_list(Value) -> list_to_atom(Value);
translate_value(Value, atom) when is_list(Value) -> list_to_atom(Value);
translate_value(Value, binary) when is_list(Value) -> list_to_binary(Value);
translate_value(Value, _) -> Value.

get_env_value(Key) ->
    Key1 = "RIAK_MESOS_" ++ string:to_upper(atom_to_list(Key)),
    os:getenv(Key1).
