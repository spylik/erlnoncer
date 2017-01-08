%% --------------------------------------------------------------------------------
%% @author  Oleksii Semilietov <spylik@gmail.com>
%% @copyright 2017 Oleksii Semilietov <spylik@gmail.com>
%% 
%% @doc
%% Supervisor for nonce keeper
%% @end
%% --------------------------------------------------------------------------------
-module(erlnoncer_sup).

% @doc supervisor is here
-behaviour(supervisor).

% @doc export start API.
-export([
        start_link/0
    ]).

% @doc export standart supervisor api.
-export([init/1]).

% @doc start api
-spec start_link() -> Result when
    Result  :: {ok, pid()}.

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% @doc init callback
-spec init([]) -> Result when
    Result      :: {ok, {SupFlags, [ChildSpec]}},
    SupFlags    :: supervisor:sup_flags(),
    ChildSpec   :: supervisor:child_spec().

init([]) ->
	Procs = [#{
            'id' => erlnoncer, 
            'start' => {erlnoncer, start_link, []}, 
            'restart' => permanent,
            'shutdown' => brutal_kill,
            'type' => worker, 
            'modules' => [erlnoncer]
        }],
	{ok, {{simple_one_for_one, 10, 10}, Procs}}.
