%% --------------------------------------------------------------------------------
%% @author  Oleksii Semilietov <spylik@gmail.com>
%% @copyright 2017 Oleksii Semilietov <spylik@gmail.com>
%% 
%% @doc
%% Nonce number keeper (OTP application)
%% --------------------------------------------------------------------------------

-module(erlnoncer_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

% @doc start application
-spec start(Type, StartArgs) -> Result when
    Type        :: application:start_type(),
    StartArgs   :: term(),
    Result      :: {'ok', pid()} | {'ok', pid(), State :: term()} | {'error', Reason :: term()}.

start(normal, _StartArgs) ->
	erlnoncer_sup:start_link().

% @doc stop application
-spec stop(State) -> Result when
    State       :: term(),
    Result      :: ok.

stop(_State) ->
    ok.
