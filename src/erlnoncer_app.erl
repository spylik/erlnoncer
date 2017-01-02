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

%% API.

start(_Type, _Args) ->
	erlnoncer_sup:start_link().

stop(_State) ->
	ok.
