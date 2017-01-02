%% --------------------------------------------------------------------------------
%% @author  Oleksii Semilietov <spylik@gmail.com>
%% @copyright 2017 Oleksii Semilietov <spylik@gmail.com>
%% 
%% @doc
%% Nonce generation API
%%
%% Module for generate 10 digit nonce (max is 4294967294 what actually is 
%% 11111111111111111111111111111110).
%%
%% Limitation: as we do not expect we will call single API more than 99 times per
%% one scond, we must always use as call parameter NonceInInterval, what actually
%% must be an integer between 1 and 99.
%% 
%% Possible strategies for flushing NonceInInterval:
%% - in any case flush every time once reach 99
%% 
%% @end
%% --------------------------------------------------------------------------------
-module(erlnoncer).

-type nonce()       :: 1..4294967294 | binary() | list().
-type return_nonce():: {'normal' | 'next_interval_nonce_must_flush', nonce()}.

-define(dec282016ms, 1482924639084).
-define(TAB, erlnoncer_tab).

-export_type([
    nonce/0
    ]).

% @doc public api
-export([
      gen_nonce/1, 
      gen_nonce/2,  
      gen_nonce/3
    ]).

% export standart gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% @doc erlnoncer gen_server start_api
-spec start_link() -> Result when
    Result  :: {'ok', pid()} | {'error', term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @doc init gen_server
-spec init([]) -> Result when
    Result  :: {'ok', [], 'infinity'}.

init([]) ->
    _ = ets:new(?TAB, [named_table, ordered_set, private]),
    {ok, [], infinity}.



% To decrease numbers in nonce, bu default we going to start from 28 Dec 2016 (but of course - better every time from api creation date).
% Limitation: we do not expect we will call single API more than 99 times per second.
-spec gen_nonce(NonceInInterval) -> Result when
    NonceInInterval :: 1..99, % last incremental number (we should update number to 0 every N-seconds)
    Result          :: return_nonce().

gen_nonce(NonceInInterval) -> 
    gen_nonce(?dec282016ms, NonceInInterval).

-spec gen_nonce(Since,NonceInInterval) -> Result when
    Since           :: mlibs:mtime(),
    NonceInInterval :: 1..99, 
    Result          :: return_nonce().

gen_nonce(Since,NonceInInterval) ->
    gen_nonce('integer', Since, NonceInInterval).

% @doc Generate monotonic nonce and produce input with defined type
-spec gen_nonce(OutType, Since, NonceInInterval) -> Result when
    OutType         :: 'integer' | 'binary' | 'list',
    Since           :: mlibs:mtime(),
    NonceInInterval :: 1..99,
    Result          :: return_nonce().

gen_nonce(Type,Since,NonceInInterval) ->
    Base = erlang:system_time('seconds')-erlang:convert_time_unit(Since, 'milli_seconds', 'seconds'),
    gen_nonce_with_base(Type,NonceInInterval, Base).

-spec gen_nonce_with_base(OutType, NonceInInterval, Base) -> Result when
    OutType         :: 'integer' | 'binary' | 'list',
    NonceInInterval :: 1..99,
    Base            :: nonce(),
    Result          :: return_nonce().

gen_nonce_with_base('list', NonceInInterval, Base) when NonceInInterval < 10 ->
    {'normal',lists:append([integer_to_list(Base),"0",integer_to_list(NonceInInterval)])};
gen_nonce_with_base('list', NonceInInterval, Base) when NonceInInterval < 99 ->
    {'normal', lists:append([integer_to_list(Base), integer_to_list(NonceInInterval)])};
gen_nonce_with_base('list', 99, Base) ->
    {'next_interval_nonce_must_flush', lists:append([integer_to_list(Base),"99"])};
gen_nonce_with_base('integer', NonceInInterval, Base) when NonceInInterval < 10 ->
    {'normal', list_to_integer(lists:append([integer_to_list(Base),"0",integer_to_list(NonceInInterval)]))};
gen_nonce_with_base('integer', NonceInInterval, Base) when NonceInInterval < 99 ->
    {'normal', list_to_integer(lists:append([integer_to_list(Base), integer_to_list(NonceInInterval)]))};
gen_nonce_with_base('integer', 99, Base) ->
    {'next_interval_nonce_must_flush', list_to_integer(lists:append([integer_to_list(Base), "99"]))}.
