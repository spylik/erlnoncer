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

-define(NOTEST, true).
-ifdef(TEST).
    -compile(export_all).
-endif.

-include("erlnoncer.hrl").

-export_type([
    nonce/0
    ]).

% @doc public api
-export([
        start_link/0,
        start_link/1,
        gen_nonce/1, 
        gen_nonce/2,  
        gen_nonce/3
    ]).

% @doc export standart gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% @doc when we do not have start parameters, we going to start with empty map as default
-spec start_link() -> Result when
    Result      :: {'ok', pid()} | {'error', term()}.

start_link() ->
    start_link(#{}).

% @doc gen_server start api
-spec start_link(Prop) -> Result when
    Prop        :: start_prop(),
    Result      :: {'ok', pid()} | {'error', term()}.

start_link(Prop) ->
    Register = maps:get('register', Prop, 'undefined'),
    InitState = #nonce_state{
        
    },
    case Register =:= 'undefined' of
        true ->
            gen_server:start_link(?MODULE, InitState, []);
        false ->
            gen_server:start_link(Register, ?MODULE, InitState, [])
    end.


% @doc init gen_server
-spec init([]) -> Result when
    Result  :: {'ok', [], 'infinity'}.

init([]) ->
    _ = ets:new(?TAB, [ordered_set, private, {keypos, #nonce_track.api_ref}, named_table]),
    {ok, 
        #nonce_state{

        }
    }.

%--------------handle_call----------------

% @doc callbacks for gen_server handle_call.
-spec handle_call(Message, From, State) -> Result when
    Message     :: term(),
    From        :: {pid(), Tag},
    Tag         :: term(),
    State       :: nonce_state(),
    Result      :: {reply, term(), State}.


% handle_call for all other thigs
handle_call(Msg, _From, State) ->
    error_logger:warning_msg("we are in undefined handle call with message ~p~n",[Msg]),
    {reply, ok, State}.

%-----------end of handle_call-------------


%--------------handle_cast-----------------

% @doc callbacks for handle_cast
-spec handle_cast(Message, State) -> Result when
    Message :: market_frames:market_frame(),
    State   :: nonce_state(),
    Result  :: {noreply, State} | {stop, normal, State}.

% handle_cast for stop
handle_cast(stop, State) ->
    {stop, normal, State};

% handle_cast for all other thigs
handle_cast(Msg, State) ->
    error_logger:warning_msg("we are in undefined handle cast with message ~p~n",[Msg]),
    {noreply, State}.
%-----------end of handle_cast-------------


%--------------handle_info-----------------

% @doc callbacks for gen_server handle_info.
-spec handle_info(Message, State) -> Result when
    Message :: term(),
    State   :: nonce_state(),
    Result  :: {noreply, State}.

%% handle_info for all other thigs
handle_info(Msg, State) ->
    error_logger:warning_msg("we are in undefined handle info with message ~p~n",[Msg]),
    {noreply, State}.
%-----------end of handle_info-------------

% @doc call back for gen_server terminate
-spec terminate(Reason, State) -> term() when
    Reason  :: 'normal' | 'shutdown' | {'shutdown',term()} | term(),
    State   :: nonce_state().

terminate(Reason, State) ->
    {noreply, Reason, State}.

% @doc call back for gen_server code_change
-spec code_change(OldVsn, State, Extra) -> Result when
    OldVsn  :: Vsn | {down, Vsn},
    Vsn     :: term(),
    State   :: nonce_state(),
    Extra   :: term(),
    Result  :: {ok, NewState},
    NewState :: nonce_state().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% --------------------------- end of gen_server part ---------------------------

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
