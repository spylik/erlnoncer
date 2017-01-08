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
%% Limitation: we do not expect we will call single API more than 99 times per 
%% one second, so current implementation support nonces in this perspective.
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

-define(TAB(Pid), list_to_atom(lists:concat([?MODULE, "_", pid_to_list(Pid)]))).

% @doc public api
-export([
        start_link/0,
        start_link/1
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
    RegisterAs = maps:get('register', Prop, 'undefined'),
    InitState = #noncer_state{
        'heartbeat_freq' = maps:get('heartbeat_freq', Prop, 1000)
    },
    case RegisterAs =:= 'undefined' of
        true ->
            gen_server:start_link({local, ?MODULE}, ?MODULE, InitState, []);
        false ->
            gen_server:start_link(RegisterAs, ?MODULE, InitState, [])
    end.

% @doc init gen_server
-spec init(InitState) -> Result when
    InitState   :: noncer_state(),
    Result      :: {ok, NState},
    NState      :: noncer_state().

init(InitState) ->
    % we going to create ets table like 'erlnoncer_32423'
    TabName = ?TAB(self()),
    _ = ets:new(TabName, [ordered_set, private, {keypos, #nonce_track.api_ref}, named_table]),
    {ok, flush(
            InitState#noncer_state{
                ets_table = TabName
            }
        )
    }.

% @doc flush trefs and data in ets
-spec flush(State) -> NState when
    State   :: noncer_state(),
    NState  :: noncer_state().

flush(State = #noncer_state{
        heartbeat_tref = Heartbeat_tref,
        heartbeat_freq = Heartbeat_freq,
        ets_table = EtsTable
    }) ->
    _ = case Heartbeat_tref of 
        'undefined' -> ok;
        _ -> erlang:cancel_timer(Heartbeat_tref)
    end,
    Time = erlang:system_time('seconds'),
    % need to test what is faster - delete ets and create new for flush or ets:delete_all_objects
    true = ets:delete_all_objects(EtsTable),
    TRef = erlang:send_after(Heartbeat_freq, self(), 'heartbeat'),
    State#noncer_state{
            heartbeat_tref = TRef,
            last_time_insec = Time 
    }.

%--------------handle_call----------------

% @doc callbacks for gen_server handle_call.
-spec handle_call(Message, From, State) -> Result when
    Message     :: term(),
    From        :: {pid(), Tag},
    Tag         :: term(),
    State       :: noncer_state(),
    Result      :: {reply, term(), State}.

handle_call({'gen_nonce', OutType, ApiRef, ApiBase}, _From, State = #noncer_state{
        ets_table = EtsTable,
        last_time_insec = Time,
        heartbeat_freq = Heartbeat_freq
    }) ->
    Base = case ApiBase of
        {'second', Since} ->
            Time - Since;
        {'millisecond', Since} ->
            Time - erlang:convert_time_unit(Since, 'milli_seconds', 'seconds')
    end,
    NonceInInterval = case ets:lookup(EtsTable, ApiRef) of
        [] -> 
            ets:insert(EtsTable, #nonce_track{
                    api_ref = ApiRef,
                    shift = 0
                }),
            1;
        [#nonce_track{shift = Shift}] ->
            NewShift = Shift+1,
            ets:insert(EtsTable, #nonce_track{
                    api_ref = ApiRef,
                    shift = NewShift
                }),
            NewShift
    end,
    {Nonce, NewState} = case gen_nonce_with_base(OutType, NonceInInterval, Base) of
        'next' when Heartbeat_freq =:= 1000 ->
            {{'wait', 1000}, State};
        'next' ->
            {{'wait', 1000}, flush(State)};
        NonceDigit ->
            {NonceDigit, State}
    end,
    {reply, Nonce, NewState};

% handle_call for all other thigs
handle_call(Msg, _From, State) ->
    error_logger:warning_msg("we are in undefined handle call with message ~p~n",[Msg]),
    {reply, ok, State}.

%-----------end of handle_call-------------

%--------------handle_cast-----------------

% @doc callbacks for handle_cast
-spec handle_cast(Message, State) -> Result when
    Message :: 'stop',
    State   :: noncer_state(),
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
    Message :: 'heartbeat',
    State   :: noncer_state(),
    Result  :: {noreply, State}.

% @doc hearbeat
handle_info('heartbeat', State) ->
    {noreply, flush(State)};

%% handle_info for all other thigs
handle_info(Msg, State) ->
    error_logger:warning_msg("we are in undefined handle info with message ~p~n",[Msg]),
    {noreply, State}.
%-----------end of handle_info-------------

% @doc call back for gen_server terminate
-spec terminate(Reason, State) -> term() when
    Reason  :: 'normal' | 'shutdown' | {'shutdown',term()} | term(),
    State   :: noncer_state().

terminate(Reason, State) ->
    {noreply, Reason, State}.

% @doc call back for gen_server code_change
-spec code_change(OldVsn, State, Extra) -> Result when
    OldVsn  :: Vsn | {down, Vsn},
    Vsn     :: term(),
    State   :: noncer_state(),
    Extra   :: term(),
    Result  :: {ok, NewState},
    NewState :: noncer_state().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% --------------------------- end of gen_server part ---------------------------

-spec gen_nonce_with_base(OutType, NonceInInterval, Base) -> Result when
    OutType         :: 'integer' | 'binary' | 'list',
    NonceInInterval :: 0..99,
    Base            :: nonce(),
    Result          :: nonce() | 'next'.

gen_nonce_with_base('list', NonceInInterval, Base) when NonceInInterval < 10 ->
    lists:append([integer_to_list(Base),"0",integer_to_list(NonceInInterval)]);
gen_nonce_with_base('list', NonceInInterval, Base) when NonceInInterval < 99 ->
    lists:append([integer_to_list(Base), integer_to_list(NonceInInterval)]);
gen_nonce_with_base('integer', NonceInInterval, Base) when NonceInInterval < 10 ->
    list_to_integer(lists:append([integer_to_list(Base),"0",integer_to_list(NonceInInterval)]));
gen_nonce_with_base('integer', NonceInInterval, Base) when NonceInInterval < 99 ->
    list_to_integer(lists:append([integer_to_list(Base), integer_to_list(NonceInInterval)]));
gen_nonce_with_base(_, 99, _) ->
    'next'.


% To decrease numbers in nonce, bu default we going to start from 28 Dec 2016 (but of course - better every time from api creation date).
% Limitation: we do not expect we will call single API more than 99 times per second.
%-spec gen_nonce(NonceInInterval) -> Result when
%    NonceInInterval :: 1..99, % last incremental number (we should update number to 0 every N-seconds)
%    Result          :: return_nonce().
%
%gen_nonce(NonceInInterval) ->
%    gen_nonce(?dec282016ms, NonceInInterval).
%
%-spec gen_nonce(Since,NonceInInterval) -> Result when
%    Since           :: mlibs:mtime(),
%    NonceInInterval :: 1..99, 
%    Result          :: return_nonce().
%
%gen_nonce(Since,NonceInInterval) ->
%    gen_nonce('integer', Since, NonceInInterval).
%
%% @doc Generate monotonic nonce and produce input with defined type
%-spec gen_nonce(OutType, Since, NonceInInterval) -> Result when
%    OutType         :: 'integer' | 'binary' | 'list',
%    Since           :: mlibs:mtime(),
%    NonceInInterval :: 1..99,
%    Result          :: return_nonce().
%
%gen_nonce(Type,Since,NonceInInterval) ->
%    Base = erlang:system_time('seconds')-erlang:convert_time_unit(Since, 'milli_seconds', 'seconds'),
%    gen_nonce_with_base(Type,NonceInInterval, Base).


