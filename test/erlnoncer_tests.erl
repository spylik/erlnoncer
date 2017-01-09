% ATTENTION: do not run this tests on production nodes
% we using mlibs:random_atom for creating random names for servers, 
% but atoms will not garbage collected

-module(erlnoncer_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlnoncer.hrl").
-compile(export_all).

-define(TESTMODULE, erlnoncer).
-define(TESTSERVER, testnoncer).

% --------------------------------- fixtures ----------------------------------

% tests for cover standart otp behaviour
otp_test_() ->
    {setup,
        fun() -> error_logger:tty(false) end,
        {inorder,
            [
                {<<"gen_server able to start via ?TESTSERVER:start_link(#{register => {'local',?TESTSERVER}})">>,
                    fun() ->
                        ?TESTMODULE:start_link(#{register => {'local',?TESTSERVER}}),
                        ?assertEqual(
                            true,
                            is_pid(whereis(?TESTSERVER))
                        )
                end},
                {<<"gen_server able to stop via ?TESTSERVER:stop('sync',?TESTSERVER)">>,
                    fun() ->
                        ?assertEqual(ok, ?TESTMODULE:stop('sync',?TESTSERVER)),
                        ?assertEqual(
                            false,
                            is_pid(whereis(?TESTSERVER))
                        )
                end},
                {<<"gen_server able to start with default name via ?TESTSERVER:start_link() and stop via ?TESTSERVER:stop()">>,
                    fun() ->
                        ?assertEqual(
                            false,
                            is_pid(whereis(?TESTMODULE))
                        ),
                        ?TESTMODULE:start_link(),
                        ?assertEqual(
                            true,
                            is_pid(whereis(?TESTMODULE))
                        ),
                        ?assertEqual(ok, ?TESTMODULE:stop()),
                        ?assertEqual(
                            false,
                            is_pid(whereis(?TESTSERVER))
                        )
                end},
                {<<"gen_server able to start with non-default name via ?TESTSERVER:start_link(#{register => {'local',?TESTSERVER}}) and stop via ?TESTSERVER:stop(})">>,
                    fun() ->
                        ?TESTMODULE:start_link(#{register => {'local',?TESTSERVER}}),
                        ?assertEqual(ok, ?TESTMODULE:stop(?TESTSERVER)),
                        ?assertEqual(
                            false,
                            is_pid(whereis(?TESTSERVER))
                        )
                end},
                {<<"gen_server able to start with non-default name via ?TESTSERVER:start_link(#{register => {'local',?TESTSERVER}}) and stop via ?TESTSERVER:stop('async',?TESTSERVER})">>,
                    fun() ->
                        ?assertEqual(
                            false,
                            is_pid(whereis(?TESTSERVER))
                        ),
                        ?TESTMODULE:start_link(#{register => {'local',?TESTSERVER}}),
                        ?assertEqual(
                            true,
                            is_pid(whereis(?TESTSERVER))
                        ),
                        ?assertEqual(ok, ?TESTMODULE:stop('async',?TESTSERVER)),
                        timer:sleep(5),
                        ?assertEqual(
                            false,
                            is_pid(whereis(?TESTSERVER))
                        )
                end}

            ]
        }
    }.

% tests which require started trades as gen_server process and erlroute as well
gen_server_unknown_message_test_ () ->
    {setup,
        fun setup_start/0,
        fun stop_server/1,
        {inparallel,
            [   
                {<<"gen_server must be registered">>,
                    fun() ->
                        ?assertEqual(
                            true,
                            is_pid(whereis(?TESTSERVER))
                        )
                    end},

                {<<"Unknown gen_calls messages must do not crash gen_server">>,
                    fun() ->
                        _ = gen_server:call(?TESTSERVER, {unknown, message}),
                        timer:sleep(1), % for async cast
                        ?assertEqual(
                            true,
                            is_pid(whereis(?TESTSERVER))
                        )
                    end},

                {<<"Unknown gen_cast messages must do not crash gen_server">>,
                    fun() ->
                        gen_server:cast(?TESTSERVER, {unknown, message}),
                        timer:sleep(1), % for async cast
                        ?assertEqual(
                            true,
                            is_pid(whereis(?TESTSERVER))
                        )
                    end},

                {<<"Unknown gen_info messages must do not crash gen_server">>,
                    fun() ->
                        ?TESTSERVER ! {unknown, message},
                        timer:sleep(1), % for async cast
                        ?assertEqual(
                            true,
                            is_pid(whereis(?TESTSERVER))
                        )
                    end}
            ]
        }
    }.


% tests which require started trades as gen_server process and erlroute as well
gen_server_init_routine_test_ () ->
    {setup,
        fun setup_start/0,
        fun stop_server/1,
        {inparallel,
            [
                {<<"ets table must be exists and defined in state">>,
                    fun() ->
                        ServerPid = whereis(?TESTSERVER),
                        EtsName = ?TAB(?TESTMODULE,ServerPid),
                        ?assertNotEqual(
                           undefined,
                           ets:info(EtsName)
                          ),
                        #noncer_state{
                            ets_table = EtsTableFromState
                        } = sys:get_state(?TESTSERVER),
                        ?assertEqual(EtsName, EtsTableFromState)
                    end},
                {<<"heartbeat_freq must be defined and heartbeat_tref must be present">>,
                    fun() ->
                        #noncer_state{
                            heartbeat_freq = Heartbeat_freq,
                            heartbeat_tref = Heartbeat_tref
                        } = sys:get_state(?TESTSERVER),
                        ?assertEqual(?DefaultFreq, Heartbeat_freq),
                        ?assertNotEqual('undefined', Heartbeat_tref)
                    end},
                {<<"After heartbeat_freq heartbeat_tref must be changed">>,
                    fun() ->
                        #noncer_state{
                            heartbeat_freq = Heartbeat_freq1,
                            heartbeat_tref = Heartbeat_tref1
                        } = sys:get_state(?TESTSERVER),
                        ?assertEqual(?DefaultFreq, Heartbeat_freq1),
                        ?assertNotEqual('undefined', Heartbeat_tref1),
                        timer:sleep(Heartbeat_freq1+1),
                        #noncer_state{
                            heartbeat_freq = Heartbeat_freq2,
                            heartbeat_tref = Heartbeat_tref2
                        } = sys:get_state(?TESTSERVER),
                        ?assertEqual(Heartbeat_freq1, Heartbeat_freq2),
                        ?assertNotEqual('undefined', Heartbeat_tref2),
                        ?assertNotEqual(Heartbeat_tref1, Heartbeat_tref2)
                    end},
                {<<"Able to get nonce update counter in ets">>,
                    fun() ->
                        gen_nonce_testing('seconds', 'integer')
                    end},
                {<<"Able to get nonce update counter in ets">>,
                    fun() ->
                        gen_nonce_testing('seconds', 'list')
                    end},
                {<<"Able to get nonce update counter in ets">>,
                    fun() ->
                        gen_nonce_testing('seconds', 'binary')
                    end},
                {<<"Able to get nonce update counter in ets">>,
                    fun() ->
                        gen_nonce_testing('milli_seconds','integer')
                    end},
                {<<"Able to get nonce update counter in ets">>,
                    fun() ->
                        gen_nonce_testing('milli_seconds', 'list')
                    end},
                {<<"Able to get nonce update counter in ets">>,
                    fun() ->
                        gen_nonce_testing('milli_seconds', 'binary')
                    end}
            ]
        }
    }.

gen_nonce_testing(TimeUnit, OutType) ->
    #noncer_state{
        ets_table = EtsTable,
        heartbeat_freq = Heartbeat_freq
    } = sys:get_state(?TESTSERVER),

    ApiId = erlang:unique_integer([monotonic,positive]),
    Now = erlang:system_time(TimeUnit),
    NonceOne = ?TESTMODULE:nonce(?TESTSERVER, OutType, ApiId, {TimeUnit,Now}),
    case OutType of 
        'integer' ->
            ?assertEqual(1, NonceOne);
        'list' ->
            ?assertEqual("1", NonceOne);
        'binary' ->
            ?assertEqual(<<"1">>, NonceOne)
    end,
    [#nonce_track{shift = Shift1}] = ets:lookup(EtsTable, ApiId),
    ?assertEqual(1, Shift1),
    NonceTwo = ?TESTMODULE:nonce(?TESTSERVER, OutType, ApiId, {TimeUnit,Now}),
    case OutType of
        'integer' ->
            ?assertEqual(2, NonceTwo);
        'list' ->
            ?assertEqual("2", NonceTwo);
        'binary' ->
            ?assertEqual(<<"2">>, NonceTwo)
    end,
    
    [#nonce_track{shift = Shift2}] = ets:lookup(EtsTable, ApiId),
    ?assertEqual(2, Shift2),
    
    timer:sleep(Heartbeat_freq+1),
    ?assertEqual([], ets:lookup(EtsTable, ApiId)),
    
    ets:insert(EtsTable, #nonce_track{
        api_ref = ApiId,
        shift = 98
    }),
    [#nonce_track{shift = Shift98}] = ets:lookup(EtsTable, ApiId),
    ?assertEqual(98, Shift98),
    
    NonceNN = ?TESTMODULE:nonce(?TESTSERVER, OutType, ApiId, {TimeUnit,Now}),
    case OutType of
        'integer' ->
            ?assertEqual(199, NonceNN);
        'list' ->
            ?assertEqual("199", NonceNN);
        'binary' ->
            ?assertEqual(<<"199">>, NonceNN)
    end,
    
    NonceNS = ?TESTMODULE:nonce(?TESTSERVER, OutType, ApiId, {TimeUnit,Now}),
    case OutType of
        'integer' ->
            ?assertEqual(201, NonceNS);
        'list' ->
            ?assertEqual("201", NonceNS);
        'binary' ->
            ?assertEqual(<<"201">>, NonceNS)
    end,
    [#nonce_track{shift = ShiftAfterFlush1}] = ets:lookup(EtsTable, ApiId),
    ?assertEqual(1, ShiftAfterFlush1).

setup_start() ->
    disable_output(),
    start_server().

disable_output() ->
    error_logger:tty(false).

stop_server(_) ->
    case whereis(?TESTSERVER) of
        undefined -> ok;
        _ -> ok = ?TESTMODULE:stop(?TESTSERVER)
    end,
    ok.

start_server() ->
    ?TESTMODULE:start_link(#{register => {'local',?TESTSERVER}}).

