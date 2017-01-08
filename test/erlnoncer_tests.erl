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
                {<<"ets table must be exists">>,
                    fun() ->
                        ServerPid = whereis(?TESTSERVER),
                        EtsName = ?TAB(?TESTMODULE,ServerPid),
                        ?assertNotEqual(
                           undefined,
                           ets:info(EtsName)
                          )
                    end},
                {<<"heartbeat_freq must be defined and heartbeat_tref must be present">>,
                    fun() ->
                        #noncer_state{
                            heartbeat_freq = Heartbeat_freq,
                            heartbeat_tref = Heartbeat_tref
                        } = sys:get_state(?TESTSERVER),
                        ?assertEqual(?DefaultFreq, Heartbeat_freq),
                        ?assertNotEqual('undefined', Heartbeat_tref)
                    end}
            ]
        }
    }.



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

