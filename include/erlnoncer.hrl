-type nonce()       :: 1..4294967294 | binary() | list().

-type server()      :: atom().


-type out_type()    :: 'binary' | 'list' | 'integer'.

-define(TAB(Module, Pid), list_to_atom(lists:concat([Module, "_", pid_to_list(Pid)]))).

-define(DefaultFreq, 1000).

-define(dec282016ms, 1482924639084).
-record(nonce_track, {
        api_ref     :: term(),
        shift       :: 1..99
    }).
-type nonce_track() :: #nonce_track{}.

-record(noncer_state, {
        last_time_insec     :: 'undefined' | pos_integer(), % undefined at start_link
        heartbeat_freq      :: pos_integer(),
        heartbeat_tref      :: 'undefined' | reference(),   % undefined at start_link
        ets_table           :: 'undefined' | atom()         % undefined at start_link
    }).
-type noncer_state()        :: #noncer_state{}.

-type gen_nonce_msg()       :: {'gen_nonce', out_type(), api_ref(), api_time()}.

-type api_ref()             :: term().
-type api_time()            :: {'seconds' | 'milli_seconds', non_neg_integer()}.

-type start_prop()  :: #{
    'register' => register_as(),
    'heartbeat_freq' => non_neg_integer()
}.

-type register_as() :: 'undefined' |            % register option: do not register
                       {'local', atom()} |      % register option: locally
                       {'global',term()} |      % register option: globally)
                       {'via',module(),term()}. % register option: via module,name
