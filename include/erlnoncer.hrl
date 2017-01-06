
-type nonce()       :: 1..4294967294 | binary() | list().
-type return_nonce():: {'normal' | 'next_interval_nonce_must_flush', nonce()}.

-define(dec282016ms, 1482924639084).
-define(TAB, erlnoncer_tab).
-record(nonce_track, {
        api_ref     :: term(),
        shift       :: 1..99
    }).
-type nonce_track() :: #nonce_track{}.

-record(nonce_state, {
        last_time_insec :: pos_integer(),
        heartbeat_tref  :: reference()
    }).
-type nonce_state()  :: #nonce_state{}.

-type start_prop()  :: #{
    'register' => register_as(),
    'heartbeat_freq' => non_neg_integer()
}.

-type register_as() :: 'undefined' |            % register option: do not register
                       {'local', atom()} |      % register option: locally
                       {'global',term()} |      % register option: globally)
                       {'via',module(),term()}. % register option: via module,name
