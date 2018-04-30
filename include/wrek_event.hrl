-record(wrek_event, {
          timestamp = erlang:monotonic_time() :: integer(),
          id        = undefined :: pos_integer() | {pos_integer(), pos_integer()},
          type      = undefined :: atom() | {atom(), atom()},
          msg       = undefined :: any()
         }).
-type wrek_event() :: #wrek_event{}.
-export_type([wrek_event/0]).
