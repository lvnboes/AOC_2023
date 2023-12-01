-module(util).
-export([timed/1]).

timed(F) ->
    Start = erlang:system_time(microsecond),
    Result = F(),
    End = erlang:system_time(microsecond),
    {{result, Result}, {microsecs, End-Start}}.