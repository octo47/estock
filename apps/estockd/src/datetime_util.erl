-module(datetime_util).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

epoch() ->
    now_to_seconds(now()).   
    
epoch_millis() ->
    now_to_millis(now()).   
    
now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.   
    
now_to_millis({Mega, Sec, Micro}) ->
    (Mega * 1000000) + (Sec * 1000) + trunc(Micro/1000).
    
millis_to_now(Millis) ->
    Seconds = trunc(Millis/1000),
    {Seconds div 1000000, Seconds rem 1000000, Millis rem 1000}.

millis_to_datetime(Millis) ->
    now_to_datetime(millis_to_now(Millis)).

now_to_datetime(Now) ->
    calendar:now_to_universal_time(Now).

now_to_date(Now) ->
    {Date, _} = calendar:now_to_universal_time(Now),
    Date.

epoch_gregorian_seconds() ->
    calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}).       
        
now_to_gregorian_seconds() ->
    epoch_to_gregorian_seconds(now()).       
        
epoch_to_gregorian_seconds({Mega, Sec, Micro}) ->
    epoch_to_gregorian_seconds(now_to_seconds({Mega, Sec, Micro}));
epoch_to_gregorian_seconds(Now) ->
    EpochSecs = epoch_gregorian_seconds(), 
    Now + EpochSecs.       

gregorian_seconds_to_epoch(Secs) ->
    EpochSecs = epoch_gregorian_seconds(), 
    Secs - EpochSecs.

date_to_epoch(Date) ->
    datetime_to_epoch({Date, {0,0,0} }).

datetime_to_epoch({Date, Time}) ->
    gregorian_seconds_to_epoch(
        datetime_to_seconds({Date, Time})).

datetime_to_seconds({Date, Time}) ->
    calendar:datetime_to_gregorian_seconds({Date, Time}).

datetime_to_millis({Date, Time}) ->
    datetime_to_epoch({Date, Time}) * 1000.

date_to_millis({Date, _}) ->
    date_to_epoch(Date) * 1000.

-spec gregorian_days_of_w01_1(integer()) -> integer().
gregorian_days_of_w01_1(Year) ->
    D0101 = calendar:date_to_gregorian_days(Year, 1, 1),
    DOW = calendar:day_of_the_week(Year, 1, 1),
    if DOW =< 4 ->
	    D0101 - DOW + 1;
       true ->
	    D0101 + 7 - DOW + 1
    end.

week_to_date(Year, Weeknum) ->
    W01 = gregorian_days_of_w01_1(Year),
    calendar:gregorian_days_to_date(W01 + (Weeknum - 1) * 7).

truncate_now_millis(Scope, Now) ->
    truncate_datetime_millis(Scope, now_to_datetime(Now)).

truncate_datetime_millis(year,  {{Year, _, _}, {_, _, _}}) ->
    datetime_to_millis({{Year, 1, 1}, {0, 0, 0}});
truncate_datetime_millis(month, {{Year, Month, _}, {_, _, _}}) ->
    datetime_to_millis({{Year, Month, 1}, {0, 0, 0}});
truncate_datetime_millis(week, {{Year, _, _} = Date, {_, _, _}}) ->
    {Year, WeekNum} = calendar:iso_week_number(Date),
    datetime_to_millis({week_to_date(Year, WeekNum), {0, 0, 0}});
truncate_datetime_millis(day, { Date, {_, _, _}}) ->
    datetime_to_millis({Date, {0, 0, 0}});
truncate_datetime_millis(hour, { Date, {HH, _, _}}) ->
    datetime_to_millis({Date, {HH, 0, 0}});
truncate_datetime_millis(minute, { Date, {HH, MM, _}}) ->
    datetime_to_millis({Date, {HH, MM, 0}}).

%% ===================================================================
%% Unit Tests
%% ===================================================================

-ifdef(EUNIT).

millis_test() ->
    ?assert({{2012, 4, 28},{8, 18, 14}} =:= millis_to_datetime(1335601094057)),
    ?assert(1335601094000 =:= datetime_to_millis({{2012, 4, 28},{8, 18, 14}})),
    ?assert({{1960, 4, 28},{8, 18, 14}} =:= millis_to_datetime(-305394106000)),

    Date = {{2011, 03, 01}, {23, 44, 22}},
    Reconv = now_to_datetime(millis_to_now(datetime_to_millis(Date))),
    ?assert( Reconv =:= Date).

truncate_test() ->
    Date = {{2011, 03, 01}, {23, 44, 22}},
    T1 = millis_to_datetime(truncate_datetime_millis(year, Date)),
    ?assert( T1 =:= {{2011, 1, 1}, {0, 0, 0}}),
    T2 = millis_to_datetime(truncate_datetime_millis(month, Date)),
    ?assert( T2 =:= {{2011, 3, 1}, {0, 0, 0}}),
    T3 = millis_to_datetime(truncate_datetime_millis(week, Date)),
    ?assert( T3 =:= {{2011, 2, 28}, {0, 0, 0}}),
    T4 = millis_to_datetime(truncate_datetime_millis(day, Date)),
    ?assert( T4 =:= {{2011, 3, 1}, {0, 0, 0}}),
    T5 = millis_to_datetime(truncate_datetime_millis(hour, Date)),
    ?assert( T5 =:= {{2011, 3, 1}, {23, 0, 0}}),
    T6 = millis_to_datetime(truncate_datetime_millis(minute, Date)),
    ?assert( T6 =:= {{2011, 3, 1}, {23, 44, 0}}).

-endif.

