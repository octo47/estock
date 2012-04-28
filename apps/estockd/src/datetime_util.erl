-module(datetime_util).
-compile(export_all).

epoch() ->
    now_to_seconds(now()).   
    
epoch_millis() ->
    now_to_millis(now()).   
    
now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.   
    
now_to_millis({Mega, Sec, Micro}) ->
    (Mega * 1000000) + (Sec * 1000) + Micro/1000.
    
millis_to_now(Millis) ->
    {Millis div 1000000, Millis rem 1000000,0}.

millis_to_datetime(Millis) ->
    now_to_datetime(millis_to_now(Millis)).

now_to_datetime(Now) ->
    calendar:now_to_universal_time(Now).

datetime_to_seconds({Date, Time}) ->
    calendar:datetime_to_gregorian_seconds({Date, Time}).

datetime_to_millis({Date, Time}) ->
    datetime_to_seconds({Date, Time}) * 1000.

date_to_millis({Date, _}) ->
    datetime_to_seconds({Date, {0,0,0}}) * 1000.

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
