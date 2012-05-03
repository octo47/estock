S = fun(Name) -> application:load(Name), application:start(Name) end.
lists:map(S, [gproc, estockd, www]).
L=gen_server:call(estockd_generator, {start_rt_gen, "LOAD", 40, 0.7, 0, 1, 800}).
M=gen_server:call(estockd_generator, {start_rt_gen, "MAIL", 40, 0.7, 100, 1, 800}).
G=gen_server:call(estockd_generator, {start_rt_gen, "GOOG", 200, 0.2, 30, 60, 1000}).
E=gen_server:call(estockd_generator, {start_rt_gen, "ECHO", 24, 0.6, 100, 1, 800}).
estockd:list_aggs("ECHO", month, 0, 100).
