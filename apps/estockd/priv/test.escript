gen_server:call(estockd_generator, {start_rt_gen, "MAIL", 40, 0.7, 100, 1, 800}).
gen_server:call(estockd_generator, {start_rt_gen, "GOOG", 200, 0.2, 30, 60, 1000}).
gen_server:call(estockd_generator, {start_rt_gen, "ECHO", 24, 0.6, 100, 1, 800}).
estockd:list_aggs("ECHO", month, 0, 100).
