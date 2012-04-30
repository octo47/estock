gen_server:call(estockd_generator, {start_rt_gen, "MAIL", 40, 0.7, 100, 1, 800}).
gen_server:call(estockd_generator, {start_rt_gen, "GOOG", 200, 0.2, 30, 60, 1000}).
gen_server:call(estockd_generator, {start_rt_gen, "YNDX", 24, 0.6, 100, 1, 800}).
ets:match(estockd_table, { '$1', '$2' }).
estockd:list_aggs("YNDX", month, 1325376000000, 1325376000000*2, 100).
