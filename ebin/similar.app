{application, similar,
    [{description, "Embryo Systems - Similar"},
     {vsn, "0.0.1"},
     {modules, [similar,
                similar_sup,
		similar_events,
		similar_file_logger,
		similar_process,
		similar_query,
		similar_server,
		similar_terminal_logger,
	        similar_utils
     ]},
     {registered, [similar_server, similar_sup]},
     {applications, [kernel, stdlib]},
     {mod, {orange, []}},
     {start_phases, []},
     {env, [ {conf, "./similar.conf"} ]}
]}.
