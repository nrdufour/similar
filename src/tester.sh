#!/bin/sh
erl -boot start_clean -noshell -pa . ./loggers ./process ./engine -s tester start -s init stop
