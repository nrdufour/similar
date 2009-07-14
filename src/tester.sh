#!/bin/sh
erl -boot start_clean -noshell -pa . ./loggers ./process -s tester start -s init stop
