#!/bin/sh
erl -boot start_clean -noshell -pa . ./loggers -s tester start -s init stop
