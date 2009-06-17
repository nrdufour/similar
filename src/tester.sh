#!/bin/sh
erl -boot start_clean -noshell -pa . -s tester start -s init stop
