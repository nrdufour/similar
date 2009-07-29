#!/bin/sh
erl -boot start_clean -noshell -pa ./ebin -s tester start -s init stop
