#!/bin/sh
erl -noshell -pa . -s tester start -s init stop
