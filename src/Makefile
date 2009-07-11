# leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean 

# Here's a list of the erlang modules you want compiling
# If the modules don't fit onto one line add a \ character 
# to the end of the line and continue on the next line
MODS = utils similar_server_impl similar_server similar sm_debug dummy_process tester

# The first target in any makefile is the default target.
# If you just type "make" then "make all" is assumed (because
#   "all" is the first target in this makefile)

all: compile

compile: ${MODS:%=%.beam} subdirs
	
tester: compile
	${ERL} -pa . ./loggers -noshell -s tester start -s init stop

# the subdirs target compiles any code in 
# sub-directories

subdirs:
	cd loggers; make

# remove all the code

clean:	
	rm -rf *.beam erl_crash.dump
	cd loggers; make clean

