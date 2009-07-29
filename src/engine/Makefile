# leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

# Here's a list of the erlang modules you want compiling
# If the modules don't fit onto one line add a \ character 
# to the end of the line and continue on the next line
MODS = similar_server_impl similar_server similar_process similar events

# The first target in any makefile is the default target.
# If you just type "make" then "make all" is assumed (because
#   "all" is the first target in this makefile)

all: compile

compile: ${MODS:%=%.beam}
	
# remove all the code

clean:	
	rm -rf *.beam erl_crash.dump
