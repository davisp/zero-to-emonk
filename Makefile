
all:
	./rebar compile

clean:
	./rebar clean
	rm -f test/*.beam
