all: compile

compile:
	@mkdir -p ebin
	@erl -make

clean:
	rm -fv ebin/*.beam

shell:
	erl -sname 'frk' -pz ebin 
