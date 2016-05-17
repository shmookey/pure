
pure:
	stack setup
	mkdir -p build/
	stack build --copy-bins --local-bin-path build/

install: pure
	./build/pure-install

clean:
	stack clean

