
pure: .stack-work
	mkdir -p build/
	stack build --copy-bins --local-bin-path build/

.stack-work:
	stack setup

install:
	./build/pure-install

clean:
	stack clean

