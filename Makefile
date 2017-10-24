all:
	@bapbuild -pkg bap-traces -pkg bap-ida -pkg bap-c -pkg bap-abi -I lib main.plugin -pp 'ppx-jane -dump-ast'

clean:
	@rm *.plugin
	@rm -r _build
	@rm *.native
