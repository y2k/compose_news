.PHONY: release build

release:
	dune clean && dune test && dune build bin --profile=release

build:
	dune build bin --profile=release
