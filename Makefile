all:
	ocp-build

watch:
	watchman watch .
	watchman-make -p '**/*.ml' '**/*.mli' '**/*.ocp2' -t all
