.PHONY: all run clean
all:
	dune build @all

run:
	dune exec albatross

test:
	./runtests.sh

test01:
	./runtests.sh "01"

test02:
	./runtests.sh "01|02"

test03:
	./runtests.sh "01|02|03"

test04:
	./runtests.sh "01|02|03|04"

test05:
	./runtests.sh "01|02|03|04|05"

test06:
	./runtests.sh "01|02|03|04|05|06"

test07:
	./runtests.sh "01|02|03|04|05|06|07"

test08:
	./runtests.sh "01|02|03|04|05|06|07|08"

test09:
	./runtests.sh "01|02|03|04|05|06|07|08|09"

test10:
	./runtests.sh "01|02|03|04|05|06|07|08|09|10"

clean:
	dune clean