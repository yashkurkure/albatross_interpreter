#!/bin/bash

EXEC=_build/default/bin/albatross.exe
DUMMY=test
OUTPUT=output
TEST_DIR=tests
TIMEOUT=1s
DIFF_FILE=d

PASSING=0

RED='\033[41;37m'
GREEN='\033[42m'
RESET='\033[0m'

export ASAN_OPTIONS="detect_leaks=false"
export MallocNanoZone="0"

make clean && make

for T in $(ls $TEST_DIR | grep -E "$1" | sort)
do
    PASS=1
    for F in $(ls $TEST_DIR/$T | grep ".albatross$" | grep pass | sort)
    do
        echo -n -e "Running ${GREEN}positive test${RESET} $T/$F"
        echo -n $'\t'
        TESTFILE="$TEST_DIR/$T/$F"
        EXPECTED=$(sed 's/.albatross/.expected/g' <<<"$TESTFILE")
        cp $TESTFILE $DUMMY
        mv $EXEC.exe $EXEC &> /dev/null
        ./$EXEC < $DUMMY &> $OUTPUT
        RET=$?
        dos2unix $OUTPUT &> /dev/null
        dos2unix $EXPECTED &> /dev/null
        diff $EXPECTED $OUTPUT &> $DIFF_FILE
        DIFF=$?
        rm $DUMMY $OUTPUT

        if [ $RET -eq 0 ]
        then
            echo -e -n "${GREEN}RET OK${RESET}"
        else
            echo -e -n "${RED}RET FAIL${RESET}"
            cat $DIFF_FILE
        fi

        if [ $DIFF -eq 0 ]
        then
            echo -e " ${GREEN}DIFF OK${RESET}"
        else
            echo -e " ${RED}DIFF FAIL${RESET}"
            cat $DIFF_FILE
        fi

        if [ $RET -ne 0 ] || [ $DIFF -ne 0 ]
        then
            PASS=0
            #continue 2
        fi
    done

    for F in $(ls $TEST_DIR/$T |  grep ".albatross$" |grep fail | sort)
    do
        echo -n -e "Running ${RED}negative test${RESET} $T/$F"
        echo -n $'\t'
        TESTFILE="$TEST_DIR/$T/$F"
        cp $TESTFILE $DUMMY
        $(./$EXEC < $DUMMY &> $DIFF_FILE)
        RET=$?
        if [ $RET -eq 3 ]
        then
            echo -e "${GREEN}RET OK${RESET}"
        else
            echo -e "${RED}RET FAIL${RESET}"
            cat $DIFF_FILE
            #continue 2
            PASS=0
        fi
        rm $DUMMY
    done

    if [ $PASS -eq 0 ]
    then
            echo -e "${RED}                   TEST ${T} FAILING                  ${RESET}"
            echo -e "${RED}                   TEST ${T} FAILING                  ${RESET}"
            echo -e "${RED}                   TEST ${T} FAILING                  ${RESET}"
    else
            echo -e "${GREEN}                   TEST ${T} PASSING                ${RESET}"
            echo -e "${GREEN}                   TEST ${T} PASSING                ${RESET}"
            echo -e "${GREEN}                   TEST ${T} PASSING                ${RESET}"
    fi

    PASSING=$(($PASSING+$PASS))
done

echo $PASSING
