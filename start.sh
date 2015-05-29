#!/bin/bash

erl -pa ebin -pa deps/*/ebin -config config +K true -s eatrun start

exit $?

