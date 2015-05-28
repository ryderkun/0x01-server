#!/bin/bash

erl -pa ebin -pa deps/*/ebin -s eatrun start

exit $?

