#!/bin/bash

erl -pa ../spewf/ebin -pa ebin -config test -sname board -boot start_sasl -run crypto start -run spewf start -run mnesia start
