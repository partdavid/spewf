#!/bin/bash

erl -pa ../spewf/ebin -pa ebin -config test -boot start_sasl -run crypto start -run spewf start
