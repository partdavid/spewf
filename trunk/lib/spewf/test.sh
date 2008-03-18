#!/bin/bash

erl -pa ebin -config test -boot start_sasl -run crypto start -run spewf start
