#!/bin/sh

exec erl -pa $PWD/deps/*/ebin $PWD/apps/*/ebin -config app.config -boot start_sasl -s reloader -s rest_api -detached
