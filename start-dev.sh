#!/bin/sh
set -x
cd `dirname $0`
exec erl -sname queue_workers -config sys.config -pa _build/default/lib/*/ebin -boot start_sasl -hidden