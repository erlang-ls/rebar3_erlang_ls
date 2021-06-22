#!/bin/sh -eu
export REBAR3_BSP_IO_FDS="3 4 5"
exec "$@" 3<&0 4<&1 5<&2 0</dev/null 1>>/tmp/rebar3_bsp.log 2>&1

