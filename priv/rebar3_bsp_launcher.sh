#!/bin/sh -eu

# Rebind stdio fds to new ones
exec 3<&0 # stdin dup'd to fd 3
exec 4<&1 # stdout dup'd to fd 4
exec 5<&2 # stderr dup'd to fd 5

# Redirect stdio fds to safety
exec 0</dev/null # no input on fd 0
exec 1>>/tmp/rebar3_bsp.log # log output
exec 2>&1 # redirect stderr output to stdout

# Inform the BSP plugin of our shenanigans
export REBAR3_BSP_IO_FDS="3 4 5"
# Invoke the command we were given
exec "$@"

