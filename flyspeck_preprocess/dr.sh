#!/bin/bash

ulimit -m 1048531

exec dReal --interpolant $1 > $2/`basename $1`.itp
