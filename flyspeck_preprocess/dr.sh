#!/bin/bash
exec dReal --interpolant $1 > $2/`basename $1`.itp
