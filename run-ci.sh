#!/bin/sh -e

# This script is intended invoke by GithubAction `CI` job
# Keep as *~ to unexpectedly be executed by developer.
test -f env.mk && mv env.mk env.mk~

# Local elpa directory.
echo "ELPA-DIR = ./elpa" >> env.mk

make ci
