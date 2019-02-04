#!/bin/bash

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

export JVMFLAGS="-Xmx1024m -ea -Djconstraints.extension.path=$DIR/lib"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/home/cav/z3/build"

export TIMEFORMAT="%U"
time java $JVMFLAGS -jar $DIR/lib/ralib-0.1-SNAPSHOT-jar-with-dependencies.jar mc "symbolic=false;spec=$1;impl=$2" > /dev/null 2>&1