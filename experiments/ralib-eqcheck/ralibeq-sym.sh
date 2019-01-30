#!/bin/sh

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

export JVM_FLAGS="-Xmx1024m -ea -Djconstraints.extension.path=$DIR/lib -cp $DIR/lib/com.microsoft.z3.jar"

java $JVMFLAGS -jar $DIR/lib/ralib-0.1-SNAPSHOT-jar-with-dependencies.jar mc "symbolic=true;spec=$1;impl=$2"
