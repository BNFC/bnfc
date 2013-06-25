#!/bin/sh

set -e
set -x
set -u

CABAL=cabal-dev

if [ -f "/usr/share/java/JLex.jar" ]
then
  CLASSPATH+=":/usr/share/java/JLex.jar"
elif [ -f "/usr/share/java/jlex.jar" ]
then
  CLASSPATH+=":/usr/share/java/jlex.jar"
elif [ -f "/usr/share/java/jflex.jar" ]
then
  CLASSPATH+=":/usr/share/java/jflex.jar"
else
  echo "Can't find the JLex library, aborting"
  exit 1
fi

if [ -f "/usr/share/java/cup.jar" ]
then
  CLASSPATH+=":/usr/share/java/cup.jar"
elif [ -f "/usr/share/java/java_cup.jar" ]
then
  CLASSPATH+=":/usr/share/java/java_cup.jar"
else
  echo "Can't find the Cup library, aborting"
  exit 1
fi

echo "Exporting the new classpath: $CLASSPATH"
export CLASSPATH

# This is a bit useless, but since $@ is an array, it gives strange results to
# put it directly in the command.
flags="$@"

# To install the runtime.
${CABAL} install 

${CABAL} install --only-dependencies --enable-tests
if [ -z "$flags" ]; then
  ${CABAL} configure --enable-tests
else
  ${CABAL} configure --enable-tests -f "$flags"
fi
${CABAL} build
${CABAL} test
