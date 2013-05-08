#!/bin/sh

if [ -f "/usr/share/java/JLex.jar" ]
then
  CLASSPATH+=":/usr/share/java/JLex.jar"
elif [ -f "/usr/share/java/jlex.jar" ]
then
  CLASSPATH+=":/usr/share/java/jlex.jar"
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
