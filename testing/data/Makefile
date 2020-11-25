# Author: Andreas Abel, 2020-11-25: pack JLex

.PHONY: jlex

jlex: JLex-1.2.6.jar

JLex-1.2.6.jar : JLex/*.class
	jar --create --file=$@ JLex/*.class
# NB: $^ instead of JLex/*.class does not work because .class names with $

JLex/Main.class : JLex/Main.java
	javac --source-path JLex $<

# EOF
