all: lib/util/NamedProtocol.class

%.class: %.java
	javac -classpath cp:`rune --src 'print(interp.getProps()["e.home"])'`/e.jar $^