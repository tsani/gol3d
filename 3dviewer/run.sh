#!/usr/bin/zsh

java -classpath .:jar/lwjgl.jar:jar/lwjgl_util.jar:../3d -Djava.library.path=native/linux Game $@
