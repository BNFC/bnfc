# Set name of binary
PROGRAM=extract
#

PACKAGE_VERSION = 1.0
prefix = /usr/local
exec_prefix = ${prefix}
bindir = ${exec_prefix}/bin
libdir = ${exec_prefix}/lib
datadir = ${datarootdir}
datarootdir = ${prefix}/share
host = @host@
build = @build@
GHCFLAGS = 
CPPFLAGS = 
LDFLAGS = 
EXEEXT = 
INSTALL = /usr/bin/install -c
TAR = tar
GHC = /usr/bin/ghc
