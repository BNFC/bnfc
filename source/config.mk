# Set name of binary
PROGRAM=bnfc
#

PACKAGE_VERSION = 2.4
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
INSTALL = /opt/local/bin/ginstall -c
TAR = tar
GHC = /usr/bin/ghc
