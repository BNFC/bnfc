GHCMAKE = $(GHC) --make
GHCFLAGS = -fglasgow-exts
GHCOPTFLAGS = -O2 -iformats -iformats/haskell2 -iformats/haskell-gadt -iformats/xml -iformats/profile -iformats/java -iformats/java1.5 -iformats/cpp -iformats/c  -iformats/oz -iformats/ocaml -iformats/f-sharp -iformats/cpp_stl -iformats/c-sharp
P_TMP=$(PROGRAM)-bin$(EXEEXT)
P=$(PROGRAM)$(EXEEXT)

all: 
        $(GHCMAKE) Main.hs $(GHCOPTFLAGS) -o $(P_TMP)
        strip $(P_TMP)
        mv $(P_TMP) ../bin/$(P)
