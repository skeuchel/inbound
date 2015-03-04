CABAL-CONFIGURE-FLAGS      := --user
CABAL-BUILD-FLAGS          :=

all : configure build

configure :
	cabal configure $(CABAL-CONFIGURE-FLAGS)

build : src/* src/Ag/AG.hs src/InBound/AG.hs src/InBound/CopyRules.hs
	cabal build $(CABAL-BUILD-FLAGS)

clean :
	cabal clean
	rm -f src/Ag/AG.hs
	rm -f src/Ag/Syntax.hs
	rm -f src/InBound/AG.hs
	rm -f src/InBound/CopyRules.hs
	rm -f src/InBound/Syntax.hs


src/Ag/Syntax.hs: src/Ag/Syntax.ag
	uuagc -Hd --self $<

src/InBound/Syntax.hs: src/InBound/Syntax.ag
	uuagc -Hd --self $<

%.hs : %.ag
	uuagc -Hsfcw --self $<

src/Ag/AG.hs : src/Ag/AG.ag \
	src/Ag/Syntax.hs \
	src/Ag/Syntax.ag \
	src/Ag/Environment.ag \
	src/Ag/Pretty.ag

src/InBound/AG.hs : \
	src/InBound/AG.ag \
	src/InBound/Syntax.hs \
	src/InBound/Syntax.ag \
	src/InBound/Environment.ag \
	src/InBound/Elaboration/Term.ag \
	src/InBound/Elaboration/Context.ag \
	src/InBound/Elaboration/FreeVar.ag \
	src/InBound/Elaboration/RenameContext.ag \
	src/InBound/Elaboration/RenameMap.ag \
	src/InBound/Elaboration/RenameSynthesis.ag \
	src/InBound/Elaboration/SubstMap.ag \
	src/InBound/Elaboration/SubstSynthesis.ag

src/InBound/CopyRules.hs : \
	src/InBound/CopyRules.ag \
	src/InBound/Syntax.hs \
	src/InBound/Syntax.ag \
	src/InBound/Environment.ag

main : src/Main.hs
	cabal configure
	cabal build

.PHONY : haskell
