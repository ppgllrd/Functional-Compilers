.SUFFIXES              :         
.SUFFIXES              : .hs .lhs .o


GHC      =               ghc -O 
OBJECTS  =               LibBalancedTree.o LibAssoc.o LibParser.o LibPretty.o LibMonadSE.o LibSIO.o LibStack.o \
                         CoreLex.o GlobalDefs.o \
                         GMStats.o GMCode.o GMHeap.o GMSStack.o GMVStack.o GMGlobals.o GMDump.o GMState.o \
                         GMachine.o \
                         CoreLang.o  CoreParser.o  \
                         Compiler.o ConstrTags.o \
                         Main.o 


default:  $(OBJECTS)
	$(GHC) -o mark1  $(OBJECTS)

Compiler.o :             Compiler.hs LibAssoc.lhs LibMonadSE.hs GlobalDefs.hs CoreLang.hs CoreLex.hs GMCode.hs \
                         GMVStack.hs GMSStack.hs GMHeap.hs GMGlobals.hs GMStats.hs GMState.hs GMDump.hs ConstrTags.o
	$(GHC) -c Compiler.hs

CoreLang.o:             CoreLang.hs GlobalDefs.hs CoreLex.hs LibPretty.hs
	$(GHC) -c CoreLang.hs

CoreLex.o:              CoreLex.hs
	$(GHC) -c CoreLex.hs

CoreParser.o:           CoreParser.hs CoreLex.hs CoreLang.hs LibParser.hs
	$(GHC) -c -H20M CoreParser.hs

GlobalDefs.o:           GlobalDefs.hs
	$(GHC) -c GlobalDefs.hs

GMachine.o:             GMachine.hs LibSIO.hs LibPretty.hs LibAssoc.lhs GlobalDefs.hs GMCode.hs GMSStack.hs \
                        GMVStack.hs GMHeap.hs GMGlobals.hs GMStats.hs GMState.hs GMDump.hs ConstrTags.hs CoreLex.hs
	$(GHC) -c GMachine.hs

GMCode.o:               GMCode.hs LibPretty.hs GlobalDefs.hs
	$(GHC) -c GMCode.hs

GMGlobals.o:            GMGlobals.hs LibAssoc.lhs  GlobalDefs.hs GMHeap.hs
	$(GHC) -c GMGlobals.hs

GMHeap.o:               GMHeap.hs LibAssoc.lhs GMCode.hs
	$(GHC) -c GMHeap.hs

GMSStack.o:              GMSStack.hs GMHeap.hs LibStack.hs
	$(GHC) -c GMSStack.hs

GMVStack.o:              GMVStack.hs LibStack.hs
	$(GHC) -c GMVStack.hs

GMDump.o:               GMDump.hs GMSStack.hs GMCode.hs
	$(GHC) -c GMDump.hs

GMState.o:              GMState.hs GMGlobals.hs GMSStack.hs GMVStack.hs GMCode.hs GMStats.hs GMHeap.hs GMDump.hs
	$(GHC) -c GMState.hs

GMStats.o:              GMStats.hs
	$(GHC) -c GMStats.hs

LibAssoc.o:             LibAssoc.lhs LibBalancedTree.lhs
	$(GHC) -c LibAssoc.lhs

LibBalancedTree.o:      LibBalancedTree.lhs
	$(GHC) -c LibBalancedTree.lhs

LibMonadSE.o:           LibMonadSE.hs
	$(GHC) -c LibMonadSE.hs

LibParser.o:            LibParser.hs
	$(GHC) -c LibParser.hs

LibPretty.o:            LibPretty.hs
	$(GHC) -c LibPretty.hs

LibStack.o:             LibStack.hs
	$(GHC) -c LibStack.hs

LibSet.o:               LibSet.lhs LibBalancedTree.lhs
	$(GHC) -c LibSet.lhs

LibSIO.o:               LibSIO.hs
	$(GHC) -c LibSIO.hs

ConstrTags.o:           ConstrTags.hs
	$(GHC) -c ConstrTags.hs

Main.o:                 Main.hs CoreParser.hs LibSIO.hs Compiler.hs GMachine.hs
	$(GHC) -c Main.hs














