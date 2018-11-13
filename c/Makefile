CC      =       cl /c 

OBJECTS =       gmachine.obj dump.obj sstack.obj vstack.obj heap.obj

default:        $(OBJECTS)
                link $(OBJECTS)
                
gmachine.obj:   gmachine.c out.c common.h sstack.h vstack.h dump.h heap.h
                $(CC) gmachine.c
                
dump.obj:       dump.c common.h dump.h 
                $(CC) dump.c

sstack.obj:     sstack.c common.h
                $(CC) sstack.c

vstack.obj:     vstack.c common.h
                $(CC) vstack.c
                
heap.obj:       heap.c common.h heap.h sstack.h
                $(CC) heap.c
                
clean:
                del *.obj
                del *.exe
                
                                
