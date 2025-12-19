.SUFFIXES: .o .for

FC   = gfortran
LIBS = 
INC  = 
FFLAGS = -O0 -ffree-line-length-none -Wunused -fallow-argument-mismatch -std=legacy -mtune=native -fbacktrace -fcheck=bounds #-x f95-cpp-input -DWGET #-g
OBJS = smerge.o   #calutils.o
EXE  = SMERGE.EXE

.for.o:
		${FC} ${FFLAGS} -c ${INC} $<

$(EXE): ${OBJS}
		${FC} -o ${EXE} ${OBJS} ${LIBS} 

clean:
		rm -f *.o *.mod $(EXE)

