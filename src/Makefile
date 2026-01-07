FC   = gfortran
LIBS = 
INC  = 
FFLAGS = -O0 -ffree-line-length-none -Wunused -fallow-argument-mismatch -std=legacy -mtune=native -fbacktrace -fcheck=bounds
OBJS = smerge.o
EXE  = SMERGE.EXE

%.o: %.for
		${FC} ${FFLAGS} -c ${INC} $<

$(EXE): ${OBJS}
		${FC} -o ${EXE} ${OBJS} ${LIBS} 

clean:
		rm -f *.o *.mod $(EXE)

