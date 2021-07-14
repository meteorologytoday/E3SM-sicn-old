.SUFFIXES: .F .o

OBJS = 	sicn_mod.o

all: $(OBJS)

sicn_mod.o:

clean:
	$(RM) *.o *.i *.mod *.f90

.F.o:
	$(RM) $@ $*.mod
ifeq "$(GEN_F90)" "true"
	$(CPP) $(CPPFLAGS) $(CPPINCLUDES) $< > $*.f90

	$(FC) $(FFLAGS) -c $*.f90 $(FCINCLUDES) -I../../framework -I../../operators -I../../external/esmf_time_f90 -I../column
else
	$(FC) $(CPPFLAGS) $(FFLAGS) -c $*.F $(CPPINCLUDES) $(FCINCLUDES) -I../../framework -I../../operators -I../../external/esmf_time_f90 -I../column
endif
