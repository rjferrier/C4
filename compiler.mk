# testing mode
# F90 =. /opt/intel/bin/compilervars.sh ia32 && ifort

F90 = ifort

# do not enable pointer checking - it causes seg faults in method stacks
# when the optional 'log' actual argument is excluded at any point
FFLAGS = -g -traceback -I$(PFUNITDIR)/mod -I$(PFUNITDIR)/include -I$(MKLROOT)/include/ia32 -I$(MKLROOT)/include -heap-arrays -O0 -fp-stack-check -check bounds -check uninit 

# LFLAGS = -L$(PFUNITDIR)/lib -L$(MKLROOT)/lib/ia32  $(MKLROOT)/lib/ia32/libmkl_blas95.a  $(MKLROOT)/lib/ia32/libmkl_lapack95.a $(MKLROOT)/lib/ia32/libmkl_intel.so -lmkl_intel -lmkl_intel_thread -lmkl_core -openmp -lpthread -lm  # -Wl,--verbose
LFLAGS = -L$(PFUNITDIR)/lib -L$(MKLROOT)/lib/ia32 -L$(MKLROOT)/lib/ia32  $(MKLROOT)/lib/ia32/libmkl_blas95.a  $(MKLROOT)/lib/ia32/libmkl_lapack95.a -lmkl_intel -lmkl_sequential -lmkl_core -lpthread -lm

## production mode
#FFLAGS = -O3 -fast
#LFLAGS= -L$(LIBDIR)

MOD=.mod
LIBTOOL=ar crv  # n.b. modified from arch.def owing to libtool bug

