CC =		cc
CFLAGS =	-g
#CFLAGS =	-O
FC =		f77
FFLAGS =	-g
#FFLAGS =	-O

#BINDIR =	.
#BINDIR =	${HOME}/bin
BINDIR =	/home/antonio/bin

BINARIES =	mctest \
		ms2meca \
		shmaxreg \
		meca2perc

all:		$(BINARIES)

mctest:	mctest.o fgf.o cross.o fgpl.o
	$(FC) $(FFLAGS) -o $(BINDIR)/mctest mctest.o \
		fgf.o cross.o fgpl.o

ms2meca:	ms2meca.o fgi.o
	$(FC) $(FFLAGS) -o $(BINDIR)/ms2meca ms2meca.o fgi.o

shmaxreg:	shmaxreg.o fgf.o cross.o pai.o fgpaf.o
	$(FC) $(FFLAGS) -o $(BINDIR)/shmaxreg shmaxreg.o \
		fgf.o cross.o pai.o fgpaf.o

meca2perc:	meca2perc.o fgf.o cross.o pai.o fgpaf.o
	$(FC) $(FFLAGS) -o $(BINDIR)/meca2perc meca2perc.o \
		fgf.o cross.o pai.o fgpaf.o

clean:
	/bin/rm -f $(BINARIES) *.o

#-------------------------------------------------------------------------------
#       program dependencies
#------------------------------------------------------------------------------- 
.c.o:
		$(CC) -c $(CFLAGS) $<

.f.o:
		$(FC) -c $(FFLAGS) $<

