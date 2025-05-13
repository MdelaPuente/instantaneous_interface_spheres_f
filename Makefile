FC = gfortran
FCFLAGS = -fopenmp

PROJDIR := $(realpath $(CURDIR))
SRCDIR := $(PROJDIR)/src
BINDIR := $(PROJDIR)/bin
BUILDDIR := $(PROJDIR)/build

ifneq ($(BINDIR),)
 $(shell test -d $(BINDIR) || mkdir -p $(BINDIR))
endif
ifneq ($(BUILDDIR),)
 $(shell test -d $(BUILDDIR) || mkdir -p $(BUILDDIR))
endif

ifeq ($(FC), gfortran)
 FCFLAGS+= -J $(BUILDDIR)
endif

SRC := $(shell find $(SRCDIR) -type f -name *.f90)
OBJ := $(patsubst $(SRCDIR)/%,$(BUILDDIR)/%,$(SRC:.f90=.o))
MOD := $(patsubst $(SRCDIR)/%,$(BUILDDIR)/%,$(SRC:.f90=.mod))

$(BUILDDIR)/main.o: $(BUILDDIR)/input.o $(BUILDDIR)/fibonacci.o $(BUILDDIR)/sub.o $(BUILDDIR)/read_dcd.o $(BUILDDIR)/read_xyz.o

$(BUILDDIR)/%.o: $(SRCDIR)/%.f90
	$(FC) $(FCFLAGS) -o$@ -c $<
	@touch $@

is_sphere: $(OBJ)
	$(FC) $(FCFLAGS) -o $(BINDIR)/$@ $^

clean:
	rm -rf $(BUILDDIR)

realclean:
	rm -rf $(BUILDDIR) $(BINDIR)
