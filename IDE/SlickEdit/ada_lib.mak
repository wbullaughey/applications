# SlickEdit generated file.  Do not edit this file except in designated areas.

# Make command to use for dependencies
MAKE=make
RM=rm
MKDIR=mkdir

# -----Begin user-editable area-----

# -----End user-editable area-----

#
# Configuration: Macosx
#
OUTDIR=Macosx
OUTFILE=$(OUTDIR)/ada_lib.a
CFG_INC=
CFG_LIB=
CFG_OBJ=
COMMON_OBJ=
OBJ=$(COMMON_OBJ) $(CFG_OBJ)
ALL_OBJ=

COMPILE=g++ -c    -g -o "$(OUTDIR)/$(*F).o" $(CFG_INC) "$<"
LINK=ar -rs  "$(OUTFILE)" $(OBJ)

# Build rules
all: $(OUTFILE)

$(OUTFILE): $(OUTDIR) $(OBJ)
	$(LINK)

$(OUTDIR):
	$(MKDIR) -p "$(OUTDIR)"

# Rebuild this project
rebuild: cleanall
	@$(MAKE) -f "$(strip $(MAKEFILE_LIST))" $(MAKEFLAGS) all

# Clean this project
clean:
	$(RM) -f $(OUTFILE)
	$(RM) -f $(OBJ)

# Clean this project and all dependencies
cleanall: clean

#
# include dependencies:
#
