# Copyright 2006-2007 Thierry Aime

# Makefile for arscanner 

# This software is governed by the CeCILL license under French law and
# abiding by the rules of distribution of free software.  You can  use, 
# modify and/ or redistribute the software under the terms of the CeCILL
# license as circulated by CEA, CNRS and INRIA at the following URL
# "http://www.cecill.info". 

# As a counterpart to the access to the source code and  rights to copy,
# modify and redistribute granted by the license, users are provided only
# with a limited warranty  and the software's author,  the holder of the
# economic rights,  and the successive licensors  have only  limited
# liability. 

# In this respect, the user's attention is drawn to the risks associated
# with loading,  using,  modifying and/or developing or reproducing the
# software by the user in light of its specific status of free software,
# that may mean  that it is complicated to manipulate,  and  that  also
# therefore means  that it is reserved for developers  and  experienced
# professionals having in-depth computer knowledge. Users are therefore
# encouraged to load and test the software's suitability as regards their
# requirements in conditions enabling the security of their systems and/or 
# data to be ensured and,  more generally, to use and operate it in the 
# same conditions as regards security. 

# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.


# Where local softwares go
prefix=/usr/local

# Where local lisp files go
lispdir = $(prefix)/share/emacs/site-lisp

# Where local bin files go
bindir = $(prefix)/bin

# How to create directories
MKDIR = mkdir -p

# How to copy a file.
CP = cp

# How to delete a file.
RM = rm

arscanner: arscanner-flex.c
	gcc arscanner-flex.c -lfl -o arscanner -ansi

arscanner-flex.c: arscanner.l 
	flex -i -o arscanner-flex.c arscanner.l

install: arscanner arscanner.el
	if [ ! -d $(lispdir) ]; then $(MKDIR) $(lispdir); else true; fi ;
	$(CP) arscanner.el $(lispdir)
	$(CP) arscanner  $(bindir)

uninstall:
	if [ -f $(lispdir)/arscanner.el ]; then $(RM) $(lispdir)/arscanner.el; 
	if [ -f $(lispdir)/arscanner ]; then $(RM) $(lispdir)/arscanner; 

clean:
	rm -f arscanner-flex.c arscanner
	rm -f *~ 
