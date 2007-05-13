# Copyright 2006-2007 Thierry Aime

# Makefile for arscanner 

# This is a scanner which explore a directory for extracting copyright 
# and licence note from sources files. Result are displayed in an emacs 
# buffer; an emacs mode help to broswe 

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation, 
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA


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
