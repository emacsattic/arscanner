/************************************************************************

Copyright 2006-2007 Thierry Aime

This is a lexer which extracts from a source file copyright holders and 
licence type if any.

This is a scanner which explore a directory for extracting copyright 
and licence note from sources files. Result are displayed in an emacs 
buffer; an emacs mode help to broswe 

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation, 
Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

**************************************************************************/




/***********************************************************************
A copyright block start with the word "Copyright" or "©" or "(C)". 
Inside a Copyright block we collect all lines which start with a years 
(four digits). The Copyright block stops after two lines without years 
indication. The licence type is deduce from few words which match 
exactly a specific piece of the original text licence.
The lexer stops automaticly after 20 lines.
************************************************************************/


%{

	int     count=0;             /* for manage copyright block */
	int     nb_line=0;           /* after 20 lines, scaning stop */
  char*   licence="UNKNOWN";   /* default value ! */

	struct copyright_holder      /* for list of copyright holders */
	{
		char   name[70];
		struct copyright_holder* next;
	} *holders, *holder;


%}

%s HOLDERS

%%


"Copyright"|"©"|"(C)" { 
	/* A copyright block start */
	count=2;
	BEGIN(HOLDERS);
}

<HOLDERS>([[:digit:]]{4}).*$ { 
	/* construction of the list of the copyright holders */	
	if (holders==NULL)
		holders=holder=malloc(sizeof(struct copyright_holder));
	else {
		holder->next=malloc(sizeof(struct copyright_holder));
		holder=holder->next;
	};
		
	snprintf(holder->name, 69, "%s", yytext);
	holder->next=NULL;
	count=2;
}


"GNU General" { /* GNU General Public License */ 
	licence = "GPL";  yyterminate(); }

"GNU Lesser" { /* GNU Lesser General Public License */ 
	licence = "LGPL"; yyterminate (); }

"Redistribution and use in source and binary forms" { /* BSD licence */
  licence = "BSD"; yyterminate (); }

"Permission is hereby granted, free of charge," { /* MIT licence */
  licence = "MIT"; yyterminate (); }

"Common Development and Distribution License"|"CDDL" { /* CDDL licence */
  licence = "CDDL"; }

"Common Public License" { /* CPL Licence */
  licence = "CPL"; yyterminate (); }

"Eclipse Public License" { /* EPL licence */
  licence = "EPL"; yyterminate (); }

"Affero General Public License" { 
	licence = "AGPL"; yyterminate (); }

"Apache License" { 
	licence = "APACHE"; yyterminate (); }

"Mozilla Public License" {
	licence = "MPL"; yyterminate (); }

"CeCILL" { /* CECILL license */
  licence = "CECILL"; yyterminate (); }

"CeCILL-B" { /* CECILL-B license */
	licence = "CeCILL-B"; yyterminate (); }

"CeCILL-C" { /* CECILL-C license */
	licence = "CeCILL-C"; yyterminate (); }

\n { /* Count lines and escape if upper 20 */
	nb_line++;   
	if (nb_line>20)
    yyterminate();
	
	if (count)
		count--;
	else
		BEGIN(INITIAL);	
}

.  /* eat up any other character */


%%

main(int argc, char **argv )
{
	++argv, --argc;  /* skip over program name */
	if ( argc > 0 )
		yyin = fopen( argv[0], "r" );
			else
				yyin = stdin;

	holders=NULL;

	yylex();

	printf("  %-10s-> %s\n", licence, argv[0]);
	
	while (holders) {
		holder=holders;
		printf("    (C) %s\n", holder->name);
		holders=holder->next;
		free(holder);
	};
}
