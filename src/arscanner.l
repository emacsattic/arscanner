/************************************************************************

Copyright 2006-2007 Thierry Aime

This is a lexer which extracts from a source file copyright holders and 
licence type if any.

This software is governed by the CeCILL license under French law and
abiding by the rules of distribution of free software.  You can  use, 
modify and/ or redistribute the software under the terms of the CeCILL
license as circulated by CEA, CNRS and INRIA at the following URL
"http://www.cecill.info". 

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability. 

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or 
data to be ensured and,  more generally, to use and operate it in the 
same conditions as regards security. 

The fact that you are presently reading this means that you have had
knowledge of the CeCILL license and that you accept its terms.

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
