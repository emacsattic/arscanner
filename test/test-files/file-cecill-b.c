/*

Copyright 2007 Thierry Aime

This software is a computer program whose purpose is to test the arscanner
functionalities.

This software is governed by the CeCILL-B license under French law and
abiding by the rules of distribution of free software.  You can  use, 
modify and/ or redistribute the software under the terms of the CeCILL-B
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
knowledge of the CeCILL-B license and that you accept its terms.


*/
/* $XFree86: xc/programs/xbiff/xbiff.c,v 1.5 2005/03/25 02:22:59 dawes Exp $ */

#include <stdio.h>
#include <stdlib.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include "Mailbox.h"
#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/Tip.h>

const char *ProgramName;

static XrmOptionDescRec options[] = {
{ "-update", "*mailbox.update", XrmoptionSepArg, (caddr_t) NULL },
{ "-file",   "*mailbox.file", XrmoptionSepArg, (caddr_t) NULL },
{ "-volume", "*mailbox.volume", XrmoptionSepArg, (caddr_t) NULL },
{ "-shape",  "*mailbox.shapeWindow", XrmoptionNoArg, (caddr_t) "on" },
};

static Atom wm_delete_window;

static void
quit(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    if (event->type == ClientMessage &&
        event->xclient.data.l[0] != wm_delete_window) {
        XBell (XtDisplay(w), 0);
        return;
    }
    XCloseDisplay (XtDisplay(w));
    exit (0);
}

static XtActionsRec xbiff_actions[] = {
    { "quit", quit },
};

static void
Usage(void)
{
    static const char *help_message[] = {
"where options include:",
"    -display host:dpy              X server to contact",
"    -geometry geom                 size of mailbox",
"    -file file                     file to watch",
"    -update seconds                how often to check for mail",
"    -volume percentage             how loud to ring the bell",
"    -bg color                      background color",
"    -fg color                      foreground color",
"    -rv                            reverse video",
"    -shape                         shape the window",
NULL};
    const char **cpp;

    fprintf (stderr, "usage:  %s [-options ...]\n", ProgramName);
    for (cpp = help_message; *cpp; cpp++)
	fprintf (stderr, "%s\n", *cpp);
    fprintf (stderr, "\n");
    exit (1);
}


int
main(int argc, char **argv)
{
    XtAppContext xtcontext;
    Widget toplevel;

    ProgramName = argv[0];

    XtSetLanguageProc(NULL, (XtLanguageProc) NULL, NULL);

    toplevel = XtAppInitialize(&xtcontext, "XBiff", options, XtNumber (options),
			       &argc, argv, NULL, NULL, 0);
    if (argc != 1) Usage ();

    /*
     * This is a hack so that f.delete will do something useful in this
     * single-window application.
     */
    wm_delete_window = XInternAtom (XtDisplay(toplevel), "WM_DELETE_WINDOW",
                                    False);
    XtAppAddActions (xtcontext, xbiff_actions, XtNumber(xbiff_actions));
    XtOverrideTranslations(toplevel,
		   XtParseTranslationTable ("<Message>WM_PROTOCOLS: quit()"));

    (void) XtCreateManagedWidget ("mailbox", mailboxWidgetClass, toplevel,
				  NULL, 0);
    XtRealizeWidget (toplevel);
    (void) XSetWMProtocols (XtDisplay(toplevel), XtWindow(toplevel),
                            &wm_delete_window, 1);
    XtAppMainLoop (xtcontext);

    return 0;
}
