&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: cec\d-panels.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF TEMP-TABLE tt-array NO-UNDO
    FIELD tt-dec AS DEC DECIMALS 6
    FIELD tt-type AS CHAR.

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-16ths AS LOG NO-UNDO.
DEF INPUT PARAM ip-code AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-array.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR op-total LIKE eb.k-wid-array2 NO-UNDO.
DEF VAR op-type LIKE eb.k-wid-scr-type2 NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.

{sys/inc/f16to32.i}

i = 0.
FOR EACH tt-array:
   i = i + 1.
   IF i LE EXTENT(op-total) THEN op-total[i] = tt-dec.
   IF i LE EXTENT(op-type)  THEN op-type[i]  = tt-type.
END.

IF v-cecscrn-dec THEN
DO:
   DEF TEMP-TABLE tt-64-dec NO-UNDO
       FIELD DEC AS DEC DECIMALS 6.

   DO v-count = 0 TO 63:
       CREATE tt-64-dec.
       tt-64-dec.DEC = v-count / 64.0.
       RELEASE tt-64-dec.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS val-1 type-1 val-2 type-2 val-3 type-3 val-4 ~
type-4 val-5 type-5 val-6 type-6 val-7 type-7 val-8 type-8 val-9 type-9 ~
val-10 type-10 val-11 type-11 val-12 type-12 val-13 type-13 val-14 type-14 ~
val-15 type-15 val-16 type-16 val-17 type-17 val-18 type-18 val-19 type-19 ~
val-20 type-20 val-21 type-21 val-22 type-22 val-23 type-23 val-24 type-24 ~
val-25 type-25 val-26 type-26 val-27 type-27 val-28 type-28 val-29 type-29 ~
val-30 type-30 btn-done btn-cancel RECT-16 
&Scoped-Define DISPLAYED-OBJECTS val-1 type-1 dscr-1 val-2 type-2 dscr-2 ~
val-3 type-3 dscr-3 val-4 type-4 dscr-4 val-5 type-5 dscr-5 val-6 type-6 ~
dscr-6 val-7 type-7 dscr-7 val-8 type-8 dscr-8 val-9 type-9 dscr-9 val-10 ~
type-10 dscr-10 val-11 type-11 dscr-11 val-12 type-12 dscr-12 val-13 ~
type-13 dscr-13 val-14 type-14 dscr-14 val-15 type-15 dscr-15 val-16 ~
type-16 dscr-16 val-17 type-17 dscr-17 val-18 type-18 dscr-18 val-19 ~
type-19 dscr-19 val-20 type-20 dscr-20 val-21 type-21 dscr-21 val-22 ~
type-22 dscr-22 val-23 type-23 dscr-23 val-24 type-24 dscr-24 val-25 ~
type-25 dscr-25 val-26 type-26 dscr-26 val-27 type-27 dscr-27 val-28 ~
type-28 dscr-28 val-29 type-29 dscr-29 val-30 type-30 dscr-30 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-done 
     LABEL "&Save" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE dscr-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-11 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-12 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-13 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-14 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-15 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-16 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-17 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-18 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-19 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-20 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-21 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-22 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-23 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-24 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-25 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-26 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-27 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-28 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-29 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-30 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE dscr-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE type-1 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-10 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-11 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-12 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-13 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-14 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-15 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-16 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-17 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-18 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-19 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-2 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-20 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-21 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-22 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-23 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-24 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-25 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-26 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-27 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-28 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-29 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-3 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-30 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-4 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-5 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-6 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-7 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-8 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE type-9 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .76 NO-UNDO.

DEFINE VARIABLE val-1 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "1" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-10 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "10" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-11 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "11" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-12 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "12" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-13 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "13" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-14 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "14" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-15 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "15" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-16 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "16" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-17 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "17" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-18 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "18" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-19 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "19" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-2 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "2" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-20 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "20" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-21 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "21" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-22 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "22" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-23 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "23" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-24 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "24" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-25 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "25" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-26 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "26" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-27 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "27" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-28 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "28" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-29 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "29" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-3 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "3" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-30 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "30" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-4 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "4" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-5 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "5" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-6 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "6" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-7 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "7" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-8 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "8" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE VARIABLE val-9 AS DECIMAL FORMAT "->>,>>9.99<<<" INITIAL 0 
     LABEL "9" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .76.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 22.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     val-1 AT ROW 1.95 COL 6 COLON-ALIGNED
     type-1 AT ROW 1.95 COL 24 COLON-ALIGNED NO-LABEL
     dscr-1 AT ROW 1.95 COL 33 COLON-ALIGNED NO-LABEL
     val-2 AT ROW 2.67 COL 6 COLON-ALIGNED
     type-2 AT ROW 2.67 COL 24 COLON-ALIGNED NO-LABEL
     dscr-2 AT ROW 2.67 COL 33 COLON-ALIGNED NO-LABEL
     val-3 AT ROW 3.38 COL 6 COLON-ALIGNED
     type-3 AT ROW 3.38 COL 24 COLON-ALIGNED NO-LABEL
     dscr-3 AT ROW 3.38 COL 33 COLON-ALIGNED NO-LABEL
     val-4 AT ROW 4.1 COL 6 COLON-ALIGNED
     type-4 AT ROW 4.1 COL 24 COLON-ALIGNED NO-LABEL
     dscr-4 AT ROW 4.1 COL 33 COLON-ALIGNED NO-LABEL
     val-5 AT ROW 4.81 COL 6 COLON-ALIGNED
     type-5 AT ROW 4.81 COL 24 COLON-ALIGNED NO-LABEL
     dscr-5 AT ROW 4.81 COL 33 COLON-ALIGNED NO-LABEL
     val-6 AT ROW 5.52 COL 6 COLON-ALIGNED
     type-6 AT ROW 5.52 COL 24 COLON-ALIGNED NO-LABEL
     dscr-6 AT ROW 5.52 COL 33 COLON-ALIGNED NO-LABEL
     val-7 AT ROW 6.24 COL 6 COLON-ALIGNED
     type-7 AT ROW 6.24 COL 24 COLON-ALIGNED NO-LABEL
     dscr-7 AT ROW 6.24 COL 33 COLON-ALIGNED NO-LABEL
     val-8 AT ROW 6.95 COL 6 COLON-ALIGNED
     type-8 AT ROW 6.95 COL 24 COLON-ALIGNED NO-LABEL
     dscr-8 AT ROW 6.95 COL 33 COLON-ALIGNED NO-LABEL
     val-9 AT ROW 7.67 COL 6 COLON-ALIGNED
     type-9 AT ROW 7.67 COL 24 COLON-ALIGNED NO-LABEL
     dscr-9 AT ROW 7.67 COL 33 COLON-ALIGNED NO-LABEL
     val-10 AT ROW 8.38 COL 6 COLON-ALIGNED
     type-10 AT ROW 8.38 COL 24 COLON-ALIGNED NO-LABEL
     dscr-10 AT ROW 8.38 COL 33 COLON-ALIGNED NO-LABEL
     val-11 AT ROW 9.1 COL 6 COLON-ALIGNED
     type-11 AT ROW 9.1 COL 24 COLON-ALIGNED NO-LABEL
     dscr-11 AT ROW 9.1 COL 33 COLON-ALIGNED NO-LABEL
     val-12 AT ROW 9.81 COL 6 COLON-ALIGNED
     type-12 AT ROW 9.81 COL 24 COLON-ALIGNED NO-LABEL
     dscr-12 AT ROW 9.81 COL 33 COLON-ALIGNED NO-LABEL
     val-13 AT ROW 10.52 COL 6 COLON-ALIGNED
     type-13 AT ROW 10.52 COL 24 COLON-ALIGNED NO-LABEL
     dscr-13 AT ROW 10.52 COL 33 COLON-ALIGNED NO-LABEL
     val-14 AT ROW 11.24 COL 6 COLON-ALIGNED
     type-14 AT ROW 11.24 COL 24 COLON-ALIGNED NO-LABEL
     dscr-14 AT ROW 11.24 COL 33 COLON-ALIGNED NO-LABEL
     val-15 AT ROW 11.95 COL 6 COLON-ALIGNED
     type-15 AT ROW 11.95 COL 24 COLON-ALIGNED NO-LABEL
     dscr-15 AT ROW 11.95 COL 33 COLON-ALIGNED NO-LABEL
     val-16 AT ROW 12.67 COL 6 COLON-ALIGNED
     type-16 AT ROW 12.67 COL 24 COLON-ALIGNED NO-LABEL
     dscr-16 AT ROW 12.67 COL 33 COLON-ALIGNED NO-LABEL
     val-17 AT ROW 13.38 COL 6 COLON-ALIGNED
     type-17 AT ROW 13.38 COL 24 COLON-ALIGNED NO-LABEL
     dscr-17 AT ROW 13.38 COL 33 COLON-ALIGNED NO-LABEL
     val-18 AT ROW 14.1 COL 6 COLON-ALIGNED
     type-18 AT ROW 14.1 COL 24 COLON-ALIGNED NO-LABEL
     dscr-18 AT ROW 14.1 COL 33 COLON-ALIGNED NO-LABEL
     val-19 AT ROW 14.81 COL 6 COLON-ALIGNED
     type-19 AT ROW 14.81 COL 24 COLON-ALIGNED NO-LABEL
     dscr-19 AT ROW 14.81 COL 33 COLON-ALIGNED NO-LABEL
     val-20 AT ROW 15.52 COL 6 COLON-ALIGNED
     type-20 AT ROW 15.52 COL 24 COLON-ALIGNED NO-LABEL
     dscr-20 AT ROW 15.52 COL 33 COLON-ALIGNED NO-LABEL
     val-21 AT ROW 16.24 COL 6 COLON-ALIGNED
     type-21 AT ROW 16.24 COL 24 COLON-ALIGNED NO-LABEL
     dscr-21 AT ROW 16.24 COL 33 COLON-ALIGNED NO-LABEL
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     val-22 AT ROW 16.95 COL 6 COLON-ALIGNED
     type-22 AT ROW 16.95 COL 24 COLON-ALIGNED NO-LABEL
     dscr-22 AT ROW 16.95 COL 33 COLON-ALIGNED NO-LABEL
     val-23 AT ROW 17.67 COL 6 COLON-ALIGNED
     type-23 AT ROW 17.67 COL 24 COLON-ALIGNED NO-LABEL
     dscr-23 AT ROW 17.67 COL 33 COLON-ALIGNED NO-LABEL
     val-24 AT ROW 18.38 COL 6 COLON-ALIGNED
     type-24 AT ROW 18.38 COL 24 COLON-ALIGNED NO-LABEL
     dscr-24 AT ROW 18.38 COL 33 COLON-ALIGNED NO-LABEL
     val-25 AT ROW 19.1 COL 6 COLON-ALIGNED
     type-25 AT ROW 19.1 COL 24 COLON-ALIGNED NO-LABEL
     dscr-25 AT ROW 19.1 COL 33 COLON-ALIGNED NO-LABEL
     val-26 AT ROW 19.81 COL 6 COLON-ALIGNED
     type-26 AT ROW 19.81 COL 24 COLON-ALIGNED NO-LABEL
     dscr-26 AT ROW 19.81 COL 33 COLON-ALIGNED NO-LABEL
     val-27 AT ROW 20.52 COL 6 COLON-ALIGNED
     type-27 AT ROW 20.52 COL 24 COLON-ALIGNED NO-LABEL
     dscr-27 AT ROW 20.52 COL 33 COLON-ALIGNED NO-LABEL
     val-28 AT ROW 21.24 COL 6 COLON-ALIGNED
     type-28 AT ROW 21.24 COL 24 COLON-ALIGNED NO-LABEL
     dscr-28 AT ROW 21.24 COL 33 COLON-ALIGNED NO-LABEL
     val-29 AT ROW 21.95 COL 6 COLON-ALIGNED
     type-29 AT ROW 21.95 COL 24 COLON-ALIGNED NO-LABEL
     dscr-29 AT ROW 21.95 COL 33 COLON-ALIGNED NO-LABEL
     val-30 AT ROW 22.67 COL 6 COLON-ALIGNED
     type-30 AT ROW 22.67 COL 24 COLON-ALIGNED NO-LABEL
     dscr-30 AT ROW 22.67 COL 33 COLON-ALIGNED NO-LABEL
     btn-done AT ROW 1.48 COL 83
     btn-cancel AT ROW 3.14 COL 83
     "Panel Size" VIEW-AS TEXT
          SIZE 14 BY .76 AT ROW 1.24 COL 8
     "Type" VIEW-AS TEXT
          SIZE 6 BY .76 AT ROW 1.24 COL 26
     "Description" VIEW-AS TEXT
          SIZE 45 BY .76 AT ROW 1.24 COL 35
     RECT-16 AT ROW 1 COL 1
     SPACE(16.99) SKIP(0.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Panels Detail"
         CANCEL-BUTTON btn-cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN dscr-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-10 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-11 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-12 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-13 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-14 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-15 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-16 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-17 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-18 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-19 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-20 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-21 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-22 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-23 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-24 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-25 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-26 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-27 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-28 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-29 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-3 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-30 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-4 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-5 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-6 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-7 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-8 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dscr-9 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Panels Detail */
DO:
  APPLY "choose" TO btn-done. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Panels Detail */
DO:
  DEF VAR char-val AS CHAR NO-UNDO.


  IF FOCUS:NAME BEGINS "type-" THEN DO:
    RUN windows/l-scores.p (cocode, FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val NE "" AND ENTRY(1,char-val) NE FOCUS:SCREEN-VALUE THEN DO:
      FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
      APPLY "value-changed" TO FOCUS.
    END.
  END.

  ELSE
  CASE FOCUS:NAME:
    WHEN "" THEN DO:        
    END.
  END.

  RETURN NO-APPLY. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Panels Detail */
DO: 
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    apply "window-close" to frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-done Dialog-Frame
ON CHOOSE OF btn-done IN FRAME Dialog-Frame /* Save */
DO:  
   def var hd1 as handle no-undo.
   def var hd2 as handle no-undo.
   def var i as int no-undo.


   /* Code placed here will execute PRIOR to standard behavior. */
   IF ip-16ths THEN DO:
     ASSIGN
     hd1 = frame {&frame-name}:HANDLE
     hd1 = hd1:FIRST-CHILD
     hd2 = hd1:first-child.
     
     do while valid-handle(hd2):
        if hd2:type = "fill-in" and 
           hd2:data-type = "decimal" AND
           decimal(hd2:screen-value) - trunc(decimal(hd2:screen-value),0) >= v-16-or-32 
        then do:
             message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                      view-as alert-box error.
             apply "entry" to hd2.
             return no-apply.
        end. 
        hd2 = hd2:next-sibling.
     end.
   END.
   /* ==== end of 16ths validation =============*/

   RUN cec/val-type.p (FRAME {&FRAME-NAME}:HANDLE, ?) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    
   DO WITH FRAME {&FRAME-NAME}:
     ASSIGN {&displayed-objects}.
   END.

   IF ip-16ths THEN RUN 16ths-to-dec.

   ASSIGN
    op-total[01] = val-1
    op-total[02] = val-2
    op-total[03] = val-3
    op-total[04] = val-4
    op-total[05] = val-5
    op-total[06] = val-6
    op-total[07] = val-7
    op-total[08] = val-8
    op-total[09] = val-9
    op-total[10] = val-10
    op-total[11] = val-11
    op-total[12] = val-12
    op-total[13] = val-13
    op-total[14] = val-14
    op-total[15] = val-15
    op-total[16] = val-16
    op-total[17] = val-17
    op-total[18] = val-18
    op-total[19] = val-19
    op-total[20] = val-20
    op-total[21] = val-21
    op-total[22] = val-22
    op-total[23] = val-23
    op-total[24] = val-24
    op-total[25] = val-25
    op-total[26] = val-26
    op-total[27] = val-27
    op-total[28] = val-28
    op-total[29] = val-29
    op-total[30] = val-30.

   ASSIGN
    op-type[01] = type-1
    op-type[02] = type-2
    op-type[03] = type-3
    op-type[04] = type-4
    op-type[05] = type-5
    op-type[06] = type-6
    op-type[07] = type-7
    op-type[08] = type-8
    op-type[09] = type-9
    op-type[10] = type-10
    op-type[11] = type-11
    op-type[12] = type-12
    op-type[13] = type-13
    op-type[14] = type-14
    op-type[15] = type-15
    op-type[16] = type-16
    op-type[17] = type-17
    op-type[18] = type-18
    op-type[19] = type-19
    op-type[20] = type-20
    op-type[21] = type-21
    op-type[22] = type-22
    op-type[23] = type-23
    op-type[24] = type-24
    op-type[25] = type-25
    op-type[26] = type-26
    op-type[27] = type-27
    op-type[28] = type-28
    op-type[29] = type-29
    op-type[30] = type-30.

   EMPTY TEMP-TABLE tt-array.

   DO i = 1 TO EXTENT(op-total):
      CREATE tt-array.
      ASSIGN
         tt-dec  = op-total[i]
         tt-type = op-type[i].
      RELEASE tt-array.
   END.

   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-1 Dialog-Frame
ON LEAVE OF type-1 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-1 Dialog-Frame
ON VALUE-CHANGED OF type-1 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-10 Dialog-Frame
ON LEAVE OF type-10 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 10}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-10 Dialog-Frame
ON VALUE-CHANGED OF type-10 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 10}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-11 Dialog-Frame
ON LEAVE OF type-11 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 11}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-11 Dialog-Frame
ON VALUE-CHANGED OF type-11 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 11}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-12 Dialog-Frame
ON LEAVE OF type-12 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 12}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-12 Dialog-Frame
ON VALUE-CHANGED OF type-12 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 12}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-13 Dialog-Frame
ON LEAVE OF type-13 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 13}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-13 Dialog-Frame
ON VALUE-CHANGED OF type-13 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 13}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-14 Dialog-Frame
ON LEAVE OF type-14 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 13}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-14 Dialog-Frame
ON VALUE-CHANGED OF type-14 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 14}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-15 Dialog-Frame
ON LEAVE OF type-15 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 15}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-15 Dialog-Frame
ON VALUE-CHANGED OF type-15 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 15}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-16 Dialog-Frame
ON LEAVE OF type-16 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 16}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-16 Dialog-Frame
ON VALUE-CHANGED OF type-16 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 16}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-17 Dialog-Frame
ON LEAVE OF type-17 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 17}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-17 Dialog-Frame
ON VALUE-CHANGED OF type-17 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 17}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-18 Dialog-Frame
ON LEAVE OF type-18 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 18}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-18 Dialog-Frame
ON VALUE-CHANGED OF type-18 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 18}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-19 Dialog-Frame
ON LEAVE OF type-19 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 19}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-19 Dialog-Frame
ON VALUE-CHANGED OF type-19 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 19}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-2 Dialog-Frame
ON LEAVE OF type-2 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-2 Dialog-Frame
ON VALUE-CHANGED OF type-2 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-20 Dialog-Frame
ON LEAVE OF type-20 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 20}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-20 Dialog-Frame
ON VALUE-CHANGED OF type-20 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 20}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-21 Dialog-Frame
ON LEAVE OF type-21 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 21}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-21 Dialog-Frame
ON VALUE-CHANGED OF type-21 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 21}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-22
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-22 Dialog-Frame
ON LEAVE OF type-22 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 22}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-22 Dialog-Frame
ON VALUE-CHANGED OF type-22 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 22}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-23 Dialog-Frame
ON LEAVE OF type-23 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 23}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-23 Dialog-Frame
ON VALUE-CHANGED OF type-23 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 23}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-24
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-24 Dialog-Frame
ON LEAVE OF type-24 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 23}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-24 Dialog-Frame
ON VALUE-CHANGED OF type-24 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 24}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-25
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-25 Dialog-Frame
ON LEAVE OF type-25 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 25}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-25 Dialog-Frame
ON VALUE-CHANGED OF type-25 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 25}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-26
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-26 Dialog-Frame
ON LEAVE OF type-26 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 26}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-26 Dialog-Frame
ON VALUE-CHANGED OF type-26 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 26}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-27
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-27 Dialog-Frame
ON LEAVE OF type-27 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 27}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-27 Dialog-Frame
ON VALUE-CHANGED OF type-27 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 27}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-28
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-28 Dialog-Frame
ON LEAVE OF type-28 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 28}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-28 Dialog-Frame
ON VALUE-CHANGED OF type-28 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 28}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-29
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-29 Dialog-Frame
ON LEAVE OF type-29 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 29}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-29 Dialog-Frame
ON VALUE-CHANGED OF type-29 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 29}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-3 Dialog-Frame
ON LEAVE OF type-3 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 3}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-3 Dialog-Frame
ON VALUE-CHANGED OF type-3 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 3}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-30
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-30 Dialog-Frame
ON LEAVE OF type-30 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 30}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-30 Dialog-Frame
ON VALUE-CHANGED OF type-30 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 30}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-4 Dialog-Frame
ON LEAVE OF type-4 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 4}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-4 Dialog-Frame
ON VALUE-CHANGED OF type-4 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 4}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-5 Dialog-Frame
ON LEAVE OF type-5 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 5}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-5 Dialog-Frame
ON VALUE-CHANGED OF type-5 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 5}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-6 Dialog-Frame
ON LEAVE OF type-6 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 6}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-6 Dialog-Frame
ON VALUE-CHANGED OF type-6 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 6}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-7 Dialog-Frame
ON LEAVE OF type-7 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 7}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-7 Dialog-Frame
ON VALUE-CHANGED OF type-7 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 7}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-8 Dialog-Frame
ON LEAVE OF type-8 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 8}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-8 Dialog-Frame
ON VALUE-CHANGED OF type-8 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 8}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME type-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-9 Dialog-Frame
ON LEAVE OF type-9 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-1.i 9}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL type-9 Dialog-Frame
ON VALUE-CHANGED OF type-9 IN FRAME Dialog-Frame
DO:
  {cec/d-pan-2.i 9}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-1 Dialog-Frame
ON LEAVE OF val-1 IN FRAME Dialog-Frame /* 1 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

   if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
    
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
         /* self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-10 Dialog-Frame
ON LEAVE OF val-10 IN FRAME Dialog-Frame /* 10 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.

   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
         /* self:screen-value = string( val-num +  op-dec) . */
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-11 Dialog-Frame
ON LEAVE OF val-11 IN FRAME Dialog-Frame /* 11 */
DO:
  DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
  IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
         /* self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-12 Dialog-Frame
ON LEAVE OF val-12 IN FRAME Dialog-Frame /* 12 */
DO:
  DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
        /*  self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-13 Dialog-Frame
ON LEAVE OF val-13 IN FRAME Dialog-Frame /* 13 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

    if lastkey <> -1 and
        decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
     then do:
        message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
            view-as alert-box error.
        return no-apply.
     end.
 IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
        /*  self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-14 Dialog-Frame
ON LEAVE OF val-14 IN FRAME Dialog-Frame /* 14 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
       /*   self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-15 Dialog-Frame
ON LEAVE OF val-15 IN FRAME Dialog-Frame /* 15 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
        /*  self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-16 Dialog-Frame
ON LEAVE OF val-16 IN FRAME Dialog-Frame /* 16 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

    if lastkey <> -1 and
       decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
    then do:
       message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
           view-as alert-box error.
       return no-apply.
    end.
    IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
        /*  self:screen-value = string( val-num +  op-dec) . */
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-17 Dialog-Frame
ON LEAVE OF val-17 IN FRAME Dialog-Frame /* 17 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
         /* self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-18 Dialog-Frame
ON LEAVE OF val-18 IN FRAME Dialog-Frame /* 18 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

 if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
         /* self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-19 Dialog-Frame
ON LEAVE OF val-19 IN FRAME Dialog-Frame /* 19 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

 if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
       /*   self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-2 Dialog-Frame
ON LEAVE OF val-2 IN FRAME Dialog-Frame /* 2 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

   if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
         /* self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-20 Dialog-Frame
ON LEAVE OF val-20 IN FRAME Dialog-Frame /* 20 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

    if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
        /*  self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-21 Dialog-Frame
ON LEAVE OF val-21 IN FRAME Dialog-Frame /* 21 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
      /*    self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-22
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-22 Dialog-Frame
ON LEAVE OF val-22 IN FRAME Dialog-Frame /* 22 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
         /* self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-23 Dialog-Frame
ON LEAVE OF val-23 IN FRAME Dialog-Frame /* 23 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

    if lastkey <> -1 and
        decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
     then do:
        message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
            view-as alert-box error.
        return no-apply.
     end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
         /* self:screen-value = string( val-num +  op-dec) . */
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-24
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-24 Dialog-Frame
ON LEAVE OF val-24 IN FRAME Dialog-Frame /* 24 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
    IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
         /* self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-25
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-25 Dialog-Frame
ON LEAVE OF val-25 IN FRAME Dialog-Frame /* 25 */
DO:
    DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
        /*  self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-26
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-26 Dialog-Frame
ON LEAVE OF val-26 IN FRAME Dialog-Frame /* 26 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

    if lastkey <> -1 and
       decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
    then do:
       message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
           view-as alert-box error.
       return no-apply.
    end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
       /*   self:screen-value = string( val-num +  op-dec) . */
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-27
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-27 Dialog-Frame
ON LEAVE OF val-27 IN FRAME Dialog-Frame /* 27 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
         /* self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-28
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-28 Dialog-Frame
ON LEAVE OF val-28 IN FRAME Dialog-Frame /* 28 */
DO:
    DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

 if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
        /*  self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-29
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-29 Dialog-Frame
ON LEAVE OF val-29 IN FRAME Dialog-Frame /* 29 */
DO:
    DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

 if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
        /*   self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-3 Dialog-Frame
ON LEAVE OF val-3 IN FRAME Dialog-Frame /* 3 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

   if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
        /*  self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-30
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-30 Dialog-Frame
ON LEAVE OF val-30 IN FRAME Dialog-Frame /* 30 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

    if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
        /*  self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-4 Dialog-Frame
ON LEAVE OF val-4 IN FRAME Dialog-Frame /* 4 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
       /*   self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-5 Dialog-Frame
ON LEAVE OF val-5 IN FRAME Dialog-Frame /* 5 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

 if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
        /*  self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-6 Dialog-Frame
ON LEAVE OF val-6 IN FRAME Dialog-Frame /* 6 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

   if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
        /*  self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-7 Dialog-Frame
ON LEAVE OF val-7 IN FRAME Dialog-Frame /* 7 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
       /*   self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-8 Dialog-Frame
ON LEAVE OF val-8 IN FRAME Dialog-Frame /* 8 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

  if lastkey <> -1 and
      decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
   then do:
      message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
          view-as alert-box error.
      return no-apply.
   end.
   IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
         /* self:screen-value = string( val-num +  op-dec) . */
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-9 Dialog-Frame
ON LEAVE OF val-9 IN FRAME Dialog-Frame /* 9 */
DO:
   DEF VAR v-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-dec AS DEC DECIMALS 6 NO-UNDO.
   DEF VAR op-error AS LOG NO-UNDO.
   DEFINE VAR val-num AS INT NO-UNDO.

   
   v-dec = decimal(self:screen-value) - trunc(decimal(self:screen-value),0).

    if lastkey <> -1 and
       decimal(self:screen-value) - trunc(decimal(self:screen-value),0) >= v-16-or-32 
    then do:
       message "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
           view-as alert-box error.
       return no-apply.
    end.
    IF v-cecscrn-dec THEN
   DO:
      val-num = INT(self:screen-value) .
      RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
      IF op-error THEN DO:
         MESSAGE "Invalid Dimension."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO SELF.
         RETURN NO-APPLY.
      END.
      ELSE do: 
          
        /*  self:screen-value = string( val-num +  op-dec) . */
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
 
   FRAME {&FRAME-NAME}:TITLE = TRIM(ip-code) + " Detail ".

   ASSIGN
    val-1  = op-total[01]
    val-2  = op-total[02]
    val-3  = op-total[03]
    val-4  = op-total[04]
    val-5  = op-total[05]
    val-6  = op-total[06]
    val-7  = op-total[07]
    val-8  = op-total[08]
    val-9  = op-total[09]
    val-10 = op-total[10]
    val-11 = op-total[11]
    val-12 = op-total[12]
    val-13 = op-total[13]
    val-14 = op-total[14]
    val-15 = op-total[15]
    val-16 = op-total[16]
    val-17 = op-total[17]
    val-18 = op-total[18]
    val-19 = op-total[19]
    val-20 = op-total[20]
    val-21 = op-total[21]
    val-22 = op-total[22]
    val-23 = op-total[23]
    val-24 = op-total[24]
    val-25 = op-total[25]
    val-26 = op-total[26]
    val-27 = op-total[27]
    val-28 = op-total[28]
    val-29 = op-total[29]
    val-30 = op-total[30].

   ASSIGN    
    type-1  = op-type[01]
    type-2  = op-type[02]
    type-3  = op-type[03]
    type-4  = op-type[04]
    type-5  = op-type[05]
    type-6  = op-type[06]
    type-7  = op-type[07]
    type-8  = op-type[08]
    type-9  = op-type[09]
    type-10 = op-type[10]
    type-11 = op-type[11]
    type-12 = op-type[12]
    type-13 = op-type[13]
    type-14 = op-type[14]
    type-15 = op-type[15]
    type-16 = op-type[16]
    type-17 = op-type[17]
    type-18 = op-type[18]
    type-19 = op-type[19]
    type-20 = op-type[20]
    type-21 = op-type[21]
    type-22 = op-type[22]
    type-23 = op-type[23]
    type-24 = op-type[24]
    type-25 = op-type[25]
    type-26 = op-type[26]
    type-27 = op-type[27]
    type-28 = op-type[28]
    type-29 = op-type[29]
    type-30 = op-type[30].

   IF ip-16ths THEN RUN dec-to-16ths.

   IF v-cecscrn-char EQ "Decimal" THEN
      RUN decimal-format.

   RUN enable_UI.

   DO WITH FRAME {&FRAME-NAME}:
     APPLY "value-changed" TO type-1.
     APPLY "value-changed" TO type-2.
     APPLY "value-changed" TO type-3.
     APPLY "value-changed" TO type-4.
     APPLY "value-changed" TO type-5.
     APPLY "value-changed" TO type-6.
     APPLY "value-changed" TO type-7.
     APPLY "value-changed" TO type-8.
     APPLY "value-changed" TO type-9.
     APPLY "value-changed" TO type-10.
     APPLY "value-changed" TO type-11.
     APPLY "value-changed" TO type-12.
     APPLY "value-changed" TO type-13.
     APPLY "value-changed" TO type-14.
     APPLY "value-changed" TO type-15.
     APPLY "value-changed" TO type-16.
     APPLY "value-changed" TO type-17.
     APPLY "value-changed" TO type-18.
     APPLY "value-changed" TO type-19.
     APPLY "value-changed" TO type-20.
     APPLY "value-changed" TO type-21.
     APPLY "value-changed" TO type-22.
     APPLY "value-changed" TO type-23.
     APPLY "value-changed" TO type-24.
     APPLY "value-changed" TO type-25.
     APPLY "value-changed" TO type-26.
     APPLY "value-changed" TO type-27.
     APPLY "value-changed" TO type-28.
     APPLY "value-changed" TO type-29.
     APPLY "value-changed" TO type-30.
   END.

   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 16ths-to-dec Dialog-Frame 
PROCEDURE 16ths-to-dec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  {sys/inc/k16bb.i val-1}
  {sys/inc/k16bb.i val-2}
  {sys/inc/k16bb.i val-3}
  {sys/inc/k16bb.i val-4}
  {sys/inc/k16bb.i val-5}
  {sys/inc/k16bb.i val-6}
  {sys/inc/k16bb.i val-7}
  {sys/inc/k16bb.i val-8}
  {sys/inc/k16bb.i val-9}
  {sys/inc/k16bb.i val-10}
  {sys/inc/k16bb.i val-11}
  {sys/inc/k16bb.i val-12}
  {sys/inc/k16bb.i val-13}
  {sys/inc/k16bb.i val-14}
  {sys/inc/k16bb.i val-15}
  {sys/inc/k16bb.i val-16}
  {sys/inc/k16bb.i val-17}
  {sys/inc/k16bb.i val-18}
  {sys/inc/k16bb.i val-19}
  {sys/inc/k16bb.i val-20}
  {sys/inc/k16bb.i val-21}
  {sys/inc/k16bb.i val-22}
  {sys/inc/k16bb.i val-23}
  {sys/inc/k16bb.i val-24}
  {sys/inc/k16bb.i val-25}
  {sys/inc/k16bb.i val-26}
  {sys/inc/k16bb.i val-27}
  {sys/inc/k16bb.i val-28}
  {sys/inc/k16bb.i val-29}
  {sys/inc/k16bb.i val-30}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dec-to-16ths Dialog-Frame 
PROCEDURE dec-to-16ths :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  ASSIGN
   val-1  = {sys/inc/k16v.i val-1}
   val-2  = {sys/inc/k16v.i val-2}
   val-3  = {sys/inc/k16v.i val-3}
   val-4  = {sys/inc/k16v.i val-4}
   val-5  = {sys/inc/k16v.i val-5}
   val-6  = {sys/inc/k16v.i val-6}
   val-7  = {sys/inc/k16v.i val-7}
   val-8  = {sys/inc/k16v.i val-8}
   val-9  = {sys/inc/k16v.i val-9}
   val-10 = {sys/inc/k16v.i val-10}
   val-11 = {sys/inc/k16v.i val-11}
   val-12 = {sys/inc/k16v.i val-12}
   val-13 = {sys/inc/k16v.i val-13}
   val-14 = {sys/inc/k16v.i val-14}
   val-15 = {sys/inc/k16v.i val-15}
   val-16 = {sys/inc/k16v.i val-16}
   val-17 = {sys/inc/k16v.i val-17}
   val-18 = {sys/inc/k16v.i val-18}
   val-19 = {sys/inc/k16v.i val-19}
   val-20 = {sys/inc/k16v.i val-20}
   val-21 = {sys/inc/k16v.i val-21}
   val-22 = {sys/inc/k16v.i val-22}
   val-23 = {sys/inc/k16v.i val-23}
   val-24 = {sys/inc/k16v.i val-24}
   val-25 = {sys/inc/k16v.i val-25}
   val-26 = {sys/inc/k16v.i val-26}
   val-27 = {sys/inc/k16v.i val-27}
   val-28 = {sys/inc/k16v.i val-28}
   val-29 = {sys/inc/k16v.i val-29}
   val-30 = {sys/inc/k16v.i val-30}.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE decimal-format Dialog-Frame 
PROCEDURE decimal-format :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
         val-1:FORMAT = "->>,>>9.999999"
         val-2:FORMAT = "->>,>>9.999999"
         val-3:FORMAT = "->>,>>9.999999"
         val-4:FORMAT = "->>,>>9.999999"
         val-5:FORMAT = "->>,>>9.999999"
         val-6:FORMAT = "->>,>>9.999999"
         val-7:FORMAT = "->>,>>9.999999"
         val-8:FORMAT = "->>,>>9.999999"
         val-9:FORMAT = "->>,>>9.999999"
         val-10:FORMAT = "->>,>>9.999999"
         val-11:FORMAT = "->>,>>9.999999"
         val-12:FORMAT = "->>,>>9.999999"
         val-13:FORMAT = "->>,>>9.999999"
         val-14:FORMAT = "->>,>>9.999999"
         val-15:FORMAT = "->>,>>9.999999"
         val-16:FORMAT = "->>,>>9.999999"
         val-17:FORMAT = "->>,>>9.999999"
         val-18:FORMAT = "->>,>>9.999999"
         val-19:FORMAT = "->>,>>9.999999"
         val-20:FORMAT = "->>,>>9.999999"
         val-21:FORMAT = "->>,>>9.999999"
         val-22:FORMAT = "->>,>>9.999999"
         val-23:FORMAT = "->>,>>9.999999"
         val-24:FORMAT = "->>,>>9.999999"
         val-25:FORMAT = "->>,>>9.999999"
         val-26:FORMAT = "->>,>>9.999999"
         val-27:FORMAT = "->>,>>9.999999"
         val-28:FORMAT = "->>,>>9.999999"
         val-29:FORMAT = "->>,>>9.999999"
         val-30:FORMAT = "->>,>>9.999999".
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY val-1 type-1 dscr-1 val-2 type-2 dscr-2 val-3 type-3 dscr-3 val-4 
          type-4 dscr-4 val-5 type-5 dscr-5 val-6 type-6 dscr-6 val-7 type-7 
          dscr-7 val-8 type-8 dscr-8 val-9 type-9 dscr-9 val-10 type-10 dscr-10 
          val-11 type-11 dscr-11 val-12 type-12 dscr-12 val-13 type-13 dscr-13 
          val-14 type-14 dscr-14 val-15 type-15 dscr-15 val-16 type-16 dscr-16 
          val-17 type-17 dscr-17 val-18 type-18 dscr-18 val-19 type-19 dscr-19 
          val-20 type-20 dscr-20 val-21 type-21 dscr-21 val-22 type-22 dscr-22 
          val-23 type-23 dscr-23 val-24 type-24 dscr-24 val-25 type-25 dscr-25 
          val-26 type-26 dscr-26 val-27 type-27 dscr-27 val-28 type-28 dscr-28 
          val-29 type-29 dscr-29 val-30 type-30 dscr-30 
      WITH FRAME Dialog-Frame.
  ENABLE val-1 type-1 val-2 type-2 val-3 type-3 val-4 type-4 val-5 type-5 val-6 
         type-6 val-7 type-7 val-8 type-8 val-9 type-9 val-10 type-10 val-11 
         type-11 val-12 type-12 val-13 type-13 val-14 type-14 val-15 type-15 
         val-16 type-16 val-17 type-17 val-18 type-18 val-19 type-19 val-20 
         type-20 val-21 type-21 val-22 type-22 val-23 type-23 val-24 type-24 
         val-25 type-25 val-26 type-26 val-27 type-27 val-28 type-28 val-29 
         type-29 val-30 type-30 btn-done btn-cancel RECT-16 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-64-dec Dialog-Frame 
PROCEDURE valid-64-dec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-dec AS DEC DECIMALS 6 NO-UNDO.
   DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.
   DEFINE OUTPUT PARAMETER op-dec AS DEC DECIMALS 6 NO-UNDO.
    
    FIND FIRST tt-64-dec WHERE
      substring(string(tt-64-dec.DEC),1,3) EQ substring(string(ip-dec),1,3) NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-64-dec  THEN
      op-error = YES.
    ELSE  op-dec = tt-64-dec.DEC .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

