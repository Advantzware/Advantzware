&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

DEF VAR lv-rowid1 AS ROWID NO-UNDO.
DEF VAR lv-rowid2 AS ROWID NO-UNDO.
def var k_frac as dec init "6.25" no-undo.
DEF BUFFER b-ref1 FOR reftable.
DEF BUFFER b-ref2 FOR reftable.
ASSIGN cocode = g_company
       locode = g_loc.
{sys/inc/f16to32.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-30 val-1 type-1 val-2 type-2 val-3 ~
type-3 val-4 type-4 val-5 type-5 val-6 type-6 val-7 type-7 val-8 type-8 ~
val-9 type-9 val-10 type-10 val-11 type-11 val-12 type-12 val-13 type-13 ~
val-14 type-14 val-15 type-15 val-16 type-16 val-17 type-17 val-18 type-18 ~
val-19 type-19 val-20 type-20 btLenWid btn_ok 
&Scoped-Define DISPLAYED-OBJECTS val-1 type-1 dscr-1 val-2 type-2 dscr-2 ~
val-3 type-3 dscr-3 val-4 type-4 dscr-4 val-5 type-5 dscr-5 val-6 type-6 ~
dscr-6 val-7 type-7 dscr-7 val-8 type-8 dscr-8 val-9 type-9 dscr-9 val-10 ~
type-10 dscr-10 val-11 type-11 dscr-11 val-12 type-12 dscr-12 val-13 ~
type-13 dscr-13 val-14 type-14 dscr-14 val-15 type-15 dscr-15 val-16 ~
type-16 dscr-16 val-17 type-17 dscr-17 val-18 type-18 dscr-18 val-19 ~
type-19 dscr-19 val-20 type-20 dscr-20 fiLenWid 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btLenWid 
     LABEL "Load Lengths" 
     SIZE 17 BY 1.19.

DEFINE BUTTON btn_ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE dscr-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-11 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-12 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-13 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-14 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-15 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-16 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-17 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-18 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-19 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-20 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE dscr-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE fiLenWid AS CHARACTER FORMAT "X(256)":U 
     LABEL "Showing" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE type-1 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-10 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-11 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-12 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-13 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-14 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-15 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-16 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-17 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-18 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-19 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-2 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-20 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-3 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-4 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-5 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-6 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-7 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-8 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE type-9 AS CHARACTER FORMAT "x":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE val-1 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "1st Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE val-10 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "10th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-11 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "11th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-12 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "12th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-13 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "13th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-14 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "14th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-15 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "15th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-16 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "16th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-17 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "17th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-18 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "18th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-19 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "19th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-2 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "2nd Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-20 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "20th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-3 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "3rd Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-4 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "4th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-5 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "5th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-6 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "6th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-7 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "7th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-8 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "8th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE val-9 AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "9th Panel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 21.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     val-1 AT ROW 2.19 COL 14 COLON-ALIGNED
     type-1 AT ROW 2.19 COL 32 COLON-ALIGNED NO-LABEL
     dscr-1 AT ROW 2.19 COL 41 COLON-ALIGNED NO-LABEL
     val-2 AT ROW 3.19 COL 14 COLON-ALIGNED
     type-2 AT ROW 3.19 COL 32 COLON-ALIGNED NO-LABEL
     dscr-2 AT ROW 3.19 COL 41 COLON-ALIGNED NO-LABEL
     val-3 AT ROW 4.19 COL 14 COLON-ALIGNED
     type-3 AT ROW 4.19 COL 32 COLON-ALIGNED NO-LABEL
     dscr-3 AT ROW 4.19 COL 41 COLON-ALIGNED NO-LABEL
     val-4 AT ROW 5.19 COL 14 COLON-ALIGNED
     type-4 AT ROW 5.19 COL 32 COLON-ALIGNED NO-LABEL
     dscr-4 AT ROW 5.19 COL 41 COLON-ALIGNED NO-LABEL
     val-5 AT ROW 6.19 COL 14 COLON-ALIGNED
     type-5 AT ROW 6.19 COL 32 COLON-ALIGNED NO-LABEL
     dscr-5 AT ROW 6.19 COL 41 COLON-ALIGNED NO-LABEL
     val-6 AT ROW 7.19 COL 14 COLON-ALIGNED
     type-6 AT ROW 7.19 COL 32 COLON-ALIGNED NO-LABEL
     dscr-6 AT ROW 7.19 COL 41 COLON-ALIGNED NO-LABEL
     val-7 AT ROW 8.19 COL 14 COLON-ALIGNED
     type-7 AT ROW 8.19 COL 32 COLON-ALIGNED NO-LABEL
     dscr-7 AT ROW 8.19 COL 41 COLON-ALIGNED NO-LABEL
     val-8 AT ROW 9.19 COL 14 COLON-ALIGNED
     type-8 AT ROW 9.19 COL 32 COLON-ALIGNED NO-LABEL
     dscr-8 AT ROW 9.19 COL 41 COLON-ALIGNED NO-LABEL
     val-9 AT ROW 10.19 COL 14 COLON-ALIGNED
     type-9 AT ROW 10.19 COL 32 COLON-ALIGNED NO-LABEL
     dscr-9 AT ROW 10.19 COL 41 COLON-ALIGNED NO-LABEL
     val-10 AT ROW 11.19 COL 14 COLON-ALIGNED
     type-10 AT ROW 11.19 COL 32 COLON-ALIGNED NO-LABEL
     dscr-10 AT ROW 11.19 COL 41 COLON-ALIGNED NO-LABEL
     val-11 AT ROW 12.19 COL 14 COLON-ALIGNED
     type-11 AT ROW 12.19 COL 32 COLON-ALIGNED NO-LABEL
     dscr-11 AT ROW 12.19 COL 41 COLON-ALIGNED NO-LABEL
     val-12 AT ROW 13.19 COL 14 COLON-ALIGNED
     type-12 AT ROW 13.19 COL 32 COLON-ALIGNED NO-LABEL
     dscr-12 AT ROW 13.19 COL 41 COLON-ALIGNED NO-LABEL
     val-13 AT ROW 14.33 COL 14 COLON-ALIGNED
     type-13 AT ROW 14.33 COL 32 COLON-ALIGNED NO-LABEL
     dscr-13 AT ROW 14.33 COL 41 COLON-ALIGNED NO-LABEL
     val-14 AT ROW 15.33 COL 14 COLON-ALIGNED
     type-14 AT ROW 15.33 COL 32 COLON-ALIGNED NO-LABEL
     dscr-14 AT ROW 15.33 COL 41 COLON-ALIGNED NO-LABEL
     val-15 AT ROW 16.33 COL 14 COLON-ALIGNED
     type-15 AT ROW 16.33 COL 32 COLON-ALIGNED NO-LABEL
     dscr-15 AT ROW 16.33 COL 41 COLON-ALIGNED NO-LABEL
     val-16 AT ROW 17.43 COL 14 COLON-ALIGNED
     type-16 AT ROW 17.43 COL 32 COLON-ALIGNED NO-LABEL
     dscr-16 AT ROW 17.43 COL 41 COLON-ALIGNED NO-LABEL
     val-17 AT ROW 18.43 COL 14 COLON-ALIGNED
     type-17 AT ROW 18.43 COL 32 COLON-ALIGNED NO-LABEL
     dscr-17 AT ROW 18.43 COL 41 COLON-ALIGNED NO-LABEL
     val-18 AT ROW 19.43 COL 14 COLON-ALIGNED
     type-18 AT ROW 19.43 COL 32 COLON-ALIGNED NO-LABEL
     dscr-18 AT ROW 19.43 COL 41 COLON-ALIGNED NO-LABEL
     val-19 AT ROW 20.52 COL 14 COLON-ALIGNED
     type-19 AT ROW 20.52 COL 32 COLON-ALIGNED NO-LABEL
     dscr-19 AT ROW 20.52 COL 41 COLON-ALIGNED NO-LABEL
     val-20 AT ROW 21.52 COL 14 COLON-ALIGNED
     type-20 AT ROW 21.52 COL 32 COLON-ALIGNED NO-LABEL
     dscr-20 AT ROW 21.52 COL 41 COLON-ALIGNED NO-LABEL
     fiLenWid AT ROW 23.38 COL 12 COLON-ALIGNED WIDGET-ID 4
     btLenWid AT ROW 23.38 COL 36 WIDGET-ID 2
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     btn_ok AT ROW 23.38 COL 55
     "Type" VIEW-AS TEXT
          SIZE 6 BY 1 AT ROW 1.24 COL 34
     "Panel Size" VIEW-AS TEXT
          SIZE 16 BY 1 AT ROW 1.24 COL 16
     "Description" VIEW-AS TEXT
          SIZE 45 BY 1 AT ROW 1.24 COL 43
     RECT-30 AT ROW 1 COL 1
     SPACE(0.19) SKIP(1.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Panel Sizes".


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
   FRAME-NAME                                                           */
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
/* SETTINGS FOR FILL-IN dscr-3 IN FRAME Dialog-Frame
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
/* SETTINGS FOR FILL-IN fiLenWid IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiLenWid:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Panel Sizes */
DO:
  DEF VAR char-val AS CHAR NO-UNDO.
  DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.


  lw-focus = FOCUS.

  IF lw-focus:NAME BEGINS "type-" THEN DO:
    RUN windows/l-scores.p (cocode, lw-focus:SCREEN-VALUE, OUTPUT char-val).
    IF char-val NE "" AND ENTRY(1,char-val) NE lw-focus:SCREEN-VALUE THEN DO:
      lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
      APPLY "value-changed" TO lw-focus.
    END.
  END.

  ELSE
  CASE lw-focus:NAME:
    WHEN "" THEN DO:        
    END.
  END.

  RETURN NO-APPLY. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Panel Sizes */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLenWid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLenWid Dialog-Frame
ON CHOOSE OF btLenWid IN FRAME Dialog-Frame /* Load Lengths */
DO:
  DEF VAR ll AS LOG NO-UNDO.
  ll = NO.

  IF fiLenWid:SCREEN-VALUE = "Width" THEN
    MESSAGE "Load length scores instread of width scores, sure?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.
  ELSE 
    MESSAGE "Load width scores instread of length scores, sure?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.

  IF ll THEN DO:
    {po/po-ordls.i}
  
    {po/poordls2W.i}

    IF AVAIL b-ref1 THEN DELETE b-ref1.
    IF AVAIL b-ref2 THEN DELETE b-ref2.
    IF fiLenWid:SCREEN-VALUE = "Width" THEN
      RUN po/po-ordlw.p (RECID(po-ordl), "Length").
    ELSE
      RUN po/po-ordls.p (RECID(po-ordl)).

    {po/po-ordls.i}  
    {po/poordls2W.i}

    ASSIGN
      lv-rowid1 = ROWID(b-ref1)
      lv-rowid2 = ROWID(b-ref2).
    RUN initial-screen.

    IF fiLenWid:SCREEN-VALUE = "Width" THEN
       ASSIGN fiLenWid = "Length" btLenWid:LABEL = "Load Widths".
    ELSE
       ASSIGN fiLenWid = "Width" btLenWid:LABEL = "Load Lengths".
    DISP fiLenWid WITH FRAME {&FRAME-NAME}.
    FIND po-ordl WHERE ROWID(po-ordl) EQ ip-rowid EXCLUSIVE-LOCK NO-ERROR.
    IF avail(po-ordl) AND po-ordl.spare-char-1 NE fiLenWid:SCREEN-VALUE THEN
      po-ordl.spare-char-1 = fiLenWid:SCREEN-VALUE.
    FIND po-ordl WHERE ROWID(po-ordl) EQ ip-rowid NO-LOCK NO-ERROR.


  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok Dialog-Frame
ON CHOOSE OF btn_ok IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR v-xg-flag AS LOG NO-UNDO.
  DEF VAR v-tot-score AS DEC EXTENT 2 NO-UNDO.
  DEF VAR choice AS LOG NO-UNDO.
  DEF VAR choice1 AS LOG NO-UNDO.
  DEF VAR v-comp-val LIKE po-ordl.s-len NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-panel (val-1:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-2:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-3:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-4:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-5:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-6:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-7:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-8:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-9:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-10:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-11:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-12:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-13:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-14:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-15:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-16:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-17:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-18:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-19:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-panel (val-20:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  RUN cec/val-type.p (FRAME {&FRAME-NAME}:HANDLE, ?) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  FIND b-ref1 WHERE ROWID(b-ref1) EQ lv-rowid1 NO-ERROR.
  FIND b-ref2 WHERE ROWID(b-ref2) EQ lv-rowid2 NO-ERROR.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
  IF NOT AVAIL b-ref1 OR NOT AVAIL b-ref2 THEN DO:

    {po/po-ordls.i}  
    {po/poordls2W.i}
    IF NOT AVAIL b-ref1 OR NOT AVAIL b-ref2 THEN DO:    
       RUN po/poaddscores.p (INPUT ROWID(po-ordl)).
       {po/po-ordls.i}  
       {po/poordls2W.i}
    END.
  END.
    

        
  DO WHILE TRUE:
    IF AVAIL b-ref1 THEN
      ASSIGN
       b-ref1.val[1]  = val-1
       b-ref1.val[2]  = val-2
       b-ref1.val[3]  = val-3
       b-ref1.val[4]  = val-4
       b-ref1.val[5]  = val-5
       b-ref1.val[6]  = val-6
       b-ref1.val[7]  = val-7
       b-ref1.val[8]  = val-8
       b-ref1.val[9]  = val-9
       b-ref1.val[10] = val-10
       b-ref1.val[11] = val-11
       b-ref1.val[12] = val-12

       b-ref1.dscr = ""
       b-ref1.dscr = b-ref1.dscr + STRING(type-1,"X")
       b-ref1.dscr = b-ref1.dscr + STRING(type-2,"X")
       b-ref1.dscr = b-ref1.dscr + STRING(type-3,"X")
       b-ref1.dscr = b-ref1.dscr + STRING(type-4,"X")
       b-ref1.dscr = b-ref1.dscr + STRING(type-5,"X")
       b-ref1.dscr = b-ref1.dscr + STRING(type-6,"X")
       b-ref1.dscr = b-ref1.dscr + STRING(type-7,"X")
       b-ref1.dscr = b-ref1.dscr + STRING(type-8,"X")
       b-ref1.dscr = b-ref1.dscr + STRING(type-9,"X")
       b-ref1.dscr = b-ref1.dscr + STRING(type-10,"X")
       b-ref1.dscr = b-ref1.dscr + STRING(type-11,"X")
       b-ref1.dscr = b-ref1.dscr + STRING(type-12,"X").

    IF AVAIL b-ref2 THEN
      ASSIGN
       b-ref2.val[1]  = val-13
       b-ref2.val[2]  = val-14
       b-ref2.val[3]  = val-15
       b-ref2.val[4]  = val-16
       b-ref2.val[5]  = val-17
       b-ref2.val[6]  = val-18
       b-ref2.val[7]  = val-19
       b-ref2.val[8]  = val-20

       b-ref2.dscr = ""
       b-ref2.dscr = b-ref2.dscr + STRING(type-13,"X")
       b-ref2.dscr = b-ref2.dscr + STRING(type-14,"X")
       b-ref2.dscr = b-ref2.dscr + STRING(type-15,"X")
       b-ref2.dscr = b-ref2.dscr + STRING(type-16,"X")
       b-ref2.dscr = b-ref2.dscr + STRING(type-17,"X")
       b-ref2.dscr = b-ref2.dscr + STRING(type-18,"X")
       b-ref2.dscr = b-ref2.dscr + STRING(type-19,"X")
       b-ref2.dscr = b-ref2.dscr + STRING(type-20,"X").

    {po/poordls2W.i}
    {po/poordls3W.i}
    
    v-tot-score = 0.

    IF AVAIL b-ref1 THEN
    DO i = 1 TO 12:
      ASSIGN 
       v-tot-score[1] = v-tot-score[1] + TRUNC(b-ref1.val[i],0)
       v-tot-score[2] = v-tot-score[2] +
                        (b-ref1.val[i] - TRUNC(b-ref1.val[i],0)).
    END.
    IF AVAIL b-ref2 THEN
    DO i = 1 TO 8:
      ASSIGN 
       v-tot-score[1] = v-tot-score[1] + TRUNC(b-ref2.val[i],0)
       v-tot-score[2] = v-tot-score[2] +
                        (b-ref2.val[i] - TRUNC(b-ref2.val[i],0)).
    END.

    ASSIGN
     v-tot-score[1] = v-tot-score[1] + (v-tot-score[2] * 100 / li-16-32).

     choice = IF fiLenWid:SCREEN-VALUE = "Length" THEN
                po-ordl.s-len = v-tot-score[1]
              ELSE
                po-ordl.s-wid = v-tot-score[1].
     v-comp-val = IF fiLenWid:SCREEN-VALUE = "Length" THEN
                po-ordl.s-len
              ELSE
                po-ordl.s-wid.


       
    IF NOT choice THEN
      MESSAGE "Total panel size not equal to PO Line Item " +
              (fiLenWid:SCREEN-VALUE) + ", continue?" SKIP
              v-comp-val " VS. " v-tot-score[1]  SKIP
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE choice1.

    IF NOT choice AND choice1 THEN DO:
      MESSAGE "Update PO Line Item?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE choice.
      
      IF NOT choice THEN DO:
        APPLY "entry" TO val-1.
        RETURN NO-APPLY.
      END.

      FIND CURRENT po-ordl.
              
      /*IF v-xg-flag THEN po-ordl.s-len = v-tot-score[1].
                   ELSE*/ 

      IF fiLenWid:SCREEN-VALUE = "Length" THEN
        po-ordl.s-len = v-tot-score[1].
      ELSE
        po-ordl.s-wid = v-tot-score[1].

      FIND FIRST job
          WHERE job.company EQ po-ordl.company
            AND job.job-no  EQ po-ordl.job-no
            AND job.job-no2 EQ po-ordl.job-no2
          NO-LOCK NO-ERROR.
    
      IF AVAIL job THEN
      FOR EACH job-mat
          WHERE job-mat.company  EQ job.company
            AND job-mat.job      EQ job.job
            AND job-mat.job-no   EQ job.job-no
            AND job-mat.job-no2  EQ job.job-no2
            AND job-mat.frm      EQ po-ordl.s-num
            AND CAN-FIND(FIRST item
                         WHERE item.company  EQ job-mat.company
                           AND item.i-no     EQ job-mat.i-no
                           AND INDEX("BPRA1234",item.mat-type) GT 0):
        ASSIGN
         job-mat.wid = po-ordl.s-wid
         job-mat.len = po-ordl.s-len.
      END.

      FIND CURRENT po-ordl NO-LOCK.
          
      NEXT.
    END.
      
    LEAVE.
  END.

  FIND CURRENT po-ordl.
  IF AVAIL(po-ordl) AND po-ordl.spare-char-1 NE fiLenWid:SCREEN-VALUE THEN
    po-ordl.spare-char-1 = fiLenWid:SCREEN-VALUE.
  FIND CURRENT po-ordl NO-LOCK.
  IF AVAIL b-ref1 THEN
    FIND CURRENT b-ref1 NO-LOCK.
  IF AVAIL b-ref2 THEN
  FIND CURRENT b-ref2 NO-LOCK.

  APPLY "go" TO FRAME {&FRAME-NAME}.
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
  {cec/d-pan-1.i 14}
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
ON LEAVE OF val-1 IN FRAME Dialog-Frame /* 1st Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-10 Dialog-Frame
ON LEAVE OF val-10 IN FRAME Dialog-Frame /* 10th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-11 Dialog-Frame
ON LEAVE OF val-11 IN FRAME Dialog-Frame /* 11th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-12 Dialog-Frame
ON LEAVE OF val-12 IN FRAME Dialog-Frame /* 12th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-13 Dialog-Frame
ON LEAVE OF val-13 IN FRAME Dialog-Frame /* 13th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-14 Dialog-Frame
ON LEAVE OF val-14 IN FRAME Dialog-Frame /* 14th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-15 Dialog-Frame
ON LEAVE OF val-15 IN FRAME Dialog-Frame /* 15th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-16 Dialog-Frame
ON LEAVE OF val-16 IN FRAME Dialog-Frame /* 16th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-17 Dialog-Frame
ON LEAVE OF val-17 IN FRAME Dialog-Frame /* 17th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-18 Dialog-Frame
ON LEAVE OF val-18 IN FRAME Dialog-Frame /* 18th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-19 Dialog-Frame
ON LEAVE OF val-19 IN FRAME Dialog-Frame /* 19th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-2 Dialog-Frame
ON LEAVE OF val-2 IN FRAME Dialog-Frame /* 2nd Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-20 Dialog-Frame
ON LEAVE OF val-20 IN FRAME Dialog-Frame /* 20th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-3 Dialog-Frame
ON LEAVE OF val-3 IN FRAME Dialog-Frame /* 3rd Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-4 Dialog-Frame
ON LEAVE OF val-4 IN FRAME Dialog-Frame /* 4th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-5 Dialog-Frame
ON LEAVE OF val-5 IN FRAME Dialog-Frame /* 5th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-6 Dialog-Frame
ON LEAVE OF val-6 IN FRAME Dialog-Frame /* 6th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-7 Dialog-Frame
ON LEAVE OF val-7 IN FRAME Dialog-Frame /* 7th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-8 Dialog-Frame
ON LEAVE OF val-8 IN FRAME Dialog-Frame /* 8th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME val-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL val-9 Dialog-Frame
ON LEAVE OF val-9 IN FRAME Dialog-Frame /* 9th Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-panel (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
DEF VAR li AS INT NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.


/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  ASSIGN
   cocode = g_company
   locode = g_loc.

  FIND po-ordl WHERE ROWID(po-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL po-ordl THEN DO:
    IF po-ordl.spare-char-1 = "LENGTH" THEN
        ASSIGN fiLenWid = "Length"  btLenWid:LABEL = "Load Widths".
    ELSE
        ASSIGN fiLenWid = "Width" btLenWid:LABEL = "Load Lengths".
    DISP fiLenWid WITH FRAME {&FRAME-NAME}.

    IF v-cecscrn-char EQ "Decimal" THEN
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
        val-20:FORMAT = "->>,>>9.999999".

    {po/po-ordls.i}
  
    {po/poordls2W.i}

    ld = 0.
    IF AVAIL b-ref1 THEN
    DO li = 1 TO 12:
      ld = ld + b-ref1.val[li].
    END.
    IF AVAIL b-ref2 THEN
    DO li = 1 TO 8:
      ld = ld + b-ref2.val[li].
    END. 

    ll = NO.
    IF (AVAIL b-ref1 OR AVAIL b-ref2) AND ld EQ 0 THEN 
      MESSAGE "Scores total zero, do you wish to default from estimate?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.

    IF ll THEN DO:
      IF AVAIL b-ref1 THEN DELETE b-ref1.
      IF AVAIL b-ref2 THEN DELETE b-ref2.
    END.

    RUN po/po-ordls.p (RECID(po-ordl)).

    {po/po-ordls.i}

/*    END. */
  END.
END.
RUN enable_UI.
RUN initial-screen.
WAIT-FOR GO OF FRAME {&FRAME-NAME}.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
          val-20 type-20 dscr-20 fiLenWid 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-30 val-1 type-1 val-2 type-2 val-3 type-3 val-4 type-4 val-5 
         type-5 val-6 type-6 val-7 type-7 val-8 type-8 val-9 type-9 val-10 
         type-10 val-11 type-11 val-12 type-12 val-13 type-13 val-14 type-14 
         val-15 type-15 val-16 type-16 val-17 type-17 val-18 type-18 val-19 
         type-19 val-20 type-20 btLenWid btn_ok 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initial-screen Dialog-Frame 
PROCEDURE initial-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF AVAIL b-ref1 OR AVAIL b-ref2 THEN DO:
      IF AVAIL b-ref1 THEN
        ASSIGN
         lv-rowid1 = ROWID(b-ref1)

         type-1  = SUBSTR(b-ref1.dscr,1,1)
         type-2  = SUBSTR(b-ref1.dscr,2,1)
         type-3  = SUBSTR(b-ref1.dscr,3,1)
         type-4  = SUBSTR(b-ref1.dscr,4,1)
         type-5  = SUBSTR(b-ref1.dscr,5,1)
         type-6  = SUBSTR(b-ref1.dscr,6,1)
         type-7  = SUBSTR(b-ref1.dscr,7,1)
         type-8  = SUBSTR(b-ref1.dscr,8,1)
         type-9  = SUBSTR(b-ref1.dscr,9,1)
         type-10 = SUBSTR(b-ref1.dscr,10,1)
         type-11 = SUBSTR(b-ref1.dscr,11,1)
         type-12 = SUBSTR(b-ref1.dscr,12,1).

      IF AVAIL b-ref2 THEN
        ASSIGN
         lv-rowid2 = ROWID(b-ref2)

         type-13 = SUBSTR(b-ref2.dscr,1,1)
         type-14 = SUBSTR(b-ref2.dscr,2,1)
         type-15 = SUBSTR(b-ref2.dscr,2,1)
         type-16 = SUBSTR(b-ref2.dscr,4,1)
         type-17 = SUBSTR(b-ref2.dscr,5,1)
         type-18 = SUBSTR(b-ref2.dscr,6,1)
         type-19 = SUBSTR(b-ref2.dscr,7,1)
         type-20 = SUBSTR(b-ref2.dscr,8,1).

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

        IF AVAIL b-ref1 THEN
          ASSIGN
           val-1:SCREEN-VALUE  = STRING(b-ref1.val[1])
           val-2:SCREEN-VALUE  = STRING(b-ref1.val[2])
           val-3:SCREEN-VALUE  = STRING(b-ref1.val[3])
           val-4:SCREEN-VALUE  = STRING(b-ref1.val[4])
           val-5:SCREEN-VALUE  = STRING(b-ref1.val[5])
           val-6:SCREEN-VALUE  = STRING(b-ref1.val[6])
           val-7:SCREEN-VALUE  = STRING(b-ref1.val[7])
           val-8:SCREEN-VALUE  = STRING(b-ref1.val[8])
           val-9:SCREEN-VALUE  = STRING(b-ref1.val[9])
           val-10:SCREEN-VALUE = STRING(b-ref1.val[10])
           val-11:SCREEN-VALUE = STRING(b-ref1.val[11])
           val-12:SCREEN-VALUE = STRING(b-ref1.val[12]).

        IF AVAIL b-ref2 THEN
          ASSIGN
           val-13:SCREEN-VALUE = STRING(b-ref2.val[1])
           val-14:SCREEN-VALUE = STRING(b-ref2.val[2])
           val-15:SCREEN-VALUE = STRING(b-ref2.val[3])
           val-16:SCREEN-VALUE = STRING(b-ref2.val[4])
           val-17:SCREEN-VALUE = STRING(b-ref2.val[5])
           val-18:SCREEN-VALUE = STRING(b-ref2.val[6])
           val-19:SCREEN-VALUE = STRING(b-ref2.val[7])
           val-20:SCREEN-VALUE = STRING(b-ref2.val[8]).

      END.
    END.
      RELEASE b-ref1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-panel Dialog-Frame 
PROCEDURE valid-panel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS WIDGET-HANDLE NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(ip-focus:SCREEN-VALUE) - TRUNC(DEC(ip-focus:SCREEN-VALUE),0) GT
       ((li-16-32 - 1) / 100) THEN DO:
      MESSAGE "Right side of decimal cannot be greater than " +
              TRIM(STRING(li-16-32 - 1,">>")) + "..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

