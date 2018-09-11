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

DEFINE VARIABLE cScoreType AS CHARACTER NO-UNDO.
DEF VAR lv-rowid1 AS ROWID NO-UNDO.
DEF VAR lv-rowid2 AS ROWID NO-UNDO.
def var k_frac as dec init "6.25" no-undo.
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
type-14 val-14 type-15 val-15 val-16 type-16 type-17 val-17 type-18 val-18 ~
val-19 type-19 type-20 val-20 btLenWid btn_ok 
&Scoped-Define DISPLAYED-OBJECTS val-1 type-1 val-2 type-2 val-3 type-3 ~
val-4 type-4 val-5 type-5 val-6 type-6 val-7 type-7 val-8 type-8 val-9 ~
type-9 val-10 type-10 val-11 type-11 val-12 type-12 val-13 type-13 type-14 ~
val-14 type-15 val-15 val-16 type-16 type-17 val-17 type-18 val-18 val-19 ~
type-19 type-20 val-20 fiLenWid 

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

DEFINE VARIABLE type-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-11 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-12 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-13 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-14 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-15 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-16 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-17 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-18 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-19 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-20 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE type-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE fiLenWid AS CHARACTER FORMAT "X(256)":U 
     LABEL "Showing" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

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
     SIZE 89 BY 25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     val-1 AT ROW 2.19 COL 14 COLON-ALIGNED
     type-1 AT ROW 2.19 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     val-2 AT ROW 3.38 COL 14 COLON-ALIGNED
     type-2 AT ROW 3.38 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     val-3 AT ROW 4.57 COL 14 COLON-ALIGNED
     type-3 AT ROW 4.57 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     val-4 AT ROW 5.76 COL 14 COLON-ALIGNED
     type-4 AT ROW 5.76 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     val-5 AT ROW 6.95 COL 14 COLON-ALIGNED
     type-5 AT ROW 6.95 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     val-6 AT ROW 8.14 COL 14 COLON-ALIGNED
     type-6 AT ROW 8.14 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     val-7 AT ROW 9.33 COL 14 COLON-ALIGNED
     type-7 AT ROW 9.33 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     val-8 AT ROW 10.52 COL 14 COLON-ALIGNED
     type-8 AT ROW 10.52 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     val-9 AT ROW 11.71 COL 14 COLON-ALIGNED
     type-9 AT ROW 11.71 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     val-10 AT ROW 12.91 COL 14 COLON-ALIGNED
     type-10 AT ROW 12.91 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     val-11 AT ROW 14.1 COL 14 COLON-ALIGNED
     type-11 AT ROW 14.1 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     val-12 AT ROW 15.29 COL 14 COLON-ALIGNED
     type-12 AT ROW 15.29 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     val-13 AT ROW 16.48 COL 14 COLON-ALIGNED
     type-13 AT ROW 16.48 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     val-14 AT ROW 17.67 COL 14 COLON-ALIGNED
     type-14 AT ROW 17.67 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     val-15 AT ROW 18.95 COL 14 COLON-ALIGNED
     type-15 AT ROW 18.95 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     val-16 AT ROW 20.05 COL 14 COLON-ALIGNED
     type-16 AT ROW 20.05 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     val-17 AT ROW 21.24 COL 14 COLON-ALIGNED
     type-17 AT ROW 21.24 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     val-18 AT ROW 22.43 COL 14 COLON-ALIGNED
     type-18 AT ROW 22.43 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     val-19 AT ROW 23.62 COL 14 COLON-ALIGNED
     type-19 AT ROW 23.62 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     val-20 AT ROW 24.81 COL 14 COLON-ALIGNED
     type-20 AT ROW 24.81 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     fiLenWid AT ROW 26.24 COL 10.4 COLON-ALIGNED WIDGET-ID 4
     btLenWid AT ROW 26.24 COL 34 WIDGET-ID 2
     btn_ok AT ROW 26.24 COL 53
     "Type" VIEW-AS TEXT
          SIZE 6 BY 1 AT ROW 1 COL 34
     "Panel Size" VIEW-AS TEXT
          SIZE 16 BY 1 AT ROW 1 COL 16
     "Description" VIEW-AS TEXT
          SIZE 45 BY 1 AT ROW 1 COL 43
     RECT-30 AT ROW 1 COL 1
     SPACE(0.19) SKIP(1.47)
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
  
    {po/poordls2W.i}

    IF fiLenWid:SCREEN-VALUE = "Width" THEN
      RUN po/po-ordlw.p (RECID(po-ordl), "Length").
    ELSE
      RUN po/po-ordls.p (RECID(po-ordl)).

    {po/poordls2W.i}

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

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
    {po/poordls2W.i}
       RUN po/poaddscores.p (INPUT ROWID(po-ordl)).
       {po/poordls2W.i}
    
 FIND CURRENT po-ordl.        
  DO WHILE TRUE:
    ASSIGN
       po-ordl.scorePanels[1]  = val-1
       po-ordl.scorePanels[2]  = val-2
       po-ordl.scorePanels[3]  = val-3
       po-ordl.scorePanels[4]  = val-4
       po-ordl.scorePanels[5]  = val-5
       po-ordl.scorePanels[6]  = val-6
       po-ordl.scorePanels[7]  = val-7
       po-ordl.scorePanels[8]  = val-8
       po-ordl.scorePanels[9]  = val-9
       po-ordl.scorePanels[10] = val-10
       po-ordl.scorePanels[11] = val-11
       po-ordl.scorePanels[12] = val-12
       po-ordl.scorePanels[13] = val-13
       po-ordl.scorePanels[14] = val-14
       po-ordl.scorePanels[15] = val-15
       po-ordl.scorePanels[16] = val-16
       po-ordl.scorePanels[17] = val-17
       po-ordl.scorePanels[18] = val-18
       po-ordl.scorePanels[19] = val-19
       po-ordl.scorePanels[20] = val-20

       po-ordl.scoreType[1] = STRING(type-1,"X")
       po-ordl.scoreType[2] = STRING(type-2,"X")
       po-ordl.scoreType[3] = STRING(type-3,"X")
       po-ordl.scoreType[4] = STRING(type-4,"X")
       po-ordl.scoreType[5] = STRING(type-5,"X")
       po-ordl.scoreType[6] = STRING(type-6,"X")
       po-ordl.scoreType[7] = STRING(type-7,"X")
       po-ordl.scoreType[8] = STRING(type-8,"X")
       po-ordl.scoreType[9] = STRING(type-9,"X")
       po-ordl.scoreType[10] = STRING(type-10,"X")
       po-ordl.scoreType[11] = STRING(type-11,"X")
       po-ordl.scoreType[12] = STRING(type-12,"X")
       po-ordl.scoreType[13] = STRING(type-11,"X")
       po-ordl.scoreType[14] = STRING(type-12,"X")
       po-ordl.scoreType[15] = STRING(type-11,"X")
       po-ordl.scoreType[16] = STRING(type-12,"X")
       po-ordl.scoreType[17] = STRING(type-11,"X")
       po-ordl.scoreType[18] = STRING(type-12,"X")
       po-ordl.scoreType[19] = STRING(type-11,"X")
       po-ordl.scoreType[20] = STRING(type-12,"X").
    
    
    
      


    {po/poordls2W.i}
    {po/poordls3W.i}
    
    v-tot-score = 0.


    DO i = 1 TO 20:
      ASSIGN 
       v-tot-score[1] = v-tot-score[1] + TRUNC(po-ordl.scorePanels[i],0)
       v-tot-score[2] = v-tot-score[2] +
                        (po-ordl.scorePanels[i] - TRUNC(po-ordl.scorePanels[i],0)).
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


  APPLY "go" TO FRAME {&FRAME-NAME}.
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

  FOR EACH scoreType NO-LOCK 
      WHERE scoreType.company EQ cocode
      :
      cScoreType = cScoreType
                 + scoreType.scoreType + " - "
                 + scoreType.description + ","
                 + scoreType.scoreType + ","
                 .
  END. /* each scoretype */
  ASSIGN 
    cScoreType = ",," + TRIM(cScoreType,",")
    type-1:LIST-ITEM-PAIRS = cScoreType
    type-2:LIST-ITEM-PAIRS = cScoreType
    type-3:LIST-ITEM-PAIRS = cScoreType
    type-4:LIST-ITEM-PAIRS = cScoreType
    type-5:LIST-ITEM-PAIRS = cScoreType
    type-6:LIST-ITEM-PAIRS = cScoreType
    type-7:LIST-ITEM-PAIRS = cScoreType
    type-8:LIST-ITEM-PAIRS = cScoreType
    type-9:LIST-ITEM-PAIRS = cScoreType
    type-10:LIST-ITEM-PAIRS = cScoreType
    type-11:LIST-ITEM-PAIRS = cScoreType
    type-12:LIST-ITEM-PAIRS = cScoreType
    type-13:LIST-ITEM-PAIRS = cScoreType
    type-14:LIST-ITEM-PAIRS = cScoreType
    type-15:LIST-ITEM-PAIRS = cScoreType
    type-16:LIST-ITEM-PAIRS = cScoreType
    type-17:LIST-ITEM-PAIRS = cScoreType
    type-18:LIST-ITEM-PAIRS = cScoreType
    type-19:LIST-ITEM-PAIRS = cScoreType
    type-20:LIST-ITEM-PAIRS = cScoreType
    .
  
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

  
    {po/poordls2W.i}

    ld = 0.

    DO li = 1 TO 20:
      ld = ld + po-ordl.scorePanels[li].
    END.


    ll = NO.
    IF ld EQ 0 THEN
      MESSAGE "Scores total zero, do you wish to default from estimate?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.

    IF ll THEN DO:
    END.

    RUN po/po-ordls.p (RECID(po-ordl)).
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
  DISPLAY val-1 type-1 val-2 type-2 val-3 type-3 val-4 type-4 val-5 type-5 val-6 
          type-6 val-7 type-7 val-8 type-8 val-9 type-9 val-10 type-10 val-11 
          type-11 val-12 type-12 val-13 type-13 type-14 val-14 type-15 val-15 
          val-16 type-16 type-17 val-17 type-18 val-18 val-19 type-19 type-20 
          val-20 fiLenWid 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-30 val-1 type-1 val-2 type-2 val-3 type-3 val-4 type-4 val-5 
         type-5 val-6 type-6 val-7 type-7 val-8 type-8 val-9 type-9 val-10 
         type-10 val-11 type-11 val-12 type-12 val-13 type-13 type-14 val-14 
         type-15 val-15 val-16 type-16 type-17 val-17 type-18 val-18 val-19 
         type-19 type-20 val-20 btLenWid btn_ok 
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


    IF AVAIL po-ordl THEN DO:
        ASSIGN
/*         lv-rowid1 = ROWID(po-ordl)*/

         type-1  = SUBSTR(po-ordl.scoreType[1],1)
         type-2  = SUBSTR(po-ordl.scoreType[2],1)
         type-3  = SUBSTR(po-ordl.scoreType[3],1)
         type-4  = SUBSTR(po-ordl.scoreType[4],1)
         type-5  = SUBSTR(po-ordl.scoreType[5],1)
         type-6  = SUBSTR(po-ordl.scoreType[6],1)
         type-7  = SUBSTR(po-ordl.scoreType[7],1)
         type-8  = SUBSTR(po-ordl.scoreType[8],1)
         type-9  = SUBSTR(po-ordl.scoreType[9],1)
         type-10 = SUBSTR(po-ordl.scoreType[10],1)
         type-11 = SUBSTR(po-ordl.scoreType[11],1)
         type-12 = SUBSTR(po-ordl.scoreType[12],1)
         type-13 = SUBSTR(po-ordl.scoreType[13],1)
         type-14 = SUBSTR(po-ordl.scoreType[14],1)
         type-15 = SUBSTR(po-ordl.scoreType[15],1)
         type-16 = SUBSTR(po-ordl.scoreType[16],1)
         type-17 = SUBSTR(po-ordl.scoreType[17],1)
         type-18 = SUBSTR(po-ordl.scoreType[18],1)
         type-19 = SUBSTR(po-ordl.scoreType[19],1)
         type-20 = SUBSTR(po-ordl.scoreType[20],1).


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


        ASSIGN
           val-1:SCREEN-VALUE  = STRING(po-ordl.scorePanels[1])
           val-2:SCREEN-VALUE  = STRING(po-ordl.scorePanels[2])
           val-3:SCREEN-VALUE  = STRING(po-ordl.scorePanels[3])
           val-4:SCREEN-VALUE  = STRING(po-ordl.scorePanels[4])
           val-5:SCREEN-VALUE  = STRING(po-ordl.scorePanels[5])
           val-6:SCREEN-VALUE  = STRING(po-ordl.scorePanels[6])
           val-7:SCREEN-VALUE  = STRING(po-ordl.scorePanels[7])
           val-8:SCREEN-VALUE  = STRING(po-ordl.scorePanels[8])
           val-9:SCREEN-VALUE  = STRING(po-ordl.scorePanels[9])
           val-10:SCREEN-VALUE = STRING(po-ordl.scorePanels[10])
           val-11:SCREEN-VALUE = STRING(po-ordl.scorePanels[11])
           val-12:SCREEN-VALUE = STRING(po-ordl.scorePanels[12])
           val-13:SCREEN-VALUE = STRING(po-ordl.scorePanels[13])
           val-14:SCREEN-VALUE = STRING(po-ordl.scorePanels[14])
           val-15:SCREEN-VALUE = STRING(po-ordl.scorePanels[15])
           val-16:SCREEN-VALUE = STRING(po-ordl.scorePanels[16])
           val-17:SCREEN-VALUE = STRING(po-ordl.scorePanels[17])
           val-18:SCREEN-VALUE = STRING(po-ordl.scorePanels[18])
           val-19:SCREEN-VALUE = STRING(po-ordl.scorePanels[19])
           val-20:SCREEN-VALUE = STRING(po-ordl.scorePanels[20]).
      END.
    END.

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

