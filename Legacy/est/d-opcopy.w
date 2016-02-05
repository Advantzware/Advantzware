&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER ip-rowid as ROWID.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared}

DEF BUFFER b-est-op FOR est-op.

&SCOPED-DEFINE key-phrase (b-est-op.company EQ est-op.company AND b-est-op.est-no EQ est-op.est-no AND b-est-op.m-code  EQ est-op.m-code AND ROWID(b-est-op) NE ROWID(est-op))

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES est-op

/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define FIELDS-IN-QUERY-D-Dialog est-op.m-code est-op.m-dscr ~
est-op.op-mr est-op.op-waste est-op.op-crew[1] est-op.op-speed ~
est-op.op-spoil est-op.op-crew[2] est-op.plates est-op.fountains 
&Scoped-define QUERY-STRING-D-Dialog FOR EACH est-op SHARE-LOCK
&Scoped-define OPEN-QUERY-D-Dialog OPEN QUERY D-Dialog FOR EACH est-op SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-D-Dialog est-op
&Scoped-define FIRST-TABLE-IN-QUERY-D-Dialog est-op


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_s-num fi_b-num tb_01 tb_02 tb_03 tb_04 ~
tb_05 tb_06 tb_07 tb_08 Btn_OK Btn_Cancel RECT-17 
&Scoped-Define DISPLAYED-FIELDS est-op.m-code est-op.m-dscr est-op.op-mr ~
est-op.op-waste est-op.op-crew[1] est-op.op-speed est-op.op-spoil ~
est-op.op-crew[2] est-op.plates est-op.fountains 
&Scoped-define DISPLAYED-TABLES est-op
&Scoped-define FIRST-DISPLAYED-TABLE est-op
&Scoped-Define DISPLAYED-OBJECTS fi_s-num fi_b-num tb_01 tb_02 tb_03 tb_04 ~
tb_05 tb_06 tb_07 tb_08 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi_b-num AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Up To Blank" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE fi_s-num AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Up To Form" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 90 BY 14.05.

DEFINE VARIABLE tb_01 AS LOGICAL INITIAL yes 
     LABEL "MR Hrs" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_02 AS LOGICAL INITIAL yes 
     LABEL "MR Waste" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_03 AS LOGICAL INITIAL yes 
     LABEL "MR Crew" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_04 AS LOGICAL INITIAL yes 
     LABEL "Run Speed" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_05 AS LOGICAL INITIAL yes 
     LABEL "Run Spoil%" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_06 AS LOGICAL INITIAL yes 
     LABEL "Run Crew" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_07 AS LOGICAL INITIAL yes 
     LABEL "Plate Changes" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_08 AS LOGICAL INITIAL yes 
     LABEL "Fountain Changes" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY D-Dialog FOR 
      est-op SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     est-op.m-code AT ROW 1.48 COL 23 COLON-ALIGNED
          LABEL "Machine"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     est-op.m-dscr AT ROW 1.48 COL 35 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 46 BY 1
     fi_s-num AT ROW 2.67 COL 23 COLON-ALIGNED
     fi_b-num AT ROW 2.67 COL 57 COLON-ALIGNED
     tb_01 AT ROW 5.05 COL 28
     est-op.op-mr AT ROW 5.05 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     tb_02 AT ROW 6.24 COL 28
     est-op.op-waste AT ROW 6.24 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     tb_03 AT ROW 7.43 COL 28
     est-op.op-crew[1] AT ROW 7.43 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     tb_04 AT ROW 8.62 COL 28
     est-op.op-speed AT ROW 8.62 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     tb_05 AT ROW 9.81 COL 28
     est-op.op-spoil AT ROW 9.81 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     tb_06 AT ROW 11 COL 28
     est-op.op-crew[2] AT ROW 11 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     tb_07 AT ROW 12.19 COL 28
     est-op.plates AT ROW 12.19 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     tb_08 AT ROW 13.38 COL 28
     est-op.fountains AT ROW 13.38 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     Btn_OK AT ROW 16.24 COL 21
     Btn_Cancel AT ROW 16.24 COL 56
     RECT-17 AT ROW 1 COL 1
     "Copy which fields?" VIEW-AS TEXT
          SIZE 26 BY 1 AT ROW 4.1 COL 34
          FONT 6
     SPACE(31.19) SKIP(13.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Copy Fields To Same Machine On Other Forms/Blanks"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN est-op.fountains IN FRAME D-Dialog
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN est-op.m-code IN FRAME D-Dialog
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN est-op.m-dscr IN FRAME D-Dialog
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN est-op.op-crew[1] IN FRAME D-Dialog
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN est-op.op-crew[2] IN FRAME D-Dialog
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN est-op.op-mr IN FRAME D-Dialog
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN est-op.op-speed IN FRAME D-Dialog
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN est-op.op-spoil IN FRAME D-Dialog
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN est-op.op-waste IN FRAME D-Dialog
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN est-op.plates IN FRAME D-Dialog
   NO-ENABLE EXP-LABEL                                                  */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _TblList          = "ASI.est-op"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Copy Fields To Same Machine On Other Forms/Blanks */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
  DEF VAR v-process AS LOG NO-UNDO.

  
  v-process = NO.

  MESSAGE "Are you sure you want to copy using these parameters?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.
        
  IF v-process THEN DO:
    run copy-records.

    MESSAGE "Copy Process Is Completed" VIEW-AS ALERT-BOX.

    APPLY "close" TO THIS-PROCEDURE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_b-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_b-num D-Dialog
ON LEAVE OF fi_b-num IN FRAME D-Dialog /* Up To Blank */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_s-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_s-num D-Dialog
ON LEAVE OF fi_s-num IN FRAME D-Dialog /* Up To Form */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_01
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_01 D-Dialog
ON VALUE-CHANGED OF tb_01 IN FRAME D-Dialog /* MR Hrs */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_02
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_02 D-Dialog
ON VALUE-CHANGED OF tb_02 IN FRAME D-Dialog /* MR Waste */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_03
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_03 D-Dialog
ON VALUE-CHANGED OF tb_03 IN FRAME D-Dialog /* MR Crew */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_04
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_04 D-Dialog
ON VALUE-CHANGED OF tb_04 IN FRAME D-Dialog /* Run Speed */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_05
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_05 D-Dialog
ON VALUE-CHANGED OF tb_05 IN FRAME D-Dialog /* Run Spoil% */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_06
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_06 D-Dialog
ON VALUE-CHANGED OF tb_06 IN FRAME D-Dialog /* Run Crew */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_07
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_07 D-Dialog
ON VALUE-CHANGED OF tb_07 IN FRAME D-Dialog /* Plate Changes */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_08
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_08 D-Dialog
ON VALUE-CHANGED OF tb_08 IN FRAME D-Dialog /* Fountain Changes */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
FIND est-op WHERE ROWID(est-op) EQ ip-rowid NO-LOCK NO-ERROR.
  
IF AVAIL est-op THEN
FIND FIRST b-est-op
    WHERE {&key-phrase}
    NO-LOCK NO-ERROR.

IF AVAIL b-est-op THEN DO:
  ASSIGN
   cocode = g_company
   locode = g_loc.

  FIND FIRST est
      WHERE est.company EQ est-op.company
        AND est.est-no  EQ est-op.est-no
      NO-LOCK.

  {src/adm/template/dialogmn.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-records D-Dialog 
PROCEDURE copy-records :
/* --------------------------------------------------- ce/op-copy.i 06/98 JLF */
/* Routing Copy                                                               */
/* -------------------------------------------------------------------------- */

SESSION:SET-WAIT-STATE ("general").

FOR EACH b-est-op
    WHERE {&key-phrase}
      AND b-est-op.s-num   GE est-op.s-num
      AND b-est-op.s-num   LE fi_s-num
      AND ((b-est-op.b-num GE est-op.b-num and
            b-est-op.b-num LE fi_b-num) or b-est-op.s-num gt est-op.s-num):
        
  IF tb_01 THEN b-est-op.op-mr      = est-op.op-mr.
  IF tb_02 THEN b-est-op.op-waste   = est-op.op-waste.
  IF tb_03 THEN b-est-op.op-crew[1] = est-op.op-crew[1].
  IF tb_04 THEN b-est-op.op-speed   = est-op.op-speed.
  IF tb_05 THEN b-est-op.op-spoil   = est-op.op-spoil.
  IF tb_06 THEN b-est-op.op-crew[2] = est-op.op-crew[2].
  IF tb_07 THEN b-est-op.plates     = est-op.plates.
  IF tb_08 THEN b-est-op.fountains  = est-op.fountains.
END.

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY fi_s-num fi_b-num tb_01 tb_02 tb_03 tb_04 tb_05 tb_06 tb_07 tb_08 
      WITH FRAME D-Dialog.
  IF AVAILABLE est-op THEN 
    DISPLAY est-op.m-code est-op.m-dscr est-op.op-mr est-op.op-waste 
          est-op.op-crew[1] est-op.op-speed est-op.op-spoil est-op.op-crew[2] 
          est-op.plates est-op.fountains 
      WITH FRAME D-Dialog.
  ENABLE fi_s-num fi_b-num tb_01 tb_02 tb_03 tb_04 tb_05 tb_06 tb_07 tb_08 
         Btn_OK Btn_Cancel RECT-17 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable D-Dialog 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST b-est-op
        WHERE {&key-phrase}
          AND b-est-op.s-num GT est-op.s-num
        NO-LOCK NO-ERROR.

    IF NOT AVAIL b-est-op THEN DISABLE fi_s-num.
    IF est-op.op-sb       THEN DISABLE fi_b-num.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "est-op"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

