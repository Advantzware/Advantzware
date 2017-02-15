&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: est-vend.p
  
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

/* Local Variable Definitions ---                                       */
def input parameter v-recid as recid no-undo.
def output parameter op-vend-no as cha no-undo.
def output parameter op-error as log no-undo.

{sys/inc/var.i shared}

DEF TEMP-TABLE tt-report LIKE report.

def var ls-vend-name as cha form "x(30)" no-undo.
def var lv-term-id   like tt-report.term-id format "x(20)" no-undo.
DEF VAR v-board-cost-not-zero AS LOG NO-UNDO.
DEF VAR v-continue AS LOG NO-UNDO.

DO TRANSACTION:
   {sys\inc\ceboard.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-report

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-report.key-01 tt-report.key-02 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-report NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH tt-report NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-report
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-report


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 Btn_OK Btn_Best Btn_Cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Best 
     LABEL "Best Price" 
     SIZE 17 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-report SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 D-Dialog _STRUCTURED
  QUERY BROWSE-1 DISPLAY
      tt-report.key-01 COLUMN-LABEL "Vendor"
      tt-report.key-02 COLUMN-LABEL "Name" FORMAT "x(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 80 BY 6.91
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-1 AT ROW 1.24 COL 3
     Btn_OK AT ROW 8.86 COL 9
     Btn_Best AT ROW 8.86 COL 35
     Btn_Cancel AT ROW 8.86 COL 62
     SPACE(6.59) SKIP(0.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 FONT 0
         TITLE "Board Vendor Selection"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
/* BROWSE-TAB BROWSE-1 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "ASI.tt-report"
     _FldNameList[1]   > ASI.tt-report.key-01
"tt-report.key-01" "Vendor" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > ASI.tt-report.key-02
"tt-report.key-02" "Name" "x(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON GO OF FRAME D-Dialog /* Board Vendor Selection */
DO:
  IF AVAIL tt-report THEN op-vend-no = tt-report.key-01.
  
  RUN clean-up.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Board Vendor Selection */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  RUN clean-up.

  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Best
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Best D-Dialog
ON CHOOSE OF Btn_Best IN FRAME D-Dialog /* Best Price */
DO:
  op-vend-no = "bestvendor".

  RUN clean-up.

  IF ceboard-log AND v-board-cost-not-zero AND op-vend-no NE "" THEN
  DO:
     MESSAGE "Special Board Cost was Entered on Layout Folder" SKIP
             "VENDOR Cost will override Special Board Cost.  Continue?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE v-continue.

     IF NOT v-continue THEN
        APPLY "CHOOSE" TO btn_cancel IN FRAME {&FRAME-NAME}.
  END.

  APPLY "go" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:   
  op-error = YES.

  RUN clean-up.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
  op-vend-no = tt-report.key-01.
  
  RUN clean-up.
  
  IF ceboard-log AND v-board-cost-not-zero AND op-vend-no NE "" THEN
  DO:
     MESSAGE "Special Board Cost was Entered on Layout Folder" SKIP
             "VENDOR Cost will override Special Board Cost.  Continue?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE v-continue.

     IF NOT v-continue THEN
        APPLY "CHOOSE" TO btn_cancel IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

RUN build-tt-report.

IF CAN-FIND(FIRST tt-report) THEN DO:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-tt-report D-Dialog 
PROCEDURE build-tt-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* --------------------------------------------- sys/look/estbvnd.p 05/01 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF VAR v-forms AS INT EXTENT 2 NO-UNDO.
DEF VAR li AS INT NO-UNDO.


FIND est WHERE RECID(est) EQ v-recid NO-LOCK NO-ERROR.

IF NOT AVAIL est                        OR
   NOT CAN-FIND(FIRST eb OF est
                WHERE eb.form-no GT 0
                  AND eb.pur-man EQ NO) THEN LEAVE.

FOR EACH ef
    WHERE ef.company EQ est.company
      AND ef.est-no  EQ est.est-no
      AND CAN-FIND(FIRST eb OF ef WHERE NOT eb.pur-man)
    NO-LOCK:

  v-forms[1] = v-forms[1] + 1.

  IF ef.cost-msh NE 0 THEN
     v-board-cost-not-zero = YES.

  FOR EACH e-item-vend
      WHERE e-item-vend.company EQ ef.company
        AND e-item-vend.i-no    EQ ef.board
        AND e-item-vend.vend-no NE ""         
        AND ef.gsh-wid          GE e-item-vend.roll-w[27]
        AND ef.gsh-wid          LE e-item-vend.roll-w[28]
        AND ef.gsh-len          GE e-item-vend.roll-w[29]
        AND ef.gsh-len          LE e-item-vend.roll-w[30]
      NO-LOCK:

    FIND FIRST vend
        WHERE vend.company EQ e-item-vend.company
          AND vend.vend-no EQ e-item-vend.vend-no
        NO-LOCK NO-ERROR.

    CREATE tt-report.
    ASSIGN
     tt-report.key-01  = e-item-vend.vend-no
     tt-report.key-02  = IF AVAIL vend THEN vend.name ELSE ""
     tt-report.rec-id  = RECID(e-item-vend).
  END.
END.

FOR EACH tt-report BREAK BY tt-report.key-01:

  IF FIRST-OF(tt-report.key-01) THEN v-forms[2] = 0.

  v-forms[2] = v-forms[2] + 1.

  IF NOT LAST-OF(tt-report.key-01) OR v-forms[1] NE v-forms[2] THEN
    DELETE tt-report.
END.

adder-blok:
FOR EACH tt-report.

  FOR EACH ef
      WHERE ef.company EQ est.company
        AND ef.est-no  EQ est.est-no
        AND CAN-FIND(FIRST eb OF ef WHERE NOT eb.pur-man)
      NO-LOCK:
    DO li = 1 TO 6:
      IF ef.adder[li] NE ""                                          AND
         NOT CAN-FIND(FIRST e-item-vend
                      WHERE e-item-vend.company EQ cocode
                        AND e-item-vend.i-no    EQ ef.adder[li]
                        AND e-item-vend.vend-no EQ tt-report.key-01) THEN DO:
        DELETE tt-report.
        NEXT adder-blok.
      END.
    END.
  END.
END.

CREATE tt-report.

/* end ---------------------------------- copr. 2001  advanced software, inc. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clean-up D-Dialog 
PROCEDURE clean-up :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH tt-report:
    DELETE tt-report.
  END.

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
  ENABLE BROWSE-1 Btn_OK Btn_Best Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
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
  {src/adm/template/snd-list.i "tt-report"}

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

