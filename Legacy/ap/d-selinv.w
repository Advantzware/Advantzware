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
DEF INPUT param ip-vend-recid AS RECID .
DEF INPUT PARAM ip-due AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */
{ap/d-selinv.i}

DEF VAR lv-num-rec AS INT NO-UNDO.

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
{sys/inc/varasgn.i}

DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.
DEF VAR period_pos AS INTEGER NO-UNDO.

DEF VAR cellColumn AS HANDLE NO-UNDO EXTENT 20.
DEF VAR columnCount AS INT NO-UNDO.


IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("NOSWEAT") + "..".
ELSE
ASSIGN
  period_pos = INDEX(PROGRAM-NAME(1),".")
  v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-inv ap-inv

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ap-inv.inv-no ap-inv.inv-date ap-inv.net ap-inv.paid ap-inv.due   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-inv USE-INDEX tt-key, ~
                FIRST ap-inv WHERE RECID(ap-inv) EQ tt-inv.rec-id NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME}      FOR EACH tt-inv USE-INDEX tt-key, ~
                FIRST ap-inv WHERE RECID(ap-inv) EQ tt-inv.rec-id NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-inv ap-inv
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-inv
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-2 ap-inv


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 BROWSE-2 rd-sort Btn_OK Btn_Select ~
Btn_Deselect Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS rd-sort 

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

DEFINE BUTTON Btn_Deselect 
     LABEL "Unselect All" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Select 
     LABEL "Select All" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE rd-sort AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Invoice#", 1,
"Invoice Date", 2
     SIZE 48 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-inv, 
      ap-inv SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 DISPLAY
      ap-inv.inv-no     LABEL "Invoice#"
      ap-inv.inv-date   LABEL "Inv Date"
      ap-inv.net        LABEL "Net"
      ap-inv.paid       LABEL "Amt Paid"
      ap-inv.due        LABEL "Balance Due"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 75 BY 12.38
         BGCOLOR 8  FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-2 AT ROW 1.24 COL 2
     rd-sort AT ROW 14.1 COL 23 NO-LABEL
     Btn_OK AT ROW 15.76 COL 7
     Btn_Select AT ROW 15.76 COL 24
     Btn_Deselect AT ROW 15.76 COL 41
     Btn_Cancel AT ROW 15.76 COL 58
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 14.1 COL 13
     RECT-1 AT ROW 13.86 COL 2
     SPACE(0.59) SKIP(1.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "AP Invoice Selection"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 RECT-1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
     FOR EACH tt-inv USE-INDEX tt-key,
         FIRST ap-inv WHERE RECID(ap-inv) EQ tt-inv.rec-id NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* AP Invoice Selection */
DO:
  APPLY "choose" TO Btn_OK.

  APPLY "go" TO FRAME {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 D-Dialog
ON ROW-DISPLAY OF BROWSE-2 IN FRAME D-Dialog
DO:
  DEF VAR li AS INT NO-UNDO.
  
  IF AVAIL ap-inv AND ap-inv.stat EQ "H" THEN
      ap-inv.inv-no:BGCOLOR IN BROWSE {&browse-name} = 11.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Deselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deselect D-Dialog
ON CHOOSE OF Btn_Deselect IN FRAME D-Dialog /* Unselect All */
DO:
  {&browse-name}:DESELECT-ROWS ().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
  DEF VAR li AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR lCheckMsg AS LOG NO-UNDO.
  IF {&browse-name}:NUM-SELECTED-ROWS GT 0 THEN DO:
      
      DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
        {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.
         IF AVAIL ap-inv AND ap-inv.stat EQ "H" THEN DO:
             MESSAGE "Invoice is on hold - Are you sure you want it paid?"
                 VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll .
             lCheckMsg = YES.
             LEAVE.
         END.
      END.

      IF not(lCheckMsg) AND AVAIL ap-inv AND ap-inv.stat NE "H" THEN DO:
             MESSAGE "This will select all highlighted Invoices, " SKIP
                 "do you want to continue?" VIEW-AS ALERT-BOX QUESTION
                 BUTTON YES-NO UPDATE ll .
             
      END.
      
    IF ll THEN DO:
      DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
        {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.

        IF AVAIL tt-inv THEN tt-inv.selekt = YES.
      END.

      APPLY "go" TO FRAME {&FRAME-NAME}.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Select D-Dialog
ON CHOOSE OF Btn_Select IN FRAME D-Dialog /* Select All */
DO:
  {&browse-name}:SELECT-ALL ().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sort D-Dialog
ON VALUE-CHANGED OF rd-sort IN FRAME D-Dialog
DO: 
  RUN set-up-sort.

  DO TRANSACTION:
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  END.   

  APPLY "entry" TO {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
/*ON 'mouse-select-click':U OF tt-inv.selekt IN BROWSE {&browse-name} 
DO:
    IF SELF:SCREEN-VALUE = "Yes" THEN SELF:SCREEN-VALUE = "No".
    ELSE SELF:SCREEN-VALUE = "Yes".
    RETURN NO-APPLY.
END.
ON 'leave':U OF tt-inv.selekt IN BROWSE {&browse-name}
DO:
    ASSIGN tt-inv.selekt = tt-inv.selekt:SCREEN-VALUE IN BROWSE {&browse-name} = "Yes".
    RETURN.
END.*/

RUN build-table.

IF lv-num-rec GT 0 THEN DO: 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table D-Dialog 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i AS INT NO-UNDO.
  DEF VAR choice AS LOG NO-UNDO.

  
  FOR EACH tt-inv:
    DELETE tt-inv.
  END.

  FIND vend WHERE RECID(vend) EQ ip-vend-recid NO-LOCK.

  IF ip-due THEN
     FOR EACH ap-inv
         WHERE ap-inv.company EQ cocode 
           AND ap-inv.vend-no EQ vend.vend-no
           AND ap-inv.due     NE 0
           AND ap-inv.posted  EQ YES
         NO-LOCK
         BY ap-inv.inv-no:
       CREATE tt-inv.
       ASSIGN tt-inv.rec-id = RECID(ap-inv).
       lv-num-rec = lv-num-rec + 1.              
     END.
  ELSE
     FOR EACH ap-inv
         WHERE ap-inv.company EQ cocode 
           AND ap-inv.vend-no EQ vend.vend-no
           AND ap-inv.posted  EQ YES
         NO-LOCK
         BY ap-inv.inv-no:
       CREATE tt-inv.
       ASSIGN tt-inv.rec-id = RECID(ap-inv).
       lv-num-rec = lv-num-rec + 1.              
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
  DISPLAY rd-sort 
      WITH FRAME D-Dialog.
  ENABLE RECT-1 BROWSE-2 rd-sort Btn_OK Btn_Select Btn_Deselect Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    IF rd-sort:SCREEN-VALUE EQ ? THEN rd-sort:SCREEN-VALUE = '1'.
    RUN set-up-sort.
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
  {src/adm/template/snd-list.i "tt-inv"}
  {src/adm/template/snd-list.i "ap-inv"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-up-sort D-Dialog 
PROCEDURE set-up-sort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN rd-sort.
    FOR EACH tt-inv,
        FIRST ap-inv WHERE RECID(ap-inv) EQ tt-inv.rec-id NO-LOCK:
      tt-key = ap-inv.inv-no.
      IF rd-sort EQ 2 THEN
        tt-key = STRING(YEAR(ap-inv.inv-date),"9999") +
                 STRING(MONTH(ap-inv.inv-date),"99")  +
                 STRING(DAY(ap-inv.inv-date),"99")    +
                 tt-key.
    END.
    {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
  END.

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

