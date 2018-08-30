&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
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
DEF INPUT PARAM ip-inv-no LIKE ar-cashl.inv-no NO-UNDO.
DEF INPUT PARAM ip-cash-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF BUFFER b-cashl FOR ar-cashl.

DEF VAR lv-num-rec AS INT NO-UNDO.

DEF TEMP-TABLE tt-inv FIELD selekt AS LOG LABEL "Selected"
                      FIELD row-id AS ROWID
                      FIELD inv-no LIKE ar-inv.inv-no
                      FIELD inv-date LIKE ar-inv.inv-date
                      FIELD i-no LIKE ar-invl.i-no
                      FIELD actnum LIKE ar-invl.actnum
                      FIELD net LIKE ar-inv.net
                      FIELD paid LIKE ar-inv.paid 
                      FIELD due LIKE ar-inv.due
                      FIELD amt-paid LIKE ar-cashl.amt-paid
                      FIELD amt-disc LIKE ar-cashl.amt-disc
                      FIELD seq-no AS INT.

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
&Scoped-define INTERNAL-TABLES tt-inv

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-inv.inv-no tt-inv.inv-date tt-inv.actnum tt-inv.i-no tt-inv.net tt-inv.paid tt-inv.due   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-inv
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-inv.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-inv
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-inv


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 Btn_OK Btn_Select Btn_Deselect ~
Btn_Cancel 

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

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Select 
     LABEL "Select All" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-inv SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-inv.inv-no     LABEL "Invoice#"
      tt-inv.inv-date   LABEL "Inv Date"
      tt-inv.actnum     LABEL "Acct#"
      tt-inv.i-no       LABEL "FG Item#"
      tt-inv.net        LABEL "Net"
      tt-inv.paid       LABEL "Amt Paid"
      tt-inv.due        LABEL "Balance Due"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 95 BY 12.38
         BGCOLOR 8  EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-2 AT ROW 1.24 COL 1
     Btn_OK AT ROW 14.81 COL 16
     Btn_Select AT ROW 14.81 COL 33
     Btn_Deselect AT ROW 14.81 COL 50
     Btn_Cancel AT ROW 14.81 COL 67
     SPACE(14.59) SKIP(0.71)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Select Line Items from Invoice:"
         DEFAULT-BUTTON Btn_OK.


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
/* BROWSE-TAB BROWSE-2 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-inv.
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Select Line Items from Invoice: */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
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
  DEF VAR ll-ans AS LOG NO-UNDO.
  DEF VAR li AS INT NO-UNDO.


  MESSAGE "This will create memo lines for the invoice line items selected." SKIP
          "Do you want to continue?" VIEW-AS ALERT-BOX QUESTION
            BUTTON YES-NO UPDATE ll-ans.

  IF ll-ans THEN DO:
    DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
      {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.

      IF AVAIL tt-inv THEN tt-inv.selekt = YES.
    END.

    RUN create-records.
  END.  /* ll-ans */
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


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
/*ON 'mouse-select-click':U OF tt-inv.selekt IN BROWSE {&browse-name} 
DO:
    IF SELF:SCREEN-VALUE = "Yes" THEN SELF:SCREEN-VALUE = "No".
    ELSE SELF:SCREEN-VALUE = "Yes".
    RETURN NO-APPLY.
END.*/

FRAME {&FRAME-NAME}:TITLE = TRIM(FRAME {&FRAME-NAME}:TITLE) + " " +
                            TRIM(STRING(ip-inv-no,">>>>>>>>>>")).

FIND ar-cashl WHERE ROWID(ar-cashl) EQ ip-cash-rowid NO-LOCK NO-ERROR.

IF AVAIL ar-cashl THEN DO:
  FIND FIRST ar-cash WHERE ar-cash.c-no EQ ar-cashl.c-no NO-LOCK NO-ERROR.
END.

IF AVAIL ar-cash THEN
FIND FIRST ar-inv NO-LOCK
    WHERE ar-inv.company EQ ar-cash.company
      AND ar-inv.inv-no  EQ ip-inv-no
      AND ar-inv.posted  EQ YES
    NO-ERROR.

IF AVAIL ar-inv THEN RUN build-table.

IF lv-num-rec GT 0 THEN
  IF lv-num-rec EQ 1 THEN DO WITH FRAME {&FRAME-NAME}:
    FOR EACH tt-inv:
      tt-inv.selekt = YES.
    END.
    RUN create-records.
  END.

  ELSE DO:
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
  
  FOR EACH ar-invl NO-LOCK
      WHERE ar-invl.x-no EQ ar-inv.x-no
        AND ar-invl.amt  NE 0:

    CREATE tt-inv.
    ASSIGN
     tt-inv.row-id   = ROWID(ar-invl)
     tt-inv.selekt   =  IF ar-cashl.invoiceXNo EQ ar-invl.x-no AND 
                                ar-cashl.invoiceLine EQ ar-invl.line THEN TRUE ELSE FALSE
     tt-inv.inv-no   = ar-inv.inv-no
     tt-inv.inv-date = ar-inv.inv-date
     tt-inv.i-no     = ar-invl.i-no
     tt-inv.actnum   = ar-invl.actnum
     tt-inv.net      = ar-invl.amt
     lv-num-rec      = lv-num-rec + 1.

    
        FOR EACH b-cashl
        WHERE b-cashl.invoiceLine   EQ ar-invl.line
          AND b-cashl.invoiceXNo EQ ar-invl.x-no :

      tt-inv.paid = tt-inv.paid + (b-cashl.amt-paid * -1).
    END.

    tt-inv.due = tt-inv.net - tt-inv.paid.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-records D-Dialog 
PROCEDURE create-records :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR li-next-line AS INT NO-UNDO.
  DEF VAR lv-dscr LIKE ar-cashl.dscr NO-UNDO.


  FOR EACH tt-inv,
      FIRST ar-invl WHERE ROWID(ar-invl) EQ tt-inv.row-id NO-LOCK,     
      FIRST b-cashl
      WHERE b-cashl.invoiceXNo EQ ar-invl.x-no
        AND b-cashl.invoiceLine EQ ar-invl.LINE :

    IF ROWID(ar-cashl) EQ ROWID(b-cashl) THEN ll = YES.

    IF NOT tt-inv.selekt THEN DELETE reftable.

    DELETE tt-inv.
  END.

  FOR EACH tt-inv WHERE tt-inv.selekt,
      FIRST ar-invl WHERE ROWID(ar-invl) EQ tt-inv.row-id NO-LOCK:

    IF ll THEN DO:
      FOR EACH b-cashl OF ar-cash NO-LOCK BY b-cashl.line DESCENDING:
        ASSIGN
         li-next-line = b-cashl.line
         lv-dscr      = b-cashl.dscr.
        LEAVE.
      END.

      CREATE b-cashl.
      ASSIGN
       b-cashl.company  = ar-cash.company
       b-cashl.c-no     = ar-cash.c-no
       b-cashl.line     = li-next-line + 1
       b-cashl.dscr     = lv-dscr.
    END.

    ELSE DO:
      FIND b-cashl WHERE ROWID(b-cashl) EQ ROWID(ar-cashl).
      ll = YES.
    END.

    ASSIGN
     b-cashl.actnum   = ar-invl.actnum
     b-cashl.cust-no  = ar-cash.cust-no
     b-cashl.check-no = STRING(ar-cash.check-no,"9999999999")
     b-cashl.memo     = ar-cash.memo
     b-cashl.inv-no   = tt-inv.inv-no
     b-cashl.inv-date = tt-inv.inv-date
     b-cashl.amt-paid = - tt-inv.due
     b-cashl.dscr     = lv-dscr
     b-cashl.invoiceXNo = ar-invl.x-no
     b-cashl.invoiceLine = ar-invl.LINE.

    IF b-cashl.actnum EQ "" THEN DO:
      find first ar-ctrl where ar-ctrl.company = ar-cash.company no-lock no-error.
      find first bank where bank.company = ar-cash.company and
                            bank.bank-code = ar-cash.bank-code no-lock no-error.
      if avail bank THEN do:
         find first account where account.company = ar-cash.company and
                        account.actnum  = bank.actnum no-lock no-error.
                   assign b-cashl.actnum = bank.actnum.
      end.
      ELSE do:
         if ar-cash.check-no ge 90000000 AND ar-cash.check-no le 99999999
         THEN find first account where account.company = ar-cash.company and
                                       account.actnum  = ar-ctrl.sales no-lock no-error.
         ELSE find first account where account.company = ar-cash.company and
                               account.actnum  = ar-ctrl.cash-act no-lock no-error.
         if avail account THEN assign b-cashl.actnum = account.actnum.
      end.
    END.
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
  ENABLE BROWSE-2 Btn_OK Btn_Select Btn_Deselect Btn_Cancel 
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
  DEF VAR li AS INT NO-UNDO.
  DEF VAR li1 AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FOR EACH tt-inv WHERE tt-inv.selekt EQ YES:
    li1 = li1 + 1.
  END.

  DO WITH FRAME {&FRAME-NAME}:
    {&browse-name}:SELECT-ALL ().
    
    DO WHILE {&browse-name}:NUM-SELECTED-ROWS GT li1:
      DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
        {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.

        IF NOT AVAIL tt-inv OR NOT tt-inv.selekt THEN DO:
          {&browse-name}:DESELECT-SELECTED-ROW (li).
          LEAVE.
        END.
      END.
    END.
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

