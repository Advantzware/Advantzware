&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: ar\d-selinv.w
  
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
DEF INPUT param ip-cash-recid AS RECID .
DEF INPUT PARAM ip-is-memo AS LOG .
DEF OUTPUT PARAM op-cancel AS LOG.

/* Local Variable Definitions ---                                       */
DEF BUFFER b-cash FOR ar-cash.
DEF BUFFER b-cashl FOR ar-cashl.

DEF VAR lv-num-rec AS INT NO-UNDO.

DEF TEMP-TABLE tt-inv FIELD selekt AS LOG LABEL "Selected"
                      FIELD row-id AS ROWID
                      FIELD inv-no LIKE ar-inv.inv-no 
                      FIELD inv-date LIKE ar-inv.inv-date
                      FIELD net LIKE ar-inv.net
                      FIELD paid LIKE ar-inv.paid 
                      FIELD due LIKE ar-inv.due
                      FIELD amt-paid LIKE ar-cashl.amt-paid
                      FIELD amt-disc LIKE ar-cashl.amt-disc
                      FIELD seq-no AS INT
                      FIELD check-no AS INT
                      FIELD row-id2 AS ROWID.

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
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-inv.inv-no tt-inv.inv-date tt-inv.net tt-inv.paid tt-inv.due   
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
      tt-inv.net        LABEL "Net"
      tt-inv.paid       LABEL "Amt Paid"
      tt-inv.due        LABEL "Balance Due"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 76 BY 12.38
         BGCOLOR 8  FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-2 AT ROW 1.24 COL 1
     Btn_OK AT ROW 14.81 COL 7
     Btn_Select AT ROW 14.81 COL 24
     Btn_Deselect AT ROW 14.81 COL 41
     Btn_Cancel AT ROW 14.81 COL 58
     SPACE(4.00) SKIP(0.71)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "AR Invoice Selection"
         DEFAULT-BUTTON Btn_OK.


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
ON WINDOW-CLOSE OF FRAME D-Dialog /* AR Invoice Selection */
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
  op-cancel = YES.
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
  DEF VAR li-next-line AS INT NO-UNDO.
  DEF VAR ld-applied AS DEC NO-UNDO.
  DEF VAR li-seq-no AS INT NO-UNDO.
  DEF VAR ll-ans AS LOG NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR ld-net1 AS DEC NO-UNDO.
  DEF VAR ld-net2 AS DEC NO-UNDO.
  DEF VAR ld-due AS DEC NO-UNDO.
  DEF VAR v-amt-paid-inv-no AS INT NO-UNDO.

  DEF BUFFER ar-c-memo FOR reftable.


  IF {&browse-name}:NUM-SELECTED-ROWS GT 0 THEN DO:
    IF ip-is-memo THEN
      MESSAGE "This will create line items from the invoices selected." SKIP
              "Do you want to continue?" VIEW-AS ALERT-BOX QUESTION
              BUTTON YES-NO UPDATE ll-ans.
    ELSE DO:
      ll-ans = ?.
      MESSAGE "This will create line items from the invoices selected." SKIP
              "Do you want to include discounts even if past terms date?" VIEW-AS ALERT-BOX QUESTION
              BUTTON YES-NO-CANCEL UPDATE ll-ans.
    END.

    IF ll-ans OR (NOT ip-is-memo AND NOT ll-ans) THEN DO:
        ASSIGN
         op-cancel      = YES
         ld-applied     = 0.

        DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
          {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.

          IF AVAIL tt-inv THEN tt-inv.selekt = YES.
        END.

        FOR EACH tt-inv WHERE tt-inv.selekt,
            FIRST ar-inv WHERE ROWID(ar-inv) EQ tt-inv.row-id NO-LOCK
            BY ar-inv.due:

           ASSIGN
            op-cancel       = NO
            li-seq-no       = li-seq-no + 1
            tt-inv.seq-no   = li-seq-no
            tt-inv.amt-disc = 0.
           
           IF NOT ip-is-memo AND
              (ll-ans OR
               (ar-inv.disc-days NE 0 AND ar-inv.due GT 0 AND
                ar-inv.inv-date + ar-inv.disc-days GE ar-cash.check-date)) THEN
             DO:

             ld-net1 = 0.
             FOR EACH ar-invl fields(amt) NO-LOCK WHERE
                 ar-invl.x-no EQ ar-inv.x-no:
                 ld-net1 = ld-net1 + ar-invl.amt.
             END.
             RUN custom/inv-dnet.p (ROWID(ar-inv), OUTPUT ld-net2).

             ld-due = (ar-inv.due -
                      (IF ar-inv.f-bill THEN ar-inv.freight ELSE 0) - 
                      ar-inv.tax-amt) * (IF ld-net1 = 0 OR ld-net2 = 0 THEN 1 ELSE ld-net2 / ld-net1).
                                                 
             IF ld-due NE 0 THEN
               tt-inv.amt-disc = IF ar-inv.disc-% = 0 THEN 0 ELSE ROUND(ld-due * (ar-inv.disc-% / 100),2).
           END.

           ASSIGN
            tt-inv.amt-paid = ar-inv.due
            ld-applied      = ld-applied + (tt-inv.amt-paid - tt-inv.amt-disc).
        END.   /* each tt-inv */


        v-amt-paid-inv-no  = 0.

        IF NOT ip-is-memo AND ld-applied GT ar-cash.check-amt THEN
        FOR EACH tt-inv WHERE tt-inv.selekt,
            FIRST ar-inv WHERE ROWID(ar-inv) EQ tt-inv.row-id NO-LOCK
            BY tt-inv.seq-no DESC:
           ASSIGN
            ld-applied    = ld-applied - (tt-inv.amt-paid - tt-inv.amt-disc)
            tt-inv.selekt = NO.

           IF ld-applied LE ar-cash.check-amt THEN DO:
             tt-inv.amt-paid = ar-cash.check-amt - ld-applied + tt-inv.amt-disc.
             IF tt-inv.amt-paid NE 0 THEN tt-inv.selekt = YES.

             v-amt-paid-inv-no = tt-inv.inv-no.
             LEAVE.
           END.
        END.

        FOR EACH tt-inv WHERE tt-inv.selekt:
         
           FOR EACH ar-cashl OF ar-cash NO-LOCK BY ar-cashl.line DESCENDING:
              li-next-line = ar-cashl.LINE.
              LEAVE.
           END.

           CREATE ar-cashl.
           ASSIGN ar-cashl.company = ar-cash.company
                  ar-cashl.c-no = ar-cash.c-no
                  ar-cashl.LINE = li-next-line + 1
                  ar-cashl.cust-no = ar-cash.cust-no
                  ar-cashl.check-no = STRING(ar-cash.check-no,"9999999999")
                  li-next-line = li-next-line + 1
                  ar-cashl.memo = ar-cash.memo.

           find first ar-ctrl where ar-ctrl.company = ar-cash.company no-lock no-error.
           find first bank where bank.company = ar-cash.company and
                                 bank.bank-code = ar-cash.bank-code no-lock no-error.
           if avail bank THEN do:
              find first account where account.company = ar-cash.company and
                             account.actnum  = bank.actnum no-lock no-error.
              assign ar-cashl.actnum = bank.actnum.
           end.
           ELSE do:
               if ar-cash.check-no ge 90000000 AND ar-cash.check-no le 99999999
               THEN find first account where account.company = ar-cash.company and
                                       account.actnum  = ar-ctrl.sales no-lock no-error.
               ELSE find first account where account.company = ar-cash.company and
                               account.actnum  = ar-ctrl.cash-act no-lock no-error.
               if avail account THEN assign ar-cashl.actnum = account.actnum.
           end.

           ASSIGN
            ar-cashl.inv-no = tt-inv.inv-no
            ar-cashl.inv-date = tt-inv.inv-date
            ar-cashl.amt-due = tt-inv.due
            ar-cashl.amt-paid = tt-inv.amt-paid
            ar-cashl.amt-disc = tt-inv.amt-disc.

           /*CM*/
           IF tt-inv.check-no >= 90000000 AND
              tt-inv.check-no <= 99999999 THEN
              ar-cashl.inv-no = v-amt-paid-inv-no.

           IF ip-is-memo OR
              (tt-inv.check-no >= 90000000 AND
              tt-inv.check-no <= 99999999) THEN
              ar-cashl.amt-paid = ar-cashl.amt-paid * -1.

           FIND b-cashl WHERE ROWID(b-cashl) EQ tt-inv.row-id NO-LOCK NO-ERROR.

           IF AVAIL b-cashl THEN DO:
             FIND FIRST ar-c-memo
                 WHERE ar-c-memo.reftable = "ar-cashl.ar-cashl"
                   AND ar-c-memo.company  = ar-cash.company
                   AND ar-c-memo.loc      = ""
                   AND ar-c-memo.code     = ar-cashl.rec_key
                 NO-ERROR.
             IF NOT AVAIL ar-c-memo THEN CREATE ar-c-memo.
             ASSIGN
              ar-c-memo.reftable = "ar-cashl.ar-cashl"
              ar-c-memo.company  = ar-cash.company
              ar-c-memo.loc      = ""
              ar-c-memo.code     = ar-cashl.rec_key
              ar-c-memo.val[1]   = b-cashl.c-no
              ar-c-memo.val[2]   = b-cashl.line.
           END.
        END.
    END.  /* ll-ans */
    ELSE op-cancel = YES.
  END.
  ELSE op-cancel = YES.
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

RUN build-table.

IF lv-num-rec GT 0 THEN DO:
  {src/adm/template/dialogmn.i}
END.
ELSE op-cancel = YES.

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
  
  FIND ar-cash WHERE RECID(ar-cash) EQ ip-cash-recid NO-LOCK NO-ERROR.

  IF AVAIL ar-cash THEN DO:
     FIND FIRST cust NO-LOCK
         WHERE cust.company EQ ar-cash.company 
           AND cust.cust-no EQ ar-cash.cust-no
         NO-ERROR.

     FOR EACH ar-inv NO-LOCK
         WHERE ar-inv.company EQ ar-cash.company
           AND ar-inv.posted  EQ YES
           AND ar-inv.cust-no EQ ar-cash.cust-no 
           AND ar-inv.due     NE 0
           AND NOT CAN-FIND(FIRST b-cashl
                            WHERE b-cashl.c-no   EQ ar-cash.c-no
                              AND b-cashl.inv-no EQ ar-inv.inv-no):
       CREATE tt-inv.
       ASSIGN
        tt-inv.row-id   = ROWID(ar-inv)
        tt-inv.inv-no   = ar-inv.inv-no
        tt-inv.inv-date = ar-inv.inv-date
        tt-inv.net      = ar-inv.net
        tt-inv.paid     = ar-inv.paid
        tt-inv.due      = ar-inv.due
        lv-num-rec      = lv-num-rec + 1.
     END.
/*10151511 - removing per Julie - on-account cash memos should not be listed for selection*/
/*on account those should be processed through AC5 (apply/reapply)*/
/*      IF NOT ip-is-memo THEN                                   */
/*      FOR EACH b-cash NO-LOCK                                  */
/*          WHERE b-cash.company EQ ar-cash.company              */
/*            AND b-cash.posted  EQ YES                          */
/*            AND b-cash.memo    EQ YES                          */
/*            AND b-cash.cust-no EQ ar-cash.cust-no,             */
/*          EACH b-cashl NO-LOCK                                 */
/*          WHERE b-cashl.c-no   EQ b-cash.c-no                  */
/*            AND b-cashl.inv-no EQ 0                            */
/*            AND b-cashl.on-account:                            */
/*                                                               */
/*        CREATE tt-inv.                                         */
/*        ASSIGN                                                 */
/*         tt-inv.row-id   = ROWID(b-cashl)                      */
/*         tt-inv.inv-date = b-cash.check-date                   */
/*         tt-inv.amt-paid = b-cashl.amt-disc - b-cashl.amt-paid */
/*         lv-num-rec      = lv-num-rec + 1                      */
/*         tt-inv.due      = b-cashl.amt-paid                    */
/*         tt-inv.check-no = b-cash.check-no.                    */
/*                                                               */
/*      END.                                                     */
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

