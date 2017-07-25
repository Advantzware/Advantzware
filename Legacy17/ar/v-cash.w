&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: ar\v-cash.w

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
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF BUFFER bf-cash FOR ar-cash.
DEF BUFFER bf-cashl FOR ar-cashl.
DEF BUFFER ar-c-memo FOR reftable.
def var look-recid as recid no-undo.
DEF VAR lv-old-cust LIKE ar-cash.cust-no NO-UNDO.
DEF VAR ll-warned AS LOG NO-UNDO.

&SCOPED-DEFINE enable-arcash proc-enable
&SCOPED-DEFINE create-more ar/c-arcash
&SCOPED-DEFINE where-ar-c-memo                                      ~
        WHERE ar-c-memo.reftable EQ "ar-cashl.ar-cashl"             ~
          AND ar-c-memo.company  EQ ar-cash.company                 ~
          AND ar-c-memo.loc      EQ ""

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES ar-cash
&Scoped-define FIRST-EXTERNAL-TABLE ar-cash


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ar-cash.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ar-cash.cust-no ar-cash.bank-code ~
ar-cash.check-no ar-cash.check-date ar-cash.check-amt 
&Scoped-define ENABLED-TABLES ar-cash
&Scoped-define FIRST-ENABLED-TABLE ar-cash
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS ar-cash.cust-no ar-cash.bank-code ~
ar-cash.check-no ar-cash.check-date ar-cash.check-amt ar-cash.curr-code[1] ~
ar-cash.ex-rate 
&Scoped-define DISPLAYED-TABLES ar-cash
&Scoped-define FIRST-DISPLAYED-TABLE ar-cash
&Scoped-Define DISPLAYED-OBJECTS cust_name bank_name ld-not-applied 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS ar-cash.curr-code[1] ar-cash.ex-rate 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE bank_name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE cust_name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE ld-not-applied AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Not Applied" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 123 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     ar-cash.cust-no AT ROW 1.24 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     cust_name AT ROW 1.24 COL 39 COLON-ALIGNED NO-LABEL
     ar-cash.bank-code AT ROW 2.38 COL 19 COLON-ALIGNED
          LABEL "Bank Code"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     bank_name AT ROW 2.43 COL 39 COLON-ALIGNED NO-LABEL
     ar-cash.check-no AT ROW 3.57 COL 19 COLON-ALIGNED FORMAT "9999999999"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     ar-cash.check-date AT ROW 4.76 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     ar-cash.check-amt AT ROW 4.81 COL 59 COLON-ALIGNED
          LABEL "Check Amount"
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     ld-not-applied AT ROW 4.81 COL 101 COLON-ALIGNED
     ar-cash.curr-code[1] AT ROW 5.76 COL 19 COLON-ALIGNED
          LABEL "Currency Code"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     ar-cash.ex-rate AT ROW 5.76 COL 59 COLON-ALIGNED
          LABEL "Exchange Rate"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.ar-cash
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 17.14
         WIDTH              = 144.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ar-cash.bank-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bank_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ar-cash.check-amt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-cash.check-no IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ar-cash.curr-code[1] IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN cust_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ar-cash.ex-rate IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ld-not-applied IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */


&Scoped-define SELF-NAME ar-cash.cust-no                    /*Task# 01031404*/
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cash.cust-no W-Win
ON HELP OF ar-cash.cust-no IN FRAME F-Main /* Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.
    APPLY "entry" TO {&self-name}.
    RUN windows/l-custact.w (g_company, ar-cash.cust-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
    IF char-val <> "" THEN ASSIGN ar-cash.cust-no:SCREEN-VALUE = ENTRY(1,char-val)
                                cust_name:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME ar-cash.bank-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cash.bank-code V-table-Win
ON LEAVE OF ar-cash.bank-code IN FRAME F-Main /* Bank Code */
DO:
  IF LASTKEY = -1 THEN RETURN.
   {&methods/lValidateError.i YES}
   IF SELF:MODIFIED THEN do:
      FIND FIRST bank WHERE bank.company = g_company AND
                            bank.bank-code = ar-cash.bank-code:SCREEN-VALUE NO-LOCK NO-ERROR.
      IF NOT AVAIL bank THEN DO:
         MESSAGE "Invalid Bank Code. Try Help." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.
      bank_name:SCREEN-VALUE = bank.bank-NAME.
   END.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-cash.check-amt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cash.check-amt V-table-Win
ON LEAVE OF ar-cash.check-amt IN FRAME F-Main /* Check Amount */
DO:
  IF LASTKEY NE -1 THEN DO:
  {&methods/lValidateError.i YES}
    IF DEC(ar-cash.check-amt:SCREEN-VALUE) EQ 0 THEN DO:
       MESSAGE "Check Amount cannot be 0." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    ELSE IF DEC(ar-cash.check-amt:SCREEN-VALUE) LT 0 AND
         CAN-FIND(FIRST ar-cashl OF ar-cash WHERE
         ar-cashl.inv-no EQ 0) THEN DO:
         MESSAGE "Check Amount cannot be less than 0 with line item of Invoice# 0." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
    END.

  END.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cash.check-amt V-table-Win
ON VALUE-CHANGED OF ar-cash.check-amt IN FRAME F-Main /* Check Amount */
DO:
  IF AVAIL ar-cash THEN RUN applied-amt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-cash.check-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cash.check-date V-table-Win
ON LEAVE OF ar-cash.check-date IN FRAME F-Main /* Check Date */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-check-date NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cash.check-date V-table-Win
ON VALUE-CHANGED OF ar-cash.check-date IN FRAME F-Main /* Check Date */
DO:
  ll-warned = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-cash.check-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cash.check-no V-table-Win
ON LEAVE OF ar-cash.check-no IN FRAME F-Main /* Check No */
DO:
    IF LASTKEY = -1 THEN RETURN.
    {&methods/lValidateError.i YES}
    IF int(ar-cash.check-no:SCREEN-VALUE) = 0 THEN DO:
        MESSAGE "Check number must be entered..." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.

    IF ar-cash.check-no:MODIFIED THEN DO:
       IF INT(ar-cash.check-no:SCREEN-VALUE) >= 90000000 AND
          INT(ar-cash.check-no:SCREEN-VALUE) <= 99999999
       THEN DO:
          MESSAGE "This number reserved for CR/DB memos." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
    END.

    FIND FIRST bf-cash WHERE bf-cash.company = g_company
                          AND bf-cash.cust-no = ar-cash.cust-no:SCREEN-VALUE
                          AND bf-cash.check-no = int(ar-cash.check-no:SCREEN-VALUE)
                          AND RECID(bf-cash) <> RECID(ar-cash)
                          NO-LOCK NO-ERROR.
    IF AVAIL bf-cash THEN DO:
        MESSAGE "Cash Receipt Already Exists for Customer " + bf-cash.cust-no +
                 " and Check " bf-cash.check-no VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-cash.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cash.cust-no V-table-Win
ON LEAVE OF ar-cash.cust-no IN FRAME F-Main /* Customer */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cust-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-cash.cust-no V-table-Win
ON VALUE-CHANGED OF ar-cash.cust-no IN FRAME F-Main /* Customer */
DO:
  RUN new-cust-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = YES.
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item V-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "ar-cash"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ar-cash"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applied-amt V-table-Win 
PROCEDURE applied-amt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ld-not-applied = DEC(ar-cash.check-amt:SCREEN-VALUE).
    FOR EACH bf-cashl OF ar-cash NO-LOCK
        WHERE NOT CAN-FIND(FIRST ar-c-memo
                           {&where-ar-c-memo}
                             AND ar-c-memo.code EQ bf-cashl.rec_key):
      ld-not-applied = ld-not-applied - (bf-cashl.amt-paid - bf-cashl.amt-disc).
    END.
    DISPLAY ld-not-applied.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-applied-amt V-table-Win 
PROCEDURE check-applied-amt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-all-applied AS LOG INIT YES NO-UNDO.


  IF AVAIL ar-cash AND NOT ar-cash.posted THEN DO:
    RUN applied-amt.
    IF ld-not-applied <> 0 THEN DO:
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-target",OUTPUT char-hdl).
      RUN create-onaccount IN WIDGET-HANDLE(char-hdl) (OUTPUT op-all-applied).  
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
----------------------------------------*/
  DEF VAR li-next-cno AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */


  FIND last bf-cash USE-INDEX c-no NO-LOCK NO-ERROR.
  li-next-cno = IF AVAIL bf-cash THEN bf-cash.c-no + 1 ELSE 1.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST ar-ctrl WHERE ar-ctrl.company = g_company NO-LOCK NO-ERROR.
  FIND FIRST bank WHERE bank.company = g_company AND
                        bank.actnum = ar-ctrl.cash-act NO-LOCK NO-ERROR.
  IF AVAIL bank THEN ASSIGN ar-cash.bank-code = bank.bank-code.
  FIND FIRST company WHERE company.company = g_company NO-LOCK NO-ERROR.
  FIND FIRST currency WHERE currency.company = g_company AND
                            currency.c-code = company.curr-code
                            NO-LOCK NO-ERROR.
  ASSIGN ar-cash.company = g_company
         ar-cash.c-no = li-next-cno
         ar-cash.check-date = TODAY
         ar-cash.curr-code[1] = IF AVAIL company THEN company.curr-code ELSE ""
         ar-cash.ex-rate = IF AVAIL currency THEN currency.ex-rate ELSE 0
         .
  /* refresh cash line browser */
/*  DEF VAR char-hdl AS cha NO-UNDO.       
  RUN get-link-handle IN adm-broker-hdl (this-procedure,"record-target", OUTPUT char-hdl).
  RUN reopen-query IN WIDGET-HANDLE(char-hdl).
*/

  RUN dispatch ('row-changed').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   cust_name = ""
   bank_name = "".

  IF AVAIL ar-cash THEN DO:
    FIND FIRST cust
        WHERE cust.company EQ g_company
          AND cust.cust-no EQ ar-cash.cust-no
        NO-LOCK NO-ERROR.
    IF AVAIL cust THEN cust_name = cust.name.

    FIND FIRST bank
        WHERE bank.company   EQ g_company
          AND bank.bank-code EQ ar-cash.bank-code
        NO-LOCK NO-ERROR.
    IF AVAIL bank THEN bank_name = bank.bank-name.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ld-not-applied = 0.
  IF AVAIL ar-cash THEN DO:
    ld-not-applied = ar-cash.check-amt.
    FOR EACH bf-cashl OF ar-cash NO-LOCK
        WHERE NOT CAN-FIND(FIRST ar-c-memo
                           {&where-ar-c-memo}
                             AND ar-c-memo.code EQ bf-cashl.rec_key):
      ld-not-applied = ld-not-applied -
                       (bf-cashl.amt-paid - IF ar-cash.posted THEN 0 ELSE bf-cashl.amt-disc).
    END.
  END.
  DISPLAY ld-not-applied WITH FRAME {&FRAME-NAME}.

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST currency NO-LOCK
        WHERE currency.company EQ ar-cash.company 
          AND currency.c-code  EQ ar-cash.curr-code[1]:SCREEN-VALUE
        NO-ERROR.
    IF AVAIL currency THEN
      ar-cash.ex-rate:SCREEN-VALUE = STRING(currency.ex-rate).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-new-record AS LOG NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  /* === validation ====*/
  RUN valid-cust-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-check-date NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
     {&methods/lValidateError.i YES}
     IF ar-cash.bank-code:MODIFIED THEN do:
        FIND FIRST bank WHERE bank.company = g_company AND
                            bank.bank-code = ar-cash.bank-code:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF NOT AVAIL bank THEN DO:
           MESSAGE "Invalid Bank Code. Try Help." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO ar-cash.bank-code.
           RETURN NO-APPLY.
        END.
        bank_name:SCREEN-VALUE = bank.bank-NAME.
        FOR EACH ar-cashl WHERE ar-cashl.company EQ ar-cash.company
                            AND ar-cashl.c-no    EQ ar-cash.c-no
                          EXCLUSIVE-LOCK.
            IF ar-cashl.actnum NE bank.actnum THEN
                ar-cashl.actnum = bank.actnum.
        END.
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).        
        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN 
            RUN refresh-detail IN WIDGET-HANDLE(char-hdl).

     END.
     IF ar-cash.check-no:MODIFIED THEN DO:
       IF int(ar-cash.check-no:SCREEN-VALUE) = 0 THEN DO:
           MESSAGE "Check number must be entered..." VIEW-AS ALERT-BOX.
           APPLY "entry" TO ar-cash.check-no.
           RETURN.
       END.
       IF INT(ar-cash.check-no:SCREEN-VALUE) >= 90000000 AND
          INT(ar-cash.check-no:SCREEN-VALUE) <= 99999999
       THEN DO:
          MESSAGE "This number reserved for CR/DB memos." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO ar-cash.check-no.
          RETURN NO-APPLY.
       END.
     END.

     IF INPUT ar-cash.check-amt EQ 0 THEN DO:
        MESSAGE "Check Amount cannot be 0." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ar-cash.check-amt.
        RETURN NO-APPLY.
     END.
     ELSE IF INPUT ar-cash.check-amt LT 0 AND
         CAN-FIND(FIRST ar-cashl OF ar-cash WHERE
         ar-cashl.inv-no EQ 0) THEN DO:
         MESSAGE "Check Amount cannot be less than 0 with line item of Invoice# 0." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
     END.

     FIND FIRST bf-cash WHERE bf-cash.company = g_company
                          AND bf-cash.cust-no = ar-cash.cust-no:SCREEN-VALUE
                          AND bf-cash.check-no = int(ar-cash.check-no:SCREEN-VALUE)
                          AND recid(bf-cash) <> RECID(ar-cash)
                          NO-LOCK NO-ERROR.
     IF AVAIL bf-cash THEN DO:
        MESSAGE "Cash Receipt Already Exists for Customer " + bf-cash.cust-no +
                 " and Check " bf-cash.check-no VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ar-cash.check-no.
        RETURN NO-APPLY.
     END.
  {&methods/lValidateError.i NO}
  END.
  /* ====== end validation =========*/
  ll-new-record = adm-new-record.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-new-record THEN DO:

     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"adding-line-target",OUTPUT char-hdl).
     RUN auto-line-add IN WIDGET-HANDLE(char-hdl).

  END.
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-cust-no V-table-Win 
PROCEDURE new-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND cust
        WHERE cust.company EQ g_company
          /*AND cust.active  EQ "A"*/
          AND cust.cust-no EQ ar-cash.cust-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL cust THEN
      ASSIGN
       cust_name:SCREEN-VALUE = cust.NAME
       ar-cash.curr-code[1]:SCREEN-VALUE = cust.curr-code
       lv-old-cust = ar-cash.cust-no:SCREEN-VALUE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-enable V-table-Win 
PROCEDURE proc-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.


  IF NOT adm-new-record AND ar-CASH.posted THEN do:    
    MESSAGE "This invoice has been posted. No changes are allowed!"           
         VIEW-AS ALERT-BOX ERROR.
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).
    RUN apply-cancel IN WIDGET-HANDLE(char-hdl).    
  END.

  DO WITH FRAME {&frame-name}:
    lv-old-cust = ar-cash.cust-no:SCREEN-VALUE.
    IF NOT adm-new-record AND AVAIL ar-cash AND CAN-FIND(FIRST ar-cashl OF ar-cash) THEN DISABLE ar-cash.cust-no.
  END.

  ll-warned = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query V-table-Win 
PROCEDURE reopen-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND CURRENT ar-cash NO-LOCK NO-ERROR.
  IF AVAIL ar-cash THEN RUN dispatch ('display-fields').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ar-cash"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-check-date V-table-Win 
PROCEDURE valid-check-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG INIT NO NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT ll-warned                                                               AND
       NOT CAN-FIND(FIRST period
                    WHERE period.company EQ g_company
                      AND period.pstat   EQ YES
                      AND period.pst     LE DATE(ar-cash.check-date:SCREEN-VALUE)
                      AND period.pend    GE DATE(ar-cash.check-date:SCREEN-VALUE)) THEN DO:
      MESSAGE TRIM(ar-cash.check-date:LABEL) +
              " is not within an open period, continue..."
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.
      IF ll THEN ll-warned = YES.
      ELSE DO:
        APPLY "entry" TO ar-cash.check-date.
        RETURN ERROR.
      END.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no V-table-Win 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF lv-old-cust NE ar-cash.cust-no:SCREEN-VALUE THEN RUN new-cust-no.

    IF NOT CAN-FIND(FIRST cust
                    WHERE cust.company EQ g_company 
                      AND LOOKUP(cust.active,"A,E") > 0  
                      AND cust.cust-no EQ ar-cash.cust-no:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid Customer, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ar-cash.cust-no.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

