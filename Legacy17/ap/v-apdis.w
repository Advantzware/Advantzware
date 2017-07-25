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

  File: viewers/<table>.w

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
DEF BUFFER bf-dis FOR ap-dis.

&SCOPED-DEFINE enable-proc proc-enable
&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF

DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES ap-dis
&Scoped-define FIRST-EXTERNAL-TABLE ap-dis


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ap-dis.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ap-dis.vend-no ap-dis.payee ap-dis.check-date ~
ap-dis.bank-code ap-dis.ex-rate 
&Scoped-define ENABLED-TABLES ap-dis
&Scoped-define FIRST-ENABLED-TABLE ap-dis
&Scoped-Define ENABLED-OBJECTS RECT-1 lv-check-no 
&Scoped-Define DISPLAYED-FIELDS ap-dis.vend-no ap-dis.payee ~
ap-dis.check-date ap-dis.bank-code ap-dis.check-amt ap-dis.curr-code[1] ~
ap-dis.ex-rate 
&Scoped-define DISPLAYED-TABLES ap-dis
&Scoped-define FIRST-DISPLAYED-TABLE ap-dis
&Scoped-Define DISPLAYED-OBJECTS vend_name bank_name lv-check-no 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company|y|y|ASI.ap-dis.company
d-no|y|y|ASI.ap-dis.d-no
check-no||y|ASI.ap-dis.check-no
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "company,d-no",
     Keys-Supplied = "company,d-no,check-no"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE bank_name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE lv-check-no AS INTEGER FORMAT "999999" INITIAL 0 
     LABEL "Check#" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE vend_name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 107 BY 6.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     ap-dis.vend-no AT ROW 1.24 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     vend_name AT ROW 1.24 COL 40 COLON-ALIGNED NO-LABEL
     ap-dis.payee AT ROW 2.19 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     ap-dis.check-date AT ROW 3.14 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ap-dis.bank-code AT ROW 4.1 COL 19 COLON-ALIGNED
          LABEL "Bank Code"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     bank_name AT ROW 4.1 COL 39 COLON-ALIGNED NO-LABEL
     lv-check-no AT ROW 5.29 COL 19 COLON-ALIGNED
     ap-dis.check-amt AT ROW 5.29 COL 55 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.6 BY 1
     ap-dis.curr-code[1] AT ROW 6.24 COL 19 COLON-ALIGNED
          LABEL "Currency Code"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     ap-dis.ex-rate AT ROW 6.48 COL 61 COLON-ALIGNED
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
   External Tables: ASI.ap-dis
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
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
         HEIGHT             = 17
         WIDTH              = 144.
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

/* SETTINGS FOR FILL-IN ap-dis.bank-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bank_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ap-dis.check-amt IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ap-dis.curr-code[1] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ap-dis.ex-rate IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN vend_name IN FRAME F-Main
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
    def var char-val as cha no-undo.
    def var look-recid as recid no-undo. 
    DEF VAR lv-handle AS HANDLE NO-UNDO.

    g_lookup-var = "".
    case focus:name :
         when "lv-check-no" THEN  DO:
             run windows/l-check.w (g_company,focus:screen-value, output char-val).
             if char-val <> "" then assign focus:screen-value = entry(1,char-val).
         end.
         OTHERWISE DO:
            lv-handle = focus:handle.
            run applhelp.p.

            if g_lookup-var <> "" then do:
               lv-handle:screen-value = g_lookup-var.
            end.  
         END.
    end case.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-dis.bank-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-dis.bank-code V-table-Win
ON LEAVE OF ap-dis.bank-code IN FRAME F-Main /* Bank Code */
DO:
    IF LASTKEY = -1 THEN RETURN.
    {&methods/lValidateError.i YES}
    {VALIDATE/bank.i ap-dis.bank-code bank_name}

   IF SELF:modified AND SELF:screen-value <> "" THEN DO:
     FIND FIRST bank NO-LOCK 
           WHERE bank.company = g_company 
           AND bank.bank-code = ap-dis.bank-code:SCREEN-VALUE no-error.
       ASSIGN
           lv-check-no:SCREEN-VALUE  = STRING( IF AVAIL bank THEN bank.last-chk + 1 ELSE 0) .
   END.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-check-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-check-no V-table-Win
ON LEAVE OF lv-check-no IN FRAME F-Main /* Check# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-check-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-dis.vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-dis.vend-no V-table-Win
ON LEAVE OF ap-dis.vend-no IN FRAME F-Main /* Vendor# */
DO:
    {&methods/lValidateError.i YES}
    IF SELF:SCREEN-VALUE <> "" THEN DO:
        {VALIDATE/vend.i ap-dis.vend-no vend_name }
    end.
    ELSE DO:
        RETURN.
    END.

    {&methods/lValidateError.i NO}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'company':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = ap-dis
           &WHERE = "WHERE ap-dis.company eq key-value"
       }
    WHEN 'd-no':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = ap-dis
           &WHERE = "WHERE ap-dis.d-no eq INTEGER(key-value)"
       }
  END CASE.

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
  {src/adm/template/row-list.i "ap-dis"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ap-dis"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VARIABLE cBankCode AS CHARACTER NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN ap-dis.check-no = INT(lv-check-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} ).
  ASSIGN cBankCode = ap-dis.bank-code .

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF adm-new-record OR (cBankCode NE ap-dis.bank-code:SCREEN-VALUE) THEN do:
      FIND FIRST bank where bank.company = g_company and
                        bank.bank-code = ap-dis.bank-code:SCREEN-VALUE EXCLUSIVE-LOCK .

      IF AVAIL bank THEN bank.last-chk = INTEGER(lv-check-no:SCREEN-VALUE IN FRAME {&FRAME-NAME})  .
      RELEASE bank .
  END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR X AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  FOR EACH bf-dis NO-LOCK BY d-no DESCENDING:
      X = bf-dis.d-no.
      LEAVE.
  END.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

 find first ap-ctrl where ap-ctrl.company = g_company no-lock no-error.

 find first bank where bank.company = g_company and
                      bank.actnum  = ap-ctrl.cash-act no-error.
 FIND FIRST company WHERE company.company = g_company NO-LOCK NO-ERROR.
 FIND FIRST currency WHERE currency.company = g_company AND
                           currency.c-code = company.curr-code
                           NO-LOCK NO-ERROR.

 assign ap-dis.company = g_company
        ap-dis.check-no = IF AVAIL bank THEN bank.last-chk + 1 ELSE 0
        ap-dis.d-no = x + 1
        ap-dis.bank-code = IF AVAIL bank THEN bank.bank-code ELSE ""
        ap-dis.curr-code[1] = IF AVAIL company THEN company.curr-code ELSE ""
        ap-dis.ex-rate = IF AVAIL currency THEN currency.ex-rate ELSE 0
        ap-dis.check-date = TODAY.

 /*IF AVAIL bank THEN bank.last-chk = ap-dis.check-no.*/

RUN dispatch ('row-changed').
IF adm-adding-record THEN ENABLE lv-check-no WITH FRAME {&FRAME-NAME}.



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
  IF AVAIL ap-dis THEN lv-check-no = ap-dis.check-no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  DISABLE lv-check-no WITH FRAME {&FRAME-NAME}.

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
  {&methods/lValidateError.i YES}
  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME} :
    IF ap-dis.vend-no:SCREEN-VALUE <> "" THEN DO:
        {validate/vend-upd.i ap-dis.vend-no vend_name}
    END.
    {VALIDATE/bank.i ap-dis.bank-code bank_name}
  END.
  {&methods/lValidateError.i NO}
  RUN valid-check-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /* Code placed here will execute AFTER standard behavior.    */
  ll-new-record = adm-new-record.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE lv-check-no WITH FRAME {&FRAME-NAME}.
  IF ll-new-record THEN DO:
     DEF VAR char-hdl AS cha NO-UNDO.
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"adding-line-target",OUTPUT char-hdl).
     RUN auto-line-add IN WIDGET-HANDLE(char-hdl).

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
  IF NOT adm-new-record AND ap-dis.posted THEN do:

      DEF VAR char-hdl AS cha NO-UNDO.
      MESSAGE "This invoice has been posted. No changes are allowed!"           
           VIEW-AS ALERT-BOX ERROR.
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).
      RUN apply-cancel IN WIDGET-HANDLE(char-hdl).

  END.
  ENABLE lv-check-no WITH FRAME {&FRAME-NAME}.

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
  FIND CURRENT ap-dis NO-LOCK NO-ERROR.
  IF AVAIL ap-dis THEN RUN dispatch ('display-fields').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "ap-dis" "company"}
  {src/adm/template/sndkycas.i "d-no" "ap-dis" "d-no"}
  {src/adm/template/sndkycas.i "check-no" "ap-dis" "check-no"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "ap-dis"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-check-no V-table-Win 
PROCEDURE valid-check-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-ap-dis FOR ap-dis.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF CAN-FIND(FIRST b-ap-dis
                WHERE b-ap-dis.company   EQ ap-dis.company
                  AND b-ap-dis.bank-code EQ ap-dis.bank-code:SCREEN-VALUE
                  AND b-ap-dis.check-no  EQ INT(lv-check-no:SCREEN-VALUE)
                  AND ROWID(b-ap-dis)    NE ROWID(ap-dis))               OR
       CAN-FIND(FIRST ap-pay
                WHERE ap-pay.company   EQ ap-dis.company
                  AND ap-pay.bank-code EQ ap-dis.bank-code:SCREEN-VALUE
                  AND ap-pay.check-no  EQ INT(lv-check-no:SCREEN-VALUE)) OR
       INT(lv-check-no:SCREEN-VALUE) EQ 0                                THEN DO:

      MESSAGE TRIM(lv-check-no:LABEL) + " " +
              (IF INT(lv-check-no:SCREEN-VALUE) EQ 0 THEN "may not be zero"
               ELSE "already exists, please re-enter") +
              "..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO lv-check-no.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

