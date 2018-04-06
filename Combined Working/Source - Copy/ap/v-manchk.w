&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
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

DEF VAR lv-bank-acct LIKE bank.actnum NO-UNDO.
DEF BUFFER bf-chk FOR ap-chk.
DEF BUFFER bf-sel FOR ap-sel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES ap-chk
&Scoped-define FIRST-EXTERNAL-TABLE ap-chk


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ap-chk.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ap-chk.vend-no ap-chk.bank-code ~
ap-chk.check-no ap-chk.check-date 
&Scoped-define ENABLED-TABLES ap-chk
&Scoped-define FIRST-ENABLED-TABLE ap-chk
&Scoped-Define ENABLED-OBJECTS RECT-2 
&Scoped-Define DISPLAYED-FIELDS ap-chk.vend-no ap-chk.bank-code ~
ap-chk.check-no ap-chk.check-date ap-chk.check-amt 
&Scoped-define DISPLAYED-TABLES ap-chk
&Scoped-define FIRST-DISPLAYED-TABLE ap-chk
&Scoped-Define DISPLAYED-OBJECTS vend_name bank_name 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS ap-chk.check-amt 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
check-no||y|ASI.ap-sel.check-no
company||y|ASI.ap-sel.company
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "check-no,company"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE bank_name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE vend_name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 112 BY 5.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     ap-chk.vend-no AT ROW 1.71 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     vend_name AT ROW 1.71 COL 35 COLON-ALIGNED NO-LABEL
     ap-chk.bank-code AT ROW 2.91 COL 19 COLON-ALIGNED
          LABEL "Bank"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     bank_name AT ROW 2.91 COL 35 COLON-ALIGNED NO-LABEL
     ap-chk.check-no AT ROW 4.57 COL 19 COLON-ALIGNED
          LABEL "Check No"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     ap-chk.check-date AT ROW 4.57 COL 52 COLON-ALIGNED
          LABEL "Check Date"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
     ap-chk.check-amt AT ROW 4.57 COL 89 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
     RECT-2 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.ap-chk
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
         HEIGHT             = 17.14
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ap-chk.bank-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bank_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ap-chk.check-amt IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN ap-chk.check-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ap-chk.check-no IN FRAME F-Main
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

&Scoped-define SELF-NAME ap-chk.bank-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-chk.bank-code V-table-Win
ON LEAVE OF ap-chk.bank-code IN FRAME F-Main /* Bank */
DO:
   {VALIDATE/bank.i ap-chk.bank-code bank_name:SCREEN-VALUE}

   IF adm-adding-record THEN ap-chk.check-no:SCREEN-VALUE = string(bank.last-chk + 1).
   lv-bank-acct = bank.actnum.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-chk.check-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-chk.check-no V-table-Win
ON LEAVE OF ap-chk.check-no IN FRAME F-Main /* Check No */
DO:
   IF LASTKEY = -1 THEN RETURN.
   {&methods/lValidateError.i YES}
   IF INPUT ap-chk.check-no <> 0 THEN DO:
      FIND FIRST ap-pay WHERE ap-pay.company = g_company
                        AND ap-pay.check-no = INPUT ap-chk.check-no
                        AND (ap-pay.check-act = lv-bank-acct OR 
                             ap-pay.bank-code = INPUT ap-chk.bank-code)
                        AND ap-pay.posted 
                        NO-LOCK NO-ERROR.
      IF AVAIL ap-pay THEN DO:
         MESSAGE "Check Number " ap-chk.check-no:SCREEN-VALUE " has already been posted." SKIP
                 "Please enter different check number."
                 VIEW-AS ALERT-BOX ERROR .
         RETURN NO-APPLY.
      END.


   END.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-chk.vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-chk.vend-no W-Win
ON HELP OF ap-chk.vend-no IN FRAME F-Main /* Vendor# */
DO:
    DEF VAR char-val AS cha NO-UNDO.
    APPLY "entry" TO {&self-name}.
    RUN windows/l-vendno.w (g_company, "A", FOCUS:SCREEN-VALUE,OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN ap-chk.vend-no:SCREEN-VALUE = ENTRY(1,char-val)
                                vend_name:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME ap-chk.vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-chk.vend-no V-table-Win
ON LEAVE OF ap-chk.vend-no IN FRAME F-Main /* Vendor */
DO:

   {VALIDATE/vend.i ap-chk.vend-no vend_name:SCREEN-VALUE}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-chk.vend-no V-table-Win
ON VALUE-CHANGED OF ap-chk.vend-no IN FRAME F-Main /* Vendor */
DO:

/* Task# 04030308
    IF ap-chk.vend-no:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST vend WHERE vend.company = g_company
                      AND vend.vend-no BEGINS ap-chk.vend-no:SCREEN-VALUE
                       NO-LOCK NO-ERROR.
        IF AVAIL vend THEN ASSIGN ap-chk.vend-no:SCREEN-VALUE = vend.vend-no
                              vend_name:SCREEN-VALUE = vend.NAME.
    END.
*/

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
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

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
  {src/adm/template/row-list.i "ap-chk"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ap-chk"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-check V-table-Win 
PROCEDURE delete-check :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAIL ap-chk THEN DO:
     FIND FIRST ap-sel WHERE ap-sel.company = ap-chk.company AND
                          ap-sel.check-no = ap-chk.check-no AND
                          ap-sel.man-check = ap-chk.man-check NO-LOCK NO-ERROR.
     IF NOT AVAIL ap-sel THEN DO:
       FIND CURRENT ap-chk NO-ERROR.
       IF AVAIL ap-chk THEN DELETE ap-chk.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-old-check-no   LIKE ap-sel.check-no   NO-UNDO.
  DEF VAR lv-old-check-date LIKE ap-sel.check-date NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   lv-old-check-no   = ap-chk.check-no
   lv-old-check-date = ap-chk.check-date.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF lv-old-check-no   NE ap-chk.check-no   OR
     lv-old-check-date NE ap-chk.check-date THEN
  FOR EACH ap-sel
      WHERE ap-sel.company   EQ ap-chk.company
        AND ap-sel.vend-no   EQ ap-chk.vend-no
        AND ap-sel.man-check EQ ap-chk.man-check
        AND ap-sel.bank-code EQ ap-chk.bank-code
        AND ap-sel.check-no  EQ lv-old-check-no:
    ASSIGN
     ap-sel.check-no = ap-chk.check-no
     ap-sel.pre-date = ap-chk.check-date.
  END.

  FIND FIRST bank WHERE bank.company = g_company
                         AND bank.bank-code = ap-chk.bank-code NO-ERROR.
  IF AVAIL bank THEN DO:
     IF ap-chk.check-no > bank.last-chk THEN bank.last-chk = ap-chk.check-no.
     ap-chk.check-act = bank.actnum.
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
  FOR EACH bf-chk NO-LOCK BY bf-chk.c-no DESCENDING:
      X = bf-chk.c-no.
      LEAVE.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST ap-ctrl WHERE ap-ctrl.company = g_company NO-LOCK NO-ERROR.
  FIND FIRST bank WHERE bank.company = g_company
                    AND bank.actnum = ap-ctrl.cash-act NO-LOCK NO-ERROR.
  ASSIGN ap-chk.bank-code = IF AVAIL bank THEN bank.bank-code ELSE ""
         ap-chk.check-no = IF AVAIL bank THEN bank.last-chk + 1 ELSE 0 
         ap-chk.man-check = YES   
         ap-chk.check-date = TODAY
         ap-chk.c-no = X + 1
         ap-chk.company = g_company         
         .

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
  DEF VAR ld-tmp-amt AS DEC NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL ap-chk THEN DO:
     FIND FIRST vend WHERE vend.company = g_company
                       AND vend.vend-no = ap-chk.vend-no NO-LOCK NO-ERROR.
     FIND FIRST bank WHERE bank.company = g_company
                       AND bank.bank-code = ap-chk.bank-code NO-LOCK NO-ERROR.
  END.

  ASSIGN
   vend_name = IF AVAIL vend THEN vend.NAME ELSE ""
   bank_name = IF AVAIL bank THEN bank.bank-NAME ELSE "".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL ap-chk THEN DO:  
     ld-tmp-amt = 0.
     FOR EACH bf-sel WHERE bf-sel.company = ap-chk.company
                    AND bf-sel.vend-no = ap-chk.vend-no
                    AND bf-sel.man-check = ap-chk.man-check
                    AND bf-sel.bank-code = ap-chk.bank-code
                    AND bf-sel.check-no = ap-chk.check-no NO-LOCK:
         ld-tmp-amt = ld-tmp-amt + bf-sel.amt-paid.
     END.
     ap-chk.check-amt:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(ld-tmp-amt).    
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

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
     {VALIDATE/vend-upd.i ap-chk.vend-no vend_name:SCREEN-VALUE}
     {VALIDATE/bank.i ap-chk.bank-code bank_name:SCREEN-VALUE}
     IF INPUT ap-chk.check-no <> 0 THEN DO:
        {&methods/lValidateError.i YES}
        FIND FIRST ap-pay WHERE ap-pay.company = g_company
                        AND ap-pay.check-no = INPUT ap-chk.check-no
                        AND (ap-pay.check-act = lv-bank-acct OR 
                             ap-pay.bank-code = INPUT ap-chk.bank-code)
                        AND ap-pay.posted 
                        NO-LOCK NO-ERROR.
        IF AVAIL ap-pay THEN DO:
           MESSAGE "Check Number " ap-chk.check-no:SCREEN-VALUE " has already been posted." SKIP
                 "Please enter different check number."
                 VIEW-AS ALERT-BOX ERROR .
           APPLY "entry" TO ap-chk.check-no.
           RETURN NO-APPLY.
        END.
	{&methods/lValidateError.i NO}
     END.
  END.
  ll-new-record = adm-new-record.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
   IF ll-new-record THEN DO:       
      RUN dispatch ('row-changed').   /* not to have inser-row error for the first line of the first header */
      DEF VAR char-hdl AS cha NO-UNDO.
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"adding-line-target",OUTPUT char-hdl).
      RUN auto-line-add IN WIDGET-HANDLE(char-hdl).

   END.
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
 DEF INPUT PARAM ip-recid AS RECID NO-UNDO.

 FIND ap-chk WHERE recid(ap-chk) = ip-recid NO-LOCK.
 /*DISPLAY {&DISPLAYED-FIELDS} WITH FRAME {&FRAME-NAME}.*/
 RUN dispatch('display-fields').

 RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, "record-source", OUTPUT char-hdl).
 /*RUN redisp-browser IN WIDGET-HANDLE(char-hdl). */

 RUN reopen-query IN WIDGET-HANDLE(char-hdl) (RECID(ap-chk)) . 

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
  {src/adm/template/sndkycas.i "check-no" "ap-sel" "check-no"}
  {src/adm/template/sndkycas.i "company" "ap-sel" "company"}

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
  {src/adm/template/snd-list.i "ap-chk"}

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

