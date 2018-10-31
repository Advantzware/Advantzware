&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/sys-ctrl.w

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

&SCOPED-DEFINE tableName sys-ctrl
&SCOPED-DEFINE nameField {&tableName}.name:SCREEN-VALUE
&SCOPED-DEFINE post-enable post-enable

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}
{custom/gcompany.i}

{sys/ref/sys-ctrl.i}

DEF VAR v-secure AS LOG INIT NO NO-UNDO.
DEF VAR v-secur  AS LOG INIT NO NO-UNDO.
DEF VAR v-valid  AS LOG INIT NO NO-UNDO.
DEF VAR v-msg    AS CHAR FORMAT  "x(100)" NO-UNDO.

/* User can select multiple values for these, and validation is on */
/* each value they select                                          */
DEF VAR gvcMultiSelect AS CHAR NO-UNDO INIT "OEDateChange,SSBOLEMAIL".
DEF VAR hPgmSecurity AS HANDLE NO-UNDO.
DEF VAR lResult AS LOG NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES sys-ctrl
&Scoped-define FIRST-EXTERNAL-TABLE sys-ctrl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR sys-ctrl.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS sys-ctrl.descrip sys-ctrl.module ~
sys-ctrl.char-fld sys-ctrl.date-fld sys-ctrl.dec-fld sys-ctrl.int-fld ~
sys-ctrl.log-fld 
&Scoped-define ENABLED-TABLES sys-ctrl
&Scoped-define FIRST-ENABLED-TABLE sys-ctrl
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS sys-ctrl.name sys-ctrl.descrip ~
sys-ctrl.module sys-ctrl.char-fld sys-ctrl.date-fld sys-ctrl.dec-fld ~
sys-ctrl.int-fld sys-ctrl.log-fld 
&Scoped-define DISPLAYED-TABLES sys-ctrl
&Scoped-define FIRST-DISPLAYED-TABLE sys-ctrl


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS sys-ctrl.name 

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
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 138 BY 9.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     sys-ctrl.name AT ROW 1.24 COL 20 COLON-ALIGNED FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
     sys-ctrl.descrip AT ROW 2.43 COL 20 COLON-ALIGNED FORMAT "x(70)"
          VIEW-AS FILL-IN 
          SIZE 116 BY 1
     sys-ctrl.module AT ROW 3.62 COL 20 COLON-ALIGNED HELP
          ""
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     sys-ctrl.char-fld AT ROW 4.81 COL 20 COLON-ALIGNED
          LABEL "Character Value" FORMAT "x(100)"
          VIEW-AS FILL-IN 
          SIZE 116 BY 1
     sys-ctrl.date-fld AT ROW 6 COL 20 COLON-ALIGNED
          LABEL "Date Value"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     sys-ctrl.dec-fld AT ROW 7.19 COL 20 COLON-ALIGNED
          LABEL "Decimal Value"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     sys-ctrl.int-fld AT ROW 8.38 COL 20 COLON-ALIGNED
          LABEL "Integer Value"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     sys-ctrl.log-fld AT ROW 9.57 COL 20 COLON-ALIGNED
          LABEL "Logical Value"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.sys-ctrl
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
         HEIGHT             = 9.76
         WIDTH              = 138.
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

/* SETTINGS FOR FILL-IN sys-ctrl.char-fld IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN sys-ctrl.date-fld IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN sys-ctrl.dec-fld IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN sys-ctrl.descrip IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN sys-ctrl.int-fld IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN sys-ctrl.log-fld IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN sys-ctrl.module IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN sys-ctrl.name IN FRAME F-Main
   NO-ENABLE 1 EXP-FORMAT                                               */
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

&Scoped-define SELF-NAME sys-ctrl.char-fld
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl.char-fld V-table-Win
ON ENTRY OF sys-ctrl.char-fld IN FRAME F-Main /* Character Value */
DO:
  /*  if sys-ctrl.name:screen-value = "CEMENU" then
       status input "Foldware, Corrware or Both".
  */
   def var ls-name-value as cha form "x(100)" no-undo. 

   status input ''.
   if can-do(name-fld-list,sys-ctrl.name:screen-value) then do:
      ls-name-value = str-init[lookup(sys-ctrl.name:screen-value,name-fld-list)].
      status input ls-name-value.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl.char-fld V-table-Win
ON HELP OF sys-ctrl.char-fld IN FRAME F-Main /* Character Value */
DO:
  {sys/ref/char-fld-help.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl.char-fld V-table-Win
ON LEAVE OF sys-ctrl.char-fld IN FRAME F-Main /* Character Value */
DO:
  STATUS INPUT ''.
  IF LASTKEY NE -1 THEN DO:
    RUN valid-char-fld NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END. 

   IF sys-ctrl.name EQ "SSRMISSUE" THEN 
       RUN ScanTagOnly-Logic.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sys-ctrl.log-fld
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl.log-fld V-table-Win
ON LEAVE OF sys-ctrl.log-fld IN FRAME F-Main /* Logical Value */
DO:

  CASE sys-ctrl.NAME:
      WHEN  "RELCREDT" 
          THEN v-msg = "Credit Checks for Past Due Invoices must be purchased, please call ASI."  .
      /*WHEN "SalesMgmt" 
          THEN v-msg = "Management Reports are available for purchase, please call ASI." .*/
      WHEN  "SalesBudget"  
          THEN v-msg = "Budget Report are available for purchase, please call ASI."  .     
  END CASE.

  IF LASTKEY NE -1 THEN DO:
    RUN valid-log-fld NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i YES}
   IF (sys-ctrl.name EQ "RELCREDT" OR
      /*sys-ctrl.name EQ "SalesMgmt" OR */
      sys-ctrl.name EQ "SalesBudget") THEN DO:       
       RUN check-flg.

       IF NOT v-valid AND 
         NOT sys-ctrl.log-fld
           THEN DO:
           MESSAGE 
              v-msg
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN.
       END.       
   END.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  {src/adm/template/row-list.i "sys-ctrl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "sys-ctrl"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-flg V-table-Win 
PROCEDURE check-flg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-flg AS LOG INIT YES NO-UNDO.

  v-valid = YES.
  
    RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
    RUN epCanAccess IN hPgmSecurity ("viewers/sys-ctrl.w", "", OUTPUT lResult).
    DELETE OBJECT hPgmSecurity.
IF lResult THEN
    ASSIGN v-secure = YES.

  DO WITH FRAME {&frame-name}:    
    IF NOT v-secure                                                       AND
       NOT adm-new-record                                                 AND       
       STRING(sys-ctrl.log-fld,"yes/no") NE sys-ctrl.log-fld:SCREEN-VALUE 
      THEN DO:

      IF sys-ctrl.name EQ "RELCREDT" AND 
         sys-ctrl.log-fld = YES THEN 
         ASSIGN v-secure = YES.
      ELSE
         RUN sys/ref/d-ASIpwd.w (OUTPUT v-secure).

      IF NOT v-secure THEN
        ASSIGN
         v-valid                   = NO
         sys-ctrl.log-fld:SCREEN-VALUE = STRING(sys-ctrl.log-fld,"yes/no").

    END.

/*     IF NOT v-valid THEN APPLY "entry" TO sys-ctrl.log-fld. */

  END.

  ASSIGN
      v-secure = NO
      v-secur  = NO.


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
  DEF VAR li AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF sys-ctrl.name EQ "FGRECPT" THEN DO:
    li = LOOKUP(sys-ctrl.char-fld,str-init[66]).

    RUN update-other-record ("AUTOPOST", li EQ 1).
    RUN update-other-record ("TSPOSTFG", li EQ 2 OR li EQ 5). /* TSPOSTFG or TSPARTS */
  END.

  IF sys-ctrl.name EQ "OEFGUPDT" THEN SUBSTRING(sys-ctrl.char-fld,8,1) = "N".
    {&methods/lValidateError.i YES}
    IF (sys-ctrl.name EQ "RELCREDT" OR
      /*sys-ctrl.name EQ "SalesMgmt" OR */
      sys-ctrl.name EQ "SalesBudget") THEN DO:  

       RUN check-flg.
       IF NOT v-valid THEN DO:
           MESSAGE 
              "Please call ASI, to purchase this functionality."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN NO-APPLY.
       END.
   END.
    {&methods/lValidateError.i NO}
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */        

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  sys-ctrl.company = gcompany. 
  /* same as {methods/triggers/create.i}   */

  ASSIGN sys-ctrl.rec_key = DYNAMIC-FUNCTION("sfGetNextRecKey").

  CREATE rec_key.
  ASSIGN rec_key.rec_key = sys-ctrl.rec_key
         rec_key.table_name = "sys-ctrl".

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  DO WITH FRAME {&FRAME-NAME}:

      IF sys-ctrl.NAME:SCREEN-VALUE  EQ "CHKFMT" OR sys-ctrl.NAME:SCREEN-VALUE  EQ "RelPrint" OR
          sys-ctrl.NAME:SCREEN-VALUE  EQ "BolFmt" OR sys-ctrl.NAME:SCREEN-VALUE  EQ "Ackhead" OR
          sys-ctrl.NAME:SCREEN-VALUE  EQ "POPrint" OR sys-ctrl.NAME:SCREEN-VALUE  EQ "InvPrint" OR
          sys-ctrl.NAME:SCREEN-VALUE  EQ "QuoPrint" OR sys-ctrl.NAME:SCREEN-VALUE EQ "BolFmtX"  THEN
          ASSIGN sys-ctrl.char-fld:LABEL = "Business Form" .
      ELSE IF sys-ctrl.NAME:SCREEN-VALUE  EQ "CasLabel" OR sys-ctrl.NAME:SCREEN-VALUE  EQ "BarDir" OR
                    sys-ctrl.NAME:SCREEN-VALUE  EQ "RMTags" THEN
          ASSIGN sys-ctrl.char-fld:LABEL = "Label Location" .
      ELSE IF sys-ctrl.NAME:SCREEN-VALUE  EQ "PushPin" THEN
          ASSIGN sys-ctrl.char-fld:LABEL = "File Directory" .
      ELSE
          ASSIGN sys-ctrl.char-fld:LABEL  = "Character Value" . 

      IF sys-ctrl.NAME:SCREEN-VALUE  EQ "BarDir" THEN
          ASSIGN sys-ctrl.descrip:LABEL = "File Directory"  .
      ELSE
        ASSIGN sys-ctrl.descrip:LABEL  = "Description" . 

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
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR lv-prev-char-fld LIKE sys-ctrl.char-fld NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */ 
  RUN valid-char-fld NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-log-fld NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  lv-prev-char-fld = sys-ctrl.char-fld.
  IF sys-ctrl.name EQ "RELCREDT" AND sys-ctrl.log-fld:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO" THEN
     ASSIGN sys-ctrl.char-fld:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF sys-ctrl.name EQ "APTAX"              AND
     sys-ctrl.char-fld EQ "NoTax"          AND
     sys-ctrl.char-fld NE lv-prev-char-fld THEN
  FOR EACH po-ord NO-LOCK
      WHERE po-ord.company EQ sys-ctrl.company
        AND po-ord.opened  EQ YES,
      EACH po-ordl
      WHERE po-ordl.company EQ po-ord.company
        AND po-ordl.po-no   EQ po-ord.po-no
      TRANSACTION:
    po-ordl.tax = NO.
  END.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).  

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(sys-ctrl)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-enable V-table-Win 
PROCEDURE post-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF sys-ctrl.NAME:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Autopost" OR
     sys-ctrl.NAME:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "TSPOSTFG" 
  THEN DO:
      DISABLE sys-ctrl.log-fld WITH FRAME {&FRAME-NAME}.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScanTagOnly-Logic V-table-Win 
PROCEDURE ScanTagOnly-Logic :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

      IF sys-ctrl.char-fld:SCREEN-VALUE = "ScanTagOnly" THEN
      ASSIGN sys-ctrl.log-fld:SCREEN-VALUE = "NO"
             sys-ctrl.log-fld:SENSITIVE = NO.
      ELSE
          ASSIGN sys-ctrl.log-fld:SCREEN-VALUE = string(sys-ctrl.log-fld)
                 sys-ctrl.log-fld:SENSITIVE = YES.

  END.


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
  {src/adm/template/snd-list.i "sys-ctrl"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-other-record V-table-Win 
PROCEDURE update-other-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-name    LIKE sys-ctrl.name NO-UNDO.
  DEF INPUT PARAM ip-log-fld LIKE sys-ctrl.log-fld NO-UNDO.

  DEF BUFFER b-sys-ctrl FOR sys-ctrl.


  FIND FIRST b-sys-ctrl
      WHERE b-sys-ctrl.company EQ sys-ctrl.company
        AND b-sys-ctrl.name    EQ ip-name
      NO-ERROR.
  IF AVAIL b-sys-ctrl THEN b-sys-ctrl.log-fld = ip-log-fld.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-char-fld V-table-Win 
PROCEDURE valid-char-fld :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


  DEF VAR thisOne AS CHAR NO-UNDO.
  DEF VAR comp-char-val AS CHAR NO-UNDO.
  DEF VAR cEntryTo AS CHAR NO-UNDO.
  DEF VAR cSingleValue AS CHAR NO-UNDO.
  DEF VAR lValid AS LOG NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR j AS INT NO-UNDO.  
  {methods/lValidateError.i YES}
  lValid = TRUE.
  DO WITH FRAME {&FRAME-NAME}:

    /* Process NK1 options where user can select more than one */
    /* option - validate each option individually              */
    IF LOOKUP(sys-ctrl.NAME:SCREEN-VALUE, gvcMultiSelect) GT 0 
      AND INDEX(sys-ctrl.char-fld:SCREEN-VALUE, ",") GT 0 THEN DO:

        DO i = 1 TO NUM-ENTRIES(sys-ctrl.char-fld:SCREEN-VALUE):

          cSingleValue = ENTRY(i, sys-ctrl.char-fld:SCREEN-VALUE).

          RUN sys/ref/validSysCtrlChar.p 
            (INPUT g_company,
             INPUT g_loc,
             INPUT sys-ctrl.NAME:SCREEN-VALUE,
             INPUT sys-ctrl.char-fld:LABEL,
             INPUT sys-ctrl.log-fld:SCREEN-VALUE,
             INPUT cSingleValue,
             INPUT name-fld-list,
             INPUT (IF LOOKUP(sys-ctrl.NAME:SCREEN-VALUE, name-fld-list) GT 0 THEN str-init[LOOKUP(sys-ctrl.NAME:SCREEN-VALUE, name-fld-list)]
                  ELSE ""),
             OUTPUT cEntryTo,
             OUTPUT lValid).


          IF NOT lValid THEN DO:   
            CASE cEntryTo:
              WHEN "Char" THEN
                APPLY 'ENTRY':U TO {&tableName}.char-fld.
              WHEN "Log" THEN
                APPLY 'ENTRY':U TO {&tableName}.log-fld.
            END CASE.
            LEAVE.
          END. /* if not lvalid */

        END. /* do i = ... */

    END. /* if multiple values to validate */
    ELSE DO:

        RUN sys/ref/validSysCtrlChar.p 
          (INPUT g_company,
           INPUT g_loc,
           INPUT sys-ctrl.NAME:SCREEN-VALUE,
           INPUT sys-ctrl.char-fld:LABEL,
           INPUT sys-ctrl.log-fld:SCREEN-VALUE,
           INPUT sys-ctrl.char-fld:SCREEN-VALUE,
           INPUT name-fld-list,
           INPUT (IF LOOKUP(sys-ctrl.NAME:SCREEN-VALUE, name-fld-list) GT 0 THEN str-init[LOOKUP(sys-ctrl.NAME:SCREEN-VALUE, name-fld-list)]
                  ELSE ""),
           OUTPUT cEntryTo,
           OUTPUT lValid). 
        IF NOT lValid THEN DO:   
          CASE cEntryTo:
            WHEN "Char" THEN
              APPLY 'ENTRY':U TO {&tableName}.char-fld.
            WHEN "Log" THEN
              APPLY 'ENTRY':U TO {&tableName}.log-fld.
          END CASE.
        END. /* Not lvalid */

    END. /* Single value to validate */

    IF NOT lValid THEN
      RETURN ERROR.
  END. /* Do with frame ... */

/*
  IF NOT (sys-ctrl.NAME:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "VendXfer" OR
     sys-ctrl.NAME:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "CustXfer" OR  sys-ctrl.NAME:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "ORDERXFER")
  THEN DO: 
      {sys/ref/valid-char-fld.i}
  END.
      ELSE DO:
          DO WITH FRAME {&FRAME-NAME}:
          ASSIGN comp-char-val = sys-ctrl.char-fld:SCREEN-VALUE.          

          DO J = 1 TO NUM-ENTRIES(comp-char-val):
              ASSIGN thisOne = ENTRY(j,comp-char-val).          

              FIND FIRST company WHERE company.company = thisOne NO-LOCK NO-ERROR.
              IF NOT AVAIL company THEN DO:         
                  MESSAGE   thisOne  "is not a valid company" VIEW-AS ALERT-BOX ERROR.
                  APPLY 'ENTRY':U TO {&tableName}.char-fld.
                  RETURN ERROR.
              END.
          END.   
      END.
  END.


  IF sys-ctrl.name EQ "SSRMISSUE" THEN DO:
      IF sys-ctrl.char-fld:SCREEN-VALUE = "ScanTagOnly" AND
          sys-ctrl.log-fld:SCREEN-VALUE = "yes" THEN DO:
          MESSAGE "Value 'ScanTagOnly' cannot be used with auto-post functionality (logical value YES)."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'ENTRY':U TO {&tableName}.log-fld.
          RETURN ERROR.
      END.
  END.
  */

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-log-fld V-table-Win 
PROCEDURE valid-log-fld :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  {sys/ref/valid-log-fld.i}

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-sys-ctrl-value V-table-Win 
PROCEDURE get-sys-ctrl-value :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opSysName AS CHARACTER NO-UNDO.
    IF AVAIL sys-ctrl THEN
        opSysName = sys-ctrl.NAME.
    ELSE opSysName = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

