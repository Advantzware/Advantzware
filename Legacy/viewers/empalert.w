&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/empalert.w

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

DEFINE VARIABLE     ip-rec_key  AS CHAR     NO-UNDO.
DEFINE SHARED VAR   vrEmpAlert  AS RECID    NO-UNDO.

DEF BUFFER b-empalert FOR empalert.

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
&Scoped-define EXTERNAL-TABLES empalert
&Scoped-define FIRST-EXTERNAL-TABLE empalert
&scoped-define empalert-layout empalert-layout      /* for method enable */

/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR empalert.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS empalert.user-id 
&Scoped-define ENABLED-TABLES empalert
&Scoped-define FIRST-ENABLED-TABLE empalert
&Scoped-Define ENABLED-OBJECTS RECT-1
&Scoped-Define DISPLAYED-FIELDS empalert.user-id 
&Scoped-define DISPLAYED-TABLES empalert
&Scoped-define FIRST-DISPLAYED-TABLE empalert
&Scoped-Define DISPLAYED-OBJECTS fi_username tb_pricnt

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdvancedNotice V-table-Win 
FUNCTION AdvancedNotice RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi_username AS CHARACTER FORMAT "X(256)":U 
     LABEL "User Name" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 11.43.

DEFINE VARIABLE tb_pricnt AS LOGICAL INITIAL no 
     LABEL "Primary Contact" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     empalert.user-id AT ROW 2.91 COL 15 COLON-ALIGNED WIDGET-ID 2
          LABEL "User ID"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     fi_username AT ROW 4.1 COL 15 COLON-ALIGNED WIDGET-ID 4
     tb_pricnt AT ROW 6.29 COL 17 WIDGET-ID 6
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.empalert
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
         HEIGHT             = 11.76
         WIDTH              = 63.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer3.i}

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

/* SETTINGS FOR FILL-IN fi_username IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN empalert.user-id IN FRAME F-Main
   EXP-LABEL                                                            */
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

&Scoped-define SELF-NAME empalert.user-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL empalert.user-id V-table-Win
ON HELP OF empalert.user-id IN FRAME F-Main /* User ID */
DO:
   def var char-val as cha no-undo.
    {&methods/lValidateError.i YES}
    run windows/l-users.w (self:screen-value, output char-val).
    if char-val <> "" then 
       ASSIGN
          empalert.USER-ID:screen-value = entry(1,char-val)
          fi_username:SCREEN-VALUE = ENTRY(2,char-val).
    return no-apply.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_pricnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_pricnt V-table-Win
ON VALUE-CHANGED OF tb_pricnt IN FRAME F-Main /* tb_pricnt */
DO:
    ASSIGN tb_pricnt .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

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
  {src/adm/template/row-list.i "empalert"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "empalert"}

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-layout-fields V-table-Win 
PROCEDURE enable-layout-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:

     ASSIGN tb_pricnt:SENSITIVE = YES .

  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmailNotify V-table-Win 
PROCEDURE EmailNotify :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAM ilQuietMode  AS LOGICAL NO-UNDO.


  IF AVAIL empalert AND NOT CAN-FIND (FIRST reftable NO-LOCK
                     WHERE reftable.rec_key = empalert.table_rec_key
                       AND reftable.CODE    = empalert.rec_key) THEN
     DO:
        CREATE reftable.
        ASSIGN reftable.rec_key   = STRING (empalert.table_rec_key)
               reftable.CODE      = empalert.rec_key.
     END.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"CONTAINER",OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN AdvancedNotice IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  IF AVAIL empalert THEN do:
      IF tb_pricnt:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Yes" THEN
          empalert.spare-char-1  = "Yes" .
      ELSE
          empalert.spare-char-1  = "No" .
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
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/create/empalert.i}

  RUN SetEmailNotify.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* After Standard Behavior. */
  RUN SetEmailNotify.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"CONTAINER",OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN AdvancedNotice IN WIDGET-HANDLE(char-hdl).

  DO WITH FRAME {&FRAME-NAME}:

      IF NOT adm-new-record THEN do:
          IF AVAIL empalert AND empalert.spare-char-1 EQ "YES" THEN
              ASSIGN  tb_pricnt:SCREEN-VALUE = "YES"  
              tb_pricnt = YES.
          ELSE
              ASSIGN tb_pricnt:SCREEN-VALUE = "no" 
                  tb_pricnt = no  .
      END.

     ASSIGN tb_pricnt:SENSITIVE = NO .

     IF AVAIL empalert AND empalert.USER-ID:SCREEN-VALUE NE "" THEN
     DO:
        FIND FIRST users WHERE
             users.USER_id EQ empalert.USER-ID:SCREEN-VALUE
             NO-LOCK NO-ERROR.

        IF AVAIL users THEN
           fi_username:SCREEN-VALUE = users.user_name.
        ELSE
           fi_username:SCREEN-VALUE = "".
     END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-choice AS LOG NO-UNDO.
  DEF VAR ll-new-record AS LOG NO-UNDO.
  DEF BUFFER bf-empalert FOR empalert .

  ll-new-record = adm-new-record.
  {&methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:

     {methods/run_link.i "CONTAINER-SOURCE" "Get-ip-rec_key" "(OUTPUT ip-rec_key)"}

     IF ll-new-record AND CAN-FIND(FIRST b-empalert WHERE
        b-empalert.TABLE_rec_key EQ ip-rec_key AND
        b-empalert.USER-ID EQ empalert.USER-ID:SCREEN-VALUE /*AND
        ROWID(b-empalert) NE ROWID(empalert)*/ ) THEN
        DO:
           MESSAGE "Duplicate Employee Alert."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           RETURN.
        END.

     IF NOT CAN-FIND(FIRST users WHERE
        users.USER_ID EQ empalert.USER-ID:SCREEN-VALUE) THEN
        DO:
           MESSAGE "Invalid User."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY "ENTRY" TO empalert.USER-ID IN FRAME {&FRAME-NAME}.
           RETURN.
        END.

  END.
  {&methods/lValidateError.i NO}
  /* Dispatch standard ADM method.      */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* After Standard Behavior. */

   ASSIGN tb_pricnt:SENSITIVE = NO . 
  RUN SetEmailNotify.
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
  {src/adm/template/snd-list.i "empalert"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetEmailNotify V-table-Win 
PROCEDURE SetEmailNotify :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

    RUN SetNotifyMode.
  END.

  vrEmpAlert = RECID (empalert).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetNotifyMode V-table-Win 
PROCEDURE SetNotifyMode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

    /*IF phone.e_mail:SENSITIVE THEN tbNotice:SENSITIVE = TRUE.
                              ELSE tbNotice:SENSITIVE = FALSE.*/
  END.

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

  RUN EmailNotify (YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdvancedNotice V-table-Win 
FUNCTION AdvancedNotice RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF CAN-FIND (FIRST reftable NO-LOCK
               WHERE reftable.rec_key = empalert.table_rec_key
                 AND reftable.CODE    = empalert.rec_key) 
      THEN RETURN TRUE.
      ELSE RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

