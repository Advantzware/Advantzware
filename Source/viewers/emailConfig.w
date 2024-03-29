&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/emailConfig.w

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
&Scoped-define EXTERNAL-TABLES emailConfig
&Scoped-define FIRST-EXTERNAL-TABLE emailConfig


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emailConfig.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS emailConfig.isActive emailConfig.smtpServer ~
emailConfig.smtpPort emailConfig.smtpUser emailConfig.smtpPassword ~
emailConfig.subject emailConfig.attachment emailConfig.body ~
emailConfig.recipientsSendTo emailConfig.recipientsSendCC ~
emailConfig.recipientsSendBcc emailConfig.recipientsReplyTo ~
emailConfig.description
&Scoped-define ENABLED-TABLES emailConfig
&Scoped-define FIRST-ENABLED-TABLE emailConfig
&Scoped-Define DISPLAYED-FIELDS emailConfig.configID emailConfig.isActive ~
emailConfig.smtpServer emailConfig.smtpPort emailConfig.smtpUser ~
emailConfig.smtpPassword emailConfig.subject emailConfig.attachment ~
emailConfig.body emailConfig.recipientsSendTo emailConfig.recipientsSendCC ~
emailConfig.recipientsSendBcc emailConfig.recipientsReplyTo ~
emailConfig.createBy emailConfig.createTime emailConfig.description
&Scoped-define DISPLAYED-TABLES emailConfig
&Scoped-define FIRST-DISPLAYED-TABLE emailConfig


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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 153 BY 18.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     emailConfig.configID AT ROW 1.24 COL 20.6 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     emailConfig.DESCRIPTION AT ROW 1.24 COL 56 COLON-ALIGNED WIDGET-ID 64 FORMAT "x(24)"
          VIEW-AS FILL-IN 
          SIZE 35 BY 1
          BGCOLOR 15 
     emailConfig.isActive AT ROW 1.24 COL 110 WIDGET-ID 62
          VIEW-AS TOGGLE-BOX
          SIZE 13.2 BY 1
     emailConfig.smtpServer AT ROW 2.43 COL 20.6 COLON-ALIGNED WIDGET-ID 52 FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
          BGCOLOR 15 
     emailConfig.smtpPort AT ROW 3.62 COL 20.6 COLON-ALIGNED WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     emailConfig.smtpUser AT ROW 4.81 COL 20.6 COLON-ALIGNED WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 40.4 BY 1
          BGCOLOR 15 
     emailConfig.smtpPassword AT ROW 6 COL 20.6 COLON-ALIGNED WIDGET-ID 48 PASSWORD-FIELD 
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
          BGCOLOR 15 
     emailConfig.subject AT ROW 7.19 COL 8.6 WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 130 BY 1
          BGCOLOR 15 
     emailConfig.attachment AT ROW 8.38 COL 3.6 WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 130 BY 1
          BGCOLOR 15 
     emailConfig.body AT ROW 9.57 COL 22.6 NO-LABEL WIDGET-ID 58
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 130 BY 4
          BGCOLOR 15 
     emailConfig.recipientsSendTo AT ROW 13.86 COL 3 WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 130 BY 1
          BGCOLOR 15 
     emailConfig.recipientsSendCC AT ROW 15.05 COL 2.6 WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 130 BY 1
          BGCOLOR 15 
     emailConfig.recipientsSendBcc AT ROW 16.24 COL 2.2 WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 130 BY 1
          BGCOLOR 15 
     emailConfig.recipientsReplyTo AT ROW 17.43 COL 2.4 WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 130 BY 1
          BGCOLOR 15 
     emailConfig.createBy AT ROW 18.62 COL 75 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 30.8 BY 1
          BGCOLOR 15 
     emailConfig.createTime AT ROW 18.62 COL 121 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 
     "Email Body:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 9.57 COL 11 WIDGET-ID 60
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.emailConfig
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
         HEIGHT             = 18.81
         WIDTH              = 153.
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

/* SETTINGS FOR FILL-IN emailConfig.attachment IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN emailConfig.configID IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN emailConfig.createBy IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN emailConfig.createTime IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN emailConfig.recipientsReplyTo IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN emailConfig.recipientsSendBcc IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN emailConfig.recipientsSendCC IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN emailConfig.recipientsSendTo IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN emailConfig.smtpServer IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN emailConfig.DESCRIPTION IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN emailConfig.subject IN FRAME F-Main
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


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
  {src/adm/template/row-list.i "emailConfig"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "emailConfig"}

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
  {src/adm/template/snd-list.i "emailConfig"}

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

