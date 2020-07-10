&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: viewers/APIOutBound.w

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
{custom/globdefs.i}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE hdFileSysProcs       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdOutboundProcs      AS HANDLE    NO-UNDO.
DEFINE VARIABLE lCreated             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRequestTypeList     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRequestVerbList     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRequestDataTypeList AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCopyAPIOutbound     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iSourceAPIOutboundID AS INTEGER   NO-UNDO.
DEFINE VARIABLE lSuperAdmin          AS LOGICAL   NO-UNDO.

/* The below variables are used in run_link.i */
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO.

RUN system/FileSysProcs.p PERSISTENT SET hdFileSysProcs.
RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

DEFINE VARIABLE hdPgmMstrSecur AS HANDLE NO-UNDO.
RUN system/PgmMstrSecur.p PERSISTENT SET hdPgmMstrSecur.

RUN epCanAccess IN hdPgmMstrSecur (
    INPUT  "viewers/APIOutbound.w", /* Program Name */
    INPUT  "",                     /* Function */
    OUTPUT lSuperAdmin
    ).
    
DELETE PROCEDURE hdPgmMstrSecur.

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
&Scoped-define EXTERNAL-TABLES APIOutbound
&Scoped-define FIRST-EXTERNAL-TABLE APIOutbound


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR APIOutbound.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS APIOutbound.saveFileFolder ~
APIOutbound.saveFile APIOutbound.userName APIOutbound.password ~
APIOutbound.requestHandler APIOutbound.responseHandler 
&Scoped-define ENABLED-TABLES APIOutbound
&Scoped-define FIRST-ENABLED-TABLE APIOutbound
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 edDescription ~
edEndPoint edRequestData 
&Scoped-Define DISPLAYED-FIELDS APIOutbound.apiID APIOutbound.clientID ~
APIOutbound.saveFileFolder APIOutbound.saveFile APIOutbound.userName ~
APIOutbound.password APIOutbound.requestHandler APIOutbound.responseHandler 
&Scoped-define DISPLAYED-TABLES APIOutbound
&Scoped-define FIRST-DISPLAYED-TABLE APIOutbound
&Scoped-Define DISPLAYED-OBJECTS fiAPIType tgInactive edDescription ~
edEndPoint cbRequestType cbRequestVerb cbRequestDataType tgSSLEnabled ~
cbAuthType edRequestData fiInactive 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS APIOutbound.apiID APIOutbound.clientID 
&Scoped-define ADM-ASSIGN-FIELDS APIOutbound.saveFileFolder 

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
DEFINE VARIABLE cbAuthType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Authentication Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "none","basic","bearer" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE cbRequestDataType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Request Data Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 17 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE cbRequestType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Request Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 20.2 BY 1 NO-UNDO.

DEFINE VARIABLE cbRequestVerb AS CHARACTER FORMAT "X(256)":U 
     LABEL "Request Verb" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE edDescription AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 132.2 BY 1.91
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE edEndPoint AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 132.2 BY 1.91
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE edRequestData AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 132 BY 4
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiAPIType AS CHARACTER FORMAT "X(6)":U 
     LABEL "Type" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE fiInactive AS CHARACTER FORMAT "X(256)":U INITIAL "Inactive" 
      VIEW-AS TEXT 
     SIZE 11 BY .81
     BGCOLOR 10 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 154.6 BY 8.05.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 154.8 BY 1.67.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 155 BY 1.62.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 155 BY 4.57.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 157 BY 18.57.

DEFINE VARIABLE tgInactive AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.4 BY .81
     BGCOLOR 10 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE tgSSLEnabled AS LOGICAL INITIAL no 
     LABEL "Enable SSL" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiAPIType AT ROW 1.24 COL 144.2 COLON-ALIGNED WIDGET-ID 74
     APIOutbound.apiID AT ROW 2.57 COL 21 COLON-ALIGNED WIDGET-ID 2
          LABEL "API ID"
          VIEW-AS FILL-IN 
          SIZE 40.4 BY 1
          BGCOLOR 3 FGCOLOR 15 
     APIOutbound.clientID AT ROW 2.57 COL 78 COLON-ALIGNED WIDGET-ID 6
          LABEL "Client ID"
          VIEW-AS FILL-IN 
          SIZE 40.4 BY 1
          BGCOLOR 3 FGCOLOR 15 
     tgInactive AT ROW 2.67 COL 126.6 WIDGET-ID 28
     edDescription AT ROW 3.86 COL 22.8 NO-LABEL WIDGET-ID 62
     edEndPoint AT ROW 6 COL 22.8 NO-LABEL WIDGET-ID 34
     APIOutbound.saveFileFolder AT ROW 8.05 COL 40 WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 94.8 BY 1
          BGCOLOR 15 FGCOLOR 0 
     APIOutbound.saveFile AT ROW 8.1 COL 22.8 WIDGET-ID 60
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .81
          BGCOLOR 15 FGCOLOR 0 
     cbRequestType AT ROW 9.24 COL 20.8 COLON-ALIGNED WIDGET-ID 66
     cbRequestVerb AT ROW 9.24 COL 64.4 COLON-ALIGNED WIDGET-ID 42
     cbRequestDataType AT ROW 9.24 COL 111.2 COLON-ALIGNED WIDGET-ID 40
     tgSSLEnabled AT ROW 9.33 COL 138.8 WIDGET-ID 38
     APIOutbound.userName AT ROW 11.24 COL 21 COLON-ALIGNED WIDGET-ID 24
          LABEL "Username"
          VIEW-AS FILL-IN 
          SIZE 30.8 BY 1
          BGCOLOR 15 FGCOLOR 0 
     APIOutbound.password AT ROW 11.24 COL 78 COLON-ALIGNED WIDGET-ID 14 PASSWORD-FIELD 
          LABEL "Password" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 30.8 BY 1
          BGCOLOR 15 FGCOLOR 0 
     cbAuthType AT ROW 11.24 COL 137 COLON-ALIGNED WIDGET-ID 32
     APIOutbound.requestHandler AT ROW 13.24 COL 21 COLON-ALIGNED WIDGET-ID 18
          LABEL "Request Handler" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 54 BY 1
          BGCOLOR 15 FGCOLOR 0 
     APIOutbound.responseHandler AT ROW 13.24 COL 98.6 COLON-ALIGNED WIDGET-ID 22
          LABEL "Response Handler" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 55 BY 1
          BGCOLOR 15 FGCOLOR 0 
     edRequestData AT ROW 15 COL 23 NO-LABEL WIDGET-ID 48
     fiInactive AT ROW 2.67 COL 128 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     "Description:" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 4.38 COL 8.8 WIDGET-ID 64
     "End Point:" VIEW-AS TEXT
          SIZE 11.6 BY .62 AT ROW 6.71 COL 11.2 WIDGET-ID 36
     "Request Data:" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 16.52 COL 6 WIDGET-ID 50
     RECT-1 AT ROW 2.48 COL 2 WIDGET-ID 26
     RECT-2 AT ROW 10.81 COL 2 WIDGET-ID 30
     RECT-3 AT ROW 12.91 COL 2 WIDGET-ID 44
     RECT-4 AT ROW 14.81 COL 2 WIDGET-ID 46
     RECT-7 AT ROW 1 COL 1 WIDGET-ID 54
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.APIOutbound
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
         HEIGHT             = 18.62
         WIDTH              = 157.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

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

/* SETTINGS FOR FILL-IN APIOutbound.apiID IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR COMBO-BOX cbAuthType IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbRequestDataType IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbRequestType IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbRequestVerb IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN APIOutbound.clientID IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
ASSIGN 
       edDescription:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       edEndPoint:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       edRequestData:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fiAPIType IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiInactive IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN APIOutbound.password IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR RECTANGLE RECT-7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN APIOutbound.requestHandler IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN APIOutbound.responseHandler IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN APIOutbound.saveFileFolder IN FRAME F-Main
   ALIGN-L 2                                                            */
/* SETTINGS FOR TOGGLE-BOX tgInactive IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tgSSLEnabled IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN APIOutbound.userName IN FRAME F-Main
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

&Scoped-define SELF-NAME APIOutbound.clientID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL APIOutbound.clientID V-table-Win
ON LEAVE OF APIOutbound.clientID IN FRAME F-Main /* Client ID */
DO:
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.    
    
    IF SELF:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "Client ID cannot be empty"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.        
    END.
        
    RUN Outbound_ValidateClientID IN hdOutboundProcs (
        INPUT  g_company,
        INPUT  SELF:SCREEN-VALUE,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).    
   
    IF NOT lSuccess THEN DO:
        MESSAGE "Client ID '" + APIOutbound.clientID:SCREEN-VALUE + "' is not available." SKIP
            "Do you want to create the client record?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lContinue AS LOGICAL.
        IF lContinue THEN
            RUN Outbound_CreateAPIClient IN hdOutboundProcs (
                INPUT g_company,
                INPUT APIOutbound.clientID:SCREEN-VALUE
                ).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME APIOutbound.saveFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL APIOutbound.saveFile V-table-Win
ON VALUE-CHANGED OF APIOutbound.saveFile IN FRAME F-Main /* Save File */
DO:

    IF {&SELF-NAME}:CHECKED THEN
        saveFileFolder:SENSITIVE = TRUE.
    ELSE DO:
        ASSIGN
            saveFileFolder:SENSITIVE = FALSE 
            saveFileFolder:SCREEN-VALUE = ""
            .
    END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME APIOutbound.saveFileFolder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL APIOutbound.saveFileFolder V-table-Win
ON HELP OF APIOutbound.saveFileFolder IN FRAME F-Main /* Saved File Folder */
DO:
    DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValid    AS LOGICAL  NO-UNDO.
   
    SYSTEM-DIALOG
    GET-DIR cFileName 
    TITLE "Select Directory"
    UPDATE lvalid.
      
    IF lValid THEN 
        SELF:SCREEN-VALUE = cFileName.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgInactive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgInactive V-table-Win
ON VALUE-CHANGED OF tgInactive IN FRAME F-Main
DO:
    IF SELF:CHECKED THEN
        ASSIGN
            SELF:BGCOLOR       = 12
            fiInactive:BGCOLOR = 12
            fiInactive:FGCOLOR = 15
            .
    ELSE
        ASSIGN
            SELF:BGCOLOR       = 10
            fiInactive:BGCOLOR = 10
            fiInactive:FGCOLOR = 0
            .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  {sys/inc/f3help.i}
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
  {src/adm/template/row-list.i "APIOutbound"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "APIOutbound"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pSetDefaults.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pUpdateFields.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN            
        lCopyAPIOutbound     = FALSE
        iSourceAPIOutboundID = 0
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record V-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    ASSIGN
        lCopyAPIOutbound     = TRUE
        iSourceAPIOutboundID = APIOutbound.apiOutboundID
        .
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    /* APIs when copied should not allow a user to update the apiID
       and empty the client id field for the user to enter */
    ASSIGN
        APIOutbound.apiID:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
        APIOutbound.clientID:SCREEN-VALUE                  = ""
        fiAPIType:SCREEN-VALUE                             = "Custom"
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {custom/askdel.i}
    
    FOR EACH APIOutboundDetail EXCLUSIVE-LOCK
        WHERE APIOutboundDetail.apiOutboundID EQ APIOutbound.apiOutboundID:
        DELETE APIOutboundDetail.
    END.
    
    FOR EACH APIOutboundTrigger EXCLUSIVE-LOCK
        WHERE APIOutboundTrigger.apiOutboundID EQ APIOutbound.apiOutboundID:
        DELETE APIOutboundTrigger.
    END.
        
    /* Dispatch standard ADM method.                             */    
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
            
    /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */    
    RUN pDisableFields.    
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
    RUN pDisplayFields.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable V-table-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pEnableFields.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.    
    DEFINE VARIABLE cRowState AS CHARACTER NO-UNDO.
    
    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

    RUN pGetRowState (
        OUTPUT cRowState
        ).

    /* Code placed here will execute AFTER standard behavior.    */
    {methods/run_link.i "TABLEIO-SOURCE" "set-buttons" "(INPUT cRowState)"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pFieldValidations (
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).

    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN.        
    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
    
    IF RETURN-VALUE = "ADM-ERROR":U THEN 
        RETURN "ADM-ERROR":U.

    IF lCopyAPIOutbound THEN DO:
        RUN Outbound_CopyAPIDependencies IN hdOutboundProcs (
            INPUT iSourceAPIOutboundID,
            INPUT APIOutbound.apiOutboundID
            ).
        
        ASSIGN
            lCopyAPIOutbound     = FALSE
            iSourceAPIOutboundID = 0
            .
        
        RUN dispatch (
            INPUT "row-changed"
            ).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisableFields V-table-Win 
PROCEDURE pDisableFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.

    ASSIGN
        tgInactive:SENSITIVE        = FALSE
        cbRequestType:SENSITIVE     = FALSE
        cbRequestVerb:SENSITIVE     = FALSE
        cbRequestDataType:SENSITIVE = FALSE
        tgSSLEnabled:SENSITIVE      = FALSE
        cbAuthType:SENSITIVE        = FALSE
        edEndPoint:READ-ONLY        = TRUE
        edDescription:READ-ONLY     = TRUE
        edRequestData:READ-ONLY     = TRUE
        .

    {methods/run_link.i "CONTAINER-SOURCE" "SetUpdateEnd"}                            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayFields V-table-Win 
PROCEDURE pDisplayFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.

    IF AVAILABLE APIOutbound THEN
        ASSIGN
            tgInactive:CHECKED             = APIOutbound.Inactive
            edEndPoint:SCREEN-VALUE        = APIOutbound.endPoint
            edDescription:SCREEN-VALUE     = APIOutbound.description
            cbRequestType:SCREEN-VALUE     = APIOutbound.requestType
            cbRequestVerb:SCREEN-VALUE     = IF APIOutbound.requestVerb EQ "" THEN
                                                 " "
                                             ELSE
                                                 APIOutbound.requestVerb
            cbRequestDataType:SCREEN-VALUE = IF APIOutbound.requestDataType EQ "" THEN
                                                 " "
                                             ELSE
                                                 APIOutbound.requestDataType
            tgSSLEnabled:CHECKED           = APIOutbound.isSSLEnabled
            cbAuthType:SCREEN-VALUE        = APIOutbound.authType
            edRequestData:SCREEN-VALUE     = STRING(APIOutbound.requestData)
            fiAPIType:SCREEN-VALUE         = IF APIOutbound.apiOutboundID GT 5000 THEN
                                                 "Custom"
                                             ELSE
                                                 "System"            
            .
    ELSE
        ASSIGN
            edEndPoint:SCREEN-VALUE        = ""
            edDescription:SCREEN-VALUE     = ""
            edRequestData:SCREEN-VALUE     = ""
            fiAPIType:SCREEN-VALUE         = ""
            .
    
    /* Changes the background color of the toggle box depending on the value */
    APPLY "VALUE-CHANGED" TO tgInactive.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEnableFields V-table-Win 
PROCEDURE pEnableFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
            
    ASSIGN
        tgInactive:SENSITIVE        = TRUE
        cbRequestType:SENSITIVE     = TRUE
        cbRequestVerb:SENSITIVE     = TRUE
        cbRequestDataType:SENSITIVE = TRUE
        tgSSLEnabled:SENSITIVE      = TRUE
        cbAuthType:SENSITIVE        = TRUE
        edEndPoint:READ-ONLY        = FALSE
        edRequestData:READ-ONLY     = FALSE
        edDescription:READ-ONLY     = FALSE
        .


    /* Not a super user */
    IF NOT lSuperAdmin THEN
        ASSIGN
            APIOutbound.requestHandler:SENSITIVE  = FALSE
            APIOutbound.responseHandler:SENSITIVE = FALSE
            cbRequestDataType:SENSITIVE           = FALSE
            .

    {methods/run_link.i "CONTAINER-SOURCE" "SetUpdateBegin"}                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFieldValidations V-table-Win 
PROCEDURE pFieldValidations :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
        
    IF APIOutbound.apiID:SCREEN-VALUE EQ "" THEN DO:
        opcMessage = "API ID cannot be empty".
        RETURN.
    END.

    IF APIOutbound.clientID:SCREEN-VALUE EQ "" THEN DO:
        opcMessage = "Client ID cannot be empty".
        RETURN.
    END.    

    RUN Outbound_ValidateClientID IN hdOutboundProcs (
        INPUT  g_company,
        INPUT  APIOutbound.clientID:SCREEN-VALUE,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).    
   
    /* Alert the user that a new apiClient record will be created */
    IF NOT oplSuccess and adm-new-record THEN DO:
        MESSAGE "Client ID '" + APIOutbound.clientID:SCREEN-VALUE + "' is not available." SKIP
            "A new record with Client ID '" + APIOutbound.clientID:SCREEN-VALUE + "' will be created."
            VIEW-AS ALERT-BOX INFORMATION.
        RUN Outbound_CreateAPIClient IN hdOutboundProcs (
            INPUT g_company,
            INPUT APIOutbound.clientID:SCREEN-VALUE
            ).
    END.
    
    IF cbRequestType:SCREEN-VALUE EQ ? OR cbRequestType:SCREEN-VALUE EQ "" THEN DO:
        opcMessage = "Request Type cannot be empty".
        RETURN.
    END.
    
    IF saveFile:CHECKED THEN DO:
        IF saveFileFolder:SCREEN-VALUE EQ "" THEN DO:
            opcMessage =  "File location cannot be empty".
            RETURN.
        END.    
    
        RUN FileSys_CreateDirectory IN hdFileSysProcs (
            INPUT  saveFileFolder:SCREEN-VALUE,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.
        
        IF NOT oplSuccess THEN
            RETURN.   
    END.             
    oplSuccess = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetRowState V-table-Win 
PROCEDURE pGetRowState :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcRowState      AS CHARACTER NO-UNDO.

    IF AVAILABLE APIOutbound THEN DO:
        IF APIOutbound.apiOutboundID GT 5000 THEN
            opcRowState = "no-add".
        ELSE
            opcRowState = "copy-only".
    END.
    ELSE
        opcRowState = "disable-all".

    IF lSuperAdmin THEN DO:
        IF AVAILABLE APIoutbound THEN
            opcRowState = "initial".
        ELSE
            opcRowState = "add-only".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit V-table-Win 
PROCEDURE pInit PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN Outbound_GetRequestTypeList In hdOutboundProcs (
        OUTPUT cRequestTypeList
        ).

    RUN Outbound_GetRequestDataTypeList In hdOutboundProcs (
        OUTPUT cRequestDataTypeList
        ).

    RUN Outbound_GetRequestVerbList In hdOutboundProcs (
        OUTPUT cRequestVerbList
        ).
        
    ASSIGN
        cbRequestType:LIST-ITEMS     = cRequestTypeList
        cbRequestDataType:LIST-ITEMS = " " + "," + cRequestDataTypeList
        cbRequestVerb:LIST-ITEMS     = " " + "," + cRequestVerbList
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetDefaults V-table-Win 
PROCEDURE pSetDefaults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.

    ASSIGN
        tgInactive:CHECKED             = TRUE
        edEndPoint:SCREEN-VALUE        = ""
        cbRequestType:SCREEN-VALUE     = ENTRY(1,cRequestTypeList)
        cbRequestVerb:SCREEN-VALUE     = ENTRY(1,cRequestVerbList)
        cbRequestDataType:SCREEN-VALUE = ENTRY(1,cRequestDataTypeList)
        tgSSLEnabled:CHECKED           = FALSE
        cbAuthType:SCREEN-VALUE        = "basic"
        edRequestData:SCREEN-VALUE     = ""
        saveFile:CHECKED               = FALSE
        edDescription:SCREEN-VALUE     = ""
        fiAPIType:SCREEN-VALUE         = "Custom"
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateFields V-table-Win 
PROCEDURE pUpdateFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.

    IF AVAILABLE APIOutbound THEN    
        ASSIGN
            APIOutbound.company         = g_company
            APIOutbound.Inactive        = tgInactive:CHECKED
            APIOutbound.isSSLEnabled    = tgSSLEnabled:CHECKED
            APIOutbound.endPoint        = edEndPoint:SCREEN-VALUE
            APIOutbound.requestType     = IF cbRequestType:SCREEN-VALUE EQ " " OR cbRequestType:SCREEN-VALUE EQ ? THEN 
                                              ""
                                          ELSE
                                              cbRequestType:SCREEN-VALUE
            APIOutbound.requestVerb     = IF cbRequestVerb:SCREEN-VALUE EQ " " OR cbRequestVerb:SCREEN-VALUE EQ ? THEN
                                              ""
                                          ELSE
                                              cbRequestVerb:SCREEN-VALUE
            APIOutbound.requestDataType = IF cbRequestDataType:SCREEN-VALUE EQ " " OR cbRequestDataType:SCREEN-VALUE EQ ? THEN
                                              ""
                                          ELSE
                                              cbRequestDataType:SCREEN-VALUE 
            APIOutbound.authType        = cbAuthType:SCREEN-VALUE
            APIOutbound.requestData     = edRequestData:SCREEN-VALUE
            APIOutbound.description     = edDescription:SCREEN-VALUE
            .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectAndCopyAPIOutbound V-table-Win 
PROCEDURE SelectAndCopyAPIOutbound :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
    DEFINE VARIABLE cAPIID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cClientID    AS CHARACTER NO-UNDO.

    RUN system/openlookup.p (
        "",  /* company */ 
        "",  /* lookup field */
        123,   /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF lookupField NE "" THEN DO:
        ASSIGN
            cAPIID    = IF NUM-ENTRIES(returnFields,"|") GE 2 THEN
                            ENTRY(2, returnFields, "|")
                        ELSE
                            ""
            cClientId = IF NUM-ENTRIES(returnFields, "|") GE 4 THEN
                            ENTRY(4, returnFields, "|")
                        ELSE
                            ""           
            .
        
        FIND FIRST APIOutbound NO-LOCK
             WHERE APIOutbound.company  EQ g_company
               AND APIOutbound.apiID    EQ cAPIID
               AND APIOutbound.clientID EQ cClientID
             NO-ERROR.
        IF AVAILABLE APIOutbound THEN DO:           
            RUN dispatch (
                INPUT "row-changed"
                ).

            RUN dispatch (
                INPUT "local-copy-record"
                ).
        END.
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
  {src/adm/template/snd-list.i "APIOutbound"}

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

