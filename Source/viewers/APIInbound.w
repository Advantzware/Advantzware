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

  File: viewers/APIInbound.w

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
DEFINE VARIABLE lSuperAdmin AS LOGICAL NO-UNDO.

DEFINE VARIABLE hdPgmMstrSecur AS HANDLE NO-UNDO.
RUN system/PgmMstrSecur.p PERSISTENT SET hdPgmMstrSecur.

RUN epCanAccess IN hdPgmMstrSecur (
    INPUT  "viewers/APIInbound.w", /* Program Name */
    INPUT  "",                     /* Function */
    OUTPUT lSuperAdmin
    ).
    
DELETE PROCEDURE hdPgmMstrSecur.

/* The below variables are used in run_link.i */
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES APIInbound
&Scoped-define FIRST-EXTERNAL-TABLE APIInbound


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR APIInbound.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS APIInbound.apiRoute APIInbound.requestHandler ~
APIInbound.importPath 
&Scoped-define ENABLED-TABLES APIInbound
&Scoped-define FIRST-ENABLED-TABLE APIInbound
&Scoped-Define ENABLED-OBJECTS RECT-27 RECT-28 edDescription edRequestData ~
edResponseData 
&Scoped-Define DISPLAYED-FIELDS APIInbound.apiRoute ~
APIInbound.requestHandler APIInbound.importPath 
&Scoped-define DISPLAYED-TABLES APIInbound
&Scoped-define FIRST-DISPLAYED-TABLE APIInbound
&Scoped-Define DISPLAYED-OBJECTS fiMessage tgInactive edDescription ~
cbRequestDataType cbRequestVerb tgCanBeQueued edRequestData edResponseData 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

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
DEFINE VARIABLE cbRequestDataType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Request Data Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "JSON","XML" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cbRequestVerb AS CHARACTER FORMAT "X(256)":U 
     LABEL "Request Verb" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "POST","GET" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE edDescription AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 120 BY 2.38
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE edRequestData AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 120 BY 4
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE edResponseData AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 120 BY 4
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiMessage AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 143 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 143 BY 7.62.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 143 BY 9.05.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 145 BY 18.57.

DEFINE VARIABLE tgCanBeQueued AS LOGICAL INITIAL no 
     LABEL "Can Be Queued?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tgInactive AS LOGICAL INITIAL no 
     LABEL "Inactive" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.2 BY .81
     BGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiMessage AT ROW 1.24 COL 2 NO-LABEL WIDGET-ID 52
     APIInbound.apiRoute AT ROW 2.67 COL 21 COLON-ALIGNED WIDGET-ID 2
          LABEL "API Route" FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 87 BY 1
          BGCOLOR 15 
     tgInactive AT ROW 2.81 COL 115.4 WIDGET-ID 28
     edDescription AT ROW 3.81 COL 23 NO-LABEL WIDGET-ID 34
     cbRequestDataType AT ROW 6.29 COL 92.8 COLON-ALIGNED WIDGET-ID 40
     cbRequestVerb AT ROW 6.33 COL 21 COLON-ALIGNED WIDGET-ID 42
     tgCanBeQueued AT ROW 7.43 COL 115.4 WIDGET-ID 38
     APIInbound.requestHandler AT ROW 7.48 COL 21 COLON-ALIGNED WIDGET-ID 12
          LABEL "Request Handler" FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 88 BY 1
          BGCOLOR 15 
     APIInbound.importPath AT ROW 8.71 COL 21 COLON-ALIGNED WIDGET-ID 64
          VIEW-AS FILL-IN 
          SIZE 88 BY 1
     edRequestData AT ROW 10.52 COL 23 NO-LABEL WIDGET-ID 48
     edResponseData AT ROW 14.95 COL 23 NO-LABEL WIDGET-ID 54
     "Response Data:" VIEW-AS TEXT
          SIZE 18.2 BY .62 AT ROW 15 COL 4 WIDGET-ID 56
     "Description:" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 3.91 COL 9 WIDGET-ID 36
     "Request Data:" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 10.57 COL 6.2 WIDGET-ID 50
     RECT-27 AT ROW 2.43 COL 2 WIDGET-ID 58
     RECT-28 AT ROW 10.29 COL 2 WIDGET-ID 60
     RECT-29 AT ROW 1 COL 1 WIDGET-ID 62
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1 FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.APIInbound
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
         HEIGHT             = 18.57
         WIDTH              = 145.
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

/* SETTINGS FOR FILL-IN APIInbound.apiRoute IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR COMBO-BOX cbRequestDataType IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbRequestVerb IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       edDescription:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       edRequestData:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       edResponseData:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fiMessage IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR RECTANGLE RECT-29 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN APIInbound.requestHandler IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR TOGGLE-BOX tgCanBeQueued IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tgInactive IN FRAME F-Main
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

&Scoped-define SELF-NAME APIInbound.importPath
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL APIInbound.importPath V-table-Win
ON HELP OF APIInbound.importPath IN FRAME F-Main /* Import Path */
DO:
    DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValid    AS LOGICAL  NO-UNDO.
   
    SYSTEM-DIALOG
    GET-DIR cFileName 
    TITLE "Select path to import files"
    UPDATE lvalid.
      
    IF lValid THEN 
        SELF:SCREEN-VALUE = cFileName.  
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
  {src/adm/template/row-list.i "APIInbound"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "APIInbound"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {custom/askdel.i}
    
    FOR EACH APIInboundDetail EXCLUSIVE-LOCK
       WHERE APIInboundDetail.apiRoute = APIInbound.apiRoute:
        DELETE APIInboundDetail.
    END.
    /* Dispatch standard ADM method.                             */    
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
        
    /* Code placed here will execute AFTER standard behavior.    */
    RUN pUpdateMessageText (
        "Record deleted successfully!",    /* Message Text */
        FALSE,       /* Error */
        FALSE        /* Alert-box*/
        ).
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
    {methods/run_link.i "CONTAINER-SOURCE" "SetUpdateEnd"}    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pEmptyMessagetext.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pEnableFields.
    {methods/run_link.i "CONTAINER-SOURCE" "SetUpdateBegin"}
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
        RUN pUpdateMessageText (
            cMessage,    /* Message Text */
            TRUE,        /* Error */
            FALSE        /* Alert-box*/
            ).
        RETURN.        
    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */        
    cMessage = IF adm-new-record THEN 
                   "Record created successfully!"
               ELSE
                   "Record updated successfully!".
                   
    RUN pUpdateMessageText (
        cMessage,    /* Message Text */
        FALSE,       /* Error */
        FALSE        /* Alert-box*/
        ).
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
        cbRequestVerb:SENSITIVE     = FALSE
        cbRequestDataType:SENSITIVE = FALSE
        tgCanBeQueued:SENSITIVE     = FALSE
        edRequestData:READ-ONLY     = TRUE
        edResponseData:READ-ONLY    = TRUE
        edDescription:READ-ONLY     = TRUE        
        .
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

    IF AVAILABLE APIInbound THEN
        ASSIGN
            tgInactive:CHECKED             = APIInbound.Inactive
            edDescription:SCREEN-VALUE     = APIInbound.description
            cbRequestVerb:SCREEN-VALUE     = APIInbound.requestVerb
            cbRequestDataType:SCREEN-VALUE = APIInbound.requestDataType
            tgCanBeQueued:CHECKED          = APIInbound.canBeQueued
            edResponseData:SCREEN-VALUE    = STRING(APIInbound.responseData)
            edRequestData:SCREEN-VALUE     = STRING(APIInbound.requestData)
            .
    ELSE
        ASSIGN
            edDescription:SCREEN-VALUE  = ""
            edRequestData:SCREEN-VALUE  = ""
            edResponseData:SCREEN-VALUE = ""
            .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEmptyMessageText V-table-Win 
PROCEDURE pEmptyMessageText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.

    fiMessage:SCREEN-VALUE = "".
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
    
    /* Enable only Inactive and Can Be queued for normal users */
    
    ASSIGN
        tgInactive:SENSITIVE        = TRUE
        tgCanBeQueued:SENSITIVE     = TRUE
        .

    IF lSuperAdmin THEN
        ASSIGN        
            cbRequestVerb:SENSITIVE     = TRUE
            cbRequestDataType:SENSITIVE = TRUE
            edRequestData:READ-ONLY     = FALSE
            edResponseData:READ-ONLY    = FALSE
            edDescription:READ-ONLY     = FALSE
            .
    ELSE
        ASSIGN
            APIInbound.apiRoute:SENSITIVE       = FALSE
            APIInbound.requestHandler:SENSITIVE = FALSE            
            .
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
        
    IF APIInbound.apiRoute:SCREEN-VALUE EQ "" THEN DO:
        opcMessage = "API Route cannot be empty".
        RETURN.
    END.

    IF APIInbound.importPath:SCREEN-VALUE NE "" THEN DO:
        RUN FileSys_CreateDirectory(
            INPUT  APIInbound.importPath:SCREEN-VALUE,
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
    DEFINE OUTPUT PARAMETER opcRowState AS CHARACTER NO-UNDO.
    
    IF AVAILABLE APIInbound THEN
        opcRowState = "update-only".
    ELSE
        opcRowState = "disable-all".
    
    IF lSuperAdmin THEN DO:
        IF AVAILABLE APIInbound THEN
            opcRowState = "initial".
        ELSE
            opcRowState = "add-only".
    END.
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
        edDescription:SCREEN-VALUE     = ""
        cbRequestVerb:SCREEN-VALUE     = "POST"
        cbRequestDataType:SCREEN-VALUE = "JSON"
        tgCanBeQueued:CHECKED          = FALSE
        edRequestData:SCREEN-VALUE     = ""
        edResponseData:SCREEN-VALUE    = ""
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

    IF AVAILABLE APIInbound THEN    
        ASSIGN
            APIInbound.company         = g_company
            APIInbound.Inactive        = tgInactive:CHECKED
            APIInbound.description     = edDescription:SCREEN-VALUE
            APIInbound.requestVerb     = cbRequestVerb:SCREEN-VALUE
            APIInbound.requestDataType = cbRequestDataType:SCREEN-VALUE
            APIInbound.responseData    = edResponseData:SCREEN-VALUE
            APIInbound.requestData     = edRequestData:SCREEN-VALUE
            APIInbound.canBeQueued     = tgCanBeQueued:CHECKED
            APIInbound.createBy        = USERID("ASI")
            APIInbound.createTime      = NOW
            .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateMessageText V-table-Win 
PROCEDURE pUpdateMessageText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMessage  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplError    AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplAlertBox AS LOGICAL   NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.

    fiMessage:SCREEN-VALUE = "".

    IF iplAlertBox THEN DO:
        MESSAGE ipcMessage
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    ASSIGN
        fiMessage:SCREEN-VALUE = ipcMessage
        fiMessage:FGCOLOR      = 2   /* Green */
        .

    IF iplError THEN
        ASSIGN
            fiMessage:SCREEN-VALUE = "**" + ipcMessage
            fiMessage:FGCOLOR      = 12  /* Red */
            .
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
  {src/adm/template/snd-list.i "APIInbound"}

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

