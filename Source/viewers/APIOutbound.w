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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

DEFINE VARIABLE hdFileSysProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE lCreated       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.

RUN system/FileSysProcs.p PERSISTENT SET hdFileSysProcs.

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
&Scoped-Define ENABLED-FIELDS APIOutbound.saveFile APIOutbound.userName ~
APIOutbound.password APIOutbound.requestHandler APIOutbound.responseHandler 
&Scoped-define ENABLED-TABLES APIOutbound
&Scoped-define FIRST-ENABLED-TABLE APIOutbound
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 edEndPoint ~
edRequestData 
&Scoped-Define DISPLAYED-FIELDS APIOutbound.apiID APIOutbound.clientID ~
APIOutbound.saveFileFolder APIOutbound.saveFile APIOutbound.userName ~
APIOutbound.password APIOutbound.requestHandler APIOutbound.responseHandler 
&Scoped-define DISPLAYED-TABLES APIOutbound
&Scoped-define FIRST-DISPLAYED-TABLE APIOutbound
&Scoped-Define DISPLAYED-OBJECTS fiMessage tgActive edEndPoint ~
cbRequestDataType cbRequestVerb tgSSLEnabled cbAuthType edRequestData 

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
     LIST-ITEMS "none","basic" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE cbRequestDataType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Request Data Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "JSON","XML","FTP" 
     DROP-DOWN-LIST
     SIZE 17 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE cbRequestVerb AS CHARACTER FORMAT "X(256)":U 
     LABEL "Request Verb" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "POST","GET" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE edEndPoint AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 132 BY 2.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE edRequestData AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 132 BY 4
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiMessage AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 154.8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 154.6 BY 6.14.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 154.8 BY 1.67.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 155 BY 1.91.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 155 BY 5.19.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 157 BY 18.57.

DEFINE VARIABLE tgActive AS LOGICAL INITIAL no 
     LABEL "Active" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.2 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE tgSSLEnabled AS LOGICAL INITIAL no 
     LABEL "Enable SSL" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiMessage AT ROW 1.24 COL 2.2 NO-LABEL WIDGET-ID 52
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
     tgActive AT ROW 2.67 COL 126.6 WIDGET-ID 28
     edEndPoint AT ROW 3.76 COL 23 NO-LABEL WIDGET-ID 34
     APIOutbound.saveFileFolder AT ROW 6.33 COL 40 WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 94.8 BY 1
          BGCOLOR 15 FGCOLOR 0 
     APIOutbound.saveFile AT ROW 6.43 COL 22.8 WIDGET-ID 60
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .81
          BGCOLOR 15 FGCOLOR 0 
     cbRequestDataType AT ROW 7.43 COL 92 COLON-ALIGNED WIDGET-ID 40
     cbRequestVerb AT ROW 7.48 COL 21 COLON-ALIGNED WIDGET-ID 42
     tgSSLEnabled AT ROW 7.52 COL 114 WIDGET-ID 38
     APIOutbound.userName AT ROW 9.33 COL 21 COLON-ALIGNED WIDGET-ID 24
          LABEL "Username"
          VIEW-AS FILL-IN 
          SIZE 30.8 BY 1
          BGCOLOR 15 FGCOLOR 0 
     APIOutbound.password AT ROW 9.33 COL 78 COLON-ALIGNED WIDGET-ID 14 PASSWORD-FIELD 
          LABEL "Password"
          VIEW-AS FILL-IN 
          SIZE 30.8 BY 1
          BGCOLOR 15 FGCOLOR 0 
     cbAuthType AT ROW 9.33 COL 137 COLON-ALIGNED WIDGET-ID 32
     APIOutbound.requestHandler AT ROW 11.95 COL 21 COLON-ALIGNED WIDGET-ID 18
          LABEL "Request Handler" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 54 BY 1
          BGCOLOR 15 FGCOLOR 0 
     APIOutbound.responseHandler AT ROW 11.95 COL 98.6 COLON-ALIGNED WIDGET-ID 22
          LABEL "Response Handler" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 55 BY 1
          BGCOLOR 15 FGCOLOR 0 
     edRequestData AT ROW 14.57 COL 23 NO-LABEL WIDGET-ID 48
     "Request Data:" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 14.81 COL 6 WIDGET-ID 50
     "End Point:" VIEW-AS TEXT
          SIZE 11.6 BY .62 AT ROW 4.33 COL 10 WIDGET-ID 36
     RECT-1 AT ROW 2.48 COL 2 WIDGET-ID 26
     RECT-2 AT ROW 9.1 COL 2 WIDGET-ID 30
     RECT-3 AT ROW 11.48 COL 2 WIDGET-ID 44
     RECT-4 AT ROW 14.05 COL 2 WIDGET-ID 46
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
         HEIGHT             = 18.57
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
/* SETTINGS FOR COMBO-BOX cbRequestVerb IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN APIOutbound.clientID IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
ASSIGN 
       edEndPoint:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       edRequestData:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fiMessage IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN APIOutbound.password IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR RECTANGLE RECT-7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN APIOutbound.requestHandler IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN APIOutbound.responseHandler IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN APIOutbound.saveFileFolder IN FRAME F-Main
   NO-ENABLE ALIGN-L 2                                                  */
/* SETTINGS FOR TOGGLE-BOX tgActive IN FRAME F-Main
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
    
    IF RETURN-VALUE = "ADM-ERROR":U THEN 
        RETURN "ADM-ERROR":U.
    
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
        tgActive:SENSITIVE          = FALSE
        cbRequestVerb:SENSITIVE     = FALSE
        cbRequestDataType:SENSITIVE = FALSE
        tgSSLEnabled:SENSITIVE      = FALSE
        cbAuthType:SENSITIVE        = FALSE
        edEndPoint:READ-ONLY        = TRUE
        edRequestData:READ-ONLY     = TRUE
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

    IF AVAILABLE APIOutbound THEN
        ASSIGN
            tgActive:CHECKED               = APIOutbound.isActive
            edEndPoint:SCREEN-VALUE        = APIOutbound.endPoint
            cbRequestVerb:SCREEN-VALUE     = APIOutbound.requestVerb
            cbRequestDataType:SCREEN-VALUE = APIOutbound.requestDataType
            tgSSLEnabled:CHECKED           = APIOutbound.isSSLEnabled
            cbAuthType:SCREEN-VALUE        = APIOutbound.authType
            edRequestData:SCREEN-VALUE     = STRING(APIOutbound.requestData)
            .
    ELSE
        ASSIGN
            edEndPoint:SCREEN-VALUE        = ""
            edRequestData:SCREEN-VALUE     = ""
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

    ASSIGN
        tgActive:SENSITIVE          = TRUE
        cbRequestVerb:SENSITIVE     = TRUE
        cbRequestDataType:SENSITIVE = TRUE
        tgSSLEnabled:SENSITIVE      = TRUE
        cbAuthType:SENSITIVE        = TRUE
        edEndPoint:READ-ONLY        = FALSE
        edRequestData:READ-ONLY     = FALSE
        saveFileFolder:SENSITIVE    = IF adm-new-record THEN FALSE ELSE saveFile:checked
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
        
    IF APIOutbound.apiID:SCREEN-VALUE EQ "" THEN DO:
        opcMessage = "API ID cannot be empty".
        RETURN.
    END.

    IF APIOutbound.clientID:SCREEN-VALUE EQ "" THEN DO:
        opcMessage = "Client ID cannot be empty".
        RETURN.
    END.
    
    IF cbAuthType:SCREEN-VALUE EQ "basic" THEN DO:        
        IF APIOutbound.userName:SCREEN-VALUE EQ "" OR 
           APIOutbound.password:SCREEN-VALUE EQ "" THEN DO:
            opcMessage = "Username and Password cannot be empty for "
                       + "Authentication Type: basic".
            RETURN.
        END.
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
        tgActive:CHECKED               = TRUE
        edEndPoint:SCREEN-VALUE        = ""
        cbRequestVerb:SCREEN-VALUE     = "POST"
        cbRequestDataType:SCREEN-VALUE = "JSON"
        tgSSLEnabled:CHECKED           = FALSE
        cbAuthType:SCREEN-VALUE        = "basic"
        edRequestData:SCREEN-VALUE     = ""
        saveFile:CHECKED               = FALSE
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
            APIOutbound.isActive        = tgActive:CHECKED
            APIOutbound.isSSLEnabled    = tgSSLEnabled:CHECKED
            APIOutbound.endPoint        = edEndPoint:SCREEN-VALUE
            APIOutbound.requestVerb     = cbRequestVerb:SCREEN-VALUE
            APIOutbound.requestDataType = cbRequestDataType:SCREEN-VALUE
            APIOutbound.authType        = cbAuthType:SCREEN-VALUE
            APIOutbound.requestData     = edRequestData:SCREEN-VALUE
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

