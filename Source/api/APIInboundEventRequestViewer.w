&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: api/APIInboundEventRequestViewer.w

  Description: View/Update API Inbound Event's request data

  Input Parameters:
        INPUT  iplcRequestData       AS LONGCHAR 
        INPUT  ipcRequestDataType    AS CHARACTER

  Output Parameters:
        OUTPUT oplcRequestData       AS LONGCHAR 
        OUTPUT oplRequestDataChanged AS LOGICAL  

  Author: Mithun Porandla

  Created: 05/05/2020
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/* Temp-table definition to load xml data */
{XMLOutput/ttNodes.i "NEW"}
/* Temp-table definition to load json data */
{api/inbound/ttRequest.i}

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER iplcRequestData       AS LONGCHAR  NO-UNDO.
DEFINE INPUT  PARAMETER ipcRequestDataType    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplcRequestData       AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT PARAMETER oplRequestDataChanged AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcRequestDataChanges AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE lUpdateSave   AS LOGICAL NO-UNDO.
DEFINE VARIABLE riPrevRowID   AS ROWID   NO-UNDO.
DEFINE VARIABLE hdJSONProcs   AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdXMLProcs    AS HANDLE  NO-UNDO.

RUN api/JSONProcs.p PERSISTENT SET hdJSONProcs.
RUN XMLOutput/XMLProcs.p PERSISTENT SET hdXMLProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttNodes

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttNodes.nodeName ttNodes.nodeValue   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttNodes                            WHERE ttNodes.nodeType NE "SYSTEM"                              AND (ttNodes.nodeName  MATCHES "*" + fiSearch:SCREEN-VALUE + "*" OR                                   ttNodes.nodeValue MATCHES "*" + fiSearch:SCREEN-VALUE + "*")
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttNodes                            WHERE ttNodes.nodeType NE "SYSTEM"                              AND (ttNodes.nodeName  MATCHES "*" + fiSearch:SCREEN-VALUE + "*" OR                                   ttNodes.nodeValue MATCHES "*" + fiSearch:SCREEN-VALUE + "*").
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttNodes
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttNodes


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 fiSearch BROWSE-2 btResetRequest ~
btSave 
&Scoped-Define DISPLAYED-OBJECTS fiSearch fiNodeName fiNodeValue 

/* Custom List Definitions                                              */
/* ENABLE-FIELDS,List-2,List-3,List-4,List-5,List-6                     */
&Scoped-define ENABLE-FIELDS btCancel fiNodeValue 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btResetRequest 
     LABEL "Reset Request Data" 
     SIZE 37 BY 1.62.

DEFINE BUTTON btSave AUTO-GO 
     LABEL "Save and Exit" 
     SIZE 37 BY 1.62.

DEFINE BUTTON btUpdate 
     LABEL "Update" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fiNodeName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 73.8 BY 1 NO-UNDO.

DEFINE VARIABLE fiNodeValue AS CHARACTER FORMAT "X(256)":U 
     LABEL "Value" 
     VIEW-AS FILL-IN 
     SIZE 73.8 BY 1 NO-UNDO.

DEFINE VARIABLE fiSearch AS CHARACTER FORMAT "X(256)":U 
     LABEL "Find Request Data" 
     VIEW-AS FILL-IN 
     SIZE 58.8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 114 BY 23.1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttNodes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 Dialog-Frame _FREEFORM
  QUERY BROWSE-2 DISPLAY
      ttNodes.nodeName  LABEL "Name"  WIDTH 30 FORMAT "X(50)"
ttNodes.nodeValue LABEL "Value" WIDTH 30 FORMAT "X(50)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 111 BY 18.57
         BGCOLOR 15 FONT 6 ROW-HEIGHT-CHARS .81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiSearch AT ROW 1.57 COL 24.2 COLON-ALIGNED WIDGET-ID 10
     BROWSE-2 AT ROW 2.91 COL 4.4 WIDGET-ID 200
     btUpdate AT ROW 21.62 COL 100.6 WIDGET-ID 18
     fiNodeName AT ROW 21.71 COL 11.2 COLON-ALIGNED WIDGET-ID 14
     btCancel AT ROW 22.81 COL 100.6 WIDGET-ID 22
     fiNodeValue AT ROW 22.91 COL 11.2 COLON-ALIGNED WIDGET-ID 16
     btResetRequest AT ROW 24.52 COL 19 WIDGET-ID 2
     btSave AT ROW 24.52 COL 60 WIDGET-ID 6
     "Use * as wildcard in search" VIEW-AS TEXT
          SIZE 26.4 BY .62 AT ROW 1.67 COL 86.4 WIDGET-ID 12
          FONT 1
     RECT-1 AT ROW 1.24 COL 3 WIDGET-ID 8
     SPACE(0.79) SKIP(2.03)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1 FONT 6
         TITLE "API Inbound Event Request Inquiry/Update" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 fiSearch Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btCancel IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON btUpdate IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiNodeName IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiNodeValue IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttNodes
                           WHERE ttNodes.nodeType NE "SYSTEM"
                             AND (ttNodes.nodeName  MATCHES "*" + fiSearch:SCREEN-VALUE + "*" OR
                                  ttNodes.nodeValue MATCHES "*" + fiSearch:SCREEN-VALUE + "*").
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* API Inbound Event Request Inquiry/Update */
DO:
    /* Update request data only when Save and exit button is clicked */
    IF oplRequestDataChanged THEN
        oplRequestDataChanged = FALSE.
        
    APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 Dialog-Frame
ON VALUE-CHANGED OF BROWSE-2 IN FRAME Dialog-Frame
DO:
    DEFINE BUFFER bf-ttNodes FOR ttNodes.

    IF lUpdateSave AND riPrevRowID NE ROWID(ttNodes) THEN DO:
        MESSAGE "Please save or cancel the selected"
                " record before selecting another record"
            VIEW-AS ALERT-BOX ERROR.
        REPOSITION {&BROWSE-NAME} TO ROWID riPrevRowID.
        RETURN NO-APPLY.
    END.
    
    ASSIGN
        fiNodeName:SCREEN-VALUE  = ""
        fiNodeValue:SCREEN-VALUE = ""
        .
        
    IF AVAILABLE ttNodes THEN DO:        
        ASSIGN
            fiNodeName:SCREEN-VALUE  = ttNodes.nodeName
            fiNodeValue:SCREEN-VALUE = ttNodes.nodeValue
            riPrevRowID              = ROWID(ttNodes)
            .
        
        IF ipcRequestDataType EQ "JSON" THEN DO:
            FIND FIRST bf-ttNodes 
                 WHERE bf-ttNodes.parentOrder EQ ttNodes.order
                 NO-ERROR.            
        END.

        IF NOT AVAILABLE bf-ttNodes OR ipcRequestDataType EQ "XML" THEN
            ENABLE btUpdate WITH FRAME {&FRAME-NAME}.
        ELSE
            DISABLE {&ENABLE-FIELDS} btUpdate WITH FRAME {&FRAME-NAME}.

    END.
    ELSE
        DISABLE {&ENABLE-FIELDS} btUpdate WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel Dialog-Frame
ON CHOOSE OF btCancel IN FRAME Dialog-Frame /* Cancel */
DO:
    lUpdateSave = NOT lUpdateSave.
    
    DISABLE {&ENABLE-FIELDS} WITH FRAME {&FRAME-NAME}.
    
    btUpdate:LABEL = "Update".
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btResetRequest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btResetRequest Dialog-Frame
ON CHOOSE OF btResetRequest IN FRAME Dialog-Frame /* Reset Request Data */
DO:
    IF lUpdateSave THEN DO:
        APPLY "CHOOSE" TO btCancel.
    END.
    
    RUN pResetTTNodes.  
    
    {&OPEN-QUERY-{&BROWSE-NAME}}
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSave Dialog-Frame
ON CHOOSE OF btSave IN FRAME Dialog-Frame /* Save and Exit */
DO:
    IF lUpdateSave THEN DO:
        APPLY "CHOOSE" TO btUpdate.
    END.  
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdate Dialog-Frame
ON CHOOSE OF btUpdate IN FRAME Dialog-Frame /* Update */
DO:
    lUpdateSave = NOT lUpdateSave.
    
    IF lUpdateSave THEN DO:        
        ENABLE {&ENABLE-FIELDS} WITH FRAME {&FRAME-NAME}.
        
        SELF:LABEL = "Save".
    END.
    ELSE DO:
        DISABLE {&ENABLE-FIELDS} WITH FRAME {&FRAME-NAME}.
        
        RUN pSaveTTNodes.
        
        SELF:LABEL = "Update".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSearch Dialog-Frame
ON VALUE-CHANGED OF fiSearch IN FRAME Dialog-Frame /* Find Request Data */
DO:
    {&OPEN-QUERY-{&BROWSE-NAME}}  
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN pInit.
    
    RUN enable_UI.
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.    
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY fiSearch fiNodeName fiNodeValue 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 fiSearch BROWSE-2 btResetRequest btSave 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit Dialog-Frame 
PROCEDURE pInit PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN pResetTTNodes.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pResetTTNodes Dialog-Frame 
PROCEDURE pResetTTNodes PRIVATE :
/*------------------------------------------------------------------------------
  Purpose: Resets all temp tables to original status
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    /* Reset output request data with original request data */
    ASSIGN
        oplcRequestData       = iplcRequestData
        oplRequestDataChanged = FALSE
        opcRequestDataChanges = "Modified request data:"
        .
    
    EMPTY TEMP-TABLE ttRequest.
    EMPTY TEMP-TABLE ttNodes.
    
    /* If request data type is JSON */
    IF ipcRequestDataType EQ "JSON" THEN DO:
        RUN ReadRequestData IN hdJSONProcs (
            INPUT  iplcRequestData,
            OUTPUT lSuccess,
            OUTPUT cMessage,
            OUTPUT TABLE ttRequest
            ).
        
        /* Create ttNodes records from ttRequest, so that all changes made
           by user can be tracked using single temp-table */
        FOR EACH ttRequest
            BY ttRequest.fieldOrder:
            CREATE ttNodes. 
            ASSIGN
              ttNodes.order       = ttRequest.fieldOrder
              ttNodes.nodeName    = ttRequest.fieldName
              ttNodes.nodeValue   = ttRequest.fieldValue
              ttNodes.parentOrder = ttRequest.fieldParent
              .            
        END.
    END.
    /* If request data type is XML */
    ELSE IF ipcRequestDataType EQ "XML" THEN
        RUN XML_ReadToTT IN hdXMLProcs (
            INPUT iplcRequestData
            ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveJSON Dialog-Frame 
PROCEDURE pSaveJSON PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-ttNodes FOR ttNodes.
    
    DEFINE VARIABLE cValueReplaceCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE cNameReplaceCount  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iIndex             AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMatchPosition     AS INTEGER NO-UNDO.
    DEFINE VARIABLE iStartPosition     AS INTEGER NO-UNDO INITIAL 1.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF AVAILABLE ttNodes THEN DO:
    
        /* If no updates made to value then return */
        IF ttNodes.nodeValue EQ fiNodeValue:SCREEN-VALUE THEN
            RETURN.

        FOR EACH bf-ttNodes
             BY bf-ttNodes.order:
            /* Get the number of places where the current node name occurred. 
               This is required to get the position of the node name for which 
               changes should be made  */
            IF bf-ttNodes.nodeName MATCHES "*" + ttNodes.nodeName + "*" THEN                
                cNameReplaceCount = cNameReplaceCount + 1.

            IF bf-ttNodes.nodeValue MATCHES "*" + ttNodes.nodeName + "*" THEN                 
                cNameReplaceCount = cNameReplaceCount + 1.

            /* Get the number of places where the current node value occurred. 
               This is required to get the position of the node value for which 
               changes should be made  */                
            IF ttNodes.nodeValue NE "" AND bf-ttNodes.nodeName MATCHES "*" + ttNodes.nodeValue + "*" THEN
                cValueReplaceCount = cValueReplaceCount + 1.

            IF ttNodes.nodeValue NE "" AND bf-ttNodes.nodeValue MATCHES "*" + ttNodes.nodeValue + "*" THEN
                cValueReplaceCount = cValueReplaceCount + 1.

            /* If current record then we found the number of places where node
               name and value occurred before the records name and value */
            IF bf-ttNodes.order EQ ttNodes.order THEN
                LEAVE.
        END.
                
        DO iIndex = 1 TO cNameReplaceCount:
            /* iMatchPosition - Finds the position of the node name in request data
                                from starting position */
            /* iStartPosition - After finding the match position increments by node name 
                                length in order to find the next node name position */
            ASSIGN
                iMatchPosition = INDEX(oplcRequestData, ttNodes.nodeName, iStartPosition)
                iStartPosition = iMatchPosition + LENGTH(ttNodes.nodeName)
                .
        END.            
        
        ASSIGN
            /* Replace node value between double quotes */
            iMatchPosition  = IF ttNodes.nodeValue EQ "" THEN
                                  INDEX(oplcRequestData, '"', iMatchPosition + LENGTH(ttNodes.nodeName) + 1)
                              ELSE
                                  INDEX(oplcRequestData, ttNodes.nodeValue, iMatchPosition)
            oplcRequestData = SUBSTRING(oplcRequestData, 1, iMatchPosition - 1) 
                            + fiNodeValue:SCREEN-VALUE 
                            + SUBSTRING(oplcRequestData, iMatchPosition + LENGTH(ttNodes.nodeValue), LENGTH(oplcRequestData))
            .
        
        /* Update ttNodes record */ 
        ASSIGN
            opcRequestDataChanges = opcRequestDataChanges 
                                  + "From:" + "'" + ttNodes.nodeValue + "'"
                                  + " To:" + "'" + fiNodeValue:SCREEN-VALUE + "';"
            ttNodes.nodeValue     = fiNodeValue:SCREEN-VALUE
            oplRequestDataChanged = TRUE
            .

        {&OPEN-QUERY-{&BROWSE-NAME}}
        
        REPOSITION {&BROWSE-NAME} TO ROWID riPrevRowID.
        
        APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveTTNodes Dialog-Frame 
PROCEDURE pSaveTTNodes PRIVATE :
/*------------------------------------------------------------------------------
  Purpose: Changes made to node values will be handled here for all request data 
           types
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/    
    IF ipcRequestDataType EQ "XML" THEN
        RUN pSaveXML.
    ELSE IF ipcRequestDataType EQ "JSON" THEN
        RUN pSaveJSON.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveXML Dialog-Frame 
PROCEDURE pSaveXML PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-ttNodes FOR ttNodes.
    
    DEFINE VARIABLE cValueReplaceCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE cNameReplaceCount  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iIndex             AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMatchPosition     AS INTEGER NO-UNDO.
    DEFINE VARIABLE iStartPosition     AS INTEGER NO-UNDO INITIAL 1.
    DEFINE VARIABLE lReposition        AS LOGICAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF AVAILABLE ttNodes THEN DO:
    
        /* If no updates made to value then return */
        IF ttNodes.nodeValue EQ fiNodeValue:SCREEN-VALUE THEN
            RETURN.
                
        /* Update the value in temp-table record */
        ASSIGN
            opcRequestDataChanges = opcRequestDataChanges
                                  + "From:" + "'" + ttNodes.nodeValue + "'"
                                  + " To:" + "'" + fiNodeValue:SCREEN-VALUE + "';"
            ttNodes.nodeValue     = fiNodeValue:SCREEN-VALUE
            oplRequestDataChanged = TRUE
            .
        
        /* Write temp-table data to xml */
        RUN XML_WriteFromTT IN hdXMLProcs (
            OUTPUT oplcRequestData
            ).
        
        /* Reposition the record only if record exists after the query refreshes */
        IF ttNodes.nodeName MATCHES "*" + fiSearch:SCREEN-VALUE + "*" OR ttNodes.nodeValue MATCHES "*" + fiSearch:SCREEN-VALUE + "*" THEN
            lReposition = TRUE.
        
        {&OPEN-QUERY-{&BROWSE-NAME}}
        
        IF lReposition THEN
            REPOSITION {&BROWSE-NAME} TO ROWID riPrevRowID.
        
        APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

