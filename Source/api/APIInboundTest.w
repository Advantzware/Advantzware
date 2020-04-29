&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: api/APIInboundTest.w

  Description: Utility to test the API Inbound calls

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Porandla Mithun

  Created: 10/16/2019

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hdJSONProcs AS HANDLE NO-UNDO.

RUN api/JSONProcs.p PERSISTENT SET hdJSONProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-19 RECT-20 Btn_Cancel cbRequestVerb ~
cbRequestDataType fiRequestURL cbAPI btSend fiUserName fiPassword ~
edRequestData tbViewTableAs btValidateRequestJSON btBeautifyRequestData ~
edResponseData tbViewResponseAs btVaidateResponseData ~
btBeautifyResponseData 
&Scoped-Define DISPLAYED-OBJECTS cbRequestVerb cbRequestDataType ~
fiRequestURL cbAPI fiUserName fiPassword fiResponseTime edRequestData ~
tbViewTableAs edResponseData tbViewResponseAs 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btBeautifyRequestData 
     LABEL "Beautify" 
     SIZE 20 BY 2.62.

DEFINE BUTTON btBeautifyResponseData 
     LABEL "Beautify" 
     SIZE 20 BY 2.62.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "Cancel" 
     SIZE 20 BY 2.62 TOOLTIP "Exit"
     BGCOLOR 8 .

DEFINE BUTTON btSend 
     IMAGE-UP FILE "Graphics/32x32/check.ico":U
     LABEL "Send" 
     SIZE 20 BY 2.62.

DEFINE BUTTON btVaidateResponseData 
     LABEL "View in Browser" 
     SIZE 20 BY 2.62.

DEFINE BUTTON btValidateRequestJSON 
     LABEL "View in Browser" 
     SIZE 20 BY 2.62.

DEFINE VARIABLE cbAPI AS CHARACTER FORMAT "X(256)":U 
     LABEL "API" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 63.8 BY 1 NO-UNDO.

DEFINE VARIABLE cbRequestDataType AS CHARACTER FORMAT "X(256)":U INITIAL "JSON" 
     LABEL "Request Datatype" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "JSON","XML" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cbRequestVerb AS CHARACTER FORMAT "X(256)":U INITIAL "POST" 
     LABEL "Request Verb" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "POST","GET" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE edRequestData AS LONGCHAR 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 133 BY 8.24 NO-UNDO.

DEFINE VARIABLE edResponseData AS LONGCHAR 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 133 BY 11 NO-UNDO.

DEFINE VARIABLE fiPassword AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 33.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiRequestURL AS CHARACTER FORMAT "X(256)":U 
     LABEL "Request URL" 
     VIEW-AS FILL-IN 
     SIZE 99.8 BY 1 NO-UNDO.

DEFINE VARIABLE fiResponseTime AS CHARACTER FORMAT "X(256)":U 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fiUserName AS CHARACTER FORMAT "X(256)":U 
     LABEL "User Name" 
     VIEW-AS FILL-IN 
     SIZE 26.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 133.2 BY 4.29.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 133.2 BY 1.67.

DEFINE VARIABLE tbViewResponseAs AS LOGICAL INITIAL no 
     LABEL "Table View" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.8 BY .81 NO-UNDO.

DEFINE VARIABLE tbViewTableAs AS LOGICAL INITIAL no 
     LABEL "Table View" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_Cancel AT ROW 1.57 COL 140 WIDGET-ID 132
     cbRequestVerb AT ROW 1.81 COL 18.4 COLON-ALIGNED WIDGET-ID 2
     cbRequestDataType AT ROW 1.81 COL 65.6 COLON-ALIGNED WIDGET-ID 4
     fiRequestURL AT ROW 3.1 COL 18.2 COLON-ALIGNED WIDGET-ID 6
     cbAPI AT ROW 4.43 COL 18.2 COLON-ALIGNED WIDGET-ID 136
     btSend AT ROW 5.05 COL 139.8 WIDGET-ID 24
     fiUserName AT ROW 6.43 COL 15.4 COLON-ALIGNED WIDGET-ID 10
     fiPassword AT ROW 6.43 COL 58.2 COLON-ALIGNED WIDGET-ID 12 PASSWORD-FIELD 
     fiResponseTime AT ROW 6.43 COL 100.4 COLON-ALIGNED WIDGET-ID 26
     edRequestData AT ROW 8.86 COL 3 NO-LABEL WIDGET-ID 16
     tbViewTableAs AT ROW 8.91 COL 140.2 WIDGET-ID 154
     btValidateRequestJSON AT ROW 9.95 COL 139.8 WIDGET-ID 144
     btBeautifyRequestData AT ROW 13.81 COL 139.8 WIDGET-ID 138
     edResponseData AT ROW 18.14 COL 3 NO-LABEL WIDGET-ID 20
     tbViewResponseAs AT ROW 18.19 COL 140.2 WIDGET-ID 156
     btVaidateResponseData AT ROW 19.38 COL 139.8 WIDGET-ID 142
     btBeautifyResponseData AT ROW 23.33 COL 139.8 WIDGET-ID 140
     "Response:" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 17.24 COL 3.2 WIDGET-ID 22
     "Request:" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 8.1 COL 3.4 WIDGET-ID 18
     RECT-19 AT ROW 1.71 COL 2 WIDGET-ID 8
     RECT-20 AT ROW 6.1 COL 2 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.48
         BGCOLOR 15 FGCOLOR 1 FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "API Inbound Tester"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 31.38
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 31.38
         VIRTUAL-WIDTH      = 160
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       edResponseData:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiResponseTime IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* API Inbound Tester */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* API Inbound Tester */
DO:
    RUN setUserPrint.
    
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btBeautifyRequestData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btBeautifyRequestData C-Win
ON CHOOSE OF btBeautifyRequestData IN FRAME DEFAULT-FRAME /* Beautify */
DO:
    ASSIGN
        edRequestData.

    IF edRequestData EQ "" THEN DO:
        MESSAGE "Empty request data" VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    edRequestData:SCREEN-VALUE = DYNAMIC-FUNCTION(
                                     "fBeautifyJSON" IN hdJSONProcs,
                                     edRequestData
                                     ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btBeautifyResponseData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btBeautifyResponseData C-Win
ON CHOOSE OF btBeautifyResponseData IN FRAME DEFAULT-FRAME /* Beautify */
DO:
    ASSIGN
        edResponseData.

    IF edResponseData EQ "" THEN DO:
        MESSAGE "Empty response data" VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
        
    edResponseData:SCREEN-VALUE = DYNAMIC-FUNCTION(
                                      "fBeautifyJSON" IN hdJSONProcs,
                                      edResponseData
                                      ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    RUN setUserPrint.

    IF VALID-HANDLE(hdJSONProcs) THEN
        DELETE OBJECT hdJSONProcs.
        
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSend C-Win
ON CHOOSE OF btSend IN FRAME DEFAULT-FRAME /* Send */
DO:
    DEFINE VARIABLE cCommand       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRequestFile   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponseFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcRequestData  AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcResponseData AS LONGCHAR  NO-UNDO.    
    DEFINE VARIABLE iTimeElapsed   AS INT64     NO-UNDO.
    DEFINE VARIABLE cRequestURL    AS CHARACTER NO-UNDO.
    
    IF fiRequestURL:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "Request URL cannot be empty"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.     

    IF fiUserName:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "User Name cannot be empty"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.     

    IF fiPassword:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "User Name cannot be empty"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.     
    
    IF cbAPI:SCREEN-VALUE EQ "" OR cbAPI:SCREEN-VALUE EQ ? THEN DO:
        MESSAGE "API cannot be empty"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.    
    END.

    IF SEARCH("curl.exe") EQ ? THEN DO:
        MESSAGE "curl not found!"
            VIEW-AS ALERT-BOX ERROR.
        RETURN. 
    END.    

    ASSIGN
        cRequestFile  = "C:\Tmp\request"
        cResponseFile = "C:\Tmp\response"
        cRequestURL   = (IF SUBSTRING(fiRequestURL:SCREEN-VALUE, LENGTH(fiRequestURL:SCREEN-VALUE), 1) EQ "/" OR
                            SUBSTRING(fiRequestURL:SCREEN-VALUE, LENGTH(fiRequestURL:SCREEN-VALUE), 1) EQ "\" THEN
                             SUBSTRING(fiRequestURL:SCREEN-VALUE, 1, LENGTH(fiRequestURL:SCREEN-VALUE) - 1)
                         ELSE
                             fiRequestURL:SCREEN-VALUE)                        
                      + cbAPI:SCREEN-VALUE
        .

    cCommand = SEARCH("curl.exe") + ' --user '
             + fiUserName:SCREEN-VALUE + ':' + fiPassword:SCREEN-VALUE + ' '
             + '-H "Content-Type: application/' +  lc(cbRequestDataType:SCREEN-VALUE + '"') /* handles XML or JSON only - not RAW */
             + (IF cbRequestVerb:SCREEN-VALUE NE 'GET' THEN ' -d "@' + cRequestFile + '" ' ELSE '')
             + (IF cbRequestVerb:SCREEN-VALUE NE 'GET' THEN ' -X ' + cbRequestVerb:SCREEN-VALUE ELSE '')  + ' '
             + cRequestURL
             + ' > ' + cResponseFile.    
    
    ASSIGN
        edRequestData
        lcRequestData = edRequestData
        .
    
    /* Put Request Data from a variable into a Temporary file */
    COPY-LOB lcRequestData TO FILE cRequestFile.
    
    SESSION:SET-WAIT-STATE("GENERAL").
    
    ETIME(YES).
    
    /* execute CURL command with required parameters to call the API */
    DOS SILENT VALUE(cCommand).

    iTimeElapsed = ETIME.
    
    SESSION:SET-WAIT-STATE("").    
        
    fiResponseTime:SCREEN-VALUE = IF iTimeElapsed GT 999 THEN
                                      (STRING(TRUNCATE(iTimeElapsed / 1000, 0)) + "s" + " " + STRING(INT(iTimeElapsed MOD 1000)) + "ms")
                                  ELSE
                                      STRING(iTimeElapsed) + "ms"
                                  .
    
    /* Put Response Data from Temporary file into a variable */
    COPY-LOB FILE cResponseFile TO lcResponseData. 
    
    edResponseData:SCREEN-VALUE = IF lcResponseData NE "" THEN
                                      lcResponseData
                                  ELSE
                                      "Could not get any response".
    
    OS-DELETE VALUE(cRequestFile).
    OS-DELETE VALUE(cResponseFile).    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btVaidateResponseData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btVaidateResponseData C-Win
ON CHOOSE OF btVaidateResponseData IN FRAME DEFAULT-FRAME /* View in Browser */
DO:
    DEFINE VARIABLE cTitle    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHTMLFile AS CHARACTER NO-UNDO.
    
    ASSIGN
        edResponseData
        tbViewResponseAs
        cbAPI
        cHTMLFile     = "C:\Tmp\NEW_jsonHTMLTable.html"
        cTitle        = cbAPI + " | response | " + cHTMLFile
        .

    IF edResponseData EQ "" THEN DO:
        MESSAGE "Empty response data" VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
            
    RUN pGenerateHTMLTableFromJSON (
        INPUT cTitle,
        INPUT IF tbViewResponseAs THEN "CreateTableView" ELSE "CreateDetailView",
        INPUT edResponseData,
        INPUT cHTMLFile,
        INPUT YES  /* lOpenInBrowser */
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btValidateRequestJSON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btValidateRequestJSON C-Win
ON CHOOSE OF btValidateRequestJSON IN FRAME DEFAULT-FRAME /* View in Browser */
DO:
    DEFINE VARIABLE cTitle    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHTMLFile AS CHARACTER NO-UNDO.
    
    ASSIGN
        edRequestData
        tbViewTableAs
        cbAPI
        cHTMLFile     = "C:\Tmp\NEW_jsonHTMLTable.html"
        cTitle        = cbAPI + " | request | " + cHTMLFile
        .
    
    IF edRequestData EQ "" THEN DO:
        MESSAGE "Empty request data" VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
        
    RUN pGenerateHTMLTableFromJSON (
        INPUT cTitle,
        INPUT IF tbViewTableAs THEN "CreateTableView" ELSE "CreateDetailView",
        INPUT edRequestData,
        INPUT cHTMLFile,
        INPUT YES  /* lOpenInBrowser */
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbAPI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbAPI C-Win
ON VALUE-CHANGED OF cbAPI IN FRAME DEFAULT-FRAME /* API */
DO:
    RUN pUpdateRequestData (
        INPUT SELF:SCREEN-VALUE
        ) NO-ERROR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbRequestDataType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbRequestDataType C-Win
ON VALUE-CHANGED OF cbRequestDataType IN FRAME DEFAULT-FRAME /* Request Datatype */
DO:
    RUN pUpdateAPI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbViewResponseAs
&Scoped-define SELF-NAME tbViewTableAs
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN pInit.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY cbRequestVerb cbRequestDataType fiRequestURL cbAPI fiUserName 
          fiPassword fiResponseTime edRequestData tbViewTableAs edResponseData 
          tbViewResponseAs 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-19 RECT-20 Btn_Cancel cbRequestVerb cbRequestDataType 
         fiRequestURL cbAPI btSend fiUserName fiPassword edRequestData 
         tbViewTableAs btValidateRequestJSON btBeautifyRequestData 
         edResponseData tbViewResponseAs btVaidateResponseData 
         btBeautifyResponseData 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGenerateHTMLTableFromJSON C-Win 
PROCEDURE pGenerateHTMLTableFromJSON :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcHTMLPageTitle AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcViewAS        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplcJSONData     AS LONGCHAR  NO-UNDO. 
    DEFINE INPUT PARAMETER ipcHTMLFilePath  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplOpenInBrowser AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE lcHTML           AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE lcJSONData       AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE lcPrettyJSONData AS LONGCHAR   NO-UNDO.

    DEFINE VARIABLE iOffSet AS INTEGER     NO-UNDO.
       
    ASSIGN
        lcJSONData       = DYNAMIC-FUNCTION(
                               "fBeautifyJSON" IN hdJSONProcs,
                               iplcJSONData
                               )
        lcPrettyJSONData = lcJSONData
        iplcJSONData     = REPLACE (iplcJSONData, CHR(10), "")
        iplcJSONData     = REPLACE (iplcJSONData, CHR(13), "")
        .

    lcHTML = '<html>' +
             '<head><style>table~{border-collapse~:collapse~;~}table,th,td~{border~:1px solid black~;text-align~: left~;vertical-align~:top~;~}</style></head>' + 
             '<body><h1>' + ipcHTMLPageTitle + '</h1><div id="jsonData">Rendering...</div>' +
             '<script language="JavaScript">var errorPos=0~; function CreateTableView(t)~{var e="object"!=typeof t?JSON.parse(t)~:new Array(t),r=Object.keys(e[0]),a="<table>"~;for(var o in a+="<thead><tr>",r)a+=~'<th scope="col">~'+r[o]+"</th>"~;a+="</tr></thead>",a+="<tbody>"~;for(var n=0~;n<e.length~;n++)~{for(var o in a+=n%2==0?~'<tr class="alt">~'~:"<tr>",r)~{var l=e[n][r[o]]~;if("object"==typeof l&&null!==l)if(Array.isArray(l))~{for(var i in a+="<td>",l)a+=CreateTableView(l[i])~;a+="</td>"~}else a+="<td>"+CreateTableView(l)+"</td>"~;else a+="<td>"+l+"</td>"~}a+="</tr>"~}return a+="</tbody>",a+="</table>"~}function CreateDetailView(t)~{var e="object"!=typeof t?JSON.parse(t)~:new Array(t),r=Object.keys(e[0]),a=~'<table style="cursor~:pointer">~'~;a+="<tbody>"~;for(var o=0~;o<e.length~;o++)~{var n=0~;for(var l in r)~{var i=e[o][r[l]]~;if(a+=n%2==0?~'<tr class="alt">~'~:"<tr>",a+=~'<th scope="row">~'+r[l]+"</th>","object"==typeof i&&null!==i)if(Array.isArray(i))~{for(var d in a+="<td>",i)a+=CreateDetailView(i[d])~;a+="</td>"~}else a+="<td>"+CreateDetailView(i)+"</td>"~;else a+="<td>"+i+"</td>"~;a+="</tr>",n++~}~}return a+="</tbody>",a+="</table>"~}var json=""~;function validateJSON(t)~{try~{json=JSON.parse(t),document.getElementById("jsonData").innerHTML=' + ipcViewAS + '(json)~}catch(t)~{document.getElementById("jsonData").innerHTML="<font color=red>Invalid JSON - "+t.message+"</font>&nbsp;<button onclick=showMeTheError()>Locate</button>" ~; errorPos=t.message.split(":")[2]~;~}~}validateJSON(~'' + iplcJSONData + '~')~;</script>' +
             '<br><table border=0><tr><td style="background-color:#e6f3ff">Raw JSON</td><td style="background-color:#e6f3ff">Pretty JSON</td></tr><tr><td><textarea name="JSONEditor" id="JSONEditor" rows="30" cols="70">' + iplcJSONData + '</textarea></td>' + 
             '<td><textarea name="JSONEditorPretty" id="JSONEditorPretty" rows="30" cols="70" readonly>' + lcPrettyJSONData + '</textarea></td></tr></table><br>' + 
             '<script language="JavaScript">' + 
             'function showMeTheError()~{' +
             'document.getElementById(~'JSONEditor~').focus()~;document.getElementById(~'JSONEditor~').setSelectionRange(errorPos - 1, errorPos)~;' +
             '~}' +
             '</script>' +
             '</body></html>'.

    COPY-LOB FROM lcHTML TO FILE ipcHTMLFilePath.

    IF iplOpenInBrowser AND SEARCH(ipcHTMLFilePath) <> ? THEN
        OS-COMMAND SILENT VALUE("start " + ipcHTMLFilePath).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit C-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
    DO WITH FRAME {&FRAME-NAME}:
    END.
                
    {custom/usrprint.i}
    
    APPLY "LEAVE" TO fiRequestURL.

    RUN pUpdateAPI.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateAPI C-Win 
PROCEDURE pUpdateAPI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cAPIList AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    FOR EACH APIInbound NO-LOCK
        WHERE APIInbound.requestDataType EQ cbRequestDataType:SCREEN-VALUE
          AND NOT APIInbound.Inactive:
        cAPIList = cAPIList + "," + APIInbound.apiRoute.
    END.
    
    cAPIList = TRIM(cAPIList,",").
    
    cbAPI:LIST-ITEMS = cAPIList.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRequestData C-Win 
PROCEDURE pUpdateRequestData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcAPIRoute AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cAPIRoute AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcData    AS LONGCHAR  NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
                  
    FIND FIRST APIInbound NO-LOCK
         WHERE APIInbound.apiRoute EQ ipcAPIRoute
         NO-ERROR.
    IF AVAILABLE APIInbound THEN
        ASSIGN
            lcData                     = APIInbound.requestData
            edRequestData:SCREEN-VALUE = lcData
            NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setUserPrint C-Win 
PROCEDURE setUserPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.

    RUN custom/setUserPrint.p (
        INPUT g_company,
        INPUT "APIInboundTest.",
        INPUT "cbRequestVerb,cbRequestDataType,fiRequestURL,fiUserName,fiPassword",
        INPUT cbRequestVerb:SCREEN-VALUE + ',' + 
              cbRequestDataType:SCREEN-VALUE + ',' + 
              fiRequestURL:SCREEN-VALUE + ',' +
              fiUserName:SCREEN-VALUE + ',' +
              fiPassword:SCREEN-VALUE
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

