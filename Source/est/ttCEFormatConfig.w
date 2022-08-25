&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: est/ttCEFormatConfig.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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
{est/ttCEFormatConfig.i}

DEFINE TEMP-TABLE ttKeyValue NO-UNDO
    FIELD formatKey      AS CHARACTER
    FIELD formatValue    AS CHARACTER
    FIELD formatDataType AS CHARACTER
    FIELD formatDesc     AS CHARACTER
    .
    
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.

RUN spGetSessionParam ("Company", OUTPUT cCompany).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttKeyValue

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttKeyValue.formatKey ttKeyValue.formatDesc ttKeyValue.formatValue   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 ttKeyValue.formatValue   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 ttKeyValue
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 ttKeyValue
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttKeyValue WHERE ttKeyValue.formatKey MATCHES "*" + fiSearch:SCREEN-VALUE IN FRAME {&FRAME-NAME} + "*"
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttKeyValue WHERE ttKeyValue.formatKey MATCHES "*" + fiSearch:SCREEN-VALUE IN FRAME {&FRAME-NAME} + "*".
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttKeyValue
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttKeyValue


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiSearch BROWSE-2 btSave btCancel btReset 
&Scoped-Define DISPLAYED-OBJECTS fiSearch 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 24 BY 1.65
     FONT 6.

DEFINE BUTTON btReset 
     LABEL "Restore Default Configuration" 
     SIZE 54 BY 1.65.

DEFINE BUTTON btSave AUTO-GO 
     LABEL "Save" 
     SIZE 24 BY 1.65
     FONT 6.

DEFINE VARIABLE fiSearch AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 92 BY 1
     FONT 24 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttKeyValue SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _FREEFORM
  QUERY BROWSE-2 DISPLAY
      ttKeyValue.formatKey FORMAT "X(100)" WIDTH 50 COLUMN-LABEL "Key"
ttKeyValue.formatDesc FORMAT "X(100)" WIDTH 100 COLUMN-LABEL "Description"
ttKeyValue.formatValue FORMAT "X(200)" WIDTH 100 COLUMN-LABEL "Value"
ENABLE ttKeyValue.formatValue
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 204 BY 19.88
         FONT 24 ROW-HEIGHT-CHARS .91 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiSearch AT ROW 1.46 COL 14 COLON-ALIGNED WIDGET-ID 6
     BROWSE-2 AT ROW 2.92 COL 1 WIDGET-ID 200
     btSave AT ROW 23.35 COL 76 WIDGET-ID 2
     btCancel AT ROW 23.35 COL 102 WIDGET-ID 4
     btReset AT ROW 23.35 COL 151 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 205.5 BY 24.58
         BGCOLOR 15 FONT 24 WIDGET-ID 100.


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
         TITLE              = "CE Format Config Maintenance"
         HEIGHT             = 24.58
         WIDTH              = 205.5
         MAX-HEIGHT         = 30.23
         MAX-WIDTH          = 206
         VIRTUAL-HEIGHT     = 30.23
         VIRTUAL-WIDTH      = 206
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
/* BROWSE-TAB BROWSE-2 fiSearch DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttKeyValue WHERE ttKeyValue.formatKey MATCHES "*" + fiSearch:SCREEN-VALUE IN FRAME {&FRAME-NAME} + "*".
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* CE Format Config Maintenance */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* CE Format Config Maintenance */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON ROW-LEAVE OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.

    ASSIGN ttKeyValue.formatValue = ttKeyValue.formatValue:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.
                   
    RUN pValidate (BUFFER ttKeyValue, OUTPUT cMessage, OUTPUT lError).

    IF lError THEN DO:
        MESSAGE cMessage
        VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReset C-Win
ON CHOOSE OF btReset IN FRAME DEFAULT-FRAME /* Restore Default Configuration */
DO:
    RUN pInit ( FALSE /* Load From NK1 CeFormatConfig */).      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME btSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSave C-Win
ON CHOOSE OF btSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE hdBuffer AS HANDLE    NO-UNDO.

    DEFINE VARIABLE lFound      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValid      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lLoaded     AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (
        INPUT cCompany, 
        INPUT "CEFormatConfig", 
        INPUT "C" /* Logical */, 
        INPUT NO /* check by cust */, 
        INPUT NO /* use cust not vendor */, 
        INPUT "" /* cust */, 
        INPUT "" /* ship-to*/,
        OUTPUT cFile, 
        OUTPUT lFound
        ). 
    IF cFile EQ "" THEN DO:
        MESSAGE "Please enter a valid file name in NK1 'CEFormatConfig' character value to save the configuration"
        VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
        
    FIND FIRST ttCeFormatConfig NO-ERROR.
    hdBuffer = BUFFER ttCeFormatConfig:HANDLE.
    
    FOR EACH ttKeyValue:
        RUN pValidate (BUFFER ttKeyValue, OUTPUT cMessage, OUTPUT lError).
        IF lError THEN DO:
            MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.    
    END.
    
    FOR EACH ttKeyValue:
        IF ttKeyValue.formatDataType EQ "Integer" THEN
            hdBuffer:BUFFER-FIELD(ttKeyValue.formatKey):BUFFER-VALUE = INTEGER(ttKeyValue.formatValue) NO-ERROR.
        ELSE IF ttKeyValue.formatDataType EQ "Decimal" THEN
            hdBuffer:BUFFER-FIELD(ttKeyValue.formatKey):BUFFER-VALUE = DECIMAL(ttKeyValue.formatValue) NO-ERROR.
        ELSE IF ttKeyValue.formatDataType EQ "Date" THEN
            hdBuffer:BUFFER-FIELD(ttKeyValue.formatKey):BUFFER-VALUE = DATE(ttKeyValue.formatValue) NO-ERROR.
        ELSE IF ttKeyValue.formatDataType EQ "DateTime" THEN
            hdBuffer:BUFFER-FIELD(ttKeyValue.formatKey):BUFFER-VALUE = DATETIME(ttKeyValue.formatValue) NO-ERROR.
        ELSE IF ttKeyValue.formatDataType EQ "DateTime-TZ" THEN
            hdBuffer:BUFFER-FIELD(ttKeyValue.formatKey):BUFFER-VALUE = DATETIME-TZ(ttKeyValue.formatValue) NO-ERROR.
        ELSE IF ttKeyValue.formatDataType EQ "Logical" THEN
            hdBuffer:BUFFER-FIELD(ttKeyValue.formatKey):BUFFER-VALUE = LOGICAL(ttKeyValue.formatValue) NO-ERROR.
        ELSE
            hdBuffer:BUFFER-FIELD(ttKeyValue.formatKey):BUFFER-VALUE = ttKeyValue.formatValue.
    END.
    
    lLoaded = TEMP-TABLE ttCeFormatConfig:WRITE-JSON("file", cFile, TRUE).
    
    APPLY "CLOSE" TO THIS-PROCEDURE.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSearch C-Win
ON VALUE-CHANGED OF fiSearch IN FRAME DEFAULT-FRAME /* Search */
DO:
    {&OPEN-QUERY-{&BROWSE-NAME}}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  RUN pInit ( TRUE /* Load From NK1 CeFormatConfig */).
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ON LEAVE OF ttKeyValue.formatValue IN BROWSE BROWSE-2
DO:
    IF ttKeyValue.formatDataType EQ "Integer" THEN
        INTEGER(ttKeyValue.formatValue) NO-ERROR.
    ELSE IF ttKeyValue.formatDataType EQ "Decimal" THEN
        DECIMAL(ttKeyValue.formatValue) NO-ERROR.
    ELSE IF ttKeyValue.formatDataType EQ "Date" THEN
        DATE(ttKeyValue.formatValue) NO-ERROR.
    ELSE IF ttKeyValue.formatDataType EQ "DateTime" THEN
        DATETIME(ttKeyValue.formatValue) NO-ERROR.
    ELSE IF ttKeyValue.formatDataType EQ "DateTime-TZ" THEN
        DATETIME-TZ(ttKeyValue.formatValue) NO-ERROR.
    ELSE IF ttKeyValue.formatDataType EQ "Logical" THEN
        LOGICAL(ttKeyValue.formatValue) NO-ERROR.
                                                    
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE ttKeyValue.formatValue + "' is not a valid value for '" + ttKeyValue.formatKey + "'. Value has to be " + STRING(ttKeyValue.formatDataType EQ "Integer", "an/a") + "'" + ttKeyValue.formatDataType + "'"        
        VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.  
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
  DISPLAY fiSearch 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fiSearch BROWSE-2 btSave btCancel btReset 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
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
    DEFINE INPUT  PARAMETER iplLoadFromNK1 AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE lFound      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValid      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lLoaded     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE hdTT        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iCount      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hdBuffer    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdField     AS HANDLE    NO-UNDO.
    
    IF iplLoadFromNK1 THEN DO:
        RUN sys/ref/nk1look.p (
            INPUT cCompany, 
            INPUT "CEFormatConfig", 
            INPUT "C" /* Logical */, 
            INPUT NO /* check by cust */, 
            INPUT NO /* use cust not vendor */, 
            INPUT "" /* cust */, 
            INPUT "" /* ship-to*/,
            OUTPUT cFile, 
            OUTPUT lFound
            ). 
        
        hdTT = TEMP-TABLE ttCeFormatConfig:HANDLE.
        
        IF lFound AND cFile NE "" THEN 
        DO: 
            RUN FileSys_ValidateFile(cFile, OUTPUT lValid, OUTPUT cMessage).
    
            IF lValid THEN 
                lLoaded = hdTT:READ-JSON("file", cFile, "empty") NO-ERROR.
        END.
    END.
    
    IF NOT lLoaded THEN
        CREATE ttCeFormatConfig.
    ELSE 
        FIND FIRST ttCeFormatConfig.
        
    hdBuffer = BUFFER ttCeFormatConfig:HANDLE.
    
    EMPTY TEMP-TABLE ttKeyValue.
            
    DO iCount = 1 TO hdBuffer:NUM-FIELDS:
        hdField = hdBuffer:BUFFER-FIELD(iCount):HANDLE.
        
        CREATE ttKeyValue.
        ASSIGN
            ttKeyValue.formatKey      = hdField:NAME
            ttKeyValue.formatValue    = STRING(hdField:BUFFER-VALUE)
            ttKeyValue.formatDataType = hdField:DATA-TYPE
            ttKeyValue.formatDesc     = hdField:LABEL
            .
    END.
    
    {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidate C-Win 
PROCEDURE pValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttKeyValue FOR ttKeyValue.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError   AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE iInteger       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE dDecimal       AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dtDate         AS DATE        NO-UNDO.
    DEFINE VARIABLE dtDateTime     AS DATETIME    NO-UNDO.
    DEFINE VARIABLE dttzDateTimeTZ AS DATETIME-TZ NO-UNDO.
    DEFINE VARIABLE lLogical       AS LOGICAL     NO-UNDO.

    IF NOT AVAILABLE ipbf-ttKeyValue THEN
        RETURN.    
    
    IF ipbf-ttKeyValue.formatDataType EQ "Integer" THEN
        iInteger = INTEGER(ipbf-ttKeyValue.formatValue) NO-ERROR.
    ELSE IF ipbf-ttKeyValue.formatDataType EQ "Decimal" THEN
        dDecimal = DECIMAL(ipbf-ttKeyValue.formatValue) NO-ERROR.
    ELSE IF ipbf-ttKeyValue.formatDataType EQ "Date" THEN
        dtDate = DATE(ipbf-ttKeyValue.formatValue) NO-ERROR.
    ELSE IF ipbf-ttKeyValue.formatDataType EQ "DateTime" THEN
        dtDateTime = DATETIME(ipbf-ttKeyValue.formatValue) NO-ERROR.
    ELSE IF ipbf-ttKeyValue.formatDataType EQ "DateTime-TZ" THEN
        dttzDateTimeTZ = DATETIME-TZ(ipbf-ttKeyValue.formatValue) NO-ERROR.
    ELSE IF ipbf-ttKeyValue.formatDataType EQ "Logical" THEN
        lLogical = LOGICAL(ipbf-ttKeyValue.formatValue) NO-ERROR.

    IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "'" + ipbf-ttKeyValue.formatValue + "' is not a valid value for '" + ipbf-ttKeyValue.formatKey + "'. Value has to be " + STRING(ipbf-ttKeyValue.formatDataType EQ "Integer", "an/a") + "'" + ipbf-ttKeyValue.formatDataType + "'"
            .
    END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

