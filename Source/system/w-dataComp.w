&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: system/w-dataComp.w

  Description: To Load and show differences of database and file records

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Rahul Rawat

  Created: Feb 12, 2020

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE hdQuery       AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdTTBuffer    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdTableBuffer AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdTempTable   AS HANDLE  NO-UNDO.
DEFINE VARIABLE iFieldsCount  AS INTEGER NO-UNDO.

DEFINE VARIABLE cTableNames    AS CHARACTER INITIAL "APIInbound,APIInboundDetail,APIOutbound,APIOutboundDetail,APIOutboundTrigger".
DEFINE VARIABLE cCompareResult AS CHARACTER NO-UNDO.
 
/* Temp-Table Definitions */

DEFINE TEMP-TABLE ttAPIInbound NO-UNDO 
    LIKE APIInbound 
    USE-INDEX rec_key     AS PRIMARY
    field differentFields AS CHARACTER
    .

DEFINE TEMP-TABLE ttAPIInboundDetail NO-UNDO
    LIKE APIInboundDetail
    USE-INDEX rec_key     AS PRIMARY
    field differentFields AS CHARACTER
    .

DEFINE TEMP-TABLE ttAPIOutbound NO-UNDO 
    LIKE APIOutbound
    USE-INDEX rec_key     AS PRIMARY
    field differentFields AS CHARACTER
    .

DEFINE TEMP-TABLE ttAPIOutboundDetail NO-UNDO
    LIKE APIOutboundDetail
    USE-INDEX rec_key     AS PRIMARY
    field differentFields AS CHARACTER
    .
       
DEFINE TEMP-TABLE ttAPIOutboundTrigger NO-UNDO
    LIKE ApiOutboundTrigger
    USE-INDEX rec_key     AS PRIMARY
    field differentFields AS CHARACTER
    .  


DEFINE TEMP-TABLE ttUnmatchedData NO-UNDO
    FIELD ttTableName       AS CHARACTER FORMAT "X(32)"  LABEL "Table Name"
    FIELD ttReckey          AS CHARACTER FORMAT "X(21)"  LABEL "REC KEY"
    FIELD ttCompany         AS CHARACTER FORMAT "X(3)"   LABEL "Company"
    FIELd ttPrimaryKey      AS CHARACTER FORMAT "X(96)"  LABEL "Primary Key"
    FIELD ttUnmatchedFields AS CHARACTER FORMAT "X(256)" LABEL "Unmatched Fields"
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brUnmatchedData

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttUnmatchedData

/* Definitions for BROWSE brUnmatchedData                               */
&Scoped-define FIELDS-IN-QUERY-brUnmatchedData ttTableName ttCompany ttPrimaryKey ttUnmatchedFields   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brUnmatchedData   
&Scoped-define SELF-NAME brUnmatchedData
&Scoped-define QUERY-STRING-brUnmatchedData FOR EACH ttUnmatchedData
&Scoped-define OPEN-QUERY-brUnmatchedData OPEN QUERY {&SELF-NAME} FOR EACH ttUnmatchedData.
&Scoped-define TABLES-IN-QUERY-brUnmatchedData ttUnmatchedData
&Scoped-define FIRST-TABLE-IN-QUERY-brUnmatchedData ttUnmatchedData


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brUnmatchedData}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-14 RECT-15 cbTableName brUnmatchedData 
&Scoped-Define DISPLAYED-OBJECTS cbTableName fiFileName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btLoadAndCompare 
     LABEL "Load and Compare" 
     SIZE 23.2 BY 1.14
     FONT 6.

DEFINE BUTTON btSelectFile 
     LABEL "Browse File" 
     SIZE 16.6 BY 1.14
     FONT 6.

DEFINE BUTTON btUpdate 
     LABEL "Update Record" 
     SIZE 19.6 BY 1.52
     FONT 6.

DEFINE BUTTON btViewDifferences 
     LABEL "View Differences" 
     SIZE 20.2 BY 1.52
     FONT 6.

DEFINE VARIABLE cbTableName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Select Table" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 32.6 BY 1
     BGCOLOR 15 FGCOLOR 1 FONT 5 NO-UNDO.

DEFINE VARIABLE fiFileName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Select File" 
     VIEW-AS FILL-IN 
     SIZE 83 BY 1.14
     FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 141.2 BY 3.57.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 60 BY 2.19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brUnmatchedData FOR
                ttUnmatchedData SCROLLING.

&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brUnmatchedData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brUnmatchedData C-Win _FREEFORM
  QUERY brUnmatchedData DISPLAY
      ttTableName  WIDTH 25
ttCompany    WIDTH 12
ttPrimaryKey WIDTH 80
ttUnmatchedFields
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 186 BY 17.14
         FONT 5 ROW-HEIGHT-CHARS .8.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cbTableName AT ROW 2 COL 16.4 COLON-ALIGNED WIDGET-ID 4
     fiFileName AT ROW 3.24 COL 16.2 COLON-ALIGNED WIDGET-ID 6
     btSelectFile AT ROW 3.24 COL 102.4 WIDGET-ID 8
     btLoadAndCompare AT ROW 3.24 COL 120 WIDGET-ID 12
     brUnmatchedData AT ROW 5.29 COL 2.6 WIDGET-ID 200
     btUpdate AT ROW 23.14 COL 63.4 WIDGET-ID 14
     btViewDifferences AT ROW 23.14 COL 98.8 WIDGET-ID 18
     RECT-14 AT ROW 1.48 COL 2.8 WIDGET-ID 10
     RECT-15 AT ROW 22.81 COL 61 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 188.8 BY 24.43
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


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
         TITLE              = "API Data Comparision"
         HEIGHT             = 24.43
         WIDTH              = 188.8
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 273.2
         MAX-BUTTON         = no
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
/* BROWSE-TAB brUnmatchedData btLoadAndCompare DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btLoadAndCompare IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btSelectFile IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btUpdate IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btViewDifferences IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFileName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiFileName:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brUnmatchedData
/* Query rebuild information for BROWSE brUnmatchedData
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttUnmatchedData.
     _END_FREEFORM
     _START_FREEFORM_DEFINE
DEFINE QUERY brUnmatchedData FOR
                ttUnmatchedData SCROLLING.
     _END_FREEFORM_DEFINE
     _Query            is OPENED
*/  /* BROWSE brUnmatchedData */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* API Data Comparision */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* API Data Comparision */
DO:

    IF VALID-HANDLE(hdQuery) THEN 
        DELETE OBJECT hdQuery.
        
    IF VALID-HANDLE(hdTTBuffer) THEN
        DELETE OBJECT hdTTBuffer.
        
    IF VALID-HANDLE(hdTableBuffer) THEN  
        DELETE OBJECT hdTableBuffer.
        
    IF VALID-HANDLE(hdTempTable) THEN
        DELETE OBJECT(hdTempTable).  
                  
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brUnmatchedData
&Scoped-define SELF-NAME brUnmatchedData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brUnmatchedData C-Win
ON DEFAULT-ACTION OF brUnmatchedData IN FRAME DEFAULT-FRAME
DO: 
    IF AVAILABLE ttUnmatchedData THEN DO:
        IF ttUnmatchedData.ttUnmatchedFields NE "Exact Match" AND 
            ttUnmatchedData.ttUnmatchedFields NE "New Record" THEN DO:      
            RUN system/d-dataComp.w(
                INPUT hdTempTable,
                INPUT ttUnmatchedData.ttRecKey,
                INPUT ttUnmatchedData.ttTableName,
                INPUT ttUnmatchedData.ttUnmatchedFields,
                INPUT ttUnmatchedData.ttPrimaryKey           
                ).
            APPLY "CHOOSE":U TO btLoadAndCompare IN FRAME {&FRAME-NAME}.             
        END.
    END.                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brUnmatchedData C-Win
ON VALUE-CHANGED OF brUnmatchedData IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE ttUnmatchedData THEN DO:
        IF ttUnmatchedData.ttUnmatchedFields EQ "Exact Match" THEN
            ASSIGN 
                btUpdate:SENSITIVE          = FALSE
                btViewDifferences:SENSITIVE = FALSE
                .
        ELSE IF ttUnmatchedData.ttUnmatchedFields EQ "New Record" THEN
            ASSIGN
                btUpdate:SENSITIVE          = TRUE
                btViewDifferences:SENSITIVE = FALSE
                .      
        ELSE
            ASSIGN
                btUpdate:SENSITIVE          = TRUE
                btViewDifferences:SENSITIVE = TRUE
                .
    END.                                              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLoadAndCompare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLoadAndCompare C-Win
ON CHOOSE OF btLoadAndCompare IN FRAME DEFAULT-FRAME /* Load and Compare */
DO:
    IF fiFileName:SCREEN-VALUE EQ "" THEN
        MESSAGE "Please select a file name to load"   
            VIEW-AS ALERT-BOX ERROR.
    ELSE DO: 
        IF INDEX(fiFileName:SCREEN-VALUE,cbTableName) EQ 0 THEN DO:
            MESSAGE "The selected file" '"' + fiFileName:SCREEN-VALUE + '"'
                "does not match with the selected table" '"' + cbTableName + '".' SKIP
                "Do you want to continue?"
                VIEW-AS ALERT-BOX WARNING BUTTONS 
                OK-CANCEL UPDATE lCheckFlag as LOGICAL.
            IF NOT lCheckFlag THEN
                RETURN.
        END.                
                                               
        IF VALID-HANDLE(hdTempTable) THEN
            DELETE OBJECT hdTempTable.  
               
        CASE cbTableName:
            WHEN "APIInbound" THEN DO: 
                RUN pLoadAndCompareAPIInbound NO-ERROR.
                hdTempTable = TEMP-TABLE ttAPIInbound:HANDLE.
                CREATE BUFFER hdTTBuffer FOR TABLE "ttAPIInbound".
            END.    
            WHEN "APIInboundDetail" THEN DO:
                RUN pLoadAndCompareAPIInboundDetail NO-ERROR.
                hdTempTable = TEMP-TABLE ttAPIInboundDetail:HANDLE.
                CREATE BUFFER hdTTBuffer FOR TABLE "ttAPIInboundDetail".
            END.       
            WHEN "APIOutbound" THEN DO:
                RUN pLoadAndCompareAPIOutbound NO-ERROR.
                hdTempTable = TEMP-TABLE ttAPIOutbound:HANDLE.
                CREATE Buffer hdTTBuffer FOR TABLE "ttAPIOutbound".
            END.    
            WHEN "APIOutboundDetail" THEN DO:
                RUN pLoadAndCompareAPIOutboundDetail NO-ERROR.
                hdTempTable = TEMP-TABLE ttAPIOutboundDetail:HANDLE.
                CREATE BUFFER hdTTBuffer FOR TABLE "ttAPIOutboundDetail".               
            END.                 
            WHEN "APIOutboundTrigger" THEN DO:
                RUN pLoadAndCompareAPIOutboundTrigger NO-ERROR.  
                hdTempTable = TEMP-TABLE ttAPIOutboundTrigger:HANDLE.
                CREATE BUFFER hdTTBuffer FOR TABLE "ttAPIOutboundTrigger".
            END.                         
        END CASE.
    
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "Invalid FILE" 
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
        
       {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME} 
       
        IF NOT AVAILABLE ttUnmatchedData THEN
            MESSAGE "No records found to load"
                VIEW-AS ALERT-BOX ERROR.

        APPLY "VALUE-CHANGED":U TO brUnmatchedData.               
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSelectFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSelectFile C-Win
ON CHOOSE OF btSelectFile IN FRAME DEFAULT-FRAME /* Browse File */
DO:
    DEFINE VARIABLE cFileName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOKpressed AS LOGICAL   NO-UNDO INITIAL TRUE.
 
    REPEAT:
        SYSTEM-DIALOG GET-FILE cFileName
        TITLE   "Choose Procedure to Run ..."
        FILTERS "Data Files (*.d)"   "*.d"                  
        MUST-EXIST
        USE-FILENAME
        UPDATE lOKpressed.

        IF lOKpressed = TRUE THEN
           fiFileName:SCREEN-VALUE = cFileName.
           LEAVE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdate C-Win
ON CHOOSE OF btUpdate IN FRAME DEFAULT-FRAME /* Update Record */
DO:           
    MESSAGE "Do you want to update the record into the database?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS 
        OK-CANCEL UPDATE lCheckFlag as LOGICAL.
       
    IF lCheckFlag THEN    
        RUN pUpdateRecord.        
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btViewDifferences
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btViewDifferences C-Win
ON CHOOSE OF btViewDifferences IN FRAME DEFAULT-FRAME /* View Differences */
DO: 
    RUN system/d-dataComp.w(
        INPUT hdTempTable,
        INPUT ttUnmatchedData.ttRecKey,
        INPUT ttUnmatchedData.ttTableName,
        INPUT ttUnmatchedData.ttUnmatchedFields,
        INPUT ttUnmatchedData.ttPrimaryKey         
        ).
    APPLY "CHOOSE":U TO btLoadAndCompare IN FRAME {&FRAME-NAME}.                   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbTableName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbTableName C-Win
ON VALUE-CHANGED OF cbTableName IN FRAME DEFAULT-FRAME /* Select Table */
DO:
    ASSIGN {&SELF-NAME}.
    
    IF {&SELF-NAME} NE "" THEN
        ASSIGN 
            fiFilename:SENSITIVE       = TRUE
            btSelectFile:SENSITIVE     = TRUE
            btLoadAndCompare:SENSITIVE = TRUE
            .    
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
  RUN pDisplayTableData. 
  RUN enable_UI.
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
  DISPLAY cbTableName fiFileName 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-14 RECT-15 cbTableName brUnmatchedData 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayTableData C-Win 
PROCEDURE pDisplayTableData PRIVATE :
/*------------------------------------------------------------------------------
  Purpose: To Populate the Select table Combo-Box Widget   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    cbTableName:LIST-ITEMS IN FRAME {&FRAME-NAME} = cTableNames.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLoadAndCompareAPIInbound C-Win 
PROCEDURE pLoadAndCompareAPIInbound PRIVATE :
/*------------------------------------------------------------------------------
  Purpose: To load the data into the Temp-Table from the selected file and compare
           it with the Database records  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {system\loadAndCompare.i &param1="fiFileName:SCREEN-VALUE IN FRAME {&FRAME-NAME}"  &param2="APIInbound" &param3="apiRoute"}    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLoadAndCompareAPIInboundDetail C-Win 
PROCEDURE pLoadAndCompareAPIInboundDetail PRIVATE :
/*------------------------------------------------------------------------------
  Purpose: To load the data into the Temp-Table from the selected file and compare
           it with the Database records     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {system\loadAndCompare.i &param1 = "fiFileName:SCREEN-VALUE IN FRAME {&FRAME-NAME}" &param2 = "APIInboundDetail" &param3 = "apiRoute" &param4 = "detailID"}    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLoadAndCompareAPIOutbound C-Win 
PROCEDURE pLoadAndCompareAPIOutbound PRIVATE :
/*------------------------------------------------------------------------------
  Purpose: To load the data into the Temp-Table from the selected file and compare
           it with the Database records    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {system\loadandcompare.i &param1 = "fiFileName:SCREEN-VALUE IN FRAME {&FRAME-NAME}" &param2 = "APIOutbound" &param3 = "apiID" &param4 = "clientID"}   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLoadAndCompareAPIOutboundDetail C-Win 
PROCEDURE pLoadAndCompareAPIOutboundDetail PRIVATE :
/*------------------------------------------------------------------------------
  Purpose: To load the data into the Temp-Table from the selected file and compare
           it with the Database records     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {system\loadandcompare.i &param1 = "fiFileName:SCREEN-VALUE IN FRAME {&FRAME-NAME}" &param2 = "APIoutboundDetail" &param3 = "apiID" &param4 = "clientID" &param5 = "detailID"}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLoadAndCompareAPIOutboundTrigger C-Win 
PROCEDURE pLoadAndCompareAPIOutboundTrigger :
/*------------------------------------------------------------------------------
  Purpose: To load the data into the Temp-Table from the selected file and compare
           it with the Database records      
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {system\loadandcompare.i &param1 = "fiFileName:SCREEN-VALUE IN FRAME {&FRAME-NAME}" &param2 = "APIOutboundTrigger" &param3 = "apiID" &param4 = "clientID" &param5 = "triggerID"}      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRecord C-Win 
PROCEDURE pUpdateRecord PRIVATE :
/*------------------------------------------------------------------------------
  Purpose: To Update the Temp-Table record into Database   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lCreated AS LOGICAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    CREATE BUFFER hdTableBuffer FOR TABLE cbTableName.

    CREATE QUERY hdQuery.
    
    DO TRANSACTION:
        IF ttUnmatchedData.ttUnmatchedFields EQ "New Record" THEN DO:
            hdQuery:SET-BUFFERS(hdTTBuffer).

            hdQuery:QUERY-PREPARE("FOR EACH " + hdTTBuffer:NAME +
                                     " WHERE " + hdTTBuffer:NAME + ".rec_key" + " EQ " + '"' + ttUnmatchedData.ttRecKey + '"').
            hdQuery:QUERY-OPEN.
            hdQuery:GET-FIRST.
            
            IF hdTTBuffer:AVAILABLE THEN DO:
                lCreated = hdTableBuffer:BUFFER-CREATE().
                IF lCreated THEN 
                    hdTableBuffer:BUFFER-COPY(hdTTBuffer).
            END.                    
        END. 
    
        ELSE IF ttUnmatchedData.ttUnmatchedFields NE "Exact Match" THEN DO :
            hdQuery:SET-BUFFERS(hdTableBuffer, hdTTBuffer).
      
            hdQuery:QUERY-PREPARE("FOR EACH "  + hdTableBuffer:name + " EXCLUSIVE-LOCK " +
                                     " WHERE " + hdTableBuffer:NAME + ".rec_key" + " EQ " + '"'+ ttUnmatchedData.ttRecKey + '"'  +
                                    ", FIRST " + hdTTBuffer:NAME +
                                     " WHERE " + hdTableBuffer:NAME + ".rec_key" + " EQ " + hdTTBuffer:NAME + ".rec_key" ).
            hdQuery:QUERY-OPEN.
            hdQuery:GET-FIRST.
            
            IF hdTableBuffer:AVAILABLE AND hdTTBuffer:AVAILABLE THEN DO:
                DO iFieldsCount = 1 TO NUM-ENTRIES(ttUnmatchedData.ttUnmatchedFields): 
                    hdTableBuffer:BUFFER-FIELD(ENTRY(iFieldsCount,ttUnmatchedData.ttUnmatchedFields)):BUFFER-VALUE = hdTTBuffer:BUFFER-FIELD(ENTRY(iFieldsCount,ttUnmatchedData.ttUnmatchedFields)):BUFFER-VALUE.      
                END.
            END.                
        END.
        ttUnmatchedData.ttUnmatchedFields = "Exact Match".     
    END.
            
    hdTableBuffer:BUFFER-RELEASE(). 
    
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME} 
      
    MESSAGE "Data Updated Successfully"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

