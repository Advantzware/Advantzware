&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: sectionUpdater.p

  Description: mini section editor utility

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 5.2.2017

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

&SCOPED-DEFINE ScriptsDat C:\Advantzware\v16\Scripts\Conv16to17\ScriptsDat
&SCOPED-DEFINE fileBase C:\Advantzware\v16\Legacy

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttSection NO-UNDO
    FIELD sectionName     AS CHARACTER FORMAT "x(50)" LABEL "Section"
    FIELD sectionLine     AS INTEGER   FORMAT ">>>>9" LABEL "Line"
    FIELD sectionContent  AS CHARACTER
    FIELD sectionValidate AS CHARACTER FORMAT "x"     LABEL "*"
    FIELD sectionModified AS LOGICAL   LABEL "Mod"
        INDEX ttSection IS PRIMARY sectionName
    .

DEFINE TEMP-TABLE ttFileCode NO-UNDO
    FIELD ln AS INT
    FIELD lnText AS CHAR
        INDEX ttFileCode IS PRIMARY ln
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME bSections

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttSection

/* Definitions for BROWSE bSections                                     */
&Scoped-define FIELDS-IN-QUERY-bSections ttSection.sectionLine ttSection.sectionName ttSection.sectionValidate ttSection.sectionModified   
&Scoped-define ENABLED-FIELDS-IN-QUERY-bSections   
&Scoped-define SELF-NAME bSections
&Scoped-define QUERY-STRING-bSections FOR EACH ttSection
&Scoped-define OPEN-QUERY-bSections OPEN QUERY {&SELF-NAME} FOR EACH ttSection.
&Scoped-define TABLES-IN-QUERY-bSections ttSection
&Scoped-define FIRST-TABLE-IN-QUERY-bSections ttSection


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-bSections}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCancel btnOK bSections fileNames ~
sectionEditor insertYes insertNo 
&Scoped-Define DISPLAYED-OBJECTS fileNames sectionEditor showFileName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON insertNo 
     LABEL "~{&methods/lValidateError.i NO}" 
     SIZE 32 BY 1.14.

DEFINE BUTTON insertYes 
     LABEL "~{&methods/lValidateError.i YES}" 
     SIZE 32 BY 1.14.

DEFINE VARIABLE sectionEditor AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 153 BY 32.38
     FONT 0 NO-UNDO.

DEFINE VARIABLE showFileName AS CHARACTER FORMAT "X(256)":U 
     LABEL "File" 
     VIEW-AS FILL-IN 
     SIZE 134 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fileNames AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 40 BY 34.52 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY bSections FOR 
      ttSection SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE bSections
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS bSections C-Win _FREEFORM
  QUERY bSections DISPLAY
      ttSection.sectionLine
      ttSection.sectionName
      ttSection.sectionValidate
      ttSection.sectionModified
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 69 BY 35.24 ROW-HEIGHT-CHARS .57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnCancel AT ROW 34.33 COL 257 WIDGET-ID 10
     btnOK AT ROW 34.33 COL 250 WIDGET-ID 12
     bSections AT ROW 1 COL 42 WIDGET-ID 200
     fileNames AT ROW 1.71 COL 1 NO-LABEL WIDGET-ID 2
     sectionEditor AT ROW 1.71 COL 112 NO-LABEL WIDGET-ID 4
     showFileName AT ROW 34.33 COL 114 COLON-ALIGNED WIDGET-ID 18
     insertYes AT ROW 35.29 COL 153 WIDGET-ID 14
     insertNo AT ROW 35.29 COL 186 WIDGET-ID 16
     "Files" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1 COL 2 WIDGET-ID 6
     "Section Editor" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1 COL 113 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 264.4 BY 35.43 WIDGET-ID 100.


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
         TITLE              = "Section Updater"
         HEIGHT             = 35.43
         WIDTH              = 264.4
         MAX-HEIGHT         = 35.43
         MAX-WIDTH          = 264.4
         VIRTUAL-HEIGHT     = 35.43
         VIRTUAL-WIDTH      = 264.4
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
/* BROWSE-TAB bSections TEXT-2 DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN showFileName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       showFileName:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE bSections
/* Query rebuild information for BROWSE bSections
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSection.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE bSections */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Section Updater */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Section Updater */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME bSections
&Scoped-define SELF-NAME bSections
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bSections C-Win
ON VALUE-CHANGED OF bSections IN FRAME DEFAULT-FRAME
DO:
    ASSIGN
        sectionEditor:SCREEN-VALUE = ttSection.sectionContent
        sectionEditor:MODIFIED     = NO
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME
DO:
  IF CAN-FIND(FIRST ttSection
              WHERE ttSection.sectionModified) THEN DO:
      MESSAGE 
          "Section Changes Exist, Save Changes?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
          UPDATE lSaveChanges AS LOGICAL.
      IF lSaveChanges EQ YES THEN
      RUN pUpdateFile.
      ELSE IF lSaveChanges EQ ? THEN
      RETURN NO-APPLY.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK C-Win
ON CHOOSE OF btnOK IN FRAME DEFAULT-FRAME
DO:
    APPLY "LEAVE":U TO sectionEditor.
    RUN pUpdateFile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fileNames
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fileNames C-Win
ON VALUE-CHANGED OF fileNames IN FRAME DEFAULT-FRAME
DO:
  ASSIGN
      {&SELF-NAME}
      cFileName = SEARCH("{&fileBase}\" + {&SELF-NAME})
      showFileName:SCREEN-VALUE = cFileName
      .
  RUN pParseFileName.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME insertNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL insertNo C-Win
ON CHOOSE OF insertNo IN FRAME DEFAULT-FRAME /* {methods/lValidateError.i NO} */
DO:
  sectionEditor:INSERT-STRING({&SELF-NAME}:LABEL).
  APPLY "ENTRY":U TO sectionEditor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME insertYes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL insertYes C-Win
ON CHOOSE OF insertYes IN FRAME DEFAULT-FRAME /* {methods/lValidateError.i YES} */
DO:
  sectionEditor:INSERT-STRING({&SELF-NAME}:LABEL).
  APPLY "ENTRY":U TO sectionEditor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sectionEditor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sectionEditor C-Win
ON ENTRY OF sectionEditor IN FRAME DEFAULT-FRAME
DO:
    IF ttSection.sectionModified THEN
    {&SELF-NAME}:MODIFIED = ttSection.sectionModified.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sectionEditor C-Win
ON LEAVE OF sectionEditor IN FRAME DEFAULT-FRAME
DO:
  IF {&SELF-NAME}:MODIFIED THEN DO:
      ASSIGN
          {&SELF-NAME}
          ttSection.sectionModified = {&SELF-NAME}:MODIFIED
          ttSection.sectionContent  = {&SELF-NAME}
          .
      bSections:REFRESH().
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sectionEditor C-Win
ON TAB OF sectionEditor IN FRAME DEFAULT-FRAME
DO:
  {&SELF-NAME}:INSERT-STRING("~t").
  RETURN NO-APPLY.
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
  RUN pGetFileNames.
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
  DISPLAY fileNames sectionEditor showFileName 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnCancel btnOK bSections fileNames sectionEditor insertYes insertNo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCodeFile C-Win 
PROCEDURE pCodeFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ipidx   AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcText AS CHARACTER NO-UNDO.

    CREATE ttFileCode.
    ASSIGN
        ipidx = ipidx + 1
        ttFileCode.ln = ipidx
        ttFileCode.lnText = ipcText
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetFileNames C-Win 
PROCEDURE pGetFileNames :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

    fileNames:LIST-ITEMS IN FRAME {&FRAME-NAME} = ?.
    INPUT FROM "{&ScriptsDat}\admViewers.dat" NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cFileName.
        fileNames:ADD-LAST(cFileName) IN FRAME {&FRAME-NAME}.
    END. /* repeat */
    INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pParseFileName C-Win 
PROCEDURE pParseFileName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFileLine AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx       AS INTEGER   NO-UNDO.

    EMPTY TEMP-TABLE ttSection.
    INPUT FROM VALUE(cFileName) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cFileLine.
        jdx = jdx + 1.
        IF cFileLine BEGINS "~&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL" THEN DO:
            IMPORT UNFORMATTED cFileLine.
            jdx = jdx + 1.
            CREATE ttSection.
            DO idx = 1 TO 4:
                ttSection.sectionName = ttSection.sectionName + ENTRY(idx,cFileLine," ") + " ".
            END.
            ASSIGN
                ttSection.sectionLine = jdx
                ttSection.sectionName = TRIM(ttSection.sectionName)
                .
            REPEAT:
                IMPORT UNFORMATTED cFileLine.
                jdx = jdx + 1.
                IF cFileLine BEGINS "/* _UIB-CODE-BLOCK-END */" THEN LEAVE.
                ttSection.sectionContent = ttSection.sectionContent + cFileLine + CHR(10).
                IF ttSection.sectionName BEGINS "ON LEAVE OF" AND
                   INDEX(cFileLine,"RETURN NO-APPLY") NE 0 THEN
                ttSection.sectionValidate = "*".
            END.
        END.
        ELSE IF cFileLine BEGINS "~&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE" THEN DO:
            CREATE ttSection.
            ASSIGN
                ttSection.sectionLine = jdx + 1
                ttSection.sectionName = "PROCEDURE " + ENTRY(4,cFileLine," ")
                .
            IMPORT UNFORMATTED cFileLine.
            jdx = jdx + 1.
            IF cFileLine BEGINS "PROCEDURE valid" THEN
            ttSection.sectionValidate = "*".
            REPEAT:
                IMPORT UNFORMATTED cFileLine.
                jdx = jdx + 1.
                IF cFileLine BEGINS "/* _UIB-CODE-BLOCK-END */" THEN LEAVE.
                ttSection.sectionContent = ttSection.sectionContent + cFileLine + CHR(10).
            END.
        END.
    END. /* repeat */
    INPUT CLOSE.
    {&OPEN-QUERY-{&BROWSE-NAME}}
    APPLY "VALUE-CHANGED":U TO bSections IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateFile C-Win 
PROCEDURE pUpdateFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFileLine AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE kdx       AS INTEGER   NO-UNDO.

    IF CAN-FIND(FIRST ttSection
                WHERE ttSection.sectionModified EQ YES) THEN DO:
        EMPTY TEMP-TABLE ttFileCode.
        INPUT FROM VALUE(cFileName).
        REPEAT:
            IMPORT UNFORMATTED cFileLine.
            jdx = jdx + 1.
            IF CAN-FIND(FIRST ttSection
                        WHERE ttSection.sectionModified EQ YES
                          AND ttSection.sectionLine     EQ jdx) THEN DO:
                FIND FIRST ttSection
                     WHERE ttSection.sectionModified EQ YES
                       AND ttSection.sectionLine     EQ jdx.
                RUN pCodeFile (INPUT-OUTPUT idx, cFileLine).
                DO kdx = 1 TO NUM-ENTRIES(ttSection.sectionContent,CHR(10)):
                    cFileLine = TRIM(ENTRY(kdx,ttSection.sectionContent,CHR(10)),CHR(10)).
                    RUN pCodeFile (INPUT-OUTPUT idx, cFileLine).
                END. /* do kdx */
                REPEAT:
                    IMPORT UNFORMATTED cFileLine.
                    jdx = jdx + 1.
                    IF cFileLine BEGINS "/* _UIB-CODE-BLOCK-END */" THEN LEAVE.
                END. /* repeat */
            END. /* if can-find */
            RUN pCodeFile (INPUT-OUTPUT idx, cFileLine).
        END. /* repeat */
        INPUT CLOSE.

        OUTPUT TO VALUE(cFileName).
        FOR EACH ttFileCode:
            IF ttFileCode.lnText NE "" THEN
            PUT UNFORMATTED ttFileCode.lnText SKIP.
            ELSE
            PUT UNFORMATTED SKIP(1).
        END. /* for each */
        OUTPUT CLOSE.

        RUN pParseFileName.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

