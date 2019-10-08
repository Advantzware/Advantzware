&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: dynImportExport.w

  Description: Import and Export of Dynamic Subjects, Parameters and Lookups

  Input Parameters: <none>

  Output Parameters: Logical indicating if Import run to refresh dynSubjct.w

  Author: Ron Stark

  Created: 8.14.2019

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

DEFINE OUTPUT PARAMETER oplRefresh AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cDynamicParam   AS CHARACTER NO-UNDO INITIAL
    "dynParamSet,dynParamSetDtl,dynParam".
DEFINE VARIABLE cDynamicSubject AS CHARACTER NO-UNDO INITIAL
    "dynSubject,dynSubjectTable,dynSubjectWhere,dynSubjectColumn,dynSubjectParamSet,dynParamValue".
DEFINE VARIABLE cFolder         AS CHARACTER NO-UNDO.
DEFINE VARIABLE idx             AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttDynLookup          NO-UNDO LIKE dynLookup.
DEFINE TEMP-TABLE ttDynParam           NO-UNDO LIKE dynParam.
DEFINE TEMP-TABLE ttDynParamSet        NO-UNDO LIKE dynParamSet
    FIELD exportParamSet AS LOGICAL LABEL "Export".
DEFINE TEMP-TABLE ttDynParamSetDtl     NO-UNDO LIKE dynParamSetDtl.
DEFINE TEMP-TABLE ttDynParamValue      NO-UNDO LIKE dynParamValue.
DEFINE TEMP-TABLE ttDynSubject         NO-UNDO LIKE dynSubject
    FIELD exportSubject AS LOGICAL LABEL "Export".
DEFINE TEMP-TABLE ttDynSubjectColumn   NO-UNDO LIKE dynSubjectColumn.
DEFINE TEMP-TABLE ttDynSubjectParamSet NO-UNDO LIKE dynSubjectParamSet.
DEFINE TEMP-TABLE ttDynSubjectTable    NO-UNDO LIKE dynSubjectTable.
DEFINE TEMP-TABLE ttDynSubjectWhere    NO-UNDO LIKE dynSubjectWhere.

{methods/defines/sortByDefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME paramSetBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttDynParamSet ttDynSubject

/* Definitions for BROWSE paramSetBrowse                                */
&Scoped-define FIELDS-IN-QUERY-paramSetBrowse ttDynParamSet.setName ttDynParamSet.paramSetID ttDynParamSet.paramSetType ttDynParamSet.exportParamSet   
&Scoped-define ENABLED-FIELDS-IN-QUERY-paramSetBrowse ttDynParamSet.exportParamSet   
&Scoped-define ENABLED-TABLES-IN-QUERY-paramSetBrowse ttDynParamSet
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-paramSetBrowse ttDynParamSet
&Scoped-define SELF-NAME paramSetBrowse
&Scoped-define QUERY-STRING-paramSetBrowse FOR EACH ttDynParamSet ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-paramSetBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttDynParamSet ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-paramSetBrowse ttDynParamSet
&Scoped-define FIRST-TABLE-IN-QUERY-paramSetBrowse ttDynParamSet


/* Definitions for BROWSE subjectBrowse                                 */
&Scoped-define FIELDS-IN-QUERY-subjectBrowse ttDynSubject.subjectTitle ttDynSubject.subjectID ttDynSubject.subjectType ttDynSubject.exportSubject   
&Scoped-define ENABLED-FIELDS-IN-QUERY-subjectBrowse ttDynSubject.exportSubject   
&Scoped-define ENABLED-TABLES-IN-QUERY-subjectBrowse ttDynSubject
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-subjectBrowse ttDynSubject
&Scoped-define SELF-NAME subjectBrowse
&Scoped-define QUERY-STRING-subjectBrowse FOR EACH ttDynSubject ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-subjectBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttDynSubject ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-subjectBrowse ttDynSubject
&Scoped-define FIRST-TABLE-IN-QUERY-subjectBrowse ttDynSubject


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-paramSetBrowse}~
    ~{&OPEN-QUERY-subjectBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS subjectBrowse btnExit paramSetBrowse ~
btnExport lExportSubject lExportParamSet cImportExportFolder btnImport ~
lExportDynLookup 
&Scoped-Define DISPLAYED-OBJECTS lExportSubject lExportParamSet ~
cImportExportFolder lExportDynLookup 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnExit 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "E&xit" 
     SIZE 8 BY 1.91 TOOLTIP "Exit"
     FONT 4.

DEFINE BUTTON btnExport 
     IMAGE-UP FILE "Graphics/32x32/export.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Export" 
     SIZE 8 BY 1.91 TOOLTIP "Export"
     FONT 4.

DEFINE BUTTON btnImport 
     IMAGE-UP FILE "Graphics/32x32/import.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Import" 
     SIZE 8 BY 1.91 TOOLTIP "Import"
     FONT 4.

DEFINE VARIABLE cImportExportFolder AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\tmp" 
     LABEL "Import/Export Folder" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE RECTANGLE portRect
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 62 BY 3.57
     BGCOLOR 8 .

DEFINE VARIABLE lExportDynLookup AS LOGICAL INITIAL no 
     LABEL "Export Dynamic Lookup Table" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE lExportParamSet AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE lExportSubject AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 2.6 BY .62 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY paramSetBrowse FOR 
      ttDynParamSet SCROLLING.

DEFINE QUERY subjectBrowse FOR 
      ttDynSubject SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE paramSetBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS paramSetBrowse C-Win _FREEFORM
  QUERY paramSetBrowse DISPLAY
      ttDynParamSet.setName LABEL-BGCOLOR 14
ttDynParamSet.paramSetID LABEL-BGCOLOR 14
ttDynParamSet.paramSetType LABEL-BGCOLOR 14
ttDynParamSet.exportParamSet VIEW-AS TOGGLE-BOX
ENABLE
ttDynParamSet.exportParamSet
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 62 BY 24.76
         TITLE "Dynamic Parameter Sets".

DEFINE BROWSE subjectBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS subjectBrowse C-Win _FREEFORM
  QUERY subjectBrowse NO-LOCK DISPLAY
      ttDynSubject.subjectTitle LABEL-BGCOLOR 14
ttDynSubject.subjectID LABEL-BGCOLOR 14
ttDynSubject.subjectType LABEL-BGCOLOR 14
ttDynSubject.exportSubject VIEW-AS TOGGLE-BOX
ENABLE
ttDynSubject.exportSubject
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 62 BY 28.57
         TITLE "Dynamic Subjects".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     subjectBrowse AT ROW 1 COL 2 WIDGET-ID 200
     btnExit AT ROW 27.43 COL 118 HELP
          "Exit Design Layout Window" WIDGET-ID 32
     paramSetBrowse AT ROW 1 COL 65 WIDGET-ID 300
     btnExport AT ROW 27.43 COL 110 HELP
          "Export" WIDGET-ID 26
     lExportSubject AT ROW 1.24 COL 55 WIDGET-ID 2
     lExportParamSet AT ROW 1.24 COL 118 WIDGET-ID 38
     cImportExportFolder AT ROW 26.24 COL 86 COLON-ALIGNED HELP
          "Enter Import~\Export Folder" WIDGET-ID 4
     btnImport AT ROW 27.43 COL 102 HELP
          "Import" WIDGET-ID 24
     lExportDynLookup AT ROW 27.91 COL 68 WIDGET-ID 36
     portRect AT ROW 26 COL 65 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 127 BY 28.57
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
         TITLE              = "Dynamics Import/Export"
         HEIGHT             = 28.57
         WIDTH              = 127
         MAX-HEIGHT         = 28.57
         MAX-WIDTH          = 127
         VIRTUAL-HEIGHT     = 28.57
         VIRTUAL-WIDTH      = 127
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* BROWSE-TAB subjectBrowse 1 DEFAULT-FRAME */
/* BROWSE-TAB paramSetBrowse btnExit DEFAULT-FRAME */
ASSIGN 
       paramSetBrowse:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR RECTANGLE portRect IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       subjectBrowse:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE paramSetBrowse
/* Query rebuild information for BROWSE paramSetBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttDynParamSet ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE paramSetBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE subjectBrowse
/* Query rebuild information for BROWSE subjectBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttDynSubject ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE subjectBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Dynamics Import/Export */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Dynamics Import/Export */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit C-Win
ON CHOOSE OF btnExit IN FRAME DEFAULT-FRAME /* Exit */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExport C-Win
ON CHOOSE OF btnExport IN FRAME DEFAULT-FRAME /* Export */
DO:
    ASSIGN
        lExportDynLookup
        cImportExportFolder
        .
    RUN pExport.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnImport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImport C-Win
ON CHOOSE OF btnImport IN FRAME DEFAULT-FRAME /* Import */
DO:
    ASSIGN cImportExportFolder.
    RUN pImport.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cImportExportFolder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cImportExportFolder C-Win
ON HELP OF cImportExportFolder IN FRAME DEFAULT-FRAME /* Import/Export Folder */
DO:
    cFolder = cImportExportFolder.
    SYSTEM-DIALOG GET-DIR cFolder INITIAL-DIR cFolder TITLE "Dynamic Import/Export Folder".
    IF cFolder NE ? THEN
    ASSIGN
        cImportExportFolder:SCREEN-VALUE = cFolder
        cImportExportFolder
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lExportParamSet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lExportParamSet C-Win
ON VALUE-CHANGED OF lExportParamSet IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    FOR EACH ttDynParamSet:
        ttDynParamSet.exportParamSet = {&SELF-NAME}.
    END. /* each ttdynParamSet */
    IF CAN-FIND(FIRST ttDynParamSet) THEN
    BROWSE paramSetBrowse:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lExportSubject
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lExportSubject C-Win
ON VALUE-CHANGED OF lExportSubject IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    FOR EACH ttDynSubject:
        ttDynSubject.exportSubject = {&SELF-NAME}.
    END. /* each ttdynsubject */
    IF CAN-FIND(FIRST ttDynSubject) THEN
    BROWSE subjectBrowse:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME paramSetBrowse
&Scoped-define SELF-NAME paramSetBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL paramSetBrowse C-Win
ON START-SEARCH OF paramSetBrowse IN FRAME DEFAULT-FRAME /* Dynamic Parameter Sets */
DO:
    IF SELF:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = SELF:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        cSaveLabel = cColumnLabel.
        RUN pReopenBrowse.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME subjectBrowse
&Scoped-define SELF-NAME subjectBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectBrowse C-Win
ON START-SEARCH OF subjectBrowse IN FRAME DEFAULT-FRAME /* Dynamic Subjects */
DO:
    IF SELF:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = SELF:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        cSaveLabel = cColumnLabel.
        RUN pReopenBrowse.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME paramSetBrowse
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
  lExportSubject:MOVE-TO-TOP().
  lExportParamSet:MOVE-TO-TOP().
  RUN pInit.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

&Scoped-define sdBrowseName subjectBrowse
{methods/sortByProc.i "pBySubjectTitle" "ttDynSubject.subjectTitle"}
{methods/sortByProc.i "pBySubjectID" "ttDynSubject.subjectID"}
{methods/sortByProc.i "pBySubjectType" "ttDynSubject.subjectType"}

&Scoped-define sdBrowseName paramSetBrowse
{methods/sortByProc.i "pByParamSetID" "ttDynParamSet.paramSetID"}
{methods/sortByProc.i "pBySetName" "ttDynParamSet.setName"}
{methods/sortByProc.i "pByParamSetType" "ttDynParamSet.paramSetType"}

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
  DISPLAY lExportSubject lExportParamSet cImportExportFolder lExportDynLookup 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE subjectBrowse btnExit paramSetBrowse btnExport lExportSubject 
         lExportParamSet cImportExportFolder btnImport lExportDynLookup 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pExport C-Win 
PROCEDURE pExport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bDynParamSet FOR ttDynParamSet.
    DEFINE BUFFER bDynSubject  FOR ttDynSubject.

    SESSION:SET-WAIT-STATE("General").
    DO idx = 1 TO NUM-ENTRIES(cDynamicSubject):
        FIND FIRST ASI._file NO-LOCK
             WHERE ASI._file._file-name EQ ENTRY(idx,cDynamicSubject)
             NO-ERROR.
        IF NOT AVAILABLE ASI._file THEN NEXT.
        OUTPUT TO VALUE(cImportExportFolder + "\" + ASI._file._Dump-name + ".d").
        FOR EACH bDynSubject
            WHERE bDynSubject.exportSubject EQ YES
            :
            CASE ENTRY(idx,cDynamicSubject):
                WHEN "dynSubject" THEN
                EXPORT bDynSubject EXCEPT exportSubject.
                WHEN "dynSubjectTable" THEN
                FOR EACH dynSubjectTable NO-LOCK
                    WHERE dynSubjectTable.subjectID EQ bDynSubject.subjectID
                    :
                    EXPORT dynSubjectTable.
                END. /* each dynsubjecttable */
                WHEN "dynSubjectWhere" THEN
                FOR EACH dynSubjectWhere NO-LOCK
                    WHERE dynSubjectWhere.subjectID EQ bDynSubject.subjectID
                    :
                    EXPORT dynSubjectWhere.
                END. /* each dynsubjectWhere */
                WHEN "dynSubjectColumn" THEN
                FOR EACH dynSubjectColumn NO-LOCK
                    WHERE dynSubjectColumn.subjectID EQ bDynSubject.subjectID
                    :
                    EXPORT dynSubjectColumn.
                END. /* each dynsubjectColumn */
                WHEN "dynSubjectParamSet" THEN
                FOR EACH dynSubjectParamSet NO-LOCK
                    WHERE dynSubjectParamSet.subjectID EQ bDynSubject.subjectID
                    :
                    EXPORT dynSubjectParamSet.
                END. /* each dynsubjectParamSet */
                WHEN "dynParamValue" THEN
                FOR EACH dynParamValue NO-LOCK
                    WHERE dynParamValue.subjectID EQ bDynSubject.subjectID
                      AND dynParamValue.user-id   EQ "_default"
                    :
                    EXPORT dynParamValue.
                END. /* each dynParamValue */
            END CASE.
        END. /* each ttdynsubject */
        OUTPUT CLOSE.
    END. /* do idx */
    EMPTY TEMP-TABLE ttDynParam.
    DO idx = 1 TO NUM-ENTRIES(cDynamicParam):
        FIND FIRST ASI._file NO-LOCK
             WHERE ASI._file._file-name EQ ENTRY(idx,cDynamicParam)
             NO-ERROR.
        IF NOT AVAILABLE ASI._file THEN NEXT.
        OUTPUT TO VALUE(cImportExportFolder + "\" + ASI._file._Dump-name + ".d").
        IF ENTRY(idx,cDynamicParam) NE "dynParam" THEN
        FOR EACH bDynParamSet
            WHERE bDynParamSet.exportParamSet EQ YES
            :
            CASE ENTRY(idx,cDynamicParam):
                WHEN "dynParamSet" THEN
                EXPORT bDynParamSet EXCEPT exportParamSet.
                WHEN "dynParamSetDtl" THEN
                FOR EACH dynParamSetDtl NO-LOCK
                    WHERE dynParamSetDtl.paramSetID EQ bDynParamSet.paramSetID
                    :
                    EXPORT dynParamSetDtl.
                    IF CAN-FIND(FIRST ttDynParam
                                WHERE ttDynParam.paramID EQ dynParamSetDtl.paramID) THEN
                    NEXT.
                    CREATE ttDynParam.
                    ttDynParam.paramID = dynParamSetDtl.paramID.
                END. /* each dynParamSetDtl */
            END CASE.
        END. /* each bdynparamset */
        ELSE
        FOR EACH ttDynParam NO-LOCK
            BY ttDynParam.paramID
            :
            FIND FIRST dynParam NO-LOCK
                 WHERE dynParam.paramID EQ ttDynParam.paramID
                 NO-ERROR.
            IF AVAILABLE dynParam THEN
            EXPORT dynParam.
        END. /* each ttdynParam */
        OUTPUT CLOSE.
    END. /* do idx */
    FIND FIRST ASI._file NO-LOCK
         WHERE ASI._file._file-name EQ "dynLookup"
         NO-ERROR.
    IF AVAILABLE ASI._file THEN DO:
        OUTPUT TO VALUE(cImportExportFolder + "\" + ASI._file._Dump-name + ".d").
        IF lExportDynLookup THEN
        FOR EACH dynLookup NO-LOCK:
            EXPORT dynLookup.
        END. /* each dynlookup */
    END. /* if avail */
    SESSION:SET-WAIT-STATE("").
    MESSAGE
        "Dynamic Table Export Complete"
    VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDynParamSet C-Win 
PROCEDURE pGetDynParamSet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttDynParamSet.
    
    FOR EACH dynParamSet NO-LOCK:
        CREATE ttDynParamSet.
        BUFFER-COPY dynParamSet TO ttDynParamSet.
    END. /* each dynParamSet */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDynSubject C-Win 
PROCEDURE pGetDynSubject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttDynSubject.
    
    FOR EACH dynSubject NO-LOCK:
        CREATE ttDynSubject.
        BUFFER-COPY dynSubject TO ttDynSubject.
    END. /* each dynsubject */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pImport C-Win 
PROCEDURE pImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDynamicFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDynamicTable AS CHARACTER NO-UNDO INITIAL "dynLookup,".
    
    MESSAGE
        "Import Dynamic Tables from Folder: ~"" + cImportExportFolder + "~"?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE lImport AS LOGICAL.
    IF NOT lImport THEN RETURN.

    SESSION:SET-WAIT-STATE("General").
    ASSIGN
        cDynamicTable = cDynamicTable + cDynamicSubject + "," + cDynamicParam
        oplRefresh    = YES
        .    
    EMPTY TEMP-TABLE ttDynLookup.
    CREATE ttDynLookup.
    EMPTY TEMP-TABLE ttDynParam.
    CREATE ttDynParam.
    EMPTY TEMP-TABLE ttDynParamSet.
    CREATE ttDynParamSet.
    EMPTY TEMP-TABLE ttDynParamSetDtl.
    CREATE ttDynParamSetDtl.
    EMPTY TEMP-TABLE ttDynParamValue.
    CREATE ttDynParamValue.
    EMPTY TEMP-TABLE ttDynSubject.
    CREATE ttDynSubject.
    EMPTY TEMP-TABLE ttDynSubjectColumn.
    CREATE ttDynSubjectColumn.
    EMPTY TEMP-TABLE ttDynSubjectParamSet.
    CREATE ttDynSubjectParamSet.
    EMPTY TEMP-TABLE ttDynSubjectTable.
    CREATE ttDynSubjectTable.
    EMPTY TEMP-TABLE ttDynSubjectWhere.
    CREATE ttDynSubjectWhere.
    
    DO idx = 1 TO NUM-ENTRIES(cDynamicTable):
        FIND FIRST ASI._file NO-LOCK
             WHERE ASI._file._file-name EQ ENTRY(idx,cDynamicTable)
             NO-ERROR.
        IF NOT AVAILABLE ASI._file THEN NEXT.
        cDynamicFile = cImportExportFolder + "\" + ASI._file._Dump-name + ".d".
        IF SEARCH(cDynamicFile) EQ ? THEN NEXT.
        INPUT FROM VALUE(cDynamicFile) NO-ECHO.
        REPEAT:
            CASE ENTRY(idx,cDynamicTable):
                WHEN "dynLookup" THEN DO:
                    IMPORT ttDynLookup.
                    FIND FIRST dynLookup EXCLUSIVE-LOCK
                         WHERE dynLookup.prgmName  EQ ttDynLookup.prgmName
                           AND dynLookup.tableDB   EQ ttDynLookup.tableDB
                           AND dynLookup.tableName EQ ttDynLookup.tableName
                           AND dynLookup.fieldName EQ ttDynLookup.fieldName
                         NO-ERROR.
                    IF NOT AVAILABLE dynLookup THEN
                    CREATE dynLookup.
                    BUFFER-COPY ttDynLookup TO dynLookup.
                    RELEASE dynLookup.
                END. /* dynLookup */
                WHEN "dynParam" THEN DO:
                    IMPORT ttDynParam.
                    FIND FIRST dynParam EXCLUSIVE-LOCK
                         WHERE dynParam.paramID EQ ttDynParam.paramID
                         NO-ERROR.
                    IF NOT AVAILABLE dynParam THEN
                    CREATE dynParam.
                    BUFFER-COPY ttDynParam TO dynParam.
                    RELEASE dynParam.
                END. /* dynParam */
                WHEN "dynParamSet" THEN DO:
                    IMPORT ttDynParamSet.
                    FIND FIRST dynParamSet EXCLUSIVE-LOCK
                         WHERE dynParamSet.paramSetID EQ ttDynParamSet.paramSetID
                         NO-ERROR.
                    IF NOT AVAILABLE dynParamSet THEN
                    CREATE dynParamSet.
                    BUFFER-COPY ttDynParamSet TO dynParamSet.
                    FOR EACH dynParamSetDtl EXCLUSIVE-LOCK
                        WHERE dynParamSetDtl.paramSetID EQ dynParamSet.paramSetID
                        :
                        DELETE dynParamSetDtl.
                    END. /* each dynparamsetdtl */
                    RELEASE dynParamSet.
                END. /* dynParamSet */
                WHEN "dynParamSetDtl" THEN DO:
                    IMPORT ttDynParamSetDtl.
                    IF NOT CAN-FIND(FIRST dynParamSet
                                    WHERE dynParamSet.paramSetID EQ ttDynParamSetDtl.paramSetID) OR
                       NOT CAN-FIND(FIRST dynParam
                                    WHERE dynParam.paramID EQ ttDynParamSetDtl.paramID) THEN
                    NEXT.
                    CREATE dynParamSetDtl.
                    BUFFER-COPY ttDynParamSetDtl TO dynParamSetDtl.
                    RELEASE dynParamSetDtl.
                END. /* dynParamSetDtl */
                WHEN "dynParamValue" THEN DO:
                    IMPORT ttDynParamValue.
                    IF NOT CAN-FIND(FIRST dynSubject
                                    WHERE dynSubject.subjectID EQ ttDynParamValue.subjectID) THEN
                    NEXT.
                    FIND FIRST dynParamValue EXCLUSIVE-LOCK
                         WHERE dynParamValue.subjectID    EQ ttDynParamValue.subjectID
                           AND dynParamValue.user-id      EQ ttDynParamValue.user-id
                           AND dynParamValue.prgmName     EQ ttDynParamValue.prgmName
                           AND dynParamValue.paramValueID EQ ttDynParamValue.paramValueID
                         NO-ERROR.
                    IF NOT AVAILABLE dynParamValue THEN
                    CREATE dynParamValue.
                    BUFFER-COPY ttDynParamValue TO dynParamValue.
                    RELEASE dynParamValue.
                END. /* dynParamValue */
                WHEN "dynSubject" THEN DO:
                    IMPORT ttDynSubject.
                    FIND FIRST dynSubject EXCLUSIVE-LOCK
                         WHERE dynSubject.subjectID EQ ttDynSubject.subjectID
                         NO-ERROR.
                    IF NOT AVAILABLE dynSubject THEN
                    CREATE dynSubject.
                    BUFFER-COPY ttDynSubject TO dynSubject.
                    FOR EACH dynSubjectColumn EXCLUSIVE-LOCK
                        WHERE dynSubjectColumn.subjectID EQ dynSubject.subjectID
                        :
                        DELETE dynSubjectColumn.
                    END. /* each dynSubjectColumn */
                    FOR EACH dynSubjectParamSet EXCLUSIVE-LOCK
                        WHERE dynSubjectParamSet.subjectID EQ dynSubject.subjectID
                        :
                        DELETE dynSubjectParamSet.
                    END. /* each dynSubjectParamSet */
                    FOR EACH dynSubjectTable EXCLUSIVE-LOCK
                        WHERE dynSubjectTable.subjectID EQ dynSubject.subjectID
                        :
                        DELETE dynSubjectTable.
                    END. /* each dynSubjectTable */
                    FOR EACH dynSubjectWhere EXCLUSIVE-LOCK
                        WHERE dynSubjectWhere.subjectID EQ dynSubject.subjectID
                        :
                        DELETE dynSubjectWhere.
                    END. /* each dynSubjectWhere */
                    RELEASE dynSubject.
                END. /* dynSubject */
                WHEN "dynSubjectColumn" THEN DO:
                    IMPORT ttDynSubjectColumn.
                    IF NOT CAN-FIND(FIRST dynSubject
                                    WHERE dynSubject.subjectID EQ ttDynSubjectColumn.subjectID) THEN
                    NEXT.
                    IF CAN-FIND(FIRST dynSubjectColumn
                                WHERE dynSubjectColumn.subjectID EQ ttDynSubjectColumn.subjectID
                                  AND dynSubjectColumn.fieldName EQ ttDynSubjectColumn.fieldName) THEN
                    NEXT.
                    CREATE dynSubjectColumn.
                    BUFFER-COPY ttDynSubjectColumn TO dynSubjectColumn.
                    RELEASE dynSubjectColumn.
                END. /* dynSubjectColumn */
                WHEN "dynSubjectParamSet" THEN DO:
                    IMPORT ttDynSubjectParamSet.
                    IF NOT CAN-FIND(FIRST dynSubject
                                    WHERE dynSubject.subjectID EQ ttDynSubjectParamSet.subjectID) THEN
                    NEXT.
                    IF CAN-FIND(FIRST dynSubjectParamSet
                                WHERE dynSubjectParamSet.subjectID  EQ ttDynSubjectParamSet.subjectID
                                  AND dynSubjectParamSet.paramSetID EQ ttDynSubjectParamSet.paramSetID) THEN
                    NEXT.
                    CREATE dynSubjectParamSet.
                    BUFFER-COPY ttDynSubjectParamSet TO dynSubjectParamSet.
                    RELEASE dynSubjectParamSet.
                END. /* dynSubjectParamSet */
                WHEN "dynSubjectTable" THEN DO:
                    IMPORT ttDynSubjectTable.
                    IF NOT CAN-FIND(FIRST dynSubject
                                    WHERE dynSubject.subjectID EQ ttDynSubjectTable.subjectID) THEN
                    NEXT.
                    IF CAN-FIND(FIRST dynSubjectTable
                                WHERE dynSubjectTable.subjectID EQ ttDynSubjectTable.subjectID
                                  AND dynSubjectTable.tableName EQ ttDynSubjectTable.tableName) THEN
                    NEXT.
                    CREATE dynSubjectTable.
                    BUFFER-COPY ttDynSubjectTable TO dynSubjectTable.
                    RELEASE dynSubjectTable.
                END. /* dynSubjectTable */
                WHEN "dynSubjectWhere" THEN DO:
                    IMPORT ttDynSubjectWhere.
                    IF NOT CAN-FIND(FIRST dynSubject
                                    WHERE dynSubject.subjectID EQ ttDynSubjectWhere.subjectID) THEN
                    NEXT.
                    IF CAN-FIND(FIRST dynSubjectWhere
                                WHERE dynSubjectWhere.subjectID EQ ttDynSubjectWhere.subjectID
                                  AND dynSubjectWhere.tableName EQ ttDynSubjectWhere.tableName
                                  AND dynSubjectWhere.sortOrder EQ ttDynSubjectWhere.sortOrder) THEN
                    NEXT.
                    CREATE dynSubjectWhere.
                    BUFFER-COPY ttDynSubjectWhere TO dynSubjectWhere.
                    RELEASE dynSubjectWhere.
                END. /* dynSubjectWhere */
            END CASE.
        END. /* repeat */
        INPUT CLOSE.
    END. /* do idx */
    RUN pInit.
    SESSION:SET-WAIT-STATE("").
    MESSAGE
        "Dynamic Table Import Complete"
    VIEW-AS ALERT-BOX.

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
    RUN pGetDynSubject.
    RUN pGetDynParamSet.
    cColumnLabel = "setName".
    RUN pReopenBrowse.
    cColumnLabel = "subjectTitle".
    RUN pReopenBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse C-Win 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    IF cColumnLabel BEGINS "subject" THEN
    CASE cColumnLabel:
        WHEN "subjectID" THEN
        RUN pBySubjectID.
        WHEN "subjectTitle" THEN
        RUN pBySubjectTitle.
        WHEN "subjectType" THEN
        RUN pBySubjectType.
        OTHERWISE
        {&OPEN-QUERY-subjectBrowse}
    END CASE.
    ELSE
    CASE cColumnLabel:
        WHEN "paramSetID" THEN
        RUN pByParamSetID.
        WHEN "setName" THEN
        RUN pBySetName.
        WHEN "paramSetType" THEN
        RUN pByParamSetType.
        OTHERWISE
        {&OPEN-QUERY-paramSetBrowse}
    END CASE.
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

