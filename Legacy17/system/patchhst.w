&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: patchhst.w

  Description: patch history browser

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 4.26.2005

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

&SCOPED-DEFINE patchDatFile patch.dat

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cellColumn AS HANDLE NO-UNDO EXTENT 13.
DEFINE VARIABLE patchTime AS CHARACTER NO-UNDO.

DEFINE BUFFER bPatchhst FOR patchhst.

DEFINE TEMP-TABLE tPatchDat NO-UNDO
  FIELD id LIKE patchhst.id
  FIELD patch LIKE patchhst.patch
  FIELD version LIKE patchhst.version
  FIELD seq LIKE patchhst.seq
  FIELD patch-date LIKE patchhst.patch-date
  FIELD run-order LIKE patchhst.run-order
  FIELD utility LIKE patchhst.utility
  FIELD run-once LIKE patchhst.run-once
  FIELD descr LIKE patchhst.descr
  FIELD dependancy LIKE patchhst.dependancy.

SESSION:SET-WAIT-STATE('').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES patchhst

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 patchhst.patch patchhst.version ~
patchhst.seq patchhst.patch-date patchTime(patchhst.patch-time) @ patchTime ~
patchhst.completed patchhst.run-order patchhst.utility patchhst.descr ~
patchhst.run-once patchhst.user_id patchhst.dependancy patchhst.returnvalue 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH patchhst WHERE ~{&KEY-PHRASE} ~
      AND (patchhst.patch EQ INTEGER(patchFilter) ~
OR patchFilter EQ 'ALL') ~
AND (patchhst.version EQ versionFilter ~
OR versionFilter EQ 'ALL') NO-LOCK ~
    BY patchhst.patch ~
       BY patchhst.version ~
        BY patchhst.seq ~
         BY patchhst.run-order INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH patchhst WHERE ~{&KEY-PHRASE} ~
      AND (patchhst.patch EQ INTEGER(patchFilter) ~
OR patchFilter EQ 'ALL') ~
AND (patchhst.version EQ versionFilter ~
OR versionFilter EQ 'ALL') NO-LOCK ~
    BY patchhst.patch ~
       BY patchhst.version ~
        BY patchhst.seq ~
         BY patchhst.run-order INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 patchhst
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 patchhst


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS patchFilter versionFilter BROWSE-1 
&Scoped-Define DISPLAYED-OBJECTS patchFilter versionFilter lvPatch ~
lvVersion lvSeq 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD patchTime C-Win 
FUNCTION patchTime RETURNS CHARACTER
  (ipTime AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE patchFilter AS CHARACTER FORMAT "X(256)":U INITIAL "ALL" 
     LABEL "Patch" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "ALL" 
     DROP-DOWN-LIST
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE versionFilter AS CHARACTER FORMAT "X(256)":U INITIAL "ALL" 
     LABEL "Version" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "ALL" 
     DROP-DOWN-LIST
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lvPatch AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Patch#" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lvSeq AS CHARACTER FORMAT "X(8)" 
     LABEL "Seq" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE lvVersion AS CHARACTER FORMAT "X(8)" 
     LABEL "Version" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1
     BGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      patchhst SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      patchhst.patch FORMAT ">>>>>>9":U
      patchhst.version FORMAT "X(8)":U WIDTH 10
      patchhst.seq COLUMN-LABEL "Sequence" FORMAT "X(8)":U
      patchhst.patch-date COLUMN-LABEL "Date" FORMAT "99/99/9999":U
      patchTime(patchhst.patch-time) @ patchTime COLUMN-LABEL "Time" FORMAT "X(12)":U
      patchhst.completed FORMAT "yes/no":U WIDTH 11
      patchhst.run-order FORMAT ">>>>9":U
      patchhst.utility FORMAT "X(50)":U
      patchhst.descr FORMAT "X(50)":U
      patchhst.run-once FORMAT "yes/no":U
      patchhst.user_id FORMAT "X(8)":U WIDTH 11.2
      patchhst.dependancy FORMAT "X(30)":U
      patchhst.returnvalue FORMAT "X(50)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 150 BY 22.81 ROW-HEIGHT-CHARS .62 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     patchFilter AT ROW 1 COL 8 COLON-ALIGNED HELP
          "Select Patch Value to Filter"
     versionFilter AT ROW 1 COL 29 COLON-ALIGNED HELP
          "Select Version Value to Filter"
     lvPatch AT ROW 1 COL 95 COLON-ALIGNED
     lvVersion AT ROW 1 COL 116 COLON-ALIGNED
     lvSeq AT ROW 1 COL 135 COLON-ALIGNED
     BROWSE-1 AT ROW 2.19 COL 1
     "Current Patch/Version:" VIEW-AS TEXT
          SIZE 26 BY 1 AT ROW 1 COL 62
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150 BY 24.


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
         TITLE              = "Patch Download and Run History"
         HEIGHT             = 24
         WIDTH              = 150
         MAX-HEIGHT         = 24
         MAX-WIDTH          = 150
         VIRTUAL-HEIGHT     = 24
         VIRTUAL-WIDTH      = 150
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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-1 lvSeq DEFAULT-FRAME */
ASSIGN 
       BROWSE-1:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 5.

/* SETTINGS FOR FILL-IN lvPatch IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lvSeq IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lvVersion IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "asi.patchhst"
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE"
     _OrdList          = "asi.patchhst.patch|yes,asi.patchhst.version|yes,asi.patchhst.seq|yes,asi.patchhst.run-order|yes"
     _Where[1]         = "(patchhst.patch EQ INTEGER(patchFilter)
OR patchFilter EQ 'ALL')
AND (patchhst.version EQ versionFilter
OR versionFilter EQ 'ALL')"
     _FldNameList[1]   = asi.patchhst.patch
     _FldNameList[2]   > asi.patchhst.version
"version" ? ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[3]   > asi.patchhst.seq
"seq" "Sequence" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > asi.patchhst.patch-date
"patch-date" "Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > "_<CALC>"
"patchTime(patchhst.patch-time) @ patchTime" "Time" "X(12)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > asi.patchhst.completed
"completed" ? ? "logical" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" ""
     _FldNameList[7]   = asi.patchhst.run-order
     _FldNameList[8]   = asi.patchhst.utility
     _FldNameList[9]   = asi.patchhst.descr
     _FldNameList[10]   = asi.patchhst.run-once
     _FldNameList[11]   > asi.patchhst.user_id
"user_id" ? ? "character" ? ? ? ? ? ? no ? no no "11.2" yes no no "U" "" ""
     _FldNameList[12]   = asi.patchhst.dependancy
     _FldNameList[13]   = asi.patchhst.returnvalue
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Patch Download and Run History */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Patch Download and Run History */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON ROW-DISPLAY OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN
    cellColumn[6]:FGCOLOR = IF patchhst.completed THEN ? ELSE 15
    cellColumn[6]:BGCOLOR = IF patchhst.completed THEN 10 ELSE 12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME patchFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL patchFilter C-Win
ON VALUE-CHANGED OF patchFilter IN FRAME DEFAULT-FRAME /* Patch */
DO:
  ASSIGN {&SELF-NAME}.
  RUN getVersions.
  APPLY 'VALUE-CHANGED' TO versionFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME versionFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL versionFilter C-Win
ON VALUE-CHANGED OF versionFilter IN FRAME DEFAULT-FRAME /* Version */
DO:
  ASSIGN {&SELF-NAME}.
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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN getCellColumns.
  RUN loadPatchData.
  RUN enable_UI.
  RUN getPatches.
  APPLY 'VALUE-CHANGED' TO patchFilter.
  APPLY 'ENTRY' TO BROWSE {&BROWSE-NAME}.
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  DISPLAY patchFilter versionFilter lvPatch lvVersion lvSeq 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE patchFilter versionFilter BROWSE-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCellColumns C-Win 
PROCEDURE getCellColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
    cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPatches C-Win 
PROCEDURE getPatches :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

  patchFilter:LIST-ITEMS IN FRAME {&FRAME-NAME} = 'ALL'.
  FOR EACH bPatchhst NO-LOCK BREAK BY bPatchhst.patch:
    IF FIRST-OF(bPatchhst.patch) THEN
    ldummy = patchFilter:ADD-LAST(STRING(bPatchhst.patch,'zzzz9')).
  END.
  patchFilter:SCREEN-VALUE = patchFilter:ENTRY(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getVersions C-Win 
PROCEDURE getVersions :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

  versionFilter:LIST-ITEMS IN FRAME {&FRAME-NAME} = 'ALL'.
  IF patchFilter NE 'ALL' THEN
  FOR EACH bPatchhst NO-LOCK WHERE bPatchhst.patch EQ INTEGER(patchFilter)
      BREAK BY bPatchhst.version:
    IF FIRST-OF(bPatchhst.version) THEN
    ldummy = versionFilter:ADD-LAST(bPatchhst.version).
  END.
  versionFilter:SCREEN-VALUE = versionFilter:ENTRY(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadPatchData C-Win 
PROCEDURE loadPatchData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST clientid NO-LOCK NO-ERROR.
  IF NOT AVAILABLE clientid THEN CREATE clientid.

  IF SEARCH('{&patchDatFile}') NE ? THEN DO:
    CREATE tPatchDat.
    INPUT FROM VALUE(SEARCH('{&patchDatFile}')) NO-ECHO.
    FIND CURRENT clientid EXCLUSIVE-LOCK.
    IMPORT clientid.
    REPEAT:
      IMPORT tPatchDat.
      FIND patchhst EXCLUSIVE-LOCK WHERE patchhst.id EQ tPatchDat.id NO-ERROR.
      IF NOT AVAILABLE patchhst THEN DO:
        CREATE patchhst.
        ASSIGN
          patchhst.patch = tPatchDat.patch
          patchhst.version = tPatchDat.version
          patchhst.seq = tPatchDat.seq
          patchhst.id = tPatchDat.id.
      END.
      ASSIGN
        patchhst.patch-date = tPatchDat.patch-date
        patchhst.patch-time = IF tPatchDat.patch-date NE ? THEN TIME ELSE 0
        patchhst.run-order = tPatchDat.run-order
        patchhst.utility = tPatchDat.utility
        patchhst.completed = tPatchDat.patch-date NE ?
        patchhst.run-once = tPatchDat.run-once
        patchhst.descr = tPatchDat.descr
        patchhst.dependancy = tPatchDat.dependancy
        patchhst.user_id = USERID('NoSweat').
    END. /* repeat */
    INPUT CLOSE.
  END.
  FIND CURRENT clientid NO-LOCK.
  ASSIGN
    lvPatch = clientid.patch
    lvVersion = clientid.version
    lvSeq = clientid.seq.
  OS-DELETE VALUE(SEARCH('{&patchDatFile}')).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION patchTime C-Win 
FUNCTION patchTime RETURNS CHARACTER
  (ipTime AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN IF ipTime EQ 0 THEN ''
         ELSE STRING(ipTime,'HH:MM:SS am').

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

