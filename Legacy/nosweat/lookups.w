&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:              lookups.w

  Description:       Lookup Browser User Interface

  Input Parameters:  Lookup Browser Program Name

  Output Parameters: Saved Indicator

  Author:            Ron Stark

  Created:           12/01/96

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) = 0 &THEN
DEFINE INPUT PARAMETER mLookupPrgm AS CHARACTER.
&ELSE
DEFINE VARIABLE mLookupPrgm AS CHARACTER INITIAL "prgrms.".
&ENDIF

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE VARIABLE sHandle           AS HANDLE    NO-UNDO.
DEFINE VARIABLE cShowFields       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShowFieldsYellow AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrderFields      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDummy            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE i                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE cDataType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDataFormat       AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnAdd btnDown btnRemove btnUP btnGetField ~
RECT-3 RECT-1 RECT-2 svWhereStatement svOrderValues svAutoSet svSelectedSet ~
svShowFields svOrderFields svAvailableSelections svFrameTitle svTopInclude ~
svDefInclude svEndInclude svUIPrgmname svFont svHeightSize svWidthSize ~
btnReset btnSave btnDelete btnCancel btnOK btnDescription 
&Scoped-Define DISPLAYED-OBJECTS svLookupPrgm svLookupDB svLookupFile ~
svReturnField svWhereStatement svOrderValues svAutoSet svSelectedSet ~
svShowFields svOrderFields svAvailableSelections svFrameTitle svTopInclude ~
svDefInclude svEndInclude svUIPrgmname svStatus svFont svHeightSize ~
svWidthSize F1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 F1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "Graphics/16x16/pvback.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Add" 
     SIZE 6.6 BY 1.33
     FONT 4.

DEFINE BUTTON btnCancel AUTO-END-KEY DEFAULT 
     LABEL "&Cancel" 
     SIZE 14 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE BUTTON btnDelete 
     LABEL "&Delete" 
     SIZE 14 BY 1.24
     FONT 4.

DEFINE BUTTON btnDescription 
     LABEL "Descriptio&n" 
     SIZE 14 BY 1.24
     FONT 4.

DEFINE BUTTON btnDown 
     IMAGE-UP FILE "Graphics/16x16/down.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Move Do&wn" 
     SIZE 6.6 BY 1.33
     FONT 4.

DEFINE BUTTON btnGetField 
     IMAGE-UP FILE "adeicon/fields-u":U
     LABEL "Get Field" 
     SIZE 8.2 BY 2.

DEFINE BUTTON btnOK AUTO-GO 
     LABEL "&OK" 
     SIZE 14 BY 1.24
     FONT 4.

DEFINE BUTTON btnRemove 
     IMAGE-UP FILE "Graphics/16x16/pvforw.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Re&move" 
     SIZE 7.2 BY 1.33
     FONT 4.

DEFINE BUTTON btnReset 
     LABEL "&Reset" 
     SIZE 14 BY 1.24
     FONT 4.

DEFINE BUTTON btnSave 
     LABEL "&Save" 
     SIZE 14 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE BUTTON btnUP 
     IMAGE-UP FILE "Graphics/16x16/up.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Move &Up" 
     SIZE 6.6 BY 1.33
     FONT 4.

DEFINE VARIABLE svFont AS INTEGER FORMAT "9":U INITIAL 4 
     LABEL "Font" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "0","1","2","3","4","5","6","7" 
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE svWhereStatement AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 128.8 BY 4.67 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE svDefInclude AS CHARACTER FORMAT "X(256)":U 
     LABEL "Def. Include" 
     VIEW-AS FILL-IN 
     SIZE 95.2 BY 1 NO-UNDO.

DEFINE VARIABLE svEndInclude AS CHARACTER FORMAT "X(256)":U 
     LABEL "End Include" 
     VIEW-AS FILL-IN 
     SIZE 95.2 BY 1 NO-UNDO.

DEFINE VARIABLE svFrameTitle AS CHARACTER FORMAT "X(256)":U 
     LABEL "Frame Title" 
     VIEW-AS FILL-IN 
     SIZE 95.2 BY 1 NO-UNDO.

DEFINE VARIABLE svHeightSize AS CHARACTER FORMAT "X(02)":U INITIAL "19" 
     LABEL "Height" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE svLookupDB AS CHARACTER FORMAT "X(256)":U 
     LABEL "DB" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE svLookupFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "Table" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE svLookupPrgm AS CHARACTER FORMAT "X(12)":U 
     LABEL "Program" 
     VIEW-AS FILL-IN 
     SIZE 18.2 BY 1 NO-UNDO.

DEFINE VARIABLE svOrderValues AS CHARACTER FORMAT "X(256)":U 
     LABEL "Order Values" 
     VIEW-AS FILL-IN 
     SIZE 102.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE svReturnField AS CHARACTER FORMAT "X(30)":U 
     LABEL "Return Field" 
     VIEW-AS FILL-IN 
     SIZE 26.6 BY 1 NO-UNDO.

DEFINE VARIABLE svStatus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE svTopInclude AS CHARACTER FORMAT "X(256)":U 
     LABEL "Top Include" 
     VIEW-AS FILL-IN 
     SIZE 95.2 BY 1 NO-UNDO.

DEFINE VARIABLE svUIPrgmname AS CHARACTER FORMAT "X(12)":U 
     LABEL "UI Program" 
     VIEW-AS FILL-IN 
     SIZE 12.6 BY 1 NO-UNDO.

DEFINE VARIABLE svWidthSize AS CHARACTER FORMAT "X(03)":U INITIAL "46" 
     LABEL "Width" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE svSelectedSet AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Show Database Fields", 1,
"Browser Order Fields", 2
     SIZE 64.8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.6 BY 2.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32.2 BY 2.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY 2.

DEFINE VARIABLE svAvailableSelections AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT SCROLLBAR-VERTICAL 
     SIZE 32.2 BY 14.95 NO-UNDO.

DEFINE VARIABLE svOrderFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 32.2 BY 5.67 NO-UNDO.

DEFINE VARIABLE svShowFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 32.2 BY 5.67 NO-UNDO.

DEFINE VARIABLE svAutoSet AS LOGICAL INITIAL yes 
     LABEL "Auto Set Order Values" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnAdd AT ROW 13.62 COL 80 HELP
          "Add Selected Item"
     btnDown AT ROW 11.95 COL 80 HELP
          "Move Selected Item Down"
     btnRemove AT ROW 15.05 COL 80 HELP
          "Remove Selected Item"
     btnUP AT ROW 10.52 COL 80 HELP
          "Move Selected Item Up"
     btnGetField AT ROW 1 COL 34.2 HELP
          "Get Database, Table, & Return Field"
     svLookupPrgm AT ROW 1.38 COL 12.6 COLON-ALIGNED
     svLookupDB AT ROW 1.38 COL 47.6 COLON-ALIGNED
     svLookupFile AT ROW 1.38 COL 77 COLON-ALIGNED
     svReturnField AT ROW 1.38 COL 114.8 COLON-ALIGNED HELP
          "Enter Field Name for Lookup Browser to Return to Calling Frame"
     svWhereStatement AT ROW 3 COL 14.6 HELP
          "Enter Lookup Table's Key Phrase" NO-LABEL
     svOrderValues AT ROW 7.91 COL 12.6 COLON-ALIGNED HELP
          "Enter List of Browse Order Values (Comma Delimited)"
     svAutoSet AT ROW 7.91 COL 118.4 HELP
          "Auto Set Order Values Indicator"
     svSelectedSet AT ROW 9.24 COL 14.6 HELP
          "Make Selection List Active" NO-LABEL
     svShowFields AT ROW 10.57 COL 14.6 HELP
          "Select Show Field" NO-LABEL
     svOrderFields AT ROW 10.57 COL 46.8 HELP
          "Select Browser Order Field" NO-LABEL
     svAvailableSelections AT ROW 10.57 COL 111.2 HELP
          "Select Item to Add to Select List" NO-LABEL
     svFrameTitle AT ROW 16.57 COL 12.6 COLON-ALIGNED HELP
          "Enter Lookup Browser's Title (will also appear in prgrms)"
     svTopInclude AT ROW 18.24 COL 12.6 COLON-ALIGNED HELP
          "Enter Include File Name to Execute at Beginning of Lookup"
     svDefInclude AT ROW 19.48 COL 12.6 COLON-ALIGNED HELP
          "Enter Definition Include File Name to Declare Local Variables"
     svEndInclude AT ROW 20.71 COL 12.6 COLON-ALIGNED HELP
          "Enter Include File Name to Execute at End of Lookup"
     svUIPrgmname AT ROW 22.19 COL 12.4 COLON-ALIGNED HELP
          "Enter UI Program to Run from within Lookup Browser"
     svStatus AT ROW 22.19 COL 30 NO-LABEL
     svFont AT ROW 22.19 COL 71 COLON-ALIGNED HELP
          "Select Font"
     svHeightSize AT ROW 22.19 COL 86.4 COLON-ALIGNED HELP
          "Enter Height of Lookup Browser"
     svWidthSize AT ROW 22.19 COL 99.4 COLON-ALIGNED HELP
          "Enter Width of Lookup Browser"
     btnReset AT ROW 24.05 COL 31.8 HELP
          "Use this function to RESET this Lookup Browser"
     btnSave AT ROW 24.05 COL 47.2 HELP
          "Use this function to SAVE this Lookup Browser"
     btnDelete AT ROW 24.05 COL 62.6 HELP
          "Use this function to DELETE this Lookup Browser"
     btnCancel AT ROW 24.05 COL 79.4 HELP
          "Use this function to CANCEL Update/Create Lookup Browser"
     btnOK AT ROW 24.05 COL 94.8
     btnDescription AT ROW 24.1 COL 15.4 HELP
          "Use this function to CREATE DESCRIPTION Lookup Browser"
     F1 AT ROW 22.19 COL 27 NO-LABEL
     "Available Selections" VIEW-AS TEXT
          SIZE 19.4 BY 1 AT ROW 9.33 COL 118
     "Key Phrase:" VIEW-AS TEXT
          SIZE 11.6 BY 1 AT ROW 2.91 COL 3
     RECT-3 AT ROW 23.62 COL 14.4
     RECT-1 AT ROW 23.62 COL 30.4
     RECT-2 AT ROW 23.62 COL 78
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 143 BY 24.76.


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
         TITLE              = "Lookups Create/Update Interface"
         HEIGHT             = 24.76
         WIDTH              = 143
         MAX-HEIGHT         = 24.76
         MAX-WIDTH          = 143
         VIRTUAL-HEIGHT     = 24.76
         VIRTUAL-WIDTH      = 143
         RESIZE             = yes
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
/* SETTINGS FOR FILL-IN F1 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN svLookupDB IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN svLookupFile IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN svLookupPrgm IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN svReturnField IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN svStatus IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Lookups Create/Update Interface */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Lookups Create/Update Interface */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME DEFAULT-FRAME /* Add */
DO:
  DO i = 1 TO svAvailableSelections:NUM-ITEMS:
    IF svAvailableSelections:IS-SELECTED(i) THEN
      IF sHandle:LOOKUP(svAvailableSelections:ENTRY(i)) = 0 THEN
      lDummy = sHandle:ADD-LAST(svAvailableSelections:ENTRY(i)).
  END.
  DO i = 1 TO svAvailableSelections:NUM-ITEMS:
    IF svAvailableSelections:IS-SELECTED(i) THEN
    svAvailableSelections:SCREEN-VALUE = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* Delete */
DO:
  svStatus:SCREEN-VALUE = "DELETE Lookup?".
  MESSAGE "Delete This Lookup Browser?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          UPDATE m_response AS LOGICAL.
  CASE m_response:
    WHEN YES THEN DO:
      FIND prgrms WHERE prgrms.prgmname = mLookupPrgm NO-ERROR.
      IF AVAILABLE prgrms THEN
      DELETE prgrms.
      IF SEARCH("lookups/" + mLookupPrgm + "p") NE ? THEN
      OS-DELETE VALUE("lookups/" + mLookupPrgm + "p").
      IF SEARCH("lookups/" + mLookupPrgm + "r") NE ? THEN
      OS-DELETE VALUE("lookups/" + mLookupPrgm + "r").
      APPLY "GO" TO FRAME {&FRAME-NAME}.
    END.
    WHEN NO THEN
    svStatus:SCREEN-VALUE = "".
    OTHERWISE
    svStatus:SCREEN-VALUE = "DELETE Cancelled".
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDescription
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDescription C-Win
ON CHOOSE OF btnDescription IN FRAME DEFAULT-FRAME /* Description */
DO:
  DEFINE VARIABLE cDescripLookup AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDummy         AS CHARACTER NO-UNDO.

  cDescripLookup = TRIM(REPLACE(mLookupPrgm,".","_.")).
  IF LENGTH(cDescripLookup) GE 10 THEN
  SUBSTR(cDescripLookup,8,1) = "".
  IF SEARCH("lookups/" + cDescripLookup + "p") NE ? THEN DO:
    MESSAGE "Lookup" cDescripLookup "already Exists, Open it?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lDummy.
    IF NOT lDummy THEN
    RETURN NO-APPLY.
    mLookupPrgm = cDescripLookup.
    APPLY "CHOOSE" TO btnReset.
    RETURN NO-APPLY.
  END.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      mLookupPrgm = cDescripLookup
      svLookupPrgm:SCREEN-VALUE = cDescripLookup
      svFrameTitle:SCREEN-VALUE =
          REPLACE(svFrameTitle:SCREEN-VALUE,"Lookup","Description Lookup")
      svReturnField:SCREEN-VALUE = svOrderFields:ENTRY(2)
      cDummy = svShowFields:ENTRY(2)
      lDummy = svShowFields:DELETE(2)
      lDummy = svShowFields:ADD-FIRST(cDummy)
      cDummy = svOrderFields:ENTRY(2)
      lDummy = svOrderFields:DELETE(2)
      lDummy = svOrderFields:ADD-FIRST(cDummy)
      .
    /*RUN Enable_UI.*/
    APPLY "VALUE-CHANGED" TO svSelectedSet.
    RUN pAutoSetOrderValues.
    ASSIGN
      svOrderValues:SENSITIVE = NOT svAutoSet
      svStatus:SCREEN-VALUE = "DESCRIPTION Lookup Created"
      .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDown C-Win
ON CHOOSE OF btnDown IN FRAME DEFAULT-FRAME /* Move Down */
DO:
  IF sHandle:SCREEN-VALUE NE "" THEN
  DO i = 1 TO sHandle:NUM-ITEMS - 1:
    IF NOT sHandle:IS-SELECTED(i) THEN NEXT.
    ASSIGN
      lDummy = sHandle:INSERT(sHandle:SCREEN-VALUE,i + 2)
      lDummy = sHandle:DELETE(i)
      sHandle:SCREEN-VALUE = sHandle:ENTRY(i + 1)
      .
    LEAVE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGetField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGetField C-Win
ON CHOOSE OF btnGetField IN FRAME DEFAULT-FRAME /* Get Field */
DO:
  DEFINE VARIABLE cGetField AS CHARACTER NO-UNDO.

  IF svLookupDB EQ "" THEN
  svLookupDB = "ASI".
  CREATE ALIAS dictdb FOR DATABASE VALUE(svLookupDB).
  cGetField = svLookupDB:SCREEN-VALUE + "."
            + svLookupFile:SCREEN-VALUE + "."
            + svReturnField:SCREEN-VALUE
            .
  RUN Get_Procedure IN Persistent-Handle ("getfield.",OUTPUT run-proc,no).
  IF run-proc NE "" THEN
  RUN VALUE(run-proc) (INPUT-OUTPUT cGetField).
  ASSIGN
    svLookupDB:SCREEN-VALUE    = SUBSTR(cGetField,1,INDEX(cGetField,".") - 1)
    svLookupFile:SCREEN-VALUE  = SUBSTR(cGetField,INDEX(cGetField,".") + 1,
                                 R-INDEX(cGetField,".") - INDEX(cGetField,".") - 1)
    svReturnField:SCREEN-VALUE = SUBSTR(cGetField,R-INDEX(cGetField,".") + 1)
    .
  FIND FIRST prgmxref NO-LOCK
       WHERE prgmxref.table_name EQ svLookupFile:SCREEN-VALUE
       NO-ERROR.
  IF AVAILABLE prgmxref THEN DO:
    svUIPrgmname:SCREEN-VALUE = prgmxref.prgmname.
    FIND prgrms OF prgmxref NO-LOCK NO-ERROR.
    IF AVAILABLE prgrms THEN
    svFrameTitle:SCREEN-VALUE = prgrms.prgtitle + " Lookup".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK C-Win
ON CHOOSE OF btnOK IN FRAME DEFAULT-FRAME /* OK */
DO:
  APPLY "CHOOSE" TO btnSave.
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove C-Win
ON CHOOSE OF btnRemove IN FRAME DEFAULT-FRAME /* Remove */
DO:
  IF sHandle:SCREEN-VALUE NE "" THEN
  APPLY "DEFAULT-ACTION" TO sHandle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset C-Win
ON CHOOSE OF btnReset IN FRAME DEFAULT-FRAME /* Reset */
DO:
  RUN pGetLookup.
  RUN Enable_UI.
  APPLY "VALUE-CHANGED" TO svSelectedSet.
  ASSIGN
    svOrderValues:SENSITIVE = NOT svAutoSet
    svStatus:SCREEN-VALUE = "Lookup Record RESET"
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
  DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.

  ASSIGN
    svLookupDB
    svLookupFile
    svWhereStatement
    svOrderValues
    svFont
    svHeightSize
    svWidthSize
    svFrameTitle
    svReturnField
    svUIPrgmname
    svDefInclude
    svTopInclude
    svEndInclude
    svWhereStatement = TRIM(svWhereStatement)
    svAutoSet.

  RUN pAutoSetOrderValues.

  IF INT(svHeightSize) GT 19 THEN
  ASSIGN
    svHeightSize:SCREEN-VALUE = "19"
    svHeightSize = "19".

  IF INT(svWidthSize) LT 32 THEN
  ASSIGN
    svWidthSize:SCREEN-VALUE = "32"
    svWidthSize = "32".

  IF INT(svWidthSize) GT 150 THEN
  ASSIGN
    svWidthSize:SCREEN-VALUE = "150"
    svWidthSize = "150".

  OUTPUT TO VALUE("lookups/" + mLookupPrgm + "p").

  IF INDEX(svWhereStatement,'"') NE 0 THEN
  ASSIGN svWhereStatement = REPLACE(svWhereStatement,'"','""').

  IF INDEX(svDefInclude,'"') NE 0 THEN
  ASSIGN svDefInclude = REPLACE(svDefInclude,'"','""').

  IF INDEX(svTopInclude,'"') NE 0 THEN
  ASSIGN svTopInclude = REPLACE(svTopInclude,'"','""').

  IF INDEX(svEndInclude,'"') NE 0 THEN
  ASSIGN svEndInclude = REPLACE(svEndInclude,'"','""').

  PUT UNFORMATTED "/* " mLookupPrgm "p - Generated " TODAY FORMAT "99/99/9999"
    " - " STRING(TIME,"HH:MM am") " by " USERID("NOSWEAT") SKIP
    "~"" mLookupPrgm " ~" ~~" SKIP
    "~"" svLookupDB " ~" ~~" SKIP
    "~"" svLookupFile " ~" ~~" SKIP         
    "~"" svWhereStatement " ~" ~~" SKIP
    "~"" svReturnField " ~" ~~" SKIP
    "~"" svFont " ~" ~~" SKIP
    "~"" svHeightSize " ~" ~~" SKIP
    "~"" svWidthSize " ~" ~~" SKIP
    "~"" svShowFields:LIST-ITEMS " ~" ~~" SKIP
    "~"" svOrderValues " ~" ~~" SKIP
    "~"" svAutoSet " ~" ~~" SKIP
    "~"" svOrderFields:LIST-ITEMS " ~" ~~" SKIP
    "~"" svFrameTitle " ~" ~~" SKIP
    "~"" svTopInclude " ~" ~~" SKIP
    "~"" svDefInclude " ~" ~~" SKIP
    "~"" svEndInclude " ~" ~~" SKIP
    "~"" svUIPrgmname " ~" ~~" SKIP
    "*/" SKIP(1).

  IF svLookupDB EQ "dictdb" OR svLookupDB = "" THEN
  PUT UNFORMATTED
    "DEFINE INPUT-OUTPUT PARAMETER m-lookup-var AS CHARACTER." SKIP(1).

  ASSIGN svLookupDB = IF svLookupDB NE "" THEN svLookupDB + "." ELSE " ".

  IF svShowFields:NUM-ITEMS NE 0 THEN
  ASSIGN cShowFields = "," + svShowFields:LIST-ITEMS
    cShowFields = REPLACE(cShowFields,","," " + svLookupFile + ".")
    cShowFields = REPLACE(cShowFields,"  "," ")
    cShowFields = LEFT-TRIM(cShowFields)
    cShowFieldsYellow = REPLACE(cShowFields," "," LABEL-BGCOLOR 14 ") +
                           " LABEL-BGCOLOR 14".

  PUT UNFORMATTED
    "~&Scoped-define lookup-db " svLookupDB SKIP
    "~&Scoped-define lookup-file " svLookupFile SKIP
    "~&Scoped-define where-statement ".

  IF svWhereStatement NE "" THEN
  PUT UNFORMATTED svWhereStatement.
  ELSE
  PUT UNFORMATTED "TRUE".

  PUT UNFORMATTED SKIP
    "~&Scoped-define return-field " svReturnField SKIP
    "~&Scoped-define font " svFont SKIP
    "~&Scoped-define height-size " svHeightSize SKIP
    "~&Scoped-define width-size " svWidthSize SKIP
    "~&Scoped-define show-fields " cShowFields SKIP
    "~&Scoped-define show-fields-yellow " cShowFieldsYellow SKIP
    "~&Scoped-define frame-title " svFrameTitle SKIP
    "~&Scoped-define top-include ".
  IF svTopInclude NE "" THEN
  PUT UNFORMATTED "~~" svTopInclude.
  PUT UNFORMATTED SKIP "~&Scoped-define def-include ".
  IF svDefInclude NE "" THEN
  PUT UNFORMATTED "~~" svDefInclude.
  PUT UNFORMATTED SKIP "~&Scoped-define end-include ".
  IF svEndInclude NE "" THEN
  PUT UNFORMATTED "~~" svEndInclude.
  PUT UNFORMATTED SKIP
    "~&Scoped-define ui-prgmname " svUIPrgmname SKIP
    "~&Scoped-define window-size " INTEGER(svHeightSize) + 5 SKIP
    "~&Scoped-define window-col " (150 - INTEGER(svWidthSize)) / 2 SKIP
    "~&Scoped-define rect-1-row " INTEGER(svHeightSize) + 1.15 SKIP
    "~&Scoped-define by-row " INTEGER(svHeightSize) + 1.42 SKIP
    "~&Scoped-define browse-order-width " INTEGER(svWidthSize) - 6 SKIP
    "~&Scoped-define browse-order-row " INTEGER(svHeightSize) + 1.42 SKIP
    "~&Scoped-define btn-row " INTEGER(svHeightSize) + 2.77 SKIP
    "~&Scoped-define btn-ok-col " INTEGER(svWidthSize) - 9 SKIP
    "~&Scoped-define btn-cancel-col " INTEGER(svWidthSize) - 20 SKIP
    "~&Scoped-define auto-find-row " INTEGER(svHeightSize) + 4.65 SKIP(1).

  DO i = 1 TO NUM-ENTRIES(svOrderValues):
    cDataType = "STRING".
    IF svLookupDB NE "" THEN
    DO:
      CREATE ALIAS dictdb FOR DATABASE VALUE(svLookupDB:SCREEN-VALUE).
      RUN Get_Procedure IN Persistent-Handle ("get_type.",OUTPUT run-proc,no).
      IF run-proc NE "" THEN
      RUN VALUE(run-proc) (svLookupFile,svOrderFields:ENTRY(i),OUTPUT cDataType).
      IF cDataType NE "STRING" THEN
      PUT UNFORMATTED "~&Global-define DATATYP" i " " CAPS(cDataType) SKIP.
      RUN Get_Procedure IN Persistent-Handle ("get_frmt.",OUTPUT run-proc,no).
      IF run-proc NE "" THEN
      RUN VALUE(run-proc) (svLookupFile,svOrderFields:ENTRY(i),OUTPUT cDataFormat).
      PUT UNFORMATTED "~&Global-define FORMAT-" i " " cDataFormat SKIP.
    END.
    PUT UNFORMATTED
      "~&Scoped-define FLDNAME" i " " svLookupFile "." svOrderFields:ENTRY(i) SKIP
      "~&Scoped-define SORTBY-" i " BY ~{&FLDNAME" i "}".
    IF i GT 1 THEN
    PUT UNFORMATTED " ~{&SORTBY-" i - 1 "}".
    PUT UNFORMATTED SKIP
      "~&Scoped-define DESCRIP" i " " ENTRY(i,svOrderValues) SKIP.
  END.

  PUT UNFORMATTED SKIP(1)
    "~{methods/lookup.i}" SKIP.

  OUTPUT CLOSE.
  
  MESSAGE "Compile Lookup?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE compile-lookup AS LOGICAL.
  IF compile-lookup THEN
  COMPILE VALUE("lookups\" + mLookupPrgm + "p") SAVE.

  ASSIGN
    btnCancel:LABEL = "&Close"
    svStatus:SCREEN-VALUE = "Lookup SAVEd"
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUP C-Win
ON CHOOSE OF btnUP IN FRAME DEFAULT-FRAME /* Move Up */
DO:
  IF sHandle:SCREEN-VALUE NE "" THEN
  DO i = 2 TO sHandle:NUM-ITEMS:
    IF NOT sHandle:IS-SELECTED(i) THEN NEXT.
    ASSIGN
      lDummy = sHandle:INSERT(sHandle:SCREEN-VALUE,i - 1)
      lDummy = sHandle:DELETE(i + 1)
      sHandle:SCREEN-VALUE = sHandle:ENTRY(i - 1)
      .
    LEAVE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAutoSet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAutoSet C-Win
ON VALUE-CHANGED OF svAutoSet IN FRAME DEFAULT-FRAME /* Auto Set Order Values */
DO:
  ASSIGN
    {&SELF-NAME}
    svOrderValues:SENSITIVE = NOT {&SELF-NAME}
    .
  RUN pAutoSetOrderValues.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAvailableSelections
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAvailableSelections C-Win
ON DEFAULT-ACTION OF svAvailableSelections IN FRAME DEFAULT-FRAME
DO:
  IF sHandle:LOOKUP(svAvailableSelections:SCREEN-VALUE) EQ 0 THEN
  lDummy = sHandle:ADD-LAST(svAvailableSelections:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svOrderFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svOrderFields C-Win
ON DEFAULT-ACTION OF svOrderFields IN FRAME DEFAULT-FRAME
DO:
  IF {&SELF-NAME}:SCREEN-VALUE NE "" THEN
  lDummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svOrderFields C-Win
ON ENTRY OF svOrderFields IN FRAME DEFAULT-FRAME
DO:
  svSelectedSet:SCREEN-VALUE = "2".
  APPLY "VALUE-CHANGED" TO svSelectedSet.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svSelectedSet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svSelectedSet C-Win
ON VALUE-CHANGED OF svSelectedSet IN FRAME DEFAULT-FRAME
DO:
  DEFINE VARIABLE item_list AS CHARACTER NO-UNDO.

  IF svLookupDB:SCREEN-VALUE = "" THEN
  RETURN NO-APPLY.
  CASE svSelectedSet:SCREEN-VALUE:
    WHEN "1" THEN DO:
      sHandle = svShowFields:HANDLE.
      CREATE ALIAS dictdb FOR DATABASE VALUE(svLookupDB:SCREEN-VALUE).
      RUN Get_Procedure IN Persistent-Handle (INPUT "fld_list.",OUTPUT run-proc,no).
      IF run-proc NE "" THEN
      RUN VALUE(run-proc) (svLookupFile:SCREEN-VALUE,OUTPUT item_list).
      svAvailableSelections:LIST-ITEMS = item_list.
    END.
    WHEN "2" THEN DO:
      sHandle = svOrderFields:HANDLE.
      CREATE ALIAS dictdb FOR DATABASE VALUE(svLookupDB:SCREEN-VALUE).
      RUN Get_Procedure IN Persistent-Handle (INPUT "fld_list.",OUTPUT run-proc,no).
      IF run-proc NE "" THEN
      RUN VALUE(run-proc) (svLookupFile:SCREEN-VALUE,OUTPUT item_list).
      svAvailableSelections:LIST-ITEMS = item_list.
      RUN pAutoSetOrderValues.
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowFields C-Win
ON DEFAULT-ACTION OF svShowFields IN FRAME DEFAULT-FRAME
DO:
  IF {&SELF-NAME}:SCREEN-VALUE NE "" THEN
  lDummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowFields C-Win
ON ENTRY OF svShowFields IN FRAME DEFAULT-FRAME
DO:
  svSelectedSet:SCREEN-VALUE = "1".
  APPLY "VALUE-CHANGED" TO svSelectedSet.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svUIPrgmname
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svUIPrgmname C-Win
ON HELP OF svUIPrgmname IN FRAME DEFAULT-FRAME /* UI Program */
DO:
  CREATE ALIAS dictdb FOR DATABASE NOSWEAT.
  RUN "lookups/ui_lkup.p".
  ASSIGN
    {&SELF-NAME}:SCREEN-VALUE = g_lookup-var
    {&SELF-NAME}.
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
  RUN pGetLookup.
  RUN enable_UI.
  svOrderValues:SENSITIVE = NOT svAutoSet.
  APPLY "VALUE-CHANGED" TO svSelectedSet.
  {methods/enhance.i}
  {methods/nowait.i}
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
  DISPLAY svLookupPrgm svLookupDB svLookupFile svReturnField svWhereStatement 
          svOrderValues svAutoSet svSelectedSet svShowFields svOrderFields 
          svAvailableSelections svFrameTitle svTopInclude svDefInclude 
          svEndInclude svUIPrgmname svStatus svFont svHeightSize svWidthSize F1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnAdd btnDown btnRemove btnUP btnGetField RECT-3 RECT-1 RECT-2 
         svWhereStatement svOrderValues svAutoSet svSelectedSet svShowFields 
         svOrderFields svAvailableSelections svFrameTitle svTopInclude 
         svDefInclude svEndInclude svUIPrgmname svFont svHeightSize svWidthSize 
         btnReset btnSave btnDelete btnCancel btnOK btnDescription 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAutoSetOrderValues C-Win 
PROCEDURE pAutoSetOrderValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE item_list AS CHARACTER NO-UNDO.

  IF NOT svAutoSet THEN RETURN.
  RUN Get_Procedure IN Persistent-Handle (INPUT "fld_lbls.",OUTPUT run-proc,no).
  IF run-proc NE "" THEN
  DO WITH FRAME {&FRAME-NAME}:
    RUN VALUE(run-proc)
        (svLookupFile:SCREEN-VALUE,svOrderFields:LIST-ITEMS,OUTPUT item_list).
    ASSIGN
      svOrderValues:SCREEN-VALUE = item_list
      svOrderValues
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetLookup C-Win 
PROCEDURE pGetLookup :
/* -----------------------------------------------------------
  Purpose:Find and Read Lookup Browser
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  IF SEARCH("lookups/" + mLookupPrgm + "p") NE ? THEN
  DO WITH FRAME {&FRAME-NAME}:
    INPUT FROM VALUE("lookups/" + mLookupPrgm + "p") NO-ECHO.
    IMPORT ^.
    IMPORT svLookupPrgm
      svLookupDB
      svLookupFile
      svWhereStatement
      svReturnField
      svFont
      svHeightSize
      svWidthSize
      cShowFields
      svOrderValues
      svAutoSet
      cOrderFields
      svFrameTitle
      svTopInclude
      svDefInclude
      svEndInclude
      svUIPrgmname.
    INPUT CLOSE.
    ASSIGN
      svShowFields:LIST-ITEMS  = TRIM(cShowFields)
      svOrderFields:LIST-ITEMS = TRIM(cOrderFields)
      .
  END.
  ELSE
  svLookupPrgm = mLookupPrgm.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

