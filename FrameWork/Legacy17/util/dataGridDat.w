&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: util/dataGridDat.w

  Description: access to .Net Grid query and search columns

  Input Parameters: Date Grid .dat file name & External Tables

  Output Parameters: <none>

  Author: Ron Stark

  Created: 4.11.2017
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipcDataGridDat     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcDataGridInclude AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcExternalTables  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcTablesInQuery   AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipcDataGridDat     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ipcDataGridInclude AS CHARACTER NO-UNDO.
DEFINE VARIABLE ipcExternalTables  AS CHARACTER NO-UNDO.
DEFINE VARIABLE ipcTablesInQuery   AS CHARACTER NO-UNDO.

ASSIGN
    ipcDataGridDat     = "C:\Advantzware\v16\Resources\dataGrid\browsers\mstd.dat"
    ipcDataGridInclude = "C:\Advantzware\v16\Legacy17\dataGrid\browsers\mstd.i"
    ipcExternalTables  = "mach"
    ipcTablesInQuery   = "mstd"
    ipcDataGridDat     = "C:\Advantzware\v16\Resources\dataGrid\browsers\mach.dat"
    ipcDataGridInclude = "C:\Advantzware\v16\Legacy17\dataGrid\browsers\mach.i"
    ipcExternalTables  = ""
    ipcTablesInQuery   = "mach"
    .
&ENDIF

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCancel externalTables externalFields ~
tablesInQuery inQueryFields queryString dataGridIncludeFields ~
selectedFields indexedFields nonIndexedFields btnClear btnReset btnForEach ~
btnWhere btnNoLock btnBegins btnEQ btnNE btnAnd btnOR btnGT btnGE btnLT ~
btnLE btnTrue btnFalse btnLeftPar btnRightPar btnComma btnUnknown btnFirst ~
btnLast btnCompany btnLoc btnOK 
&Scoped-Define DISPLAYED-OBJECTS externalTables externalFields ~
tablesInQuery inQueryFields indexText queryString dataGridIncludeFields ~
selectedFields indexedFields nonIndexedFields generated dataGridIncludeFile 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 indexText queryString selectedFields indexedFields ~
nonIndexedFields 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetFields Dialog-Frame 
FUNCTION fGetFields RETURNS CHARACTER
  ( ipcTable AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fInsertField Dialog-Frame 
FUNCTION fInsertField RETURNS CHARACTER
  ( ipcTable AS CHARACTER, ipcField AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fInsertQuery Dialog-Frame 
FUNCTION fInsertQuery RETURNS LOGICAL
  ( ipcText AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAnd 
     LABEL "AND" 
     SIZE 7 BY 1.14.

DEFINE BUTTON btnBegins 
     LABEL "BEGINS" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON btnClear 
     LABEL "Clear Query" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnComma 
     LABEL "," 
     SIZE 7 BY 1.14.

DEFINE BUTTON btnCompany 
     LABEL "~"%company%~"" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnEQ 
     LABEL "EQ" 
     SIZE 7 BY 1.14.

DEFINE BUTTON btnFalse 
     LABEL "FALSE" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnFirst 
     LABEL "FIRST" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnForEach 
     LABEL "FOR EACH" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnGE 
     LABEL "GE" 
     SIZE 7 BY 1.14.

DEFINE BUTTON btnGT 
     LABEL "GT" 
     SIZE 7 BY 1.14.

DEFINE BUTTON btnLast 
     LABEL "LAST" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnLE 
     LABEL "LE" 
     SIZE 7 BY 1.14.

DEFINE BUTTON btnLeftPar 
     LABEL "(" 
     SIZE 7 BY 1.14.

DEFINE BUTTON btnLoc 
     LABEL "~"%loc%~"" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnLT 
     LABEL "LT" 
     SIZE 7 BY 1.14.

DEFINE BUTTON btnNE 
     LABEL "NE" 
     SIZE 7 BY 1.14.

DEFINE BUTTON btnNoLock 
     LABEL "NO-LOCK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON btnOR 
     LABEL "OR" 
     SIZE 7 BY 1.14.

DEFINE BUTTON btnReset 
     LABEL "Reset Query" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnRightPar 
     LABEL ")" 
     SIZE 7 BY 1.14.

DEFINE BUTTON btnTrue 
     LABEL "TRUE" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUnknown 
     LABEL "?" 
     SIZE 7 BY 1.14.

DEFINE BUTTON btnWhere 
     LABEL "WHERE" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE indexText AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 50 BY 35.95
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE queryString AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 135 BY 9.29
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE dataGridIncludeFields AS CHARACTER FORMAT "X(256)":U 
     LABEL "Include Fields" 
     VIEW-AS FILL-IN 
     SIZE 116 BY 1 NO-UNDO.

DEFINE VARIABLE dataGridIncludeFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "Data Grid Include" 
      VIEW-AS TEXT 
     SIZE 116 BY .62 NO-UNDO.

DEFINE VARIABLE generated AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 48 BY .62
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE externalFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 33 BY 6.67 NO-UNDO.

DEFINE VARIABLE externalTables AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 33 BY 6.67 NO-UNDO.

DEFINE VARIABLE indexedFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 33 BY 15.95 NO-UNDO.

DEFINE VARIABLE inQueryFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 33 BY 6.67 NO-UNDO.

DEFINE VARIABLE nonIndexedFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 33 BY 15.95 NO-UNDO.

DEFINE VARIABLE selectedFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 33 BY 15.95 NO-UNDO.

DEFINE VARIABLE tablesInQuery AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 33 BY 6.67 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnCancel AT ROW 35.76 COL 127
     externalTables AT ROW 1.71 COL 1 NO-LABEL WIDGET-ID 26
     externalFields AT ROW 1.71 COL 35 NO-LABEL WIDGET-ID 32
     tablesInQuery AT ROW 1.71 COL 69 NO-LABEL WIDGET-ID 28
     inQueryFields AT ROW 1.71 COL 103 NO-LABEL WIDGET-ID 36
     indexText AT ROW 1.71 COL 137 NO-LABEL WIDGET-ID 4
     queryString AT ROW 9.33 COL 1 NO-LABEL WIDGET-ID 2
     dataGridIncludeFields AT ROW 19.81 COL 18 COLON-ALIGNED WIDGET-ID 92
     selectedFields AT ROW 21.71 COL 1 NO-LABEL WIDGET-ID 10
     indexedFields AT ROW 21.71 COL 35 NO-LABEL WIDGET-ID 6
     nonIndexedFields AT ROW 21.71 COL 69 NO-LABEL WIDGET-ID 8
     btnClear AT ROW 21.71 COL 104 WIDGET-ID 66
     btnReset AT ROW 21.71 COL 120 WIDGET-ID 80
     btnForEach AT ROW 24.1 COL 104 WIDGET-ID 40
     btnWhere AT ROW 24.1 COL 120 WIDGET-ID 42
     btnNoLock AT ROW 25.29 COL 104 WIDGET-ID 48
     btnBegins AT ROW 25.29 COL 120 WIDGET-ID 78
     btnEQ AT ROW 26.48 COL 104 WIDGET-ID 50
     btnNE AT ROW 26.48 COL 112 WIDGET-ID 60
     btnAnd AT ROW 26.48 COL 120 WIDGET-ID 68
     btnOR AT ROW 26.48 COL 128 WIDGET-ID 70
     btnGT AT ROW 27.67 COL 104 WIDGET-ID 52
     btnGE AT ROW 27.67 COL 112 WIDGET-ID 54
     btnLT AT ROW 27.67 COL 120 WIDGET-ID 58
     btnLE AT ROW 27.67 COL 128 WIDGET-ID 56
     btnTrue AT ROW 28.86 COL 104 WIDGET-ID 86
     btnFalse AT ROW 28.86 COL 120 WIDGET-ID 88
     btnLeftPar AT ROW 30.05 COL 104 WIDGET-ID 72
     btnRightPar AT ROW 30.05 COL 112 WIDGET-ID 76
     btnComma AT ROW 30.05 COL 120 WIDGET-ID 82
     btnUnknown AT ROW 30.05 COL 128 WIDGET-ID 84
     btnFirst AT ROW 31.24 COL 104 WIDGET-ID 44
     btnLast AT ROW 31.24 COL 120 WIDGET-ID 46
     btnCompany AT ROW 33.62 COL 104 WIDGET-ID 62
     btnLoc AT ROW 33.62 COL 120 WIDGET-ID 64
     btnOK AT ROW 35.76 COL 118
     generated AT ROW 8.62 COL 49 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     dataGridIncludeFile AT ROW 18.86 COL 18 COLON-ALIGNED WIDGET-ID 90
     "Non-Indexed Fields" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 21 COL 70 WIDGET-ID 14
     "Selected Fields" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 21 COL 2 WIDGET-ID 16
     "Query" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 8.62 COL 2 WIDGET-ID 18
     "Tables In Query Fields" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 1 COL 104 WIDGET-ID 38
     "Table / Indexes / Record Count" VIEW-AS TEXT
          SIZE 32 BY .62 AT ROW 1 COL 137 WIDGET-ID 20
     "External Tables" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1 COL 2 WIDGET-ID 24
     "Tables In Query" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1 COL 70 WIDGET-ID 30
     "External Table Fields" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 1 COL 36 WIDGET-ID 34
     "Indexed Fields" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 21 COL 36 WIDGET-ID 12
     SPACE(136.99) SKIP(16.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Data Grid .Dat"
         CANCEL-BUTTON btnCancel WIDGET-ID 100.


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
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN dataGridIncludeFile IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN generated IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST indexedFields IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR EDITOR indexText IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR SELECTION-LIST nonIndexedFields IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR EDITOR queryString IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR SELECTION-LIST selectedFields IN FRAME Dialog-Frame
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Data Grid .Dat */
DO:
    APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAnd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAnd Dialog-Frame
ON CHOOSE OF btnAnd IN FRAME Dialog-Frame /* AND */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBegins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBegins Dialog-Frame
ON CHOOSE OF btnBegins IN FRAME Dialog-Frame /* BEGINS */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClear Dialog-Frame
ON CHOOSE OF btnClear IN FRAME Dialog-Frame /* Clear Query */
DO:
    queryString:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnComma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnComma Dialog-Frame
ON CHOOSE OF btnComma IN FRAME Dialog-Frame /* , */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCompany Dialog-Frame
ON CHOOSE OF btnCompany IN FRAME Dialog-Frame /* "%company%" */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEQ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEQ Dialog-Frame
ON CHOOSE OF btnEQ IN FRAME Dialog-Frame /* EQ */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFalse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFalse Dialog-Frame
ON CHOOSE OF btnFalse IN FRAME Dialog-Frame /* FALSE */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst Dialog-Frame
ON CHOOSE OF btnFirst IN FRAME Dialog-Frame /* FIRST */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnForEach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnForEach Dialog-Frame
ON CHOOSE OF btnForEach IN FRAME Dialog-Frame /* FOR EACH */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGE Dialog-Frame
ON CHOOSE OF btnGE IN FRAME Dialog-Frame /* GE */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGT Dialog-Frame
ON CHOOSE OF btnGT IN FRAME Dialog-Frame /* GT */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast Dialog-Frame
ON CHOOSE OF btnLast IN FRAME Dialog-Frame /* LAST */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLE Dialog-Frame
ON CHOOSE OF btnLE IN FRAME Dialog-Frame /* LE */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLeftPar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLeftPar Dialog-Frame
ON CHOOSE OF btnLeftPar IN FRAME Dialog-Frame /* ( */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLoc Dialog-Frame
ON CHOOSE OF btnLoc IN FRAME Dialog-Frame /* "%loc%" */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLT Dialog-Frame
ON CHOOSE OF btnLT IN FRAME Dialog-Frame /* LT */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNE Dialog-Frame
ON CHOOSE OF btnNE IN FRAME Dialog-Frame /* NE */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNoLock
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNoLock Dialog-Frame
ON CHOOSE OF btnNoLock IN FRAME Dialog-Frame /* NO-LOCK */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame
DO:
    RUN pSetDataGridDat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOR Dialog-Frame
ON CHOOSE OF btnOR IN FRAME Dialog-Frame /* OR */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset Dialog-Frame
ON CHOOSE OF btnReset IN FRAME Dialog-Frame /* Reset Query */
DO:
    RUN pGetDataGridDat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRightPar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRightPar Dialog-Frame
ON CHOOSE OF btnRightPar IN FRAME Dialog-Frame /* ) */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTrue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTrue Dialog-Frame
ON CHOOSE OF btnTrue IN FRAME Dialog-Frame /* TRUE */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUnknown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUnknown Dialog-Frame
ON CHOOSE OF btnUnknown IN FRAME Dialog-Frame /* ? */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnWhere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnWhere Dialog-Frame
ON CHOOSE OF btnWhere IN FRAME Dialog-Frame /* WHERE */
DO:
    fInsertQuery({&SELF-NAME}:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME externalFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL externalFields Dialog-Frame
ON DEFAULT-ACTION OF externalFields IN FRAME Dialog-Frame
DO:
    ASSIGN {&SELF-NAME}.
    fInsertQuery(fInsertField(externalTables,{&SELF-NAME})).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME externalTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL externalTables Dialog-Frame
ON VALUE-CHANGED OF externalTables IN FRAME Dialog-Frame
DO:
    ASSIGN {&SELF-NAME}.
    externalFields:LIST-ITEMS = fGetFields({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME indexedFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL indexedFields Dialog-Frame
ON DEFAULT-ACTION OF indexedFields IN FRAME Dialog-Frame
DO:
    IF selectedFields:LIST-ITEMS EQ ? OR
       NOT CAN-DO(selectedFields:LIST-ITEMS,{&SELF-NAME}:SCREEN-VALUE) THEN
    selectedFields:ADD-LAST({&SELF-NAME}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inQueryFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inQueryFields Dialog-Frame
ON DEFAULT-ACTION OF inQueryFields IN FRAME Dialog-Frame
DO:
    ASSIGN {&SELF-NAME}.
    fInsertQuery(tablesInQuery + "." + {&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME nonIndexedFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nonIndexedFields Dialog-Frame
ON DEFAULT-ACTION OF nonIndexedFields IN FRAME Dialog-Frame
DO:
    IF selectedFields:LIST-ITEMS EQ ? OR
       NOT CAN-DO(selectedFields:LIST-ITEMS,{&SELF-NAME}:SCREEN-VALUE) THEN
    selectedFields:ADD-LAST({&SELF-NAME}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selectedFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selectedFields Dialog-Frame
ON DEFAULT-ACTION OF selectedFields IN FRAME Dialog-Frame
DO:
    {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tablesInQuery
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tablesInQuery Dialog-Frame
ON DEFAULT-ACTION OF tablesInQuery IN FRAME Dialog-Frame
DO:
    ASSIGN {&SELF-NAME}.
    fInsertQuery({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tablesInQuery Dialog-Frame
ON VALUE-CHANGED OF tablesInQuery IN FRAME Dialog-Frame
DO:
    ASSIGN {&SELF-NAME}.
    inQueryFields:LIST-ITEMS = fGetFields({&SELF-NAME}).
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
  RUN enable_UI.
  FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + ": " + ipcDataGridDat.
  RUN pGetDataGridDat.
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
  DISPLAY externalTables externalFields tablesInQuery inQueryFields indexText 
          queryString dataGridIncludeFields selectedFields indexedFields 
          nonIndexedFields generated dataGridIncludeFile 
      WITH FRAME Dialog-Frame.
  ENABLE btnCancel externalTables externalFields tablesInQuery inQueryFields 
         queryString dataGridIncludeFields selectedFields indexedFields 
         nonIndexedFields btnClear btnReset btnForEach btnWhere btnNoLock 
         btnBegins btnEQ btnNE btnAnd btnOR btnGT btnGE btnLT btnLE btnTrue 
         btnFalse btnLeftPar btnRightPar btnComma btnUnknown btnFirst btnLast 
         btnCompany btnLoc btnOK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDataGridDat Dialog-Frame 
PROCEDURE pGetDataGridDat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cColumns     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIndexed     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNonIndexed  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cGenerated   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIndexText   AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        INPUT FROM VALUE(ipcDataGridDat) NO-ECHO.
        IMPORT UNFORMATTED cQueryString.
        IMPORT UNFORMATTED cColumns.
        IMPORT ^.
        IMPORT UNFORMATTED cIndexed.
        IMPORT UNFORMATTED cNonIndexed.
        IMPORT ^.
        IMPORT UNFORMATTED cGenerated.
        
        ASSIGN
            ipcExternalTables           = REPLACE(ipcExternalTables," ",",")
            ipcExternalTables           = TRIM(ipcExternalTables,",")
            externalTables:LIST-ITEMS   = ipcExternalTables
            ipcTablesInQuery            = REPLACE(ipcTablesInQuery," ",",")
            ipcTablesInQuery            = TRIM(ipcTablesInQuery,",")
            tablesInQuery:LIST-ITEMS    = ipcTablesInQuery
            tablesInQuery:SCREEN-VALUE  = tablesInQuery:ENTRY(1)
            generated:SCREEN-VALUE      = cGenerated
            queryString:SCREEN-VALUE    = cQueryString
            cIndexed                    = REPLACE(cIndexed, "Indexed: ", "")
            indexedFields:LIST-ITEMS    = cIndexed
            cNonIndexed                 = REPLACE(cNonIndexed, "Non-Idx: ", "")
            nonIndexedFields:LIST-ITEMS = cNonIndexed
            selectedFields:LIST-ITEMS   = cColumns
            indexText:SCREEN-VALUE      = ""
            .
        REPEAT:
            IMPORT UNFORMATTED cIndexText.
            indexText:SCREEN-VALUE = indexText:SCREEN-VALUE + cIndexText + CHR(10).
        END. /* repeat */
        APPLY "VALUE-CHANGED":U TO tablesInQuery.
        IF externalTables:NUM-ITEMS NE 0 THEN DO:
            externalTables:SCREEN-VALUE = externalTables:ENTRY(1).
            APPLY "VALUE-CHANGED":U TO externalTables.
        END.
        ELSE
        DISABLE externalTables externalFields.
        INPUT CLOSE.
        
        IF ipcDataGridInclude NE ? THEN DO:
            IF SEARCH(ipcDataGridInclude) EQ ? THEN DO:
                OUTPUT TO VALUE(ipcDataGridInclude).
                PUT UNFORMATTED SKIP(1).
                OUTPUT CLOSE.
            END. /* if ne ? */
            ELSE DO:            
                INPUT FROM VALUE(SEARCH(ipcDataGridInclude)) NO-ECHO.
                IMPORT UNFORMATTED dataGridIncludeFields.
                INPUT CLOSE.
            END. /* else */
    
            ASSIGN
                dataGridIncludeFile:SCREEN-VALUE   = SEARCH(ipcDataGridInclude)
                dataGridIncludeFields:SCREEN-VALUE = dataGridIncludeFields
                .
        END. /* if ne ? */
        
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetDataGridDat Dialog-Frame 
PROCEDURE pSetDataGridDat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        OUTPUT TO VALUE(ipcDataGridDat).
        PUT UNFORMATTED
            LEFT-TRIM(TRIM(REPLACE(queryString:SCREEN-VALUE,CHR(10)," "))) SKIP
            selectedFields:LIST-ITEMS SKIP(1)
            "Indexed: " indexedFields:LIST-ITEMS SKIP
            "Non-Idx: " nonIndexedFields:LIST-ITEMS SKIP(1)
            "Generated " STRING (TODAY ,"99.99.9999") " @ "
            STRING (TIME ,"hh:mm:ss am") " by: " USERID ("ASI") SKIP
            indexText:SCREEN-VALUE
            .
        OUTPUT CLOSE.

        IF ipcDataGridInclude NE ? THEN DO:
            OUTPUT TO VALUE(ipcDataGridInclude).
            PUT UNFORMATTED dataGridIncludeFields:SCREEN-VALUE SKIP(1).
            OUTPUT CLOSE.
        END. /* if ne ? */
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetFields Dialog-Frame 
FUNCTION fGetFields RETURNS CHARACTER
  ( ipcTable AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.

    FIND FIRST asi._file NO-LOCK
         WHERE asi._file._file-name EQ ipcTable
         NO-ERROR.
    IF AVAILABLE asi._file THEN
    FOR EACH asi._field OF asi._file NO-LOCK:
        cFields = cFields + asi._field._field-name + ",".
    END. /* for each */
    cFields = TRIM(cFields,",").

    RETURN cFields.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fInsertField Dialog-Frame 
FUNCTION fInsertField RETURNS CHARACTER
  ( ipcTable AS CHARACTER, ipcField AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuote  AS CHARACTER NO-UNDO.

    FIND FIRST asi._file NO-LOCK
         WHERE asi._file._file-name EQ ipcTable
         NO-ERROR.
    IF NOT AVAILABLE asi._file THEN RETURN "[Error Missing Table]".
    FIND FIRST asi._field OF asi._file NO-LOCK
         WHERE asi._field._field-name EQ ipcField
         NO-ERROR.
    IF NOT AVAILABLE asi._field THEN RETURN "[Error Missing Field]".
    IF asi._field._data-type EQ "character" THEN cQuote = "~"".
    RETURN cQuote + "%" + ipcTable + "." + ipcField + "%" + cQuote.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fInsertQuery Dialog-Frame 
FUNCTION fInsertQuery RETURNS LOGICAL
  ( ipcText AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    queryString:INSERT-STRING(ipcText + " ") IN FRAME {&FRAME-NAME}.
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

