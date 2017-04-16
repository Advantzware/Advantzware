&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: colorGrid.w

  Description: Color Grid Window

  Input Parameters: schedule board handle

  Output Parameters: <none>

  Author: Ron Stark

  Created: 7.17.2004

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

{schedule/scopDir.i}
{{&includes}/defBoard.i}

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipBoardHandle AS HANDLE NO-UNDO.

/* Local Variable Definitions ---                                       */

&SCOPED-DEFINE noTempTables YES
{{&includes}/ttblJob.i}

DEFINE VARIABLE colorWidget AS WIDGET NO-UNDO EXTENT {&statusExtent}.
DEFINE VARIABLE colorOnWidget AS WIDGET-HANDLE NO-UNDO EXTENT {&statusExtent}.
DEFINE VARIABLE hPixels AS INTEGER NO-UNDO.
DEFINE VARIABLE ID AS CHARACTER NO-UNDO {{&includes}/initID.i}.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE priorityWidget AS WIDGET NO-UNDO EXTENT {&statusExtent}.
DEFINE VARIABLE scenario AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BtnApply btnON btnOFF setColorType 
&Scoped-Define DISPLAYED-OBJECTS setColorType 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnApply 
     LABEL "Apply Selected Colors by &Priority" 
     SIZE 34 BY 1.14.

DEFINE BUTTON btnOFF 
     LABEL "All Colors Of&f" 
     SIZE 16 BY 1.14.

DEFINE BUTTON btnON 
     LABEL "All Colors O&n" 
     SIZE 16 BY 1.14.

DEFINE VARIABLE colorLabel-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-11 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-12 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-13 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-14 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-15 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-16 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-17 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-18 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-19 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-20 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-21 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-22 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-23 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-24 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-25 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-26 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-27 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-28 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-29 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-30 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE colorLabel-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE priorityLabel-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-11 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-12 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-13 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-14 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-15 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-16 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-17 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-18 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-19 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-20 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-21 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-22 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-23 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-24 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-25 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-26 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-27 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-28 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-29 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-30 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE priorityLabel-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .62
     FONT 2 NO-UNDO.

DEFINE VARIABLE setColorType AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "&All", "All",
"&Time Only", "Time",
"&Status Only", "Status"
     SIZE 34 BY .62
     BGCOLOR 7  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 36 BY 3.57
     BGCOLOR 7 .

DEFINE VARIABLE colorOn-1 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-10 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-11 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-12 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-13 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-14 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-15 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-16 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-17 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-18 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-19 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-2 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-20 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-21 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-22 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-23 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-24 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-25 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-26 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-27 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-28 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-29 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-3 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-30 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-4 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-5 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-6 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-7 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-8 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE colorOn-9 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .62 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BtnApply AT ROW 1.24 COL 2
     btnON AT ROW 2.67 COL 2
     btnOFF AT ROW 2.67 COL 20
     setColorType AT ROW 3.86 COL 2 NO-LABEL
     priorityLabel-17 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-10 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-4 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-25 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-30 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-23 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-24 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-7 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-8 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-19 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-15 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-11 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-9 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-18 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-5 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-14 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-20 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-21 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-16 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-3 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-13 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-12 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-6 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-26 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-1 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-2 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-22 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-28 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-27 AT ROW 4.57 COL 1 NO-LABEL
     priorityLabel-29 AT ROW 4.57 COL 1 NO-LABEL
     colorLabel-30 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-12 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-17 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-13 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-21 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-7 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-25 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-5 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-1 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-18 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-24 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-8 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-9 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-6 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-26 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-10 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-19 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-27 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-11 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-14 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-20 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-15 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-29 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-23 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-2 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-28 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-22 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-4 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-16 AT ROW 4.57 COL 6 NO-LABEL
     colorLabel-3 AT ROW 4.57 COL 6 NO-LABEL
     colorOn-28 AT ROW 4.57 COL 34
     colorOn-6 AT ROW 4.57 COL 34
     colorOn-25 AT ROW 4.57 COL 34
     colorOn-4 AT ROW 4.57 COL 34
     colorOn-11 AT ROW 4.57 COL 34
     colorOn-24 AT ROW 4.57 COL 34
     colorOn-30 AT ROW 4.57 COL 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 36 BY 23.67.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     colorOn-7 AT ROW 4.57 COL 34
     colorOn-18 AT ROW 4.57 COL 34
     colorOn-10 AT ROW 4.57 COL 34
     colorOn-15 AT ROW 4.57 COL 34
     colorOn-21 AT ROW 4.57 COL 34
     colorOn-5 AT ROW 4.57 COL 34
     colorOn-19 AT ROW 4.57 COL 34
     colorOn-22 AT ROW 4.57 COL 34
     colorOn-16 AT ROW 4.57 COL 34
     colorOn-1 AT ROW 4.57 COL 34
     colorOn-14 AT ROW 4.57 COL 34
     colorOn-27 AT ROW 4.57 COL 34
     colorOn-17 AT ROW 4.57 COL 34
     colorOn-2 AT ROW 4.57 COL 34
     colorOn-8 AT ROW 4.57 COL 34
     colorOn-13 AT ROW 4.57 COL 34
     colorOn-26 AT ROW 4.57 COL 34
     colorOn-9 AT ROW 4.57 COL 34
     colorOn-23 AT ROW 4.57 COL 34
     colorOn-3 AT ROW 4.57 COL 34
     colorOn-29 AT ROW 4.57 COL 34
     colorOn-20 AT ROW 4.57 COL 34
     colorOn-12 AT ROW 4.57 COL 34
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 36 BY 23.67.


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
         TITLE              = "Color Legend"
         HEIGHT             = 23.67
         WIDTH              = 36
         MAX-HEIGHT         = 23.67
         MAX-WIDTH          = 62.4
         VIRTUAL-HEIGHT     = 23.67
         VIRTUAL-WIDTH      = 62.4
         SHOW-IN-TASKBAR    = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         TOP-ONLY           = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("schedule/images/scheduler.ico":U) THEN
    MESSAGE "Unable to load icon: schedule/images/scheduler.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FILL-IN colorLabel-1 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-10 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-10:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-11 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-11:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-12 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-12:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-13 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-13:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-14 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-14:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-15 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-15:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-16 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-16:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-17 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-17:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-18 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-18:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-19 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-19:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-2 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-20 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-20:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-21 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-21:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-22 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-22:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-23 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-23:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-24 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-24:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-25 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-25:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-26 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-26:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-27 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-27:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-28 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-28:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-29 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-29:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-3 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-30 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-30:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-4 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-4:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-5 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-5:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-6 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-6:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-7 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-7:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-8 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-8:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN colorLabel-9 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       colorLabel-9:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-1 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-10 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-10:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-11 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-11:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-12 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-12:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-13 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-13:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-14 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-14:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-15 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-15:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-16 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-16:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-17 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-17:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-18 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-18:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-19 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-19:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-2 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-20 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-20:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-21 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-21:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-22 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-22:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-23 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-23:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-24 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-24:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-25 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-25:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-26 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-26:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-27 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-27:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-28 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-28:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-29 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-29:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-3 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-30 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-30:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-4 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-4:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-5 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-5:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-6 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-6:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-7 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-7:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-8 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-8:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX colorOn-9 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       colorOn-9:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-1 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-10 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-10:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-11 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-11:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-12 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-12:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-13 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-13:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-14 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-14:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-15 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-15:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-16 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-16:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-17 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-17:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-18 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-18:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-19 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-19:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-2 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-20 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-20:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-21 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-21:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-22 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-22:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-23 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-23:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-24 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-24:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-25 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-25:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-26 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-26:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-27 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-27:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-28 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-28:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-29 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-29:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-3 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-30 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-30:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-4 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-4:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-5 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-5:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-6 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-6:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-7 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-7:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-8 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-8:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN priorityLabel-9 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       priorityLabel-9:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Color Legend */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Color Legend */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnApply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnApply C-Win
ON CHOOSE OF BtnApply IN FRAME DEFAULT-FRAME /* Apply Selected Colors by Priority */
DO:
  RUN setJobColors IN ipBoardHandle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOFF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOFF C-Win
ON CHOOSE OF btnOFF IN FRAME DEFAULT-FRAME /* All Colors Off */
DO:
  RUN setAllColorsOnOff (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnON C-Win
ON CHOOSE OF btnON IN FRAME DEFAULT-FRAME /* All Colors On */
DO:
  RUN setAllColorsOnOff (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-1 C-Win
ON VALUE-CHANGED OF colorOn-1 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-10 C-Win
ON VALUE-CHANGED OF colorOn-10 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-11 C-Win
ON VALUE-CHANGED OF colorOn-11 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-12 C-Win
ON VALUE-CHANGED OF colorOn-12 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-13 C-Win
ON VALUE-CHANGED OF colorOn-13 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-14 C-Win
ON VALUE-CHANGED OF colorOn-14 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-15 C-Win
ON VALUE-CHANGED OF colorOn-15 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-16 C-Win
ON VALUE-CHANGED OF colorOn-16 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-17 C-Win
ON VALUE-CHANGED OF colorOn-17 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-18 C-Win
ON VALUE-CHANGED OF colorOn-18 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-19 C-Win
ON VALUE-CHANGED OF colorOn-19 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-2 C-Win
ON VALUE-CHANGED OF colorOn-2 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-20 C-Win
ON VALUE-CHANGED OF colorOn-20 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-21 C-Win
ON VALUE-CHANGED OF colorOn-21 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-22
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-22 C-Win
ON VALUE-CHANGED OF colorOn-22 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-23 C-Win
ON VALUE-CHANGED OF colorOn-23 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-24
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-24 C-Win
ON VALUE-CHANGED OF colorOn-24 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-25
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-25 C-Win
ON VALUE-CHANGED OF colorOn-25 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-26
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-26 C-Win
ON VALUE-CHANGED OF colorOn-26 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-27
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-27 C-Win
ON VALUE-CHANGED OF colorOn-27 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-28
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-28 C-Win
ON VALUE-CHANGED OF colorOn-28 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-29
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-29 C-Win
ON VALUE-CHANGED OF colorOn-29 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-3 C-Win
ON VALUE-CHANGED OF colorOn-3 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-30
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-30 C-Win
ON VALUE-CHANGED OF colorOn-30 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-4 C-Win
ON VALUE-CHANGED OF colorOn-4 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-5 C-Win
ON VALUE-CHANGED OF colorOn-5 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-6 C-Win
ON VALUE-CHANGED OF colorOn-6 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-7 C-Win
ON VALUE-CHANGED OF colorOn-7 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-8 C-Win
ON VALUE-CHANGED OF colorOn-8 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME colorOn-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorOn-9 C-Win
ON VALUE-CHANGED OF colorOn-9 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorOnOff (SELF:PRIVATE-DATA,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setColorType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setColorType C-Win
ON VALUE-CHANGED OF setColorType IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  RUN setColorType ({&SELF-NAME}).
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
  RUN ID.
  RUN getConfiguration (OUTPUT hpixels).
  ASSIGN
    {&WINDOW-NAME}:HEIGHT-PIXELS = hpixels
    {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = hpixels
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS = hpixels
    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = hpixels.
  APPLY 'ENTRY':U TO setColorType.
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
  DISPLAY setColorType 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BtnApply btnON btnOFF setColorType 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getConfiguration C-Win 
PROCEDURE getConfiguration :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER yCoord AS INTEGER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      {{&includes}/colorGrid.i 1}
      {{&includes}/colorGrid.i 2}
      {{&includes}/colorGrid.i 3}
      {{&includes}/colorGrid.i 4}
      {{&includes}/colorGrid.i 5}
      {{&includes}/colorGrid.i 6}
      {{&includes}/colorGrid.i 7}
      {{&includes}/colorGrid.i 8}
      {{&includes}/colorGrid.i 9}
      {{&includes}/colorGrid.i 10}.
    ASSIGN
      {{&includes}/colorGrid.i 11}
      {{&includes}/colorGrid.i 12}
      {{&includes}/colorGrid.i 13}
      {{&includes}/colorGrid.i 14}
      {{&includes}/colorGrid.i 15}
      {{&includes}/colorGrid.i 16}
      {{&includes}/colorGrid.i 17}
      {{&includes}/colorGrid.i 18}
      {{&includes}/colorGrid.i 19}
      {{&includes}/colorGrid.i 20}.
    ASSIGN
      {{&includes}/colorGrid.i 21}
      {{&includes}/colorGrid.i 22}
      {{&includes}/colorGrid.i 23}
      {{&includes}/colorGrid.i 24}
      {{&includes}/colorGrid.i 25}
      {{&includes}/colorGrid.i 26}
      {{&includes}/colorGrid.i 27}
      {{&includes}/colorGrid.i 28}
      {{&includes}/colorGrid.i 29}
      {{&includes}/colorGrid.i 30}
      yCoord = RECT-1:HEIGHT-PIXELS + 1.
    FOR EACH jobColors NO-LOCK:
      IF jobColors.bgColorValue EQ ? AND jobColors.colorLabel EQ '' THEN NEXT.
      ASSIGN
        i = i + 1
        colorWidget[i]:HIDDEN = NO
        colorWidget[i]:BGCOLOR = jobColors.bgColorValue
        colorWidget[i]:FGCOLOR = jobColors.fgColorValue
        colorWidget[i]:SCREEN-VALUE = jobColors.colorLabel
        colorWidget[i]:PRIVATE-DATA = STRING(jobColors.timeColor,'Time/Status')
        colorWidget[i]:Y = yCoord
        colorOnWidget[i]:HIDDEN = NO
        colorOnWidget[i]:SENSITIVE = jobColors.priority NE 0
        colorOnWidget[i]:SCREEN-VALUE = STRING(jobColors.colorOn)
        colorOnWidget[i]:PRIVATE-DATA = STRING(jobColors.priority)
        colorOnWidget[i]:Y = yCoord.
      IF jobColors.priority NE 0 THEN
      ASSIGN
        priorityWidget[i]:HIDDEN = NO
        priorityWidget[i]:SCREEN-VALUE = STRING(jobColors.priority,'z9')
        priorityWidget[i]:Y = yCoord.
      yCoord = yCoord + 14.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ID C-Win 
PROCEDURE ID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/id.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setAllColorsOnOff C-Win 
PROCEDURE setAllColorsOnOff :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipOnOff AS LOGICAL NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = 3 TO EXTENT(colorOnWidget):
    IF NOT colorOnWidget[i]:SENSITIVE THEN NEXT.
    colorOnWidget[i]:SCREEN-VALUE = STRING(ipOnOff).
    APPLY 'VALUE-CHANGED' TO colorOnWidget[i].
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setColorOnOff C-Win 
PROCEDURE setColorOnOff :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipPriority AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipOnOff AS LOGICAL NO-UNDO.

  FIND jobColors EXCLUSIVE-LOCK WHERE jobColors.priority EQ ipPriority.
  jobColors.colorOn = ipOnOff.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setColorType C-Win 
PROCEDURE setColorType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipType AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = 3 TO EXTENT(colorOnWidget):
    IF NOT colorOnWidget[i]:SENSITIVE THEN NEXT.
    colorOnWidget[i]:SCREEN-VALUE = STRING(colorWidget[i]:PRIVATE-DATA EQ ipType OR ipType EQ 'All').
    APPLY 'VALUE-CHANGED' TO colorOnWidget[i].
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

