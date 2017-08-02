&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: popupcal.w

  Description: Popup Calendar

  Input Parameters:
      <none>

  Output Parameters: Calendar Date

  Author: Ron Stark

  Created: 7.26.2000

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) = 0 &THEN
DEFINE OUTPUT PARAMETER calendarDate AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE calendarDate AS CHARACTER NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE dateValue AS DATE NO-UNDO.
DEFINE VARIABLE days AS CHARACTER NO-UNDO EXTENT 37.
DEFINE VARIABLE iDay AS INTEGER NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnClose btnOK btnToday months sDay years ~
Btn-1 Btn-2 Btn-3 Btn-4 Btn-5 Btn-6 Btn-7 Btn-8 Btn-9 Btn-10 Btn-11 Btn-12 ~
Btn-13 Btn-14 Btn-15 Btn-16 Btn-17 Btn-18 Btn-19 Btn-20 Btn-21 Btn-22 ~
Btn-23 Btn-24 Btn-25 Btn-26 Btn-27 Btn-28 Btn-29 Btn-30 Btn-31 Btn-32 ~
Btn-33 Btn-34 Btn-35 Btn-36 Btn-37 
&Scoped-Define DISPLAYED-OBJECTS months sDay years 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Btn-1 Btn-2 Btn-3 Btn-4 Btn-5 Btn-6 Btn-7 Btn-8 Btn-9 ~
Btn-10 Btn-11 Btn-12 Btn-13 Btn-14 Btn-15 Btn-16 Btn-17 Btn-18 Btn-19 ~
Btn-20 Btn-21 Btn-22 Btn-23 Btn-24 Btn-25 Btn-26 Btn-27 Btn-28 Btn-29 ~
Btn-30 Btn-31 Btn-32 Btn-33 Btn-34 Btn-35 Btn-36 Btn-37 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-1 
     LABEL "1" 
     SIZE 4 BY 1
     FONT 2.

DEFINE BUTTON Btn-10 
     LABEL "10" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-11 
     LABEL "11" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-12 
     LABEL "12" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-13 
     LABEL "13" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-14 
     LABEL "14" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-15 
     LABEL "15" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-16 
     LABEL "16" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-17 
     LABEL "17" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-18 
     LABEL "18" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-19 
     LABEL "19" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-2 
     LABEL "2" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-20 
     LABEL "20" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-21 
     LABEL "21" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-22 
     LABEL "22" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-23 
     LABEL "23" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-24 
     LABEL "24" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-25 
     LABEL "25" 
     SIZE 4 BY 1
     FONT 2.

DEFINE BUTTON Btn-26 
     LABEL "26" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-27 
     LABEL "27" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-28 
     LABEL "28" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-29 
     LABEL "29" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-3 
     LABEL "3" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-30 
     LABEL "30" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-31 
     LABEL "31" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-32 
     LABEL "32" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-33 
     LABEL "33" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-34 
     LABEL "34" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-35 
     LABEL "35" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-36 
     LABEL "36" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-37 
     LABEL "37" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-4 
     LABEL "4" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-5 
     LABEL "5" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-6 
     LABEL "6" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-7 
     LABEL "7" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-8 
     LABEL "8" 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-9 
     LABEL "9" 
     SIZE 4 BY 1.

DEFINE BUTTON btnClose 
     IMAGE-UP FILE "Graphics/16x16/delete.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "&Close" 
     SIZE 5 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON btnOK 
     IMAGE-UP FILE "Graphics/16x16/save.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "&OK" 
     SIZE 5 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON btnToday 
     IMAGE-UP FILE "Graphics/16x16/calendar_clock.png":U NO-FOCUS FLAT-BUTTON
     LABEL "&Today" 
     SIZE 5 BY 1.1.

DEFINE VARIABLE months AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "January","February","March","April","May","June","July","August","September","October","November","December" 
     DROP-DOWN-LIST
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE years AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     DROP-DOWN-LIST
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE sDay AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnClose AT ROW 9.33 COL 27
     btnOK AT ROW 9.33 COL 32
     btnToday AT ROW 9.33 COL 22
     months AT ROW 1.24 COL 2 HELP
          "Select Month" NO-LABEL
     sDay AT ROW 1.24 COL 16 COLON-ALIGNED NO-LABEL
     years AT ROW 1.24 COL 23 HELP
          "Select Month" NO-LABEL
     Btn-1 AT ROW 3.14 COL 2
     Btn-2 AT ROW 3.14 COL 7
     Btn-3 AT ROW 3.14 COL 12
     Btn-4 AT ROW 3.14 COL 17
     Btn-5 AT ROW 3.14 COL 22
     Btn-6 AT ROW 3.14 COL 27
     Btn-7 AT ROW 3.14 COL 32
     Btn-8 AT ROW 4.33 COL 2
     Btn-9 AT ROW 4.33 COL 7
     Btn-10 AT ROW 4.33 COL 12
     Btn-11 AT ROW 4.33 COL 17
     Btn-12 AT ROW 4.33 COL 22
     Btn-13 AT ROW 4.33 COL 27
     Btn-14 AT ROW 4.33 COL 32
     Btn-15 AT ROW 5.52 COL 2
     Btn-16 AT ROW 5.52 COL 7
     Btn-17 AT ROW 5.52 COL 12
     Btn-18 AT ROW 5.52 COL 17
     Btn-19 AT ROW 5.52 COL 22
     Btn-20 AT ROW 5.52 COL 27
     Btn-21 AT ROW 5.52 COL 32
     Btn-22 AT ROW 6.71 COL 2
     Btn-23 AT ROW 6.71 COL 7
     Btn-24 AT ROW 6.71 COL 12
     Btn-25 AT ROW 6.71 COL 17
     Btn-26 AT ROW 6.71 COL 22
     Btn-27 AT ROW 6.71 COL 27
     Btn-28 AT ROW 6.71 COL 32
     Btn-29 AT ROW 7.91 COL 2
     Btn-30 AT ROW 7.91 COL 7
     Btn-31 AT ROW 7.91 COL 12
     Btn-32 AT ROW 7.91 COL 17
     Btn-33 AT ROW 7.91 COL 22
     Btn-34 AT ROW 7.91 COL 27
     Btn-35 AT ROW 7.91 COL 32
     Btn-36 AT ROW 9.1 COL 2
     Btn-37 AT ROW 9.1 COL 7
     "S" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 2.43 COL 33
          FONT 6
     "W" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 2.43 COL 18
          FONT 6
     "M" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 2.43 COL 8
          FONT 6
     "T" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 2.43 COL 23
          FONT 6
     "F" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 2.43 COL 28
          FONT 6
     "T" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 2.43 COL 13
          FONT 6
     "S" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 2.43 COL 3
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 36.4 BY 9.48.


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
         TITLE              = "Popup Calendar"
         HEIGHT             = 9.48
         WIDTH              = 36.4
         MAX-HEIGHT         = 9.48
         MAX-WIDTH          = 36.4
         VIRTUAL-HEIGHT     = 9.48
         VIRTUAL-WIDTH      = 36.4
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = yes
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
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
/* SETTINGS FOR BUTTON Btn-1 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-1:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1".

/* SETTINGS FOR BUTTON Btn-10 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-10:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "10".

/* SETTINGS FOR BUTTON Btn-11 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-11:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "11".

/* SETTINGS FOR BUTTON Btn-12 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-12:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "12".

/* SETTINGS FOR BUTTON Btn-13 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-13:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "13".

/* SETTINGS FOR BUTTON Btn-14 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-14:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "14".

/* SETTINGS FOR BUTTON Btn-15 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-15:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "15".

/* SETTINGS FOR BUTTON Btn-16 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-16:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "16".

/* SETTINGS FOR BUTTON Btn-17 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-17:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "17".

/* SETTINGS FOR BUTTON Btn-18 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-18:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "18".

/* SETTINGS FOR BUTTON Btn-19 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-19:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "19".

/* SETTINGS FOR BUTTON Btn-2 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-2:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "2".

/* SETTINGS FOR BUTTON Btn-20 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-20:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "20".

/* SETTINGS FOR BUTTON Btn-21 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-21:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "21".

/* SETTINGS FOR BUTTON Btn-22 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-22:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "22".

/* SETTINGS FOR BUTTON Btn-23 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-23:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "23".

/* SETTINGS FOR BUTTON Btn-24 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-24:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "24".

/* SETTINGS FOR BUTTON Btn-25 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-25:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "25".

/* SETTINGS FOR BUTTON Btn-26 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-26:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "26".

/* SETTINGS FOR BUTTON Btn-27 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-27:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "27".

/* SETTINGS FOR BUTTON Btn-28 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-28:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "28".

/* SETTINGS FOR BUTTON Btn-29 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-29:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "29".

/* SETTINGS FOR BUTTON Btn-3 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-3:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "3".

/* SETTINGS FOR BUTTON Btn-30 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-30:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "30".

/* SETTINGS FOR BUTTON Btn-31 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-31:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "31".

/* SETTINGS FOR BUTTON Btn-32 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-32:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "32".

/* SETTINGS FOR BUTTON Btn-33 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-33:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "33".

/* SETTINGS FOR BUTTON Btn-34 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-34:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "34".

/* SETTINGS FOR BUTTON Btn-35 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-35:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "35".

/* SETTINGS FOR BUTTON Btn-36 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-36:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "36".

/* SETTINGS FOR BUTTON Btn-37 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-37:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "37".

/* SETTINGS FOR BUTTON Btn-4 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-4:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "4".

/* SETTINGS FOR BUTTON Btn-5 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-5:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "5".

/* SETTINGS FOR BUTTON Btn-6 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-6:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "6".

/* SETTINGS FOR BUTTON Btn-7 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-7:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "7".

/* SETTINGS FOR BUTTON Btn-8 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-8:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "8".

/* SETTINGS FOR BUTTON Btn-9 IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       Btn-9:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "9".

/* SETTINGS FOR COMBO-BOX months IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX years IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Popup Calendar */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Popup Calendar */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-1 C-Win
ON CHOOSE OF Btn-1 IN FRAME DEFAULT-FRAME /* 1 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-10 C-Win
ON CHOOSE OF Btn-10 IN FRAME DEFAULT-FRAME /* 10 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-11 C-Win
ON CHOOSE OF Btn-11 IN FRAME DEFAULT-FRAME /* 11 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-12 C-Win
ON CHOOSE OF Btn-12 IN FRAME DEFAULT-FRAME /* 12 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-13 C-Win
ON CHOOSE OF Btn-13 IN FRAME DEFAULT-FRAME /* 13 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-14 C-Win
ON CHOOSE OF Btn-14 IN FRAME DEFAULT-FRAME /* 14 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-15 C-Win
ON CHOOSE OF Btn-15 IN FRAME DEFAULT-FRAME /* 15 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-16 C-Win
ON CHOOSE OF Btn-16 IN FRAME DEFAULT-FRAME /* 16 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-17 C-Win
ON CHOOSE OF Btn-17 IN FRAME DEFAULT-FRAME /* 17 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-18 C-Win
ON CHOOSE OF Btn-18 IN FRAME DEFAULT-FRAME /* 18 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-19 C-Win
ON CHOOSE OF Btn-19 IN FRAME DEFAULT-FRAME /* 19 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-2 C-Win
ON CHOOSE OF Btn-2 IN FRAME DEFAULT-FRAME /* 2 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-20 C-Win
ON CHOOSE OF Btn-20 IN FRAME DEFAULT-FRAME /* 20 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-21 C-Win
ON CHOOSE OF Btn-21 IN FRAME DEFAULT-FRAME /* 21 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-22
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-22 C-Win
ON CHOOSE OF Btn-22 IN FRAME DEFAULT-FRAME /* 22 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-23 C-Win
ON CHOOSE OF Btn-23 IN FRAME DEFAULT-FRAME /* 23 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-24
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-24 C-Win
ON CHOOSE OF Btn-24 IN FRAME DEFAULT-FRAME /* 24 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-25
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-25 C-Win
ON CHOOSE OF Btn-25 IN FRAME DEFAULT-FRAME /* 25 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-26
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-26 C-Win
ON CHOOSE OF Btn-26 IN FRAME DEFAULT-FRAME /* 26 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-27
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-27 C-Win
ON CHOOSE OF Btn-27 IN FRAME DEFAULT-FRAME /* 27 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-28
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-28 C-Win
ON CHOOSE OF Btn-28 IN FRAME DEFAULT-FRAME /* 28 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-29
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-29 C-Win
ON CHOOSE OF Btn-29 IN FRAME DEFAULT-FRAME /* 29 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-3 C-Win
ON CHOOSE OF Btn-3 IN FRAME DEFAULT-FRAME /* 3 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-30
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-30 C-Win
ON CHOOSE OF Btn-30 IN FRAME DEFAULT-FRAME /* 30 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-31
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-31 C-Win
ON CHOOSE OF Btn-31 IN FRAME DEFAULT-FRAME /* 31 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-32
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-32 C-Win
ON CHOOSE OF Btn-32 IN FRAME DEFAULT-FRAME /* 32 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-33
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-33 C-Win
ON CHOOSE OF Btn-33 IN FRAME DEFAULT-FRAME /* 33 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-34
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-34 C-Win
ON CHOOSE OF Btn-34 IN FRAME DEFAULT-FRAME /* 34 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-35
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-35 C-Win
ON CHOOSE OF Btn-35 IN FRAME DEFAULT-FRAME /* 35 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-36
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-36 C-Win
ON CHOOSE OF Btn-36 IN FRAME DEFAULT-FRAME /* 36 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-37
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-37 C-Win
ON CHOOSE OF Btn-37 IN FRAME DEFAULT-FRAME /* 37 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-4 C-Win
ON CHOOSE OF Btn-4 IN FRAME DEFAULT-FRAME /* 4 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-5 C-Win
ON CHOOSE OF Btn-5 IN FRAME DEFAULT-FRAME /* 5 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-6 C-Win
ON CHOOSE OF Btn-6 IN FRAME DEFAULT-FRAME /* 6 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-7 C-Win
ON CHOOSE OF Btn-7 IN FRAME DEFAULT-FRAME /* 7 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-8 C-Win
ON CHOOSE OF Btn-8 IN FRAME DEFAULT-FRAME /* 8 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-9 C-Win
ON CHOOSE OF Btn-9 IN FRAME DEFAULT-FRAME /* 9 */
DO:
  RUN daySelected(SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME DEFAULT-FRAME /* Close */
DO:
  calendarDate = ''.
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK C-Win
ON CHOOSE OF btnOK IN FRAME DEFAULT-FRAME /* OK */
DO:
  calendarDate = STRING(DATE(months:LOOKUP(months:SCREEN-VALUE),INTEGER(sDay:SCREEN-VALUE),INTEGER(years:SCREEN-VALUE))).
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnToday
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnToday C-Win
ON CHOOSE OF btnToday IN FRAME DEFAULT-FRAME /* Today */
DO:
  ASSIGN
    months:SCREEN-VALUE = months:ENTRY(MONTH(TODAY))
    sDay:SCREEN-VALUE = STRING(DAY(TODAY))
    years:SCREEN-VALUE = STRING(YEAR(TODAY)).
  RUN setDays.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME months
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL months C-Win
ON VALUE-CHANGED OF months IN FRAME DEFAULT-FRAME
DO:
  RUN setDays.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sDay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sDay C-Win
ON LEAVE OF sDay IN FRAME DEFAULT-FRAME
DO:
  SELF:SCREEN-VALUE = STRING(iDay).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME years
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL years C-Win
ON VALUE-CHANGED OF years IN FRAME DEFAULT-FRAME
DO:
  RUN setDays.
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
  dateValue = DATE(FRAME-VALUE) NO-ERROR.
  IF dateValue EQ ? THEN
  dateValue = TODAY.
  DO WITH FRAME {&FRAME-NAME}:
    DO i = 1900 TO 3000:
      years:ADD-LAST(STRING(i)).
    END.
    ASSIGN
      months:SCREEN-VALUE = months:ENTRY(MONTH(dateValue))
      years:SCREEN-VALUE = STRING(YEAR(dateValue))
      iDay = DAY(dateValue)
      sDay:SCREEN-VALUE = STRING(iDay).
  END.
  RUN setDays.
  ldummy = SESSION:SET-WAIT-STATE('').
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE daySelected C-Win 
PROCEDURE daySelected :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER currentWidget AS WIDGET-HANDLE NO-UNDO.

  ASSIGN
    iDay = INTEGER(currentWidget:LABEL)
    sDay:SCREEN-VALUE IN FRAME {&FRAME-NAME} = currentWidget:LABEL.
  ASSIGN
    currentWidget = FRAME {&FRAME-NAME}:HANDLE
    currentWidget = currentWidget:FIRST-CHILD
    currentWidget = currentWidget:FIRST-CHILD.
  DO WHILE currentWidget NE ?:
    IF currentWidget:TYPE EQ 'BUTTON' THEN
    DO:
      IF currentWidget:PRIVATE-DATA NE ? AND
         currentWidget:FONT NE 2 THEN
      ASSIGN
        currentWidget:FONT = 2
        currentWidget:WIDTH-PIXELS = 20
        currentWidget:HEIGHT-PIXELS = 21
        currentWidget:X = currentWidget:X + 5
        currentWidget:Y = currentWidget:Y + 5.
      IF currentWidget:LABEL EQ STRING(iDay) THEN
      ASSIGN
        currentWidget:FONT = 6
        currentWidget:X = currentWidget:X - 5
        currentWidget:Y = currentWidget:Y - 5
        currentWidget:WIDTH-PIXELS = 30
        currentWidget:HEIGHT-PIXELS = 30.
    END.
    currentWidget = currentWidget:NEXT-SIBLING.
  END.
  APPLY 'ENTRY' TO btnOK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY months sDay years 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnClose btnOK btnToday months sDay years Btn-1 Btn-2 Btn-3 Btn-4 
         Btn-5 Btn-6 Btn-7 Btn-8 Btn-9 Btn-10 Btn-11 Btn-12 Btn-13 Btn-14 
         Btn-15 Btn-16 Btn-17 Btn-18 Btn-19 Btn-20 Btn-21 Btn-22 Btn-23 Btn-24 
         Btn-25 Btn-26 Btn-27 Btn-28 Btn-29 Btn-30 Btn-31 Btn-32 Btn-33 Btn-34 
         Btn-35 Btn-36 Btn-37 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setDays C-Win 
PROCEDURE setDays :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentWidget AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE firstDay AS INTEGER NO-UNDO.
  DEFINE VARIABLE lastDay AS INTEGER NO-UNDO.
  DEFINE VARIABLE sDate AS DATE NO-UNDO.
  DEFINE VARIABLE dDate AS DATE NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    sDate = DATE(months:LOOKUP(months:SCREEN-VALUE),1,INTEGER(years:SCREEN-VALUE)).
  END.
  dDate = sDate.
  DO WHILE TRUE:
    dDate = dDate + 1.
    IF MONTH(dDate) NE MONTH(sDate) THEN
    LEAVE.
  END.
  ASSIGN
    dDate = dDate - 1
    firstDay = WEEKDAY(sDate)
    lastDay = DAY(dDate)
    days = ''.
  IF iDay GT lastDay THEN
  iDay = lastDay.
  DO i = firstDay TO firstDay + lastDay - 1:
    ASSIGN
      j = j + 1
      days[i] = STRING(j).
  END.
  ASSIGN
    currentWidget = FRAME {&FRAME-NAME}:HANDLE
    currentWidget = currentWidget:FIRST-CHILD
    currentWidget = currentWidget:FIRST-CHILD.
  DO WHILE currentWidget NE ?:
    IF currentWidget:TYPE EQ 'BUTTON' THEN
    DO:
      IF currentWidget:FONT EQ 6 THEN
      ASSIGN
        currentWidget:WIDTH-PIXELS = 20
        currentWidget:HEIGHT-PIXELS = 21
        currentWidget:X = currentWidget:X + 5
        currentWidget:Y = currentWidget:Y + 5.
      IF currentWidget:PRIVATE-DATA NE ? THEN
      ASSIGN
        currentWidget:LABEL = days[INTEGER(currentWidget:PRIVATE-DATA)]
        currentWidget:HIDDEN = NO
        currentWidget:SENSITIVE = YES
        currentWidget:FONT = 2.
      IF currentWidget:LABEL EQ STRING(iDay) THEN
      ASSIGN
        currentWidget:FONT = 6
        currentWidget:X = currentWidget:X - 5
        currentWidget:Y = currentWidget:Y - 5
        currentWidget:WIDTH-PIXELS = 30
        currentWidget:HEIGHT-PIXELS = 30.
      IF currentWidget:LABEL EQ '' THEN
      currentWidget:HIDDEN = YES.
    END.
    currentWidget = currentWidget:NEXT-SIBLING.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

