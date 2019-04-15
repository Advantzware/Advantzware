&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WINDOW-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File:              enhance.w

  Description:       font & color settings for programs & user parameters

  Input Parameters:  <none>

  Output Parameters: <none>

  Author:            Ron Stark

  Created:           04/06/96 - 11:57 am

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
DEFINE INPUT PARAMETER prgm-name AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE prgm-name AS CHARACTER INITIAL "prgrms." NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE w-bgc AS INTEGER EXTENT 13 NO-UNDO.
DEFINE VARIABLE w-fgc AS INTEGER EXTENT 13 NO-UNDO.
DEFINE VARIABLE w-font AS INTEGER EXTENT 13 NO-UNDO.
DEFINE VARIABLE list-pos AS INTEGER NO-UNDO.
DEFINE VARIABLE k AS INTEGER NO-UNDO.

{methods/defines/hndldefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-9 RECT-4 RECT-8 color-0 color-1 color-2 ~
color-3 color-5 color-4 color-6 color-7 color-8 color-9 color-10 color-11 ~
color-12 color-13 color-14 color-15 color-rect color-16 color-17 color-24 ~
color-25 color-26 color-27 color-28 color-29 color-30 color-31 color-18 ~
color-19 color-20 color-21 color-22 color-23 RECT-10 widget-list ~
widget-fonts use_colors use_fonts Btn_Reset_Font Btn_Reset_BGCOLOR ~
Btn_Reset_FGCOLOR Btn_Save Btn_Reset Btn_Cancel Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS widget-list widget-fonts use_colors ~
use_fonts m-example 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 color-0 color-1 color-2 color-3 color-5 color-4 ~
color-6 color-7 color-8 color-9 color-10 color-11 color-12 color-13 ~
color-14 color-15 color-16 color-17 color-24 color-25 color-26 color-27 ~
color-28 color-29 color-30 color-31 color-18 color-19 color-20 color-21 ~
color-22 color-23 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY DEFAULT 
     LABEL "&Cancel" 
     SIZE 14 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_OK AUTO-END-KEY DEFAULT 
     LABEL "&OK" 
     SIZE 14 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_Reset 
     LABEL "&Reset" 
     SIZE 14 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Reset_BGCOLOR 
     LABEL "Reset Bac&kground Color" 
     SIZE 30.8 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Reset_FGCOLOR 
     LABEL "Reset For&eground Color" 
     SIZE 30.8 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Reset_Font 
     LABEL "Reset &System Font" 
     SIZE 30.8 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Save 
     LABEL "&Save" 
     SIZE 14 BY 1.24
     FONT 4.

DEFINE VARIABLE m-example AS CHARACTER FORMAT "X(256)":U INITIAL "Font Text" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 3 NO-UNDO.

DEFINE RECTANGLE color-0
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 0 .

DEFINE RECTANGLE color-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 1 .

DEFINE RECTANGLE color-10
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 10 .

DEFINE RECTANGLE color-11
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 11 .

DEFINE RECTANGLE color-12
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 12 .

DEFINE RECTANGLE color-13
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 13 .

DEFINE RECTANGLE color-14
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 14 .

DEFINE RECTANGLE color-15
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 15 .

DEFINE RECTANGLE color-16
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 0 .

DEFINE RECTANGLE color-17
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 1 .

DEFINE RECTANGLE color-18
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 10 .

DEFINE RECTANGLE color-19
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 11 .

DEFINE RECTANGLE color-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 2 .

DEFINE RECTANGLE color-20
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 12 .

DEFINE RECTANGLE color-21
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 13 .

DEFINE RECTANGLE color-22
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 14 .

DEFINE RECTANGLE color-23
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 15 .

DEFINE RECTANGLE color-24
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 2 .

DEFINE RECTANGLE color-25
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 3 .

DEFINE RECTANGLE color-26
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 4 .

DEFINE RECTANGLE color-27
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 5 .

DEFINE RECTANGLE color-28
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 6 .

DEFINE RECTANGLE color-29
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 7 .

DEFINE RECTANGLE color-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 3 .

DEFINE RECTANGLE color-30
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 8 .

DEFINE RECTANGLE color-31
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 9 .

DEFINE RECTANGLE color-4
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 4 .

DEFINE RECTANGLE color-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 5 .

DEFINE RECTANGLE color-6
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 6 .

DEFINE RECTANGLE color-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 7 .

DEFINE RECTANGLE color-8
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 8 .

DEFINE RECTANGLE color-9
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 5.6 BY 1.24
     BGCOLOR 9 .

DEFINE RECTANGLE color-rect
     EDGE-PIXELS 8  NO-FILL   
     SIZE 148.4 BY 2.67.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 8  NO-FILL   
     SIZE 72.8 BY 2.62.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 8  NO-FILL   
     SIZE 33.6 BY 4.38.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 8  NO-FILL   
     SIZE 148.4 BY 2.48.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 8  NO-FILL   
     SIZE 33.6 BY 4.38.

DEFINE VARIABLE widget-fonts AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 92.4 BY 10.57 NO-UNDO.

DEFINE VARIABLE widget-list AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE 
     LIST-ITEMS "Browser","Button","Combo-Box","Dialog-Box","Editor","Fill-In","Frame","Literal","Radio-Set","Selection-List","Rectangle","Text","Toggle-Box" 
     SIZE 21 BY 10.57 NO-UNDO.

DEFINE VARIABLE use_colors AS LOGICAL INITIAL no 
     LABEL "Use Co&lors" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.6 BY 1.24 NO-UNDO.

DEFINE VARIABLE use_fonts AS LOGICAL INITIAL no 
     LABEL "Use Fo&nts" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.8 BY 1.24 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     widget-list AT ROW 2.24 COL 2.4 HELP
          "Select Widget Object" NO-LABEL
     widget-fonts AT ROW 2.24 COL 24.8 HELP
          "Select Font Setting" NO-LABEL
     use_colors AT ROW 3 COL 124.2 HELP
          "Use Color Settings Indicator"
     use_fonts AT ROW 4.67 COL 124.2 HELP
          "Use Fonts Settings Indicator"
     m-example AT ROW 7.38 COL 118 COLON-ALIGNED NO-LABEL
     Btn_Reset_Font AT ROW 11.33 COL 118.6
     Btn_Reset_BGCOLOR AT ROW 13.38 COL 118.2
     Btn_Reset_FGCOLOR AT ROW 18.14 COL 118.2
     Btn_Save AT ROW 23.67 COL 44.4 HELP
          "Use this function to SAVE selected settings"
     Btn_Reset AT ROW 23.67 COL 61.2 HELP
          "Use this function to RESET settings"
     Btn_Cancel AT ROW 23.67 COL 78 HELP
          "Use this function to CANCEL/CLOSE"
     Btn_OK AT ROW 23.67 COL 94.8 HELP
          "Use this function to SAVE selected settings and EXIT"
     "Available Background Colors" VIEW-AS TEXT
          SIZE 37.8 BY 1.24 AT ROW 13.76 COL 2
     "Available Fonts" VIEW-AS TEXT
          SIZE 21 BY 1.24 AT ROW 1 COL 27.6
     "Widget Objects" VIEW-AS TEXT
          SIZE 21 BY 1.24 AT ROW 1 COL 2.4
     "Available Foreground Colors" VIEW-AS TEXT
          SIZE 37.8 BY 1.24 AT ROW 18.48 COL 2
     RECT-9 AT ROW 2.38 COL 117.2
     RECT-4 AT ROW 6.67 COL 117.2
     RECT-8 AT ROW 15.05 COL 2.4
     color-0 AT ROW 15.67 COL 10.8
     color-1 AT ROW 15.67 COL 19.2
     color-2 AT ROW 15.67 COL 27.6
     color-3 AT ROW 15.67 COL 36
     color-5 AT ROW 15.67 COL 44.4
     color-4 AT ROW 15.67 COL 52.8
     color-6 AT ROW 15.67 COL 61.2
     color-7 AT ROW 15.67 COL 69.6
     color-8 AT ROW 15.67 COL 78
     color-9 AT ROW 15.67 COL 86.4
     color-10 AT ROW 15.67 COL 94.8
     color-11 AT ROW 15.67 COL 103.2
     color-12 AT ROW 15.67 COL 111.6
     color-13 AT ROW 15.67 COL 120
     color-14 AT ROW 15.67 COL 128.4
     color-15 AT ROW 15.67 COL 136.8
     color-rect AT ROW 19.71 COL 2.4
     color-16 AT ROW 20.33 COL 10.8
     color-17 AT ROW 20.33 COL 19.2
     color-24 AT ROW 20.33 COL 27.6
     color-25 AT ROW 20.33 COL 36
     color-26 AT ROW 20.33 COL 44.4
     color-27 AT ROW 20.33 COL 52.8
     color-28 AT ROW 20.33 COL 61.2
     color-29 AT ROW 20.33 COL 69.6
     color-30 AT ROW 20.33 COL 78
     color-31 AT ROW 20.33 COL 86.4
     color-18 AT ROW 20.33 COL 94.8
     color-19 AT ROW 20.33 COL 103.2
     color-20 AT ROW 20.33 COL 111.6
     color-21 AT ROW 20.33 COL 120
     color-22 AT ROW 20.33 COL 128.4
     color-23 AT ROW 20.33 COL 136.8
     RECT-10 AT ROW 23 COL 40.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149.8 BY 24.76
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "User Color and Font Settings"
         HEIGHT             = 24.76
         WIDTH              = 149.8
         MAX-HEIGHT         = 24.76
         MAX-WIDTH          = 155.6
         VIRTUAL-HEIGHT     = 24.76
         VIRTUAL-WIDTH      = 155.6
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT WINDOW-1:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-1
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* SETTINGS FOR RECTANGLE color-0 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-1 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-10 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-11 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-12 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-13 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-14 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-15 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-16 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-17 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-18 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-19 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-2 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-20 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-21 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-22 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-23 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-24 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-25 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-26 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-27 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-28 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-29 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-3 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-30 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-31 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-4 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-5 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-6 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-7 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-8 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR RECTANGLE color-9 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR FILL-IN m-example IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK WINDOW-1
ON CHOOSE OF Btn_OK IN FRAME FRAME-A /* OK */
DO:
  APPLY "CHOOSE" TO Btn_Save.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Reset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Reset WINDOW-1
ON CHOOSE OF Btn_Reset IN FRAME FRAME-A /* Reset */
DO:
  IF prgm-name = "" THEN
  DO:
    FIND users WHERE users.user_id = USERID("NOSWEAT") NO-LOCK.
    DO k = 1 TO 13:
      ASSIGN
        w-bgc[k] = users.widget_bgc[k]
        w-fgc[k] = users.widget_fgc[k]
        w-font[k] = users.widget_font[k].
    END.
    ASSIGN
      use_fonts:SCREEN-VALUE = STRING(users.use_fonts)
      use_colors:SCREEN-VALUE = STRING(users.use_colors).
  END.
  ELSE
  DO:
    FIND prgrms WHERE prgrms.prgmname = prgm-name NO-LOCK.
    DO k = 1 TO 13:
      ASSIGN
        w-bgc[k] = prgrms.widget_bgc[k]
        w-fgc[k] = prgrms.widget_fgc[k]
        w-font[k] = prgrms.widget_font[k].
    END.
    ASSIGN
      use_fonts:SCREEN-VALUE = STRING(prgrms.use_fonts)
      use_colors:SCREEN-VALUE = STRING(prgrms.use_colors).
  END.
  widget-list:SCREEN-VALUE = widget-list:ENTRY(1).
  APPLY "VALUE-CHANGED" TO widget-list.
  APPLY "ENTRY" TO widget-list.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Reset_BGCOLOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Reset_BGCOLOR WINDOW-1
ON CHOOSE OF Btn_Reset_BGCOLOR IN FRAME FRAME-A /* Reset Background Color */
DO:
  ASSIGN
    m-example:BGCOLOR = ?
    w-bgc[list-pos] = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Reset_FGCOLOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Reset_FGCOLOR WINDOW-1
ON CHOOSE OF Btn_Reset_FGCOLOR IN FRAME FRAME-A /* Reset Foreground Color */
DO:
  ASSIGN
    m-example:FGCOLOR = ?
    w-fgc[list-pos] = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Reset_Font
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Reset_Font WINDOW-1
ON CHOOSE OF Btn_Reset_Font IN FRAME FRAME-A /* Reset System Font */
DO:
  ASSIGN
    m-example:FONT = ?
    w-font[list-pos] = ?
    widget-fonts:SCREEN-VALUE = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Save WINDOW-1
ON CHOOSE OF Btn_Save IN FRAME FRAME-A /* Save */
DO:
  ASSIGN
    use_fonts
    use_colors.
  IF prgm-name = "" THEN
  DO:
    FIND users WHERE users.user_id = USERID("NOSWEAT") EXCLUSIVE-LOCK.
    DO k = 1 TO 13:
      ASSIGN
        users.widget_bgc[k] = w-bgc[k]
        users.widget_fgc[k] = w-fgc[k]
        users.widget_font[k] = w-font[k].
    END.
    ASSIGN
      users.use_fonts = use_fonts
      users.use_colors = use_colors.
  END.
  ELSE
  DO:
    FIND prgrms WHERE prgrms.prgmname = prgm-name EXCLUSIVE-LOCK.
    DO k = 1 TO 13:
      ASSIGN
        prgrms.widget_bgc[k] = w-bgc[k]
        prgrms.widget_fgc[k] = w-fgc[k]
        prgrms.widget_font[k] = w-font[k].
    END.
    ASSIGN
      prgrms.use_fonts = use_fonts
      prgrms.use_colors = use_colors.
  END.
  Btn_Cancel:LABEL = "&Close".
  DISABLE Btn_Reset WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-0 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-0 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-1 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-1 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-10 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-10 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-11 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-11 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-12 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-12 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-13 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-13 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-14 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-14 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-15 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-15 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-16 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-16 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-17 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-17 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-18 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-18 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-19 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-19 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-2 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-2 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-20 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-20 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-21 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-21 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-22
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-22 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-22 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-23 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-23 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-24
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-24 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-24 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-25
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-25 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-25 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-26
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-26 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-26 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-27
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-27 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-27 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-28
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-28 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-28 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-29
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-29 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-29 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-3 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-3 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-30
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-30 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-30 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-31
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-31 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-31 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-4 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-4 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-5 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-5 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-6 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-6 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-7 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-7 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-8 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-8 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME color-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL color-9 WINDOW-1
ON MOUSE-SELECT-CLICK OF color-9 IN FRAME FRAME-A
DO:
  RUN Set_Color ({&SELF-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME widget-fonts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL widget-fonts WINDOW-1
ON VALUE-CHANGED OF widget-fonts IN FRAME FRAME-A
DO:
  ASSIGN
    m-example:FONT = INT(SUBSTR(
            widget-fonts:SCREEN-VALUE,1,INDEX(widget-fonts:SCREEN-VALUE,",") - 1))
    w-font[list-pos] = m-example:FONT.
  ENABLE Btn_Reset WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME widget-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL widget-list WINDOW-1
ON VALUE-CHANGED OF widget-list IN FRAME FRAME-A
DO:
  ASSIGN
    list-pos = LOOKUP({&SELF-NAME}:SCREEN-VALUE,{&SELF-NAME}:LIST-ITEMS)
    m-example:BGCOLOR = w-bgc[list-pos]
    m-example:FGCOLOR = w-fgc[list-pos]
    m-example:FONT = w-font[list-pos]
    widget-fonts:SCREEN-VALUE = widget-fonts:ENTRY(w-font[list-pos] + 1).
  ENABLE widget-fonts {&LIST-1} WITH FRAME {&FRAME-NAME}.
  CASE {&SELF-NAME}:SCREEN-VALUE:
    WHEN "Browser" THEN
    DO:
      DISABLE widget-fonts WITH FRAME {&FRAME-NAME}.
      m-example:FONT = ?.
    END.
    WHEN "Button" THEN
    DO:
      DISABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
      ASSIGN
        m-example:BGCOLOR = ?
        m-example:FGCOLOR = ?.
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  ASSIGN 
    {&WINDOW-NAME}:TITLE = IF prgm-name = "" THEN "User '" + USERID("NOSWEAT")
                                ELSE "Program '" + prgm-name
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + "' Font and Color Settings".
  widget-fonts:DELIMITER = "@".
  RUN enable_UI.
  RUN Get_Fonts.
  APPLY "CHOOSE" TO Btn_Reset.
  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  DISPLAY widget-list widget-fonts use_colors use_fonts m-example 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-9 RECT-4 RECT-8 color-0 color-1 color-2 color-3 color-5 color-4 
         color-6 color-7 color-8 color-9 color-10 color-11 color-12 color-13 
         color-14 color-15 color-rect color-16 color-17 color-24 color-25 
         color-26 color-27 color-28 color-29 color-30 color-31 color-18 
         color-19 color-20 color-21 color-22 color-23 RECT-10 widget-list 
         widget-fonts use_colors use_fonts Btn_Reset_Font Btn_Reset_BGCOLOR 
         Btn_Reset_FGCOLOR Btn_Save Btn_Reset Btn_Cancel Btn_OK 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW WINDOW-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Fonts WINDOW-1 
PROCEDURE Get_Fonts :
/* -----------------------------------------------------------
  Purpose:     find number of available fonts & load font selection list
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
  DEFINE VARIABLE x AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  REPEAT WHILE x NE ?:
    GET-KEY-VALUE SECTION "fonts" KEY "font" + STRING(i) VALUE x.
    i = i + 1.
  END.

  FONT-TABLE:NUM-ENTRIES = i - 1.

  DO i = 0 TO FONT-TABLE:NUM-ENTRIES - 1 WITH FRAME {&FRAME-NAME}:
    GET-KEY-VALUE SECTION "fonts" KEY "font" + STRING(i) VALUE x.
    ldummy = widget-fonts:ADD-LAST(STRING(i) + ", " + x).
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus WINDOW-1 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     Set Focus done by calling program
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/setfocus.i Btn_Cancel}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set_Color WINDOW-1 
PROCEDURE Set_Color :
/* -----------------------------------------------------------
  Purpose:     Set color for selected widget
  Parameters:  Color Widget Handle
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER color-widget AS WIDGET-HANDLE NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF INT(SUBSTR(color-widget:NAME,R-INDEX(color-widget:NAME,"-") + 1)) LT 16 THEN
    ASSIGN
      m-example:BGCOLOR = color-widget:BGCOLOR
      w-bgc[list-pos] = color-widget:BGCOLOR.
    ELSE
    ASSIGN
      m-example:FGCOLOR = color-widget:BGCOLOR
      w-fgc[list-pos] = color-widget:BGCOLOR.
    ENABLE Btn_Reset.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

