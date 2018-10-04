&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: touch/keyboard.w

  Description: Alphabetic Keyboard

  Input Parameters: Window Handle of Calling Procedure.

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 4.6.2000

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

&IF DEFINED(UIB_is_Running) NE 0 &THEN
DEFINE VARIABLE h_calling_window AS HANDLE NO-UNDO.
DEFINE VARIABLE h_touchfrm AS HANDLE NO-UNDO.
DEFINE VARIABLE parents_row AS DECIMAL NO-UNDO.
&ELSE
DEFINE INPUT PARAMETER h_calling_window AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER h_touchfrm AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER parents_row AS DECIMAL NO-UNDO.
&ENDIF  

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
DO TRANSACTION:
   {sys/inc/tskey.i}
END.

IF NOT tskey-log THEN
   RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Exclaim Btn_Atsign Btn_Pound Btn_Dollar ~
Btn_Percent Btn_Carot Btn_And Btn_Asterisk Btn_lparenth Btn_rparenth ~
Btn_lcurly Btn_rcurly Btn_underln Btn_Pipe Btn_Lbox Btn_Q Btn_W Btn_E Btn_R ~
Btn_T Btn_Y Btn_U Btn_I Btn_O Btn_P Btn_Minus Btn_Plus Btn_Equal ~
Btn_Lessthan Btn_A Btn_S Btn_D Btn_F Btn_G Btn_H Btn_J Btn_K Btn_L ~
Btn_Colon Btn_semicoln Btn_Dblquote Btn_Sglquote Btn_rbox Btn_Grtrthan ~
Btn_Z Btn_X Btn_C Btn_V Btn_B Btn_N Btn_M Btn_Comma Btn_Period Btn_fwdslash ~
Btn_bckslash Btn_Question Btn_Caps_Lock Btn_Clear Btn_Space Btn_Backspace ~
Btn_Sort Btn_Alpha 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_A 
     IMAGE-UP FILE "keyboard\a":U
     LABEL "A" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Alpha 
     LABEL "ALPHA" 
     SIZE 17 BY 1.91 TOOLTIP "ALPHA"
     FONT 6.

DEFINE BUTTON Btn_And 
     IMAGE-UP FILE "keyboard\and":U
     LABEL "&" 
     SIZE 11 BY 2.38 TOOLTIP "Ampersand".

DEFINE BUTTON Btn_Asterisk 
     IMAGE-UP FILE "keyboard\asterisk":U
     LABEL "*" 
     SIZE 11 BY 2.38 TOOLTIP "Asterisk".

DEFINE BUTTON Btn_Atsign 
     IMAGE-UP FILE "keyboard\atsign":U
     LABEL "@" 
     SIZE 11 BY 2.38 TOOLTIP "At Sign".

DEFINE BUTTON Btn_B 
     IMAGE-UP FILE "keyboard\b":U
     LABEL "B" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Backspace 
     LABEL "BACKSPACE" 
     SIZE 44 BY 1.91 TOOLTIP "BACKSPACE"
     FONT 6.

DEFINE BUTTON Btn_bckslash 
     IMAGE-UP FILE "keyboard\bckslash":U
     LABEL "~\" 
     SIZE 11 BY 2.38 TOOLTIP "Back Slash".

DEFINE BUTTON Btn_C 
     IMAGE-UP FILE "keyboard\c":U
     LABEL "C" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Caps_Lock 
     LABEL "CAPS LOCK (ON)" 
     SIZE 43 BY 1.91 TOOLTIP "CAPS LOCK"
     FONT 6.

DEFINE BUTTON Btn_Carot 
     IMAGE-UP FILE "keyboard\carot":U
     LABEL "^" 
     SIZE 11 BY 2.38 TOOLTIP "Carot".

DEFINE BUTTON Btn_Clear 
     LABEL "CLEAR" 
     SIZE 14 BY 1.91 TOOLTIP "CLEAR"
     FONT 6.

DEFINE BUTTON Btn_Colon 
     IMAGE-UP FILE "keyboard\colon":U
     LABEL ":" 
     SIZE 11 BY 2.38 TOOLTIP "Colon".

DEFINE BUTTON Btn_Comma 
     IMAGE-UP FILE "keyboard\comma":U
     LABEL "," 
     SIZE 11 BY 2.38 TOOLTIP "Comma".

DEFINE BUTTON Btn_D 
     IMAGE-UP FILE "keyboard\d":U
     LABEL "D" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Dblquote 
     IMAGE-UP FILE "keyboard\dblquote":U
     LABEL "DQ" 
     SIZE 11 BY 2.38 TOOLTIP "Semi-Colon".

DEFINE BUTTON Btn_Dollar 
     IMAGE-UP FILE "keyboard\dollar":U
     LABEL "$" 
     SIZE 11 BY 2.38 TOOLTIP "Dollar".

DEFINE BUTTON Btn_E 
     IMAGE-UP FILE "keyboard\e":U
     LABEL "E" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Equal 
     IMAGE-UP FILE "keyboard\equal":U
     LABEL "=" 
     SIZE 11 BY 2.38 TOOLTIP "Equal".

DEFINE BUTTON Btn_Exclaim 
     IMAGE-UP FILE "keyboard\exclaim":U
     LABEL "!" 
     SIZE 11 BY 2.38 TOOLTIP "Exclamition".

DEFINE BUTTON Btn_F 
     IMAGE-UP FILE "keyboard\f":U
     LABEL "F" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_fwdslash 
     IMAGE-UP FILE "keyboard\fwdslash":U
     LABEL "/" 
     SIZE 11 BY 2.38 TOOLTIP "Forward Slash".

DEFINE BUTTON Btn_G 
     IMAGE-UP FILE "keyboard\g":U
     LABEL "G" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Grtrthan 
     IMAGE-UP FILE "keyboard\grtrthan":U
     LABEL ">" 
     SIZE 11 BY 2.38 TOOLTIP "Greater Than".

DEFINE BUTTON Btn_H 
     IMAGE-UP FILE "keyboard\h":U
     LABEL "H" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_I 
     IMAGE-UP FILE "keyboard\i":U
     LABEL "I" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_J 
     IMAGE-UP FILE "keyboard\j":U
     LABEL "J" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_K 
     IMAGE-UP FILE "keyboard\k":U
     LABEL "K" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_L 
     IMAGE-UP FILE "keyboard\l":U
     LABEL "L" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Lbox 
     IMAGE-UP FILE "keyboard\lbox":U
     LABEL "[" 
     SIZE 5 BY 2.38 TOOLTIP "Left Square Bracket".

DEFINE BUTTON Btn_lcurly 
     IMAGE-UP FILE "keyboard\lcurly":U
     LABEL "~{" 
     SIZE 11 BY 2.38 TOOLTIP "Left Curly Bracket".

DEFINE BUTTON Btn_Lessthan 
     IMAGE-UP FILE "keyboard\lessthan":U
     LABEL "<" 
     SIZE 11 BY 2.38 TOOLTIP "Less Than".

DEFINE BUTTON Btn_lparenth 
     IMAGE-UP FILE "keyboard\lparenth":U
     LABEL "(" 
     SIZE 11 BY 2.38 TOOLTIP "Left Parenthesis".

DEFINE BUTTON Btn_M 
     IMAGE-UP FILE "keyboard\m":U
     LABEL "M" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Minus 
     IMAGE-UP FILE "keyboard\minus":U
     LABEL "-" 
     SIZE 11 BY 2.38 TOOLTIP "Minus".

DEFINE BUTTON Btn_N 
     IMAGE-UP FILE "keyboard\n":U
     LABEL "N" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_O 
     IMAGE-UP FILE "keyboard\o":U
     LABEL "O" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_P 
     IMAGE-UP FILE "keyboard\p":U
     LABEL "P" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Percent 
     IMAGE-UP FILE "keyboard\percent":U
     LABEL "%" 
     SIZE 11 BY 2.38 TOOLTIP "Percent".

DEFINE BUTTON Btn_Period 
     IMAGE-UP FILE "keyboard\period":U
     LABEL "." 
     SIZE 11 BY 2.38 TOOLTIP "Period".

DEFINE BUTTON Btn_Pipe 
     IMAGE-UP FILE "keyboard\pipe":U
     LABEL "|" 
     SIZE 5 BY 2.38 TOOLTIP "Pipe".

DEFINE BUTTON Btn_Plus 
     IMAGE-UP FILE "keyboard\plus":U
     LABEL "+" 
     SIZE 11 BY 2.38 TOOLTIP "Plus".

DEFINE BUTTON Btn_Pound 
     IMAGE-UP FILE "keyboard\pound":U
     LABEL "#" 
     SIZE 11 BY 2.38 TOOLTIP "Pound".

DEFINE BUTTON Btn_Q 
     IMAGE-UP FILE "keyboard\q":U
     LABEL "Q" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Question 
     IMAGE-UP FILE "keyboard\question":U
     LABEL "?" 
     SIZE 11 BY 2.38 TOOLTIP "Question Mark".

DEFINE BUTTON Btn_R 
     IMAGE-UP FILE "keyboard\r":U
     LABEL "R" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_rbox 
     IMAGE-UP FILE "keyboard\rbox":U
     LABEL "]" 
     SIZE 5 BY 2.38 TOOLTIP "Right Square Bracket".

DEFINE BUTTON Btn_rcurly 
     IMAGE-UP FILE "keyboard\rcurly":U
     LABEL "}" 
     SIZE 11 BY 2.38 TOOLTIP "Right Curly Bracket".

DEFINE BUTTON Btn_rparenth 
     IMAGE-UP FILE "keyboard\rparenth":U
     LABEL ")" 
     SIZE 11 BY 2.38 TOOLTIP "Right Parenthesis".

DEFINE BUTTON Btn_S 
     IMAGE-UP FILE "keyboard\s":U
     LABEL "S" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_semicoln 
     IMAGE-UP FILE "keyboard\semicoln":U
     LABEL "~;" 
     SIZE 11 BY 2.38 TOOLTIP "Semi-Colon".

DEFINE BUTTON Btn_Sglquote 
     IMAGE-UP FILE "keyboard\sglquote":U
     LABEL "'" 
     SIZE 5 BY 2.38 TOOLTIP "Single Quote".

DEFINE BUTTON Btn_Sort 
     LABEL "SORT" 
     SIZE 16 BY 1.91 TOOLTIP "SORT"
     FONT 6.

DEFINE BUTTON Btn_Space 
     LABEL "SPACE" 
     SIZE 14 BY 1.91 TOOLTIP "SPACE"
     FONT 6.

DEFINE BUTTON Btn_T 
     IMAGE-UP FILE "keyboard\t":U
     LABEL "T" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_U 
     IMAGE-UP FILE "keyboard\u":U
     LABEL "U" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_underln 
     IMAGE-UP FILE "keyboard\underln":U
     LABEL "_" 
     SIZE 11 BY 2.38 TOOLTIP "Underscore".

DEFINE BUTTON Btn_V 
     IMAGE-UP FILE "keyboard\v":U
     LABEL "V" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_W 
     IMAGE-UP FILE "keyboard\w":U
     LABEL "W" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_X 
     IMAGE-UP FILE "keyboard\x":U
     LABEL "X" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Y 
     IMAGE-UP FILE "keyboard\y":U
     LABEL "Y" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Z 
     IMAGE-UP FILE "keyboard\z":U
     LABEL "Z" 
     SIZE 11 BY 2.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_Exclaim AT ROW 1 COL 1
     Btn_Atsign AT ROW 1 COL 12
     Btn_Pound AT ROW 1 COL 23
     Btn_Dollar AT ROW 1 COL 34
     Btn_Percent AT ROW 1 COL 45
     Btn_Carot AT ROW 1 COL 56
     Btn_And AT ROW 1 COL 67
     Btn_Asterisk AT ROW 1 COL 78
     Btn_lparenth AT ROW 1 COL 89
     Btn_rparenth AT ROW 1 COL 100
     Btn_lcurly AT ROW 1 COL 111
     Btn_rcurly AT ROW 1 COL 122
     Btn_underln AT ROW 1 COL 133
     Btn_Pipe AT ROW 1 COL 144
     Btn_Lbox AT ROW 3.38 COL 1
     Btn_Q AT ROW 3.38 COL 6
     Btn_W AT ROW 3.38 COL 17
     Btn_E AT ROW 3.38 COL 28
     Btn_R AT ROW 3.38 COL 39
     Btn_T AT ROW 3.38 COL 50
     Btn_Y AT ROW 3.38 COL 61
     Btn_U AT ROW 3.38 COL 72
     Btn_I AT ROW 3.38 COL 83
     Btn_O AT ROW 3.38 COL 94
     Btn_P AT ROW 3.38 COL 105
     Btn_Minus AT ROW 3.38 COL 116
     Btn_Plus AT ROW 3.38 COL 127
     Btn_Equal AT ROW 3.38 COL 138
     Btn_Lessthan AT ROW 5.76 COL 1
     Btn_A AT ROW 5.76 COL 12
     Btn_S AT ROW 5.76 COL 23
     Btn_D AT ROW 5.76 COL 34
     Btn_F AT ROW 5.76 COL 45
     Btn_G AT ROW 5.76 COL 56
     Btn_H AT ROW 5.76 COL 67
     Btn_J AT ROW 5.76 COL 78
     Btn_K AT ROW 5.76 COL 89
     Btn_L AT ROW 5.76 COL 100
     Btn_Colon AT ROW 5.76 COL 111
     Btn_semicoln AT ROW 5.76 COL 122
     Btn_Dblquote AT ROW 5.76 COL 133
     Btn_Sglquote AT ROW 5.76 COL 144
     Btn_rbox AT ROW 8.14 COL 1
     Btn_Grtrthan AT ROW 8.14 COL 6
     Btn_Z AT ROW 8.14 COL 17
     Btn_X AT ROW 8.14 COL 28
     Btn_C AT ROW 8.14 COL 39
     Btn_V AT ROW 8.14 COL 50
     Btn_B AT ROW 8.14 COL 61
     Btn_N AT ROW 8.14 COL 72
     Btn_M AT ROW 8.14 COL 83
     Btn_Comma AT ROW 8.14 COL 94
     Btn_Period AT ROW 8.14 COL 105
     Btn_fwdslash AT ROW 8.14 COL 116
     Btn_bckslash AT ROW 8.14 COL 127
     Btn_Question AT ROW 8.14 COL 138
     Btn_Caps_Lock AT ROW 10.52 COL 1
     Btn_Clear AT ROW 10.52 COL 44
     Btn_Space AT ROW 10.52 COL 58
     Btn_Backspace AT ROW 10.52 COL 72
     Btn_Sort AT ROW 10.52 COL 116
     Btn_Alpha AT ROW 10.52 COL 132
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 148.2 BY 11.48
         BGCOLOR 7 .


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
         TITLE              = "Qwerty Keyboard"
         COLUMN             = 1
         ROW                = 16.95
         HEIGHT             = 11.48
         WIDTH              = 148.2
         MAX-HEIGHT         = 11.48
         MAX-WIDTH          = 148.2
         VIRTUAL-HEIGHT     = 11.48
         VIRTUAL-WIDTH      = 148.2
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
ASSIGN 
       Btn_Alpha:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ALPHA".

ASSIGN 
       Btn_Backspace:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "BACKSPACE".

ASSIGN 
       Btn_Caps_Lock:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "CAPS LOCK".

ASSIGN 
       Btn_Clear:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "CLEAR".

ASSIGN 
       Btn_Sort:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "SORT".

ASSIGN 
       Btn_Space:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "SPACE".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Qwerty Keyboard */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Qwerty Keyboard */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_A C-Win
ON CHOOSE OF Btn_A IN FRAME DEFAULT-FRAME /* A */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Alpha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Alpha C-Win
ON CHOOSE OF Btn_Alpha IN FRAME DEFAULT-FRAME /* ALPHA */
DO:
  RUN Apply_Key (SELF:TOOLTIP).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_And
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_And C-Win
ON CHOOSE OF Btn_And IN FRAME DEFAULT-FRAME /*  */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Asterisk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Asterisk C-Win
ON CHOOSE OF Btn_Asterisk IN FRAME DEFAULT-FRAME /* * */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Atsign
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Atsign C-Win
ON CHOOSE OF Btn_Atsign IN FRAME DEFAULT-FRAME /* @ */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_B
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_B C-Win
ON CHOOSE OF Btn_B IN FRAME DEFAULT-FRAME /* B */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Backspace
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Backspace C-Win
ON CHOOSE OF Btn_Backspace IN FRAME DEFAULT-FRAME /* BACKSPACE */
DO:
  RUN Apply_Key (SELF:TOOLTIP).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_bckslash
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_bckslash C-Win
ON CHOOSE OF Btn_bckslash IN FRAME DEFAULT-FRAME /* \ */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_C
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_C C-Win
ON CHOOSE OF Btn_C IN FRAME DEFAULT-FRAME /* C */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Caps_Lock
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Caps_Lock C-Win
ON CHOOSE OF Btn_Caps_Lock IN FRAME DEFAULT-FRAME /* CAPS LOCK (ON) */
DO:
  DEFINE VARIABLE capsLock AS LOGICAL NO-UNDO.

  ASSIGN
    capsLock = NOT INDEX(SELF:LABEL,'ON') NE 0
    SELF:LABEL = SELF:TOOLTIP.
  {touch/kbLanguage.i}
  SELF:LABEL = SELF:LABEL + ' ' + STRING(capsLock,'(ON)/(OFF)').
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Carot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Carot C-Win
ON CHOOSE OF Btn_Carot IN FRAME DEFAULT-FRAME /* ^ */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Clear C-Win
ON CHOOSE OF Btn_Clear IN FRAME DEFAULT-FRAME /* CLEAR */
DO:
  RUN Apply_Key (SELF:TOOLTIP).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Colon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Colon C-Win
ON CHOOSE OF Btn_Colon IN FRAME DEFAULT-FRAME /* : */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Comma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Comma C-Win
ON CHOOSE OF Btn_Comma IN FRAME DEFAULT-FRAME /* , */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_D
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_D C-Win
ON CHOOSE OF Btn_D IN FRAME DEFAULT-FRAME /* D */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Dblquote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Dblquote C-Win
ON CHOOSE OF Btn_Dblquote IN FRAME DEFAULT-FRAME /* DQ */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Dollar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Dollar C-Win
ON CHOOSE OF Btn_Dollar IN FRAME DEFAULT-FRAME /* $ */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_E
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_E C-Win
ON CHOOSE OF Btn_E IN FRAME DEFAULT-FRAME /* E */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Equal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Equal C-Win
ON CHOOSE OF Btn_Equal IN FRAME DEFAULT-FRAME /* = */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Exclaim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Exclaim C-Win
ON CHOOSE OF Btn_Exclaim IN FRAME DEFAULT-FRAME /* ! */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_F
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_F C-Win
ON CHOOSE OF Btn_F IN FRAME DEFAULT-FRAME /* F */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_fwdslash
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_fwdslash C-Win
ON CHOOSE OF Btn_fwdslash IN FRAME DEFAULT-FRAME /* / */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_G
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_G C-Win
ON CHOOSE OF Btn_G IN FRAME DEFAULT-FRAME /* G */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Grtrthan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grtrthan C-Win
ON CHOOSE OF Btn_Grtrthan IN FRAME DEFAULT-FRAME /* > */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_H
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_H C-Win
ON CHOOSE OF Btn_H IN FRAME DEFAULT-FRAME /* H */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_I
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_I C-Win
ON CHOOSE OF Btn_I IN FRAME DEFAULT-FRAME /* I */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_J
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_J C-Win
ON CHOOSE OF Btn_J IN FRAME DEFAULT-FRAME /* J */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_K
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_K C-Win
ON CHOOSE OF Btn_K IN FRAME DEFAULT-FRAME /* K */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_L
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_L C-Win
ON CHOOSE OF Btn_L IN FRAME DEFAULT-FRAME /* L */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Lbox
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Lbox C-Win
ON CHOOSE OF Btn_Lbox IN FRAME DEFAULT-FRAME /* [ */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_lcurly
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_lcurly C-Win
ON CHOOSE OF Btn_lcurly IN FRAME DEFAULT-FRAME /* { */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Lessthan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Lessthan C-Win
ON CHOOSE OF Btn_Lessthan IN FRAME DEFAULT-FRAME /* < */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_lparenth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_lparenth C-Win
ON CHOOSE OF Btn_lparenth IN FRAME DEFAULT-FRAME /* ( */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_M
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_M C-Win
ON CHOOSE OF Btn_M IN FRAME DEFAULT-FRAME /* M */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Minus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Minus C-Win
ON CHOOSE OF Btn_Minus IN FRAME DEFAULT-FRAME /* - */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_N
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_N C-Win
ON CHOOSE OF Btn_N IN FRAME DEFAULT-FRAME /* N */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_O
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_O C-Win
ON CHOOSE OF Btn_O IN FRAME DEFAULT-FRAME /* O */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_P
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_P C-Win
ON CHOOSE OF Btn_P IN FRAME DEFAULT-FRAME /* P */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Percent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Percent C-Win
ON CHOOSE OF Btn_Percent IN FRAME DEFAULT-FRAME /* % */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Period C-Win
ON CHOOSE OF Btn_Period IN FRAME DEFAULT-FRAME /* . */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Pipe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Pipe C-Win
ON CHOOSE OF Btn_Pipe IN FRAME DEFAULT-FRAME /* | */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Plus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Plus C-Win
ON CHOOSE OF Btn_Plus IN FRAME DEFAULT-FRAME /* + */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Pound
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Pound C-Win
ON CHOOSE OF Btn_Pound IN FRAME DEFAULT-FRAME /* # */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Q
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Q C-Win
ON CHOOSE OF Btn_Q IN FRAME DEFAULT-FRAME /* Q */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Question
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Question C-Win
ON CHOOSE OF Btn_Question IN FRAME DEFAULT-FRAME /* ? */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_R
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_R C-Win
ON CHOOSE OF Btn_R IN FRAME DEFAULT-FRAME /* R */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_rbox
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_rbox C-Win
ON CHOOSE OF Btn_rbox IN FRAME DEFAULT-FRAME /* ] */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_rcurly
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_rcurly C-Win
ON CHOOSE OF Btn_rcurly IN FRAME DEFAULT-FRAME /* } */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_rparenth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_rparenth C-Win
ON CHOOSE OF Btn_rparenth IN FRAME DEFAULT-FRAME /* ) */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_S
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_S C-Win
ON CHOOSE OF Btn_S IN FRAME DEFAULT-FRAME /* S */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_semicoln
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_semicoln C-Win
ON CHOOSE OF Btn_semicoln IN FRAME DEFAULT-FRAME /* ; */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Sglquote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Sglquote C-Win
ON CHOOSE OF Btn_Sglquote IN FRAME DEFAULT-FRAME /* ' */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Sort C-Win
ON CHOOSE OF Btn_Sort IN FRAME DEFAULT-FRAME /* SORT */
DO:
  RUN Apply_Key (SELF:TOOLTIP).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Space
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Space C-Win
ON CHOOSE OF Btn_Space IN FRAME DEFAULT-FRAME /* SPACE */
DO:
  RUN Apply_Key (SELF:TOOLTIP).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_T
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_T C-Win
ON CHOOSE OF Btn_T IN FRAME DEFAULT-FRAME /* T */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_U
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_U C-Win
ON CHOOSE OF Btn_U IN FRAME DEFAULT-FRAME /* U */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_underln
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_underln C-Win
ON CHOOSE OF Btn_underln IN FRAME DEFAULT-FRAME /* _ */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_V
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_V C-Win
ON CHOOSE OF Btn_V IN FRAME DEFAULT-FRAME /* V */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_W
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_W C-Win
ON CHOOSE OF Btn_W IN FRAME DEFAULT-FRAME /* W */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_X
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_X C-Win
ON CHOOSE OF Btn_X IN FRAME DEFAULT-FRAME /* X */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Y
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Y C-Win
ON CHOOSE OF Btn_Y IN FRAME DEFAULT-FRAME /* Y */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Z
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Z C-Win
ON CHOOSE OF Btn_Z IN FRAME DEFAULT-FRAME /* Z */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
  CURRENT-WINDOW:ROW = parents_row + 14.
  RUN enable_UI.
  {touch/kbLanguage.i}
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Apply_Key C-Win 
PROCEDURE Apply_Key :
/*------------------------------------------------------------------------------
  Purpose:     Accept keystroke and send to calling procedure.
  Parameters:  Input Keystroke Value
  Notes:       
------------------------------------------------------------------------------*/
  {touch/applykey.i}

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
  ENABLE Btn_Exclaim Btn_Atsign Btn_Pound Btn_Dollar Btn_Percent Btn_Carot 
         Btn_And Btn_Asterisk Btn_lparenth Btn_rparenth Btn_lcurly Btn_rcurly 
         Btn_underln Btn_Pipe Btn_Lbox Btn_Q Btn_W Btn_E Btn_R Btn_T Btn_Y 
         Btn_U Btn_I Btn_O Btn_P Btn_Minus Btn_Plus Btn_Equal Btn_Lessthan 
         Btn_A Btn_S Btn_D Btn_F Btn_G Btn_H Btn_J Btn_K Btn_L Btn_Colon 
         Btn_semicoln Btn_Dblquote Btn_Sglquote Btn_rbox Btn_Grtrthan Btn_Z 
         Btn_X Btn_C Btn_V Btn_B Btn_N Btn_M Btn_Comma Btn_Period Btn_fwdslash 
         Btn_bckslash Btn_Question Btn_Caps_Lock Btn_Clear Btn_Space 
         Btn_Backspace Btn_Sort Btn_Alpha 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

