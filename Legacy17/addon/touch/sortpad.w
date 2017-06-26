&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: touch/sortpad.w

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
&Scoped-Define ENABLED-OBJECTS Btn_A Btn_B Btn_C Btn_D Btn_E Btn_F Btn_G ~
Btn_H Btn_I Btn_J Btn_K Btn_L Btn_M Btn_N Btn_O Btn_P Btn_Q Btn_R Btn_S ~
Btn_T Btn_U Btn_V Btn_W Btn_X Btn_Y Btn_Z Btn_Clear Btn_Space Btn_Backspace ~
Btn_Alpha Btn_Qwerty 

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
     IMAGE-DOWN FILE "keyboard/down\a":U
     LABEL "A" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Alpha 
     LABEL "ALPHA" 
     SIZE 17 BY 1.91 TOOLTIP "ALPHA"
     FONT 6.

DEFINE BUTTON Btn_B 
     IMAGE-UP FILE "keyboard\b":U
     IMAGE-DOWN FILE "keyboard/down\b":U
     LABEL "B" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Backspace 
     LABEL "BACKSPACE" 
     SIZE 33 BY 1.91 TOOLTIP "BACKSPACE"
     FONT 6.

DEFINE BUTTON Btn_C 
     IMAGE-UP FILE "keyboard\c":U
     IMAGE-DOWN FILE "keyboard/down\c":U
     LABEL "C" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Clear 
     LABEL "CLEAR" 
     SIZE 33 BY 1.91 TOOLTIP "CLEAR"
     FONT 6.

DEFINE BUTTON Btn_D 
     IMAGE-UP FILE "keyboard\d":U
     IMAGE-DOWN FILE "keyboard/down\d":U
     LABEL "D" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_E 
     IMAGE-UP FILE "keyboard\e":U
     IMAGE-DOWN FILE "keyboard/down\e":U
     LABEL "E" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_F 
     IMAGE-UP FILE "keyboard\f":U
     IMAGE-DOWN FILE "keyboard/down\f":U
     LABEL "F" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_G 
     IMAGE-UP FILE "keyboard\g":U
     IMAGE-DOWN FILE "keyboard/down\g":U
     LABEL "G" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_H 
     IMAGE-UP FILE "keyboard\h":U
     IMAGE-DOWN FILE "keyboard/down\h":U
     LABEL "H" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_I 
     IMAGE-UP FILE "keyboard\i":U
     IMAGE-DOWN FILE "keyboard/down\i":U
     LABEL "I" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_J 
     IMAGE-UP FILE "keyboard\j":U
     IMAGE-DOWN FILE "keyboard/down/j.bmp":U
     LABEL "J" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_K 
     IMAGE-UP FILE "keyboard\k":U
     IMAGE-DOWN FILE "keyboard/down/k.bmp":U
     LABEL "K" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_L 
     IMAGE-UP FILE "keyboard\l":U
     IMAGE-DOWN FILE "keyboard/down/l.bmp":U
     LABEL "L" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_M 
     IMAGE-UP FILE "keyboard\m":U
     IMAGE-DOWN FILE "keyboard/down/m.bmp":U
     LABEL "M" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_N 
     IMAGE-UP FILE "keyboard\n":U
     IMAGE-DOWN FILE "keyboard/down/n.bmp":U
     LABEL "N" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_O 
     IMAGE-UP FILE "keyboard\o":U
     IMAGE-DOWN FILE "keyboard/down/o.bmp":U
     LABEL "O" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_P 
     IMAGE-UP FILE "keyboard\p":U
     IMAGE-DOWN FILE "keyboard/down/p.bmp":U
     LABEL "P" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Q 
     IMAGE-UP FILE "keyboard\q":U
     IMAGE-DOWN FILE "keyboard/down/q.bmp":U
     LABEL "Q" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Qwerty 
     LABEL "QWERTY" 
     SIZE 16 BY 1.91 TOOLTIP "QWERTY Keyboard"
     FONT 6.

DEFINE BUTTON Btn_R 
     IMAGE-UP FILE "keyboard\r":U
     IMAGE-DOWN FILE "keyboard/down/r.bmp":U
     LABEL "R" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_S 
     IMAGE-UP FILE "keyboard\s":U
     IMAGE-DOWN FILE "keyboard/down/s.bmp":U
     LABEL "S" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Space 
     LABEL "SPACE" 
     SIZE 44 BY 1.91 TOOLTIP "SPACE"
     FONT 6.

DEFINE BUTTON Btn_T 
     IMAGE-UP FILE "keyboard\t":U
     IMAGE-DOWN FILE "keyboard/down/t.bmp":U
     LABEL "T" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_U 
     IMAGE-UP FILE "keyboard\u":U
     IMAGE-DOWN FILE "keyboard/down/u.bmp":U
     LABEL "U" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_V 
     IMAGE-UP FILE "keyboard\v":U
     IMAGE-DOWN FILE "keyboard/down/v.bmp":U
     LABEL "V" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_W 
     IMAGE-UP FILE "keyboard\w":U
     IMAGE-DOWN FILE "keyboard/down/w.bmp":U
     LABEL "W" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_X 
     IMAGE-UP FILE "keyboard\x":U
     IMAGE-DOWN FILE "keyboard/down/x.bmp":U
     LABEL "X" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Y 
     IMAGE-UP FILE "keyboard\y":U
     IMAGE-DOWN FILE "keyboard/down/y.bmp":U
     LABEL "Y" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Z 
     IMAGE-UP FILE "keyboard\z":U
     IMAGE-DOWN FILE "keyboard/down/z.bmp":U
     LABEL "Z" 
     SIZE 11 BY 2.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_A AT ROW 1 COL 1
     Btn_B AT ROW 1 COL 12
     Btn_C AT ROW 1 COL 23
     Btn_D AT ROW 1 COL 34
     Btn_E AT ROW 1 COL 45
     Btn_F AT ROW 1 COL 56
     Btn_G AT ROW 1 COL 67
     Btn_H AT ROW 1 COL 78
     Btn_I AT ROW 1 COL 89
     Btn_J AT ROW 1 COL 100
     Btn_K AT ROW 1 COL 111
     Btn_L AT ROW 1 COL 122
     Btn_M AT ROW 1 COL 133
     Btn_N AT ROW 3.38 COL 1
     Btn_O AT ROW 3.38 COL 12
     Btn_P AT ROW 3.38 COL 23
     Btn_Q AT ROW 3.38 COL 34
     Btn_R AT ROW 3.38 COL 45
     Btn_S AT ROW 3.38 COL 56
     Btn_T AT ROW 3.38 COL 67
     Btn_U AT ROW 3.38 COL 78
     Btn_V AT ROW 3.38 COL 89
     Btn_W AT ROW 3.38 COL 100
     Btn_X AT ROW 3.38 COL 111
     Btn_Y AT ROW 3.38 COL 122
     Btn_Z AT ROW 3.38 COL 133
     Btn_Clear AT ROW 5.76 COL 1
     Btn_Space AT ROW 5.76 COL 34
     Btn_Backspace AT ROW 5.76 COL 78
     Btn_Alpha AT ROW 5.76 COL 111
     Btn_Qwerty AT ROW 5.76 COL 128
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 143.2 BY 6.71
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
         TITLE              = "Alphabet Keyboard"
         COLUMN             = 1
         ROW                = 11.14
         HEIGHT             = 6.71
         WIDTH              = 143.2
         MAX-HEIGHT         = 33.91
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.91
         VIRTUAL-WIDTH      = 204.8
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
       Btn_Clear:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "CLEAR".

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
ON END-ERROR OF C-Win /* Alphabet Keyboard */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Alphabet Keyboard */
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


&Scoped-define SELF-NAME Btn_C
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_C C-Win
ON CHOOSE OF Btn_C IN FRAME DEFAULT-FRAME /* C */
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


&Scoped-define SELF-NAME Btn_D
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_D C-Win
ON CHOOSE OF Btn_D IN FRAME DEFAULT-FRAME /* D */
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


&Scoped-define SELF-NAME Btn_F
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_F C-Win
ON CHOOSE OF Btn_F IN FRAME DEFAULT-FRAME /* F */
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


&Scoped-define SELF-NAME Btn_M
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_M C-Win
ON CHOOSE OF Btn_M IN FRAME DEFAULT-FRAME /* M */
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


&Scoped-define SELF-NAME Btn_Q
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Q C-Win
ON CHOOSE OF Btn_Q IN FRAME DEFAULT-FRAME /* Q */
DO:
  RUN Apply_Key (SELF:LABEL).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Qwerty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Qwerty C-Win
ON CHOOSE OF Btn_Qwerty IN FRAME DEFAULT-FRAME /* QWERTY */
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


&Scoped-define SELF-NAME Btn_S
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_S C-Win
ON CHOOSE OF Btn_S IN FRAME DEFAULT-FRAME /* S */
DO:
  RUN Apply_Key (SELF:LABEL).
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
  current-window:ROW = parents_row + 14.
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
  ENABLE Btn_A Btn_B Btn_C Btn_D Btn_E Btn_F Btn_G Btn_H Btn_I Btn_J Btn_K 
         Btn_L Btn_M Btn_N Btn_O Btn_P Btn_Q Btn_R Btn_S Btn_T Btn_U Btn_V 
         Btn_W Btn_X Btn_Y Btn_Z Btn_Clear Btn_Space Btn_Backspace Btn_Alpha 
         Btn_Qwerty 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

