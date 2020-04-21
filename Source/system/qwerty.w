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
&ELSE
DEFINE INPUT PARAMETER h_calling_window AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER h_touchfrm AS HANDLE NO-UNDO.
&ENDIF  

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

/* This NK1 setting disables keyboards on WIP modules if set to YES.
   Below code has been commeneted User do not use the NK1 to drive 
   the functionality but can select them on demand */ 
/*
DO TRANSACTION:
   {sys/inc/tskey.i}
END.

IF NOT tskey-log THEN
   RETURN.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_A Btn_Tab Btn_Enter Btn_Clear Btn_Space ~
Btn_Backspace Btn_Eight Btn_Five Btn_Four Btn_Nine Btn_B Btn_One Btn_Seven ~
Btn_C Btn_Six Btn_D Btn_Three Btn_E Btn_Two Btn_F Btn_Zero Btn_G Btn_H ~
Btn_I Btn_J Btn_K Btn_L Btn_M Btn_N Btn_O Btn_P Btn_Q Btn_R Btn_S Btn_T ~
Btn_U Btn_V Btn_W Btn_X Btn_Y Btn_Z 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_A 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_a.png":U NO-FOCUS FLAT-BUTTON
     LABEL "A" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_B 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_b.png":U NO-FOCUS FLAT-BUTTON
     LABEL "B" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Backspace 
     LABEL "BACKSPACE" 
     SIZE 16 BY 1.91 TOOLTIP "BACKSPACE"
     FONT 6.

DEFINE BUTTON Btn_C 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_c.png":U NO-FOCUS FLAT-BUTTON
     LABEL "C" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Clear 
     LABEL "CLEAR" 
     SIZE 15 BY 1.91 TOOLTIP "CLEAR"
     FONT 6.

DEFINE BUTTON Btn_D 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_d.png":U NO-FOCUS FLAT-BUTTON
     LABEL "D" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_E 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_e.png":U NO-FOCUS FLAT-BUTTON
     LABEL "E" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Eight 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_8.png":U NO-FOCUS FLAT-BUTTON
     LABEL "8" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Enter 
     LABEL "ENTER" 
     SIZE 16 BY 1.91 TOOLTIP "ENTER"
     FONT 6.

DEFINE BUTTON Btn_F 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_f.png":U NO-FOCUS FLAT-BUTTON
     LABEL "F" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Five 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_5.png":U NO-FOCUS FLAT-BUTTON
     LABEL "5" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Four 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_4.png":U NO-FOCUS FLAT-BUTTON
     LABEL "4" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_G 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_g.png":U NO-FOCUS FLAT-BUTTON
     LABEL "G" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_H 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_h.png":U NO-FOCUS FLAT-BUTTON
     LABEL "H" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_I 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_i.png":U NO-FOCUS FLAT-BUTTON
     LABEL "I" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_J 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_j.png":U NO-FOCUS FLAT-BUTTON
     LABEL "J" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_K 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_k.png":U NO-FOCUS FLAT-BUTTON
     LABEL "K" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_L 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_l.png":U NO-FOCUS FLAT-BUTTON
     LABEL "L" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_M 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_m.png":U NO-FOCUS FLAT-BUTTON
     LABEL "M" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_N 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_n.png":U NO-FOCUS FLAT-BUTTON
     LABEL "N" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Nine 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_9.png":U NO-FOCUS FLAT-BUTTON
     LABEL "9" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_O 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_o.png":U NO-FOCUS FLAT-BUTTON
     LABEL "O" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_One 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_1.png":U NO-FOCUS FLAT-BUTTON
     LABEL "1" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_P 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_p.png":U NO-FOCUS FLAT-BUTTON
     LABEL "P" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Q 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_q.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Q" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_R 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_r.png":U NO-FOCUS FLAT-BUTTON
     LABEL "R" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_S 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_s.png":U NO-FOCUS FLAT-BUTTON
     LABEL "S" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Seven 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_7.png":U NO-FOCUS FLAT-BUTTON
     LABEL "7" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Six 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_6.png":U NO-FOCUS FLAT-BUTTON
     LABEL "6" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Space 
     LABEL "SPACE" 
     SIZE 55 BY 1.91 TOOLTIP "SPACE"
     FONT 6.

DEFINE BUTTON Btn_T 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_t.png":U NO-FOCUS FLAT-BUTTON
     LABEL "T" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Tab 
     LABEL "TAB" 
     SIZE 15 BY 1.91 TOOLTIP "TAB"
     FONT 6.

DEFINE BUTTON Btn_Three 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_3.png":U NO-FOCUS FLAT-BUTTON
     LABEL "3" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Two 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_2.png":U NO-FOCUS FLAT-BUTTON
     LABEL "2" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_U 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_u.png":U NO-FOCUS FLAT-BUTTON
     LABEL "U" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_V 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_v.png":U NO-FOCUS FLAT-BUTTON
     LABEL "V" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_W 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_w.png":U NO-FOCUS FLAT-BUTTON
     LABEL "W" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_X 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_x.png":U NO-FOCUS FLAT-BUTTON
     LABEL "X" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Y 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_y.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Y" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Z 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_z.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Z" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Zero 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_0.png":U NO-FOCUS FLAT-BUTTON
     LABEL "0" 
     SIZE 11 BY 2.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_A AT ROW 5.76 COL 6
     Btn_Tab AT ROW 8.38 COL 1 WIDGET-ID 22
     Btn_Enter AT ROW 8.38 COL 95 WIDGET-ID 24
     Btn_Clear AT ROW 10.52 COL 1
     Btn_Space AT ROW 10.52 COL 28
     Btn_Backspace AT ROW 10.52 COL 95
     Btn_Eight AT ROW 1 COL 78 WIDGET-ID 2
     Btn_Five AT ROW 1 COL 45 WIDGET-ID 4
     Btn_Four AT ROW 1 COL 34 WIDGET-ID 6
     Btn_Nine AT ROW 1 COL 89 WIDGET-ID 8
     Btn_B AT ROW 8.14 COL 61
     Btn_One AT ROW 1 COL 1 WIDGET-ID 10
     Btn_Seven AT ROW 1 COL 67 WIDGET-ID 12
     Btn_C AT ROW 8.14 COL 39
     Btn_Six AT ROW 1 COL 56 WIDGET-ID 14
     Btn_D AT ROW 5.76 COL 28
     Btn_Three AT ROW 1 COL 23 WIDGET-ID 16
     Btn_E AT ROW 3.38 COL 23
     Btn_Two AT ROW 1 COL 12 WIDGET-ID 18
     Btn_F AT ROW 5.76 COL 39
     Btn_Zero AT ROW 1 COL 100 WIDGET-ID 20
     Btn_G AT ROW 5.76 COL 50
     Btn_H AT ROW 5.76 COL 61
     Btn_I AT ROW 3.38 COL 78
     Btn_J AT ROW 5.76 COL 72
     Btn_K AT ROW 5.76 COL 83
     Btn_L AT ROW 5.76 COL 94
     Btn_M AT ROW 8.14 COL 83
     Btn_N AT ROW 8.14 COL 72
     Btn_O AT ROW 3.38 COL 89
     Btn_P AT ROW 3.38 COL 100
     Btn_Q AT ROW 3.38 COL 1
     Btn_R AT ROW 3.38 COL 34
     Btn_S AT ROW 5.76 COL 17
     Btn_T AT ROW 3.38 COL 45
     Btn_U AT ROW 3.38 COL 67
     Btn_V AT ROW 8.14 COL 50
     Btn_W AT ROW 3.38 COL 12
     Btn_X AT ROW 8.14 COL 28
     Btn_Y AT ROW 3.38 COL 56
     Btn_Z AT ROW 8.14 COL 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110 BY 11.43
         BGCOLOR 0 .


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
         TITLE              = "Keyboard"
         HEIGHT             = 11.43
         WIDTH              = 110
         MAX-HEIGHT         = 33.9
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.9
         VIRTUAL-WIDTH      = 204.8
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
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       Btn_Backspace:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "BACKSPACE".

ASSIGN 
       Btn_Clear:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "CLEAR".

ASSIGN 
       Btn_Enter:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ENTER".

ASSIGN 
       Btn_Space:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "SPACE".

ASSIGN 
       Btn_Tab:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "TAB".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Keyboard */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Keyboard */
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_B
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_B C-Win
ON CHOOSE OF Btn_B IN FRAME DEFAULT-FRAME /* B */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Backspace
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Backspace C-Win
ON CHOOSE OF Btn_Backspace IN FRAME DEFAULT-FRAME /* BACKSPACE */
DO:
  RUN Apply_Key (SELF:TOOLTIP).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_C
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_C C-Win
ON CHOOSE OF Btn_C IN FRAME DEFAULT-FRAME /* C */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Clear C-Win
ON CHOOSE OF Btn_Clear IN FRAME DEFAULT-FRAME /* CLEAR */
DO:
  RUN Apply_Key (SELF:TOOLTIP).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_D
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_D C-Win
ON CHOOSE OF Btn_D IN FRAME DEFAULT-FRAME /* D */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_E
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_E C-Win
ON CHOOSE OF Btn_E IN FRAME DEFAULT-FRAME /* E */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Eight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Eight C-Win
ON CHOOSE OF Btn_Eight IN FRAME DEFAULT-FRAME /* 8 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Enter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Enter C-Win
ON CHOOSE OF Btn_Enter IN FRAME DEFAULT-FRAME /* ENTER */
DO:
  RUN Apply_Key (SELF:TOOLTIP).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_F
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_F C-Win
ON CHOOSE OF Btn_F IN FRAME DEFAULT-FRAME /* F */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Five
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Five C-Win
ON CHOOSE OF Btn_Five IN FRAME DEFAULT-FRAME /* 5 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Four
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Four C-Win
ON CHOOSE OF Btn_Four IN FRAME DEFAULT-FRAME /* 4 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_G
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_G C-Win
ON CHOOSE OF Btn_G IN FRAME DEFAULT-FRAME /* G */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_H
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_H C-Win
ON CHOOSE OF Btn_H IN FRAME DEFAULT-FRAME /* H */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_I
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_I C-Win
ON CHOOSE OF Btn_I IN FRAME DEFAULT-FRAME /* I */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_J
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_J C-Win
ON CHOOSE OF Btn_J IN FRAME DEFAULT-FRAME /* J */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_K
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_K C-Win
ON CHOOSE OF Btn_K IN FRAME DEFAULT-FRAME /* K */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_L
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_L C-Win
ON CHOOSE OF Btn_L IN FRAME DEFAULT-FRAME /* L */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_M
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_M C-Win
ON CHOOSE OF Btn_M IN FRAME DEFAULT-FRAME /* M */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_N
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_N C-Win
ON CHOOSE OF Btn_N IN FRAME DEFAULT-FRAME /* N */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Nine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Nine C-Win
ON CHOOSE OF Btn_Nine IN FRAME DEFAULT-FRAME /* 9 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_O
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_O C-Win
ON CHOOSE OF Btn_O IN FRAME DEFAULT-FRAME /* O */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_One
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_One C-Win
ON CHOOSE OF Btn_One IN FRAME DEFAULT-FRAME /* 1 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_P
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_P C-Win
ON CHOOSE OF Btn_P IN FRAME DEFAULT-FRAME /* P */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Q
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Q C-Win
ON CHOOSE OF Btn_Q IN FRAME DEFAULT-FRAME /* Q */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_R
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_R C-Win
ON CHOOSE OF Btn_R IN FRAME DEFAULT-FRAME /* R */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_S
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_S C-Win
ON CHOOSE OF Btn_S IN FRAME DEFAULT-FRAME /* S */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Seven
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Seven C-Win
ON CHOOSE OF Btn_Seven IN FRAME DEFAULT-FRAME /* 7 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Six
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Six C-Win
ON CHOOSE OF Btn_Six IN FRAME DEFAULT-FRAME /* 6 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Space
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Space C-Win
ON CHOOSE OF Btn_Space IN FRAME DEFAULT-FRAME /* SPACE */
DO:
  RUN Apply_Key (SELF:TOOLTIP).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_T
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_T C-Win
ON CHOOSE OF Btn_T IN FRAME DEFAULT-FRAME /* T */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Tab C-Win
ON CHOOSE OF Btn_Tab IN FRAME DEFAULT-FRAME /* TAB */
DO:
  RUN Apply_Key (SELF:TOOLTIP).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Three
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Three C-Win
ON CHOOSE OF Btn_Three IN FRAME DEFAULT-FRAME /* 3 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Two
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Two C-Win
ON CHOOSE OF Btn_Two IN FRAME DEFAULT-FRAME /* 2 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_U
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_U C-Win
ON CHOOSE OF Btn_U IN FRAME DEFAULT-FRAME /* U */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_V
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_V C-Win
ON CHOOSE OF Btn_V IN FRAME DEFAULT-FRAME /* V */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_W
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_W C-Win
ON CHOOSE OF Btn_W IN FRAME DEFAULT-FRAME /* W */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_X
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_X C-Win
ON CHOOSE OF Btn_X IN FRAME DEFAULT-FRAME /* X */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Y
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Y C-Win
ON CHOOSE OF Btn_Y IN FRAME DEFAULT-FRAME /* Y */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Z
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Z C-Win
ON CHOOSE OF Btn_Z IN FRAME DEFAULT-FRAME /* Z */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Zero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Zero C-Win
ON CHOOSE OF Btn_Zero IN FRAME DEFAULT-FRAME /* 0 */
DO:
  RUN Apply_Key (SELF:LABEL).
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
  {touch/kbLanguage.i}
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
  ENABLE Btn_A Btn_Tab Btn_Enter Btn_Clear Btn_Space Btn_Backspace Btn_Eight 
         Btn_Five Btn_Four Btn_Nine Btn_B Btn_One Btn_Seven Btn_C Btn_Six Btn_D 
         Btn_Three Btn_E Btn_Two Btn_F Btn_Zero Btn_G Btn_H Btn_I Btn_J Btn_K 
         Btn_L Btn_M Btn_N Btn_O Btn_P Btn_Q Btn_R Btn_S Btn_T Btn_U Btn_V 
         Btn_W Btn_X Btn_Y Btn_Z 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetPosition C-Win 
PROCEDURE pSetPosition :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCol AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcRow AS DECIMAL NO-UNDO.
    
    IF VALID-HANDLE({&WINDOW-NAME}) THEN
    ASSIGN
        {&WINDOW-NAME}:COL = ipcCol
        {&WINDOW-NAME}:ROW = ipcRow
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

