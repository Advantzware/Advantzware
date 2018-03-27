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

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Atsign Btn_Dblquote Btn_And ~
Btn_Caps_Lock Btn_Asterisk Btn_Clear Btn_Space Btn_Backspace Btn_bckslash ~
Btn_Sort Btn_Carot Btn_Qwerty Btn_Colon Btn_Comma Btn_Dollar Btn_Equal ~
Btn_Exclaim Btn_fwdslash Btn_Grtrthan Btn_Lbox Btn_lcurly Btn_Lessthan ~
Btn_lparenth Btn_Minus Btn_Percent Btn_Period Btn_Pipe Btn_Plus Btn_Pound ~
Btn_Question Btn_rbox Btn_rcurly Btn_rparenth Btn_semicoln Btn_Sglquote ~
Btn_underln Btn_A Btn_B Btn_C Btn_D Btn_E Btn_F Btn_G Btn_H Btn_I Btn_J ~
Btn_K Btn_L Btn_M Btn_N Btn_O Btn_P Btn_Q Btn_R Btn_S Btn_T Btn_U Btn_V ~
Btn_W Btn_X Btn_Y Btn_Z 

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

DEFINE BUTTON Btn_And 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_amp.png":U NO-FOCUS FLAT-BUTTON
     LABEL "&" 
     SIZE 11 BY 2.38 TOOLTIP "Ampersand".

DEFINE BUTTON Btn_Asterisk 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_asterisk.png":U NO-FOCUS FLAT-BUTTON
     LABEL "*" 
     SIZE 11 BY 2.38 TOOLTIP "Asterisk".

DEFINE BUTTON Btn_Atsign 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_at.png":U NO-FOCUS FLAT-BUTTON
     LABEL "@" 
     SIZE 11 BY 2.38 TOOLTIP "At Sign".

DEFINE BUTTON Btn_B 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_b.png":U NO-FOCUS FLAT-BUTTON
     LABEL "B" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Backspace 
     LABEL "BACKSPACE" 
     SIZE 43 BY 1.91 TOOLTIP "BACKSPACE"
     FONT 6.

DEFINE BUTTON Btn_bckslash 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_backslash.png":U NO-FOCUS FLAT-BUTTON
     LABEL "~\" 
     SIZE 11 BY 2.38 TOOLTIP "Back Slash".

DEFINE BUTTON Btn_C 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_c.png":U NO-FOCUS FLAT-BUTTON
     LABEL "C" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Caps_Lock 
     LABEL "CAPS LOCK (ON)" 
     SIZE 47 BY 1.91 TOOLTIP "CAPS LOCK"
     FONT 6.

DEFINE BUTTON Btn_Carot 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_carrot.png":U NO-FOCUS FLAT-BUTTON
     LABEL "^" 
     SIZE 11 BY 2.38 TOOLTIP "Carot".

DEFINE BUTTON Btn_Clear 
     LABEL "CLEAR" 
     SIZE 16 BY 1.91 TOOLTIP "CLEAR"
     FONT 6.

DEFINE BUTTON Btn_Colon 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_colon.png":U NO-FOCUS FLAT-BUTTON
     LABEL ":" 
     SIZE 11 BY 2.38 TOOLTIP "Colon".

DEFINE BUTTON Btn_Comma 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_comma.png":U NO-FOCUS FLAT-BUTTON
     LABEL "," 
     SIZE 11 BY 2.38 TOOLTIP "Comma".

DEFINE BUTTON Btn_D 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_d.png":U NO-FOCUS FLAT-BUTTON
     LABEL "D" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Dblquote 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_doublequote.png":U NO-FOCUS FLAT-BUTTON
     LABEL "DQ" 
     SIZE 11 BY 2.38 TOOLTIP "Double-Quote".

DEFINE BUTTON Btn_Dollar 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_dollar.png":U NO-FOCUS FLAT-BUTTON
     LABEL "$" 
     SIZE 11 BY 2.38 TOOLTIP "Dollar".

DEFINE BUTTON Btn_E 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_e.png":U NO-FOCUS FLAT-BUTTON
     LABEL "E" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Equal 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_equal.png":U NO-FOCUS FLAT-BUTTON
     LABEL "=" 
     SIZE 11 BY 2.38 TOOLTIP "Equal".

DEFINE BUTTON Btn_Exclaim 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_bang.png":U NO-FOCUS FLAT-BUTTON
     LABEL "!" 
     SIZE 11 BY 2.38 TOOLTIP "Exclamition".

DEFINE BUTTON Btn_F 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_f.png":U NO-FOCUS FLAT-BUTTON
     LABEL "F" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_fwdslash 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_forwardslash.png":U NO-FOCUS FLAT-BUTTON
     LABEL "/" 
     SIZE 11 BY 2.38 TOOLTIP "Forward Slash".

DEFINE BUTTON Btn_G 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_g.png":U NO-FOCUS FLAT-BUTTON
     LABEL "G" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Grtrthan 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_greaterthan.png":U NO-FOCUS FLAT-BUTTON
     LABEL ">" 
     SIZE 11 BY 2.38 TOOLTIP "Greater Than".

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

DEFINE BUTTON Btn_Lbox 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_left_bracket.png":U NO-FOCUS FLAT-BUTTON
     LABEL "[" 
     SIZE 11 BY 2.38 TOOLTIP "Left Square Bracket".

DEFINE BUTTON Btn_lcurly 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_left_curly.png":U NO-FOCUS FLAT-BUTTON
     LABEL "~{" 
     SIZE 11 BY 2.38 TOOLTIP "Left Curly Bracket".

DEFINE BUTTON Btn_Lessthan 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_lessthan.png":U NO-FOCUS FLAT-BUTTON
     LABEL "<" 
     SIZE 11 BY 2.38 TOOLTIP "Less Than".

DEFINE BUTTON Btn_lparenth 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_left_paren.png":U NO-FOCUS FLAT-BUTTON
     LABEL "(" 
     SIZE 11 BY 2.38 TOOLTIP "Left Parenthesis".

DEFINE BUTTON Btn_M 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_m.png":U NO-FOCUS FLAT-BUTTON
     LABEL "M" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Minus 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_minus.png":U NO-FOCUS FLAT-BUTTON
     LABEL "-" 
     SIZE 11 BY 2.38 TOOLTIP "Minus".

DEFINE BUTTON Btn_N 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_n.png":U NO-FOCUS FLAT-BUTTON
     LABEL "N" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_O 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_o.png":U NO-FOCUS FLAT-BUTTON
     LABEL "O" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_P 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_p.png":U NO-FOCUS FLAT-BUTTON
     LABEL "P" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Percent 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_percent.png":U NO-FOCUS FLAT-BUTTON
     LABEL "%" 
     SIZE 11 BY 2.38 TOOLTIP "Percent".

DEFINE BUTTON Btn_Period 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_period.png":U NO-FOCUS FLAT-BUTTON
     LABEL "." 
     SIZE 11 BY 2.38 TOOLTIP "Period".

DEFINE BUTTON Btn_Pipe 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_pipe.png":U NO-FOCUS FLAT-BUTTON
     LABEL "|" 
     SIZE 11 BY 2.38 TOOLTIP "Pipe".

DEFINE BUTTON Btn_Plus 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_plus.png":U NO-FOCUS FLAT-BUTTON
     LABEL "+" 
     SIZE 11 BY 2.38 TOOLTIP "Plus".

DEFINE BUTTON Btn_Pound 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_pound.png":U NO-FOCUS FLAT-BUTTON
     LABEL "#" 
     SIZE 11 BY 2.38 TOOLTIP "Pound".

DEFINE BUTTON Btn_Q 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_q.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Q" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Question 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_question.png":U NO-FOCUS FLAT-BUTTON
     LABEL "?" 
     SIZE 11 BY 2.38 TOOLTIP "Question Mark".

DEFINE BUTTON Btn_Qwerty 
     LABEL "QWERTY" 
     SIZE 16 BY 1.91 TOOLTIP "QWERTY Keyboard"
     FONT 6.

DEFINE BUTTON Btn_R 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_r.png":U NO-FOCUS FLAT-BUTTON
     LABEL "R" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_rbox 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_right_bracket.png":U NO-FOCUS FLAT-BUTTON
     LABEL "]" 
     SIZE 11 BY 2.38 TOOLTIP "Right Square Bracket".

DEFINE BUTTON Btn_rcurly 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_right_curly.png":U NO-FOCUS FLAT-BUTTON
     LABEL "}" 
     SIZE 11 BY 2.38 TOOLTIP "Right Curly Bracket".

DEFINE BUTTON Btn_rparenth 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_right_paren.png":U NO-FOCUS FLAT-BUTTON
     LABEL ")" 
     SIZE 11 BY 2.38 TOOLTIP "Right Parenthesis".

DEFINE BUTTON Btn_S 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_s.png":U NO-FOCUS FLAT-BUTTON
     LABEL "S" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_semicoln 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_semicolon.png":U NO-FOCUS FLAT-BUTTON
     LABEL "~;" 
     SIZE 11 BY 2.38 TOOLTIP "Semi-Colon".

DEFINE BUTTON Btn_Sglquote 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_quote.png":U NO-FOCUS FLAT-BUTTON
     LABEL "'" 
     SIZE 11 BY 2.38 TOOLTIP "Single Quote".

DEFINE BUTTON Btn_Sort 
     LABEL "SORT" 
     SIZE 16 BY 1.91 TOOLTIP "SORT"
     FONT 6.

DEFINE BUTTON Btn_Space 
     LABEL "SPACE" 
     SIZE 16 BY 1.91 TOOLTIP "SPACE"
     FONT 6.

DEFINE BUTTON Btn_T 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_t.png":U NO-FOCUS FLAT-BUTTON
     LABEL "T" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_U 
     IMAGE-UP FILE "Graphics\Keyboard\keyboard_key_u.png":U NO-FOCUS FLAT-BUTTON
     LABEL "U" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_underln 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_underline.png":U NO-FOCUS FLAT-BUTTON
     LABEL "_" 
     SIZE 11 BY 2.38 TOOLTIP "Underscore".

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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_Atsign AT ROW 7.67 COL 12
     Btn_Dblquote AT ROW 7.67 COL 133
     Btn_And AT ROW 7.67 COL 67
     Btn_Caps_Lock AT ROW 5.76 COL 1
     Btn_Asterisk AT ROW 7.67 COL 78
     Btn_Clear AT ROW 5.76 COL 48
     Btn_Space AT ROW 5.76 COL 64
     Btn_Backspace AT ROW 5.76 COL 80
     Btn_bckslash AT ROW 10.05 COL 78
     Btn_Sort AT ROW 5.76 COL 123
     Btn_Carot AT ROW 7.67 COL 56
     Btn_Qwerty AT ROW 5.76 COL 139
     Btn_Colon AT ROW 10.05 COL 89
     Btn_Comma AT ROW 10.05 COL 45
     Btn_Dollar AT ROW 7.67 COL 34
     Btn_Equal AT ROW 10.05 COL 122
     Btn_Exclaim AT ROW 7.67 COL 1
     Btn_fwdslash AT ROW 10.05 COL 67
     Btn_Grtrthan AT ROW 10.05 COL 23
     Btn_Lbox AT ROW 1 COL 144 WIDGET-ID 2
     Btn_lcurly AT ROW 10.05 COL 133
     Btn_Lessthan AT ROW 10.05 COL 12
     Btn_lparenth AT ROW 7.67 COL 89
     Btn_Minus AT ROW 10.05 COL 111
     Btn_Percent AT ROW 7.67 COL 45
     Btn_Period AT ROW 10.05 COL 56
     Btn_Pipe AT ROW 10.05 COL 1
     Btn_Plus AT ROW 7.67 COL 122
     Btn_Pound AT ROW 7.67 COL 23
     Btn_Question AT ROW 10.05 COL 34
     Btn_rbox AT ROW 3.38 COL 144 WIDGET-ID 4
     Btn_rcurly AT ROW 10.05 COL 144
     Btn_rparenth AT ROW 7.67 COL 100
     Btn_semicoln AT ROW 10.05 COL 100
     Btn_Sglquote AT ROW 7.67 COL 144
     Btn_underln AT ROW 7.67 COL 111
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
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 154.2 BY 11.48
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
         TITLE              = "Alphabetic Keyboard"
         COLUMN             = 1
         ROW                = 17.38
         HEIGHT             = 11.48
         WIDTH              = 154.2
         MAX-HEIGHT         = 11.48
         MAX-WIDTH          = 154.2
         VIRTUAL-HEIGHT     = 11.48
         VIRTUAL-WIDTH      = 154.2
         MAX-BUTTON         = no
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
ON END-ERROR OF C-Win /* Alphabetic Keyboard */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Alphabetic Keyboard */
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


&Scoped-define SELF-NAME Btn_And
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_And C-Win
ON CHOOSE OF Btn_And IN FRAME DEFAULT-FRAME /*  */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Asterisk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Asterisk C-Win
ON CHOOSE OF Btn_Asterisk IN FRAME DEFAULT-FRAME /* * */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Atsign
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Atsign C-Win
ON CHOOSE OF Btn_Atsign IN FRAME DEFAULT-FRAME /* @ */
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


&Scoped-define SELF-NAME Btn_bckslash
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_bckslash C-Win
ON CHOOSE OF Btn_bckslash IN FRAME DEFAULT-FRAME /* \ */
DO:
  RUN Apply_Key (SELF:LABEL).
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Carot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Carot C-Win
ON CHOOSE OF Btn_Carot IN FRAME DEFAULT-FRAME /* ^ */
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


&Scoped-define SELF-NAME Btn_Colon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Colon C-Win
ON CHOOSE OF Btn_Colon IN FRAME DEFAULT-FRAME /* : */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Comma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Comma C-Win
ON CHOOSE OF Btn_Comma IN FRAME DEFAULT-FRAME /* , */
DO:
  RUN Apply_Key (SELF:LABEL).
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


&Scoped-define SELF-NAME Btn_Dblquote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Dblquote C-Win
ON CHOOSE OF Btn_Dblquote IN FRAME DEFAULT-FRAME /* DQ */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Dollar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Dollar C-Win
ON CHOOSE OF Btn_Dollar IN FRAME DEFAULT-FRAME /* $ */
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


&Scoped-define SELF-NAME Btn_Equal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Equal C-Win
ON CHOOSE OF Btn_Equal IN FRAME DEFAULT-FRAME /* = */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Exclaim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Exclaim C-Win
ON CHOOSE OF Btn_Exclaim IN FRAME DEFAULT-FRAME /* ! */
DO:
  RUN Apply_Key (SELF:LABEL).
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


&Scoped-define SELF-NAME Btn_fwdslash
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_fwdslash C-Win
ON CHOOSE OF Btn_fwdslash IN FRAME DEFAULT-FRAME /* / */
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


&Scoped-define SELF-NAME Btn_Grtrthan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grtrthan C-Win
ON CHOOSE OF Btn_Grtrthan IN FRAME DEFAULT-FRAME /* > */
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


&Scoped-define SELF-NAME Btn_Lbox
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Lbox C-Win
ON CHOOSE OF Btn_Lbox IN FRAME DEFAULT-FRAME /* [ */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_lcurly
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_lcurly C-Win
ON CHOOSE OF Btn_lcurly IN FRAME DEFAULT-FRAME /* { */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Lessthan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Lessthan C-Win
ON CHOOSE OF Btn_Lessthan IN FRAME DEFAULT-FRAME /* < */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_lparenth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_lparenth C-Win
ON CHOOSE OF Btn_lparenth IN FRAME DEFAULT-FRAME /* ( */
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


&Scoped-define SELF-NAME Btn_Minus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Minus C-Win
ON CHOOSE OF Btn_Minus IN FRAME DEFAULT-FRAME /* - */
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


&Scoped-define SELF-NAME Btn_O
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_O C-Win
ON CHOOSE OF Btn_O IN FRAME DEFAULT-FRAME /* O */
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


&Scoped-define SELF-NAME Btn_Percent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Percent C-Win
ON CHOOSE OF Btn_Percent IN FRAME DEFAULT-FRAME /* % */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Period C-Win
ON CHOOSE OF Btn_Period IN FRAME DEFAULT-FRAME /* . */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Pipe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Pipe C-Win
ON CHOOSE OF Btn_Pipe IN FRAME DEFAULT-FRAME /* | */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Plus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Plus C-Win
ON CHOOSE OF Btn_Plus IN FRAME DEFAULT-FRAME /* + */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Pound
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Pound C-Win
ON CHOOSE OF Btn_Pound IN FRAME DEFAULT-FRAME /* # */
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


&Scoped-define SELF-NAME Btn_Question
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Question C-Win
ON CHOOSE OF Btn_Question IN FRAME DEFAULT-FRAME /* ? */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Qwerty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Qwerty C-Win
ON CHOOSE OF Btn_Qwerty IN FRAME DEFAULT-FRAME /* QWERTY */
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


&Scoped-define SELF-NAME Btn_rbox
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_rbox C-Win
ON CHOOSE OF Btn_rbox IN FRAME DEFAULT-FRAME /* ] */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_rcurly
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_rcurly C-Win
ON CHOOSE OF Btn_rcurly IN FRAME DEFAULT-FRAME /* } */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_rparenth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_rparenth C-Win
ON CHOOSE OF Btn_rparenth IN FRAME DEFAULT-FRAME /* ) */
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


&Scoped-define SELF-NAME Btn_semicoln
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_semicoln C-Win
ON CHOOSE OF Btn_semicoln IN FRAME DEFAULT-FRAME /* ; */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Sglquote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Sglquote C-Win
ON CHOOSE OF Btn_Sglquote IN FRAME DEFAULT-FRAME /* ' */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Sort C-Win
ON CHOOSE OF Btn_Sort IN FRAME DEFAULT-FRAME /* SORT */
DO:
  RUN Apply_Key (SELF:TOOLTIP).
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


&Scoped-define SELF-NAME Btn_U
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_U C-Win
ON CHOOSE OF Btn_U IN FRAME DEFAULT-FRAME /* U */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_underln
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_underln C-Win
ON CHOOSE OF Btn_underln IN FRAME DEFAULT-FRAME /* _ */
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
  current-window:ROW = parents_row + 14.
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
  ENABLE Btn_Atsign Btn_Dblquote Btn_And Btn_Caps_Lock Btn_Asterisk Btn_Clear 
         Btn_Space Btn_Backspace Btn_bckslash Btn_Sort Btn_Carot Btn_Qwerty 
         Btn_Colon Btn_Comma Btn_Dollar Btn_Equal Btn_Exclaim Btn_fwdslash 
         Btn_Grtrthan Btn_Lbox Btn_lcurly Btn_Lessthan Btn_lparenth Btn_Minus 
         Btn_Percent Btn_Period Btn_Pipe Btn_Plus Btn_Pound Btn_Question 
         Btn_rbox Btn_rcurly Btn_rparenth Btn_semicoln Btn_Sglquote Btn_underln 
         Btn_A Btn_B Btn_C Btn_D Btn_E Btn_F Btn_G Btn_H Btn_I Btn_J Btn_K 
         Btn_L Btn_M Btn_N Btn_O Btn_P Btn_Q Btn_R Btn_S Btn_T Btn_U Btn_V 
         Btn_W Btn_X Btn_Y Btn_Z 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

