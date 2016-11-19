&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-use-code AS LOG NO-UNDO.
DEF OUTPUT PARAM op-code AS CHAR NO-UNDO.
DEF OUTPUT PARAM op-max-yld AS LOG NO-UNDO.
DEF INPUT-OUTPUT PARAM io-m-code LIKE mach.m-code NO-UNDO.
DEF OUTPUT PARAM op-CodeOrName AS cha NO-UNDO.

/* Local Variable Definitions ---                                       */
/*{custom/globdefs.i} */
{methods/prgsecur.i}
{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-25 begin_mach fi_code rd-CodeName ~
tb_max-yld Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS begin_mach fi_code rd-CodeName tb_max-yld 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)" 
     LABEL "Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_code AS CHARACTER FORMAT "X(30)":U 
     LABEL "Select RM Item Name starting with" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE rd-CodeName AS CHARACTER INITIAL "N" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item Name", "N",
"Item Code", "C"
     SIZE 36 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 6.91.

DEFINE VARIABLE tb_max-yld AS LOGICAL INITIAL no 
     LABEL "Maximize Yield using All Dimensions?" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_mach AT ROW 1.95 COL 31.2 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     fi_code AT ROW 3.38 COL 7.2
     rd-CodeName AT ROW 4.81 COL 39 NO-LABEL WIDGET-ID 2
     tb_max-yld AT ROW 6.48 COL 18.4
     Btn_OK AT ROW 8.62 COL 32
     "Search by:" VIEW-AS TEXT
          SIZE 14 BY .95 AT ROW 4.86 COL 24 WIDGET-ID 6
     RECT-25 AT ROW 1.24 COL 1
     SPACE(0.19) SKIP(1.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Sheet Calculation Parameters"
         DEFAULT-BUTTON Btn_OK.


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

ASSIGN 
       begin_mach:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR FILL-IN fi_code IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Sheet Calculation Parameters */
DO:
  def var ls-cur-val as cha no-undo.
  def var char-val as cha no-undo.

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN rd-CodeName rd-CodeName.
  END.

  case focus:name :     
      when "begin_mach" then do:
          run windows/l-mach.w (cocode, locode, focus:screen-value, output char-val).
          IF char-val NE "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
      END.
      when "fi_code" then do:

        IF rd-CodeName EQ "C" THEN
           run windows/l-item5.w (g_company,fi_code:SCREEN-VALUE,
                                  output char-val).
        ELSE
           RUN windows/l-item6.w (g_company,fi_code:SCREEN-VALUE,
                                  output char-val).

        if char-val <> "" then do:
           FIND ITEM WHERE RECID(ITEM) EQ int(char-val) NO-LOCK NO-ERROR.
           IF AVAIL ITEM THEN
              fi_code:SCREEN-VALUE = IF rd-CodeName EQ "N" THEN ITEM.i-name
                                     ELSE ITEM.i-no.
        end.                
      end. 
  END.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Sheet Calculation Parameters */
DO:
  op-max-yld = ?.
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach Dialog-Frame
ON LEAVE OF begin_mach IN FRAME Dialog-Frame /* Machine */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-m-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-m-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    ASSIGN {&displayed-objects}.
     
    ASSIGN
     op-code    = fi_code
     op-max-yld = tb_max-yld
     io-m-code  = begin_mach
     op-CodeOrName = rd-CodeName   .

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_code Dialog-Frame
ON LEAVE OF fi_code IN FRAME Dialog-Frame /* Select RM Item Name starting with */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
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

  begin_mach = io-m-code.

  RUN enable_UI.

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    IF io-m-code NE "" THEN APPLY "tab" TO begin_mach.
    IF NOT ip-use-code THEN DISABLE fi_code.
  END.

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
  DISPLAY begin_mach fi_code rd-CodeName tb_max-yld 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-25 begin_mach fi_code rd-CodeName tb_max-yld Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-code Dialog-Frame 
PROCEDURE valid-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF ip-use-code AND fi_code:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE "Code may not be blank..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fi_code.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-m-code Dialog-Frame 
PROCEDURE valid-m-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST mach
        WHERE mach.company EQ cocode
          AND mach.m-code  EQ begin_mach:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF NOT AVAIL mach OR begin_mach:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE TRIM(begin_mach:LABEL) + " invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO begin_mach.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

