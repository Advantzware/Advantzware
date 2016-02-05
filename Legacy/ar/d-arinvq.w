&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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
DEF OUTPUT PARAM op-selection AS cha NO-UNDO.


{methods/defines/hndldefs.i}
{methods/defines/globdefs.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}
    
assign
 cocode = gcompany
 locode = gloc.

DEFINE VARIABLE v-prgmname LIKE prgrms.prgmname NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("NOSWEAT") + "..".
ELSE
ASSIGN
  period_pos = INDEX(PROGRAM-NAME(1),".")
  v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Detail Btn_Summary td-Detail

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Detail AUTO-GO
     LABEL "&Detail" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Summary AUTO-GO
     LABEL "&Summary" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE td-Detail AS LOGICAL INITIAL no 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.



/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_Detail AT ROW 1.95 COL 12
     Btn_Summary AT ROW 1.95 COL 34
     td-Detail AT ROW 1.95 COL 12
     SPACE(16.19) SKIP(1.95)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Invoice Lookup"
         DEFAULT-BUTTON Btn_Detail CANCEL-BUTTON Btn_Summary WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX Dialog-Frame                                 */

ASSIGN 
       td-Detail:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE
       td-Detail:HIDDEN IN FRAME Dialog-Frame         = TRUE.
      

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Invoice Lookup */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Detail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Detail Dialog-Frame
ON CHOOSE OF Btn_Detail IN FRAME Dialog-Frame /* Detail */
DO:
  op-selection = "Detail".
  td-Detail:HIDDEN IN FRAME Dialog-Frame = TRUE .
  td-Detail:SCREEN-VALUE = "YES".
  td-Detail = YES.
                            
  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).       /*task# 01291407*/
  APPLY 'go' TO FRAME {&FRAME-NAME}.   
  APPLY "close" TO THIS-PROCEDURE.
     RETURN .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Summary
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Summary Dialog-Frame
ON CHOOSE OF Btn_Summary IN FRAME Dialog-Frame /* Summary */
DO:
  op-selection = "Summary".
  td-Detail:HIDDEN IN FRAME Dialog-Frame         = TRUE.
  td-Detail:SCREEN-VALUE = "No".
  td-Detail = NO. 
                            
  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).           /*task# 01291407*/
   APPLY 'go' TO FRAME {&FRAME-NAME}. 
   APPLY "close" TO THIS-PROCEDURE.
     RETURN .
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
 /* WAIT-FOR GO OF FRAME {&FRAME-NAME}.*/
  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:  
    {custom/usrprint.i}
                                                        /*task# 01291407*/
    IF td-Detail:SCREEN-VALUE EQ "YES" THEN do: 
        APPLY "entry" TO Btn_Detail .
    END.
    ELSE do: 
        APPLY "entry" TO Btn_Summary .
    END.
    td-Detail:HIDDEN = TRUE.
  END.
 
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.

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
  ENABLE Btn_Detail Btn_Summary td-Detail
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

