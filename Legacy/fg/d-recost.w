&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: d-recost.w

  Description: Recalculates Cost for a given item

  Input Parameters:
      rowid of FG item

  Output Parameters:
      <none>

  Author: Last edited by BV  10/23/14

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ipri-itemfg AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-26 begin_i-no end_i-no begin_cust-no ~
end_cust-no tb_inactive btnRecalc btnCancel 
&Scoped-Define DISPLAYED-OBJECTS begin_i-no end_i-no begin_cust-no ~
end_cust-no tb_inactive 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 22 BY 1.19
     BGCOLOR 8 .

DEFINE BUTTON btnRecalc 
     LABEL "Recalculate Cost" 
     SIZE 22 BY 1.19.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 5.71.

DEFINE VARIABLE tb_inactive AS LOGICAL INITIAL no 
     LABEL "Include Inactive Items" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_i-no AT ROW 2.19 COL 22 COLON-ALIGNED HELP
          "Enter Beginning Order Number" WIDGET-ID 4
     end_i-no AT ROW 2.19 COL 65 COLON-ALIGNED HELP
          "Enter Ending Item Number" WIDGET-ID 8
     begin_cust-no AT ROW 3.76 COL 22 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 2
     end_cust-no AT ROW 3.76 COL 65 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 6
     tb_inactive AT ROW 5.43 COL 25 WIDGET-ID 10
     btnRecalc AT ROW 7.67 COL 21
     btnCancel AT ROW 7.67 COL 53
     RECT-26 AT ROW 1.14 COL 1.8
     SPACE(0.39) SKIP(2.38)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Recalculate FG Cost"
         DEFAULT-BUTTON btnRecalc CANCEL-BUTTON btnCancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Recalculate FG Cost */
DO:
    DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE hFocus AS WIDGET-HANDLE NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        hFocus = FOCUS.
        CASE hFocus:NAME :
            WHEN 'begin_i-no' OR WHEN 'end_i-no' THEN DO:
                RUN windows/l-itemfg.w (INPUT g_company, 
                                        INPUT begin_cust-no,
                                        INPUT hFocus:SCREEN-VALUE, 
                                        OUTPUT cReturn).
                hFocus:SCREEN-VALUE = ENTRY(1,cReturn).
                IF hFocus:NAME EQ 'begin_i-no' THEN
                    APPLY 'entry' TO end_i-no.
                ELSE
                    APPLY 'entry' TO begin_cust-no.
            END. 
            WHEN 'begin_cust-no' OR WHEN 'end_cust-no' THEN DO:
                RUN windows/l-cust.w (INPUT g_company, 
                                      INPUT hFocus:SCREEN-VALUE,
                                      OUTPUT cReturn).
                hFocus:SCREEN-VALUE = ENTRY(1,cReturn).
                IF hFocus:NAME EQ 'begin_cust-no' THEN
                    APPLY 'entry' TO end_cust-no.
            END.
        END. /*case*/
    END. /*do with frame*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Recalculate FG Cost */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no Dialog-Frame
ON LEAVE OF begin_cust-no IN FRAME Dialog-Frame /* Beginning Customer# */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no Dialog-Frame
ON LEAVE OF begin_i-no IN FRAME Dialog-Frame /* Beginning Item# */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRecalc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRecalc Dialog-Frame
ON CHOOSE OF btnRecalc IN FRAME Dialog-Frame /* Recalculate Cost */
DO:
    DEFINE VARIABLE iCount AS INTEGER     NO-UNDO.
    SESSION:SET-WAIT-STATE('general').
    
    RUN RunProcess(OUTPUT iCount).
    
    SESSION:SET-WAIT-STATE('').
    MESSAGE FRAME {&FRAME-NAME}:TITLE "Recalculation completed. " SKIP
        iCount " item(s) processed." 
        VIEW-AS ALERT-BOX.
    APPLY "GO" to FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no Dialog-Frame
ON LEAVE OF end_cust-no IN FRAME Dialog-Frame /* Ending Customer# */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no Dialog-Frame
ON LEAVE OF end_i-no IN FRAME Dialog-Frame /* Ending Item# */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inactive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inactive Dialog-Frame
ON VALUE-CHANGED OF tb_inactive IN FRAME Dialog-Frame /* Include Inactive Items */
DO:
  ASSIGN {&self-name}.
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
  
    FIND itemfg 
        WHERE ROWID(itemfg) EQ ipri-itemfg 
        NO-LOCK.
    
    ASSIGN
        begin_i-no = itemfg.i-no
        end_i-no = itemfg.i-no
        cocode  = itemfg.company
        locode  = g_loc
        .
    IF itemfg.cust-no NE '' THEN
        ASSIGN
            begin_cust-no = itemfg.cust-no
            end_cust-no = itemfg.cust-no
            .
    ELSE
        ASSIGN
            begin_cust-no = ''
            end_cust-no = 'zzzzzzzz'
            .
    tb_inactive = itemfg.stat NE 'A'.
  
  RUN enable_UI.
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
  DISPLAY begin_i-no end_i-no begin_cust-no end_cust-no tb_inactive 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-26 begin_i-no end_i-no begin_cust-no end_cust-no tb_inactive 
         btnRecalc btnCancel 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RunProcess Dialog-Frame 
PROCEDURE RunProcess :
/*------------------------------------------------------------------------------
  Purpose:   Executes the recalc cost program for range of items  
  Parameters:  Outputs a count of items processed
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER opiCount AS INTEGER NO-UNDO.

DEFINE BUFFER bf-itemfg FOR itemfg.
   
opiCount = 0.  
FOR EACH bf-itemfg
    WHERE bf-itemfg.company EQ cocode
      AND bf-itemfg.i-no GE begin_i-no
      AND bf-itemfg.i-no LE end_i-no
      AND bf-itemfg.cust-no GE begin_cust-no
      AND bf-itemfg.cust-no LE end_cust-no
      AND (bf-itemfg.stat EQ 'A' OR tb_inactive)
    NO-LOCK:
    opiCount = opiCount + 1.
    RUN fg/updfgcst.p (INPUT bf-itemfg.i-no).
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

