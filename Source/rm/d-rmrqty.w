&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
def input parameter ip-rowid as rowid no-undo.
def input parameter ip-mkbin as log no-undo.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
DEF BUFFER bfItem FOR ITEM.
{sys/inc/var.i new shared}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-26 tb_zer tb_neg tgCalculateEstimated ~
btn-recalc Btn-Cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_zer tb_neg tgCalculateEstimated 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 11 BY 1.19
     BGCOLOR 8 FONT 6.

DEFINE BUTTON btn-recalc 
     LABEL "Recalculate Qtys" 
     SIZE 22 BY 1.19
     FONT 6.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64.2 BY 6.19.

DEFINE VARIABLE tb_neg AS LOGICAL INITIAL no 
     LABEL "Delete Bins w/Negative Qty" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_zer AS LOGICAL INITIAL no 
     LABEL "Delete Bins w/Zero Qty" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE tgCalculateEstimated AS LOGICAL INITIAL no 
     LABEL "Recalc Estimated Materials?" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     tb_zer AT ROW 2.19 COL 15
     tb_neg AT ROW 3.86 COL 15
     tgCalculateEstimated AT ROW 5.52 COL 15 WIDGET-ID 2
     btn-recalc AT ROW 8.14 COL 8
     Btn-Cancel AT ROW 8.14 COL 45
     RECT-26 AT ROW 1 COL 1
     SPACE(0.00) SKIP(3.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Recalculate RM Qtys"
         CANCEL-BUTTON Btn-Cancel.


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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Recalculate RM Qtys */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Cancel Dialog-Frame
ON CHOOSE OF Btn-Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-recalc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-recalc Dialog-Frame
ON CHOOSE OF btn-recalc IN FRAME Dialog-Frame /* Recalculate Qtys */
DO:
  session:set-wait-state('general').

  IF tgCalculateEstimated AND Item.spare-char-2 NE "calc-est-qty" THEN DO:
      FIND bfItem WHERE ROWID(bfItem) = ROWID(ITEM) EXCLUSIVE-LOCK.
      bfItem.spare-char-2 = "calc-est-qty".
      RELEASE bfItem.
  END.
  IF tgCalculateEstimated = NO AND Item.spare-char-2 EQ "calc-est-qty" THEN DO:
      FIND bfItem WHERE ROWID(bfItem) = ROWID(ITEM) EXCLUSIVE-LOCK.
      bfItem.spare-char-2 = "".
      RELEASE bfItem.
  END.
  /* if ip-mkbin then run rm/rm-mkbin.p (recid(item)).  */
  IF ip-mkbin THEN
      RUN rm/rm-mkbinrc.p (ROWID(ITEM)).
  
  if tb_zer then
  for each rm-bin
      where rm-bin.company eq item.company
        and rm-bin.i-no    eq item.i-no
        and rm-bin.qty     eq 0:
    delete rm-bin.
  end.

  if tb_neg then
  for each rm-bin
      where rm-bin.company eq item.company
        and rm-bin.i-no    eq item.i-no
        and rm-bin.qty     lt 0:
        
    RUN rm/cre-tran.p (ROWID(rm-bin), "C", 0,"").

    delete rm-bin.
  end.

  run rm/rm-reset.p (recid(item)). 

  session:set-wait-state('').
  
  message frame {&FRAME-NAME}:title "completed..." view-as alert-box.
  
  apply "GO" to FRAME {&FRAME-NAME}.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_neg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_neg Dialog-Frame
ON VALUE-CHANGED OF tb_neg IN FRAME Dialog-Frame /* Delete Bins w/Negative Qty */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_zer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zer Dialog-Frame
ON VALUE-CHANGED OF tb_zer IN FRAME Dialog-Frame /* Delete Bins w/Zero Qty */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgCalculateEstimated
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgCalculateEstimated Dialog-Frame
ON VALUE-CHANGED OF tgCalculateEstimated IN FRAME Dialog-Frame /* Recalc Estimated Materials? */
DO:
    assign {&self-name}.
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
  
  find item where rowid(item) eq ip-rowid no-lock.

  assign
   cocode  = item.company
   locode  = g_loc.
  IF Item.spare-char-2 = "calc-est-qty" THEN
      tgCalculateEstimated = YES.
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
  DISPLAY tb_zer tb_neg tgCalculateEstimated 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-26 tb_zer tb_neg tgCalculateEstimated btn-recalc Btn-Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

