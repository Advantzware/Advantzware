&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
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

/* Local Variable Definitions ---                                       */
def input param ip-title as cha no-undo.
def input param ip-style as cha no-undo.
def input param ip-flute as cha no-undo.
def input param ip-code as cha no-undo.
def output param op-total as dec no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS reftable.val[1] reftable.val[2] ~
reftable.val[3] reftable.val[4] reftable.val[5] reftable.val[6] ~
reftable.val[7] reftable.val[8] reftable.val[9] reftable.val[10] ~
reftable.val[11] reftable.val[12] 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}val[1] ~{&FP2}val[1] ~{&FP3}~
 ~{&FP1}val[2] ~{&FP2}val[2] ~{&FP3}~
 ~{&FP1}val[3] ~{&FP2}val[3] ~{&FP3}~
 ~{&FP1}val[4] ~{&FP2}val[4] ~{&FP3}~
 ~{&FP1}val[5] ~{&FP2}val[5] ~{&FP3}~
 ~{&FP1}val[6] ~{&FP2}val[6] ~{&FP3}~
 ~{&FP1}val[7] ~{&FP2}val[7] ~{&FP3}~
 ~{&FP1}val[8] ~{&FP2}val[8] ~{&FP3}~
 ~{&FP1}val[9] ~{&FP2}val[9] ~{&FP3}~
 ~{&FP1}val[10] ~{&FP2}val[10] ~{&FP3}~
 ~{&FP1}val[11] ~{&FP2}val[11] ~{&FP3}~
 ~{&FP1}val[12] ~{&FP2}val[12] ~{&FP3}
&Scoped-define ENABLED-TABLES reftable
&Scoped-define FIRST-ENABLED-TABLE reftable
&Scoped-Define ENABLED-OBJECTS RECT-16 btn-done btn-cancel 
&Scoped-Define DISPLAYED-FIELDS reftable.val[1] reftable.val[2] ~
reftable.val[3] reftable.val[4] reftable.val[5] reftable.val[6] ~
reftable.val[7] reftable.val[8] reftable.val[9] reftable.val[10] ~
reftable.val[11] reftable.val[12] 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-done 
     LABEL "&Save" 
     SIZE 15 BY 1.14.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 31 BY 13.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     reftable.val[1] AT ROW 1.24 COL 6 COLON-ALIGNED
          LABEL "1"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     reftable.val[2] AT ROW 2.24 COL 6 COLON-ALIGNED
          LABEL "2"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     reftable.val[3] AT ROW 3.24 COL 6 COLON-ALIGNED
          LABEL "3"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     reftable.val[4] AT ROW 4.24 COL 6 COLON-ALIGNED
          LABEL "4"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     reftable.val[5] AT ROW 5.24 COL 6 COLON-ALIGNED
          LABEL "5"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     reftable.val[6] AT ROW 6.24 COL 6 COLON-ALIGNED
          LABEL "6"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     reftable.val[7] AT ROW 7.24 COL 6 COLON-ALIGNED
          LABEL "7"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     reftable.val[8] AT ROW 8.24 COL 6 COLON-ALIGNED
          LABEL "8"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     reftable.val[9] AT ROW 9.24 COL 6 COLON-ALIGNED
          LABEL "9"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     reftable.val[10] AT ROW 10.29 COL 6 COLON-ALIGNED
          LABEL "10"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     reftable.val[11] AT ROW 11.29 COL 6 COLON-ALIGNED
          LABEL "11"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     reftable.val[12] AT ROW 12.29 COL 6 COLON-ALIGNED
          LABEL "12"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     btn-done AT ROW 3.38 COL 44
     btn-cancel AT ROW 5.52 COL 44
     RECT-16 AT ROW 1 COL 1
     SPACE(33.19) SKIP(1.22)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Style Matrix"
         CANCEL-BUTTON btn-cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN reftable.val[10] IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN reftable.val[11] IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN reftable.val[12] IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN reftable.val[1] IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN reftable.val[2] IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN reftable.val[3] IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN reftable.val[4] IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN reftable.val[5] IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN reftable.val[6] IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN reftable.val[7] IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN reftable.val[8] IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN reftable.val[9] IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Style Matrix */
DO:
  apply "choose" to btn-done. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Style Matrix */
DO:
  op-total = ?.
 
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
  run calc-total.
  
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-done Dialog-Frame
ON CHOOSE OF btn-done IN FRAME Dialog-Frame /* Save */
DO:  
  do with frame {&frame-name} :
    assign reftable.val[1 for 12].
  end.
  
  run calc-total.

  apply "GO" to frame {&frame-name}.
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

  frame {&frame-name}:title = ip-title.
   
  find first reftable where reftable.reftable = "STYFLU" 
                        and reftable.company = ip-style
                        and reftable.loc = ip-flute
                        and reftable.code = ip-code
                        no-error.
  if not avail reftable then do:
    
     create reftable.
     assign reftable.reftable = "STYFLU" 
            reftable.company = ip-style
            reftable.loc = ip-flute
            reftable.code = ip-code
            . 
  end.                       
  
  /*RUN enable_UI.*/
  IF AVAILABLE reftable THEN 
    DISPLAY reftable.val[1] reftable.val[2] reftable.val[3] reftable.val[4] 
          reftable.val[5] reftable.val[6] reftable.val[7] reftable.val[8] 
          reftable.val[9] reftable.val[10] reftable.val[11] reftable.val[12] 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-16 reftable.val[1] reftable.val[2] reftable.val[3] btn-done 
         reftable.val[4] reftable.val[5] btn-cancel reftable.val[6] 
         reftable.val[7] reftable.val[8] reftable.val[9] reftable.val[10] 
         reftable.val[11] reftable.val[12] 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-total Dialog-Frame 
PROCEDURE calc-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def var i as int no-undo.


op-total = 0.
do i = 1 to 12:
  op-total = op-total + reftable.val[i] .
end.
reftable.val[13] = op-total.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
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
  IF AVAILABLE reftable THEN 
    DISPLAY reftable.val[1] reftable.val[2] reftable.val[3] reftable.val[4] 
          reftable.val[5] reftable.val[6] reftable.val[7] reftable.val[8] 
          reftable.val[9] reftable.val[10] reftable.val[11] reftable.val[12] 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-16 reftable.val[1] reftable.val[2] reftable.val[3] 
         reftable.val[4] reftable.val[5] reftable.val[6] reftable.val[7] 
         reftable.val[8] reftable.val[9] reftable.val[10] reftable.val[11] 
         reftable.val[12] btn-done btn-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


