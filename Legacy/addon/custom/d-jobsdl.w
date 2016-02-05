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
def input parameter v-recid as recid no-undo.
def input parameter ip-hdl as handle no-undo.

/* Local Variable Definitions ---                                       */
def var v-process as log no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_job-no begin_job-no2 ~
end_job-no end_job-no2 tb_pending Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_job-no begin_job-no2 end_job-no ~
end_job-no2 tb_pending 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Delete" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-9x":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-9x":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 9.76.

DEFINE VARIABLE tb_pending AS LOGICAL INITIAL no 
     LABEL "Include Pending Jobs?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_job-no AT ROW 3.86 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 3.86 COL 41 COLON-ALIGNED HELP
          "Enter Beginning Job Number" NO-LABEL
     end_job-no AT ROW 4.81 COL 29 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 4.81 COL 41 COLON-ALIGNED HELP
          "Enter Ending Job Number" NO-LABEL
     tb_pending AT ROW 7.43 COL 21
     Btn_OK AT ROW 11.48 COL 16
     Btn_Cancel AT ROW 11.48 COL 41
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 2.19 COL 6
     RECT-17 AT ROW 1 COL 1
     SPACE(0.00) SKIP(2.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Delete Selected Job(s)"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Delete Selected Job(s) */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no Dialog-Frame
ON LEAVE OF begin_job-no IN FRAME Dialog-Frame /* Beginning Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 Dialog-Frame
ON LEAVE OF begin_job-no2 IN FRAME Dialog-Frame
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Delete */
DO:
  run run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no Dialog-Frame
ON LEAVE OF end_job-no IN FRAME Dialog-Frame /* Ending Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 Dialog-Frame
ON LEAVE OF end_job-no2 IN FRAME Dialog-Frame
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_pending
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_pending Dialog-Frame
ON VALUE-CHANGED OF tb_pending IN FRAME Dialog-Frame /* Include Pending Jobs? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
def var i as int no-undo.
def var lv-dash as log init no no-undo.

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  find jobs where recid(jobs) eq v-recid no-lock.
  
  assign
   begin_job-no  = ""
   begin_job-no2 = "".
   
  do i = 1 to length(jobs.job):
    if substr(jobs.job,i,1) eq "-" then
      lv-dash = yes.
    else
    if lv-dash then
      begin_job-no2 = begin_job-no2 + substr(jobs.job,i,1).
    else
      begin_job-no  = begin_job-no  + substr(jobs.job,i,1).
  end.
  
  assign
   begin_job-no  = trim(begin_job-no)
   begin_job-no2 = trim(begin_job-no2)
   end_job-no    = begin_job-no
   end_job-no2   = begin_job-no2.
   
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY begin_job-no begin_job-no2 end_job-no end_job-no2 tb_pending 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-17 begin_job-no begin_job-no2 end_job-no end_job-no2 tb_pending 
         Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process Dialog-Frame 
PROCEDURE run-process :
/************************************************ custom/d-jobsdl.w 01/02 JLF */
/* Delete Selected Jobs from CAD/CAM Link                                     */
/******************************************************************************/

def var jobs-valid as log no-undo.
def var jobs-deleted as int no-undo.
 
assign
 v-process = no.
   
message "Are you sure you want to delete the jobs within the " +
        "selection parameters?"
        view-as alert-box question button yes-no update v-process.

if v-process then do:
  assign
   begin_job-no = fill(" ",6 - length(trim(begin_job-no))) +
                  trim(begin_job-no) + "-" + begin_job-no2
   end_job-no   = fill(" ",6 - length(trim(end_job-no))) +
                  trim(end_job-no) + "-" + end_job-no2.

  for each jobs
      where jobs.job ge begin_job-no
        and jobs.job le end_job-no
        and (tb_pending or jobs.cadcam_status ne "Pending")
      exclusive-lock
      
      transaction:  

    run Check_Jobs in ip-hdl (rowid(jobs), output jobs-valid).
        
    if jobs-valid then do:
      jobs-deleted = jobs-deleted + 1.
      delete jobs.
    end.
  end. /* each jobs */

  message trim(string(jobs-deleted,">>,>>9")) "Job(s) Deleted" view-as alert-box.
  apply "close" to this-procedure.
end.

return no-apply.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


