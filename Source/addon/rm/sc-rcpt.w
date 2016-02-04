&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/*
{methods/defines/hndldefs.i}
*/
{touch/touchdef.i}

{custom/gcompany.i}
{custom/gloc.i}
def var x as int no-undo.
def var y as int no-undo.
def var v-num as int no-undo.
def var v-text as char extent 4 no-undo.
def var v-num-mats as int no-undo.
def var v-job-no like rm-rcpt.job-no no-undo.
def var v-job-no2 as char format "x(2)" no-undo.

DEFINE var Frame_Handle AS WIDGET-HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Scan_Label BUTTON-1 Scan_Job BUTTON-2 ~
Btn_Close 
&Scoped-Define DISPLAYED-OBJECTS Scan_Label Scan_Job 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Close 
     LABEL "Close" 
     SIZE 48 BY 3.1.

DEFINE BUTTON BUTTON-1 
     LABEL "Simulate Label Scan" 
     SIZE 24 BY 1.

DEFINE BUTTON BUTTON-2 
     LABEL "Simulate Job# Scan" 
     SIZE 24 BY 1.

DEFINE VARIABLE Scan_Blank AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scan Blank" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Scan_Form AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scan Form" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Scan_Job AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scan Job#" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE Scan_Label AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scan Label" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE Scan_Pass AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scan Pass" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Scan_Label AT ROW 1.71 COL 14.6 COLON-ALIGNED
     BUTTON-1 AT ROW 1.71 COL 83.6
     Scan_Job AT ROW 3.86 COL 14.2 COLON-ALIGNED
     BUTTON-2 AT ROW 3.86 COL 83.6
     Scan_Form AT ROW 5.29 COL 14 COLON-ALIGNED
     Scan_Pass AT ROW 5.29 COL 45.6 COLON-ALIGNED
     Scan_Blank AT ROW 5.29 COL 76.6 COLON-ALIGNED
     Btn_Close AT ROW 7.19 COL 30.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.8 BY 10.05
         DEFAULT-BUTTON Btn_Close.


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
         TITLE              = "Add Receipt"
         COLUMN             = 4.8
         ROW                = 12.67
         HEIGHT             = 10.67
         WIDTH              = 110.8
         MAX-HEIGHT         = 18.95
         MAX-WIDTH          = 137.2
         VIRTUAL-HEIGHT     = 18.95
         VIRTUAL-WIDTH      = 137.2
         RESIZE             = yes
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


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FILL-IN Scan_Blank IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Scan_Blank:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN Scan_Form IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Scan_Form:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN Scan_Pass IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Scan_Pass:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Add Receipt */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Add Receipt */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close C-Win
ON CHOOSE OF Btn_Close IN FRAME DEFAULT-FRAME /* Close */
DO:
  if avail rm-rcpt and rm-rcpt.i-no ne "" then
  do:
    find first job-mat where job-mat.company eq gcompany
                         and job-mat.i-no eq rm-rcpt.i-no
                         and job-mat.job-no eq rm-rcpt.job-no
                         and job-mat.job-no2 eq rm-rcpt.job-no2
                         and job-mat.frm eq int(Scan_Form:screen-value)
                         and job-mat.blank-no eq int(Scan_Blank:screen-value)
                         and job-mat.pass eq int(Scan_Pass:screen-value)
                       no-lock no-error.
    if not avail job-mat then
    do:
      message "Job Material Record not Found.  Please Try Again." view-as alert-box.
      for each rm-rdtl where rm-rdtl.company eq gcompany
                         and rm-rdtl.r-no eq rm-rcpt.r-no:
        delete rm-rdtl.
      end.
      delete rm-rcpt.
    end.
  end.
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Simulate Label Scan */
DO:

/*
  assign Scan_Label:screen-value = "7375/200c 60x80/500.0/00737501".
*/
  assign Scan_Label:screen-value = "7376/200C 60X80/200.0/00737601".

  assign v-text = ""
         v-num = 1.
  do x = 1 to length(trim(Scan_Label:screen-value)):
    if substring(Scan_Label:screen-value,x,1) ne "/" then
      assign v-text[v-num] = v-text[v-num] + substring(Scan_Label:screen-value,x,1).
    else
      v-num = v-num + 1.
  end.

  find first po-ordl where po-ordl.company eq gcompany
                       and po-ordl.po-no eq int(v-text[1])
                       and po-ordl.i-no eq v-text[2] 
                     no-lock no-error.
  if not avail po-ordl then
  do:
    message "Item is not on PO. Please Check Label." view-as alert-box.
    assign Scan_Label:screen-value = ""
           Scan_Job:screen-value = "".
  end.
  else
    APPLY "TAB".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Simulate Job# Scan */
DO: 
  assign Scan_Job:screen-value = "5414-0".
  
  assign v-job-no = ""
         v-job-no2 = ""
         Scan_Blank:screen-value = "0".
         
  do x = 1 to length(trim(Scan_Job:screen-value)):
    if substring(trim(Scan_Job:screen-value),x,1) ne "-" then
      v-job-no = v-job-no + substring(trim(Scan_Job:screen-value),x,1).
    else
      leave.
  end.

  y = x + 1.  
  do x = y to length(trim(Scan_Job:screen-value)):
      v-job-no2 = v-job-no2 + substring(trim(Scan_Job:screen-value),x,1).
  end.
  
  v-job-no =  fill(" ",6 - length(trim(string(v-job-no)))) +
                        trim(string(v-job-no)).

  find first job-mat where job-mat.company eq gcompany 
                       and job-mat.job-no eq v-job-no
                       and job-mat.job-no2 eq int(v-job-no2)
                       and job-mat.i-no eq v-text[2]
                     no-lock no-error.
  if not avail job-mat then
  do:
    message "Item is not on Job. Please Check Label and Job Number." view-as alert-box.
    assign Scan_Label:screen-value = ""
           Scan_Job:screen-value = "".
    APPLY 'TAB'.
  end.
  
  x = 1.
  FIND LAST rm-rcpt USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAILABLE rm-rcpt THEN
    x = rm-rcpt.r-no + 1.
  FIND LAST rm-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAILABLE rm-rcpth AND rm-rcpth.r-no >= x THEN
    x = rm-rcpth.r-no + 1.

  create rm-rcpt.
  assign rm-rcpt.r-no = x
         rm-rcpt.trans-date = today
         rm-rcpt.rita-code = "I"
         rm-rcpt.company = gcompany
         rm-rcpt.loc = gloc 
         rm-rcpt.i-no = v-text[2]
         rm-rcpt.job-no = v-job-no
         rm-rcpt.job-no2 = int(v-job-no2).
  
  create rm-rdtl.
  assign rm-rdtl.r-no       = rm-rcpt.r-no
         rm-rdtl.company    = gcompany
         rm-rdtl.loc        = gloc
         rm-rdtl.rita-code  = "I"
         rm-rdtl.job-no     = rm-rcpt.job-no
         rm-rdtl.job-no2    = rm-rcpt.job-no2.

  find first rm-bin where rm-bin.company eq gcompany
                      and rm-bin.i-no eq rm-rcpt.i-no
                      and rm-bin.loc eq gloc
                      and rm-bin.tag eq v-text[4] no-lock no-error.
  if avail rm-bin then
    assign rm-rdtl.tag = v-text[4]
           rm-rdtl.loc = rm-bin.loc
           rm-rdtl.loc-bin = rm-bin.loc-bin
           rm-rdtl.cost = rm-bin.cost
           rm-rdtl.qty = rm-bin.qty.

  /* Find out if we need Form/Blank/Pass */
  find first job where job.company  eq gcompany
                   and job.job-no   eq rm-rcpt.job-no
                   and job.job-no2 eq rm-rcpt.job-no2
                 no-lock no-error.
                 
  for each job-mat where job-mat.company eq gcompany
                     and job-mat.job     eq job.job 
                     and job-mat.i-no    eq rm-rcpt.i-no
                     and job-mat.job-no  eq rm-rcpt.job-no
                     and job-mat.job-no2 eq rm-rcpt.job-no2
                   no-lock:
    assign v-num-mats = v-num-mats + 1.
  end.
  
  if v-num-mats eq 1 then
  do:
    find first job-mat where job-mat.company eq gcompany
                         and job-mat.i-no eq rm-rcpt.i-no
                         and job-mat.job-no eq rm-rcpt.job-no
                         and job-mat.job-no2 eq rm-rcpt.job-no2
                       no-lock no-error.
    if avail job-mat then
    do:
      assign rm-rdtl.s-num = job-mat.frm
             rm-rdtl.b-num = job-mat.blank-no
             rm-rdtl.pass = job-mat.pass.
    end.
  end.
  else
  do:
    run value("touch/numeric.w") PERSISTENT SET h_numeric (Frame_Handle).
    ENABLE Scan_form with frame {&frame-name}.
    Scan_Form:hidden eq false.
/*
    ENABLE Scan_Blank with frame {&frame-name}.
    Scan_Blank:hidden eq false.
*/
    ENABLE Scan_Pass with frame {&frame-name}.
    Scan_Pass:hidden eq false.
  end.
  
  APPLY "TAB".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Scan_Blank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Scan_Blank C-Win
ON LEAVE OF Scan_Blank IN FRAME DEFAULT-FRAME /* Scan Blank */
DO:
  assign rm-rdtl.b-num = int(Scan_Blank:screen-value).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Scan_Form
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Scan_Form C-Win
ON LEAVE OF Scan_Form IN FRAME DEFAULT-FRAME /* Scan Form */
DO:
  assign rm-rdtl.s-num = int(Scan_Form:screen-value).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Scan_Pass
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Scan_Pass C-Win
ON LEAVE OF Scan_Pass IN FRAME DEFAULT-FRAME /* Scan Pass */
DO:
  assign rm-rdtl.pass = int(Scan_Pass:screen-value).
  release rm-rcpt.
  release rm-rdtl.
 
  /****** This is where we would need to post ********/

  assign Scan_Label:screen-value = ""
         Scan_Job:screen-value = ""
         Scan_Form:screen-value = ""
         Scan_Blank:screen-value = ""
         Scan_Pass:screen-value = "".
  
  APPLY 'TAB'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

{custom/getcmpny.i}
{custom/getloc.i}

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
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
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
  DISPLAY Scan_Label Scan_Job 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Scan_Label BUTTON-1 Scan_Job BUTTON-2 Btn_Close 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


