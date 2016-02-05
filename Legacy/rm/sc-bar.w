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
{methods/defines/hndldefs.i}

{custom/gcompany.i}
{custom/gloc.i}
def var x as int no-undo.
def var y as int no-undo.
def var v-num as int no-undo.
def var v-text as char extent 4 no-undo.
def var v-num-mats as int no-undo.
def var v-job-no like rm-rcpt.job-no no-undo.
def var v-job-no2 as char format "x(2)" no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-2 Scan_Label BUTTON-1 Qty Cost ~
Btn_Close 
&Scoped-Define DISPLAYED-OBJECTS Scan_Label From_Whse From_Bin From_Tag Qty ~
Cost 

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
     SIZE 24 BY 2.62.

DEFINE BUTTON BUTTON-2  NO-FOCUS
     LABEL "Simulate Qty" 
     SIZE 24 BY 2.62.

DEFINE VARIABLE Cost AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Cost" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE From_Bin AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE From_Tag AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tag" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE From_Whse AS CHARACTER FORMAT "X(256)":U 
     LABEL "Whse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Qty AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Qty" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE Scan_Label AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scan Label" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-2 AT ROW 5.76 COL 94
     Scan_Label AT ROW 1.71 COL 14 COLON-ALIGNED
     BUTTON-1 AT ROW 1.95 COL 94
     From_Whse AT ROW 3.38 COL 14 COLON-ALIGNED
     From_Bin AT ROW 3.38 COL 38 COLON-ALIGNED
     From_Tag AT ROW 3.38 COL 63 COLON-ALIGNED
     Qty AT ROW 5.05 COL 14 COLON-ALIGNED
     Cost AT ROW 6.71 COL 14 COLON-ALIGNED
     Btn_Close AT ROW 9.57 COL 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 121.2 BY 12.62
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
         TITLE              = "Scan Physical Count(Online)"
         HEIGHT             = 12.81
         WIDTH              = 122.8
         MAX-HEIGHT         = 20.38
         MAX-WIDTH          = 130.4
         VIRTUAL-HEIGHT     = 20.38
         VIRTUAL-WIDTH      = 130.4
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
ASSIGN 
       BUTTON-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN From_Bin IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN From_Tag IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN From_Whse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Scan Physical Count(Online) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Scan Physical Count(Online) */
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
  assign Scan_Label:screen-value = "7375/200c 60x80/500.0/00737501".

  assign v-text = ""
         v-num = 1.
  do x = 1 to length(trim(Scan_Label:screen-value)):
    if substring(Scan_Label:screen-value,x,1) ne "/" then
      assign v-text[v-num] = v-text[v-num] + substring(Scan_Label:screen-value,x,1).
    else
      v-num = v-num + 1.
  end.

  assign Scan_Label:screen-value = v-text[2]
         Qty:screen-value = v-text[3].

  find first item where item.company eq gcompany
                    and item.i-no eq v-text[2] 
                  no-lock no-error.
  find first rm-bin where rm-bin.company eq gcompany
                      and rm-bin.i-no eq v-text[2]
                      and rm-bin.tag eq v-text[4]
                    no-lock no-error.
  if not avail item and not avail rm-bin then
  do:
    message "Invalid Item/Tag Entered. Please Check Label." view-as alert-box.
    assign Scan_Label:screen-value = ""
           Qty:screen-value = "".
  end.
  else
  do:
    assign From_Whse:screen-value = rm-bin.loc
           From_Bin:screen-value = rm-bin.loc-bin 
           From_Tag:screen-value = rm-bin.tag.
    APPLY "TAB".
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Simulate Qty */
DO: 
/*
  assign Scan_Job:screen-value = "5414-0".
  
  assign v-job-no = ""
         v-job-no2 = "".
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
  for each job-mat where job-mat.company eq gcompany
                      and job-mat.i-no eq rm-rcpt.i-no
                      and job-mat.job-no eq rm-rcpt.job-no
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
      assign rm-rdtl.s-num = job-mat.frm
             rm-rdtl.b-num = job-mat.blank-no
             rm-rdtl.pass = job-mat.pass.
  end.
  else
  do:
    ENABLE Scan_form with frame {&frame-name}.
    Scan_Form:hidden eq false.
    ENABLE Scan_Blank with frame {&frame-name}.
    Scan_Blank:hidden eq false.
    ENABLE Scan_Pass with frame {&frame-name}.
    Scan_Pass:hidden eq false.
  end.
  
  APPLY "TAB".
*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME From_Bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL From_Bin C-Win
ON LEAVE OF From_Bin IN FRAME DEFAULT-FRAME /* Bin */
DO:
  find first rm-bin where rm-bin.company eq gcompany
                      and rm-bin.loc     eq From_Whse:screen-value
                      and rm-bin.loc-bin eq From_Bin:screen-value
                      and rm-bin.i-no    eq Scan_Label:screen-value
                 no-lock no-error.
  if not avail rm-bin then
  do:
    message "Invalid Warehouse and/or Bin Entered. Please Try Again." view-as alert-box.
    assign From_Bin:screen-value = "".
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME From_Tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL From_Tag C-Win
ON LEAVE OF From_Tag IN FRAME DEFAULT-FRAME /* Tag */
DO:
  find first rm-bin where rm-bin.company eq gcompany
                      and rm-bin.loc     eq From_Whse:screen-value
                      and rm-bin.loc-bin eq From_Bin:screen-value
                      and rm-bin.i-no    eq Scan_Label:screen-value
                      and rm-bin.tag     eq From_Tag:screen-value
                 no-lock no-error.
  if not avail rm-bin then
  do:
    message "Tag Number does not Exist in this Location. Please Try Again." view-as alert-box.
    assign From_Tag:screen-value = "".
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME From_Whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL From_Whse C-Win
ON LEAVE OF From_Whse IN FRAME DEFAULT-FRAME /* Whse */
DO:
  find first loc where loc.company eq gcompany
                   and loc.loc eq From_Whse:screen-value
                 no-lock no-error.
  if not avail loc then
  do:
    message "Invalid Warehouse Entered. Please Try Again." view-as alert-box.
    assign From_Whse:screen-value = "".
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Scan_Label
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Scan_Label C-Win
ON LEAVE OF Scan_Label IN FRAME DEFAULT-FRAME /* Scan Label */
DO:
/*
  find first item where item.company eq gcompany
                    and item.i-no eq Scan_label:screen-value 
                  no-lock no-error.
  if not avail item and not(button-1:selected) then
  do:
    message "Invalid Item Entered. Please Item on Label." view-as alert-box.
    assign Scan_Label:screen-value = ""
           Qty:screen-value = "".
  end.
*/
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
  DISPLAY Scan_Label From_Whse From_Bin From_Tag Qty Cost 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BUTTON-2 Scan_Label BUTTON-1 Qty Cost Btn_Close 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


