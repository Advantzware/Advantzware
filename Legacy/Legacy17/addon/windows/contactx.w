&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin-sman end-sman btn_ok btn_cancel ~
Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS begin-sman end-sman 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_cancel 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btn_ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin-sman AS CHARACTER FORMAT "X(256)":U 
     LABEL "Begin SalesRep" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end-sman AS CHARACTER FORMAT "X(256)":U INITIAL "zzzz" 
     LABEL "End SalesRep" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin-sman AT ROW 1.95 COL 21 COLON-ALIGNED
     end-sman AT ROW 1.95 COL 64 COLON-ALIGNED
     btn_ok AT ROW 4.57 COL 14
     btn_cancel AT ROW 4.57 COL 36
     Btn_Help AT ROW 4.57 COL 76
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.4 ROW 1
         SIZE 99.8 BY 7.86.


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
         TITLE              = "Download Contacts to Laptop"
         HEIGHT             = 8
         WIDTH              = 99.6
         MAX-HEIGHT         = 22.67
         MAX-WIDTH          = 100.2
         VIRTUAL-HEIGHT     = 22.67
         VIRTUAL-WIDTH      = 100.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
ASSIGN
       btn_cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       Btn_Help:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn_ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Download Contacts to Laptop */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Download Contacts to Laptop */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin-sman
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin-sman C-Win
ON LEAVE OF begin-sman IN FRAME FRAME-A /* Begin Salesman */
DO:
    assign begin-sman.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_cancel C-Win
ON CHOOSE OF btn_cancel IN FRAME FRAME-A /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME FRAME-A /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
MESSAGE "Help for File: {&FILE-NAME}":U VIEW-AS ALERT-BOX INFORMATION.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok C-Win
ON CHOOSE OF btn_ok IN FRAME FRAME-A /* OK */
DO:   /* must run from laptop only not from server */

   def var init-dir as cha no-undo.
   def var li-num-of-rec as int no-undo.
   def var li-num-of-notes as int no-undo.

   message "All existing contacts will be updated and new contacts will be created." skip
           "Are you sure you want to download contacts from SERVER?"
           view-as alert-box question button yes-no update ll-ans as log.
   if not ll-ans then return no-apply.

  /* init-dir = "users\" + USERID("NOSWEAT") + "\contactx.rpt".  */

   SESSION:SET-WAIT-STATE("GENERAL").
   status default "Connecting to Server. Please wait." .
   /* ======= connect to server ========*/
  /* connect -db r:/asiaddon/development/db/emptrack -ld emp_server -H Ntserver1 -N TCP -S emptrack-dev no-error.*/
/*  connect -pf lapemp.pf no-error. */
  /*{system/connect.i} .*/
  run system/lapnet.p. 
   if error-status:error then do:
      message "Not connected".
      return no-apply.
   end. 

   /*   output to value(init-dir). */
   if connected("emp_server") then
       run system/contdown.p (gcompany, input begin-sman, input end-sman, output li-num-of-rec, output li-num-of-notes) .

   /*
   for each emp_server.contact no-lock where emp_server.contact.sman >= begin-sman and
                                             emp_server.contact.sman <= end-sman
                                  :
       find emp_server.usergrps where emp_server.usergrps.usergrps = "sman" no-lock no-error.
       if not can-do(emp_server.usergrps.users,userid("Nosweat")) then next.

       find emp_server.usergrps where emp_server.usergrps.usergrps = userid("nosweat") no-lock no-error.
       if not can-do(emp_server.usergrps.users,contact.sman) then next.

       /*export contact.*/
       buffer-copy emp_server.contact to contact.

   end.                               
   output close.
   os-copy value(init-dir) value(session:temp-directory + "\contactx.rpt").
   */

   /* if connected("emp_server") then disconnect  emp_server. */
   /* if connected("nos_server") then disconnect  nos_server. */

   SESSION:SET-WAIT-STATE("").
   message "Download Completed. " li-num-of-rec   " Contact Updated. " skip
           "                    " li-num-of-notes " Notes   Updated."
      view-as alert-box.
   APPLY "CLOSE" TO THIS-PROCEDURE.


    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end-sman
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end-sman C-Win
ON LEAVE OF end-sman IN FRAME FRAME-A /* End Salesman */
DO:
    assign end-sman.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  /* this procedure must run from laptop only not on server */
   if index(dbparam("nosweat"),"-1") <= 0 then do:
      message "This Procedure Can Run Only From Laptop or Single User." view-as alert-box error.
      APPLY "CLOSE" TO THIS-PROCEDURE.
      return.
   end.

  find first sman where sman.company = gcompany 
                        use-index sman 
                     no-lock no-error.
  if avail sman then begin-sman = sman.sman.
  find last sman where sman.company = gcompany
                       use-index sman
                 no-lock no-error.
  if avail sman then end-sman = sman.sman.
  RUN enable_UI.

  {methods/nowait.i}
    {methods/setButton.i btn_cancel "Cancel"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i Btn_Help "Help"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i btn_ok "OK"} /* added by script _nonAdm1Images1.p */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY begin-sman end-sman 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin-sman end-sman btn_ok btn_cancel Btn_Help 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

