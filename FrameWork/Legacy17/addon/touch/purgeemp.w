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

def stream st-mach.
def stream st-emp.
def stream st-emplogin.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_date end_date begin_emp end_emp btn_ok ~
btn_cancel RECT-27 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_emp end_emp 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 begin_date end_date begin_emp end_emp 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_cancel AUTO-END-KEY 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn_ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE begin_emp AS CHARACTER FORMAT "X(5)" 
     LABEL "From Employee ID" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_emp AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
     LABEL "To Employee ID" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 91 BY 4.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_date AT ROW 2.67 COL 23 COLON-ALIGNED
     end_date AT ROW 2.67 COL 55 COLON-ALIGNED
     begin_emp AT ROW 4.1 COL 23 COLON-ALIGNED
     end_emp AT ROW 4.1 COL 56 COLON-ALIGNED
     btn_ok AT ROW 7.19 COL 22
     btn_cancel AT ROW 7.19 COL 57
     RECT-27 AT ROW 1.24 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 1.24 COL 7
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.4 ROW 1
         SIZE 94.8 BY 9.19
         DEFAULT-BUTTON btn_cancel.


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
         TITLE              = "Employee Transaction Purge"
         HEIGHT             = 8.05
         WIDTH              = 95
         MAX-HEIGHT         = 24.91
         MAX-WIDTH          = 100.2
         VIRTUAL-HEIGHT     = 24.91
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
       btn_ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FILL-IN begin_date IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR FILL-IN begin_emp IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR FILL-IN end_date IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR FILL-IN end_emp IN FRAME FRAME-A
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Employee Transaction Purge */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Employee Transaction Purge */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON HELP OF FRAME FRAME-A
DO:
   def var char-val as cha no-undo.

   if focus:name = "begin_emp" or focus:name = "end_emp" then do:
      run windows/l-emp.w (input gcompany, focus:screen-value, output char-val).
      if char-val <> "" then focus:screen-value = char-val.   
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* From Date */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_emp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_emp C-Win
ON LEAVE OF begin_emp IN FRAME FRAME-A /* From Employee ID */
DO:
      assign {&self-name}.
      find employee where employee.employee = {&self-name} no-lock no-error.
      if not avail employee then do:
         message "Invalid Employee ID. Please enter correct employee id."
                   view-as alert-box error.
         return no-apply.
      end.
      assign end_emp:screen-value = {&self-name}
             end_emp = {&self-name}
              .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_cancel C-Win
ON CHOOSE OF btn_cancel IN FRAME FRAME-A /* Cancel */
DO:
    apply "close" to this-procedure. 

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok C-Win
ON CHOOSE OF btn_ok IN FRAME FRAME-A /* OK */
DO:
    if begin_date > end_date then do:
     message "To Date not be less than From Employee ID. "
              view-as alert-box error.
     return no-apply.
  end.
  if begin_emp > end_emp then do:
     message "To Employee ID can not be less than From Employee ID. "
              view-as alert-box error.
     return no-apply.
  end.

  run purge-trans.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* To Date */
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_emp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_emp C-Win
ON LEAVE OF end_emp IN FRAME FRAME-A /* To Employee ID */
DO:
      assign {&self-name}.
      find employee where employee.employee = {&self-name} no-lock no-error.
      if not avail employee then do:
         message "Invalid Employee ID. Please enter correct employee id."
                   view-as alert-box error.
         return no-apply.
      end.

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
   IF access-close THEN DO:
      APPLY "close" TO THIS-PROCEDURE.
      RETURN .
   END.

   find first employee no-lock no-error.
   begin_emp = if avail employee then employee.employee else "".
   find last employee no-lock no-error.
   end_emp = if avail employee then employee.employee else "".

   assign begin_date = today
          end_date = today
          .
   RUN enable_UI.

  {methods/nowait.i}
    {methods/setButton.i btn_cancel "Cancel"} /* added by script _nonAdm1Images1.p */
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
  DISPLAY begin_date end_date begin_emp end_emp 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_date end_date begin_emp end_emp btn_ok btn_cancel RECT-27 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purge-trans C-Win 
PROCEDURE purge-trans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    message "All machine transactions will be purged " skip
           "for period between " begin_date
          " - " end_date " and for employee " begin_emp " - " end_emp "."
          skip
          "Are you sure?" 
          view-as alert-box question button yes-no update ll-ans as log.
   if ll-ans then do:  
     disable triggers for load of machemp.
     disable triggers for load of machtran.
     disable triggers for load of emplogin.

     session:set-wait-state("general").
     output stream st-mach to value("system\machtran.d" + string(time)).
     output stream st-emp to value("system\machemp.d" + string(time)).
     output stream st-emplogin to value("system\emplogin.d" + string(time)).


     /* may need code not to delete notes but can't display anyway 
        even notes are not deleted */

     for EACH machemp EXCLUSIVE-LOCK WHERE /*machemp.table_rec_key = machtran.rec_key */
                      /*AND machemp.shift GE begin_shift
                      AND machemp.shift LE end_shift */
                          machemp.end_date >= begin_date 
                      and machemp.end_date <= end_date
                      and machemp.employee >= begin_emp
                      and machemp.employee <= end_emp 
                      AND machemp.posted :

         FOR EACH machtran WHERE    machtran.rec_key = machemp.table_rec_key
             AND machtran.posted :

             export stream st-mach machtran. 
             delete machtran.  
         end.
         export stream st-emp machemp.
         delete machemp.

     END.  /* each machtran */
     output stream st-mach close.
     output stream st-emp close.

     for each emplogin where emplogin.company EQ g_company
                         AND emplogin.end_date >= begin_date 
                         and emplogin.end_date <= end_date
                         and emplogin.employee >= begin_emp
                         and emplogin.employee <= end_emp :
         export stream st-emplogin emplogin.
         delete emplogin.                         
     end.

     session:set-wait-state("").
   end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

