&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: addon\touch\d-clocki.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-clock-in AS LOG NO-UNDO.
/* Local Variable Definitions ---                                       */

DEF VAR cocode AS CHAR NO-UNDO.
DEF BUFFER b-emplogin FOR emplogin.

{touch/touchdef.i}
{custom/shftdefs.i}

/* internal procedures */
{custom/shftproc.i}
{custom/emprate.i}
{custom/globdefs.i}
ASSIGN company_code = g_company
       cocode = g_company.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-employee btn-next btn-close 
&Scoped-Define DISPLAYED-OBJECTS v-clockio v-employee 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-close AUTO-END-KEY 
     LABEL "C&lose" 
     SIZE 25 BY 2.52.

DEFINE BUTTON btn-next 
     LABEL "Next Clock In" 
     SIZE 25 BY 2.52.

DEFINE VARIABLE v-clockio AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE v-employee AS CHARACTER FORMAT "X(256)":U 
     LABEL "Employee ID" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     FONT 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     v-clockio AT ROW 1.71 COL 26.2 COLON-ALIGNED NO-LABEL
     v-employee AT ROW 3.62 COL 24 COLON-ALIGNED
     btn-next AT ROW 6 COL 8
     btn-close AT ROW 6 COL 42
     SPACE(3.79) SKIP(0.80)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Touch Screen Clock In".


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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN v-clockio IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Touch Screen Clock In */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-close Dialog-Frame
ON CHOOSE OF btn-close IN FRAME Dialog-Frame /* Close */
DO:
  APPLY "go" TO FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-next Dialog-Frame
ON CHOOSE OF btn-next IN FRAME Dialog-Frame /* Next Clock In */
DO:
   IF  v-employee:SCREEN-VALUE <> "" THEN run DO-clock-inout NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /* v-employee:SCREEN-VALUE = "".
   APPLY "entry" TO v-employee.
   RETURN NO-APPLY. */
    APPLY "choose" TO btn-close.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-employee
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-employee Dialog-Frame
ON HELP OF v-employee IN FRAME Dialog-Frame /* Employee ID */
DO:
   DEF VAR char-val AS cha NO-UNDO.
   RUN windows/l-emp.w (company_code,"", OUTPUT char-val).
   IF char-val <> "" THEN 
       v-employee:SCREEN-VALUE IN FRAME {&FRAME-NAME} = char-val.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-employee Dialog-Frame
ON LEAVE OF v-employee IN FRAME Dialog-Frame /* Employee ID */
DO:
    ASSIGN {&self-name}.
    /*IF ip-clock-in THEN RUN do-clock-in NO-ERROR.
    ELSE RUN do-clock-out NO-ERROR.*/
    IF LASTKEY <> -1 THEN do:
       run DO-clock-inout NO-ERROR.       
       IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
       APPLY "choose" TO btn-close.
       RETURN NO-APPLY.
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

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN v-clockio = IF ip-clock-in THEN "     CLOCK IN" ELSE "     CLOCK OUT"
         v-clockio:BGCOLOR IN FRAME {&FRAME-NAME} = IF ip-clock-in THEN 10 ELSE 12
         btn-next:LABEL = /*IF ip-clock-in THEN "Next Clock In" ELSE "Next Clock Out".*/
                         "&SAVE".

  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-dock-time Dialog-Frame 
PROCEDURE calc-dock-time :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ip-status as cha no-undo.
  def input-output parameter iop-time as int no-undo.
  
  def var li-hour as int no-undo.
  def var li-mini as int no-undo.
  def var li-dmin as int no-undo.
  def var li-dtim as int no-undo.
  def var li-docktime as int no-undo.
  def var li-dout as int no-undo.
  def var li-out-min as int no-undo.


 /* RUN Get-Shift(company_code,emplogin.machine,stoptime,"END",OUTPUT shiftvar). */
  find first employee where employee.company = company_code 
                                 and employee.employee = employee_code
                    no-lock no-error.
  if avail employee and employee.dock-time = 0 then return.
   
  ASSIGN
    li-docktime = if avail employee then employee.dock-time else 15  /* 15 min */
  
  /* login In */
  li-hour = truncate(iop-time / 3600,0).
  li-mini = iop-time mod 3600.
  li-mini = round(li-mini / 60,0).
  li-dtim = trunc(li-mini / li-docktime,0) + int(li-mini mod li-docktime > 0).
  li-dmin = li-dtim * li-docktime.
  li-dout = trunc(li-mini / li-docktime,0).
  li-out-min = li-dout * li-docktime.
  
  if ip-status = "login" then iop-time = li-hour * 3600 + li-dmin * 60.
  else iop-time = li-hour * 3600 + li-out-min * 60.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-clock-inout Dialog-Frame 
PROCEDURE do-clock-inout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE machinecode AS CHARACTER NO-UNDO.
DEF VAR activity_status AS cha NO-UNDO.
/* 
   Maybe we can say if record locked, copy existing data from locked record
   into a new record so we could then logout and into new machine.
   if this record locked then create new emplogin and  
   ASSIGN
      emplogin.company
      emplogin.employee
      emplogin.machine 
      emplogin.start_date
      emplogin.start_time
      emplogin.shift
  from locked record and continue with normal processing.  The only problem
  is that we are leaving a login record hung out there.  The system may find
  this record when it becomes available.  Find should be last not first.
*/   
  /* 07/09/01 ysk modification to apply dockttime login/logout */
def var li-time as int no-undo.
DEF VAR li-time-no-dock AS INT NO-UNDO.
DEF VAR li-diff-time AS INT NO-UNDO.

IF g_company NE company_code THEN
   g_company = company_code.

DO TRANSACTION:
   {sys/inc/maxbreak.i}
END.

ASSIGN
  li-time = TIME
  li-time-no-dock = li-time
  activity_status = IF ip-clock-in THEN "LOGIN" ELSE "LOGOUT".
  employee_code = v-employee.
  machine_code = "CLOCK".

FIND employee WHERE
     employee.company = company_code AND
     employee.employee = employee_code
     NO-LOCK NO-ERROR.

IF NOT AVAILABLE employee THEN DO:
   MESSAGE "Invalid Employee ID. " VIEW-AS ALERT-BOX ERROR.
   RETURN ERROR.
END.
run calc-dock-time (activity_status, input-output li-time).

/*========== clock in */
IF activity_status = 'login' THEN
DO:
    IF CAN-FIND(FIRST emplogin WHERE
       emplogin.company = company_code AND
       emplogin.employee = employee_code AND
       emplogin.machine = machine_code AND
       emplogin.END_date EQ ? AND
       emplogin.end_time = 0 AND
       emplogin.total_time = 0) THEN DO:
       MESSAGE "Already Clocked In. " VIEW-AS ALERT-BOX ERROR.
       RETURN ERROR.
    END.

    IF maxbreak-int GT 0 THEN
    DO:
       FIND FIRST b-emplogin WHERE
            b-emplogin.company EQ company_code AND
            b-emplogin.employee EQ employee_code AND
            b-emplogin.machine EQ machine_code AND
            b-emplogin.END_date NE ?
            USE-INDEX pi-emplogin
            NO-LOCK NO-ERROR.
    
       IF AVAIL b-emplogin THEN
       DO:
          IF emplogin.start_date = b-emplogin.end_date THEN
             li-diff-time = li-time-no-dock - b-emplogin.end_time .
          ELSE
             li-diff-time = (86400 - b-emplogin.end_time)
                          + (emplogin.start_date - b-emplogin.end_date - 1) * 86400
                          +  li-time-no-dock.
         
          IF NOT (li-diff-time < 0 OR li-diff-time EQ ?) AND
             li-diff-time LE maxbreak-int then
             li-time = li-time-no-dock.
          
          RELEASE b-emplogin.
       END.
    END.

    IF CAN-FIND(FIRST emplogin WHERE
       emplogin.company = company_code AND
       emplogin.employee = employee_code AND
       emplogin.START_date = TODAY AND
       emplogin.START_time = li-time AND
       emplogin.machine = machine_code) THEN DO:
       MESSAGE "Already Clocked In for" TODAY " Time:" STRING(li-time,"hh:mm")  VIEW-AS ALERT-BOX ERROR.
       RETURN ERROR.
    END.

    CREATE emplogin.
    ASSIGN
      emplogin.company = company_code
      emplogin.employee = employee_code
      emplogin.machine = machine_code
      emplogin.start_date = TODAY
      emplogin.start_time = li-time . /*TIME */
    RUN Get-Shift(company_code,machine_code,emplogin.start_time,"START",
           OUTPUT emplogin.shift).           

END. /* activity_status = login */

ELSE DO:  /* logout/clock out*/
  IF CAN-FIND(FIRST emplogin WHERE
       emplogin.company = company_code AND
       emplogin.employee = employee_code AND
       emplogin.machine <> machine_code AND
       emplogin.END_date EQ ? AND
       emplogin.end_time = 0 AND
       emplogin.total_time = 0) THEN DO: /* Do logout first */
     MESSAGE "Sorry, Employee Must Log Out of Machine on Plant Floor." VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.

  FIND FIRST emplogin WHERE
       emplogin.company = company_code AND
       emplogin.employee = employee_code AND
       emplogin.machine = machine_code AND
       emplogin.END_date EQ ? AND
       emplogin.end_time = 0 AND
       emplogin.total_time = 0
       EXCLUSIVE-LOCK NO-ERROR.

  IF NOT AVAIL emplogin THEN DO:
     MESSAGE "Clock In first." VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.
  else DO:
     ASSIGN emplogin.END_date = TODAY
            emplogin.END_time = TIME /*Don't apply doctime i-time*/.
     {custom/calctime.i &file="emplogin"}
  END.
  
END.
v-employee:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

release emplogin.
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
  DISPLAY v-clockio v-employee 
      WITH FRAME Dialog-Frame.
  ENABLE v-employee btn-next btn-close 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

