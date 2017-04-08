&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: addon\touch\w-clocki.w

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

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-employee btn-next btn-close 
&Scoped-Define DISPLAYED-OBJECTS v-clockio v-employee 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-close 
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

DEFINE FRAME F-Main
     v-clockio AT ROW 1.95 COL 30 COLON-ALIGNED NO-LABEL
     v-employee AT ROW 3.62 COL 24 COLON-ALIGNED
     btn-next AT ROW 6 COL 8
     btn-close AT ROW 6 COL 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 8.29
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Touch Screen Clock In"
         HEIGHT             = 8.33
         WIDTH              = 80
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 80
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR FILL-IN v-clockio IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Touch Screen Clock In */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Touch Screen Clock In */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-close W-Win
ON CHOOSE OF btn-close IN FRAME F-Main /* Close */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-next W-Win
ON CHOOSE OF btn-next IN FRAME F-Main /* Next Clock In */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-employee W-Win
ON HELP OF v-employee IN FRAME F-Main /* Employee ID */
DO:
   DEF VAR char-val AS cha NO-UNDO.
   RUN windows/l-emp.w (company_code,"", OUTPUT char-val).
   IF char-val <> "" THEN 
       v-employee:SCREEN-VALUE IN FRAME {&FRAME-NAME} = char-val.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-employee W-Win
ON LEAVE OF v-employee IN FRAME F-Main /* Employee ID */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
v-clockio = IF ip-clock-in THEN "     CLOCK IN" ELSE "     CLOCK OUT".

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-dock-time W-Win 
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
  li-hour = truncate(iop-time / 3600,0)
  li-mini = iop-time mod 3600
  li-mini = round(li-mini / 60,0)
  li-dtim = trunc(li-mini / li-docktime,0) + int(li-mini mod li-docktime > 0)
  li-dmin = li-dtim * li-docktime
  li-dout = trunc(li-mini / li-docktime,0)
  li-out-min = li-dout * li-docktime.

  if ip-status = "login" then iop-time = li-hour * 3600 + li-dmin * 60.
  else iop-time = li-hour * 3600 + li-out-min * 60.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-clock-inout W-Win 
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

ASSIGN
  li-time = TIME
  li-time-no-dock = li-time
  activity_status = IF ip-clock-in THEN "LOGIN" ELSE "LOGOUT"
  employee_code = v-employee
  machine_code = "CLOCK".

IF g_company NE company_code THEN
   g_company = company_code.

DO WITH TRANSACTION:
   {sys/inc/maxbreak.i}
END.

FIND employee WHERE employee.company = company_code
                         AND employee.employee = employee_code
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

   IF CAN-FIND(FIRST emplogin WHERE emplogin.company = company_code AND
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
     emplogin.start_time = li-time.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE v-employee btn-next btn-close 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.

   RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  v-clockio:BGCOLOR IN FRAME {&FRAME-NAME} = IF ip-clock-in THEN 10 ELSE 12.
  btn-next:LABEL = /*IF ip-clock-in THEN "Next Clock In" ELSE "Next Clock Out".*/
                   "&SAVE".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

