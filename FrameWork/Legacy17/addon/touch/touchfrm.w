&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS F-Frame-Win 
/*------------------------------------------------------------------------

  File: touch/touchfrm.w

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

{touch/touchdef.i}
{custom/shftdefs.i}

/* internal procedures */
{custom/shftproc.i}
{custom/emprate.i}

{custom/globdefs.i}

DEF BUFFER b-emplogin FOR emplogin.
DEF BUFFER bf-machemp FOR machemp.

DEF VAR cocode AS CHAR NO-UNDO.

cocode = g_company.

/*do not put any n-k-1 includes here,
  running from touchscreen icon, company here is blank*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_alljobs AS HANDLE NO-UNDO.
DEFINE VARIABLE h_allmachs AS HANDLE NO-UNDO.
DEFINE VARIABLE h_blanks AS HANDLE NO-UNDO.
DEFINE VARIABLE h_company AS HANDLE NO-UNDO.
DEFINE VARIABLE h_employee AS HANDLE NO-UNDO.
DEFINE VARIABLE h_enterjob AS HANDLE NO-UNDO.
DEFINE VARIABLE h_forms AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobdata AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobs AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobseq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_machines AS HANDLE NO-UNDO.
DEFINE VARIABLE h_menu AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pass AS HANDLE NO-UNDO.
DEFINE VARIABLE h_password AS HANDLE NO-UNDO.
DEFINE VARIABLE h_status AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124 BY 12.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Design Page: 14
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW F-Frame-Win ASSIGN
         HEIGHT             = 13
         WIDTH              = 124.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB F-Frame-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}
{touch/translations.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW F-Frame-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK F-Frame-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}


&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN dispatch IN THIS-PROCEDURE ('initialize':U).
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects F-Frame-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'touch/company.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_company ).
       RUN set-position IN h_company ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.95 , 124.00 ) */

    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'touch/menu.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_menu ).
       RUN set-position IN h_menu ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.95 , 124.00 ) */

    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'touch/employee.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  '':U ,
             OUTPUT h_employee ).
       RUN set-position IN h_employee ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.95 , 124.00 ) */

    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'touch/password.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_password ).
       RUN set-position IN h_password ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.95 , 124.00 ) */

       /* Links to SmartObject h_password. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'passwd':U , h_password ).

    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'touch/machines.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_machines ).
       RUN set-position IN h_machines ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.95 , 124.00 ) */

    END. /* Page 5 */
    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'touch/status.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_status ).
       RUN set-position IN h_status ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.95 , 124.00 ) */

    END. /* Page 7 */
    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'touch/allmachs.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_allmachs ).
       RUN set-position IN h_allmachs ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.95 , 124.00 ) */

    END. /* Page 8 */
    WHEN 9 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'touch/jobs.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_jobs ).
       RUN set-position IN h_jobs ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.95 , 124.00 ) */

    END. /* Page 9 */
    WHEN 10 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'touch/forms.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_forms ).
       RUN set-position IN h_forms ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.95 , 124.00 ) */

    END. /* Page 10 */
    WHEN 11 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'touch/blanks.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_blanks ).
       RUN set-position IN h_blanks ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.95 , 124.00 ) */

    END. /* Page 11 */
    WHEN 12 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'touch/pass.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  '':U ,
             OUTPUT h_pass ).
       RUN set-position IN h_pass ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.95 , 124.00 ) */

    END. /* Page 12 */
    WHEN 13 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'touch/jobseq.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  '':U ,
             OUTPUT h_jobseq ).
       RUN set-position IN h_jobseq ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.95 , 124.00 ) */

    END. /* Page 13 */
    WHEN 14 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'touch/jobdata.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_jobdata ).
       RUN set-position IN h_jobdata ( 1.10 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.86 , 124.00 ) */

    END. /* Page 14 */
    WHEN 15 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'touch/alljobs.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_alljobs ).
       RUN set-position IN h_alljobs ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.95 , 124.00 ) */

    END. /* Page 15 */
    WHEN 16 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'touch/enterjob.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_enterjob ).
       RUN set-position IN h_enterjob ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.95 , 124.00 ) */

       /* Adjust the tab order of the smart objects. */
    END. /* Page 16 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available F-Frame-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-dock-time F-Frame-Win 
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

  find first employee where
       employee.company = company_code AND
       employee.employee = employee_code
       no-lock no-error.

  if avail employee and
     employee.dock-time = 0 then
     return.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Change_Page F-Frame-Win 
PROCEDURE Change_Page :
/*------------------------------------------------------------------------------
  Purpose:     change to page of calling procedure
  Parameters:  INPUT page-no
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER page_no AS INTEGER NO-UNDO.

  IF VALID-HANDLE(h_numeric) THEN  DELETE PROCEDURE h_numeric.
  IF VALID-HANDLE(h_keyboard) THEN  DELETE PROCEDURE h_keyboard.
  IF VALID-HANDLE(h_w-mchtrn) THEN  DELETE PROCEDURE h_w-mchtrn.

  IF page_no = 1 THEN DO:
   {sys/inc/tslogin.i}
   IF tslogin-log THEN RETURN NO-APPLY.   
  END.
  IF activity_status = 'logout' AND page_no = 5 THEN /* bypass machines */
  page_no = 6. /* login_logout */
  ELSE
  IF activity_status = 'logout' AND page_no = 8 THEN /* bypass allmachs */
  page_no = 6. /* login_logout */
  ELSE
  IF activity_status = 'job-data-collection' AND page_no = 3 THEN /* cancel status */
  page_no = 2. /* menu */
  ELSE
  IF activity_status = 'job-data-collection' AND page_no = 6 THEN /* bypass login/out */
  page_no = 9. /* job data entry */
  ELSE
  IF activity_status = 'employee-status' AND page_no = 4 THEN /* bypass password */
  page_no = 7. /* status */
  ELSE
  IF activity_status = 'machine-status' AND page_no = 6 THEN /* bypass login/out */
  page_no = 7. /* status */
  ELSE
  IF activity_status = 'machine-status' AND page_no = 3 THEN /* cancel status */
  page_no = 2. /* menu */
  RUN SELECT-PAGE (page_no).
  CASE page_no:
    WHEN 1 THEN /* company */
    RUN Set_Title('AdvantzWare ' + translate('Touch Screen',NO)).
    WHEN 2 THEN /* menu */
    RUN Set_Title(translate('Main Menu',NO) + ' - ' +
                  translate('Company',NO) + ': ' + company_name + ' (' + company_code + ')').
    WHEN 3 THEN /* employee */
    DO:
      RUN Set_Title(translate('Employee',NO) + ' - ' +
                    translate('Company',NO) + ': ' + company_name + ' (' + company_code + ')').
      FIND employee WHERE employee.company = company_code
                      AND employee.employee = employee_code
                    NO-LOCK NO-ERROR.
      IF AVAILABLE employee THEN
      RUN Display_Keyboard (employee.keyboard_type,h_employee).
      ELSE
      RUN Display_Keyboard ('sortpad.',h_employee).
      RELEASE employee.
      IF activity_status = 'login' THEN
      RUN Get_Employees IN h_employee.
      ELSE
      RUN Get_Active_Employees IN h_employee.
    END.
    WHEN 4 THEN /* password */
    DO:
      RUN Set_Title(translate('Password',NO) + ' - ' +
                    translate('Employee',NO) + ': ' + employee_name + ' (' + employee_code + '), ' +
                    translate('Company',NO) + ': ' + company_name + ' (' + company_code + ')').
      RUN Display_Keyboard ('numeric.',h_password).
      FIND employee WHERE employee.company = company_code
                      AND employee.employee = employee_code
                    NO-LOCK NO-ERROR.
      IF AVAILABLE employee THEN
      RUN Display_Keyboard (employee.keyboard_type,h_password).
      ELSE
      RUN Display_Keyboard ('sortpad.',h_password).
      RELEASE employee.
      {methods/run_link.i "passwd-target" "set_focus"}
    END.
    WHEN 5 THEN /* machines */
    DO:
      IF activity_status = 'login' THEN
      DO:
        RUN Set_Title(translate('Machine',NO) + ' - ' +
                      translate('Employee',NO) + ': ' + employee_name + ' (' + employee_code + '), ' +
                      translate('Company',NO) + ': ' + company_name + ' (' + company_code + ')').
        RUN Get_Machines IN h_machines.
      END.
      ELSE
      DO:
        RUN Set_Title(translate('Machine',NO) + ' - ' +
                      translate('Company',NO) + ': ' + company_name + ' (' + company_code + ')').
        RUN Get_Active_Machines IN h_machines.
      END.
    END.
    WHEN 6 THEN /* run login_logout */
    DO: /* nothing appears within this frame page,
           it's simply a holder for processing logic */
      RUN Login_Logout.
      RUN SELECT-PAGE (2).
    END.
    WHEN 7 THEN /* status */
    DO:
      CASE activity_status:
        WHEN 'employee-status' THEN
        DO:
          RUN Set_Title(translate('Status',NO) + ' - ' +
                        translate('Employee',NO) + ': ' + employee_name + ' (' + employee_code + '), ' +
                        translate('Company',NO) + ': ' + company_name + ' (' + company_code + ')').
          RUN Employee_Status IN h_status.
        END.
        WHEN 'machine-status' THEN
        DO:
          RUN Set_Title(translate('Status',NO) + ' - ' +
                        translate('Machine',NO) + ': ' + machine_code + ', ' +
                        translate('Company',NO) + ': ' + company_name + ' (' + company_code + ')').
          RUN Machine_Status IN h_status.
        END.
      END CASE.
    END.
    WHEN 8 THEN /* allmachs */
    DO:
      RUN Display_Keyboard ('numeric.',h_allmachs).
      FIND employee WHERE employee.company = company_code
                      AND employee.employee = employee_code
                    NO-LOCK NO-ERROR.
      IF AVAILABLE employee THEN
      RUN Display_Keyboard (employee.keyboard_type,h_allmachs).
      ELSE
      RUN Display_Keyboard ('sortpad.',h_allmachs).
      RELEASE employee.
      IF activity_status = 'login' THEN
      DO:
        RUN Set_Title(translate('Machine',NO) + ' - ' +
                      translate('Employee',NO) + ': ' + employee_name + ' (' + employee_code + '), ' +
                      translate('Company',NO) + ': ' + company_name + ' (' + company_code + ')').
        RUN Get_Machines IN h_allmachs.
      END.
      ELSE
      DO:
        RUN Set_Title(translate('Machine',NO) + ' - ' +
                      translate('Company',NO) + ': ' + company_name + ' (' + company_code + ')').
        RUN Get_Active_Machines IN h_allmachs.
      END.
    END.
    WHEN 9 THEN /* jobs */
    DO:
      RUN Set_Title(translate('Job',NO) + ' - ' +
                    translate('Machine',NO) + ': ' + machine_code + ', ' +
                    translate('Company',NO) + ': ' + company_name + ' (' + company_code + ')').
      RUN Get_Jobs IN h_jobs ("Job").
    END.
    WHEN 10 THEN /* forms */
    DO:
      RUN Set_Title(translate('Form',NO) + ' - ' +
                    translate('Job',NO) + ': ' + LEFT-TRIM(job#) + ', ' +
                    translate('Machine',NO) + ': ' + machine_code + ', ' +
                    translate('Company',NO) + ': ' + company_name + ' (' + company_code + ')').
      RUN Get_Forms IN h_forms.
    END.
    WHEN 11 THEN /* blanks */
    DO:
      RUN Set_Title(translate('Blank',NO) + ' - ' +
                    translate('Form',NO) + ': ' + form_number + ', ' +
                    translate('Job',NO) + ': ' + LEFT-TRIM(job#) + ', ' +
                    translate('Machine',NO) + ': ' + machine_code + ', ' +
                    translate('Company',NO) + ': ' + company_name + ' (' + company_code + ')').
      RUN Get_Blanks IN h_blanks.
    END.
    WHEN 12 THEN /* pass */
    DO:
      RUN Set_Title(translate('Pass',NO) + ' - ' +
                    translate('Blank',NO) + ': ' + blank_number +
                   (IF item_number NE '' THEN ' (' + item_number + ')' ELSE '') +
                    ', ' +
                    translate('Form',NO) + ': ' + form_number + ', ' +
                    translate('Job',NO) + ': ' + LEFT-TRIM(job#) + ', ' +
                    translate('Machine',NO) + ': ' + machine_code + ', ' +
                    translate('Company',NO) + ': ' + company_code).
      RUN Get_Passes IN h_pass.
    END.
    WHEN 13 THEN /* job sequence */
    DO:
      RUN Set_Title(translate('Sequence',NO) + ' - ' +
                    translate('Pass',NO) + ': ' + pass_sequence + ', ' +
                    translate('Blank',NO) + ': ' + blank_number +
                   (IF item_number NE '' THEN ' (' + item_number + ')' ELSE '') +
                    ', ' +
                    translate('Form',NO) + ': ' + form_number + ', ' +
                    translate('Job',NO) + ': ' + LEFT-TRIM(job#) + ', ' +
                    translate('Machine',NO) + ': ' + machine_code + ', ' +
                    translate('Company',NO) + ': ' + company_code).
      IF VALID-HANDLE(h_w-mchtrn) THEN
      DELETE PROCEDURE h_w-mchtrn.
      RUN touch/w-mchtrn.w PERSISTENT SET h_w-mchtrn (THIS-PROCEDURE).
      RUN dispatch IN h_w-mchtrn ('initialize':U) NO-ERROR.
      RUN Get_Job_Sequence IN h_jobseq.
    END.
    WHEN 14 THEN /* job data */
    DO:
      RUN Display_Keyboard ('numeric.',h_jobdata).
      RUN Set_Title(translate('Job Data',NO) + ' - ' +
                    translate('Sequence',NO) + ': ' + job_sequence + ', ' +
                    translate('Pass',NO) + ': ' + pass_sequence + ', ' +
                    translate('Blank',NO) + ': ' + blank_number +
                   (IF item_number NE '' THEN ' (' + item_number + ')' ELSE '') +
                    ', ' +
                    translate('Form',NO) + ': ' + form_number + ', ' +
                    translate('Job',NO) + ': ' + LEFT-TRIM(job#) + ', ' +
                    translate('Machine',NO) + ': ' + machine_code + ', ' +
                    translate('Company',NO) + ': ' + company_code).

      IF VALID-HANDLE(h_w-mchtrn) THEN DELETE PROCEDURE h_w-mchtrn.
      RUN touch/w-mchtrn.w PERSISTENT SET h_w-mchtrn (THIS-PROCEDURE).
      RUN dispatch IN h_w-mchtrn ('initialize':U) NO-ERROR.
      RUN Init_Job IN h_jobdata.

    END.
    WHEN 15 THEN /* alljobs */
    DO:
      RUN Display_Keyboard ('numeric.',h_alljobs).
      FIND employee WHERE employee.company = company_code
                      AND employee.employee = employee_code
                    NO-LOCK NO-ERROR.
      IF AVAILABLE employee THEN
      RUN Display_Keyboard (employee.keyboard_type,h_alljobs).
      ELSE
      RUN Display_Keyboard ('sortpad.',h_alljobs).
      RELEASE employee.
      RUN Set_Title(translate('Job',NO) + ' - ' +
                    translate('Machine',NO) + ': ' + machine_code + ', ' +
                    translate('Company',NO) + ': ' + company_name + ' (' + company_code + ')').
      RUN Get_Jobs IN h_alljobs.
    END.
    WHEN 16 THEN /* manually enter job number */
    DO:
      RUN Display_Keyboard ('numeric.',h_enterjob).
      FIND employee WHERE employee.company = company_code
                      AND employee.employee = employee_code
                    NO-LOCK NO-ERROR.
      IF AVAILABLE employee THEN
      RUN Display_Keyboard (employee.keyboard_type,h_enterjob).
      ELSE
      RUN Display_Keyboard ('sortpad.',h_enterjob).
      RELEASE employee.
      RUN Set_Title(translate('Enter Job',NO) + ' - ' +
                    translate('Machine',NO) + ': ' + machine_code + ', ' +
                    translate('Company',NO) + ': ' + company_name + ' (' + company_code + ')').
    END.
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Close_Touch_Screen F-Frame-Win 
PROCEDURE Close_Touch_Screen :
/*------------------------------------------------------------------------------
  Purpose:     apply close window to container window
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/run_link.i "CONTAINER" "Close_Touch_Screen"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Delete_Keyboard F-Frame-Win 
PROCEDURE Delete_Keyboard :
/*------------------------------------------------------------------------------
  Purpose:     remove keyboard when called from job data entry
  Parameters:  INPUT calling procedure frame handle
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(h_keyboard) THEN
  DELETE PROCEDURE h_keyboard.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI F-Frame-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Display_Keyboard F-Frame-Win 
PROCEDURE Display_Keyboard :
/*------------------------------------------------------------------------------
  Purpose:     Run requested Keyboard
  Parameters:  Input Keyboard Program Name
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER Keyboard_Name AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER Frame_Handle AS WIDGET-HANDLE NO-UNDO.


  CASE Keyboard_Name:
    WHEN 'numeric.' THEN
    DO:
      IF VALID-HANDLE(h_numeric) THEN
      DELETE PROCEDURE h_numeric.
      Keyboard_Name = 'touch/' + Keyboard_Name + 'w'.
      RUN VALUE(Keyboard_Name)
          PERSISTENT SET h_numeric (Frame_Handle,THIS-PROCEDURE).
    END.
    OTHERWISE
    DO:
      IF VALID-HANDLE(h_keyboard) THEN
      DELETE PROCEDURE h_keyboard.
      IF employee_code NE '' THEN
      DO:
        FIND employee WHERE employee.company = company_code
                        AND employee.employee = employee_code
                      EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE employee THEN
        employee.keyboard_type = Keyboard_Name.
        RELEASE employee.
      END.
      Keyboard_Name = 'touch/' + Keyboard_Name + 'w'.      
      RUN VALUE(Keyboard_Name)
          PERSISTENT SET h_keyboard (Frame_Handle,THIS-PROCEDURE,FRAME {&FRAME-NAME}:ROW).
    END.
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI F-Frame-Win  _DEFAULT-ENABLE
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
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_MachTran_Rowid F-Frame-Win 
PROCEDURE Get_MachTran_Rowid :
/*------------------------------------------------------------------------------
  Purpose:     provide machine transaction rowid value to calling procedure
  Parameters:  OUTPUT machtran ROWID
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER rowid-machtran AS ROWID NO-UNDO.

  rowid-machtran = machtran-rowid.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Value F-Frame-Win 
PROCEDURE Get_Value :
/*------------------------------------------------------------------------------
  Purpose:     get value for use by calling procedures
  Parameters:  INPUT field to get, OUTPUT field value
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER get_field AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER field_value AS CHARACTER NO-UNDO.

  CASE get_field:
    WHEN 'activity_status' THEN
    field_value = activity_status.
    WHEN 'blank_number' THEN
    field_value = blank_number.
    WHEN 'charge_code' THEN
    field_value = charge_code.
    WHEN 'company_code' THEN
    field_value = company_code.
    WHEN 'company_name' THEN
    field_value = company_name.
    WHEN 'employee_code' THEN
    field_value = employee_code.
    WHEN 'employee_name' THEN
    field_value = employee_name.
    WHEN 'form_number' THEN
    field_value = form_number.
    WHEN 'item_number' THEN
    field_value = item_number.
    WHEN 'job#' THEN
    field_value = job#.
    WHEN 'job_number' THEN
    field_value = job_number.
    WHEN 'job_sequence' THEN
    field_value = job_sequence.
    WHEN 'job_sub' THEN
    field_value = job_sub.
    WHEN 'label_language' THEN
    field_value = label_language.
    WHEN 'language_list' THEN
    field_value = language_list.
    WHEN 'machine_code' THEN
    field_value = machine_code.
    WHEN 'pass_sequence' THEN
    field_value = pass_sequence.
    WHEN 'machine_list' THEN DO:
        DEF BUFFER buf-mach FOR mach.
        DEF VAR lv-mach-list AS cha NO-UNDO.
        FIND FIRST mach NO-LOCK WHERE mach.company = company_code
                                    AND mach.m-code = machine_code NO-ERROR.
        IF mach.sch-m-code <> "" THEN DO:
           FOR EACH buf-mach FIELDS(m-code) WHERE
               buf-mach.company EQ mach.company AND
               buf-mach.loc     EQ mach.loc AND
               buf-mach.sch-m-code EQ mach.sch-m-code
               NO-LOCK:
               lv-mach-list = lv-mach-list + buf-mach.m-code + ",".
           END.
        END.
        field_value = lv-mach-list.
    END.
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Login_Logout F-Frame-Win 
PROCEDURE Login_Logout :
/*------------------------------------------------------------------------------
  Purpose:     login or logout employee activity
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE machinecode AS CHARACTER NO-UNDO.
  DEF VAR v-add-one-sec AS LOG NO-UNDO.

IF g_company <> company_code THEN g_company = company_code.

DO TRANSACTION:
   {sys/inc/tsclock.i}
   {sys/inc/maxbreak.i}
   {sys/inc/tsdocksec.i}
END.
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
  DEF VAR v-tmp-start-date AS DATE NO-UNDO.
  DEF VAR v-today AS DATE NO-UNDO.
  DEF BUFFER buf-machemp FOR machemp.

  ASSIGN
    li-time = TIME
    v-today = TODAY
    li-time-no-dock = li-time.

  run calc-dock-time (activity_status, input-output li-time).

  /* ============= end of mods ========= */ 

FIND FIRST emplogin WHERE
     emplogin.company = company_code AND
     emplogin.employee = employee_code AND
     emplogin.machine <> "CLOCK" AND
     emplogin.end_date EQ ? AND
     emplogin.end_time = 0 AND
     emplogin.total_time = 0
     EXCLUSIVE-LOCK NO-ERROR.

IF AVAILABLE emplogin THEN  /* logout */
DO:
    /* need date loop */
    def var lv-end-date as date no-undo.

    if emplogin.start_date < v-today then  
    do lv-end-date = emplogin.start_date to v-today - 1:
       {touch/overdate.i}     
    end.
    else do:  /* same date login/logout */
        /* ========= end mods =========*/             
       ASSIGN
          emplogin.end_date = v-today
          machinecode = emplogin.machine.

       /*when logging out for lunch, don't dock time*/
       IF maxbreak-int GT 0 THEN
       DO:
          FIND FIRST b-emplogin WHERE
               b-emplogin.company = company_code AND
               b-emplogin.employee = employee_code AND
               b-emplogin.machine <> "CLOCK" AND
               b-emplogin.END_date NE ? AND
               b-emplogin.rec_key NE emplogin.rec_key
               USE-INDEX pi-emplogin
               NO-LOCK NO-ERROR.

          IF AVAIL b-emplogin THEN
          DO:
             IF emplogin.start_date = b-emplogin.end_date THEN
                li-diff-time = emplogin.START_time - b-emplogin.end_time .
             ELSE
                li-diff-time = (86400 - b-emplogin.end_time)
                             + (emplogin.start_date - b-emplogin.end_date - 1) * 86400
                             +  emplogin.START_time.

             if NOT (li-diff-time < 0 OR li-diff-time EQ ?) AND
                li-diff-time GT maxbreak-int then
                stoptime = li-time-no-dock.
             ELSE
                stoptime = li-time.

             RELEASE b-emplogin.
          END.
          ELSE
            stoptime = li-time.
       END.
       ELSE
          stoptime = li-time.

       RUN Get-Shift(company_code,emplogin.machine,stoptime,"END",OUTPUT shiftvar).

       IF shiftvar = emplogin.shift THEN
          emplogin.end_time = stoptime. /* no shift change, close out current */
       ELSE
       DO: /* shift change, close out current */
         RUN Shift-Data(company_code,emplogin.machine,emplogin.shift,
                     OUTPUT starttime,OUTPUT endtime).

         IF tsdocksec-log AND
            employee.dock-time GT 0 AND
            endtime NE li-time-no-dock THEN
            ASSIGN
               v-add-one-sec = TRUE
               endtime = endtime + 1.
         ELSE
            v-add-one-sec = FALSE.

         ASSIGN emplogin.end_time = endtime
                machinecode = emplogin.machine.
         {custom/calctime.i &file="emplogin"}
         RUN Missing-Shift(company_code,machinecode,emplogin.shift,shiftvar,
                        OUTPUT missingshift).                        

         IF missingshift NE '' THEN /* skipped a shift */
         DO: /* create record for skipped shift */
            RUN Shift-Data(company_code,machinecode,missingshift,
                       OUTPUT starttime,OUTPUT endtime).

            IF v-add-one-sec THEN
               endtime = endtime + 1.

            IF NOT CAN-FIND(FIRST b-emplogin WHERE
               b-emplogin.company = company_code AND
               b-emplogin.employee = employee_code AND
               b-emplogin.machine = machinecode AND
               b-emplogin.start_date = v-today AND
               b-emplogin.start_time = starttime) THEN
               DO:
                  CREATE emplogin.
                  ASSIGN emplogin.company = company_code
                         emplogin.employee = employee_code
                         emplogin.machine = machinecode
                         emplogin.start_date = v-today
                         emplogin.start_time = starttime
                         emplogin.end_date = v-today
                         emplogin.end_time = endtime
                         emplogin.shift = missingshift.
                  {custom/calctime.i &file="emplogin"}
               END.
         END.
         /* create record for current shift */
         RUN Shift-Data(company_code,machinecode,shiftvar,
                     OUTPUT starttime,OUTPUT endtime).

         IF NOT(v-add-one-sec AND starttime EQ stoptime) AND
            NOT CAN-FIND(FIRST b-emplogin WHERE
            b-emplogin.company = company_code AND
            b-emplogin.employee = employee_code AND
            b-emplogin.machine = machinecode AND
            b-emplogin.start_date = v-today AND
            b-emplogin.start_time = starttime) THEN
            DO:
               CREATE emplogin.
               ASSIGN emplogin.company = company_code
                      emplogin.employee = employee_code
                      emplogin.machine = machinecode
                      emplogin.start_date = v-today
                      emplogin.start_time = starttime
                      emplogin.end_date = v-today
                      emplogin.end_time = stoptime
                      emplogin.shift = shiftvar.
               {custom/calctime.i &file="emplogin"}
            END.
       END.  /* different shift */
       {custom/calctime.i &file="emplogin"}
       /* get active machine if it exists and logout of machine */
       FIND FIRST machtran WHERE
            machtran.company = company_code AND
            machtran.machine = machinecode AND
            machtran.end_date EQ ? AND
            machtran.end_time = 0 AND
            machtran.total_time = 0
            NO-LOCK NO-ERROR.

       IF AVAILABLE machtran THEN
       DO:
         FIND LAST machemp WHERE
              machemp.table_rec_key = machtran.rec_key AND
              machemp.employee = employee_code AND
              machemp.end_date EQ ? AND
              machemp.end_time = 0 AND
              machemp.total_time = 0
              EXCLUSIVE-LOCK NO-ERROR.

         IF AVAILABLE machemp THEN
         DO:
            machemp.end_date = v-today.
            RUN Get-Shift(company_code,machinecode,stoptime,"END",OUTPUT shiftvar).
            IF shiftvar = machemp.shift THEN
               machemp.end_time = stoptime. /* no shift change, close out current */
            ELSE
            DO: /* shift change, close out current */
               FIND employee WHERE employee.company = company_code
                            AND employee.employee = employee_code NO-LOCK NO-ERROR.
               RUN Shift-Data(company_code,machinecode,machemp.shift,
                           OUTPUT starttime,OUTPUT endtime).

               IF tsdocksec-log AND
                  employee.dock-time GT 0 AND
                  endtime NE li-time-no-dock THEN
                  ASSIGN
                     v-add-one-sec = TRUE
                     endtime = endtime + 1.
               ELSE
                  v-add-one-sec = FALSE.

               machemp.end_time = endtime.

               {custom/calctime.i &file="machemp"}
               RUN Missing-Shift(company_code,machinecode,machemp.shift,shiftvar,
                              OUTPUT missingshift).
               IF missingshift NE '' THEN /* skipped a shift */
               DO: /* create record for skipped shift */
                  RUN Shift-Data(company_code,machinecode,missingshift,
                                 OUTPUT starttime,OUTPUT endtime).

                  IF NOT CAN-FIND(FIRST buf-machemp WHERE
                     buf-machemp.table_rec_key = machtran.rec_key AND
                     buf-machemp.employee = employee_code AND
                     buf-machemp.start_date = v-today AND
                     buf-machemp.start_time = starttime) THEN
                     DO:
                        IF v-add-one-sec THEN
                           endtime = endtime + 1.

                        CREATE buf-machemp.
                        ASSIGN buf-machemp.table_rec_key = machtran.rec_key
                               buf-machemp.employee = employee_code
                               buf-machemp.start_date = v-today
                               buf-machemp.start_time = starttime
                               buf-machemp.end_date = v-today
                               buf-machemp.end_time = endtime
                               buf-machemp.shift = missingshift
                               buf-machemp.ratetype = 'Standard'
                               buf-machemp.rate_usage = employee.rate_usage.

                        RUN Employee-Rate(company_code,employee_code,buf-machemp.shift,machinecode,
                                      buf-machemp.rate_usage,buf-machemp.ratetype,OUTPUT buf-machemp.rate).
                        {custom/calctime.i &file="buf-machemp"}
                        RELEASE buf-machemp.
                     END.
               END.
               /* create record for current shift */
               RUN Shift-Data (company_code,machine_code,shiftvar,
                            OUTPUT starttime,OUTPUT endtime).

               IF NOT(v-add-one-sec AND starttime EQ stoptime) AND
                  NOT CAN-FIND(FIRST buf-machemp WHERE
                  buf-machemp.table_rec_key = machtran.rec_key AND
                  buf-machemp.employee = employee_code AND
                  buf-machemp.start_date = v-today AND
                  buf-machemp.start_time = starttime) THEN
                  DO:
                     CREATE buf-machemp.
                     ASSIGN buf-machemp.table_rec_key = machtran.rec_key
                            buf-machemp.employee = employee_code
                            buf-machemp.start_date = v-today
                            buf-machemp.start_time = starttime
                            buf-machemp.end_date = v-today
                            buf-machemp.end_time = stoptime
                            buf-machemp.shift = shiftvar
                            buf-machemp.ratetype = 'Standard'
                            buf-machemp.rate_usage = employee.rate_usage.

                     RUN Employee-Rate(company_code,employee_code,buf-machemp.shift,machinecode,
                                    buf-machemp.rate_usage,buf-machemp.ratetype,OUTPUT buf-machemp.rate).
                     {custom/calctime.i &file="buf-machemp"}
                     RELEASE buf-machemp.
                  END.
            END.
            {custom/calctime.i &file="machemp"}
            RELEASE machemp.
         END. /* avail machemp */
       END. /* avail machtran */
    end. /* else  start_date = end_data(today) YSK 09/20/01 */
    /* TSCLOCK mods*/

    IF tsclock-log AND machine_code <> "CLOCK" THEN do:
       CREATE emplogin.
       ASSIGN emplogin.company = company_code
              emplogin.employee = employee_code
              emplogin.machine = "CLOCK"
              emplogin.start_date = v-today
              emplogin.start_time = stoptime.

       RUN Get-Shift(company_code,machine_code,emplogin.start_time,"START",
                     OUTPUT emplogin.shift).           
    END.
END. /* avail emplogin - logout */

IF activity_status = 'login' THEN
DO:
    IF tsclock-log THEN do:

       /* check clockin first */
       FIND FIRST emplogin WHERE
            emplogin.company = company_code AND
            emplogin.employee = employee_code AND
            emplogin.machine = "CLOCK" AND
            emplogin.END_date = ? AND
            emplogin.end_time = 0 AND
            emplogin.total_time = 0
            EXCLUSIVE-LOCK NO-ERROR.

       IF NOT AVAILABLE emplogin THEN DO:
          MESSAGE "CLOCK IN First." VIEW-AS ALERT-BOX ERROR.
          RETURN.
       END.
       ELSE DO:
          /* don't dock time for a login after a break*/
          IF maxbreak-int GT 0 THEN
          DO:
             FIND FIRST b-emplogin WHERE
                  b-emplogin.company EQ company_code AND
                  b-emplogin.employee EQ employee_code AND
                  b-emplogin.machine EQ "CLOCK" AND
                  b-emplogin.rec_key NE emplogin.rec_key
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

                if NOT (li-diff-time < 0 OR li-diff-time EQ ?) AND
                   li-diff-time LE maxbreak-int then
                   emplogin.end_time = li-time-no-dock.
                ELSE
                   emplogin.end_time = li-time.

                RELEASE b-emplogin.
             END.
             ELSE
                emplogin.end_time = li-time.
          END.
          ELSE
             emplogin.end_time = li-time.

          emplogin.end_date = v-today.
          {custom/calctime.i &file="emplogin"}
       END. 
    END. /* end of clockin check*/

    FIND employee WHERE
         employee.company = company_code AND
         employee.employee = employee_code
         NO-LOCK NO-ERROR.

    CREATE emplogin.
    ASSIGN
      emplogin.company = company_code
      emplogin.employee = employee_code
      emplogin.machine = machine_code
      emplogin.start_date = v-today.

    /* don't use dock time for a login after a break*/
    IF maxbreak-int GT 0 THEN
    DO:
       FIND FIRST b-emplogin WHERE
            b-emplogin.company EQ company_code AND
            b-emplogin.employee EQ employee_code AND
            b-emplogin.machine EQ machine_code AND
            b-emplogin.rec_key NE emplogin.rec_key
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

          if NOT (li-diff-time < 0 OR li-diff-time EQ ?) AND
             li-diff-time LE maxbreak-int then
             li-time = li-time-no-dock.

          RELEASE b-emplogin.
       END.
    END.

    emplogin.start_time = li-time.

    RUN Get-Shift(company_code,machine_code,emplogin.start_time,"START",
                  OUTPUT emplogin.shift).   


    /* get active machine if it exists and log into machine */
    FIND FIRST machtran WHERE
         machtran.company = company_code AND
         machtran.machine = machine_code AND
         machtran.end_date EQ ? AND
         machtran.end_time = 0 AND
         machtran.total_time = 0
         NO-LOCK NO-ERROR.

    /*ESP 07/13/09 - Put this code back, there were some machemp missing that should have been
                     created*/
    IF AVAILABLE machtran THEN
    DO:
       IF NOT CAN-FIND(FIRST machemp WHERE         
          machemp.table_rec_key = machtran.rec_key AND
          machemp.employee = emplogin.employee AND
          machemp.start_date = emplogin.start_date AND
          machemp.start_time <= emplogin.start_time AND
          (machemp.END_time >= emplogin.START_time OR 
           (machemp.start_time <> machemp.end_time AND
            machemp.total_time = 0))) THEN
          DO:
             CREATE machemp.
             ASSIGN
               machemp.table_rec_key = machtran.rec_key
               machemp.employee = emplogin.employee
               machemp.start_date = emplogin.start_date
               machemp.start_time = emplogin.start_time
               machemp.shift = emplogin.shift
               machemp.ratetype = 'Standard'
               machemp.rate_usage = employee.rate_usage.
             RUN Employee-Rate(company_code,machemp.employee,machemp.shift,machine_code,
                               machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
          END.

    END. /* avail machtran */

    /* ======= mods for dummy transaction for people working without job ===*/
    else do: /*if emplogin.machine = "w/h" or emplogin.machine = "COINDR" or
            emplogin.machine = "FOINDR" or emplogin.machine = "MAINTC"
            */
        find first mach where mach.company = emplogin.company and
                                 mach.m-code = machine_code
                                 no-lock no-error.
        if avail mach and mach.industry = "X" 
        then do:
           create machtran.
           ASSIGN machtran.company = company_code
                machtran.machine = machine_code
        machtran.job_number = " "  /*job_number*/
        machtran.job_sub = 0     /*INTEGER(job_sub)*/
        machtran.form_number = 0 /*INTEGER(form_number)*/
        machtran.blank_number = 0 /*INTEGER(blank_number)*/
        machtran.pass_sequence = 0 /*INTEGER(pass_sequence)*/
        machtran.start_date = emplogin.start_date
        machtran.start_time = emplogin.start_time
        machtran.jobseq = 0 /*IF AVAILABLE jobseq THEN jobseq.jobseq ELSE 0 */
        machtran.charge_code = charge_code
        machtran-rowid = ROWID(machtran)
        machtran.shift = emplogin.shift.      

     /*  RUN Get-Shift(company_code,machine_code,machtran.start_time,job_sequence,
             OUTPUT machtran.shift).
       {methods/run_link.i "CONTAINER" "Set_MachTran_Rowid" "(machtran-rowid)"} */

        IF NOT CAN-FIND(FIRST machemp WHERE
           machemp.table_rec_key = machtran.rec_key AND
           machemp.employee = emplogin.employee AND
           machemp.start_date = emplogin.start_date AND
           machemp.start_time = emplogin.start_time) THEN
           DO:
              CREATE machemp.
              ASSIGN
              machemp.table_rec_key = machtran.rec_key
              machemp.employee = emplogin.employee
              machemp.start_date = emplogin.start_date
              machemp.start_time = emplogin.start_time
              machemp.shift = emplogin.shift
              machemp.ratetype = 'Standard'
              machemp.rate_usage = employee.rate_usage.
              RUN Employee-Rate(company_code,machemp.employee,machemp.shift,machine_code,
                              machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
           END.
      end.  /* industry = x */
    end.  /* else */
    /* end of mods =====*/

END. /* activity_status = login */
  FIND CURRENT machemp NO-LOCK NO-ERROR.
  FIND CURRENT emplogin NO-LOCK NO-ERROR.
  FIND CURRENT machtran NO-LOCK NO-ERROR.
  release emplogin.
  release machemp.
  release machtran.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Run_Get_Job_Sequence F-Frame-Win 
PROCEDURE Run_Get_Job_Sequence :
/*------------------------------------------------------------------------------
  Purpose:     run get_job_sequence in jobseq.w, requested by jobdata.w
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Get_Job_Sequence IN h_jobseq.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records F-Frame-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartFrame, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set_MachTran_Rowid F-Frame-Win 
PROCEDURE Set_MachTran_Rowid :
/*------------------------------------------------------------------------------
  Purpose:     accept machine transaction rowid value
  Parameters:  INPUT machtran ROWID
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER rowid-machtran AS ROWID NO-UNDO.

  machtran-rowid = rowid-machtran.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set_Title F-Frame-Win 
PROCEDURE Set_Title :
/*------------------------------------------------------------------------------
  Purpose:     receive and pass title value to container
  Parameters:  INPUT window_title
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER window_title AS CHARACTER NO-UNDO.

  {methods/run_link.i "CONTAINER" "Set_Title" "(window_title)"}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set_Value F-Frame-Win 
PROCEDURE Set_Value :
/*------------------------------------------------------------------------------
  Purpose:     set value here for use by calling procedures later
  Parameters:  INPUT field to set, INPUT field value
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER set_field AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER field_value AS CHARACTER NO-UNDO.

  CASE set_field:
    WHEN 'activity_status' THEN
    activity_status = field_value.
    WHEN 'blank_number' THEN
    blank_number = field_value.
    WHEN 'charge_code' THEN
    charge_code = field_value.
    WHEN 'company_code' THEN
    company_code = field_value.
    WHEN 'company_name' THEN
    company_name = field_value.
    WHEN 'employee_code' THEN
    employee_code = field_value.
    WHEN 'employee_name' THEN
    employee_name = field_value.
    WHEN 'form_number' THEN
    form_number = field_value.
    WHEN 'item_number' THEN
    item_number = field_value.
    WHEN 'job#' THEN
    ASSIGN
      job# = field_value
      job_number = SUBSTR(job#,1,INDEX(job#,'-') - 1)
      job_number = FILL(' ',6 - LENGTH(job_number)) + job_number
      job_sub = SUBSTR(job#,INDEX(job#,'-') + 1).
    WHEN 'job_number' THEN
    job_number = field_value.
    WHEN 'job_sequence' THEN
    job_sequence = field_value.
    WHEN 'job_sub' THEN
    job_sub = field_value.
    WHEN 'label_language' THEN
    label_language = field_value.
    WHEN 'language_list' THEN
    language_list = field_value.
    WHEN 'machine_code' THEN
    machine_code = field_value.
    WHEN 'pass_sequence' THEN
    pass_sequence = field_value.
    OTHERWISE
    MESSAGE 'Set_Value:' set_field field_value VIEW-AS ALERT-BOX.
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed F-Frame-Win 
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

