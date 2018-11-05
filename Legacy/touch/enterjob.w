&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: touch/enterjob.w

  Description: from SMART.W - Template for basic SmartObject

  Author: Ron Stark
  Created: 8.31.2000

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

DEFINE VARIABLE lVerifyJob AS LOGICAL NO-UNDO.

&SCOPED-DEFINE PageNo 16
{touch/touchdef.i}
{custom/globdefs.i}

DO TRANSACTION:
   {sys/inc/tskey.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-OBJECTS job_# form_# blank_# pass_# item_# ~
item_name 

/* Custom List Definitions                                              */
/* ENTRY-FIELDS,ENTRY-BUTTONS,List-3,List-4,List-5,List-6               */
&Scoped-define ENTRY-FIELDS job_# form_# blank_# pass_# item_# item_name 
&Scoped-define ENTRY-BUTTONS Btn_Job_# Btn_Form_# Btn_Blank_# Btn_Pass_# ~
Btn_Item_# 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD actual-entered s-object 
FUNCTION actual-entered RETURNS LOGICAL
  (INPUT ip-m-code AS CHAR, INPUT ip-job AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Blank_# 
     LABEL "Blank Number" 
     SIZE 23 BY 1.67 TOOLTIP "BLANK NUMBER".

DEFINE BUTTON Btn_Form_# 
     LABEL "Form Number" 
     SIZE 23 BY 1.67 TOOLTIP "FORM NUMBER".

DEFINE BUTTON Btn_Item_# 
     LABEL "Item Number" 
     SIZE 23 BY 1.67 TOOLTIP "ITEM NUMBER".

DEFINE BUTTON Btn_Job_# 
     LABEL "Job Number" 
     SIZE 23 BY 1.67 TOOLTIP "JOB NUMBER".

DEFINE BUTTON Btn_Pass_# 
     LABEL "Pass Number" 
     SIZE 23 BY 1.67 TOOLTIP "PASS NUMBER".

DEFINE VARIABLE blank_# AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE form_# AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE item_# AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE item_name AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE job_# AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE pass_# AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 12.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_Job_# AT ROW 1.24 COL 7
     job_# AT ROW 1.48 COL 29 COLON-ALIGNED NO-LABEL
     Btn_Form_# AT ROW 3.14 COL 7
     form_# AT ROW 3.38 COL 29 COLON-ALIGNED NO-LABEL
     Btn_Blank_# AT ROW 5.05 COL 7
     blank_# AT ROW 5.29 COL 29 COLON-ALIGNED NO-LABEL
     Btn_Pass_# AT ROW 6.95 COL 7
     pass_# AT ROW 7.19 COL 29 COLON-ALIGNED NO-LABEL
     Btn_Item_# AT ROW 8.86 COL 7
     item_# AT ROW 9.1 COL 29 COLON-ALIGNED NO-LABEL
     item_name AT ROW 9.1 COL 70 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
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
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 12.95
         WIDTH              = 124.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN blank_# IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON Btn_Blank_# IN FRAME F-Main
   NO-ENABLE 2                                                          */
ASSIGN 
       Btn_Blank_#:PRIVATE-DATA IN FRAME F-Main     = 
                "Blank Number".

/* SETTINGS FOR BUTTON Btn_Form_# IN FRAME F-Main
   NO-ENABLE 2                                                          */
ASSIGN 
       Btn_Form_#:PRIVATE-DATA IN FRAME F-Main     = 
                "Form Number".

/* SETTINGS FOR BUTTON Btn_Item_# IN FRAME F-Main
   NO-ENABLE 2                                                          */
ASSIGN 
       Btn_Item_#:PRIVATE-DATA IN FRAME F-Main     = 
                "Item Number".

/* SETTINGS FOR BUTTON Btn_Job_# IN FRAME F-Main
   NO-ENABLE 2                                                          */
ASSIGN 
       Btn_Job_#:PRIVATE-DATA IN FRAME F-Main     = 
                "Job Number".

/* SETTINGS FOR BUTTON Btn_Pass_# IN FRAME F-Main
   NO-ENABLE 2                                                          */
ASSIGN 
       Btn_Pass_#:PRIVATE-DATA IN FRAME F-Main     = 
                "Pass Number".

/* SETTINGS FOR FILL-IN form_# IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN item_# IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN item_name IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN job_# IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN pass_# IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_Blank_#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Blank_# s-object
ON CHOOSE OF Btn_Blank_# IN FRAME F-Main /* Blank Number */
DO:
  RUN Reset_Field_Colors.
  h_field = blank_#:HANDLE.
  RUN Set_Field_Colors.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Form_#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Form_# s-object
ON CHOOSE OF Btn_Form_# IN FRAME F-Main /* Form Number */
DO:
  RUN Reset_Field_Colors.
  h_field = form_#:HANDLE.
  RUN Set_Field_Colors.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Item_#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Item_# s-object
ON CHOOSE OF Btn_Item_# IN FRAME F-Main /* Item Number */
DO:
  RUN Reset_Field_Colors.
  h_field = item_#:HANDLE.
  RUN Set_Field_Colors.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Job_#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Job_# s-object
ON CHOOSE OF Btn_Job_# IN FRAME F-Main /* Job Number */
DO:
  RUN Reset_Field_Colors.
  h_field = job_#:HANDLE.
  RUN Set_Field_Colors.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Pass_#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Pass_# s-object
ON CHOOSE OF Btn_Pass_# IN FRAME F-Main /* Pass Number */
DO:
  RUN Reset_Field_Colors.
  h_field = pass_#:HANDLE.
  RUN Set_Field_Colors.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{touch/pCreateINIObjects.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-num-on s-object 
PROCEDURE get-num-on :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    job-mch.n-on = job-mch.n-out.

    FIND FIRST mach NO-LOCK
        WHERE mach.company EQ job-mch.company
          AND mach.m-code  EQ job-mch.m-code
        NO-ERROR.
    IF AVAIL mach AND NOT CAN-DO("A,P,B",mach.p-type) THEN
    FOR EACH job NO-LOCK
        WHERE job.company      EQ job-mch.company
          AND job.job          EQ job-mch.job
          AND job.job-no       EQ job-mch.job-no
          AND job.job-no2      EQ job-mch.job-no2
          AND TRIM(job.est-no) NE "",
        FIRST ef NO-LOCK
        WHERE ef.company EQ job.company
          AND ef.est-no  EQ job.est-no
          AND ef.form-no EQ job-mch.frm:
      RUN sys/inc/numup.p (ef.company, ef.est-no, ef.form-no, OUTPUT job-mch.n-on).
      job-mch.n-on = job-mch.n-on * job-mch.n-out.
      LEAVE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initScreen s-object 
PROCEDURE initScreen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      form_# = ''
      blank_# = ''
      pass_# = ''
      item_# = ''
      item_name = ''
      field_value = ''
      h_field = job_#:HANDLE
      h_field:SCREEN-VALUE = ''
      lVerifyJob = YES
      .
    RUN pSetSensitive ("VerifyJobNumber",YES).
    RUN pSetSensitive ("VerifyEntries",NO).
    DISABLE {&ENTRY-BUTTONS}.
    DISPLAY {&ENTRY-FIELDS}.
    ENABLE Btn_Job_#.
    {touch/localview.i}
    APPLY 'CHOOSE' TO Btn_Job_#.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Key_Stroke s-object 
PROCEDURE Key_Stroke :
/*------------------------------------------------------------------------------
  Purpose:     Apply keystroke to field with focus
  Parameters:  Input Keystroke
  Notes:       
------------------------------------------------------------------------------*/
  &Scoped-define KEYSTROKE ENTERJOB

  {touch/keystrok.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize s-object 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN pCreateINIObjects ("VerifyEntries,VerifyJobNumber,Back,HomeSmall").
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN initScreen.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view s-object 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*{touch/localview.i}*/
  RUN initScreen.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClick s-object 
PROCEDURE pClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcClick AS CHARACTER NO-UNDO.
    
    CASE ipcClick:
        WHEN "HomeSmall" THEN DO WITH FRAME {&FRAME-NAME}:
            {methods/run_link.i "CONTAINER" "Change_Page" "(2)"}
        END.
        WHEN "Back" THEN DO:
            IF lVerifyJob EQ YES THEN DO:
                {methods/run_link.i "CONTAINER" "Change_Page" "(9)"}
            END.
            ELSE
            RUN InitScreen.
        END.
        WHEN "VerifyJobNumber" THEN DO:
            ASSIGN {&ENTRY-FIELDS}.
            RUN Verify_Job.
        END.
        WHEN "VerifyEntries" THEN DO:
            ASSIGN {&ENTRY-FIELDS}.
            RUN Verify_Entries.
        END.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE promptToReplace s-object 
PROCEDURE promptToReplace :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipcMCode AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER ipcJobMach AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcChoice AS CHARACTER   NO-UNDO.
DEF VAR lCancel AS LOG.
DEF VAR lChoice AS LOG.
DEF VAR v-msg1 AS CHAR NO-UNDO.
DEF VAR v-msg1-1 AS CHAR NO-UNDO.
DEF VAR v-msg2 AS CHAR NO-UNDO.
DEF VAR v-msg3 AS CHAR NO-UNDO.
DEF VAR v-msg4 AS CHAR NO-UNDO.
DEF VAR choice AS CHAR NO-UNDO.
DEF VAR op-values AS CHAR NO-UNDO.
DEF VAR lValid AS LOG NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR ip-parms AS CHAR NO-UNDO.
DEF VAR lcUserPrompt AS CHAR  NO-UNDO.
DEF VAR cOrderList AS CHAR NO-UNDO.
DEF VAR liCnt AS INT NO-UNDO.
DEF VAR lcUserButton AS CHAR NO-UNDO.

      ASSIGN
          v-msg1 =  'Machine ' + ipcMCode + ' is not defined in the job standards for ' + string(job-mch.job-no) + '.' 
          v-msg1-1 = 'Would you like to replace the the GL machine ' + ipcJobMAch + ' with ' + ipcMCode + ' or add ' + ipcMCode + ' as an additional machine? '
         
            ip-parms = 
               "type=fill-in,name=fi1,row=2,col=22,enable=false,width=98,scrval=" + v-msg1 + ",FORMAT=X(85)"
                + "|type=fill-in,name=fi5,row=2.9,col=22,enable=false,width=108,scrval=" + v-msg1-1 + ",FORMAT=X(105)"
                + "|type=buttonLabel,name=OK,row=11.6,col=22,label=Add Machine,scrval=" + v-msg4 + ",FORMAT=X(62)"   
                + "|type=buttonLabel,name=Yes,row=11.6,col=22,label=Replace,scrval=" + v-msg4 + ",FORMAT=X(62)"                 
                + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true " 
                + "|type=win,name=fi3,enable=true,label=Question,FORMAT=X(30),height=8,width=140".
             RUN custom/d-prompt.w (INPUT "yes-no-cancel", ip-parms, "", OUTPUT op-values).
        DO i = 1 TO NUM-ENTRIES(op-values) BY 2.
            IF ENTRY(i, op-values) EQ "default" THEN
              choice = ENTRY(i + 1, op-values) NO-ERROR.          
        END. /* Do i = 1... */
        
        CASE choice:
          WHEN "YES" THEN
             opcChoice = "Replace".
          WHEN "NO" THEN
             opcChoice = "Add".
          WHEN "Cancel" THEN
             opcChoice = "Cancel".
          OTHERWISE
             opcChoice = "Cancel".
        END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reset_Field_Colors s-object 
PROCEDURE Reset_Field_Colors :
/*------------------------------------------------------------------------------
  Purpose:     Reset all data field colors to initial settings
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      h_field = job_#:HANDLE
      job_#:FGCOLOR = 15
      job_#:BGCOLOR = 0
      h_field = form_#:HANDLE
      form_#:FGCOLOR = 15
      form_#:BGCOLOR = 0
      h_field = blank_#:HANDLE
      blank_#:FGCOLOR = 15
      blank_#:BGCOLOR = 0
      h_field = pass_#:HANDLE
      pass_#:FGCOLOR = 15
      pass_#:BGCOLOR = 0
      h_field = pass_#:HANDLE
      pass_#:FGCOLOR = 15
      pass_#:BGCOLOR = 0
      h_field = item_#:HANDLE
      item_#:FGCOLOR = 15
      item_#:BGCOLOR = 0
      .
    IF tskey-log = NO THEN
       ASSIGN
          job_#:SENSITIVE = NO
          form_#:SENSITIVE = NO
          blank_#:SENSITIVE = NO
          pass_#:SENSITIVE = NO
          item_#:SENSITIVE = NO
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set_Field_Colors s-object 
PROCEDURE Set_Field_Colors :
/*------------------------------------------------------------------------------
  Purpose:     Set field colors to show field has focus
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      field_value = ''
      h_field:FGCOLOR = 15
      h_field:BGCOLOR = 4 /* dark red */
      h_field:BGCOLOR = 3 /* tiel */
      .
    IF tskey-log EQ NO THEN DO:
       h_field:SENSITIVE = YES.
       APPLY "ENTRY":U TO h_field.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verify_Entries s-object 
PROCEDURE Verify_Entries :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-job-mch FOR job-mch.
DEF VAR cAction AS CHAR NO-UNDO.
    IF NOT CAN-FIND(FIRST job-mch WHERE
     job-mch.company = company_code AND
     job-mch.job = job.job AND
     job-mch.job-no = job_number AND
     job-mch.job-no2 = INTEGER(job_sub) AND
     job-mch.frm = INTEGER(form_#)) THEN
     DO:
        MESSAGE 'FORM DOES NOT EXIST FOR JOB - TRY AGAIN' VIEW-AS ALERT-BOX.
        RETURN.
     END.

  IF NOT CAN-FIND(FIRST job-mch WHERE
     job-mch.company = company_code AND
     job-mch.job = job.job AND
     job-mch.job-no = job_number AND
     job-mch.job-no2 = INTEGER(job_sub) AND
     job-mch.frm = INTEGER(form_#) AND
     job-mch.blank-no = INTEGER(blank_#)) THEN
     DO:
        MESSAGE 'BLANK NOT VALID FOR JOB/FORM - TRY AGAIN' VIEW-AS ALERT-BOX.
        RETURN.
     END.
  
  FIND FIRST job-hdr WHERE
       job-hdr.company = company_code AND
       job-hdr.job = job.job AND
       job-hdr.frm = INTEGER(form_#) AND
       job-hdr.blank-no = INTEGER(blank_#)
       NO-LOCK NO-ERROR.

  IF NOT AVAIL job-hdr AND blank_# EQ '0' THEN
     FIND FIRST job-hdr WHERE
          job-hdr.company = company_code AND
          job-hdr.job = job.job AND
          job-hdr.frm = INTEGER(form_#)
          NO-LOCK NO-ERROR.

  IF AVAILABLE job-hdr THEN DO:
     ASSIGN item_# = job-hdr.i-no.
     FIND FIRST itemfg WHERE itemfg.company = company_code
                         AND ITEMfg.i-no = job-hdr.i-no NO-LOCK NO-ERROR.
     ITEM_name = IF AVAIL itemfg THEN itemfg.i-name ELSE "".
     DISPLAY ITEM_# ITEM_name WITH FRAME {&FRAME-NAME}.
  END.

  FIND FIRST job-mch WHERE job-mch.company = company_code
                       AND job-mch.job = job.job
                       AND job-mch.job-no = job_number
                       AND job-mch.job-no2 = INTEGER(job_sub)
                       AND job-mch.frm = INTEGER(form_#)
                       AND (job-mch.blank-no = INTEGER(blank_#)
                        OR mach.p-type NE 'B')
                       AND job-mch.m-code = machine_code
                       AND job-mch.dept = mach.dept[1]
                     NO-LOCK NO-ERROR.
  IF NOT AVAILABLE job-mch THEN
  FIND FIRST job-mch WHERE job-mch.company = company_code
                       AND job-mch.job = job.job
                       AND job-mch.job-no = job_number
                       AND job-mch.job-no2 = INTEGER(job_sub)
                       AND job-mch.frm = INTEGER(form_#)
                       AND (job-mch.blank-no = INTEGER(blank_#)
                        OR mach.p-type NE 'B')
                       AND job-mch.dept = mach.dept[1]
                     NO-LOCK NO-ERROR.
  IF NOT AVAILABLE job-mch THEN
  DO:
    MESSAGE 'DEPARTMENT NOT VALID FOR JOB/FORM/BLANK' SKIP(1)
            'ADD THIS DEPARTMENT TO JOB STANDARDS?'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE add-dept AS LOGICAL.
    IF NOT add-dept THEN
    RETURN.
    CREATE job-mch.
    ASSIGN
      job-mch.company = company_code
      job-mch.job = job.job
      job-mch.job-no = job.job-no
      job-mch.job-no2 = job.job-no2
      job-mch.mr-rate  = mach.mr-rate
      job-mch.mr-varoh = mach.mr-varoh
      job-mch.mr-fixoh = mach.mr-fixoh
      job-mch.frm = INTEGER(form_#)
      job-mch.blank-no = INTEGER(blank_#)
      job-mch.pass = INTEGER(pass_#)
      job-mch.m-code = machine_code
      job-mch.i-name = itemfg.i-name WHEN AVAILABLE itemfg
      job-mch.dept = mach.dept[1]
      job-mch.wst-prct = mach.run-spoil
      /* this let's SB know touch screen data collection made changes */
      job-mch.est-op_rec_key = 'TS ' + STRING(TODAY) + ' ' + STRING(TIME,'HH:MM:SS')
      .
     
    FIND CURRENT job-mch NO-LOCK.

    {methods/run_link.i "CONTAINER" "Set_Value" "('job_number',job.job-no)"}
    {methods/run_link.i "CONTAINER" "Set_Value" "('company_code', company_code)"}
    {methods/run_link.i "CONTAINER" "Set_Value" "('machine_code', job-mch.m-code)"}
    {methods/run_link.i "CONTAINER" "Set_Value" "('job_sub', job.job-no2)"}
    {methods/run_link.i "CONTAINER" "Set_Value" "('form_number', form_#)"}
    {methods/run_link.i "CONTAINER" "Set_Value" "('blank_number', blank_#)"}
    {methods/run_link.i "CONTAINER" "Set_Value" "('pass_sequence',pass_#)"}
    {methods/run_link.i "CONTAINER" "Set_Value" "('item_number',item_#)"}

  END.
  ELSE
  DO:
    /* added if condition          ysk */
    IF machine_code <> job-mch.m-code THEN DO:

       IF actual-entered(job-mch.m-code, job-mch.job) = NO THEN DO: 
           RUN promptToReplace (INPUT machine_code, INPUT job-mch.m-code, OUTPUT cAction).
/*            MESSAGE 'MACHINE' machine_code 'NOT DEFINED IN JOB STANDARDS' SKIP(1)   */
/*                 'REPLACE MACHINE' job-mch.m-code 'WITH MACHINE' machine_code + '?' */
/*             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE repl-mach AS LOGICAL. */
           CASE cAction:
             WHEN "Cancel" THEN
               RETURN.
             WHEN "Add" THEN DO:
               CREATE bf-job-mch.
               BUFFER-COPY job-mch EXCEPT m-code frm blank-no pass TO bf-job-mch.
               ASSIGN
               bf-job-mch.m-code   = machine_code
               bf-job-mch.mr-rate  = mach.mr-rate
               bf-job-mch.mr-varoh = mach.mr-varoh
               bf-job-mch.mr-fixoh = mach.mr-fixoh
               bf-job-mch.frm      = INTEGER(form_#)
               bf-job-mch.blank-no = INTEGER(blank_#)
               bf-job-mch.pass     = INTEGER(pass_#)
               bf-job-mch.i-name = itemfg.i-name WHEN AVAILABLE itemfg
               bf-job-mch.dept = mach.dept[1]
               bf-job-mch.wst-prct = mach.run-spoil.

              IF bf-job-mch.n-out EQ 0 THEN bf-job-mch.n-out = 1.
              FIND job-mch WHERE ROWID(job-mch) = ROWID(bf-job-mch) EXCLUSIVE-LOCK.
              RELEASE bf-job-mch.
              IF job-mch.n-on  EQ 0 THEN RUN get-num-on.
              FIND CURRENT job-mch NO-LOCK.
             END.
             WHEN "Replace" THEN DO:
               FIND CURRENT job-mch EXCLUSIVE-LOCK.
               job-mch.m-code = machine_code.
               FIND CURRENT job-mch NO-LOCK.

             END.
           END CASE.
           IF cAction EQ "Cancel" THEN     RETURN.
           
       END.
       ELSE DO:
           MESSAGE 'MACHINE' machine_code 'NOT DEFINED IN JOB STANDARDS' SKIP(1)
                'COPY MACHINE' job-mch.m-code 'TO MACHINE' machine_code + '?'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE copy-mach AS LOGICAL.
           IF NOT copy-mach THEN     RETURN.

           CREATE bf-job-mch.
           BUFFER-COPY job-mch EXCEPT m-code frm blank-no pass TO bf-job-mch.
           ASSIGN
           bf-job-mch.m-code   = machine_code
           bf-job-mch.mr-rate  = mach.mr-rate
           bf-job-mch.mr-varoh = mach.mr-varoh
           bf-job-mch.mr-fixoh = mach.mr-fixoh
           bf-job-mch.frm      = INTEGER(form_#)
           bf-job-mch.blank-no = INTEGER(blank_#)
           bf-job-mch.pass     = INTEGER(pass_#)
           bf-job-mch.i-name = itemfg.i-name WHEN AVAILABLE itemfg
           bf-job-mch.dept = mach.dept[1]
           bf-job-mch.wst-prct = mach.run-spoil.

          IF bf-job-mch.n-out EQ 0 THEN bf-job-mch.n-out = 1.
          FIND job-mch WHERE ROWID(job-mch) = ROWID(bf-job-mch) EXCLUSIVE-LOCK.
          RELEASE bf-job-mch.
          IF job-mch.n-on  EQ 0 THEN RUN get-num-on.
          FIND CURRENT job-mch NO-LOCK.

/*           IF CAN-DO("CR,RC,GU",pc-prdd.dept) THEN DO:    */
/*             job-mch.n-on = job-mch.n-on / job-mch.n-out. */
        
/*            MESSAGE "Please enter #out for this pass?"
                UPDATE tt-job-mch.n-out. */            
/*                                                          */
/*             job-mch.n-on = job-mch.n-on * job-mch.n-out. */
/*                                                          */
/*           END.                                           */

       END.
    END.  /* 07/09/01  YSK */   
    /* ========= 07/09/01  ysk mods ======= */
    {methods/run_link.i "CONTAINER" "Set_Value" "('company_code', company_code)"}
    {methods/run_link.i "CONTAINER" "Set_Value" "('machine_code', machine_code)"}
    {methods/run_link.i "CONTAINER" "Set_Value" "('job_number', job_number)"}
    {methods/run_link.i "CONTAINER" "Set_Value" "('job_sub', job_sub)"}
    {methods/run_link.i "CONTAINER" "Set_Value" "('form_number', form_#)"}
    {methods/run_link.i "CONTAINER" "Set_Value" "('blank_number', blank_#)"}
    {methods/run_link.i "CONTAINER" "Set_Value" "('pass_sequence',pass_#)"}
    {methods/run_link.i "CONTAINER" "Set_Value" "('item_number',item_#)"}
    {methods/run_link.i "CONTAINER" "Change_Page" "(13)"}
    RETURN.
    /* ========= 07/09/01  ysk  end of mods ======= */
  END.
 
  {methods/run_link.i "CONTAINER" "Change_Page" "(10)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verify_Job s-object 
PROCEDURE Verify_Job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF job_# = '' THEN  RETURN NO-APPLY.
  
  {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('machine_code',OUTPUT machine_code)"}
  {methods/run_link.i "CONTAINER" "Set_Value" "('job#',job_#)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('job_number',OUTPUT job_number)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('job_sub',OUTPUT job_sub)"}
  
  /* ==== 7/16/01  YSK default sub_job to "-0" if user does not enter value === */
  if trim(job_sub) = trim(job_number) then do:
     assign job_sub = "0"
            job_# = job_# + "-0"
            .
     display job_# with frame {&frame-name}.       
  end.                                                
  
  FIND FIRST job WHERE job.company = company_code
                   AND job.job-no = job_number
                   AND job.job-no2 = INTEGER(job_sub)
                 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE job THEN
  DO:
    MESSAGE 'INVALID JOB NUMBER - TRY AGAIN' VIEW-AS ALERT-BOX.
    RETURN.
  END.
  /* === check job on hold ==== */
  if job.stat = "H" then do:
     message "JOB ON HOLD. DO NOT PROCESS".
     return.
  end.
  /* ============ end of mods ==============*/
  FIND mach WHERE mach.company = company_code
              AND mach.m-code = machine_code NO-LOCK.
  FIND FIRST job-hdr WHERE job-hdr.company = company_code
                       AND job-hdr.job = job.job NO-LOCK NO-ERROR.
  IF NOT AVAILABLE job-hdr THEN
  DO:
    MESSAGE 'MISSING JOB HEADER - TRY AGAIN' VIEW-AS ALERT-BOX.
    RETURN.
  END.
  FIND est WHERE est.company = company_code
             /*AND est.loc = job-hdr.loc 11081503*/
             AND est.est-no = job-hdr.est-no NO-LOCK NO-ERROR.
  IF NOT AVAILABLE est THEN
  DO:
    MESSAGE 'MISSING ESTIMATE - TRY AGAIN' VIEW-AS ALERT-BOX.
    RETURN.
  END.
  DEF BUFFER bf-machtran FOR machtran.
  FIND FIRST bf-machtran NO-LOCK WHERE bf-machtran.company = company_code 
                       AND bf-machtran.machine = machine_code 
                       AND (bf-machtran.job_number NE job_number
                           OR bf-machtran.job_sub NE int(job_sub) )
                       AND bf-machtran.END_date = ?
                       AND bf-machtran.end_time = 0 
                       AND bf-machtran.TOTAL_time = 0
                       NO-ERROR.
  IF AVAIL bf-machtran THEN do:
     MESSAGE "Job " + trim(bf-machtran.job_number) + "-" + TRIM(string(bf-machtran.job_sub,"99")) +
             " has data collection transaction started. You must end that job's operation before selecting a new job."
             VIEW-AS ALERT-BOX ERROR.
     RETURN error.
  END.
  FIND FIRST job-mch WHERE job-mch.company = company_code
                       AND job-mch.job = job.job
                       AND job-mch.m-code = machine_code
                     NO-LOCK NO-ERROR.
  IF NOT AVAILABLE job-mch THEN
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
       form_# = '1'
       blank_# = IF mach.p-type = 'B' THEN '1' ELSE '0'
       pass_# = '1'.
     FIND FIRST job-hdr WHERE job-hdr.company = company_code
                          AND job-hdr.job = job.job
                          AND job-hdr.frm = INTEGER(form_#)
                          AND job-hdr.blank-no = INTEGER(blank_#)
                        NO-LOCK NO-ERROR.

     IF NOT AVAIL job-hdr AND BLANK_# EQ '0' THEN
        FIND FIRST job-hdr WHERE
             job-hdr.company = company_code AND
             job-hdr.job = job.job AND
             job-hdr.frm = INTEGER(form_#)
             NO-LOCK NO-ERROR.

     IF AVAILABLE job-hdr THEN
        item_# = job-hdr.i-no.
     DISPLAY {&ENTRY-FIELDS}.
     IF est.est-type NE 1 THEN
        ENABLE Btn_Form_#.
     IF mach.p-type = 'B' OR (est.est-type = 3 AND mach.dept[1] = 'PR') THEN
        ENABLE Btn_Blank_# Btn_Item_#.
     IF mach.dept[1] = 'PR' THEN
        ENABLE Btn_Pass_#.
     lVerifyJob = NO.
     DISABLE Btn_Job_#.
     RUN Reset_Field_Colors.
  END. /* do with frame */

  /* === modification to allow user to update transaction for completed job 
         07/05/01  YSK ===============*/
  do WITH FRAME {&FRAME-NAME}:  
    ASSIGN
      form_# = '1'
      blank_# = IF mach.p-type = 'B' THEN '1' ELSE '0'
      pass_# = '1'.
    FIND FIRST job-hdr WHERE job-hdr.company = company_code
                         AND job-hdr.job = job.job
                         AND job-hdr.frm = INTEGER(form_#)
                         AND job-hdr.blank-no = INTEGER(blank_#)
                     NO-LOCK NO-ERROR.

    IF NOT AVAIL job-hdr AND blank_# EQ '0' THEN
       FIND FIRST job-hdr WHERE
            job-hdr.company = company_code AND
            job-hdr.job = job.job AND
            job-hdr.frm = INTEGER(form_#)
            NO-LOCK NO-ERROR.

    IF AVAILABLE job-hdr THEN    item_# = job-hdr.i-no.
    DISPLAY {&ENTRY-FIELDS}.
    IF est.est-type NE 1 THEN    ENABLE Btn_Form_#.
    IF mach.p-type = 'B' OR (est.est-type = 3 AND mach.dept[1] = 'PR') THEN
       ENABLE Btn_Blank_# Btn_Item_#.
    IF mach.dept[1] = 'PR' THEN ENABLE Btn_Pass_#.
    lVerifyJob = NO.
    RUN pSetSensitive ("VerifyJobNumber",NO).
    RUN pSetSensitive ("VerifyEntries",YES).
    DISABLE Btn_Job_#.
    RUN Reset_Field_Colors.
    /*  run verify_entries.*/
  end.
  /* ============= end of mods ============ */
  {touch/localview.i}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION actual-entered s-object 
FUNCTION actual-entered RETURNS LOGICAL
  (INPUT ip-m-code AS CHAR, INPUT ip-job AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR v-qty AS DEC NO-UNDO.

v-qty = 0.
for each mch-act where mch-act.company = company_code and
                       mch-act.job = ip-job AND
                       mch-act.m-code = ip-m-code
                       no-lock:
  v-qty = v-qty + mch-act.hours.
END.

IF v-qty GT 0 THEN
  RETURN TRUE.
ELSE
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

