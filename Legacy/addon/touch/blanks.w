&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: touch/blanks.w

  Description: from SMART.W - Template for basic SmartObject

  Author: Ron Stark
  Created: 4.18.2000

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

&SCOPED-DEFINE PageNo 11
{touch/touchdef.i}

&Scoped-define BUTTON-INCLUDE BLANKS

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_Button-1 Btn_Button-6 ~
Btn_Button-2 Btn_Button-7 Btn_Button-3 Btn_Button-8 Btn_Button-4 ~
Btn_Button-9 Btn_Button-5 Btn_Button-10 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Btn_Button-1 Btn_Button-6 Btn_Button-2 Btn_Button-7 ~
Btn_Button-3 Btn_Button-8 Btn_Button-4 Btn_Button-9 Btn_Button-5 ~
Btn_Button-10 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Button-1 
     LABEL "BUTTON 1" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-10 
     LABEL "BUTTON 10" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-2 
     LABEL "BUTTON 2" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-3 
     LABEL "BUTTON 3" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-4 
     LABEL "BUTTON 4" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-5 
     LABEL "BUTTON 5" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-6 
     LABEL "BUTTON 6" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-7 
     LABEL "BUTTON 7" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-8 
     LABEL "BUTTON 8" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-9 
     LABEL "BUTTON 9" 
     SIZE 40 BY 2.38.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 12.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_Button-1 AT ROW 1.71 COL 2
     Btn_Button-6 AT ROW 1.71 COL 43
     Btn_Button-2 AT ROW 4.1 COL 2
     Btn_Button-7 AT ROW 4.1 COL 43
     Btn_Button-3 AT ROW 6.48 COL 2
     Btn_Button-8 AT ROW 6.48 COL 43
     Btn_Button-4 AT ROW 8.86 COL 2
     Btn_Button-9 AT ROW 8.86 COL 43
     Btn_Button-5 AT ROW 11.24 COL 2
     Btn_Button-10 AT ROW 11.24 COL 43
     "BLANK" VIEW-AS TEXT
          SIZE 8 BY .52 AT ROW 1.24 COL 3 WIDGET-ID 2
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON Btn_Button-1 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-10 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-2 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-3 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-4 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-5 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-6 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-7 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-8 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-9 IN FRAME F-Main
   1                                                                    */
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

&Scoped-define SELF-NAME Btn_Button-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-1 s-object
ON CHOOSE OF Btn_Button-1 IN FRAME F-Main /* BUTTON 1 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-10 s-object
ON CHOOSE OF Btn_Button-10 IN FRAME F-Main /* BUTTON 10 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-2 s-object
ON CHOOSE OF Btn_Button-2 IN FRAME F-Main /* BUTTON 2 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-3 s-object
ON CHOOSE OF Btn_Button-3 IN FRAME F-Main /* BUTTON 3 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-4 s-object
ON CHOOSE OF Btn_Button-4 IN FRAME F-Main /* BUTTON 4 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-5 s-object
ON CHOOSE OF Btn_Button-5 IN FRAME F-Main /* BUTTON 5 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-6 s-object
ON CHOOSE OF Btn_Button-6 IN FRAME F-Main /* BUTTON 6 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-7 s-object
ON CHOOSE OF Btn_Button-7 IN FRAME F-Main /* BUTTON 7 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-8 s-object
ON CHOOSE OF Btn_Button-8 IN FRAME F-Main /* BUTTON 8 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-9 s-object
ON CHOOSE OF Btn_Button-9 IN FRAME F-Main /* BUTTON 9 */
DO:
  {touch/buttons.i}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Button_Labels s-object 
PROCEDURE Button_Labels :
/*------------------------------------------------------------------------------
  Purpose:     place values on button labels
  Parameters:  Input current button item
  Notes:       
------------------------------------------------------------------------------*/
  {touch/btnlabel.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Blanks s-object 
PROCEDURE Get_Blanks :
/*------------------------------------------------------------------------------
  Purpose:     populate selection list with blank numbers
  Parameters:  <none>
  Notes:     03/23/01  YSK  display all blanks for the job  
------------------------------------------------------------------------------*/
  {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('machine_code',OUTPUT machine_code)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('job_number',OUTPUT job_number)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('job_sub',OUTPUT job_sub)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('form_number',OUTPUT form_number)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('machine_list',OUTPUT machine_list)"}

  ASSIGN
    itemlist = ''
    button_item = 1.
    
  def var lv-itemlist as cha no-undo.
    
  FIND FIRST job WHERE job.company = company_code
                   AND job.job-no = job_number
                   AND job.job-no2 = INTEGER(job_sub)
                 NO-LOCK NO-ERROR.
  IF AVAILABLE job THEN
  FOR EACH job-mch NO-LOCK WHERE job-mch.company = company_code
                             AND job-mch.job = job.job
                             AND job-mch.frm = INTEGER(form_number)
                             AND (job-mch.m-code = machine_code or
                                  job-mch.dept = mach.dept[1] OR
                                  LOOKUP(job-mch.m-code,machine_list) > 0 )
                             /*/*  all blanks for the job */
                             and job-mch.blank-no > 0 /* only for blank fed*/
                             */
                             :
                                     
  /*  IF INDEX(itemlist,STRING(job-mch.blank-no)) NE 0 THEN
    NEXT.
  */
    if INDEX(lv-itemlist,STRING(job-mch.blank-no)) NE 0 THEN  NEXT.   
    
    IF NOT CAN-FIND(FIRST cmpltjob WHERE cmpltjob.company = company_code
                                  AND cmpltjob.machine = machine_code
                                  AND cmpltjob.job_number = job-mch.job-no
                                  AND cmpltjob.job_sub = job-mch.job-no2
                                  AND cmpltjob.FORM_number = job-mch.frm
                                  AND cmpltjob.blank_number = job-mch.blank-no)
     THEN DO:
         assign itemlist = IF itemlist = '' THEN 'BLANK: ' + STRING(job-mch.blank-no)
                      ELSE itemlist + '@' + 'BLANK: ' + STRING(job-mch.blank-no)
           lv-itemlist = IF lv-itemlist = '' THEN 'BLANK: ' + STRING(job-mch.blank-no)
                         ELSE lv-itemlist + '@' + 'BLANK: ' + STRING(job-mch.blank-no)
                         .
         FIND FIRST job-hdr WHERE job-hdr.company = company_code
                         AND job-hdr.job-no = job-mch.job-no
                         AND job-hdr.job-no2 = job-mch.job-no2
                         AND job-hdr.frm = job-mch.frm
                         AND job-hdr.blank-no = job-mch.blank-no
                       NO-LOCK NO-ERROR.
         IF NOT AVAIL job-hdr THEN
            FIND FIRST job-hdr WHERE job-hdr.company = company_code
                         AND job-hdr.job-no = job-mch.job-no
                         AND job-hdr.job-no2 = job-mch.job-no2
                         AND job-hdr.frm = job-mch.frm
                         /*AND job-hdr.blank-no = job-mch.blank-no
                              For Sheet fed machine - PRESS...  */
                       NO-LOCK NO-ERROR.
          IF AVAILABLE job-hdr THEN
             itemlist = itemlist + ' (' + job-hdr.i-no + ')'.
    END.
  END.
  
  RUN Button_Labels (INPUT-OUTPUT button_item).
  
  IF button_item = 2 THEN
  APPLY 'CHOOSE' TO Btn_Button-1 IN FRAME {&FRAME-NAME}.

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
  RUN pCreateINIObjects ("HomeSmall,PageUp,PageDown,First,Last,Back").

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  {touch/localview.i}

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
        WHEN "HomeSmall" THEN DO:
            {methods/run_link.i "CONTAINER" "Change_Page" "(2)"}
        END.
        WHEN "PageUp" THEN DO:
            button_item = button_item - 20.
            RUN Button_Labels (INPUT-OUTPUT button_item).
        END.
        WHEN "PageDown" THEN DO:
            RUN Button_Labels (INPUT-OUTPUT button_item).
        END.
        WHEN "First" THEN DO:
            button_item = 1.
            RUN Button_Labels (INPUT-OUTPUT button_item).
        END.
        WHEN "Last" THEN DO:
            button_item = NUM-ENTRIES(itemlist,'@') + 1.
            RUN Button_Labels (INPUT-OUTPUT button_item).
        END.
        WHEN "Back" THEN DO:
            {methods/run_link.i "CONTAINER" "Change_Page" "(9)"}
        END.
    END CASE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-blank s-object 
PROCEDURE valid-blank :
/*------------------------------------------------------------------------------
  Purpose:     validate blank for the machine and update machine-code in job-mch
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('machine_code',OUTPUT machine_code)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('job_number',OUTPUT job_number)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('job_sub',OUTPUT job_sub)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('form_number',OUTPUT form_number)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('blank_number',OUTPUT blank_number)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('machine_list',OUTPUT machine_list)"}
  FIND FIRST job WHERE job.company = company_code
                   AND job.job-no = job_number
                   AND job.job-no2 = INTEGER(job_sub)
                 NO-LOCK NO-ERROR.
  IF AVAILABLE job THEN do:
    find first job-mch NO-LOCK WHERE job-mch.company = company_code
                                 AND job-mch.job = job.job
                                 AND job-mch.frm = INTEGER(form_number)
                                 AND job-mch.m-code = machine_code 
                                 and job-mch.blank-no =  integer(blank_number)
                             no-error.                          
    if not avail job-mch then do:       
       find first job-mch NO-LOCK WHERE job-mch.company = company_code
                                 AND job-mch.job = job.job
                                 AND job-mch.frm = INTEGER(form_number)
                                 AND job-mch.m-code <> machine_code 
                                 and job-mch.blank-no =  integer(blank_number)
                             no-error.                                                                  
       if avail job-mch then do:
           find first job-mch NO-LOCK WHERE job-mch.company = company_code
                                  AND job-mch.job = job.job
                                  AND job-mch.frm = INTEGER(form_number)
                                  AND LOOKUP(job-mch.m-code,machine_list) > 0 
                                  and job-mch.blank-no =  integer(blank_number)
                              no-error.                                      
           IF AVAIL job-mch THEN DO:
              machine_code = job-mch.m-code.
              {methods/run_link.i "CONTAINER" "SET_Value" "('machine_code',job-mch.m-code)"}
           END.
           ELSE DO:
                MESSAGE 'MACHINE' machine_code 'NOT DEFINED IN JOB STANDARDS' SKIP(1)
                   'REPLACE MACHINE' job-mch.m-code 'WITH MACHINE' machine_code + '?'
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE repl-mach AS LOGICAL.
                IF NOT repl-mach THEN      RETURN error.
                FIND CURRENT job-mch EXCLUSIVE-LOCK.
                job-mch.m-code = machine_code.
                FIND CURRENT job-mch NO-LOCK.
           END.
       end.
    end.  
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

