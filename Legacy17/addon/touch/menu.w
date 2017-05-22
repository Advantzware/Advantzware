&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: touch/menu.w

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
DEFINE VARIABLE hJobChecklist AS HANDLE NO-UNDO.

{touch/touchdef.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_Change_Company Btn_Login ~
Btn_Job_Data_Collection Btn_Logout Btn_Employee_Status Btn_Machine_Status ~
Btn_Close Btn_Job_Checklist 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 Btn_Login Btn_Job_Data_Collection Btn_Logout ~
Btn_Employee_Status Btn_Machine_Status Btn_Close Btn_Job_Checklist 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnLanguage-1 
     LABEL "Lang 1" 
     SIZE 14 BY 2.38.

DEFINE BUTTON btnLanguage-2 
     LABEL "Lang 2" 
     SIZE 14 BY 2.38.

DEFINE BUTTON btnLanguage-3 
     LABEL "Lang 3" 
     SIZE 14 BY 2.38.

DEFINE BUTTON btnLanguage-4 
     LABEL "Lang 4" 
     SIZE 14 BY 2.38.

DEFINE BUTTON btnLanguage-5 
     LABEL "Lang 5" 
     SIZE 14 BY 2.38.

DEFINE BUTTON btnLanguage-6 
     LABEL "Lang 6" 
     SIZE 14 BY 2.38.

DEFINE BUTTON btnLanguage-7 
     LABEL "Lang 7" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Change_Company 
     IMAGE-UP FILE "images\exit-au":U
     LABEL "Exit" 
     SIZE 10 BY 2.38.

DEFINE BUTTON Btn_Close 
     LABEL "CLOSE" 
     SIZE 20.4 BY 2.38 TOOLTIP "CLOSE".

DEFINE BUTTON Btn_Employee_Status 
     LABEL "EMPLOYEE STATUS" 
     SIZE 33 BY 2.38 TOOLTIP "EMPLOYEE STATUS".

DEFINE BUTTON Btn_Job_Checklist 
     LABEL "JOB CHECKLIST" 
     SIZE 37 BY 2.38 TOOLTIP "JOB CHECKLIST".

DEFINE BUTTON Btn_Job_Data_Collection 
     LABEL "JOB DATA COLLECTION" 
     SIZE 44 BY 2.38 TOOLTIP "JOB DATA COLLECTION".

DEFINE BUTTON Btn_Login 
     LABEL "LOGIN" 
     SIZE 40 BY 2.38 TOOLTIP "LOGIN".

DEFINE BUTTON Btn_Logout 
     LABEL "LOGOUT" 
     SIZE 40 BY 2.38 TOOLTIP "LOGOUT".

DEFINE BUTTON Btn_Machine_Status 
     LABEL "MACHINE STATUS" 
     SIZE 31.2 BY 2.38 TOOLTIP "MACHINE STATUS".

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 12.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnLanguage-1 AT ROW 1.24 COL 2
     btnLanguage-2 AT ROW 1.24 COL 17
     btnLanguage-3 AT ROW 1.24 COL 32
     btnLanguage-4 AT ROW 1.24 COL 47
     btnLanguage-5 AT ROW 1.24 COL 62
     btnLanguage-6 AT ROW 1.24 COL 77
     btnLanguage-7 AT ROW 1.24 COL 92
     Btn_Change_Company AT ROW 1.24 COL 114
     Btn_Login AT ROW 4.81 COL 17
     Btn_Job_Data_Collection AT ROW 6.24 COL 62
     Btn_Logout AT ROW 7.67 COL 17
     Btn_Employee_Status AT ROW 11.24 COL 2
     Btn_Machine_Status AT ROW 11.24 COL 35.2
     Btn_Close AT ROW 11.24 COL 66.6
     Btn_Job_Checklist AT ROW 11.24 COL 87.2 WIDGET-ID 2
    /* "New" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 3.62 COL 114
     "Company" VIEW-AS TEXT
          SIZE 10.6 BY .62 AT ROW 4.33 COL 114 */
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 7 FGCOLOR 15 FONT 6.


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

/* SETTINGS FOR BUTTON btnLanguage-1 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnLanguage-1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btnLanguage-2 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnLanguage-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btnLanguage-3 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnLanguage-3:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btnLanguage-4 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnLanguage-4:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btnLanguage-5 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnLanguage-5:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btnLanguage-6 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnLanguage-6:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btnLanguage-7 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnLanguage-7:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON Btn_Close IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Close:PRIVATE-DATA IN FRAME F-Main     = 
                "CLOSE".

/* SETTINGS FOR BUTTON Btn_Employee_Status IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Employee_Status:PRIVATE-DATA IN FRAME F-Main     = 
                "EMPLOYEE STATUS".

/* SETTINGS FOR BUTTON Btn_Job_Checklist IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Job_Checklist:PRIVATE-DATA IN FRAME F-Main     = 
                "JOB CHECKLIST".

/* SETTINGS FOR BUTTON Btn_Job_Data_Collection IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Job_Data_Collection:PRIVATE-DATA IN FRAME F-Main     = 
                "JOB DATA COLLECTION".

/* SETTINGS FOR BUTTON Btn_Login IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Login:PRIVATE-DATA IN FRAME F-Main     = 
                "LOGIN".

/* SETTINGS FOR BUTTON Btn_Logout IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Logout:PRIVATE-DATA IN FRAME F-Main     = 
                "LOGOUT".

/* SETTINGS FOR BUTTON Btn_Machine_Status IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Machine_Status:PRIVATE-DATA IN FRAME F-Main     = 
                "MACHINE STATUS".

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

&Scoped-define SELF-NAME btnLanguage-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLanguage-1 s-object
ON CHOOSE OF btnLanguage-1 IN FRAME F-Main /* Lang 1 */
,btnLanguage-2,btnLanguage-3,btnLanguage-4
,btnLanguage-5,btnLanguage-6,btnLanguage-7
DO:
  {methods/run_link.i "CONTAINER" "Set_Value" "('label_language',SELF:LABEL)"}
  {methods/run_link.i "CONTAINER" "Change_Page" "(2)"}
  {touch/localview.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Change_Company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Change_Company s-object
ON CHOOSE OF Btn_Change_Company IN FRAME F-Main /* CHANGE COMPANY */
DO:
  /* {methods/run_link.i "CONTAINER" "Change_Page" "(1)"} */
    {methods/run_link.i "CONTAINER" "Close_Touch_Screen"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close s-object
ON CHOOSE OF Btn_Close IN FRAME F-Main /* CLOSE */
DO:
  {methods/run_link.i "CONTAINER" "Close_Touch_Screen"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Employee_Status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Employee_Status s-object
ON CHOOSE OF Btn_Employee_Status IN FRAME F-Main /* EMPLOYEE STATUS */
DO:
  {methods/run_link.i "CONTAINER" "Set_Value" "('employee_code','')"}
  {methods/run_link.i "CONTAINER" "Set_Value" "('activity_status','employee-status')"}
  {methods/run_link.i "CONTAINER" "Change_Page" "(3)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Job_Checklist
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Job_Checklist s-object
ON CHOOSE OF Btn_Job_Checklist IN FRAME F-Main /* JOB CHECKLIST */
DO:
    RUN windows/machtran.w PERSISTENT SET hJobChecklist.
    RUN adm-initialize IN hJobChecklist.
    RUN Disable-Folder-Tabs IN hJobChecklist (INPUT THIS-PROCEDURE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Job_Data_Collection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Job_Data_Collection s-object
ON CHOOSE OF Btn_Job_Data_Collection IN FRAME F-Main /* JOB DATA COLLECTION */
DO:
  {methods/run_link.i "CONTAINER" "Set_Value" "('employee_code','')"}
  {methods/run_link.i "CONTAINER" "Set_Value" "('activity_status','job-data-collection')"}
  {methods/run_link.i "CONTAINER" "Change_Page" "(5)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Login
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Login s-object
ON CHOOSE OF Btn_Login IN FRAME F-Main /* LOGIN */
DO:
  {methods/run_link.i "CONTAINER" "Set_Value" "('employee_code','')"}
  {methods/run_link.i "CONTAINER" "Set_Value" "('activity_status','login')"}
  {methods/run_link.i "CONTAINER" "Change_Page" "(3)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Logout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Logout s-object
ON CHOOSE OF Btn_Logout IN FRAME F-Main /* LOGOUT */
DO:
  {methods/run_link.i "CONTAINER" "Set_Value" "('employee_code','')"}
  {methods/run_link.i "CONTAINER" "Set_Value" "('activity_status','logout')"}
  {methods/run_link.i "CONTAINER" "Change_Page" "(3)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Machine_Status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Machine_Status s-object
ON CHOOSE OF Btn_Machine_Status IN FRAME F-Main /* MACHINE STATUS */
DO:
  {methods/run_link.i "CONTAINER" "Set_Value" "('employee_code','')"}
  {methods/run_link.i "CONTAINER" "Set_Value" "('activity_status','machine-status')"}
  {methods/run_link.i "CONTAINER" "Change_Page" "(5)"}
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Destroy-JobNotes s-object 
PROCEDURE Destroy-JobNotes :
/*------------------------------------------------------------------------------
  Purpose:     Destroy the job notes procedure handle.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DELETE OBJECT hJobChecklist NO-ERROR.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize s-object 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DEFINE VARIABLE languageList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  IF languageList NE '' THEN RETURN.

  {methods/run_link.i "CONTAINER" "Get_Value" "('language_list',OUTPUT languageList)"}
  DO i = 1 TO NUM-ENTRIES(languageList) WITH FRAME {&FRAME-NAME}:
    {touch/btnLanguage.i 1}
    {touch/btnLanguage.i 2}
    {touch/btnLanguage.i 3}
    {touch/btnLanguage.i 4}
    {touch/btnLanguage.i 5}
    {touch/btnLanguage.i 6}
    {touch/btnLanguage.i 7}
  END.

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

