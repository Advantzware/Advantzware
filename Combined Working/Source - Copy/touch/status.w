&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: touch/status.w

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

DEFINE VARIABLE cType AS CHARACTER NO-UNDO.

&SCOPED-DEFINE PageNo 7
{touch/touchdef.i}

&Scoped-define BUTTON-INCLUDE STATUS

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 ~
IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 IMAGE-10 EDITOR-1 EDITOR-6 EDITOR-2 ~
EDITOR-7 EDITOR-3 EDITOR-8 EDITOR-4 EDITOR-9 EDITOR-5 EDITOR-10 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-1 EDITOR-6 EDITOR-2 EDITOR-7 ~
EDITOR-3 EDITOR-8 EDITOR-4 EDITOR-9 EDITOR-5 EDITOR-10 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 ~
IMAGE-7 IMAGE-9 IMAGE-10 EDITOR-1 EDITOR-6 EDITOR-2 EDITOR-7 EDITOR-3 ~
EDITOR-8 EDITOR-4 EDITOR-9 EDITOR-5 EDITOR-10 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 32 BY 2.14 NO-UNDO.

DEFINE VARIABLE EDITOR-10 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 32 BY 2.14 NO-UNDO.

DEFINE VARIABLE EDITOR-2 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 32 BY 2.14 NO-UNDO.

DEFINE VARIABLE EDITOR-3 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 32 BY 2.14 NO-UNDO.

DEFINE VARIABLE EDITOR-4 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 32 BY 2.14 NO-UNDO.

DEFINE VARIABLE EDITOR-5 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 32 BY 2.14 NO-UNDO.

DEFINE VARIABLE EDITOR-6 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 32 BY 2.14 NO-UNDO.

DEFINE VARIABLE EDITOR-7 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 32 BY 2.14 NO-UNDO.

DEFINE VARIABLE EDITOR-8 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 32 BY 2.14 NO-UNDO.

DEFINE VARIABLE EDITOR-9 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 32 BY 2.14 NO-UNDO.

DEFINE IMAGE IMAGE-1
     STRETCH-TO-FIT RETAIN-SHAPE TRANSPARENT
     SIZE 9 BY 2.14.

DEFINE IMAGE IMAGE-10
     STRETCH-TO-FIT RETAIN-SHAPE TRANSPARENT
     SIZE 9 BY 2.14.

DEFINE IMAGE IMAGE-2
     STRETCH-TO-FIT RETAIN-SHAPE TRANSPARENT
     SIZE 9 BY 2.14.

DEFINE IMAGE IMAGE-3
     STRETCH-TO-FIT RETAIN-SHAPE TRANSPARENT
     SIZE 9 BY 2.14.

DEFINE IMAGE IMAGE-4
     STRETCH-TO-FIT RETAIN-SHAPE TRANSPARENT
     SIZE 9 BY 2.14.

DEFINE IMAGE IMAGE-5
     STRETCH-TO-FIT RETAIN-SHAPE TRANSPARENT
     SIZE 9 BY 2.14.

DEFINE IMAGE IMAGE-6
     STRETCH-TO-FIT RETAIN-SHAPE TRANSPARENT
     SIZE 9 BY 2.14.

DEFINE IMAGE IMAGE-7
     STRETCH-TO-FIT RETAIN-SHAPE TRANSPARENT
     SIZE 9 BY 2.14.

DEFINE IMAGE IMAGE-8
     STRETCH-TO-FIT RETAIN-SHAPE TRANSPARENT
     SIZE 9 BY 2.14.

DEFINE IMAGE IMAGE-9
     STRETCH-TO-FIT RETAIN-SHAPE TRANSPARENT
     SIZE 9 BY 2.14.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 12.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     EDITOR-1 AT ROW 1.95 COL 11 NO-LABEL WIDGET-ID 12
     EDITOR-6 AT ROW 1.95 COL 53 NO-LABEL WIDGET-ID 24
     EDITOR-2 AT ROW 4.33 COL 11 NO-LABEL WIDGET-ID 14
     EDITOR-7 AT ROW 4.33 COL 53 NO-LABEL WIDGET-ID 26
     EDITOR-3 AT ROW 6.71 COL 11 NO-LABEL WIDGET-ID 16
     EDITOR-8 AT ROW 6.71 COL 53 NO-LABEL WIDGET-ID 30
     EDITOR-4 AT ROW 9.1 COL 11 NO-LABEL WIDGET-ID 18
     EDITOR-9 AT ROW 9.1 COL 53 NO-LABEL WIDGET-ID 34
     EDITOR-5 AT ROW 11.48 COL 11 NO-LABEL WIDGET-ID 20
     EDITOR-10 AT ROW 11.48 COL 53 NO-LABEL WIDGET-ID 38
     RECT-1 AT ROW 1 COL 1
     IMAGE-1 AT ROW 1.95 COL 2 WIDGET-ID 2
     IMAGE-2 AT ROW 4.33 COL 2 WIDGET-ID 4
     IMAGE-3 AT ROW 6.71 COL 2 WIDGET-ID 6
     IMAGE-4 AT ROW 9.1 COL 2 WIDGET-ID 8
     IMAGE-5 AT ROW 11.48 COL 2 WIDGET-ID 10
     IMAGE-6 AT ROW 1.95 COL 44 WIDGET-ID 22
     IMAGE-7 AT ROW 4.33 COL 44 WIDGET-ID 28
     IMAGE-8 AT ROW 6.71 COL 44 WIDGET-ID 32
     IMAGE-9 AT ROW 9.1 COL 44 WIDGET-ID 36
     IMAGE-10 AT ROW 11.48 COL 44 WIDGET-ID 40
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

/* SETTINGS FOR EDITOR EDITOR-1 IN FRAME F-Main
   1                                                                    */
ASSIGN 
       EDITOR-1:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-10 IN FRAME F-Main
   1                                                                    */
ASSIGN 
       EDITOR-10:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-2 IN FRAME F-Main
   1                                                                    */
ASSIGN 
       EDITOR-2:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-3 IN FRAME F-Main
   1                                                                    */
ASSIGN 
       EDITOR-3:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-4 IN FRAME F-Main
   1                                                                    */
ASSIGN 
       EDITOR-4:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-5 IN FRAME F-Main
   1                                                                    */
ASSIGN 
       EDITOR-5:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-6 IN FRAME F-Main
   1                                                                    */
ASSIGN 
       EDITOR-6:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-7 IN FRAME F-Main
   1                                                                    */
ASSIGN 
       EDITOR-7:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-8 IN FRAME F-Main
   1                                                                    */
ASSIGN 
       EDITOR-8:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-9 IN FRAME F-Main
   1                                                                    */
ASSIGN 
       EDITOR-9:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR IMAGE IMAGE-1 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR IMAGE IMAGE-10 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR IMAGE IMAGE-2 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR IMAGE IMAGE-3 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR IMAGE IMAGE-4 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR IMAGE IMAGE-5 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR IMAGE IMAGE-6 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR IMAGE IMAGE-7 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR IMAGE IMAGE-9 IN FRAME F-Main
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Employee_Status s-object 
PROCEDURE Employee_Status :
/*------------------------------------------------------------------------------
  Purpose:     get employee status
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('employee_code',OUTPUT employee_code)"}
  itemlist = ''.
  FOR EACH emplogin NO-LOCK
      WHERE emplogin.company    EQ company_code
        AND emplogin.employee   EQ employee_code
        AND emplogin.END_date   EQ ?
        AND emplogin.end_time   EQ 0
        AND emplogin.total_time EQ 0
      :
    FIND FIRST mach NO-LOCK
         WHERE mach.company EQ company_code
           AND mach.m-code  EQ emplogin.machine
         NO-ERROR.
    itemlist = itemlist
             + 'Graphics\48x48\gearwheels.png' + ','
             + CAPS(emplogin.machine) + ','
             + (IF AVAILABLE mach THEN mach.m-dscr ELSE '** error **') + ','
             + STRING(emplogin.start_date) + ' @ '
             + STRING(emplogin.start_time,'HH:MM am')
             + '|'
             .
  END.
  ASSIGN
      itemlist = TRIM(itemlist,'|')
      cType = 'Employee'
      .
  RUN Status_Labels (INPUT-OUTPUT button_item,cType).

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
  RUN pCreateINIObjects ("PageUp,PageDown,Home").  

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Machine_Status s-object 
PROCEDURE Machine_Status :
/*------------------------------------------------------------------------------
  Purpose:     get machine status
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('machine_code',OUTPUT machine_code)"}
  ASSIGN
    itemlist = ''
    button_item = 1
    .
  FOR EACH emplogin NO-LOCK
      WHERE emplogin.company = company_code
        AND emplogin.machine = machine_code
        AND emplogin.END_date = ?
        AND emplogin.end_time = 0
        AND emplogin.total_time = 0
      :
    FIND employee OF emplogin NO-LOCK NO-ERROR.
    IF NOT AVAILABLE employee THEN NEXT.
    itemlist = itemlist
             + 'Graphics\48x48\user.png' + ','
             + employee.last_name + ','
             + employee.first_name + ','
             + employee.employee + ','
             + '' + ','
             + 'Login: '
             + STRING(emplogin.start_date) + ' @ '
             + STRING(emplogin.start_time,'HH:MM am')
             + '|'
             .
  END.
  ASSIGN
      itemlist = TRIM(itemlist,'|')
      cType = 'Machine'
      .
  RUN Status_Labels (INPUT-OUTPUT button_item,cType).

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
        WHEN "PageUp" THEN DO:
            button_item = button_item - 20.
            RUN Status_Labels (INPUT-OUTPUT button_item,cType).
        END.
        WHEN "PageDown" THEN DO:
            RUN Status_Labels (INPUT-OUTPUT button_item,cType).
        END.
        WHEN "Home" THEN DO:
            {methods/run_link.i "CONTAINER" "Change_Page" "(2)"}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Status_Labels s-object 
PROCEDURE Status_Labels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {touch/status.i}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

