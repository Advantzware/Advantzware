&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: touch/alljobs.w

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

&SCOPED-DEFINE PageNo 15
{touch/touchdef.i}
{custom/globdefs.i}

DO TRANSACTION:
   {sys/inc/tskey.i}
END.

DEF TEMP-TABLE tt-job-list NO-UNDO
    FIELD job-text AS CHAR FORMAT "X(40)"
    FIELD job-no AS CHAR FORMAT "X(6)"
    FIELD job-no2 AS INT
    FIELD COUNT AS INT
    INDEX job-idx2 IS PRIMARY COUNT ASC
    INDEX job-idx job-no ASC job-no2 ASC.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-job-list

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-job-list.job-text   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-job-list USE-INDEX job-idx ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-job-list USE-INDEX job-idx ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-job-list
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-job-list


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 BROWSE-2 keystroke 
&Scoped-Define DISPLAYED-OBJECTS keystroke 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE keystroke AS CHARACTER FORMAT "X(256)":U 
     LABEL "Job Filter" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 TOOLTIP "Job Filter"
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 12.95.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-job-list SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 s-object _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-job-list.job-text WIDTH 46 LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 40 BY 12.38
         BGCOLOR 15 FGCOLOR 0 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BROWSE-2 AT ROW 1.24 COL 2
     keystroke AT ROW 1.24 COL 65 COLON-ALIGNED
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
/* BROWSE-TAB BROWSE-2 RECT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       keystroke:PRIVATE-DATA IN FRAME F-Main     = 
                "Job Filter".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-job-list USE-INDEX job-idx ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 s-object
ON MOUSE-SELECT-DBLCLICK OF BROWSE-2 IN FRAME F-Main
DO:
    RUN pClick ("SelectJob").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME keystroke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL keystroke s-object
ON VALUE-CHANGED OF keystroke IN FRAME F-Main /* Job Filter */
DO:
   IF tskey-log = NO THEN
   DO:
      IF keystroke:SCREEN-VALUE NE "" THEN
         RUN key_stroke(INPUT SUBSTRING(keystroke:SCREEN-VALUE,LENGTH(keystroke:SCREEN-VALUE))).
      ELSE
         RUN key_stroke(INPUT "Clear").
   END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Jobs s-object 
PROCEDURE Get_Jobs :
/*------------------------------------------------------------------------------
  Purpose:     populate selection list with job numbers
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR viCount AS INT NO-UNDO.
  
  {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('machine_code',OUTPUT machine_code)"}
  
  EMPTY TEMP-TABLE tt-job-list.

  FOR EACH job-mch NO-LOCK WHERE job-mch.company = company_code
                             AND job-mch.m-code = machine_code
                             BY job-mch.job-no 
                             BY job-mch.job-no2:

    IF CAN-FIND(FIRST tt-job-list WHERE
       tt-job-list.job-no EQ job-mch.job-no AND
       tt-job-list.job-no2 EQ job-mch.job-no2) THEN
       NEXT.

    FIND job OF job-mch NO-LOCK NO-ERROR.
    IF NOT AVAILABLE job THEN
    NEXT.
/* Removed so Fibre can run to any Job not just open ones
    IF CAN-DO('C,Z',job.stat) THEN
    NEXT.
*/
    IF CAN-FIND(cmpltjob WHERE cmpltjob.company = company_code
                           AND cmpltjob.machine = machine_code
                           AND cmpltjob.job_number = job-mch.job-no
                           AND cmpltjob.job_sub = job-mch.job-no2) THEN
    NEXT.

    CREATE tt-job-list.
    ASSIGN tt-job-list.job-text = LEFT-TRIM(job-mch.job-no) + '-' + STRING(job-mch.job-no2)
                                + IF '   (' + STRING(job-mch.start-date) + ' - ' + STRING(job-mch.start-time,'HH:MM am') + ')' EQ ? THEN ""
                                  ELSE '   (' + STRING(job-mch.start-date) + ' - ' + STRING(job-mch.start-time,'HH:MM am') + ')'
           tt-job-list.job-no   = job-mch.job-no
           tt-job-list.job-no2  = job-mch.job-no2
           viCount              = viCount + 1
           tt-job-list.COUNT    = viCount.
    RELEASE tt-job-list.
  END.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      h_field = keystroke:HANDLE
      field_value = ''
      h_field = keystroke:HANDLE
      h_field:SCREEN-VALUE = ''.

    open query {&browse-name} for each tt-job-list no-lock.
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
  DEFINE INPUT PARAMETER keystroke AS CHARACTER NO-UNDO.

  IF INDEX(keystroke,'(ON)') NE 0 THEN
     caps_lock = YES.
  ELSE
  IF INDEX(keystroke,'(OFF)') NE 0 THEN
     caps_lock = NO.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
     IF NOT caps_lock THEN
        keystroke = LC(keystroke).
     CASE keystroke:
       WHEN 'ALPHA' THEN
       {methods/run_link.i "CONTAINER" "Display_Keyboard" "('alphabet.',THIS-PROCEDURE:HANDLE)"}
       WHEN 'BACKSPACE' THEN
       IF field_value NE '' THEN
       field_value = SUBSTR(field_value,1,LENGTH(field_value) - 1).
       WHEN 'CLEAR' THEN
       field_value = ''.
       WHEN 'DQ' THEN /* Double Quote */
       field_value = field_value + '"'.
       WHEN 'QWERTY' THEN
       {methods/run_link.i "CONTAINER" "Display_Keyboard" "('keyboard.',THIS-PROCEDURE:HANDLE)"}
       WHEN 'SPACE' THEN
       field_value = field_value + '`'.
       WHEN 'SORT' THEN
       {methods/run_link.i "CONTAINER" "Display_Keyboard" "('sortpad.',THIS-PROCEDURE:HANDLE)"}
       OTHERWISE
       field_value = field_value + keystroke.
     END CASE.
     IF VALID-HANDLE(h_field) THEN
     h_field:SCREEN-VALUE = REPLACE(field_value,'`',' ').
  END.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

     FIND FIRST tt-job-list WHERE
          tt-job-list.job-text BEGINS h_field:SCREEN-VALUE
          NO-LOCK NO-ERROR.

     IF NOT AVAIL tt-job-list THEN
     DO:
        MESSAGE 'NO MATCH FOUND BEGINNING' SKIP(1) h_field:SCREEN-VALUE
           VIEW-AS ALERT-BOX.
        RUN Key_Stroke ('BACKSPACE').
        RETURN.
     END.

     REPOSITION {&browse-name} TO ROWID(ROWID(tt-job-list)).
  END.  

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
  RUN pCreateINIObjects ("HomeSmall,Up,PageUp,Down,PageDown,SelectJob,Back").

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF tskey-log EQ NO THEN
     keystroke:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

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
        WHEN "Up" THEN DO:
            APPLY "CURSOR-UP":U TO BROWSE {&BROWSE-NAME}.
        END.
        WHEN "PageUp" THEN DO:
            APPLY "PAGE-UP":U TO BROWSE {&BROWSE-NAME}.
        END.
        WHEN "Down" THEN DO:
            APPLY "CURSOR-DOWN":U TO BROWSE {&BROWSE-NAME}.
        END.
        WHEN "PageDown" THEN DO:
            APPLY "PAGE-DOWN":U TO BROWSE {&BROWSE-NAME}.
        END.
        WHEN "SelectJob" THEN DO:
            IF AVAIL tt-job-list THEN DO:
              job# = FILL(' ',6 - LENGTH(tt-job-list.job-no)) + tt-job-list.job-no + "-" + STRING(tt-job-list.job-no2).             
              /* ============ check po whether on hold or release */
              IF AVAIL tt-job-list THEN
              FIND FIRST job NO-LOCK
                   WHERE job.company EQ company_code
                     AND job.job-no  EQ tt-job-list.job-no
                     AND job.job-no2 EQ tt-job-list.job-no2                               
                   NO-ERROR.
              IF AVAIL job AND job.stat EQ "H" THEN DO:
                 MESSAGE "JOB ON HOLD. DO NOT PROCEED!" VIEW-AS ALERT-BOX.
                 RELEASE job.
                 RETURN.
              END.
              DEF BUFFER bf-machtran FOR machtran.
              FIND FIRST bf-machtran NO-LOCK
                   WHERE bf-machtran.company     EQ company_code 
                     AND bf-machtran.machine     EQ machine_code 
                     AND (bf-machtran.job_number NE tt-job-list.job-no
                      OR bf-machtran.job_sub     NE tt-job-list.job-no2)
                     AND bf-machtran.end_date    EQ ?
                     AND bf-machtran.end_time    EQ 0 
                     AND bf-machtran.total_time  EQ 0
                   NO-ERROR.
              IF AVAIL bf-machtran THEN do:
                 MESSAGE "Job " + trim(bf-machtran.job_number) + "-" + TRIM(string(bf-machtran.job_sub,"99")) +
                         " has data collection transaction started. You must end that job's operation before selecting a new job."
                         VIEW-AS ALERT-BOX ERROR.
                 RELEASE job.
                 RETURN .
              END.
              /* ===================*/             
              {methods/run_link.i "CONTAINER" "Set_Value" "('job#',job#)"}
              {methods/run_link.i "CONTAINER" "Change_Page" "(10)"}
            END.
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

