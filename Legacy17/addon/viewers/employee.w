&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/employee.w

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

DEFINE VARIABLE copy-employee_company AS CHARACTER NO-UNDO.
DEFINE VARIABLE copy-employee_employee AS CHARACTER NO-UNDO.
def var is-password-changed as log no-undo.
{custom/gcompany.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES employee
&Scoped-define FIRST-EXTERNAL-TABLE employee


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR employee.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS employee.first_name employee.middle_name ~
employee.last_name employee.soc_sec employee.rate_usage employee.emp_type ~
employee.keyboard_type employee.start_date employee.ref_no employee.actnum ~
employee.dock-time employee.lunch_paid 
&Scoped-define ENABLED-TABLES employee
&Scoped-define FIRST-ENABLED-TABLE employee
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS employee.employee employee.first_name ~
employee.middle_name employee.last_name employee.soc_sec ~
employee.rate_usage employee.emp_type employee.keyboard_type ~
employee.start_date employee.ref_no employee.actnum employee.dock-time ~
employee.lunch_paid 
&Scoped-define DISPLAYED-TABLES employee
&Scoped-define FIRST-DISPLAYED-TABLE employee
&Scoped-Define DISPLAYED-OBJECTS emp_type_description pass-word F1 F-2 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS employee.employee 
&Scoped-define DISPLAY-FIELD employee.emp_type 
&Scoped-define F1 F1 F-2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE emp_type_description AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE pass-word AS CHARACTER FORMAT "X(12)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE verify-passwd AS CHARACTER FORMAT "X(12)":U 
     LABEL "Verify Password" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 105 BY 12.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     employee.employee AT ROW 1.24 COL 23 COLON-ALIGNED
          LABEL "Employee ID"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     employee.first_name AT ROW 2.67 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     employee.middle_name AT ROW 2.67 COL 54 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     employee.last_name AT ROW 2.67 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 15 FONT 4
     employee.soc_sec AT ROW 4.1 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 4
     employee.rate_usage AT ROW 4.1 COL 77 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Shift", yes,
"Machine", no
          SIZE 24 BY 1
          FONT 4
     employee.emp_type AT ROW 5.52 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     emp_type_description AT ROW 5.52 COL 33 COLON-ALIGNED NO-LABEL
     employee.keyboard_type AT ROW 5.52 COL 81 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Mini Alphabetic", "sortpad.":U,
"Full Alphabetic", "alphabet.":U,
"QWERTY Format", "keyboard.":U
          SIZE 24 BY 3.57
          FONT 4
     employee.start_date AT ROW 6.95 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     employee.ref_no AT ROW 8.14 COL 23.2 COLON-ALIGNED
          LABEL " Ref# / Manager ID"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
          BGCOLOR 15 FONT 4
     employee.actnum AT ROW 9.33 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     employee.dock-time AT ROW 9.33 COL 83 COLON-ALIGNED
          LABEL "Dock Time(Minute)"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 
     pass-word AT ROW 10.76 COL 23 COLON-ALIGNED HELP
          "Enter Employee Password" BLANK 
     employee.lunch_paid AT ROW 10.76 COL 62
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .81
     verify-passwd AT ROW 11.95 COL 23 COLON-ALIGNED HELP
          "Re-enter Password for Verification"
     F1 AT ROW 5.52 COL 33 NO-LABEL
     F-2 AT ROW 6.95 COL 39 NO-LABEL
     "Rate Usage:" VIEW-AS TEXT
          SIZE 14.6 BY .81 AT ROW 4.1 COL 62
     "Keyboard Type:" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 5.76 COL 62
     "(leave blank to auto assign employee id)" VIEW-AS TEXT
          SIZE 56 BY .62 AT ROW 1.48 COL 34
          FONT 2
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: employee
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 12.67
         WIDTH              = 105.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN employee.dock-time IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN employee.employee IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN employee.emp_type IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN emp_type_description IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN pass-word IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN employee.ref_no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN verify-passwd IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       verify-passwd:HIDDEN IN FRAME F-Main           = TRUE.

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

&Scoped-define SELF-NAME employee.emp_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL employee.emp_type V-table-Win
ON LEAVE OF employee.emp_type IN FRAME F-Main /* Employee Type */
DO:
  {&methods/lValidateError.i YES}
  {methods/dispflds.i}
  {methods/entryerr.i
      &can-find="emp_type WHERE emp_type.emp_type = SELF:SCREEN-VALUE"
      &error-message="Invalid Employee Type"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pass-word
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pass-word V-table-Win
ON ENTRY OF pass-word IN FRAME F-Main /* Password */
DO:
  ASSIGN
   /* SELF:SCREEN-VALUE = employee.passwd  not to concatenate new value to current value */
    verify-passwd = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pass-word V-table-Win
ON LEAVE OF pass-word IN FRAME F-Main /* Password */
DO:
  IF SELF:MODIFIED THEN
  DO:
    {&methods/lValidateError.i YES}
    is-password-changed = yes.
    ASSIGN
      {&SELF-NAME}
      SELF:MODIFIED = NO.
    ENABLE verify-passwd WITH FRAME {&FRAME-NAME}.
    APPLY 'ENTRY' TO verify-passwd.
    RETURN NO-APPLY.
  {&methods/lValidateError.i NO}
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pass-word V-table-Win
ON RETURN OF pass-word IN FRAME F-Main /* Password */
DO:
  APPLY 'LEAVE' TO SELF.
  APPLY 'ENTRY' TO verify-passwd.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME employee.start_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL employee.start_date V-table-Win
ON HELP OF employee.start_date IN FRAME F-Main /* Start Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME verify-passwd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL verify-passwd V-table-Win
ON ENTRY OF verify-passwd IN FRAME F-Main /* Verify Password */
DO:
  ASSIGN verify-passwd:SCREEN-VALUE = employee.passwd.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL verify-passwd V-table-Win
ON LEAVE OF verify-passwd IN FRAME F-Main /* Verify Password */
DO:
  {&methods/lValidateError.i YES}
  ASSIGN {&SELF-NAME}.
  HIDE {&SELF-NAME}.
  IF {&SELF-NAME} = pass-word THEN
  RETURN.
  MESSAGE 'PASSWORD VERIFICATION FAILED - PLEASE TRY AGAIN' VIEW-AS ALERT-BOX.
  APPLY 'ENTRY' TO pass-word.
  RETURN NO-APPLY.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  {custom/getcmpny.i}

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "employee"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "employee"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-empmach-old FOR empmach.
DEF BUFFER b-empmach-new FOR empmach.
DEF VAR v-old-employee LIKE employee.employee.
  /* Code placed here will execute PRIOR to standard behavior. */
v-old-employee = employee.employee.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/assign/employee.i}

     IF adm-new-record AND NOT adm-adding-record THEN DO:
        FOR EACH b-empmach-old WHERE b-empmach-old.company = gcompany
                             AND b-empmach-old.employee = v-old-employee NO-LOCK:
           CREATE b-empmach-new.
            BUFFER-COPY b-empmach-old EXCEPT b-empmach-old.rec_key b-empmach-old.employee
            TO b-empmach-new ASSIGN b-empmach-new.employee = employee.employee NO-ERROR.  
        END.

     END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  verify-passwd = ''.
  DISABLE pass-word WITH FRAME {&FRAME-NAME}.
  HIDE verify-passwd IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/create/employee.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {&methods/lValidateError.i YES}
  /*=== validate password ===*/
  if is-password-changed then do with frame {&frame-name}:  
     is-password-changed = no.
     ASSIGN verify-passwd .
     HIDE verify-passwd.
     IF verify-passwd <> pass-word then do:
        MESSAGE 'PASSWORD VERIFICATION FAILED - PLEASE TRY AGAIN' VIEW-AS ALERT-BOX.
        APPLY 'ENTRY' TO pass-word.
        RETURN NO-APPLY.
     end.
  end.
  {&methods/lValidateError.i NO} 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "employee"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

