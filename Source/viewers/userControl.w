&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/<table>.w

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
&SCOPED-DEFINE proc-enable proc-enable

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

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
&Scoped-define EXTERNAL-TABLES userControl
&Scoped-define FIRST-EXTERNAL-TABLE userControl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR userControl.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS userControl.maxAllowedUsers ~
userControl.numUsersOverLimit userControl.maxSessionsPerUser ~
userControl.inactivityLockout userControl.autoLogoutTime ~
userControl.adminEmailAddr userControl.nofityLocked ~
userControl.notifyAutoLogout userControl.notifyDisable 
&Scoped-define ENABLED-TABLES userControl
&Scoped-define FIRST-ENABLED-TABLE userControl
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 RECT-18 
&Scoped-Define DISPLAYED-FIELDS userControl.maxAllowedUsers ~
userControl.numUsersOverLimit userControl.maxSessionsPerUser ~
userControl.inactivityLockout userControl.autoLogoutTime ~
userControl.adminEmailAddr userControl.nofityLocked ~
userControl.notifyAutoLogout userControl.notifyDisable 
&Scoped-define DISPLAYED-TABLES userControl
&Scoped-define FIRST-DISPLAYED-TABLE userControl


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */

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
DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 108 BY 2.38.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 108 BY 4.52.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 108 BY 6.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     userControl.maxAllowedUsers AT ROW 2.43 COL 47 COLON-ALIGNED WIDGET-ID 8
          LABEL "Number Concurrent Users"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     userControl.numUsersOverLimit AT ROW 3.62 COL 47 COLON-ALIGNED WIDGET-ID 12
          LABEL "Concurrent Users Allowed Over Limit"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     userControl.maxSessionsPerUser AT ROW 4.81 COL 47 COLON-ALIGNED WIDGET-ID 14
          LABEL "Max Sessions Per User"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     userControl.inactivityLockout AT ROW 7.19 COL 47 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     userControl.autoLogoutTime AT ROW 10.05 COL 31 COLON-ALIGNED WIDGET-ID 20
          LABEL "Max Session Time (hrs)"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     userControl.adminEmailAddr AT ROW 11.1 COL 31 COLON-ALIGNED WIDGET-ID 18
          LABEL "Notification Email List"
          VIEW-AS FILL-IN 
          SIZE 78 BY 1
     userControl.nofityLocked AT ROW 12.43 COL 33
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .81
     userControl.notifyAutoLogout AT ROW 13.38 COL 33
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .81
     userControl.notifyDisable AT ROW 14.33 COL 33
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .81
     "Notification Types:" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 12.52 COL 10 WIDGET-ID 34
     " Auto Logout Parameters" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 8.86 COL 6 WIDGET-ID 26
     " License Counts" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 1.48 COL 6 WIDGET-ID 24
     RECT-2 AT ROW 1.71 COL 4 WIDGET-ID 16
     RECT-3 AT ROW 9.1 COL 4 WIDGET-ID 22
     RECT-18 AT ROW 6.48 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 117.2 BY 14.91
         FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.userControl
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
         HEIGHT             = 14.91
         WIDTH              = 117.2.
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
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN userControl.adminEmailAddr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN userControl.autoLogoutTime IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN userControl.maxAllowedUsers IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN userControl.maxSessionsPerUser IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN userControl.numUsersOverLimit IN FRAME F-Main
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

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
  {src/adm/template/row-list.i "userControl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "userControl"}

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
  

  /* Code placed here will execute PRIOR to standard behavior. */
 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 
  /*assign
   userControl.notifyAutoLogout   = logical(tgAutoLogout:SCREEN-VALUE IN FRAME {&FRAME-NAME}) .*/
   
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  /*IF AVAIL userControl AND NOT adm-new-record THEN DO:
   ASSIGN 
       tgAutoLogout = userControl.notifyAutoLogout 
       tgAutoLogout:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(userControl.notifyAutoLogout) .
  END.*/

    /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-enable V-table-Win 
PROCEDURE proc-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR hPgmSecurity AS HANDLE NO-UNDO.
    DEF VAR lResult AS LOG NO-UNDO.
    RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
    RUN epCanAccess IN hPgmSecurity ("viewers/userControl.w", "Access1", OUTPUT lResult).

    IF lResult THEN ASSIGN 
        userControl.maxAllowedUsers:SENSITIVE IN FRAME {&FRAME-NAME} = YES 
        userControl.numUsersOverLimit:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
    ELSE ASSIGN 
        userControl.maxAllowedUsers:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
        userControl.numUsersOverLimit:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

    RUN epCanAccess IN hPgmSecurity ("viewers/userControl.w", "Access2", OUTPUT lResult).
    DELETE OBJECT hPgmSecurity.
  
    IF lResult THEN ASSIGN 
        userControl.maxSessionsPerUser:SENSITIVE IN FRAME {&FRAME-NAME} = YES  .
    ELSE ASSIGN
        userControl.maxSessionsPerUser:SENSITIVE IN FRAME {&FRAME-NAME} = NO  .
    
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
  {src/adm/template/snd-list.i "userControl"}

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

