&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/notes.w

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
DEFINE SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.
DEFINE VARIABLE ip-rec_key AS CHARACTER NO-UNDO.
{custom/globdefs.i}

DEF VAR v-got-dept AS LOG NO-UNDO.
DEF VAR v-new-record AS LOG NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES notes
&Scoped-define FIRST-EXTERNAL-TABLE notes


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR notes.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS notes.note_code notes.note_form_no 
&Scoped-define ENABLED-TABLES notes
&Scoped-define FIRST-ENABLED-TABLE notes
&Scoped-Define ENABLED-OBJECTS RECT-1 btProgram 
&Scoped-Define DISPLAYED-FIELDS notes.viewed notes.note_code ~
notes.note_form_no notes.note_title notes.note_text notes.createDate ~
notes.createTime notes.createUser notes.updateDate notes.updateTime ~
notes.updateUser 
&Scoped-define DISPLAYED-TABLES notes
&Scoped-define FIRST-DISPLAYED-TABLE notes
&Scoped-Define DISPLAYED-OBJECTS dept-dscr 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,List-4,List-5,F1   */
&Scoped-define ADM-ASSIGN-FIELDS notes.note_title notes.note_text 

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
DEFINE BUTTON btn-dept 
     LABEL "Department List" 
     SIZE 23 BY 1.14.

DEFINE BUTTON btProgram 
     LABEL "Program" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE dept-dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 122 BY 14.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     notes.viewed AT ROW 1.24 COL 108
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY 1
     notes.note_code AT ROW 1.33 COL 14 COLON-ALIGNED
          LABEL "Dept"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     dept-dscr AT ROW 1.33 COL 21 COLON-ALIGNED NO-LABEL
     notes.note_form_no AT ROW 1.33 COL 82 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     btn-dept AT ROW 2.43 COL 98
     notes.note_title AT ROW 2.52 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
          BGCOLOR 15 FONT 4
     notes.note_text AT ROW 3.71 COL 16 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 106 BY 9.29
          BGCOLOR 15 
     notes.createDate AT ROW 13.05 COL 24 COLON-ALIGNED WIDGET-ID 2
          LABEL "Created Date"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     notes.createTime AT ROW 13.05 COL 56 COLON-ALIGNED WIDGET-ID 4
          LABEL "Time"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     notes.createUser AT ROW 13.05 COL 84 COLON-ALIGNED WIDGET-ID 6
          LABEL "User ID"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     btProgram AT ROW 13.67 COL 104 WIDGET-ID 14
     notes.updateDate AT ROW 13.95 COL 24 COLON-ALIGNED WIDGET-ID 8
          LABEL "Last Updated Date"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     notes.updateTime AT ROW 13.95 COL 56 COLON-ALIGNED WIDGET-ID 10
          LABEL "Time"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     notes.updateUser AT ROW 13.95 COL 84 COLON-ALIGNED WIDGET-ID 12
          LABEL "User ID"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: NOSWEAT.notes
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
         HEIGHT             = 14.19
         WIDTH              = 122.
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

/* SETTINGS FOR BUTTON btn-dept IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN notes.createDate IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN notes.createTime IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN notes.createUser IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN dept-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN notes.note_code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR EDITOR notes.note_text IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN notes.note_title IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN notes.updateDate IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN notes.updateTime IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN notes.updateUser IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX notes.viewed IN FRAME F-Main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME btn-dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-dept V-table-Win
ON CHOOSE OF btn-dept IN FRAME F-Main /* Department List */
DO:
  DEF VAR v-machine AS cha NO-UNDO.
  DEF VAR v-deptlst AS cha NO-UNDO.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,'container-source', OUTPUT char-hdl).
  RUN get-ip-machine IN WIDGET-HANDLE(char-hdl) (OUTPUT v-machine).
  RUN addon/windows/d-deptlk.w (v-machine,OUTPUT v-deptlst).

  IF v-deptlst <> "" THEN DO:
     FIND FIRST mach WHERE mach.company = g_company AND
                           mach.m-code = v-machine NO-LOCK NO-ERROR.
     ASSIGN notes.note_code:SCREEN-VALUE = IF AVAIL mach THEN mach.dept[1] ELSE ""
            notes.note_title:SCREEN-VALUE = ENTRY(1,v-deptlst) + "," + ENTRY(2,v-deptlst)
            notes.note_text:SCREEN-VALUE = notes.note_title:SCREEN-VALUE.

     FIND FIRST dept WHERE dept.code EQ notes.note_code:SCREEN-VALUE NO-LOCK NO-ERROR.
     dept-dscr:screen-value in frame {&frame-name} = if avail dept then dept.dscr else "".
     APPLY "leave" TO notes.note_code.

  END.
  v-got-dept = YES.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btProgram
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btProgram V-table-Win
ON CHOOSE OF btProgram IN FRAME F-Main /* Program */
DO:
    IF AVAILABLE notes THEN
    MESSAGE notes.updateProgram VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME notes.note_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_code V-table-Win
ON ENTRY OF notes.note_code IN FRAME F-Main /* Dept */
DO:
  ENABLE btn-dept WITH FRAME {&FRAME-NAME}.
/*  IF NOT v-got-dept AND v-new-record THEN       */
/*  IF PROGRAM-NAME(9) MATCHES "*add-record*" THEN*/
/*  APPLY "choose" TO  btn-dept.                  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_code V-table-Win
ON HELP OF notes.note_code IN FRAME F-Main /* Dept */
DO:
  RUN lookups/dept.p.
  IF g_lookup-var NE '' AND g_lookup-var NE SELF:SCREEN-VALUE THEN
  DO:
    FIND dept NO-LOCK WHERE dept.code EQ g_lookup-var NO-ERROR.
    ASSIGN
      SELF:SCREEN-VALUE = g_lookup-var
      dept-dscr:SCREEN-VALUE = dept.dscr
      notes.note_title:SCREEN-VALUE = IF AVAIL dept THEN dept.dscr ELSE ''.
  END.
  APPLY 'ENTRY' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_code V-table-Win
ON LEAVE OF notes.note_code IN FRAME F-Main /* Dept */
DO:

  IF LASTKEY NE -1 THEN DO:
    RUN valid-note_code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME notes.note_form_no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_form_no V-table-Win
ON HELP OF notes.note_form_no IN FRAME F-Main /* Form */
DO:
    def var ls-header as cha no-undo.
    def var char-hdl as cha no-undo.
    def var char-val as cha no-undo.

    run get-link-handle in adm-broker-hdl (this-procedure,"container-source", output char-hdl).
    run get-ip-header in widget-handle(char-hdl) (output ls-header).
    run windows/l-formno.w (g_company, ls-header, output char-val).
    if char-val <> "" then self:screen-value = char-val.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_form_no V-table-Win
ON LEAVE OF notes.note_form_no IN FRAME F-Main /* Form */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-note_form_no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

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
  {src/adm/template/row-list.i "notes"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "notes"}

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
  RUN local-display-fields.
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
  DISABLE btn-dept WITH FRAME {&FRAME-NAME}.
  v-got-dept = NO.
  v-new-record = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-form# AS INT NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/create/notes.i}

  notes.note_type = "D".
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
  RUN get-form-number IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-form#).
  notes.note_FORM_no = lv-form#.



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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  find first dept where dept.code eq notes.note_code no-lock no-error.
  dept-dscr:screen-value in frame {&frame-name} =
      if avail dept then dept.dscr else "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR local-new-rec AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-note_code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-note_form_no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  local-new-rec = adm-new-record.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF local-new-rec THEN DO:
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
     RUN reopen-query IN WIDGET-HANDLE(char-hdl) (ROWID(notes)).
  END.
  adm-new-record = NO.
  DISABLE btn-dept WITH FRAME {&FRAME-NAME}.
  v-got-dept = NO.
  v-new-record = NO.

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
  {src/adm/template/snd-list.i "notes"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-note_code V-table-Win 
PROCEDURE valid-note_code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF notes.note_code:SCREEN-VALUE NE "" AND
       NOT CAN-FIND(FIRST dept WHERE dept.code EQ notes.note_code:SCREEN-VALUE)
    THEN DO:
      MESSAGE "Invalid " + TRIM(notes.note_code:LABEL) + ", try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO notes.note_code.
      RETURN ERROR.
    END.
  END.


  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-note_form_no V-table-Win 
PROCEDURE valid-note_form_no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ls-header AS CHAR NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.


  {methods/lValidateError.i YES}
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source", OUTPUT char-hdl).
  RUN get-ip-header IN WIDGET-HANDLE(char-hdl) (OUTPUT ls-header).

  DO WITH FRAME {&FRAME-NAME}:
    IF INT(notes.note_form_no:SCREEN-VALUE) NE 0 AND
       NOT CAN-FIND(FIRST ef WHERE ef.company EQ g_company
                               AND ef.est-no  EQ ls-header
                               AND ef.form-no EQ INT(notes.note_form_no:SCREEN-VALUE))
    THEN DO:
      MESSAGE "Invalid " + TRIM(notes.note_form_no:LABEL) VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO notes.note_form_no.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

