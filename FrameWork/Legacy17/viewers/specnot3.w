&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

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

DEFINE VARIABLE ip-rec_key AS CHARACTER NO-UNDO.
{custom/globdefs.i}
&scoped-define spec-note spec-note

DEF TEMP-TABLE tt-notes LIKE notes.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES notes
&Scoped-define FIRST-EXTERNAL-TABLE notes


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR notes.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS notes.note_type notes.note_title ~
notes.note_text 
&Scoped-define ENABLED-TABLES notes
&Scoped-define FIRST-ENABLED-TABLE notes
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS notes.note_date notes.note_time ~
notes.user_id notes.viewed notes.note_type notes.note_group notes.note_code ~
notes.note_title notes.note_text 
&Scoped-define DISPLAYED-TABLES notes
&Scoped-define FIRST-DISPLAYED-TABLE notes


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,List-4,List-5,F1   */
&Scoped-define ADM-ASSIGN-FIELDS notes.note_group notes.note_code 

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
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 122 BY 14.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     notes.note_date AT ROW 1.24 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     notes.note_time AT ROW 1.24 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     notes.user_id AT ROW 1.24 COL 71 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     notes.viewed AT ROW 1.24 COL 108
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY 1
     notes.note_type AT ROW 2.43 COL 17 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Customer", "C":U,
"Group", "G":U,
"Department", "D":U
          SIZE 48 BY .95
     notes.note_group AT ROW 3.62 COL 14 COLON-ALIGNED FORMAT "X(20)"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     notes.note_code AT ROW 3.62 COL 56 COLON-ALIGNED
          LABEL "Dept"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     notes.note_title AT ROW 4.81 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
          FONT 4
     notes.note_text AT ROW 6 COL 16 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 106 BY 9.29
     RECT-1 AT ROW 1 COL 1
     "Note Type:" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 2.67 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.notes
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
         HEIGHT             = 15
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN notes.note_code IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN notes.note_date IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN notes.note_group IN FRAME F-Main
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN notes.note_time IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN notes.user_id IN FRAME F-Main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME notes.note_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_code V-table-Win
ON LEAVE OF notes.note_code IN FRAME F-Main /* Dept */
DO:
     {&methods/lValidateError.i YES}
     if lastkey <> -1 and self:screen-value <> "" and
      not can-find(dept where dept.code = self:screen-value)
      then do:
         message "Invalid Deptartment Code. Try Help." view-as alert-box error.
         return no-apply.
      end.
      {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME notes.note_group
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_group V-table-Win
ON HELP OF notes.note_group IN FRAME F-Main /* Group */
DO:
    def var char-val as cha no-undo.
    run windows/l-grp.w (g_company,output char-val).
    if char-val <> "" then self:screen-value = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_group V-table-Win
ON LEAVE OF notes.note_group IN FRAME F-Main /* Group */
DO:
    {&methods/lValidateError.i YES}
    if lastkey <> -1 and notes.note_group:screen-value <> "" and
       not can-find(first usergrps where usergrps.usergrps = self:screen-value)
    then do:
         message "Invalid Group. Try Help."  view-as alert-box error.
         return no-apply.
    end.
     {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME notes.note_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_type V-table-Win
ON VALUE-CHANGED OF notes.note_type IN FRAME F-Main /* Type */
DO:
    case notes.note_type:screen-value :
         when "C" then do:
            disable notes.note_group notes.note_code with frame {&frame-name}.
            assign  notes.note_code:screen-value = ""
                    notes.note_group:screen-value = "".
         end.   
         when "G" then do:
             enable notes.note_group with frame {&frame-name}.
             disable notes.note_code with frame {&frame-name}.
             notes.note_code:screen-value = "".
         end.    
         when "D" then do:
           enable notes.note_code with frame {&frame-name}.
           disable notes.note_group with frame {&frame-name}.
           notes.note_group:screen-value = "".
         end.  
    end.
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
  if notes.note_type = "C" then assign notes.note_group = ""
                                       notes.note_code = "".
  else if notes.note_type = "G" then notes.note_code = "".
  else if notes.note_type = "D" then notes.note_group = "".

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
   disable notes.note_code notes.note_group with frame {&frame-name}.

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

  {methods/viewers/create/notes.i}
  notes.note_source = "CUST".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG NO-UNDO.

 {&methods/lValidateError.i YES}
  /* Code placed here will execute PRIOR to standard behavior. */
  if notes.note_code:screen-value in frame {&frame-name} <> "" and
     not can-find(dept where dept.code = notes.note_code:screen-value)
  then do:
         message "Invalid Deptartment Code. Try Help." view-as alert-box error.
         apply "entry" to notes.note_code.
         return no-apply.
  end.
  if  notes.note_group:screen-value <> "" and
     /* not can-find(first permg where permg.usr-grp = notes.note_group:screen-value) */
     not can-find(first usergrps where usergrps.usergrps = notes.note_group:screen-value)
    then do:
         message "Invalid Group. Try Help." view-as alert-box error.
         apply "entry" to notes.note_group.
         return no-apply.
    end.
  {&methods/lValidateError.i NO}
  FOR EACH tt-notes:
    DELETE tt-notes.
  END.

  CREATE tt-notes.
  BUFFER-COPY notes TO tt-notes.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  BUFFER-COMPARE notes TO tt-notes SAVE RESULT IN ll.
  IF NOT ll THEN RUN custom/notewtrg.p (ROWID(notes)).

  disable notes.note_code notes.note_group with frame {&frame-name}.
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

