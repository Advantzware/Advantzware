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

DEFINE VARIABLE saveNoteCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE ip-rec_key AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
{custom/globdefs.i}

DEF BUFFER bf-item-spec FOR item-spec.

DEF TEMP-TABLE tt-notes LIKE notes.

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
&Scoped-Define ENABLED-FIELDS notes.note_title notes.note_text 
&Scoped-define ENABLED-TABLES notes
&Scoped-define FIRST-ENABLED-TABLE notes
&Scoped-Define ENABLED-OBJECTS RECT-1 cbTitle btProgram 
&Scoped-Define DISPLAYED-FIELDS notes.note_code notes.viewed ~
notes.note_title notes.note_text notes.createTime notes.createUser ~
notes.createDate notes.updateTime notes.updateUser notes.updateDate 
&Scoped-define DISPLAYED-TABLES notes
&Scoped-define FIRST-DISPLAYED-TABLE notes
&Scoped-Define DISPLAYED-OBJECTS spec-desc cbTitle 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,List-4,List-5,F1   */

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
DEFINE BUTTON btProgram 
     LABEL "Program" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cbTitle AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "1","1"
     DROP-DOWN-LIST
     SIZE 74 BY 1 NO-UNDO.

DEFINE VARIABLE spec-desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 122 BY 14.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     notes.note_code AT ROW 1.24 COL 14 COLON-ALIGNED
          LABEL "Spec" FORMAT "X(3)"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     spec-desc AT ROW 1.24 COL 23 COLON-ALIGNED NO-LABEL
     notes.viewed AT ROW 1.24 COL 108
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY 1
     cbTitle AT ROW 2.43 COL 14 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     notes.note_title AT ROW 3.62 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
          FONT 4
     notes.note_text AT ROW 3.62 COL 16 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 106 BY 9.29
     notes.createTime AT ROW 12.91 COL 53 COLON-ALIGNED WIDGET-ID 6
          LABEL "Time"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     notes.createUser AT ROW 12.91 COL 85 COLON-ALIGNED WIDGET-ID 8
          LABEL "User ID"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     notes.createDate AT ROW 13 COL 23.2 COLON-ALIGNED WIDGET-ID 4
          LABEL "Created Date"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     btProgram AT ROW 13.38 COL 103 WIDGET-ID 16
     notes.updateTime AT ROW 13.86 COL 53 COLON-ALIGNED WIDGET-ID 12
          LABEL "Time"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     notes.updateUser AT ROW 13.86 COL 85 COLON-ALIGNED WIDGET-ID 14
          LABEL "User ID"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     notes.updateDate AT ROW 13.95 COL 23.2 COLON-ALIGNED WIDGET-ID 10
          LABEL "Last Updated Date"
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

/* SETTINGS FOR FILL-IN notes.createDate IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN notes.createTime IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN notes.createUser IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN notes.note_code IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
ASSIGN 
       notes.note_code:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN spec-desc IN FRAME F-Main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME btProgram
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btProgram V-table-Win
ON CHOOSE OF btProgram IN FRAME F-Main /* Program */
DO:
    IF AVAILABLE notes THEN
    MESSAGE notes.updateProgram VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbTitle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbTitle V-table-Win
ON VALUE-CHANGED OF cbTitle IN FRAME F-Main
DO:
  ASSIGN cbTitle.
  FIND FIRST rejct-cd WHERE rejct-cd.CODE = cbTitle NO-LOCK NO-ERROR.
  IF AVAIL(rejct-cd) AND notes.note_text:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN
     notes.note_text:SCREEN-VALUE IN FRAME {&FRAME-NAME} = rejct-cd.dscr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME notes.note_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_code V-table-Win
ON ENTRY OF notes.note_code IN FRAME F-Main /* Spec */
DO:
  saveNoteCode = {&self-name}:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_code V-table-Win
ON HELP OF notes.note_code IN FRAME F-Main /* Spec */
DO:
  DEF VAR char-val AS CHAR NO-UNDO.


  RUN cec/l-itspec.w (g_company, {&self-name}:SCREEN-VALUE, OUTPUT char-val).
  IF char-val NE "" AND {&self-name}:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
    {&self-name}:SCREEN-VALUE = ENTRY(1,char-val).
    RUN new-note_code.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_code V-table-Win
ON LEAVE OF notes.note_code IN FRAME F-Main /* Spec */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-note_code (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_code V-table-Win
ON VALUE-CHANGED OF notes.note_code IN FRAME F-Main /* Spec */
DO:
  RUN new-note_code.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME notes.note_title
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_title V-table-Win
ON HELP OF notes.note_title IN FRAME F-Main /* Note Title */
DO:
    RUN windows/l-rejpo.w  (g_company,FOCUS:SCREEN-VALUE, OUTPUT char-val). 
    IF char-val <> "" THEN 
      FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = ENTRY(1,char-val).
    IF notes.note_text:SCREEN-VALUE  IN FRAME {&FRAME-NAME} EQ "" THEN
       notes.note_text:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = ENTRY(2,char-val).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asi-initialize V-table-Win 
PROCEDURE asi-initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
cbTitle:DELETE("1") IN FRAME {&FRAME-NAME}.
FOR EACH rejct-cd 
    WHERE rejct-cd.TYPE    EQ "R"
    NO-LOCK WITH FRAME {&FRAME-NAME}.
    cbTitle:ADD-LAST(rejct-cd.CODE + " " + rejct-cd.dscr, rejct-cd.CODE).        
END.
notes.note_code:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "RDC".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-title V-table-Win 
PROCEDURE get-title :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* DEF INPUT PARAMETER note-row AS ROWID NO-UNDO.*/
  DEF BUFFER bfNotes FOR notes.

  IF AVAIL(notes) THEN
  FIND bfNotes WHERE ROWID(bfNotes) = ROWID(notes) NO-LOCK NO-ERROR.

  IF AVAIL bfNotes THEN DO WITH FRAME {&FRAME-NAME}:     
      FIND rejct-cd WHERE rejct-cd.CODE = bfNotes.note_title NO-LOCK NO-ERROR.

      IF AVAIL rejct-cd THEN
      cbTitle:SCREEN-VALUE = bfNotes.note_title.
  END.
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
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
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

  notes.note_type = "R".

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
  RUN new-note_code.
  ASSIGN NOSWEAT.notes.note_code:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "RDC" 
         spec-desc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Release Date Change" .

  IF AVAIL notes THEN DO:     
      FIND rejct-cd WHERE rejct-cd.CODE = notes.note_title NO-LOCK NO-ERROR.
      IF AVAIL rejct-cd THEN
      cbTitle:SCREEN-VALUE = notes.note_title.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN asi-initialize.
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


  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-note_code (notes.note_code:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-note_code V-table-Win 
PROCEDURE new-note_code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

    FIND FIRST item-spec NO-LOCK
        WHERE item-spec.company EQ g_company
          AND item-spec.i-no    EQ ""
          AND item-spec.code    EQ notes.note_code:SCREEN-VALUE
        NO-ERROR.
    IF AVAIL item-spec THEN DO:
      spec-desc:SCREEN-VALUE = item-spec.note[1].
      IF notes.note_code:SCREEN-VALUE NE saveNoteCode THEN
        ASSIGN
         /* notes.note_title:SCREEN-VALUE = spec-desc:SCREEN-VALUE */
         saveNoteCode                  = notes.note_code:SCREEN-VALUE.
    END.
  END.

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
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST item-spec
                    WHERE item-spec.company EQ g_company
                      AND item-spec.i-no    EQ ""
                      AND item-spec.code    EQ ip-focus:SCREEN-VALUE) THEN DO:
       MESSAGE "Invalid RM/FG Specfication, try help..."
           VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO ip-focus.
       RETURN ERROR.
     END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

