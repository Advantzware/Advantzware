&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p on 04.18.2017 @ 11:37:40 am */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/prgrms.w

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

&Scoped-define ENHANCE no

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
&Scoped-define EXTERNAL-TABLES prgrms
&Scoped-define FIRST-EXTERNAL-TABLE prgrms


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR prgrms.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS prgrms.prgtitle prgrms.dir_group ~
prgrms.prgm_ver prgrms.mfgroup prgrms.menu_item prgrms.popup ~
prgrms.run_persistent prgrms.track_usage prgrms.can_run prgrms.can_create ~
prgrms.can_update prgrms.can_delete 
&Scoped-define ENABLED-TABLES prgrms
&Scoped-define FIRST-ENABLED-TABLE prgrms
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS prgrms.prgmname prgrms.prgtitle ~
prgrms.dir_group prgrms.prgm_ver prgrms.mfgroup prgrms.menu_item ~
prgrms.popup prgrms.run_persistent prgrms.track_usage prgrms.can_run ~
prgrms.can_create prgrms.can_update prgrms.can_delete 
&Scoped-define DISPLAYED-TABLES prgrms
&Scoped-define FIRST-DISPLAYED-TABLE prgrms
&Scoped-Define DISPLAYED-OBJECTS F1 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,List-4,List-5,F1   */
&Scoped-define ADM-CREATE-FIELDS prgrms.prgmname 
&Scoped-define F1 F1 

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
DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 100 BY 14.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     prgrms.prgmname AT ROW 1.24 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 FONT 4
     prgrms.prgtitle AT ROW 1.24 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
          BGCOLOR 15 FONT 4
     prgrms.dir_group AT ROW 2.43 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 FONT 4
     prgrms.prgm_ver AT ROW 2.43 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 FONT 4
     prgrms.mfgroup AT ROW 3.62 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 FONT 4
     prgrms.menu_item AT ROW 4.81 COL 17
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY 1
     prgrms.popup AT ROW 4.81 COL 36
          VIEW-AS TOGGLE-BOX
          SIZE 13.6 BY 1
     prgrms.run_persistent AT ROW 4.81 COL 50
          VIEW-AS TOGGLE-BOX
          SIZE 21.8 BY 1
     prgrms.track_usage AT ROW 4.81 COL 74
          VIEW-AS TOGGLE-BOX
          SIZE 19.8 BY 1
     prgrms.can_run AT ROW 6 COL 17 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 83 BY 2.38
          BGCOLOR 15 
     prgrms.can_create AT ROW 8.38 COL 17 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 83 BY 2.38
          BGCOLOR 15 
     prgrms.can_update AT ROW 10.76 COL 17 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 83 BY 2.38
          BGCOLOR 15 
     prgrms.can_delete AT ROW 13.14 COL 17 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 83 BY 2.38
          BGCOLOR 15 
     F1 AT ROW 2.43 COL 33 NO-LABEL
     "Can Delete:" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 13.29 COL 3
     "Can Run:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 6.14 COL 6
     "Can Create:" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 8.52 COL 3
     "Can Update:" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 10.91 COL 2
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: NOSWEAT.prgrms
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
         HEIGHT             = 14.76
         WIDTH              = 100.
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

/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN prgrms.prgmname IN FRAME F-Main
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

&Scoped-define SELF-NAME prgrms.dir_group
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prgrms.dir_group V-table-Win
ON HELP OF prgrms.dir_group IN FRAME F-Main /* Directory */
DO:
  DEFINE VARIABLE m-lookup-var AS CHARACTER NO-UNDO.
  RUN "lookups/dir_lkup.p" (INPUT-OUTPUT m-lookup-var).
  IF m-lookup-var NE "" THEN
  {&SELF-NAME}:SCREEN-VALUE = m-lookup-var.
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
  {src/adm/template/row-list.i "prgrms"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "prgrms"}

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
  {src/adm/template/snd-list.i "prgrms"}

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

