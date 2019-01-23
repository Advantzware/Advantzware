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
&Scoped-define EXTERNAL-TABLES ftpConfig
&Scoped-define FIRST-EXTERNAL-TABLE ftpConfig


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ftpConfig.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ftpConfig.ediType ftpConfig.ftpCode ~
ftpConfig.partner ftpConfig.ftpSite ftpConfig.ftpBinary ftpConfig.ftpUser ~
ftpConfig.ftpPassword ftpConfig.ftpSoftware ftpConfig.ftpCommand ~
ftpConfig.ftpDir ftpConfig.ftpScript ftpConfig.ftpDirection ~
ftpConfig.ftpDeleteFile ftpConfig.ftpMode 
&Scoped-define ENABLED-TABLES ftpConfig
&Scoped-define FIRST-ENABLED-TABLE ftpConfig
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS ftpConfig.ediType ftpConfig.ftpCode ~
ftpConfig.partner ftpConfig.ftpSite ftpConfig.ftpBinary ftpConfig.ftpUser ~
ftpConfig.ftpPassword ftpConfig.ftpSoftware ftpConfig.ftpCommand ~
ftpConfig.ftpDir ftpConfig.ftpScript ftpConfig.ftpDirection ~
ftpConfig.ftpDeleteFile ftpConfig.ftpMode 
&Scoped-define DISPLAYED-TABLES ftpConfig
&Scoped-define FIRST-DISPLAYED-TABLE ftpConfig


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
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 17.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     ftpConfig.ediType AT ROW 1.95 COL 33.2 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS COMBO-BOX INNER-LINES 2
          LIST-ITEMS "PoExport" 
          DROP-DOWN-LIST
          SIZE 16 BY 1
     ftpConfig.ftpCode AT ROW 2.91 COL 33.2 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ftpConfig.partner AT ROW 3.86 COL 33.2 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     ftpConfig.ftpSite AT ROW 4.81 COL 33.2 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     ftpConfig.ftpBinary AT ROW 4.81 COL 107.4 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS COMBO-BOX INNER-LINES 2
          LIST-ITEMS "ASC","BIN" 
          DROP-DOWN-LIST
          SIZE 16 BY 1 TOOLTIP "Select ASCII data or Binary"
     ftpConfig.ftpUser AT ROW 5.76 COL 33.2 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ftpConfig.ftpPassword AT ROW 5.81 COL 107.2 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ftpConfig.ftpSoftware AT ROW 6.71 COL 33 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "FTP","WinSCP" 
          DROP-DOWN-LIST
          SIZE 16 BY 1
     ftpConfig.ftpCommand AT ROW 6.76 COL 107.2 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     ftpConfig.ftpDir AT ROW 7.67 COL 33.2 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 54 BY 1
     ftpConfig.ftpScript AT ROW 7.71 COL 107.2 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     ftpConfig.ftpDirection AT ROW 8.62 COL 33.2 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "IN","OUT" 
          DROP-DOWN-LIST
          SIZE 16 BY 1
     ftpConfig.ftpDeleteFile AT ROW 8.71 COL 109.2 WIDGET-ID 30
          VIEW-AS TOGGLE-BOX
          SIZE 18.2 BY .81
     ftpConfig.ftpMode AT ROW 9.57 COL 33.2 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS COMBO-BOX INNER-LINES 3
          LIST-ITEMS "FTP","SFTP" 
          DROP-DOWN-LIST
          SIZE 16 BY 1
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.ftpConfig
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
         HEIGHT             = 17.14
         WIDTH              = 144.
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartViewerCues" V-table-Win _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartViewer,uib,49270
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
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
  {src/adm/template/row-list.i "ftpConfig"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ftpConfig"}

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
  {src/adm/template/snd-list.i "ftpConfig"}

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

