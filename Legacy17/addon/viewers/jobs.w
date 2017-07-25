&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          jobs             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/jobs.w

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

DEFINE VARIABLE phandle AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES jobs
&Scoped-define FIRST-EXTERNAL-TABLE jobs


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR jobs.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS jobs.job jobs.cadcam_status jobs.customer ~
jobs.scheduling_status jobs.name jobs.ship_name jobs.address1 ~
jobs.ship_address1 jobs.address2 jobs.ship_address2 jobs.city ~
jobs.ship_city jobs.state jobs.zip jobs.ship_state jobs.ship_zip ~
jobs.salesrep jobs.run_number jobs.estimate jobs.overrun jobs.underrun 
&Scoped-define ENABLED-TABLES jobs
&Scoped-define FIRST-ENABLED-TABLE jobs
&Scoped-define DISPLAYED-TABLES jobs
&Scoped-define FIRST-DISPLAYED-TABLE jobs
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS jobs.job jobs.cadcam_status jobs.customer ~
jobs.scheduling_status jobs.name jobs.estimate_type jobs.ship_name ~
jobs.address1 jobs.ship_address1 jobs.address2 jobs.ship_address2 jobs.city ~
jobs.ship_city jobs.state jobs.zip jobs.ship_state jobs.ship_zip ~
jobs.salesrep jobs.run_number jobs.estimate jobs.overrun jobs.underrun 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define List-6 Btn_Clear_CADCAM_Status Btn_Clear_Scheduling_Status 

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
DEFINE BUTTON Btn_Clear_CADCAM_Status 
     LABEL "Remove from CADCAM" 
     SIZE 24 BY 1.

DEFINE BUTTON Btn_Clear_Scheduling_Status 
     LABEL "Remove from Sched" 
     SIZE 24 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 115 BY 9.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     jobs.job AT ROW 1.24 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     Btn_Clear_CADCAM_Status AT ROW 1.24 COL 51 HELP
          "Set CADCAM Status to N/A"
     jobs.cadcam_status AT ROW 1.24 COL 99 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
          BGCOLOR 15 
     jobs.customer AT ROW 2.43 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     Btn_Clear_Scheduling_Status AT ROW 2.43 COL 51 HELP
          "Set Scheduling Status to N/A"
     jobs.scheduling_status AT ROW 2.43 COL 99 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
          BGCOLOR 15 
     jobs.name AT ROW 3.62 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     jobs.estimate_type AT ROW 3.62 COL 51 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Single", 1,
"Two Piece", 2,
"Tandem", 3,
"Combination", 4,
"Corr Single", 5,
"Corr Set", 6
          SIZE 16 BY 5.71
          BGCOLOR 15 
     jobs.ship_name AT ROW 3.62 COL 81 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     jobs.address1 AT ROW 4.81 COL 16 COLON-ALIGNED
          LABEL "Address1"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     jobs.ship_address1 AT ROW 4.81 COL 81 COLON-ALIGNED
          LABEL "Address1"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     jobs.address2 AT ROW 6 COL 16 COLON-ALIGNED
          LABEL "Address2"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     jobs.ship_address2 AT ROW 6 COL 81 COLON-ALIGNED
          LABEL "Address2"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     jobs.city AT ROW 7.19 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     jobs.ship_city AT ROW 7.19 COL 81 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     jobs.state AT ROW 8.38 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
          BGCOLOR 15 
     jobs.zip AT ROW 8.38 COL 32.4 COLON-ALIGNED
          LABEL "Zip"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
          BGCOLOR 15 
     jobs.ship_state AT ROW 8.38 COL 81 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
          BGCOLOR 15 
     jobs.ship_zip AT ROW 8.38 COL 97.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
          BGCOLOR 15 
     jobs.salesrep AT ROW 9.57 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
          BGCOLOR 15 
     jobs.run_number AT ROW 9.57 COL 42 COLON-ALIGNED
          LABEL "Run #"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
          BGCOLOR 15 
     jobs.estimate AT ROW 9.57 COL 59 COLON-ALIGNED
          LABEL "Est#"
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
          BGCOLOR 15 
     jobs.overrun AT ROW 9.57 COL 81 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     jobs.underrun AT ROW 9.57 COL 102.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: jobs.jobs
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
         HEIGHT             = 9.76
         WIDTH              = 115.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

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

/* SETTINGS FOR FILL-IN jobs.address1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN jobs.address2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR BUTTON Btn_Clear_CADCAM_Status IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       Btn_Clear_CADCAM_Status:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON Btn_Clear_Scheduling_Status IN FRAME F-Main
   NO-ENABLE 6                                                          */
ASSIGN 
       Btn_Clear_Scheduling_Status:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN jobs.estimate IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR RADIO-SET jobs.estimate_type IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN jobs.run_number IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN jobs.ship_address1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN jobs.ship_address2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN jobs.zip IN FRAME F-Main
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





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_Clear_CADCAM_Status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Clear_CADCAM_Status V-table-Win
ON CHOOSE OF Btn_Clear_CADCAM_Status IN FRAME F-Main /* Remove from CADCAM */
DO:
  IF jobs.cadcam_status = 'Pending' THEN
  DO:
    FIND CURRENT jobs EXCLUSIVE-LOCK.
    jobs.cadcam_status = ''.
    FIND CURRENT jobs NO-LOCK.
    RUN Re-Display-Fields.
    DISPLAY jobs.cadcam_status WITH FRAME {&FRAME-NAME}.
    DISABLE Btn_Clear_CADCAM_Status WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Clear_Scheduling_Status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Clear_Scheduling_Status V-table-Win
ON CHOOSE OF Btn_Clear_Scheduling_Status IN FRAME F-Main /* Remove from Sched */
DO:
  IF jobs.scheduling_status = 'Pending' THEN
  DO:
    FIND CURRENT jobs EXCLUSIVE-LOCK.
    jobs.scheduling_status = ''.
    FIND CURRENT jobs NO-LOCK.
    RUN Re-Display-Fields.
    DISPLAY jobs.scheduling_status WITH FRAME {&FRAME-NAME}.
    DISABLE Btn_Clear_Scheduling_Status WITH FRAME {&FRAME-NAME}.
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
  {src/adm/template/row-list.i "jobs"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "jobs"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAILABLE jobs THEN
  DO:
  /*IF jobs.cadcam_status = 'Pending' THEN
    ENABLE Btn_Clear_CADCAM_Status WITH FRAME {&FRAME-NAME}.
    ELSE
    DISABLE Btn_Clear_CADCAM_Status WITH FRAME {&FRAME-NAME}.
    IF jobs.Scheduling_status = 'Pending' THEN
    ENABLE Btn_Clear_Scheduling_Status WITH FRAME {&FRAME-NAME}.
    ELSE
    DISABLE Btn_Clear_Scheduling_Status WITH FRAME {&FRAME-NAME}.*/
  END.
  ELSE
    DISABLE {&LIST-6} WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Re-Display-Fields V-table-Win 
PROCEDURE Re-Display-Fields :
/*------------------------------------------------------------------------------
  Purpose:     Re-Display Fields in Record-Source Broswer
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(adm-broker-hdl) THEN
  DO:
    RUN get-link-handle IN adm-broker-hdl
        (THIS-PROCEDURE,'RECORD-SOURCE':U,OUTPUT char-hdl).
    phandle = WIDGET-HANDLE(char-hdl).
    IF VALID-HANDLE(phandle) THEN
    RUN adm-display-fields IN phandle.
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
  {src/adm/template/snd-list.i "jobs"}

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

