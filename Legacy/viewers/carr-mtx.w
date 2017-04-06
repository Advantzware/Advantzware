&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p on 03.28.2017 @ 10:44:20 am */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/carr-mtx.w

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

DEFINE VARIABLE op-company AS CHARACTER NO-UNDO.
DEFINE VARIABLE op-carrier AS CHARACTER NO-UNDO.
DEFINE VARIABLE op-loc AS CHARACTER NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES carr-mtx
&Scoped-define FIRST-EXTERNAL-TABLE carr-mtx


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR carr-mtx.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS carr-mtx.del-dscr carr-mtx.del-zip ~
carr-mtx.min-rate carr-mtx.weight[1] carr-mtx.rate[1] carr-mtx.weight[2] ~
carr-mtx.rate[2] carr-mtx.weight[3] carr-mtx.rate[3] carr-mtx.weight[4] ~
carr-mtx.rate[4] carr-mtx.weight[5] carr-mtx.rate[5] carr-mtx.weight[6] ~
carr-mtx.rate[6] carr-mtx.weight[7] carr-mtx.rate[7] carr-mtx.weight[8] ~
carr-mtx.rate[8] carr-mtx.weight[9] carr-mtx.rate[9] carr-mtx.weight[10] ~
carr-mtx.rate[10] 
&Scoped-define ENABLED-TABLES carr-mtx
&Scoped-define FIRST-ENABLED-TABLE carr-mtx
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS carr-mtx.del-zone carr-mtx.del-dscr ~
carr-mtx.del-zip carr-mtx.min-rate carr-mtx.weight[1] carr-mtx.rate[1] ~
carr-mtx.weight[2] carr-mtx.rate[2] carr-mtx.weight[3] carr-mtx.rate[3] ~
carr-mtx.weight[4] carr-mtx.rate[4] carr-mtx.weight[5] carr-mtx.rate[5] ~
carr-mtx.weight[6] carr-mtx.rate[6] carr-mtx.weight[7] carr-mtx.rate[7] ~
carr-mtx.weight[8] carr-mtx.rate[8] carr-mtx.weight[9] carr-mtx.rate[9] ~
carr-mtx.weight[10] carr-mtx.rate[10] 
&Scoped-define DISPLAYED-TABLES carr-mtx
&Scoped-define FIRST-DISPLAYED-TABLE carr-mtx
&Scoped-Define DISPLAYED-OBJECTS ls-wgt-title ls-rate-title 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS carr-mtx.del-zone 

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
DEFINE VARIABLE ls-rate-title AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 18.6 BY .62 NO-UNDO.

DEFINE VARIABLE ls-wgt-title AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 26.6 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 16.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     carr-mtx.del-zone AT ROW 1.24 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.del-dscr AT ROW 2.43 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.del-zip AT ROW 3.62 COL 14 COLON-ALIGNED FORMAT "X(10)"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.min-rate AT ROW 3.62 COL 40 COLON-ALIGNED FORMAT ">>>9.999"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.weight[1] AT ROW 5.48 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.rate[1] AT ROW 5.52 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.weight[2] AT ROW 6.71 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.rate[2] AT ROW 6.71 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.weight[3] AT ROW 7.91 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.rate[3] AT ROW 7.91 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.weight[4] AT ROW 9.1 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.rate[4] AT ROW 9.1 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.weight[5] AT ROW 10.29 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.rate[5] AT ROW 10.29 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.weight[6] AT ROW 11.48 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.rate[6] AT ROW 11.48 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.weight[7] AT ROW 12.67 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.rate[7] AT ROW 12.67 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.weight[8] AT ROW 13.86 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.rate[8] AT ROW 13.86 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.weight[9] AT ROW 15.05 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.rate[9] AT ROW 15.05 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     carr-mtx.weight[10] AT ROW 16.24 COL 14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     carr-mtx.rate[10] AT ROW 16.24 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 FONT 4
     ls-wgt-title AT ROW 4.81 COL 1 COLON-ALIGNED NO-LABEL
     ls-rate-title AT ROW 4.81 COL 33 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.carr-mtx
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
         HEIGHT             = 18.33
         WIDTH              = 62.6.
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

/* SETTINGS FOR FILL-IN carr-mtx.del-zip IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN carr-mtx.del-zone IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ls-rate-title IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ls-wgt-title IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN carr-mtx.min-rate IN FRAME F-Main
   EXP-FORMAT                                                           */
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
  {src/adm/template/row-list.i "carr-mtx"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "carr-mtx"}

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
  {methods/viewers/create/carr-mtx.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-chg-method LIKE carrier.chg-method no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */
  run get-link-handle in adm-broker-hdl (this-procedure, "record-source", output char-hdl).
  run chg-method in widget-handle(char-hdl) (output lv-chg-method).

  IF lv-chg-method EQ "P" THEN
    ASSIGN
     ls-rate-title = "Rate / Pallet"
     ls-wgt-title  = "Num Pallets - UP TO".
  ELSE
  IF lv-chg-method EQ "W" THEN
    ASSIGN
     ls-rate-title = "Rate / 100lbs."
     ls-wgt-title  = "Weight (lbs.) - UP TO".
  ELSE
    ASSIGN
     ls-rate-title = "Rate / MSF"
     ls-wgt-title  = "MSF - UP TO".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  display ls-wgt-title ls-rate-title with frame {&frame-name}.

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
  {src/adm/template/snd-list.i "carr-mtx"}

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

