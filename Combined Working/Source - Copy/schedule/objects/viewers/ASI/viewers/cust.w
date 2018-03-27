&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

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

{schedule/scopDir.i}
{{&includes}/defBoard.i}

DEFINE VARIABLE boardType AS CHARACTER NO-UNDO.
DEFINE VARIABLE charHandle AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE newStartDate AS DATE NO-UNDO.
DEFINE VARIABLE newStartTime AS INTEGER NO-UNDO.
DEFINE VARIABLE newEndDate AS DATE NO-UNDO.
DEFINE VARIABLE newEndTime AS INTEGER NO-UNDO.
DEFINE VARIABLE newEndDateSU AS DATE NO-UNDO.
DEFINE VARIABLE newEndTimeSU AS INTEGER NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES job job-mch cust
&Scoped-define FIRST-EXTERNAL-TABLE job


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR job, job-mch, cust.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSave 
&Scoped-Define DISPLAYED-FIELDS cust.cust-no cust.name ~
job-mch.start-date-su job-mch.start-date job-mch.dept job-mch.frm ~
cust.addr[1] job-mch.end-date-su job-mch.end-date job-mch.anchored ~
job-mch.blank-no cust.addr[2] job-mch.mr-waste job-mch.mr-hr job-mch.run-hr ~
job-mch.run-qty job-mch.speed job-mch.pass cust.city cust.state cust.zip ~
job.est-no job-mch.line 
&Scoped-define DISPLAYED-TABLES cust job-mch job
&Scoped-define FIRST-DISPLAYED-TABLE cust
&Scoped-define SECOND-DISPLAYED-TABLE job-mch
&Scoped-define THIRD-DISPLAYED-TABLE job
&Scoped-Define DISPLAYED-OBJECTS setupStart runStart setupEnd runEnd ~
lastShipDate mfgDate 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,updateFields,updateButtons,List-5,List-6 */
&Scoped-define updateFields job-mch.mr-hr job-mch.run-hr job-mch.run-qty ~
job-mch.speed 
&Scoped-define updateButtons btnSave btnReset btnCancel 

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
DEFINE BUTTON btnCancel 
     IMAGE-UP FILE "schedule/images/cancel.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.05 TOOLTIP "Cancel Update".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.05 TOOLTIP "Reset".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.05 TOOLTIP "Update".

DEFINE VARIABLE errorMsg AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP
     SIZE 178 BY 5 NO-UNDO.

DEFINE VARIABLE lastShipDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Last Ship" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE mfgDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Mfg" 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1
     BGCOLOR 9 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE runEnd AS CHARACTER FORMAT "X(256)":U 
     LABEL "@" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE runStart AS CHARACTER FORMAT "X(256)":U 
     LABEL "@" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE setupEnd AS CHARACTER FORMAT "X(256)":U 
     LABEL "@" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE setupStart AS CHARACTER FORMAT "X(256)":U 
     LABEL "@" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 18.6 BY 1.24
     BGCOLOR 0 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     errorMsg AT ROW 1 COL 1 NO-LABEL
     cust.cust-no AT ROW 1.24 COL 8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
          BGCOLOR 15 
     cust.name AT ROW 1.24 COL 30 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     job-mch.start-date-su AT ROW 1.24 COL 75 COLON-ALIGNED
          LABEL "Setup Start"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     setupStart AT ROW 1.24 COL 95 COLON-ALIGNED
     job-mch.start-date AT ROW 1.24 COL 120 COLON-ALIGNED
          LABEL "Run Start"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     runStart AT ROW 1.24 COL 141 COLON-ALIGNED
     job-mch.dept AT ROW 1.24 COL 170 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
          BGCOLOR 15 
     job-mch.frm AT ROW 2.43 COL 8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
          BGCOLOR 15 
     cust.addr[1] AT ROW 2.43 COL 30 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     job-mch.end-date-su AT ROW 2.43 COL 75 COLON-ALIGNED
          LABEL "Setup End"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     setupEnd AT ROW 2.43 COL 95 COLON-ALIGNED
     job-mch.end-date AT ROW 2.43 COL 120 COLON-ALIGNED
          LABEL "Run  End"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     runEnd AT ROW 2.43 COL 141 COLON-ALIGNED
     job-mch.anchored AT ROW 2.43 COL 170 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
          BGCOLOR 15 
     job-mch.blank-no AT ROW 3.62 COL 8 COLON-ALIGNED
          LABEL "Blank"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
          BGCOLOR 15 
     cust.addr[2] AT ROW 3.62 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     job-mch.mr-waste AT ROW 3.62 COL 75 COLON-ALIGNED
          LABEL "Waste"
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
          BGCOLOR 15 
     job-mch.mr-hr AT ROW 3.62 COL 95 COLON-ALIGNED
          LABEL "MR Hours"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 14 
     job-mch.run-hr AT ROW 3.62 COL 120 COLON-ALIGNED FORMAT ">>>9.99"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 14 
     job-mch.run-qty AT ROW 3.62 COL 141 COLON-ALIGNED
          LABEL "Quantity"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
          BGCOLOR 14 
     job-mch.speed AT ROW 3.62 COL 167 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 14 
     job-mch.pass AT ROW 4.81 COL 8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
          BGCOLOR 15 
     cust.city AT ROW 4.81 COL 30 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 
     cust.state AT ROW 4.81 COL 55.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
          BGCOLOR 15 
     cust.zip AT ROW 4.81 COL 63 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
          BGCOLOR 15 
     job.est-no AT ROW 4.81 COL 85 COLON-ALIGNED
          LABEL "Est#" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
          BGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     job-mch.line AT ROW 4.81 COL 101 COLON-ALIGNED
          LABEL "Ln#"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
          BGCOLOR 15 
     lastShipDate AT ROW 4.81 COL 120 COLON-ALIGNED
     mfgDate AT ROW 4.81 COL 141 COLON-ALIGNED
     btnSave AT ROW 4.81 COL 160 HELP
          "Update/Save"
     btnReset AT ROW 4.81 COL 166 HELP
          "Reset"
     btnCancel AT ROW 4.81 COL 172 HELP
          "Cancel Update"
     RECT-2 AT ROW 4.67 COL 159.4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.job,asi.job-mch,asi.cust
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
         HEIGHT             = 5.1
         WIDTH              = 178.4.
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

/* SETTINGS FOR FILL-IN cust.addr[1] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN cust.addr[2] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN job-mch.anchored IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job-mch.blank-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR BUTTON btnCancel IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON btnReset IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON btnSave IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN cust.city IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.cust-no IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job-mch.dept IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job-mch.end-date IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN job-mch.end-date-su IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR EDITOR errorMsg IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       errorMsg:HIDDEN IN FRAME F-Main           = TRUE
       errorMsg:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN job.est-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN job-mch.frm IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lastShipDate IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job-mch.line IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mfgDate IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job-mch.mr-hr IN FRAME F-Main
   NO-ENABLE 3 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN job-mch.mr-waste IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN cust.name IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN job-mch.pass IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job-mch.run-hr IN FRAME F-Main
   NO-ENABLE 3 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN job-mch.run-qty IN FRAME F-Main
   NO-ENABLE 3 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN runEnd IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN runStart IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN setupEnd IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN setupStart IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job-mch.speed IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN job-mch.start-date IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN job-mch.start-date-su IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN cust.state IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust.zip IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
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

&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel V-table-Win
ON CHOOSE OF btnCancel IN FRAME F-Main
DO:
  DISPLAY {&updateFields} WITH FRAME {&FRAME-NAME}.
  DISABLE {&updateFields} btnReset btnCancel WITH FRAME {&FRAME-NAME}.
  btnSave:TOOLTIP = 'Update'.
  APPLY 'CHOOSE':U TO btnReset.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset V-table-Win
ON CHOOSE OF btnReset IN FRAME F-Main
DO:
  ASSIGN
    newEndDateSU = job-mch.end-date-su
    newEndTimeSU = job-mch.end-time-su
    newStartDate = job-mch.start-date
    newStartTime = job-mch.start-time
    newEndDate = job-mch.end-date
    newEndTime = job-mch.end-time
    setupEnd:SCREEN-VALUE = STRING(job-mch.end-time-su,'hh:mm:ss am')
    runStart:SCREEN-VALUE = STRING(job-mch.start-time,'hh:mm:ss am')
    runEnd:SCREEN-VALUE = STRING(job-mch.end-time,'hh:mm:ss am').
  DISPLAY {&updateFields} job-mch.end-date-su job-mch.start-date job-mch.end-date
      WITH FRAME {&FRAME-NAME}.
  APPLY 'ENTRY' TO job-mch.mr-hr.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave V-table-Win
ON CHOOSE OF btnSave IN FRAME F-Main
DO:
  IF SELF:TOOLTIP EQ 'Update' THEN
  DO: /* update */
    ENABLE {&updateFields} btnReset btnCancel WITH FRAME {&FRAME-NAME}.
    ASSIGN
      SELF:TOOLTIP = 'Save'
      newEndDateSU = job-mch.end-date-su
      newEndTimeSU = job-mch.end-time-su
      newStartDate = job-mch.start-date
      newStartTime = job-mch.start-time
      newEndDate = job-mch.end-date
      newEndTime = job-mch.end-time.
    APPLY 'ENTRY' TO job-mch.mr-hr.
    RETURN NO-APPLY.
  END. /* if update */
  ELSE
  DO: /* save */
    DISABLE {&updateFields} btnReset btnCancel WITH FRAME {&FRAME-NAME}.
    FIND CURRENT job-mch EXCLUSIVE-LOCK.
    ASSIGN {&updateFields}
      SELF:TOOLTIP = 'Update'
      job-mch.end-date-su = newEndDateSU
      job-mch.end-time-su = newEndTimeSU
      job-mch.start-date = newStartDate
      job-mch.start-time = newStartTime
      job-mch.end-date = newEndDate
      job-mch.end-time = newEndTime.
    FIND CURRENT job-mch NO-LOCK.
    IF VALID-HANDLE(adm-broker-hdl) THEN
    DO:
      RUN get-link-handle IN adm-broker-hdl
          (THIS-PROCEDURE,'CONTAINER-SOURCE':U,OUTPUT charHandle).
      pHandle = WIDGET-HANDLE(charHandle).
      IF VALID-HANDLE(pHandle) THEN
      RUN valueChange IN pHandle (ROWID(job-mch)).
    END. /* if valid-handle */
  END. /* else save */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.mr-hr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.mr-hr V-table-Win
ON VALUE-CHANGED OF job-mch.mr-hr IN FRAME F-Main /* MR Hours */
DO:
  RUN changeDateTime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.run-hr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.run-hr V-table-Win
ON VALUE-CHANGED OF job-mch.run-hr IN FRAME F-Main /* Run Hours */
DO:
  RUN changeSpeed.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.run-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.run-qty V-table-Win
ON VALUE-CHANGED OF job-mch.run-qty IN FRAME F-Main /* Quantity */
DO:
  RUN changeRunHR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job-mch.speed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job-mch.speed V-table-Win
ON VALUE-CHANGED OF job-mch.speed IN FRAME F-Main /* Speed */
DO:
  RUN changeRunHR.
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

{{&includes}/{&Board}/calcEnd.i}

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
  {src/adm/template/row-list.i "job"}
  {src/adm/template/row-list.i "job-mch"}
  {src/adm/template/row-list.i "cust"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "job"}
  {src/adm/template/row-find.i "job-mch"}
  {src/adm/template/row-find.i "cust"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changeDateTime V-table-Win 
PROCEDURE changeDateTime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvMrHr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvRunHr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvSpeed AS INTEGER NO-UNDO.


  ASSIGN
    lvMrHr = DECIMAL(job-mch.mr-hr:SCREEN-VALUE IN FRAME {&FRAME-NAME})
    lvRunHr = DECIMAL(job-mch.run-hr:SCREEN-VALUE).
  RUN calcEnd (job-mch.start-date-su,job-mch.start-time-su,lvMrHr,0,OUTPUT lvEndDate,OUTPUT lvEndTime).
  ASSIGN
    job-mch.end-date-su:SCREEN-VALUE = STRING(lvEndDate)
    setupEnd:SCREEN-VALUE = STRING(lvEndTime,'HH:MM:SS am')
    newEndDateSU = lvEndDate
    newEndTimeSU = lvEndTime
    job-mch.start-date:SCREEN-VALUE = STRING(lvEndDate)
    runStart:SCREEN-VALUE = STRING(lvEndTime,'HH:MM:SS am')
    newStartDate = lvEndDate
    newStartTime = lvEndTime.
  RUN calcEnd (newStartDate,newStartTime,0,lvRunHr,OUTPUT lvEndDate,OUTPUT lvEndTime).
  ASSIGN
    job-mch.end-date:SCREEN-VALUE = STRING(lvEndDate)
    runEnd:SCREEN-VALUE = STRING(lvEndTime,'HH:MM:SS am')
    newEndDate = lvEndDate
    newEndTime = lvEndTime.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changeRunHR V-table-Win 
PROCEDURE changeRunHR :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvRunHr AS DECIMAL NO-UNDO.

  ASSIGN
    lvRunHr = DECIMAL(job-mch.run-qty:SCREEN-VALUE IN FRAME {&FRAME-NAME}) /
              INTEGER(job-mch.speed:SCREEN-VALUE)
    lvRunHr = IF lvRunHr EQ ? THEN 0 ELSE lvRunHr
    job-mch.run-hr:SCREEN-VALUE = STRING(lvRunHr).
  RUN calcEnd (job-mch.start-date,job-mch.end-time,0,lvRunHr,OUTPUT lvEndDate,OUTPUT lvEndTime).
  ASSIGN
    job-mch.end-date:SCREEN-VALUE = STRING(lvEndDate)
    runEnd:SCREEN-VALUE = STRING(lvEndTime,'HH:MM:SS am')
    newEndDate = lvEndDate
    newEndTime = lvEndTime.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changeSpeed V-table-Win 
PROCEDURE changeSpeed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvRunHr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvSpeed AS INTEGER NO-UNDO.

  ASSIGN
    lvSpeed = DECIMAL(job-mch.run-qty:SCREEN-VALUE IN FRAME {&FRAME-NAME}) /
              DECIMAL(job-mch.run-hr:SCREEN-VALUE)
    lvSpeed = IF lvSpeed EQ ? THEN 0 ELSE lvSpeed
    lvSpeed = lvSpeed + IF lvSpeed - INTEGER(lvSpeed) GT 0 THEN 1 ELSE 0
    job-mch.speed:SCREEN-VALUE = STRING(lvSpeed)
    lvRunHr = DECIMAL(job-mch.run-hr:SCREEN-VALUE).
  RUN calcEnd (job-mch.start-date,job-mch.start-time,0,lvRunHr,OUTPUT lvEndDate,OUTPUT lvEndTime).
  ASSIGN
    job-mch.end-date:SCREEN-VALUE = STRING(lvEndDate)
    runEnd:SCREEN-VALUE = STRING(lvEndTime,'HH:MM:SS am')
    newEndDate = lvEndDate
    newEndTime = lvEndTime.

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
  IF AVAILABLE job-mch THEN
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST mach NO-LOCK
         WHERE mach.company EQ job-mch.company
           AND mach.m-code EQ job-mch.m-code NO-ERROR.
    ASSIGN
      errorMsg:HIDDEN = YES
      setupStart:SCREEN-VALUE = STRING(job-mch.start-time-su,'hh:mm:ss am')
      setupEnd:SCREEN-VALUE = STRING(job-mch.end-time-su,'hh:mm:ss am')
      runStart:SCREEN-VALUE = STRING(job-mch.start-time,'hh:mm:ss am')
      runEnd:SCREEN-VALUE = STRING(job-mch.end-time,'hh:mm:ss am').
    FIND FIRST oe-ord NO-LOCK WHERE oe-ord.company EQ job-mch.company
                                AND oe-ord.job-no EQ job-mch.job-no
                                AND oe-ord.job-no2 EQ job-mch.job-no2 NO-ERROR.
    lastShipDate:SCREEN-VALUE = IF NOT AVAILABLE oe-ord THEN ''
                                ELSE STRING(oe-ord.last-date).
    FIND FIRST oe-ordl NO-LOCK
         WHERE oe-ordl.company EQ job-mch.company
           AND oe-ordl.job-no EQ job-mch.job-no
           AND oe-ordl.job-no2 EQ job-mch.job-no2 
           AND oe-ordl.i-no EQ job-mch.i-no NO-ERROR.
    mfgDate:SCREEN-VALUE = IF NOT AVAILABLE oe-ordl THEN ''
                           ELSE STRING(oe-ordl.prom-date).
  END.
  ELSE
  ASSIGN
    errorMsg:HIDDEN = NO
    errorMsg:SENSITIVE = YES
    errorMsg:SCREEN-VALUE = CHR(10) + '    Connection to Job Routing Record has' +
      ' been lost ... Job Standards ReBuild may have occured' + CHR(10) + CHR(10) +
      '    Reload the Schedule Board ...'.

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
  {src/adm/template/snd-list.i "job"}
  {src/adm/template/snd-list.i "job-mch"}
  {src/adm/template/snd-list.i "cust"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setUpdateButtons V-table-Win 
PROCEDURE setUpdateButtons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplSetUpdateButtons AS LOGICAL NO-UNDO.
  
  IF iplSetUpdateButtons THEN
  DISABLE {&updateButtons} WITH FRAME {&FRAME-NAME}.
  ELSE
  ENABLE btnSave WITH FRAME {&FRAME-NAME}.

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

