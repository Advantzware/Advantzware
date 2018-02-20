&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: jc\v-job.w

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
DEFINE NEW SHARED VARIABLE nufile AS LOG NO-UNDO.   /* for jc-calc.p */
DEFINE NEW SHARED VARIABLE lv-qty AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE fil_id AS RECID NO-UNDO.
{custom/globdefs.i}
{sys/inc/var.i "new shared"}
ASSIGN cocode = g_company
       locode = g_loc.

DEFINE VARIABLE ll-valid AS LOG NO-UNDO.
DEFINE VARIABLE ll-secure AS LOG NO-UNDO.
DEFINE VARIABLE ll-sch-updated AS LOG NO-UNDO.
DEFINE VARIABLE lv-new-job-hdr-rowid AS ROWID NO-UNDO.
DEFINE VARIABLE copyJob AS LOG NO-UNDO.
DEFINE VARIABLE copyJobRowID AS ROWID NO-UNDO.
/* rstark 06241201 */
DEFINE VARIABLE noDate AS LOG NO-UNDO.
DEFINE VARIABLE planDate AS LOG NO-UNDO.
DEFINE VARIABLE scheduleHndl AS HANDLE NO-UNDO.
/* rstark 06241201 */

/* gdm - 05290901 */
DEFINE VARIABLE v-jbmch-stdate LIKE job.start-date NO-UNDO.
DEFINE VARIABLE v-okflg        AS LOG              NO-UNDO.
DEFINE VARIABLE v-stdflg       AS LOG              NO-UNDO.
DEFINE VARIABLE v-rowid        AS ROWID            NO-UNDO.
DEFINE VARIABLE gvlUseJobQty AS LOG NO-UNDO.
DEFINE VARIABLE K_FRAC AS DECIMAL INIT 6.25 NO-UNDO.
DEFINE VARIABLE v-corr AS LOG NO-UNDO.
DEFINE VARIABLE ll-one-part AS LOG NO-UNDO.
DEFINE            BUFFER x-eb   FOR eb.
DEFINE NEW SHARED BUFFER xeb    FOR eb.
DEFINE            BUFFER bf-job FOR job.
/* gdm - 05290901 end */

/* gdm - 05290901*/
DEFINE VARIABLE v-startdate LIKE job.start-date NO-UNDO.
DEFINE BUFFER bf-job-mch FOR job-mch.
DEFINE VARIABLE OEJobHold-log AS LOG NO-UNDO.
DEFINE VARIABLE lcReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE llRecFound AS LOG NO-UNDO.
RUN sys/ref/nk1look.p (cocode, "OEJobHold", "L", NO, NO, "", "", 
                          OUTPUT lcReturn, OUTPUT llRecFound).

IF llRecFound THEN
   OEJobHold-log = LOGICAL(lcReturn) NO-ERROR.  

DEFINE VARIABLE JobHoldReason-log AS LOG NO-UNDO.
RUN sys/ref/nk1look.p (cocode, "JOBHoldReason", "L", NO, NO, "", "", 
                          OUTPUT lcReturn, OUTPUT llRecFound).
IF llRecFound THEN
   JobHoldReason-log = LOGICAL(lcReturn) NO-ERROR.

DEFINE BUFFER xjob FOR job.

noDate = CAN-FIND(FIRST sys-ctrl
                  WHERE sys-ctrl.company EQ cocode
                    AND sys-ctrl.name EQ 'Schedule'
                    AND sys-ctrl.char-fld EQ 'NoDate'
                    AND sys-ctrl.log-fld EQ YES).
/* rstark 06241201 */
planDate = CAN-FIND(FIRST sys-ctrl
                    WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name EQ 'Schedule'
                      AND sys-ctrl.char-fld EQ 'PlanDate'
                      AND sys-ctrl.log-fld EQ YES).
/* rstark 06241201 */
DEFINE VARIABLE lvReturnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvFound AS LOG NO-UNDO.
DEFINE VARIABLE autofgissue-log AS LOGICAL NO-UNDO.

RUN sys/ref/nk1look.p (cocode, "AUTOFGISSUE", "L", NO, NO, "", "", 
    OUTPUT lvReturnChar, OUTPUT lvFound).

{sys/inc/f16to32.i}
{sys/ref/CustList.i NEW}
DO TRANSACTION:
  {sys/inc/jobpass.i}
  {sys/inc/jobdatesmax.i}
  {sys/inc/unappju2.i}
   {sys/inc/custlistform.i ""JU1"" }
   {sys/inc/graphic.i}
END.

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
&Scoped-define EXTERNAL-TABLES job
&Scoped-define FIRST-EXTERNAL-TABLE job


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR job.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS job.start-date job.loc job.due-date 
&Scoped-define ENABLED-TABLES job
&Scoped-define FIRST-ENABLED-TABLE job
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS job.job-no job.job-no2 job.est-no job.stat ~
job.start-date job.close-date job.user-id job.loc job.due-date 
&Scoped-define DISPLAYED-TABLES job
&Scoped-define FIRST-DISPLAYED-TABLE job


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS job.job-no job.job-no2 job.est-no ~
job.start-date job.close-date job.due-date 
&Scoped-define DISPLAY-FIELD job.start-date job.close-date job.due-date 

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
DEFINE VARIABLE vHoldReason AS CHARACTER FORMAT "x(50)" 
     LABEL "Hold Reason" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 4.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     job.job-no AT ROW 1.24 COL 19 COLON-ALIGNED
          LABEL "Job Number" FORMAT "x(6)"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     job.job-no2 AT ROW 1.24 COL 37 COLON-ALIGNED NO-LABELS
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     job.est-no AT ROW 1.24 COL 65 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     job.stat AT ROW 1.24 COL 101 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     job.start-date AT ROW 1.24 COL 127 COLON-ALIGNED
          LABEL "Start"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     vHoldReason AT ROW 2.43 COL 79 COLON-ALIGNED WIDGET-ID 4
     job.close-date AT ROW 2.43 COL 127 COLON-ALIGNED
          LABEL "Close"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     job.user-id AT ROW 3.62 COL 67 COLON-ALIGNED
          LABEL "User ID"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     job.loc AT ROW 3.62 COL 101 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     job.due-date AT ROW 3.62 COL 127 COLON-ALIGNED
          LABEL "Due Date"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.job
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
         WIDTH              = 148.6.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN job.close-date IN FRAME F-Main
   NO-ENABLE 2 4 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN job.due-date IN FRAME F-Main
   2 4 EXP-LABEL                                                        */
/* SETTINGS FOR FILL-IN job.est-no IN FRAME F-Main
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN job.job-no IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN job.job-no2 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN job.start-date IN FRAME F-Main
   2 4 EXP-LABEL                                                        */
/* SETTINGS FOR FILL-IN job.stat IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job.user-id IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN vHoldReason IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vHoldReason:HIDDEN IN FRAME F-Main           = TRUE.

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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
  DEFINE VARIABLE char-val AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE lw-focus AS WIDGET-HANDLE NO-UNDO.


  lw-focus = FOCUS.

  CASE lw-focus:NAME:
      WHEN "est-no" THEN DO:
        RUN windows/l-est.w (g_company,g_loc,lw-focus:SCREEN-VALUE, OUTPUT char-val).
        IF char-val NE "" THEN DO:
          FIND eb WHERE RECID(eb) EQ INT(char-val) NO-LOCK NO-ERROR.
          IF AVAILABLE eb THEN lw-focus:SCREEN-VALUE = eb.est-no.
        END.                
      END. 
      WHEN "loc" THEN DO:
        RUN windows/l-loc.w (cocode, lw-focus:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN 
           ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
      END.
      WHEN "reason" THEN DO:
            RUN windows/l-rejjob.w (cocode, lw-focus:SCREEN-VALUE, OUTPUT char-val).
            /* If value selected, set code to first entry of string,
               set tooltip to second entry of string (description). */
            IF char-val <> "" THEN
                ASSIGN lw-focus:SCREEN-VALUE = TRIM(char-val)
                       .
        END.
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job.due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job.due-date V-table-Win
ON LEAVE OF job.due-date IN FRAME F-Main /* Due Date */
DO:
  /* per task #11280506
  IF LASTKEY NE -1 THEN DO:
    RUN valid-due-date NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job.est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job.est-no V-table-Win
ON LEAVE OF job.est-no IN FRAME F-Main /* Estimate # */
DO:
  IF LASTKEY NE -1 THEN DO:

    RUN valid-cust-user NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN validate-est (""). 
    IF NOT ll-valid THEN RETURN NO-APPLY.


  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job.job-no V-table-Win
ON LEAVE OF job.job-no IN FRAME F-Main /* Job Number */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no.
    IF NOT ll-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job.job-no2 V-table-Win
ON LEAVE OF job.job-no2 IN FRAME F-Main /* Run # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no2.
    IF NOT ll-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job.loc V-table-Win
ON LEAVE OF job.loc IN FRAME F-Main /* Warehouse */
DO:
    IF LASTKEY NE -1 THEN DO:
    RUN valid-whse. 
    IF NOT ll-valid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME job.start-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL job.start-date V-table-Win
ON LEAVE OF job.start-date IN FRAME F-Main /* Start */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN validate-start-date.
    IF NOT ll-valid THEN RETURN NO-APPLY.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-job V-table-Win 
PROCEDURE add-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch ('add-record').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "job"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-start-date V-table-Win 
PROCEDURE assign-start-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST bf-job EXCLUSIVE-LOCK
    WHERE bf-job.company EQ cocode
      AND bf-job.job     EQ job.job
      AND bf-job.job-no  EQ job.job-no
      AND bf-job.job-no2 EQ job.job-no2 NO-ERROR.
IF AVAILABLE bf-job THEN DO:
   ASSIGN bf-job.start-date = v-jbmch-stdate. 
END.
RELEASE bf-job.
RUN updateOrderDate.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-reopen V-table-Win 
PROCEDURE close-reopen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ll-process AS LOG NO-UNDO.


  ll-process = job.opened OR ll-secure OR NOT v-jobpass.

  IF NOT ll-process THEN DO:
    RUN sys/ref/d-passwd.w (5, OUTPUT ll-secure).
    ll-process = ll-secure.
  END.

  IF ll-process THEN DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).

    RUN browse-rowid IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-rowid).

    RUN jc/d-clsjc.w (ROWID(job)).

    /*RUN repo-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).*/
    RUN dispatch IN WIDGET-HANDLE(char-hdl) ("row-changed") .
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-due-date V-table-Win 
PROCEDURE get-due-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  DO WITH FRAME {&frame-name}:
    FIND job-hdr WHERE ROWID(job-hdr) EQ ip-rowid NO-LOCK NO-ERROR.

    IF NOT AVAILABLE job-hdr AND AVAILABLE job THEN
    FIND FIRST job-hdr
        WHERE job-hdr.company EQ job.company
          AND job-hdr.job     EQ job.job
          AND job-hdr.job-no  EQ job.job-no
          AND job-hdr.job-no2 EQ job.job-no2
        NO-LOCK NO-ERROR.

    IF AVAILABLE job-hdr AND job-hdr.ord-no NE 0 THEN
      FIND FIRST oe-ordl
          WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no  EQ job-hdr.ord-no
            AND oe-ordl.i-no    EQ job-hdr.i-no
          NO-LOCK NO-ERROR.
    ELSE
    IF ip-rowid EQ ? THEN
      FIND FIRST oe-ordl
          WHERE oe-ordl.company EQ cocode
            AND oe-ordl.job-no  EQ job.job-no:SCREEN-VALUE
            AND oe-ordl.job-no2 EQ INT(job.job-no2:SCREEN-VALUE)
            AND (NOT AVAILABLE job-hdr OR
                 (AVAILABLE job-hdr AND oe-ordl.i-no EQ job-hdr.i-no))
          NO-LOCK NO-ERROR.

    IF AVAILABLE oe-ordl THEN job.due-date:SCREEN-VALUE = STRING(oe-ordl.req-date).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-start-date V-table-Win 
PROCEDURE get-start-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ip-flg AS LOG NO-UNDO.

DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-assign-start-date AS LOG NO-UNDO.

ASSIGN v-jbmch-stdate = ?.

FOR EACH job-mch 
  WHERE job-mch.company EQ job.company
    AND job-mch.job     EQ job.job
    AND job-mch.job-no  EQ job.job-no
    AND job-mch.job-no2 EQ job.job-no2
    USE-INDEX line-idx NO-LOCK:

    IF job-mch.start-date-su NE ? 
      THEN ASSIGN v-jbmch-stdate = job-mch.start-date-su.

    IF v-jbmch-stdate NE ? THEN LEAVE.
END.

IF ip-flg THEN DO:    

   ASSIGN v-okflg = NO
          v-rowid = ?.

   RUN get-link-handle IN adm-broker-hdl
       (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
   RUN browse-rowid IN WIDGET-HANDLE(char-hdl) (OUTPUT v-rowid).

   IF job.start-date EQ ? AND
      job.start-date:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" AND
      v-jbmch-stdate NE ? AND 
      noDate EQ FALSE THEN
      DO:
         RUN assign-start-date.
         v-assign-start-date = YES.
      END.

  IF (job.start-date NE v-jbmch-stdate) AND
     (job.start-date:SCREEN-VALUE NE STRING(v-jbmch-stdate,"99/99/99")) AND
      v-jbmch-stdate NE ? THEN
      MESSAGE 
       "Machine Routing Start Date is " STRING(v-jbmch-stdate,"99/99/99") " ." SKIP
                   "Job Start Date is " STRING(job.start-date,"99/99/99") " ." SKIP
       "  Transfer Routing Date to Start Date ?" 
      VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO
      UPDATE v-okflg.

  IF v-okflg THEN
  DO:
     RUN assign-start-date.
     v-assign-start-date = YES.
  END.

  IF v-assign-start-date THEN
  DO:
     RUN get-link-handle IN adm-broker-hdl
        (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
     IF v-rowid NE ? THEN 
        RUN repo-query IN WIDGET-HANDLE(char-hdl) (v-rowid).
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getUseJobQty V-table-Win 
PROCEDURE getUseJobQty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oplUseJobQty AS LOGICAL     NO-UNDO.
oplUseJobQty = gvlUseJobQty.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hold-release V-table-Win 
PROCEDURE hold-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.  
  DEFINE VARIABLE lOrderOnHold AS LOGICAL NO-UNDO.  

    IF AVAILABLE job AND job.opened THEN 
    DO:
        FIND CURRENT job NO-ERROR.

        lOrderOnHold = NO.
        IF oeJobHold-log AND job.stat EQ "H" THEN 
        DO:
            FOR EACH job-hdr WHERE job-hdr.company EQ job.company
                AND job-hdr.job EQ job.job
                NO-LOCK:
                IF job-hdr.ord-no GT 0 THEN 
                DO:
                    FIND FIRST oe-ord 
                        WHERE oe-ord.company EQ job-hdr.company
                        AND oe-ord.ord-no EQ job-hdr.ord-no
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE oe-ord AND oe-ord.stat EQ "H" THEN 
                    DO:
                        lOrderOnHold= TRUE.
                        MESSAGE "Order " oe-ord.ord-no "is on hold and must be released before this job can be released."
                            VIEW-AS ALERT-BOX.
                    END. /* sales order is on hold */
                END. /* has a sales order defined */
            END. /* each job-hdr */
        END. /* Job on hold */

        IF NOT lOrderOnHold THEN 
        DO:
            job.stat = IF job.stat EQ "H" THEN
                IF CAN-FIND(FIRST mat-act
                WHERE mat-act.company EQ job.company
                AND mat-act.job     EQ job.job
                AND mat-act.job-no  EQ job.job-no
                AND mat-act.job-no2 EQ job.job-no2)  OR
                CAN-FIND(FIRST mch-act
                WHERE mch-act.company EQ job.company
                AND mch-act.job     EQ job.job
                AND mch-act.job-no  EQ job.job-no
                AND mch-act.job-no2 EQ job.job-no2)  OR
                CAN-FIND(FIRST misc-act
                WHERE misc-act.company EQ job.company
                AND misc-act.job     EQ job.job
                AND misc-act.job-no  EQ job.job-no
                AND misc-act.job-no2 EQ job.job-no2) THEN
                IF CAN-FIND(FIRST job-mat
                WHERE job-mat.company EQ job.company
                AND job-mat.job     EQ job.job
                AND job-mat.job-no  EQ job.job-no
                AND job-mat.job-no2 EQ job.job-no2
                AND job-mat.qty-all GT 0
                AND job-mat.all-flg)               THEN "A"
                ELSE "W"
                ELSE "R"
                ELSE "H".
            IF JobHoldReason-log AND job.stat = "H" THEN DO:                
                RUN windows/l-rejjob.w (cocode, job.reason, OUTPUT char-hdl).
                job.reason = ENTRY(1,char-hdl).                
            END.
            IF job.stat <> "H" THEN job.reason = "".

            FIND CURRENT job NO-LOCK NO-ERROR.

            RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
            RUN dispatch IN WIDGET-HANDLE(char-hdl) ("row-changed") .
        END. /* Not lOrderOnHold */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE is-row-available V-table-Win 
PROCEDURE is-row-available :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-row-avail AS LOG NO-UNDO.

  op-row-avail = IF AVAILABLE job THEN YES ELSE NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-start-date AS DATE NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  lv-start-date = IF AVAILABLE job THEN job.start-date ELSE ?.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /* task# 12070507 per joe */
  FOR EACH job-hdr
      WHERE job-hdr.company EQ job.company
        AND job-hdr.job     EQ job.job
        AND job-hdr.job-no  EQ job.job-no
        AND job-hdr.job-no2 EQ job.job-no2:
    ASSIGN job-hdr.due-date = job.due-date
           job-hdr.loc      = job.loc.
  END. /* each job-hdr */

  IF job.start-date NE lv-start-date AND NOT adm-new-record THEN DO:
    RUN update-schedule.    
    RUN updateOrderDate.
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
  copyJob = NO.
  DO WITH FRAME {&frame-name}:
    DISABLE ALL.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record V-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  MESSAGE 'Copy all Related Job Data?' VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE copyJob.
  IF NOT copyJob THEN RETURN.
  copyJobRowID = ROWID(job).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF noDate THEN
  ASSIGN
    job.start-date:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
    job.due-date:SCREEN-VALUE = ''.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE li LIKE job.job.

  /* Code placed here will execute PRIOR to standard behavior. */

  li = 1.
  FIND LAST job WHERE job.company EQ cocode USE-INDEX job NO-LOCK NO-ERROR.
  FIND LAST job-hdr WHERE job-hdr.company EQ cocode
      USE-INDEX job NO-LOCK NO-ERROR.
  IF job-hdr.job GT job.job THEN li = job-hdr.job + 1.
  IF job.job GE job-hdr.job THEN li = job.job + 1.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN
   job.job        = li
   job.company    = cocode
   job.loc        = locode
   /*job.start-date = TODAY v-startdate */
   job.stat       = "P".


  IF copyJob THEN
  ASSIGN
    job.start-date = DATE(job.start-date:SCREEN-VALUE IN FRAME {&FRAME-NAME})
    job.due-date = DATE(job.due-date:SCREEN-VALUE).

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE ALL.
    ENABLE job.est-no.
  END.

  RUN dispatch ('row-changed').


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-msg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE char-hdl AS cha NO-UNDO.
  DEFINE VARIABLE ll-warn AS LOG NO-UNDO.

  {&methods/lValidateError.i YES}
  /* Code placed here will execute PRIOR to standard behavior. */
  /*IF NOT adm-new-record THEN DO:*/
    ASSIGN
     lv-msg = ""
     ll-warn = NO.

    IF job.stat EQ "A" THEN
      ASSIGN
       lv-msg  = "This job has material allocated, de-allocate materials"
       ll-warn = YES.

    ELSE
    IF job.stat EQ "W" THEN
      lv-msg = "This job has work-in-process".

    ELSE
    IF INDEX("CXZ", job.stat) GT 0 THEN
      lv-msg = "This job has been closed".

    ELSE DO:
      FIND FIRST oe-ordl
          WHERE oe-ordl.company EQ job.company
            AND oe-ordl.job-no  EQ job.job-no
            AND oe-ordl.job-no2 EQ job.job-no2
          NO-LOCK NO-ERROR.
      IF AVAILABLE oe-ordl THEN lv-msg = "An order exists with this Job#".
    END.

    IF lv-msg NE "" THEN DO:
      IF ll-warn THEN DO:
        ll-warn = NO.
        MESSAGE lv-msg + " and delete anyway?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll-warn.
      END.

      ELSE
       IF job.job-no NE "" THEN                     /*Task# 12161304*/
        MESSAGE lv-msg + ", cannot delete..."
            VIEW-AS ALERT-BOX ERROR.

      IF NOT ll-warn THEN RETURN ERROR.
    END.

    IF lv-msg EQ "" THEN DO:
        FIND FIRST po-ordl NO-LOCK
             WHERE po-ordl.company EQ job.company
               AND po-ordl.job-no  EQ job.job-no
               AND po-ordl.job-no2 EQ job.job-no2
               AND po-ordl.opened NO-ERROR .
        IF AVAILABLE po-ordl THEN DO:
            MESSAGE "There is an open Purchase Order for this job. This PO" SKIP
                    "must be closed or deleted before the job can be deleted."
                VIEW-AS ALERT-BOX INFO .
            RETURN ERROR .
        END.
    END.

    IF lv-msg EQ "" THEN DO:
        {custom/askdel.i}
     END.

  /*END.*/

  /* set attrib rec-deleted for reopen-query */
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"Record-source",OUTPUT char-hdl).
  RUN set-attribute-list IN WIDGET-HANDLE(char-hdl) ("REC-DELETED=yes").


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  RUN get-link-handle IN adm-broker-hdl
                      (THIS-PROCEDURE, "record-target", OUTPUT char-hdl).

  RUN dispatch IN WIDGET-HANDLE(ENTRY(1,char-hdl)) ('open-query'). 
  /* Code placed here will execute AFTER standard behavior.    */
  {&methods/lValidateError.i NO}
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       gdm - 05290901 
               THIS WILL CHECK THE JOB START DATE VERSUS 
               MACHINE SETUP START DATE. GETTING THE MACHINE SETUP START DATE.

------------------------------------------------------------------------------*/
DEFINE VARIABLE li AS INTEGER NO-UNDO.
DEFINE VARIABLE char-hdl AS cha NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */  

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAILABLE job AND job.stat = "H" THEN DO: 
     FIND FIRST rejct-cd WHERE rejct-cd.type = "JH" 
                          AND rejct-cd.code = job.reason NO-LOCK NO-ERROR.
     IF AVAILABLE rejct-cd THEN
        ASSIGN vHoldReason:hidden IN FRAME {&frame-name} = NO
               vHoldReason:screen-value  = job.reason + " " + rejct-cd.dscr.      
  END.
  ELSE ASSIGN vHoldReason:hidden IN FRAME {&frame-name} = YES
              vHoldReason:screen-value  = "".

  RUN get-start-date (NO). 

/*   DISP job.start-date WITH FRAME {&FRAME-NAME}. */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* do it when scheduling is ready
     IF NOT adm-new-record THEN DISABLE job.start-date WITH FRAME {&FRAME-NAME}.
  */  

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE job.due-date.

    IF adm-new-record                 OR
       job.due-date LT job.start-date THEN ENABLE job.due-date.

    ELSE
    FOR EACH job-hdr
        WHERE job-hdr.company EQ job.company
          AND job-hdr.job     EQ job.job
          AND job-hdr.job-no  EQ job.job-no
          AND job-hdr.job-no2 EQ job.job-no2
          AND job-hdr.ord-no  EQ 0
        NO-LOCK
        BREAK BY job-hdr.job:
      IF FIRST(job-hdr.job) AND LAST(job-hdr.job) THEN LEAVE.
    END.

    IF AVAILABLE job-hdr THEN ENABLE job.due-date.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bf-job FOR job.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

   IF AVAILABLE job AND job.est-no EQ "" THEN DO:

      FIND FIRST job-hdr
            WHERE job-hdr.company EQ job.company
              AND job-hdr.job     EQ job.job
              AND job-hdr.job-no  EQ job.job-no
              AND job-hdr.job-no2 EQ job.job-no2
              AND job-hdr.est-no GT ""
            NO-LOCK NO-ERROR.
      IF AVAIL(job-hdr) AND AVAIL(job) AND job.est-no EQ "" AND job-hdr.est-no GT "" THEN DO:
          FIND bf-job WHERE ROWID(bf-job) EQ ROWID(job) EXCLUSIVE-LOCK.
          bf-job.est-no = job-hdr.est-no.
          RUN refresh-browser.
          RUN dispatch ('display-fields').
      END.
   END. 
   IF lv-new-job-hdr-rowid <> ? THEN DO:
       FIND FIRST job-hdr WHERE ROWID(job-hdr) = lv-new-job-hdr-rowid NO-LOCK NO-ERROR.
       IF AVAILABLE job-hdr THEN FIND FIRST job OF job-hdr NO-LOCK NO-ERROR.       
       RUN dispatch ('display-fields').
       lv-new-job-hdr-rowid = ?.
   END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ll-new AS LOG NO-UNDO.
  DEFINE VARIABLE char-hdl AS cha NO-UNDO.
  DEFINE VARIABLE lv-job-rowid AS ROWID NO-UNDO.
  DEFINE VARIABLE jobHdrRowID AS ROWID NO-UNDO.
  DEFINE VARIABLE li AS INTEGER NO-UNDO.
  DEFINE VARIABLE v-reprint AS LOG NO-UNDO.
  DEFINE BUFFER xeb FOR eb .
  DEFINE VARIABLE choice AS LOG NO-UNDO.
  DEFINE VARIABLE rEbRow AS ROWID NO-UNDO.
  DEFINE VARIABLE cNewItem AS CHARACTER NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN validate-start-date.
  IF NOT ll-valid THEN RETURN NO-APPLY.

  /* per task #11280506
  RUN valid-due-date NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */

  RUN valid-cust-user NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN validate-est ("Update").
  IF NOT ll-valid THEN RETURN NO-APPLY.

  RUN valid-job-no.
  IF NOT ll-valid THEN RETURN NO-APPLY.

  RUN valid-job-no2.
  IF NOT ll-valid THEN RETURN NO-APPLY.

  RUN valid-whse.
  IF NOT ll-valid THEN RETURN NO-APPLY.

  RUN valid-job-est .
  IF NOT ll-valid THEN RETURN NO-APPLY.

  ASSIGN
     ll-new = adm-new-record
     lv-new-job-hdr-rowid = ?.


  /* Deal with blank item number here so that can undo the add */
  /* begin insert */
  {&methods/lValidateError.i YES}
  DEFINE BUFFER xest FOR est.
  IF adm-new-record THEN DO:
    FIND FIRST xest WHERE xest.company EQ job.company
       AND xest.est-no = job.est-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      NO-LOCK NO-ERROR.

    FOR EACH xeb WHERE xeb.company = xest.company
                   AND xeb.est-no = xest.est-no
                 NO-LOCK
        BREAK BY xeb.est-no
              BY xeb.form-no
              BY xeb.blank-no:

      /* Code from jc/jc-calc.p */
      IF xeb.form-no   EQ 0
         AND (xeb.est-type NE xest.est-type OR
           (xest.est-type NE 2 AND xest.est-type NE 6)) THEN
        NEXT.

      FIND FIRST itemfg WHERE itemfg.company EQ cocode
                          AND itemfg.i-no    EQ xeb.stock-no
                          NO-LOCK NO-ERROR.

      choice = NO.
      IF NOT AVAILABLE itemfg OR xeb.stock-no EQ "" THEN DO WHILE TRUE:

        IF NOT choice OR xeb.stock-no EQ '' OR NOT AVAIL (itemfg) THEN DO:
          choice = NO.
          cNewItem = xeb.stock-no.
          /* IF NOT gvlNoPrompt THEN */
            RUN jc/fgPrompt.w (cocode,xeb.cust-no,xeb.part-no,RECID(xeb),INPUT-OUTPUT cNewItem).
           rEbRow = ROWID(xeb).
          IF cNewitem EQ '' THEN DO:
            RUN dispatch ("cancel-record").
            RETURN.
          END.
          IF cNewitem <> "" THEN DO:
              xeb.stock-no = cNewItem .
              FIND FIRST itemfg
              {sys/look/itemfgrlW.i}
                AND itemfg.i-no   EQ xeb.stock-no
              NO-LOCK NO-ERROR.
             IF NOT AVAILABLE itemfg THEN
            MESSAGE "Item: " + TRIM(xeb.stock-no) + " doesn't exist for Cust Part#: " +  TRIM(xeb.part-no) + 
                 " and Form#: " + STRING(xeb.form-no) + ", would you LIKE to create it?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
            IF choice THEN DO:
              {jc/fgadd.i} 
            END.

          END.

        END. /* Not Choice */
        LEAVE.
      END. /* not avail itemfg do: */
      FIND FIRST itemfg WHERE itemfg.company EQ cocode
                          AND itemfg.i-no    EQ xeb.stock-no
                          NO-LOCK NO-ERROR.
      IF AVAILABLE itemfg AND itemfg.prod-uom EQ "" THEN DO:
       MESSAGE "FG Item " + itemfg.i-no + " has no cost UOM. Please correct the item master and try again."
           VIEW-AS ALERT-BOX ERROR.         
       RETURN NO-APPLY. 
      END.      
    END. /* each xeb */
  END. /* if adm-new-record */
  {&methods/lValidateError.i NO}
      /* end insert */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&frame-name}:
    DISABLE ALL.
  END.

  IF ll-new AND NOT copyJob THEN DO:
    IF job.est-no NE "" THEN DO:
      SESSION:SET-WAIT-STATE("general").
      nufile = YES.
      FIND xeb WHERE ROWID(xeb) = rEbRow NO-LOCK NO-ERROR.

      IF AVAILABLE xeb AND cNewItem GT "" THEN DO:
        FIND xeb WHERE ROWID(xeb) = rEbRow EXCLUSIVE-LOCK NO-ERROR.
        xeb.stock-no = cNewItem.
      END.

      RUN jc/jc-calc.p (RECID(job), NO) NO-ERROR.

      fil_id = RECID(job).
      RUN po/doPo.p (YES) /* Yes Indicates to prompt for RM */.
      RUN jc/addJobFarm.p (INPUT job.job).
      nufile = NO.
      SESSION:SET-WAIT-STATE("").
    END.
    lv-job-rowid = ROWID(job).

    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source", OUTPUT char-hdl).
    FIND FIRST job-hdr NO-LOCK
        WHERE job-hdr.company EQ job.company
          AND job-hdr.job     EQ job.job
          AND job-hdr.job-no  EQ job.job-no
          AND job-hdr.job-no2 EQ job.job-no2 NO-ERROR.

    IF AVAIL job-hdr AND job-hdr.est-no GT "" THEN DO:
         FIND FIRST xeb WHERE xeb.company EQ job-hdr.company
             AND xeb.est-no EQ job-hdr.est-no
             AND xeb.pur-man
             NO-LOCK NO-ERROR.
         IF NOT AVAILABLE xeb THEN
         FIND FIRST xeb WHERE xeb.company EQ job-hdr.company
          AND xeb.est-no EQ job-hdr.est-no            
          NO-LOCK NO-ERROR.

         IF AVAILABLE xeb THEN DO:               
             RUN disable-enable-farm IN WIDGET-HANDLE(char-hdl) (xeb.pur-man) NO-ERROR.
         END.
    END.
    RUN set-attribute-list IN WIDGET-HANDLE(char-hdl) ("NEW-JOB = " + STRING(ROWID(job-hdr))).
    lv-new-job-hdr-rowid = ROWID(job-hdr).
    /* dispatch to children objects */
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-target", OUTPUT char-hdl).
    RUN dispatch IN WIDGET-HANDLE(ENTRY(1,char-hdl)) ('open-query').  
  END.  

  IF copyJob THEN DO:
    RUN util/copyjob.p (copyJobRowID,ROWID(job),OUTPUT jobHdrRowID).
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).     
    RUN reopen-query IN WIDGET-HANDLE(char-hdl) (jobHdrRowID).
  END.
  /*ELSE
  IF ll-sch-updated THEN DO:
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).     
    RUN reopen-query IN WIDGET-HANDLE(char-hdl) (ROWID(job-hdr)).
  END.*/

  /* gdm - 05290901 */
  IF ll-new AND NOT copyJob THEN DO:     
    IF job.start-date EQ ? 
      THEN ASSIGN job.start-date = DATE(job.start-date:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

    IF CAN-FIND(FIRST bf-job-mch NO-LOCK
                WHERE bf-job-mch.company EQ job.company
                AND bf-job-mch.job     EQ job.job
                AND bf-job-mch.job-no  EQ job.job-no
                AND bf-job-mch.job-no2 EQ job.job-no2) AND
       job.start-date NE ? 
      THEN RUN update-job-mch.
  END.
  /* gdm - 05290901 end */    

  DO li = 1 TO NUM-ENTRIES(char-hdl):
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-target", OUTPUT char-hdl).     
    RUN dispatch IN WIDGET-HANDLE(ENTRY(li,char-hdl)) ('open-query':U).
  END.

  ASSIGN
    ll-sch-updated = NO
    copyJob = NO.

  /*needed for immediately pushing print button after adding new job*/
  IF ll-new THEN
  DO:
     IF NOT AVAILABLE job-hdr THEN
        FIND FIRST job-hdr NO-LOCK
             WHERE job-hdr.company EQ job.company
               AND job-hdr.job     EQ job.job
               AND job-hdr.job-no  EQ job.job-no
               AND job-hdr.job-no2 EQ job.job-no2 NO-ERROR.

     IF AVAILABLE job-hdr THEN
        v-reprint = job-hdr.ftick-prnt.

     RUN custom/setUserPrint.p (job.company,'job_.',
                             'begin_job1,begin_job2,end_job1,end_job2,tb_reprint,fl-jobord',
                             job.job-no + ',' + STRING(job.job-no2) + ',' +
                             job.job-no + ',' + STRING(job.job-no2) + ',' +
                             STRING(v-reprint) + ',' +  "0" ). /* gdm - 07130906 */  
  END.


  /* re-open the query so when user selects Estimate folder, it shows the new estimate added. */
  IF ll-new THEN DO:  
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).     
    RUN reopen-query IN WIDGET-HANDLE(char-hdl) (ROWID(job-hdr)).  /*Task# 12161304*/
  END.

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rebuild-Stds V-table-Win 
PROCEDURE Rebuild-Stds :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAILABLE job AND job.est-no NE "" THEN DO:   /*** STANDARD CALCULATION ***/

     IF job.stat = "Z" OR job.stat = "C" THEN DO:
         MESSAGE "Job is closed." SKIP
                 "You must REOPEN job before Rebuilding Job Qty" 
             VIEW-AS ALERT-BOX ERROR.
         UNDO, RETURN.
     END.

     FOR EACH job-hdr FIELDS(company est-no i-no) 
         where job-hdr.company EQ job.company
           and job-hdr.job     EQ job.job
           and job-hdr.job-no  EQ job.job-no
         and job-hdr.job-no2 EQ job.job-no2 NO-LOCK ,
         FIRST itemfg FIELDS(pur-man) WHERE
               itemfg.company EQ job-hdr.company AND
               itemfg.i-no EQ job-hdr.i-no NO-LOCK:
         FIND FIRST eb NO-LOCK
             WHERE eb.company EQ cocode
               AND eb.est-no EQ job.est-no 
               AND eb.stock EQ job-hdr.i-no  NO-ERROR.
         IF AVAIL eb AND eb.pur-man NE itemfg.pur-man AND NOT itemfg.isaset  THEN DO:
             MESSAGE "FG Item file indicates item is (x) (which would be either purchased " SKIP
                "or manufactured) while estimate indicates it is (y) - These should be" SKIP
                " set the same." VIEW-AS ALERT-BOX WARNING . 
             LEAVE .
         END.

     END.
          


     FOR EACH job-mat FIELDS(company all-flg rm-i-no) WHERE
         job-mat.company = job.company AND
         job-mat.job = job.job AND
         job-mat.job-no = job.job-no AND
         job-mat.job-no2 = ASI.job.job-no2 AND
         job-mat.all-flg = YES
         NO-LOCK,
         FIRST item FIELDS(mat-type) WHERE
               item.company EQ job-mat.company AND
               item.i-no EQ job-mat.rm-i-no AND
               ITEM.mat-type EQ 'B'
               NO-LOCK:

               MESSAGE "Real Board Has Been Committed to Import Estimated Board Material." SKIP
                       "You Must Deallocate Board."
                  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
               LEAVE.
     END.

     RUN jc/jobstds.p (ROWID(job)).
     RUN refresh-browser.
     RUN get-start-date(INPUT YES).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalc-costs V-table-Win 
PROCEDURE recalc-costs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hld-stat AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ll AS LOG NO-UNDO.
  DEF VAR iQty     AS INTEGER NO-UNDO .
  DEF BUFFER bf-job-hdr FOR job-hdr .

  IF AVAILABLE job AND job.est-no NE "" THEN DO:
    MESSAGE "Recalculate Job Cost?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll.  

    IF ll THEN DO:
      ASSIGN
       nufile   = NO
       hld-stat = job.stat.
       FOR EACH bf-job-hdr NO-LOCK 
            WHERE bf-job-hdr.company EQ job.company 
             AND bf-job-hdr.job     EQ job.job
             AND bf-job-hdr.job-no  EQ job.job-no
             AND bf-job-hdr.job-no2 EQ job.job-no2 :
            iQty = iQty + bf-job-hdr.qty .
        END.
        IF iQty EQ 0 THEN nufile = YES .

      RUN jc/jc-calc.p (RECID(job), NO).

      nufile = NO.

      IF hld-stat NE "P" THEN job.stat = hld-stat.

      RUN refresh-browser.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh-browser V-table-Win 
PROCEDURE refresh-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  DEFINE VARIABLE li AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
  DEFINE VARIABLE char-hdl2 AS CHARACTER NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"job-hdr-source", OUTPUT char-hdl).     
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
    char-hdl2 = char-hdl.
    RUN get-job-hdr IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-rowid).
  END.

/*
  lv-rowid = IF AVAIL job THEN ROWID(job) ELSE ?.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).     
  RUN reopen-query IN WIDGET-HANDLE(char-hdl) (?).

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-target", OUTPUT char-hdl).
  DO li = 1 TO NUM-ENTRIES(char-hdl):
    IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(li,char-hdl))) THEN DO:
      RUN dispatch IN WIDGET-HANDLE(ENTRY(li,char-hdl)) ('open-query').
      RUN dispatch IN WIDGET-HANDLE(ENTRY(li,char-hdl)) ('row-changed').
    END.
  END.
*/
  IF lv-rowid NE ? THEN
  FIND job-hdr WHERE ROWID(job-hdr) EQ lv-rowid NO-LOCK NO-ERROR.

  IF NOT AVAILABLE job-hdr THEN
  FIND FIRST job-hdr
      WHERE job-hdr.company EQ job.company
        AND job-hdr.job     EQ job.job
        AND job-hdr.job-no  EQ job.job-no
        AND job-hdr.job-no2 EQ job.job-no2
      NO-LOCK NO-ERROR.

  IF AVAILABLE job-hdr THEN DO:
    lv-rowid = ROWID(job-hdr).

    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).     
    RUN reopen-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).

    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-target", OUTPUT char-hdl).
    DO li = 1 TO NUM-ENTRIES(char-hdl):
      IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(li,char-hdl))) THEN DO:
        RUN dispatch IN WIDGET-HANDLE(ENTRY(li,char-hdl)) ('open-query').
        IF char-hdl EQ char-hdl2 THEN
          RUN repo-query IN WIDGET-HANDLE(ENTRY(li,char-hdl)) (lv-rowid).
        ELSE
          RUN dispatch IN WIDGET-HANDLE(ENTRY(li,char-hdl)) ('row-changed').
      END.
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
  {src/adm/template/snd-list.i "job"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setUseJobQty V-table-Win 
PROCEDURE setUseJobQty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iplUseJobQty AS LOG NO-UNDO.
gvlUseJobQty = iplUseJobQty.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE unapprove V-table-Win 
PROCEDURE unapprove :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.  
   {&methods/lValidateError.i YES}
   IF v-unapp-security THEN DO:
      IF USERID("nosweat") NE job.cs-user-id-t THEN DO:
         FIND FIRST usergrps WHERE usergrps.usergrps = "JU2" NO-LOCK NO-ERROR.
         IF NOT AVAILABLE usergrps THEN DO:
            MESSAGE "User ID: " USERID("nosweat") 
                    " did not approve this JOB and " SKIP
                    "there is no JU2 group override available." SKIP(1)
                    "Contact your system administrator."
            VIEW-AS ALERT-BOX ERROR.

            RETURN NO-APPLY.        
         END. /* IF NOT AVAIL usergrps */
         ELSE IF AVAILABLE usergrps AND 
                 NOT CAN-DO(usergrps.users,USERID("NOSWEAT")) THEN DO:
            MESSAGE "User ID: " USERID("nosweat") 
                    " did not approve this JOB and " SKIP
                    "is not a memeber of the JU2 override group." SKIP(1)
                    "Contact your system administrator."
            VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
         END. /* ELSE IF AVAIL usergrps AND */
      END. /* IF USERID("nosweat") NE job.USER-ID */
   END. /* IF AVAIL sys-ctrl AND sys-ctrl.log-fld */

  IF AVAILABLE job THEN DO:
    FIND CURRENT job NO-ERROR.

    ASSIGN
     job.cs-to-pr      = NO
     job.cs-user-id-t  = ""
     job.cs-trans-date = ?
     job.cs-trans-time = 0
     job.pr-printed    = NO
     job.pr-user-id-p  = ""
     job.pr-print-date = ?
     job.pr-print-time = 0.

    FIND CURRENT job NO-LOCK NO-ERROR.

    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
    RUN reopen-query IN WIDGET-HANDLE(char-hdl) (?) .
  END.
  {&methods/lValidateError.i NO}
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-job-mch V-table-Win 
PROCEDURE update-job-mch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF job-mch.

/* rstark 06241201 */
DEFINE VARIABLE calcDueDate AS DATE NO-UNDO.
DEFINE VARIABLE calcStartDate AS DATE NO-UNDO.

IF noDate THEN RETURN.
IF planDate THEN DO:
  IF NOT VALID-HANDLE(scheduleHndl) THEN
  RUN custom/schedule.p PERSISTENT SET scheduleHndl.
  RUN scheduleJob IN scheduleHndl (ROWID(job),OUTPUT calcStartDate,OUTPUT calcDueDate).
  IF calcDueDate NE job.due-date THEN
  MESSAGE 'Machine Capacity calulated Scheduled Completion date of'
    calcDueDate SKIP 'Update Due Date?'
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE updateDueDate AS LOGICAL.
  IF updateDueDate THEN job.due-date = calcDueDate.
  job.start-date = calcStartDate.
END.
/* rstark 06241201 */

FOR EACH bf-job-mch EXCLUSIVE-LOCK
  WHERE bf-job-mch.company EQ job.company
    AND bf-job-mch.job     EQ job.job
    AND bf-job-mch.job-no  EQ job.job-no
    AND bf-job-mch.job-no2 EQ job.job-no2
    USE-INDEX line-idx :

    ASSIGN bf-job-mch.start-date-su = job.start-date.

END.
RELEASE bf-job-mch.
/*
RUN get-link-handle IN adm-broker-hdl
        (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(job)).
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-schedule V-table-Win 
PROCEDURE update-schedule :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF noDate THEN RETURN. /* rstark 06241201 */
RUN jc/updateSchedule.p (INPUT job.start-date, INPUT ROWID(job)).

/*                                                                                                                   */
/*   DEF BUFFER bf-hdr FOR job-hdr.                                                                                  */
/*   DEF BUFFER bf-mch FOR job-mch.                                                                                  */
/*                                                                                                                   */
/*   DEF VAR lv-start-date AS DATE NO-UNDO.                                                                          */
/*   DEF VAR lv-m-time AS INT no-undo.                                                                               */
/*   DEF VAR lv-run-time AS INT NO-UNDO.                                                                             */
/*   DEF VAR lv-mr-time AS INT NO-UNDO.                                                                              */
/*   DEF VAR lv-job-time  AS INT NO-UNDO.                                                                            */
/*   DEF VAR lv-job2-time  AS INT NO-UNDO.  /* job time if job needs more than a day */                              */
/*   DEF VAR lv-start-time AS INT NO-UNDO.                                                                           */
/*   DEF VAR lv-got-st-time AS LOG NO-UNDO.                                                                          */
/*   DEF VAR lv-prev-end-time AS INT NO-UNDO.                                                                        */
/*   DEF VAR lv-day-time AS INT NO-UNDO.  /* mach.start-time - previous mach's end-time */                           */
/*   DEF VAR lv-lap-time AS INT NO-UNDO.                                                                             */
/*                                                                                                                   */
/*   IF noDate THEN RETURN. /* rstark 06241201 */                                                                    */
/*                                                                                                                   */
/*   FOR EACH bf-hdr OF job:                                                                                         */
/*       bf-hdr.start-date = job.start-date.                                                                         */
/*   END.                                                                                                            */
/*   ASSIGN                                                                                                          */
/*     lv-start-date = job.start-date                                                                                */
/*     lv-job-time = 0.                                                                                              */
/*   FOR EACH bf-mch OF job /*WHERE NOT bf-mch.anchored */                                                           */
/*       BY bf-mch.frm BY bf-mch.blank-no by bf-mch.pass BY bf-mch.m-code:                                           */
/*                                                                                                                   */
/*       FIND FIRST mach-calendar WHERE mach-calendar.company = job.company                                          */
/*                         AND mach-calendar.m-code = bf-mch.m-code                                                  */
/*                         AND mach-calendar.m-date = lv-start-date                                                  */
/*                         NO-LOCK NO-ERROR.                                                                         */
/*       lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time                   */
/*                   ELSE 28800. /* 8 HRs*/                                                                          */
/*       IF lv-m-time LT 0 THEN lv-m-time = 28800.                                                                   */
/*                                                                                                                   */
/*       IF AVAIL mach-calendar AND NOT lv-got-st-time THEN                                                          */
/*                       ASSIGN lv-start-time = mach-calendar.start-time                                             */
/*                              lv-got-st-time = YES.                                                                */
/*                                                                                                                   */
/*       lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE                                                                */
/*                   truncate(bf-mch.mr-hr,0) * 3600 +                                                               */
/*                 ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60.                                */
/*       lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE                                                              */
/*                   truncate(bf-mch.run-hr,0) * 3600 +                                                              */
/*                 ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60.                              */
/*                                                                                                                   */
/*       ASSIGN bf-mch.seq-no = 0                                                                                    */
/*              bf-mch.start-time-su = lv-start-time + lv-job-time + lv-day-time.                                    */
/*              bf-mch.start-date-su = lv-start-date               .                                                 */
/*                                                                                                                   */
/*       lv-job-time = lv-job-time + lv-mr-time + lv-run-time.                                                       */
/*                                                                                                                   */
/*       lv-start-date = lv-start-date +                                                                             */
/*                       IF lv-mr-time > lv-m-time THEN TRUNCATE(lv-mr-time / lv-m-time,0)                           */
/*                       ELSE 0.                                                                                     */
/*                                                                                                                   */
/*       IF lv-mr-time > lv-m-time THEN DO:                                                                          */
/*          lv-job2-time = lv-mr-time - lv-m-time.                                                                   */
/*          lv-lap-time = bf-mch.start-time-su - lv-start-time.                                                      */
/*          FIND FIRST mach-calendar WHERE mach-calendar.company = job.company                                       */
/*                         AND mach-calendar.m-code = bf-mch.m-code                                                  */
/*                         AND mach-calendar.m-date = lv-start-date                                                  */
/*                         NO-LOCK NO-ERROR.                                                                         */
/*          lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time                */
/*                      ELSE 28800. /* 8 HRs*/.                                                                      */
/*          IF lv-m-time LT 0 THEN lv-m-time = 28800.                                                                */
/*          lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.                             */
/*          ASSIGN bf-mch.end-time-su = lv-start-time + lv-job2-time + lv-lap-time                                   */
/*                 bf-mch.start-time = lv-start-time + lv-job2-time + lv-lap-time                                    */
/*                 bf-mch.end-date-su = lv-start-date                                                                */
/*                 bf-mch.start-date = lv-start-date                                                                 */
/*                 lv-day-time = lv-start-time - lv-prev-end-time + 86400 .                                          */
/*       END.                                                                                                        */
/*       ELSE ASSIGN bf-mch.end-time-su = lv-start-time + lv-job-time - lv-run-time + lv-day-time                    */
/*                   bf-mch.start-time = lv-start-time + lv-job-time - lv-run-time + lv-day-time                     */
/*                   bf-mch.end-date-su = lv-start-date                                                              */
/*                   bf-mch.start-date = lv-start-date                                                               */
/*                   lv-lap-time = 0.                                                                                */
/*                                                                                                                   */
/*       lv-start-date = lv-start-date +                                                                             */
/*                       IF (lv-run-time ) > lv-m-time THEN TRUNCATE((lv-run-time) / lv-m-time,0)                    */
/*                       ELSE 0.                                                                                     */
/*                                                                                                                   */
/*       IF (lv-run-time) > lv-m-time THEN DO:                                                                       */
/*          lv-job2-time = lv-mr-time + lv-run-time - lv-m-time.                                                     */
/*          lv-lap-time = bf-mch.start-time - lv-start-time.                                                         */
/*          FIND FIRST mach-calendar WHERE mach-calendar.company = job.company                                       */
/*                         AND mach-calendar.m-code = bf-mch.m-code                                                  */
/*                         AND mach-calendar.m-date = lv-start-date                                                  */
/*                         NO-LOCK NO-ERROR.                                                                         */
/*          lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time                */
/*                      ELSE 28800. /* 8 HRs*/.                                                                      */
/*          IF lv-m-time LT 0 THEN lv-m-time = 28800.                                                                */
/*          lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.                             */
/*          ASSIGN bf-mch.end-time = lv-start-time + lv-job2-time + lv-lap-time                                      */
/*                 bf-mch.end-date = lv-start-date                                                                   */
/*                 lv-day-time = lv-day-time + lv-start-time - lv-prev-end-time + 86400 .                            */
/*       END.                                                                                                        */
/*       ELSE ASSIGN bf-mch.end-time = /*lv-start-time + lv-job-time */                                              */
/*                                     bf-mch.start-time + lv-run-time                                               */
/*                   bf-mch.end-date = lv-start-date                                                                 */
/*                   lv-lap-time = 0.                                                                                */
/*                                                                                                                   */
/*       lv-prev-end-time = IF AVAIL mach-calendar THEN mach-calendar.end-time ELSE 86400. /* 24 HRs*/               */
/*                                                                                                                   */
/*       IF string(bf-mch.end-time,"hh:mm:ss") > string(lv-prev-end-time,"hh:mm:ss") THEN DO:                        */
/*                         lv-start-date = lv-start-date + 1.                                                        */
/*                         lv-lap-time = bf-mch.end-time - lv-prev-end-time.                                         */
/*                         FIND FIRST mach-calendar WHERE mach-calendar.company = job.company                        */
/*                                    AND mach-calendar.m-code = bf-mch.m-code                                       */
/*                                    AND mach-calendar.m-date = lv-start-date                                       */
/*                                    NO-LOCK NO-ERROR.                                                              */
/*                         lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time */
/*                                 ELSE 28800. /* 8 HRs*/.                                                           */
/*                         IF lv-m-time LT 0 THEN lv-m-time = 28800.                                                 */
/*                         lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.              */
/*                         ASSIGN bf-mch.end-time = lv-start-time + lv-lap-time                                      */
/*                                bf-mch.end-date = lv-start-date                                                    */
/*                                lv-day-time = lv-day-time + lv-start-time - lv-prev-end-time + 86400.              */
/*      END.                                                                                                         */
/*   END.                                                                                                            */
  ll-sch-updated = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateOrderDate V-table-Win 
PROCEDURE updateOrderDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bf-job-hdr FOR job-hdr.
DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
DEFINE BUFFER bfUpd-oe-ordl FOR oe-ordl.

FOR EACH bf-job-hdr OF job NO-LOCK:
    FOR EACH bf-oe-ordl WHERE bf-oe-ordl.company EQ bf-job-hdr.company
        AND bf-oe-ordl.ord-no EQ bf-job-hdr.ord-no
        NO-LOCK:

        IF DATE(bf-oe-ordl.spare-int-3) NE job.start-date THEN DO:
            FIND bfUpd-oe-ordl WHERE ROWID(bfUpd-oe-ordl) EQ ROWID(bf-oe-ordl)
                EXCLUSIVE-LOCK.
            ASSIGN bfUpd-oe-ordl.spare-int-2 = INT(job.start-date).
            RELEASE bfUpd-oe-ordl.
        END.

    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-user V-table-Win 
PROCEDURE valid-cust-user :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
custcount = "".
DEFINE VARIABLE lActive AS LOG NO-UNDO.
DEFINE VARIABLE v-cust-chk AS CHARACTER NO-UNDO.
RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'JU1',
                            INPUT YES,
                            OUTPUT lActive).
{sys/inc/chblankcust.i ""JU1""}
  IF ou-log THEN DO:
    DO WITH FRAME {&FRAME-NAME}:
        job.est-no:SCREEN-VALUE = FILL(" ",8 - LENGTH(TRIM(INPUT job.est-no))) + TRIM(INPUT job.est-no).

        FIND FIRST est
          WHERE est.company EQ cocode
            AND est.loc     EQ locode
            AND est.est-no  EQ job.est-no:SCREEN-VALUE
          NO-LOCK NO-ERROR.
        IF AVAILABLE est THEN
            FIND FIRST eb
          WHERE eb.company = cocode
            AND eb.est-no   EQ est.est-no
            AND eb.form-no NE 0
            AND TRIM(eb.cust-no) NE "" 
          NO-LOCK NO-ERROR.
        IF AVAILABLE eb THEN
            v-cust-chk = eb.cust-no .
        ELSE v-cust-chk = "" .

     IF LOOKUP(v-cust-chk,custcount) = 0 THEN DO:
          MESSAGE "Customer is not on Users Customer List.  "  SKIP
              "Please add customer to Network Admin - Users Customer List."  VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO job.est-no .
          RETURN ERROR.
      END.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-due-date V-table-Win 
PROCEDURE valid-due-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DATE(job.due-date:SCREEN-VALUE) LT DATE(job.start-date:SCREEN-VALUE) THEN DO:
      MESSAGE TRIM(job.due-date:LABEL) +
              " may not be before " +
              TRIM(job.start-date:LABEL) +
              "..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO job.due-date.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no V-table-Win 
PROCEDURE valid-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  ll-valid = YES.

  DO WITH FRAME {&frame-name}:
    job.job-no:SCREEN-VALUE =
        FILL(" ",6 - LENGTH(TRIM(job.job-no:SCREEN-VALUE))) +
        TRIM(job.job-no:SCREEN-VALUE).

    IF TRIM(job.job-no:SCREEN-VALUE) EQ "" THEN DO:
      MESSAGE "Job# Cannot be BLANK..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO job.job-no.
      ll-valid = NO.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no2 V-table-Win 
PROCEDURE valid-job-no2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  ll-valid = YES.

  DO WITH FRAME {&frame-name}:
    IF CAN-FIND(FIRST xjob
        WHERE xjob.company EQ job.company
          AND xjob.job-no  EQ job.job-no:SCREEN-VALUE
          AND xjob.job-no2 EQ INT(job.job-no2:SCREEN-VALUE)
          AND ROWID(xjob)  NE ROWID(job)) THEN DO:

      MESSAGE "Job# is already on File..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO job.job-no.
      ll-valid = NO.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-whse V-table-Win 
PROCEDURE valid-whse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  ll-valid = YES.

  DO WITH FRAME {&frame-name}:

    IF TRIM(job.loc:SCREEN-VALUE) EQ "" THEN DO:
      MESSAGE "Warehouse Cannot be BLANK..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO job.loc.
      ll-valid = NO.
    END.
    FIND FIRST loc 
        WHERE loc.company EQ cocode 
          AND loc.loc EQ job.loc:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE loc THEN DO:
      MESSAGE "Invalid warehouse code entered..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO job.loc.
      ll-valid = NO.
    END.

  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-est V-table-Win 
PROCEDURE validate-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-lastevent AS cha NO-UNDO.

  DEFINE VARIABLE v-bld-job LIKE job.job-no NO-UNDO.
  DEFINE VARIABLE li AS INTEGER NO-UNDO.
  DEFINE VARIABLE ll AS LOG NO-UNDO.
  DEFINE VARIABLE lActive AS LOG NO-UNDO.

    ASSIGN ll-Valid = NO.
    FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name    EQ "JOBCREAT"
        NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN DO:
        MESSAGE 
            "Must have System Control Parameter 'JOBCREATE'..."
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO job.est-no IN FRAME {&FRAME-NAME}.
        RETURN.
    END.

  {methods/lValidateError.i YES}
  ll-valid = YES.

  DO WITH FRAME {&FRAME-NAME}:

    IF job.est-no:SCREEN-VALUE NE "" THEN DO:
      job.est-no:SCREEN-VALUE =
          FILL(" ",8 - LENGTH(TRIM(INPUT job.est-no))) + TRIM(INPUT job.est-no).

      ll-valid = NO.

      FIND FIRST est
          WHERE est.company EQ cocode
            AND est.loc     EQ locode
            AND est.est-no  EQ job.est-no:SCREEN-VALUE
          NO-LOCK NO-ERROR.

      IF NOT AVAILABLE est THEN DO:
        MESSAGE "Estimate Number is not valid, try help..."
              VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO job.est-no.
        RETURN NO-APPLY.
      END.
      /* Task 05231202 - check for inactive FG in estimate */
      FOR EACH eb
          WHERE eb.company = cocode
            AND eb.est-no   EQ est.est-no
            AND eb.form-no NE 0
            AND TRIM(eb.cust-no) NE "" 
          NO-LOCK:
          IF eb.stock-no <> "" THEN DO:
              RUN fg/GetItemfgActInact.p (INPUT g_company,
                                          INPUT eb.stock-no,
                                          OUTPUT lActive).
/*             FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS" */
/*                              AND reftable.company  EQ g_company       */
/*                              AND reftable.loc      EQ ""              */
/*                              AND reftable.code     EQ eb.stock-no     */
/*                              NO-LOCK NO-ERROR.                        */
/*             IF AVAIL reftable AND reftable.code2 = "I" THEN DO:       */
              IF NOT lActive THEN DO:
                MESSAGE "Item: " eb.stock-no " has status of Inactive. Job cannot be added for this item."
                    VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
              END.  /* if not active */
          END. /* if eb.stock-no <>"" */
      END. /* for each eb */

      FOR EACH est-op NO-LOCK
          WHERE est-op.company EQ est.company
            AND est-op.est-no  EQ est.est-no
            AND est-op.line    LT 500,
          FIRST mach NO-LOCK
          {sys/look/machW.i}
            AND mach.m-code EQ est-op.m-code:
       IF mach.obsolete THEN DO:
        MESSAGE "Machine: " + TRIM(mach.m-code) +
                " is obsolete, please replace to create job..."
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO job.est-no.
        RETURN NO-APPLY.
       END.
      END.

      ll-valid = YES.
    END.

    IF ip-lastevent EQ "" OR job.job-no:SCREEN-VALUE EQ "" THEN DO:
      RUN jc/job-no.p (INPUT-OUTPUT v-bld-job, 
                       INPUT-OUTPUT li,
                       INPUT "", 
                       INPUT "").

      IF v-bld-job EQ "" THEN DO:
        IF job.est-no:SCREEN-VALUE NE "" THEN DO:
           ASSIGN
            v-bld-job = " " + job.est-no:SCREEN-VALUE
            li        = 0.  

           IF AVAILABLE sys-ctrl THEN
             v-bld-job = SUBSTR(sys-ctrl.char-fld,1,1) + TRIM(v-bld-job).

           ASSIGN
              v-bld-job = FILL(" ",6 - LENGTH(TRIM(v-bld-job))) + TRIM(v-bld-job)
              ll = NO.

           IF CAN-FIND(FIRST xjob WHERE
              xjob.company EQ cocode AND
              xjob.job-no  EQ v-bld-job AND
              ROWID(xjob)  NE ROWID(job) AND
              xjob.opened EQ YES) THEN
              DO:
                 MESSAGE "Job(s) Already Exist for This Estimate, Create New One?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE ll.

                 IF NOT ll THEN DO:
                    APPLY "entry" TO job.est-no.
                    RETURN NO-APPLY.
                 END.
              END.

           FOR EACH xjob FIELDS(job-no2) NO-LOCK
               WHERE xjob.company EQ cocode
                 AND xjob.job-no  EQ v-bld-job
                 AND ROWID(xjob)  NE ROWID(job)
               USE-INDEX job-no
               BY xjob.job-no2 DESCENDING:

               li = xjob.job-no2 + 1.

               LEAVE.
           END.
        END.

        ELSE DO:

          FIND LAST xjob
              WHERE xjob.company EQ cocode
                AND xjob.job-no  BEGINS SUBSTR(sys-ctrl.char-fld,2,1)
              NO-LOCK NO-ERROR.

          ASSIGN
           li        = (IF AVAILABLE xjob THEN INT(SUBSTR(xjob.job-no,2,5)) ELSE 0) + 1
           v-bld-job = SUBSTR(sys-ctrl.char-fld,2,1) + STRING(li,"99999")
           li        = 0.
        END.
      END.

      ASSIGN
       job.job-no:SCREEN-VALUE  = v-bld-job
       job.job-no2:SCREEN-VALUE = STRING(li,"99").
    END.

    /*FIND FIRST job-hdr OF job NO-LOCK NO-ERROR.
    RUN get-due-date(ROWID(job-hdr)) .*/
    DISABLE ALL.

    IF ip-lastevent EQ "" THEN DO:   /* except update button clicked */
      ENABLE job.job-no job.job-no2 job.start-date job.due-date.
      APPLY "entry" TO job.job-no.
      RETURN NO-APPLY.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-start-date V-table-Win 
PROCEDURE validate-start-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:
        RUN jc/validStartDate.p (INPUT job.start-date:SCREEN-VALUE,
                                 OUTPUT ll-valid).
        IF NOT ll-valid THEN
            APPLY "entry" TO job.start-date.
    END.
/*   DEF VAR lv-msg AS CHAR NO-UNDO.                            */
/*                                                              */
/*   ll-valid = YES.                                            */
/*                                                              */
/*   DO WITH FRAME {&FRAME-NAME}:                               */
/*     IF lv-msg EQ ""                               AND        */
/*        DATE(job.start-date:SCREEN-VALUE) LT TODAY THEN       */
/*       lv-msg = "earlier than today".                         */
/*                                                              */
/*     IF lv-msg EQ ""                                     AND  */
/*        DATE(job.start-date:SCREEN-VALUE) GT TODAY + 180 THEN */
/*       lv-msg = "later than 180 days from today".             */
/*                                                              */
/*     IF lv-msg NE "" THEN DO:                                 */
/*       MESSAGE TRIM(job.start-date:LABEL) +                   */
/*               " Date cannot be " + TRIM(lv-msg) + "..."      */
/*           VIEW-AS ALERT-BOX ERROR.                           */
/*       APPLY "entry" TO job.start-date.                       */
/*       ll-valid = NO.                                         */
/*     END.                                                     */
/*   END.                                                       */

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE view-user-id V-table-Win 
PROCEDURE view-user-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:

      DISPLAY job.user-id.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-est V-table-Win 
PROCEDURE valid-job-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  ll-valid = YES.

  DO WITH FRAME {&frame-name}:
   IF adm-new-record THEN DO: 
    IF TRIM(job.est-no:SCREEN-VALUE) EQ "" THEN DO:
      MESSAGE "Job# is not allowed  to create with  blank line..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO job.job-no.
      ll-valid = NO.
    END.
   END.
  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

