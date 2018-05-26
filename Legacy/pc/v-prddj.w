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
{custom/globdefs.i}
DEF VAR li-help-job AS INT NO-UNDO.
DEF VAR ll-new-record AS LOG NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE oeDateAuto-log AS LOGICAL NO-UNDO.

RUN sys/ref/nk1look.p (INPUT g_company, "DCClosedJobs", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    oeDateAuto-log = LOGICAL(cRtnChar) NO-ERROR.

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
&Scoped-define EXTERNAL-TABLES pc-prdd
&Scoped-define FIRST-EXTERNAL-TABLE pc-prdd


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR pc-prdd.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS pc-prdd.job-no pc-prdd.job-no2 ~
pc-prdd.op-date pc-prdd.shift 
&Scoped-define ENABLED-TABLES pc-prdd
&Scoped-define FIRST-ENABLED-TABLE pc-prdd
&Scoped-Define ENABLED-OBJECTS RECT-5 
&Scoped-Define DISPLAYED-FIELDS pc-prdd.job-no pc-prdd.job-no2 ~
pc-prdd.op-date pc-prdd.shift 
&Scoped-define DISPLAYED-TABLES pc-prdd
&Scoped-define FIRST-DISPLAYED-TABLE pc-prdd


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
DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 142 BY 2.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     pc-prdd.job-no AT ROW 1.71 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     pc-prdd.job-no2 AT ROW 1.71 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     pc-prdd.op-date AT ROW 1.71 COL 51 COLON-ALIGNED
          LABEL "Date"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     pc-prdd.shift AT ROW 1.71 COL 92 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     RECT-5 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.pc-prdd
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN pc-prdd.op-date IN FRAME F-Main
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR rec-val AS RECID NO-UNDO.

    CASE FOCUS:NAME :
        WHEN "job-no" THEN DO:
             RUN windows/l-jobno.w (g_company,FOCUS:SCREEN-VALUE ,OUTPUT char-val, OUTPUT rec-val).
             IF rec-val <> ? THEN DO:
                FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.
                IF AVAIL job-hdr THEN 
                   ASSIGN pc-prdd.job-no:SCREEN-VALUE = job-hdr.job-no
                          pc-prdd.job-no2:SCREEN-VALUE = string(job-hdr.job-no2)
                          li-help-job = job-hdr.job.
                            
             END.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-prdd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.job-no V-table-Win
ON LEAVE OF pc-prdd.job-no IN FRAME F-Main /* Job Number */
DO:
    IF LASTKEY = -1  THEN RETURN.

    IF length(pc-prdd.job-no:SCREEN-VALUE ) < 6 THEN
        pc-prdd.job-no:SCREEN-VALUE = FILL(" ",6 - length(pc-prdd.job-no:SCREEN-VALUE)) + 
                                      TRIM(pc-prdd.job-no:SCREEN-VALUE).

    FIND FIRST job WHERE job.company = g_company AND
                         job.job-no = pc-prdd.job-no:SCREEN-VALUE
                         NO-LOCK NO-ERROR.
    
    IF NOT AVAIL job THEN DO:
       MESSAGE "Invalid Job#. Try Help" VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    ELSE IF AVAIL job AND job.opened EQ NO AND oeDateAuto-log THEN DO:
        MESSAGE "Job " STRING(job.job-no) " is currently closed. You must re-open the job to add data collection data." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    ELSE li-help-job = job.job.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-prdd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.job-no2 V-table-Win
ON LEAVE OF pc-prdd.job-no2 IN FRAME F-Main /* Run # */
DO:
   IF LASTKEY = -1 THEN RETURN.

    FIND FIRST job-hdr WHERE job-hdr.company = g_company 
                   AND job-hdr.job-no = pc-prdd.job-no:SCREEN-VALUE 
                   AND job-hdr.job-no2 = int(pc-prdd.job-no2:SCREEN-VALUE)
               NO-LOCK NO-ERROR.
   IF NOT AVAIL job-hdr THEN DO:
        MESSAGE "Invalid Job#. Try Help. " VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-prdd.op-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-prdd.op-date V-table-Win
ON LEAVE OF pc-prdd.op-date IN FRAME F-Main /* Date */
DO:
  {custom/currentDatePrompt.i SELF:SCREEN-VALUE}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = YES.
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addPlusButton V-table-Win 
PROCEDURE addPlusButton :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch('add-record').

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
  {src/adm/template/row-list.i "pc-prdd"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "pc-prdd"}

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
  DEF VAR old-job-no LIKE pc-prdd.job-no NO-UNDO.
  DEF VAR old-job-no2 LIKE pc-prdd.job-no2 NO-UNDO.
  DEF VAR old-op-date LIKE pc-prdd.op-date NO-UNDO.
  DEF VAR old-shift LIKE pc-prdd.shift NO-UNDO.
  
  DEF BUFFER bf-prdd FOR pc-prdd.


  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   old-job-no  = pc-prdd.job-no
   old-job-no2 = pc-prdd.job-no2
   old-op-date = pc-prdd.op-date
   old-shift   = pc-prdd.shift.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  pc-prdd.job-no = FILL(" ",6 - LENGTH(TRIM(pc-prdd.job-no))) + TRIM(pc-prdd.job-no).

  ll-new-record = pc-prdd.job-no  NE old-job-no  OR
                  pc-prdd.job-no2 NE old-job-no2 OR
                  pc-prdd.op-date NE old-op-date OR
                  pc-prdd.shift   NE old-shift.

  FIND FIRST job WHERE job.company = pc-prdd.company
                   AND job.job-no = pc-prdd.job-no
                   AND job.job-no2 = pc-prdd.job-no2
                 NO-LOCK NO-ERROR.
  IF AVAIL job THEN pc-prdd.job = job.job.
                           
  IF ll-new-record THEN DO:
      FIND FIRST job WHERE job.company = pc-prdd.company
                       AND job.job-no = pc-prdd.job-no
                       AND job.job-no2 = pc-prdd.job-no2
                       NO-LOCK NO-ERROR.
      IF AVAIL job THEN pc-prdd.rec_key = job.rec_key.
  END.
 
  FOR EACH bf-prdd
      WHERE (bf-prdd.company EQ pc-prdd.company AND
             bf-prdd.job-no  EQ old-job-no      AND
             bf-prdd.job-no2 EQ old-job-no2     AND
             bf-prdd.op-date EQ old-op-date     AND
             bf-prdd.shift   EQ old-shift)
         OR ROWID(bf-prdd) EQ ROWID(pc-prdd):

    IF ROWID(bf-prdd) NE ROWID(pc-prdd) THEN
      ASSIGN
       bf-prdd.job-no  = pc-prdd.job-no
       bf-prdd.job-no2 = pc-prdd.job-no2
       bf-prdd.op-date = pc-prdd.op-date
       bf-prdd.shift   = pc-prdd.shift.

    IF NOT CAN-FIND(FIRST pc-prdh
       WHERE pc-prdh.company    EQ bf-prdd.company
         AND pc-prdh.m-code     EQ bf-prdd.m-code
         AND pc-prdh.trans-date EQ bf-prdd.op-date
         AND pc-prdh.shift      EQ bf-prdd.shift) THEN DO:
      CREATE pc-prdh.
      ASSIGN
       pc-prdh.company    = bf-prdd.company
       pc-prdh.m-code     = bf-prdd.m-code
       pc-prdh.trans-date = bf-prdd.op-date
       pc-prdh.shift      = bf-prdd.shift.
    END.
  END.

  FOR EACH pc-prdh
      WHERE pc-prdh.company EQ pc-prdd.company
        AND NOT CAN-FIND(FIRST pc-prdd
                         WHERE pc-prdd.company EQ pc-prdh.company
                           AND pc-prdd.m-code  EQ pc-prdh.m-code
                           AND pc-prdd.op-date EQ pc-prdh.trans-date
                           AND pc-prdd.shift   EQ pc-prdh.shift):
    DELETE pc-prdh.
  END.

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
  ASSIGN pc-prdd.company = g_company
         pc-prdd.op-date = TODAY
         pc-prdd.op-time = TIME
         pc-prdd.opn = YES
         pc-prdd.shift = 1
         pc-prdd.USER-ID = USERID(LDBNAME(1)) 
         .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
/* ========= validataion =============== */
 ll-new-record = adm-new-record.
 DO WITH FRAME {&FRAME-NAME} :
 FIND FIRST job WHERE job.company = pc-prdd.company
                   AND job.job-no = pc-prdd.job-no:SCREEN-VALUE
                   AND job.job-no2 = INT(pc-prdd.job-no2:SCREEN-VALUE)
                 NO-LOCK NO-ERROR.
  if avail job and job.stat = "H" then do:
     message "JOB ON HOLD. DO NOT PROCEED!" 
              VIEW-AS alert-box.
     APPLY "entry" TO pc-prdd.job-no.
     return .
  end.
  IF AVAIL job AND job.opened EQ NO AND oeDateAuto-log THEN DO:
      MESSAGE "Job " STRING(job.job-no) " is currently closed. You must re-open the job to add data collection data." VIEW-AS ALERT-BOX .
      APPLY "entry" TO pc-prdd.job-no.
      return .
  END.
 END. /* do */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-new-record THEN DO: 
    RUN notify ('row-available').
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-target",OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN dispatch IN WIDGET-HANDLE(char-hdl) ("open-query").
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"autoadd-target",OUTPUT char-hdl).
    RUN auto-add IN WIDGET-HANDLE(char-hdl).
  END.

  ELSE DO:
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
    RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(pc-prdd)).
  END.

  li-help-job = 0.

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
  {src/adm/template/snd-list.i "pc-prdd"}

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

