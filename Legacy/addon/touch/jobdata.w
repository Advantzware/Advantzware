&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: addon/touch/jobdata.w

  Description: from SMART.W - Template for basic SmartObject

  Author: Ron Stark
  Created: 4.18.2000

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

&SCOPED-DEFINE PageNo 14
{touch/touchdef.i}
{custom/shftdefs.i}

/* internal procedures */
{custom/shftproc.i}
{custom/emprate.i}
{methods/defines/globdefs.i}

DEF VAR lv-timer AS INT NO-UNDO. /* clock timer */
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE vTsampmWarn AS CHAR NO-UNDO.

{sys/inc/var.i NEW SHARED}

ASSIGN cocode = g_company
       locode = g_loc.

DO TRANSACTION:
   {sys/inc/tspostfg.i}
   {sys/inc/fgrecpt.i}
   {sys/inc/tsqty.i}
   {sys/inc/tsfinish.i}
   {sys/inc/tsdocksec.i}
   {sys/inc/tscomplete.i}
   {sys/inc/tstimeb.i}
   {sys/inc/tsendwash.i}
   {sys/inc/tskey.i}
END.

RUN sys/ref/nk1look.p (INPUT cocode, "TSAMPMWarn", "L" /* Logical */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT vTsampmWarn, OUTPUT lRecFound).

DEF VAR v-time-clock-off AS LOG  NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE tt-comp NO-UNDO 
    FIELD i-no AS CHARACTER 
    FIELD rcv-qty AS INTEGER 
    FIELD est-no AS CHARACTER 
    FIELD form-no AS INTEGER 
    FIELD set-qty AS INTEGER 
    .
DEFINE TEMP-TABLE tt-mach NO-UNDO
    FIELD machine AS CHARACTER 
        INDEX machine machine
        .
DEFINE TEMP-TABLE ttTrans NO-UNDO
    FIELD chargeCode AS CHARACTER
    FIELD shift      AS CHARACTER
    FIELD startDate  AS DATE
    FIELD startTime  AS INTEGER
    FIELD endDate    AS DATE
    FIELD endTime    AS INTEGER
    FIELD totalTime  AS INTEGER
    FIELD pct        AS DECIMAL
    FIELD machemp    AS LOGICAL
    FIELD loginTime  AS INTEGER 
    FIELD logoutTime AS INTEGER 
    FIELD done       AS LOGICAL
    FIELD runQty     AS INTEGER
    FIELD wasteQty   AS INTEGER
    FIELD completed  AS LOGICAL 
    FIELD rRowID     AS ROWID 
        INDEX ttTrans IS PRIMARY
            startDate startTime
            .
DEFINE BUFFER bttTrans  FOR ttTrans.
DEFINE BUFFER bMachTran FOR machtran.

/*{methods/defines/hndldefs.i}            */
{methods/prgsecd3.i "p-tchupd."}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_Hour Btn_Minute Btn_AMPM ~
Btn_Quantity Btn_Waste Btn_complete 
&Scoped-Define DISPLAYED-OBJECTS time-hour time-minute run-qty waste-qty ~
v-completed timerStatus 

/* Custom List Definitions                                              */
/* JOB-DATA-FIELDS,JOB-DATA-BUTTONS,List-3,List-4,List-5,List-6         */
&Scoped-define JOB-DATA-FIELDS time-hour time-minute run-qty waste-qty ~
v-completed 
&Scoped-define JOB-DATA-BUTTONS Btn_Hour Btn_Minute Btn_AMPM Btn_Quantity ~
Btn_Waste Btn_complete 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fTotalTime s-object
FUNCTION fTotalTime RETURNS INTEGER 
  (ipdtStartDate AS DATE,
    ipdtEndDate AS DATE,
    ipiStartTime AS INTEGER,
    ipiEndTime AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNumForms s-object 
FUNCTION getNumForms RETURNS INTEGER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTimerStatus s-object 
FUNCTION setTimerStatus RETURNS CHARACTER
  (ipStatus AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ftsDockSec s-object
FUNCTION ftsDockSec RETURNS LOGICAL 
  (ipiTime AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_AMPM 
     LABEL "AM" 
     SIZE 15 BY 1.67.

DEFINE BUTTON Btn_complete 
     LABEL "Complete?" 
     SIZE 16 BY 1.67 TOOLTIP "Complete?".

DEFINE BUTTON Btn_Hour 
     LABEL "Hour" 
     SIZE 16 BY 1.67 TOOLTIP "Hour".

DEFINE BUTTON Btn_Minute 
     LABEL "Minute" 
     SIZE 15 BY 1.67 TOOLTIP "Minute".

DEFINE BUTTON Btn_Quantity 
     LABEL "Quantity" 
     SIZE 16 BY 1.67 TOOLTIP "Quantity".

DEFINE BUTTON Btn_Waste 
     LABEL "Waste" 
     SIZE 16 BY 1.67 TOOLTIP "Waste".

DEFINE VARIABLE run-qty AS DECIMAL FORMAT "->>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE time-hour AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE time-minute AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE timerStatus AS CHARACTER FORMAT "X(256)":U INITIAL "Auto Timer On" 
      VIEW-AS TEXT 
     SIZE 19 BY .62 NO-UNDO.

DEFINE VARIABLE v-completed AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE waste-qty AS DECIMAL FORMAT "->>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 12.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_Hour AT ROW 1.71 COL 22
     Btn_Minute AT ROW 1.71 COL 48
     Btn_AMPM AT ROW 1.71 COL 73
     time-hour AT ROW 1.95 COL 37 COLON-ALIGNED NO-LABEL
     time-minute AT ROW 1.95 COL 62 COLON-ALIGNED NO-LABEL
     Btn_Quantity AT ROW 4.1 COL 22
     run-qty AT ROW 4.33 COL 37 COLON-ALIGNED NO-LABEL
     Btn_Waste AT ROW 6.24 COL 22
     waste-qty AT ROW 6.48 COL 37 COLON-ALIGNED NO-LABEL
     Btn_complete AT ROW 8.38 COL 22
     v-completed AT ROW 8.62 COL 37 COLON-ALIGNED NO-LABEL
     timerStatus AT ROW 2.19 COL 89 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
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
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 12.95
         WIDTH              = 124.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON Btn_AMPM IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR BUTTON Btn_complete IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_complete:PRIVATE-DATA IN FRAME F-Main     = 
                "Complete".

/* SETTINGS FOR BUTTON Btn_Hour IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Hour:PRIVATE-DATA IN FRAME F-Main     = 
                "Hour".

/* SETTINGS FOR BUTTON Btn_Minute IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Minute:PRIVATE-DATA IN FRAME F-Main     = 
                "Minute".

/* SETTINGS FOR BUTTON Btn_Quantity IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Quantity:PRIVATE-DATA IN FRAME F-Main     = 
                "Quantity".

/* SETTINGS FOR BUTTON Btn_Waste IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Waste:PRIVATE-DATA IN FRAME F-Main     = 
                "Waste".

/* SETTINGS FOR FILL-IN run-qty IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN time-hour IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN time-minute IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN timerStatus IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-completed IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN waste-qty IN FRAME F-Main
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

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME F-Main:HANDLE
       ROW             = 5.05
       COLUMN          = 103
       HEIGHT          = 4.76
       WIDTH           = 20
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(run-qty:HANDLE IN FRAME F-Main).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_AMPM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AMPM s-object
ON CHOOSE OF Btn_AMPM IN FRAME F-Main /* AM */
DO:
    IF  vTsampmWarn EQ "YES" THEN DO:
        MESSAGE "Are you sure you want to change the AM/PM value?"
                     VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
        IF ll-ans THEN DO:
            ASSIGN
                v-time-clock-off = YES
                timerStatus:SCREEN-VALUE = setTimerStatus(v-time-clock-off)
                SELF:LABEL = IF SELF:LABEL = 'AM' THEN 'PM' ELSE 'AM'.
        END.
    END.
    ELSE DO:
        ASSIGN
            v-time-clock-off = YES
            timerStatus:SCREEN-VALUE = setTimerStatus(v-time-clock-off)
            SELF:LABEL = IF SELF:LABEL = 'AM' THEN 'PM' ELSE 'AM'.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_complete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_complete s-object
ON CHOOSE OF Btn_complete IN FRAME F-Main /* Complete? */
DO:
  /*RUN Reset_Field_Colors.
  h_field = v-completed:HANDLE.
  RUN Set_Field_Colors.
  */
    ASSIGN
      v-completed
      v-completed = NOT v-completed.
    DISPLAY v-completed WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Hour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Hour s-object
ON CHOOSE OF Btn_Hour IN FRAME F-Main /* Hour */
DO:
  

  ASSIGN
    v-time-clock-off = YES
    timerStatus:SCREEN-VALUE = setTimerStatus(v-time-clock-off).
  RUN Reset_Field_Colors.
  h_field = time-hour:HANDLE.
  RUN Set_Field_Colors.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Minute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Minute s-object
ON CHOOSE OF Btn_Minute IN FRAME F-Main /* Minute */
DO:
  

  ASSIGN
    v-time-clock-off = YES
    timerStatus:SCREEN-VALUE = setTimerStatus(v-time-clock-off).
  RUN Reset_Field_Colors.
  h_field = time-minute:HANDLE.
  RUN Set_Field_Colors.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Quantity
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Quantity s-object
ON CHOOSE OF Btn_Quantity IN FRAME F-Main /* Quantity */
DO:
  RUN Reset_Field_Colors.
  h_field = run-qty:HANDLE.
  RUN Set_Field_Colors.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Waste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Waste s-object
ON CHOOSE OF Btn_Waste IN FRAME F-Main /* Waste */
DO:
  RUN Reset_Field_Colors.
  h_field = waste-qty:HANDLE.
  RUN Set_Field_Colors.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame s-object OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
    IF timerStatus:SCREEN-VALUE IN FRAME {&FRAME-NAME} MATCHES "*on*" THEN DO:
        ASSIGN
            lv-timer = lv-timer + 1
            time-hour:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTR(STRING(lv-timer,'HH:MM AM'),1,2)
            time-minute:SCREEN-VALUE = SUBSTR(STRING(lv-timer,'HH:MM AM'),4,2)
            .
        IF time-hour:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "12" THEN
        Btn_AMPM:LABEL = SUBSTR(STRING(lv-timer,'HH:MM AM'),7,2).
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{touch/pCreateINIObjects.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-tsqty s-object 
PROCEDURE check-tsqty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-machtran FOR machtran.
DEF VAR v-total-sheet-qty AS INT NO-UNDO.
DEF VAR v-on AS INT NO-UNDO.
DEF VAR v-up AS INT NO-UNDO.
DEF VAR v-out AS INT NO-UNDO.
DEF VAR v-runqty AS INT NO-UNDO.
DEF VAR v-wasteqty AS INT NO-UNDO.

IF NOT v-tsqty-log THEN RETURN.

{methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
{methods/run_link.i "CONTAINER" "Get_Value" "('machine_code',OUTPUT machine_code)"}
{methods/run_link.i "CONTAINER" "Get_Value" "('job_number',OUTPUT job_number)"}
{methods/run_link.i "CONTAINER" "Get_Value" "('job_sub',OUTPUT job_sub)"}
{methods/run_link.i "CONTAINER" "Get_Value" "('form_number',OUTPUT form_number)"}
{methods/run_link.i "CONTAINER" "Get_Value" "('blank_number',OUTPUT blank_number)"}
{methods/run_link.i "CONTAINER" "Get_Value" "('pass_sequence',OUTPUT pass_sequence)"}
{methods/run_link.i "CONTAINER" "Get_Value" "('charge_code',OUTPUT charge_code)"}    

/*
FOR EACH bf-machtran NO-LOCK WHERE bf-machtran.company = company_code AND
                           bf-machtran.machine = machine_code AND
                           bf-machtran.job_number = job_number AND
                           bf-machtran.job_sub = INTEGER(job_sub) AND
                           bf-machtran.form_number = INTEGER(form_number) AND
                           bf-machtran.blank_number = INTEGER(blank_number) AND
                           bf-machtran.pass_sequence = INTEGER(pass_sequence):        
    v-total-sheet-qty = v-total-sheet-qty + bf-machtran.RUN_qty + bf-machtran.waste_qty.
END.
*/
FIND FIRST job-mch WHERE job-mch.company = company_code AND
                         job-mch.job-no = job_number AND
                         job-mch.job-no2 = INTEGER(job_sub) AND
                         job-mch.frm = INTEGER(form_number) AND
                         job-mch.m-code = machine_code
    USE-INDEX line-idx NO-LOCK NO-ERROR.
FIND PREV job-mch USE-INDEX line-idx NO-LOCK 
    WHERE job-mch.company = company_code AND
                         job-mch.job-no = job_number AND
                         job-mch.job-no2 = INTEGER(job_sub) NO-ERROR.
IF AVAIL job-mch THEN DO:
   FOR EACH bf-machtran NO-LOCK WHERE bf-machtran.company = company_code AND
                           bf-machtran.machine = job-mch.m-code AND
                           bf-machtran.job_number = job_number AND
                           bf-machtran.job_sub = INTEGER(job_sub) AND
                           bf-machtran.form_number = INTEGER(form_number) 
                           /*bf-machtran.blank_number = INTEGER(blank_number) AND
                           bf-machtran.pass_sequence = INTEGER(pass_sequence)*/:        
      ASSIGN v-on = 1
             v-up = 1
             v-out = 1.

      FIND FIRST mach WHERE mach.company = job-mch.company
                        AND mach.m-code = job-mch.m-code NO-LOCK NO-ERROR.
      FIND FIRST job WHERE job.company = company_code
                       AND job.job-no = job-mch.job-no
                       AND job.job-no2 = job-mch.job-no2 NO-LOCK NO-ERROR.
      IF AVAIL job THEN
         FIND FIRST est WHERE est.company EQ company_code
                          AND est.est-no  EQ job.est-no
                          NO-LOCK NO-ERROR.
      IF AVAIL job THEN
         FIND FIRST job-hdr WHERE job-hdr.company  EQ company_code
                              AND job-hdr.job      EQ job.job
                              AND job-hdr.job-no   EQ job.job-no
                              AND job-hdr.job-no2  EQ job.job-no2
                              AND job-hdr.frm = job-mch.frm
                              NO-LOCK NO-ERROR.
      IF NOT AVAIL job-hdr THEN
         FIND FIRST job-hdr WHERE job-hdr.company  EQ company_code
                              AND job-hdr.job      EQ job.job
                              AND job-hdr.job-no   EQ job.job-no
                              AND job-hdr.job-no2  EQ job.job-no2 NO-LOCK NO-ERROR.
      IF AVAIL est THEN DO:
         FIND FIRST eb WHERE eb.company   EQ est.company
                       AND eb.est-no    EQ est.est-no
                       AND eb.form-no   EQ job-mch.frm
                       AND (eb.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0)
                       NO-LOCK NO-ERROR.
         IF AVAIL eb THEN v-up = eb.num-up.
         FIND FIRST ef WHERE ef.company EQ est.company
                       AND ef.est-no  EQ est.est-no
                       AND ef.form-no EQ job-mch.frm
                       NO-LOCK NO-ERROR.
         IF AVAIL ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-out).
      END.
      IF job-hdr.n-on NE 0 THEN v-up = job-hdr.n-on.

      IF AVAIL est AND AVAIL mach AND index("APB",mach.p-type) LE 0 THEN DO:
        v-on = v-up * v-out.
        FIND FIRST est-op
            WHERE est-op.company EQ est.company
              AND est-op.est-no  EQ est.est-no
              AND est-op.s-num   EQ job-mch.frm
              AND (est-op.b-num  EQ job-mch.blank-no OR
                   job-mch.blank-no EQ 0)
              AND est-op.m-code  EQ job-mch.m-code
              AND est-op.op-pass EQ job-mch.pass
              AND est-op.dept    EQ job-mch.dept
              AND est-op.line    LT 500
            NO-LOCK NO-ERROR.

        IF AVAIL est-op THEN
          RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-out).

        ELSE v-out = 1.

        v-on = v-on / v-out.
      END.

       v-total-sheet-qty = v-total-sheet-qty + 
                           (bf-machtran.RUN_qty * v-on) 
                           /* + (bf-machtran.waste_qty * v-on)*/.
   END.

/* get qty for the machine*/
ASSIGN v-runqty = 0
       v-wasteqty = 0.
FOR EACH bf-machtran NO-LOCK WHERE bf-machtran.company = company_code AND
                           bf-machtran.machine = machine_code AND
                           bf-machtran.job_number = job_number AND
                           bf-machtran.job_sub = INTEGER(job_sub) AND
                           bf-machtran.form_number = INTEGER(form_number) :
    FIND FIRST job-mch WHERE job-mch.company = company_code AND
                         job-mch.job-no = job_number AND
                         job-mch.job-no2 = INTEGER(job_sub) AND
                         job-mch.m-code = machine_code AND
                         job-mch.frm = INTEGER(form_number) AND
                         (job-mch.blank-no = INTEGER(blank_number) OR job-mch.blank-no = 0) AND
                         job-mch.pass = INT(pass_sequence)
         USE-INDEX line-idx NO-LOCK NO-ERROR.

      ASSIGN v-on = 1
            v-up = 1
            v-out = 1.

      FIND FIRST mach WHERE mach.company = company_code
                        AND mach.m-code = machine_code NO-LOCK NO-ERROR.
      FIND FIRST job WHERE job.company = company_code
                       AND job.job-no = job_number
                       AND job.job-no2 = int(job_sub) NO-LOCK NO-ERROR.
      IF AVAIL job THEN
         FIND FIRST est WHERE est.company EQ company_code
                          AND est.est-no  EQ job.est-no
                          NO-LOCK NO-ERROR.
      IF AVAIL job THEN
         FIND FIRST job-hdr WHERE job-hdr.company  EQ company_code
                              AND job-hdr.job      EQ job.job
                              AND job-hdr.job-no   EQ job.job-no
                              AND job-hdr.job-no2  EQ job.job-no2
                              AND job-hdr.frm = int(form_number)
                              NO-LOCK NO-ERROR.   
      IF NOT AVAIL job-hdr THEN
         FIND FIRST job-hdr WHERE job-hdr.company  EQ company_code
                              AND job-hdr.job      EQ job.job
                              AND job-hdr.job-no   EQ job.job-no
                              AND job-hdr.job-no2  EQ job.job-no2
                              NO-LOCK NO-ERROR.
      IF AVAIL est THEN DO:
         FIND FIRST eb WHERE eb.company   EQ est.company
                       AND eb.est-no    EQ est.est-no
                       AND eb.form-no   EQ job-mch.frm
                       AND (eb.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0)
                       NO-LOCK NO-ERROR.
         IF AVAIL eb THEN v-up = eb.num-up.
         FIND FIRST ef WHERE ef.company EQ est.company
                       AND ef.est-no  EQ est.est-no
                       AND ef.form-no EQ job-mch.frm
                       NO-LOCK NO-ERROR.
         IF AVAIL ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-out).
      END.
      IF job-hdr.n-on NE 0 THEN v-up = job-hdr.n-on.

      IF AVAIL est AND AVAIL mach AND index("APB",mach.p-type) LE 0 THEN DO:
        v-on = v-up * v-out.
        FIND FIRST est-op
            WHERE est-op.company EQ est.company
              AND est-op.est-no  EQ est.est-no
              AND est-op.s-num   EQ int(form_number)
              AND (est-op.b-num  EQ int(blank_number) OR int(blank_number) EQ 0)
              AND est-op.m-code  EQ machine_code
              AND est-op.op-pass EQ INT(pass_sequence)
              AND est-op.dept    EQ job-mch.dept
              AND est-op.line    LT 500
            NO-LOCK NO-ERROR.
        IF AVAIL est-op THEN RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-out).
        ELSE v-out = 1.

        v-on = v-on / v-out.
      END.

      ASSIGN
         v-runqty = v-runqty + bf-machtran.RUN_qty
         v-wasteqty = v-wasteqty + bf-machtran.waste_qty.
END.
ASSIGN v-runqty = v-runqty + (RUN-qty * v-on)
       v-wasteqty = v-wasteqty + (waste-qty * v-on).

IF (v-runqty + v-wasteqty) > v-total-sheet-qty THEN DO:

   DEF VAR ll-ans AS LOG NO-UNDO.

   RUN addon/touch/d-valqty.w (OUTPUT ll-ans).
   IF ll-ans THEN DO:  /* correct entries*/
      APPLY "entry" TO run-qty IN FRAME {&FRAME-NAME}.
      RETURN ERROR.
   END.
END.

END. /* prev job-mch avail*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-job s-object 
PROCEDURE close-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM ip-company AS cha NO-UNDO.
    DEF INPUT PARAM ip-job_number AS cha NO-UNDO.
    DEF INPUT PARAM ip-job_sub AS cha NO-UNDO.
    DEF VAR v-fin-qty AS INT NO-UNDO.
    DEF VAR close_date AS DATE NO-UNDO.

    FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ ip-company
                                 AND sys-ctrl.name    EQ "CLOSEJOB" NO-ERROR.
    IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "TS" THEN 
       MESSAGE "Do you want to close job " TRIM(ip-job_number) + "-" + ip-job_sub
                     VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
    IF ll-ans THEN DO:

       close_date = TODAY.
       FOR EACH job WHERE job.company EQ ip-company
                      AND job.job-no  EQ ip-job_number
                      AND job.job-no2 EQ INT(ip-job_sub)
                      AND job.opened  EQ YES
                    USE-INDEX stat-idx,
           EACH job-hdr WHERE job-hdr.company EQ job.company
                          AND job-hdr.job     EQ job.job
                          AND job-hdr.job-no  EQ job.job-no
                          AND job-hdr.job-no2 EQ job.job-no2 USE-INDEX job NO-LOCK:

           {jc/job-clos.i}
           FIND CURRENT reftable NO-LOCK NO-ERROR.
       END.
       MESSAGE "Job " TRIM(ip-job_number) + "-" + ip-job_sub " is closed." VIEW-AS ALERT-BOX.
       SESSION:SET-WAIT-STATE("").
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE completeJobMch s-object 
PROCEDURE completeJobMch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       move this code here b/c Job_Data_Collection exceed 63K action code
               segment size
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMCode   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo2  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipFrm     AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipBlankNo AS INTEGER   NO-UNDO.

  FIND FIRST job-mch EXCLUSIVE-LOCK
       WHERE job-mch.company  EQ ipCompany
         AND job-mch.m-code   EQ ipMCode
         AND job-mch.job-no   EQ ipJobNo
         AND job-mch.job-no2  EQ ipJobNo2
         AND job-mch.frm      EQ ipFrm
         AND job-mch.blank-no EQ ipBlankNo
       NO-ERROR.
  IF AVAILABLE job-mch THEN DO:
     ASSIGN
        job-mch.run-complete = YES
        job-mch.mr-complete  = YES
        .
     FIND CURRENT job-mch NO-LOCK.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load s-object  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "jobdata.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN DISPATCH IN THIS-PROCEDURE("initialize-controls":U) NO-ERROR.
END.
ELSE MESSAGE "jobdata.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init_Job s-object 
PROCEDURE Init_Job :
/*------------------------------------------------------------------------------
  Purpose:     Initialize Job Data Entry Fields
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/run_link.i "CONTAINER" "Get_Value" "('job_sequence',OUTPUT job_sequence)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('charge_code',OUTPUT charge_code)"}
  
  ASSIGN
     lv-timer = TIME
     chCtrlFrame:PSTimer:Interval = 1000. /* 1 SECOND, 2000- 2 SECOND */
  
  FIND job-code WHERE job-code.code = charge_code NO-LOCK NO-ERROR.
  DO WITH FRAME {&FRAME-NAME}:
    ENABLE {&JOB-DATA-BUTTONS}.
    IF job_sequence BEGINS 'END' THEN DO:
        RUN pSetSensitive ("Back",NO).
        RUN pSetSensitive ("SetTiDO",NO).
    END.
    FIND LAST jobseq NO-LOCK NO-ERROR.
    ASSIGN
      Btn_AMPM:LABEL = SUBSTR(STRING(TIME,'HH:MM AM'),7,2)
      time-hour:SCREEN-VALUE = SUBSTR(STRING(TIME,'HH:MM AM'),1,2)
      time-minute:SCREEN-VALUE = SUBSTR(STRING(TIME,'HH:MM AM'),4,2)
      run-qty:HIDDEN = FALSE
      run-qty:SCREEN-VALUE = ''
      waste-qty:SCREEN-VALUE = ''
      waste-qty:HIDDEN = FALSE
      v-completed:HIDDEN = FALSE
      v-completed:SCREEN-VALUE =  IF (v-tsfinish-char-val = "Last Machine" AND AVAILABLE jobseq AND jobseq.charge_code <> charge_code) OR
         v-tsfinish-char-val = "NO" OR (tsendwash-log EQ YES AND job_sequence BEGINS "END WASH UP") THEN "NO" ELSE "YES".

    IF job_sequence BEGINS 'START' OR
       NOT CAN-FIND(jobseq WHERE jobseq.charge_code = charge_code) THEN
    DO: 
      ASSIGN
        Btn_Quantity:HIDDEN = TRUE
        run-qty:HIDDEN = TRUE
        v-completed:SCREEN-VALUE = "No" 
        .
      APPLY 'CHOOSE' TO Btn_Hour.
    END.
    ELSE
    IF AVAILABLE job-code AND job-code.cat = 'MR' THEN
    DO:
      ASSIGN
        Btn_Quantity:HIDDEN = TRUE
        run-qty:HIDDEN = TRUE.
      APPLY 'CHOOSE' TO Btn_Waste.
    END.
    ELSE
    APPLY 'CHOOSE' TO Btn_Quantity.
  END.

  ASSIGN
    v-time-clock-off = NO
    timerStatus:SCREEN-VALUE = setTimerStatus(NO).

  IF NOT v-can-update AND tstimeb-log = NO THEN DO:
      ASSIGN btn_hour:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             btn_minute:SENSITIVE = NO
             btn_ampm:SENSITIVE = NO
             .              
      RUN pSetSensitive ("ResetTime",NO).
      RUN pSetSensitive ("SetTime",NO).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE job-start s-object 
PROCEDURE job-start :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER v-today AS DATE NO-UNDO.
    
    DEFINE VARIABLE ampm      AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTimeHour AS INTEGER NO-UNDO.
  
    DO WITH FRAME {&FRAME-NAME}:
        {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
        {methods/run_link.i "CONTAINER" "Get_Value" "('machine_code',OUTPUT machine_code)"}
        {methods/run_link.i "CONTAINER" "Get_Value" "('job_number',OUTPUT job_number)"}
        {methods/run_link.i "CONTAINER" "Get_Value" "('job_sub',OUTPUT job_sub)"}
        {methods/run_link.i "CONTAINER" "Get_Value" "('form_number',OUTPUT form_number)"}
        {methods/run_link.i "CONTAINER" "Get_Value" "('blank_number',OUTPUT blank_number)"}
        {methods/run_link.i "CONTAINER" "Get_Value" "('pass_sequence',OUTPUT pass_sequence)"}
        {methods/run_link.i "CONTAINER" "Get_Value" "('charge_code',OUTPUT charge_code)"}    

        CREATE machtran.
        ASSIGN
            ampm                   = IF Btn_AMPM:LABEL EQ 'PM' AND time-hour NE 12 THEN 43200 ELSE 0
            iTimeHour              = IF Btn_AMPM:LABEL EQ 'AM' AND time-hour EQ 12 THEN 0 ELSE time-hour
            machtran.company       = company_code
            machtran.machine       = machine_code
            machtran.job_number    = job_number
            machtran.job_sub       = INTEGER(job_sub)
            machtran.form_number   = INTEGER(form_number)
            machtran.blank_number  = INTEGER(blank_number)
            machtran.pass_sequence = INTEGER(pass_sequence)
            machtran.start_date    = v-today
            machtran.start_time    = iTimeHour * 3600 + time-minute * 60 + ampm
            machtran.jobseq        = IF AVAILABLE jobseq THEN jobseq.jobseq ELSE 0
            machtran.charge_code   = charge_code
            machtran.posted        = NO        
            machtran-rowid         = ROWID(machtran)
            .      
        RUN Get-Shift (company_code,machine_code,machtran.start_time,job_sequence,OUTPUT machtran.shift).
        FIND FIRST shift_break NO-LOCK USE-INDEX shift
             WHERE shift_break.company    EQ machtran.company
               AND shift_break.shift      EQ machtran.shift
               AND shift_break.start_time LE machtran.start_time
               AND shift_break.end_time   GE machtran.start_time
             NO-ERROR.
        IF AVAILABLE shift_break THEN 
            machtran.start_time = shift_break.end_time + 1.
  
        {methods/run_link.i "CONTAINER" "Set_MachTran_Rowid" "(machtran-rowid)"}
        /* get active employees logged into this machine */
        FOR EACH emplogin NO-LOCK
            WHERE emplogin.company    EQ company_code
              AND (emplogin.machine   EQ machine_code
               OR LOOKUP(emplogin.machine,machine_list) GT 0)
              AND emplogin.end_date   EQ ?
              AND emplogin.end_time   EQ 0
              AND emplogin.total_time EQ 0,
            FIRST employee OF emplogin NO-LOCK:

            IF NOT CAN-FIND(FIRST machemp
                            WHERE machemp.table_rec_key = machtran.rec_key
                              AND machemp.employee = emplogin.employee
                              AND machemp.start_date = machtran.start_date
                              AND machemp.start_time = machtran.start_time) THEN DO:
                CREATE machemp.
                ASSIGN
                    machemp.table_rec_key = machtran.rec_key
                    machemp.employee      = emplogin.employee
                    machemp.start_date    = machtran.start_date
                    machemp.start_time    = machtran.start_time
                    machemp.shift         = machtran.shift
                    machemp.ratetype      = 'Standard'
                    machemp.rate_usage    = employee.rate_usage
                    .
                RUN Employee-Rate (
                    company_code,
                    machemp.employee,
                    machemp.shift,
                    machine_code,
                    machemp.rate_usage,
                    machemp.ratetype,
                    OUTPUT machemp.rate
                    ).
                RELEASE machemp.
            END. /* not can-find */
        END. /* each emplogin */
    END. /* with frame */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Job_Data_Collection s-object 
PROCEDURE Job_Data_Collection :
/*------------------------------------------------------------------------------
  Purpose:     create/update machine transactions
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &Scoped-define log1 no
    &Scoped-define log2 no
    &Scoped-define log3 no
    
    DEFINE INPUT PARAMETER ipdtToday AS DATE NO-UNDO.

    DEFINE VARIABLE iAMPM         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTimeHour     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTimeMinute   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iStopTime     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dtToday       AS DATE      NO-UNDO.    
    DEFINE VARIABLE dtDate        AS DATE      NO-UNDO.
    DEFINE VARIABLE dtStartDate   AS DATE      NO-UNDO.
    DEFINE VARIABLE dtEndDate     AS DATE      NO-UNDO.
    DEFINE VARIABLE iTime         AS INTEGER   NO-UNDO EXTENT 2.
    DEFINE VARIABLE iStartTime    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iEndTime      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSTime        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iETime        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cShift        AS CHARACTER NO-UNDO EXTENT 2.
    DEFINE VARIABLE cMissingShift AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompany      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMachine      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cType         AS CHARACTER NO-UNDO EXTENT 2 INIT ["Start","End"].
    DEFINE VARIABLE cChargeCode   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iRunQty       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iRQty         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iWasteQty     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iWQty         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iJobQty       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTotalTime    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lCompleted    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lLastMachine  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMachines     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rRecKey       AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bMach FOR mach.

    DO WITH FRAME {&FRAME-NAME}:
        {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
        {methods/run_link.i "CONTAINER" "Get_Value" "('machine_code',OUTPUT machine_code)"}
        {methods/run_link.i "CONTAINER" "Get_Value" "('job_number',OUTPUT job_number)"}
        {methods/run_link.i "CONTAINER" "Get_Value" "('job_sub',OUTPUT job_sub)"}
        {methods/run_link.i "CONTAINER" "Get_Value" "('form_number',OUTPUT form_number)"}
        {methods/run_link.i "CONTAINER" "Get_Value" "('blank_number',OUTPUT blank_number)"}
        {methods/run_link.i "CONTAINER" "Get_Value" "('pass_sequence',OUTPUT pass_sequence)"}
        {methods/run_link.i "CONTAINER" "Get_Value" "('charge_code',OUTPUT charge_code)"}
        
        FIND FIRST jobseq NO-LOCK
             WHERE jobseq.charge_code EQ charge_code
             NO-ERROR.
        ASSIGN
            {&JOB-DATA-FIELDS}
            dtToday   = ipdtToday
            iAMPM     = IF Btn_AMPM:LABEL EQ 'PM' AND time-hour NE 12 THEN 43200 ELSE 0
            iTimeHour = IF Btn_AMPM:LABEL EQ 'AM' AND time-hour EQ 12 THEN 0 ELSE time-hour
            iStopTime = iTimeHour * 3600 + time-minute * 60 + iAMPM
            .
        IF job_sequence BEGINS 'START' THEN
            RUN job-start(INPUT dtToday).
        ELSE DO: /* close out current operation */
            {methods/run_link.i "CONTAINER" "Get_MachTran_Rowid" "(OUTPUT machtran-rowid)"}
            FIND machtran EXCLUSIVE-LOCK WHERE ROWID(machtran) EQ machtran-rowid.
            IF CAN-FIND(FIRST sys-ctrl
                        WHERE sys-ctrl.company EQ cCompany
                          AND sys-ctrl.name    EQ "TSBREAKS"
                          AND sys-ctrl.log-fld EQ YES) THEN DO:
                FIND FIRST shift_break NO-LOCK
                     WHERE shift_break.company   EQ machtran.company
                       AND shift_break.shift      EQ machtran.shift
                       AND shift_break.start_time LE iStopTime
                       AND shift_break.end_time   GE iStopTime
                     NO-ERROR.
                IF AVAILABLE shift_break THEN 
                iStopTime= shift_break.start_time.                                      
            END. /* if can-find */
            IF ((machtran.start_date EQ dtToday AND 
                 machtran.start_time GT iStopTime) OR
                (machtran.start_date GT dtToday)) THEN DO:
                MESSAGE
                    "WARNING: End Time is Earlier then Start Time." SKIP(1)
                    "Please ReEnter End Time." SKIP(1)
                    "Start Time:" STRING(machtran.start_date, "99/99/99") 
                                  STRING(machtran.start_time, "HH:MM AM") SKIP
                    "End Time:"   STRING(dtToday, "99/99/99")
                                  STRING(iStopTime, "HH:MM AM")
                VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END. /* if date/time check */
            
            EMPTY TEMP-TABLE ttTrans.

            IF LOOKUP(charge_code,"MR,RUN")       GT 0 AND
              (LOOKUP(machine_code,tspostfg-char) GT 0 OR
               CAN-FIND(FIRST mach
                        WHERE mach.company EQ company_code
                          AND mach.m-code  EQ machine_code
                          AND LOOKUP(mach.p-type,"A,P") GT 0)) THEN                          
            RUN touch/chkrecpt.p (company_code,machine_code,job_number,job_sub,charge_code,run-qty) NO-ERROR.
            
            ASSIGN 
                dtStartDate = machtran.start_date
                dtEndDate   = dtToday
                iStartTime  = machtran.start_time
                iEndTime    = iStopTime
                cCompany    = machtran.company
                cMachine    = machtran.machine
                cChargeCode = machtran.charge_code
                iRunQty     = run-qty
                iWasteQty   = waste-qty
                lCompleted  = v-completed
                .
            DO dtDate = dtStartDate TO dtEndDate:
                ASSIGN
                    iTime[1]      = iStartTime
                    iTime[2]      = iEndTime
                    cMissingShift = ""
                    .
                IF dtDate NE dtStartDate THEN
                    iTime[1] = 0.
                IF dtDate NE dtEndDate THEN
                    iTime[2] = 86399.
            
                RUN Get-Shift (cCompany,cMachine,iTime[1],cType[1],OUTPUT cShift[1]).
                RUN Get-Shift (cCompany,cMachine,iTime[2],cType[2],OUTPUT cShift[2]).
            
                IF cShift[1] NE cShift[2] THEN DO:
                    RUN Shift-Data (cCompany,cMachine,cShift[1],OUTPUT iSTime,OUTPUT iETime).
                    IF ftsDockSec(iETime) THEN iETime = iEtime + 1.
                    
                    CREATE ttTrans.
                    ASSIGN
                        ttTrans.chargeCode = cChargeCode
                        ttTrans.shift      = cShift[1]
                        ttTrans.startDate  = dtDate
                        ttTrans.startTime  = iTime[1]
                        ttTrans.endDate    = dtDate
                        ttTrans.endTime    = iETime
                        ttTrans.totalTime  = fTotalTime(ttTrans.startDate,ttTrans.endDate,ttTrans.startTime,ttTrans.endTime)
                        ttTrans.machEmp    = YES
                        ttTrans.loginTime  = ttTrans.startTime
                        ttTrans.logoutTime = ttTrans.endTime
                        .
                    
                    RUN Shift-Data (cCompany,cMachine,cShift[2],OUTPUT iSTime,OUTPUT iETime).
                    IF ftsDockSec(iETime) THEN iETime = iEtime + 1.
                    
                    CREATE ttTrans.
                    ASSIGN
                        ttTrans.chargeCode = cChargeCode
                        ttTrans.shift      = cShift[2]
                        ttTrans.startDate  = dtDate
                        ttTrans.startTime  = iSTime
                        ttTrans.endDate    = dtDate
                        ttTrans.endTime    = iTime[2]
                        ttTrans.totalTime  = fTotalTime(ttTrans.startDate,ttTrans.endDate,ttTrans.startTime,ttTrans.endTime)
                        ttTrans.machEmp    = YES
                        ttTrans.loginTime  = ttTrans.startTime
                        ttTrans.logoutTime = ttTrans.endTime
                        .
                    RUN Missing-Shift(cCompany,cMachine,cShift[1],cShift[2],OUTPUT cMissingShift).
                    IF cMissingShift NE "" THEN DO:
                        RUN Shift-Data (cCompany,cMachine,cMissingShift,OUTPUT iSTime,OUTPUT iETime).
                        IF ftsDockSec(iETime) THEN iETime = iEtime + 1.                        
                        CREATE ttTrans.
                        ASSIGN
                            ttTrans.chargeCode = cChargeCode
                            ttTrans.shift      = cMissingShift
                            ttTrans.startDate  = dtDate
                            ttTrans.startTime  = iSTime
                            ttTrans.endDate    = dtDate
                            ttTrans.endTime    = iETime
                            ttTrans.totalTime  = fTotalTime(ttTrans.startDate,ttTrans.endDate,ttTrans.startTime,ttTrans.endTime)
                            ttTrans.machEmp    = YES
                            ttTrans.loginTime  = ttTrans.startTime
                            ttTrans.logoutTime = ttTrans.endTime
                            .
                    END. /* if missing shift */
                END. /* if span multi shifts */
                ELSE DO: /* single shift */
                    /* check if shift spans multiple shifts */
                    DO WHILE TRUE:
                        RUN Shift-Data (cCompany,cMachine,cShift[1],OUTPUT iSTime,OUTPUT iETime).
                        IF ftsDockSec(iETime) THEN iETime = iEtime + 1.
                        /* shift spans midnight, if before midnight then done */
                        IF iSTime GT iETime AND 86400 GE iTime[2] THEN LEAVE.
                        /* if shift end time ge end time then done */
                        IF iETime GE iTime[2] THEN LEAVE.            
                        CREATE ttTrans.
                        ASSIGN
                            ttTrans.chargeCode = cChargeCode
                            ttTrans.shift      = cShift[1]
                            ttTrans.startDate  = dtDate
                            ttTrans.startTime  = iTime[1]
                            ttTrans.endDate    = dtDate
                            ttTrans.endTime    = iETime
                            ttTrans.totalTime  = fTotalTime(ttTrans.startDate,ttTrans.endDate,ttTrans.startTime,ttTrans.endTime)
                            ttTrans.machEmp    = YES
                            ttTrans.loginTime  = ttTrans.startTime
                            ttTrans.logoutTime = ttTrans.endTime
                            /* set the start time to shift end time plus 1 second */
                            iTime[1]           = ttTrans.endTime + 1
                            .
                        /* check if crossing over midnight */
                        IF iTime[1] GE 86400 THEN iTime[1] = 0.
                        RUN Get-Shift (cCompany,cMachine,iTime[1],cType[1],OUTPUT cShift[1]).
                        /* if start and end shift the same, we've come full circle, done */
                        IF cShift[1] EQ cShift[2] THEN LEAVE.            
                    END. /* do while */
                    CREATE ttTrans.
                    ASSIGN
                        ttTrans.chargeCode = cChargeCode
                        ttTrans.shift      = cShift[1]
                        ttTrans.startDate  = dtDate
                        ttTrans.startTime  = iTime[1]
                        ttTrans.endDate    = dtDate
                        ttTrans.endTime    = iTime[2]
                        ttTrans.totalTime  = fTotalTime(ttTrans.startDate,ttTrans.endDate,ttTrans.startTime,ttTrans.endTime)
                        ttTrans.machEmp    = YES
                        ttTrans.loginTime  = ttTrans.startTime
                        ttTrans.logoutTime = ttTrans.endTime
                        .
                END. /* else */
            END. /* do dtDate */
            
            &if "{&log1}" EQ "yes" &then
            OUTPUT TO c:\tmp\jobdata1.txt.
            FOR EACH ttTrans:
                DISPLAY 
                    ttTrans.chargeCode
                    ttTrans.shift     
                    ttTrans.startDate 
                    STRING(ttTrans.startTime,"hh:mm:ss am") FORMAT "x(11)" LABEL "startTime"
                    ttTrans.endDate   
                    STRING(ttTrans.endTime,"hh:mm:ss am") FORMAT "x(11)" LABEL "endTime"   
                    STRING(ttTrans.totalTime,"hh:mm:ss am") FORMAT "x(11)" LABEL "totalTime"
                    ttTrans.pct       
                    ttTrans.machemp   
                    STRING(ttTrans.loginTime,"hh:mm:ss am") FORMAT "x(11)" LABEL "loginTime"  
                    STRING(ttTrans.logoutTime,"hh:mm:ss am") FORMAT "x(11)" LABEL "logoutTime"
                    ttTrans.done      
                    ttTrans.runQty    
                    ttTrans.wasteQty  
                    ttTrans.completed
                    STRING(ttTrans.rRowID) FORMAT "x(20)"
                        WITH STREAM-IO WIDTH 200.  
            END.
            OUTPUT CLOSE.
            OS-COMMAND NO-WAIT notepad.exe c:\tmp\jobdata1.txt.
            &endif
            
            /* create breaks if tsbreaks turned on */
            IF CAN-FIND(FIRST sys-ctrl
                        WHERE sys-ctrl.company EQ cCompany
                          AND sys-ctrl.name    EQ "TSBREAKS"
                          AND sys-ctrl.log-fld EQ YES) THEN DO:
                FOR EACH ttTrans
                    WHERE ttTrans.chargeCode EQ cChargeCode
                      AND ttTrans.done       EQ NO
                    :
                    FOR EACH shift_break NO-LOCK
                        WHERE shift_break.company      EQ cCompany
                          AND shift_break.shift        EQ ttTrans.shift
                          AND ((shift_break.start_time LE ttTrans.startTime
                          AND   shift_break.end_time   GT ttTrans.startTime)                                        
                           OR  (shift_break.start_time LT ttTrans.endTime
                          AND   shift_break.end_time   GE ttTrans.endTime)                                      
                           OR  (shift_break.start_time GE ttTrans.startTime
                          AND   shift_break.end_time   LE ttTrans.endTime))
                        :
                        CREATE bttTrans.
                        ASSIGN
                            bttTrans.chargeCode = shift_break.charge_code
                            bttTrans.shift      = shift_break.shift
                            bttTrans.startDate  = ttTrans.startDate
                            bttTrans.startTime  = shift_break.start_time
                            bttTrans.endDate    = ttTrans.endDate
                            bttTrans.endTime    = shift_break.end_time
                            bttTrans.totalTime  = fTotalTime(bttTrans.startDate,bttTrans.endDate,bttTrans.startTime,bttTrans.endTime)
                            bttTrans.machEmp    = YES
                            .
                        CREATE bttTrans.
                        BUFFER-COPY ttTrans EXCEPT machemp loginTime logoutTime TO bttTrans
                            ASSIGN
                                bttTrans.endTime   = shift_break.start_time
                                bttTrans.totalTime = fTotalTime(bttTrans.startDate,bttTrans.endDate,bttTrans.startTime,bttTrans.endTime)
                                bttTrans.done      = YES
                                bttTrans.machEmp   = YES
                                .
                        ASSIGN
                            ttTrans.startTime = shift_break.end_time
                            ttTrans.totalTime = fTotalTime(ttTrans.startDate,ttTrans.endDate,ttTrans.startTime,ttTrans.endTime)
                            .
                    END. /* each shift_break */
                END. /* each ttTrans */
            END. /* if tsbreaks */
            
            /* remove done tttrans that have zeroed out */
            FOR EACH ttTrans
                WHERE ttTrans.done      EQ YES
                  AND ttTrans.startDate EQ ttTrans.endDate
                  AND ttTrans.startTime EQ ttTrans.endTime
                :
                DELETE ttTrans.
            END. /* each ttTrans */

            /* calculate total run time */
            FOR EACH ttTrans
                WHERE ttTrans.chargeCode EQ cChargeCode
                :
                iTotalTime = iTotalTime + ttTrans.totalTime.
            END. /* each ttTrans */
            
            /* set qty values based on pct of total time */
            FOR EACH ttTrans
                WHERE ttTrans.chargeCode EQ cChargeCode
                :
                ASSIGN
                    ttTrans.pct      = IF ttTrans.totalTime / iTotalTime EQ ? THEN 0
                                       ELSE ttTrans.totalTime / iTotalTime
                    ttTrans.runQty   = iRunQty * ttTrans.pct
                    ttTrans.wasteQty = iWasteQty * ttTrans.pct
                    iRQty            = iRQty + ttTrans.runQty
                    iWQty            = iWQty + ttTrans.wasteQty
                    .
            END. /* each ttTrans */
            
            /* calculate if any over/under after distributing qty values */
            ASSIGN
                iRqty = iRQty - iRunQty
                iWQty = iWQty - iWasteQty
                .
            /* adjust qty values in case of rounding errors */
            /* set run complete value on last transaction */
            FIND LAST ttTrans
                WHERE ttTrans.chargeCode EQ cChargeCode
                NO-ERROR.
            IF AVAILABLE ttTrans THEN
            ASSIGN
                ttTrans.runQty    = ttTrans.runQty + iRQty
                ttTrans.wasteQty  = ttTrans.wasteQty + iWQty
                ttTrans.completed = lCompleted
                .
            FIND FIRST ttTrans
                 WHERE ttTrans.chargeCode EQ cChargeCode
                 NO-ERROR.
            IF AVAILABLE ttTrans THEN 
            ttTrans.rRowID = machtran-rowid.

            FOR EACH ttTrans:
                /* no machtran record, create it */
                IF ttTrans.rRowID EQ ? THEN DO:
                    CREATE bMachTran.
                    BUFFER-COPY machtran EXCEPT rec_key TO bMachTran
                        ASSIGN
                            bMachTran.charge_code = ttTrans.chargeCode
                            bMachTran.shift       = ttTrans.shift
                            bMachTran.end_date    = ttTrans.endDate
                            bMachTran.end_time    = ttTrans.endTime
                            bMachTran.run_qty     = ttTrans.runQty
                            bMachTran.start_date  = ttTrans.startDate
                            bMachTran.start_time  = ttTrans.startTime
                            bMachTran.waste_qty   = ttTrans.wasteQty
                            bMachTran.total_time  = ttTrans.totalTime
                            bMachTran.completed   = ttTrans.completed
                            dtStartDate           = ttTrans.startDate
                            iStartTime            = ttTrans.startTime
                            dtEndDate             = ttTrans.endDate
                            iEndTime              = ttTrans.endTime
                            rRecKey               = bMachTran.rec_key
                            .
                END. /* if rrowid eq ? */
                ELSE /* update the already created start machtran */
                ASSIGN 
                    machtran.end_date   = ttTrans.endDate
                    machtran.end_time   = ttTrans.endTime
                    machtran.run_qty    = ttTrans.runQty
                    machtran.start_date = ttTrans.startDate
                    machtran.start_time = ttTrans.startTime
                    machtran.waste_qty  = ttTrans.wasteQty
                    machtran.total_time = ttTrans.totalTime
                    machtran.completed  = ttTrans.completed
                    dtStartDate         = ttTrans.startDate
                    iStartTime          = ttTrans.startTime
                    dtEndDate           = ttTrans.endDate
                    iEndTime            = ttTrans.endTime
                    rRecKey             = machtran.rec_key
                    .                
                /* create machaine employee transactions */
                IF ttTrans.machemp THEN DO:
                    FIND FIRST mach NO-LOCK
                         WHERE mach.company EQ cCompany
                           AND mach.m-code  EQ machine_code
                         NO-ERROR.
                    IF NOT AVAILABLE mach THEN NEXT.
                    IF mach.sch-m-code NE "" THEN
                    FOR EACH bMach NO-LOCK 
                        WHERE bMach.company    EQ mach.company
                          AND bMach.loc        EQ mach.loc
                          AND bMach.sch-m-code EQ mach.sch-m-code
                        :
                        cMachines = cMachines + bMach.m-code + ",".
                    END. /* if sch-m-code */
                    cMachines = TRIM(cMachines,",").
                    FOR EACH emplogin NO-LOCK
                        WHERE emplogin.company    EQ machtran.company
                          AND (emplogin.machine   EQ machtran.machine
                           OR LOOKUP(emplogin.machine,cMachines) GT 0)
                          AND emplogin.end_date   EQ ?
                          AND emplogin.end_time   EQ 0
                          AND emplogin.total_time EQ 0
                        :                        
                        IF STRING(YEAR(emplogin.start_date),"9999") +
                           STRING(MONTH(emplogin.start_date),"99")  +
                           STRING(DAY(emplogin.start_date),"99")    +
                           STRING(emplogin.start_time,"99999")     GT
                           STRING(YEAR(dtStartDate),"9999")         +
                           STRING(MONTH(dtStartDate),"99")          +
                           STRING(DAY(dtStartDate),"99")            +
                           STRING(iStartTime,"99999") THEN NEXT.                        
                        FIND FIRST machemp EXCLUSIVE-LOCK
                             WHERE machemp.table_rec_key EQ rRecKey
                               AND machemp.employee      EQ emplogin.employee
                               AND machemp.start_date    EQ dtStartDate
                               AND machemp.start_time    EQ iStartTime
                             NO-ERROR.
                        IF NOT AVAILABLE machemp THEN DO:
                            FIND FIRST employee NO-LOCK
                                 WHERE employee.company  EQ emplogin.company
                                   AND employee.employee EQ emplogin.employee
                                 NO-ERROR.                        
                            CREATE machemp.                            
                            ASSIGN
                                machemp.table_rec_key = rRecKey
                                machemp.employee      = emplogin.employee
                                machemp.start_date    = dtStartDate
                                machemp.start_time    = iStartTime
                                machemp.shift         = ttTrans.shift
                                machemp.ratetype      = "Standard"
                                machemp.rate_usage    = employee.rate_usage WHEN AVAILABLE(employee)
                                .
                            RUN Employee-Rate (
                                company_code,
                                machemp.employee,
                                machemp.shift,
                                machine_code,
                                machemp.rate_usage,
                                machemp.ratetype,
                                OUTPUT machemp.rate
                                ).                            
                        END. /* not avail */
                        ASSIGN
                            machemp.end_date   = dtEndDate
                            machemp.end_time   = iEndTime
                            machemp.total_time = fTotalTime(machemp.start_date,machemp.end_date,machemp.start_time,machemp.end_time)
                            .
                        RELEASE machemp.
                    END. /* each emplogin */
                END. /* if machemp */
            END. /* each tttrans */

            &if "{&log2}" EQ "yes" &then
            OUTPUT TO c:\tmp\jobdata2.txt.
            FOR EACH ttTrans:
                DISPLAY 
                    ttTrans.chargeCode
                    ttTrans.shift     
                    ttTrans.startDate 
                    STRING(ttTrans.startTime,"hh:mm:ss am") FORMAT "x(11)" LABEL "startTime"
                    ttTrans.endDate   
                    STRING(ttTrans.endTime,"hh:mm:ss am") FORMAT "x(11)" LABEL "endTime"   
                    STRING(ttTrans.totalTime,"hh:mm:ss am") FORMAT "x(11)" LABEL "totalTime"
                    ttTrans.pct       
                    ttTrans.machemp   
                    STRING(ttTrans.loginTime,"hh:mm:ss am") FORMAT "x(11)" LABEL "loginTime"  
                    STRING(ttTrans.logoutTime,"hh:mm:ss am") FORMAT "x(11)" LABEL "logoutTime"
                    ttTrans.done      
                    ttTrans.runQty    
                    ttTrans.wasteQty  
                    ttTrans.completed
                    STRING(ttTrans.rRowID) FORMAT "x(20)"
                        WITH STREAM-IO WIDTH 200.  
            END.
            OUTPUT CLOSE.
            OS-COMMAND NO-WAIT notepad.exe c:\tmp\jobdata2.txt.
            &endif
            
            &if "{&log3}" EQ "yes" &then
            OUTPUT TO c:\tmp\jobdata3.txt.
            FOR EACH bMachTran NO-LOCK
                WHERE bMachTran.company       EQ company_code
                  AND bMachTran.machine       EQ machine_code
                  AND bMachTran.job_number    EQ job_number
                  AND bMachTran.job_sub       EQ INTEGER(job_sub)
                  AND bMachTran.form_number   EQ INTEGER(form_number)
                  AND bMachTran.blank_number  EQ INTEGER(blank_number)
                  AND bMachTran.pass_sequence EQ INTEGER(pass_sequence)
                :        
                DISPLAY 
                    bMachTran.charge_Code
                    bMachTran.shift     
                    bMachTran.start_Date 
                    STRING(bMachTran.start_Time,"hh:mm:ss am") FORMAT "x(11)" LABEL "startTime"
                    bMachTran.end_Date   
                    STRING(bMachTran.end_Time,"hh:mm:ss am") FORMAT "x(11)" LABEL "endTime"   
                    STRING(bMachTran.total_Time,"hh:mm:ss am") FORMAT "x(11)" LABEL "totalTime"
                    bMachTran.run_Qty    
                    bMachTran.waste_Qty  
                    bMachTran.complete
                    STRING(RowID(bMachTran)) FORMAT "x(20)"
                        WITH STREAM-IO WIDTH 200.
            END. /* each bmachtran */
            OUTPUT CLOSE.
            OS-COMMAND NO-WAIT notepad.exe c:\tmp\jobdata3.txt.
            &endif

            /* logic that existed before rewrite above */
            RUN Job_Data_Collection_2 (dtToday).
        END. /* else */
        RUN updateRouting (
            company_code,
            machine_code,
            job_number,
            INTEGER(job_sub),
            INTEGER(form_number),
            INTEGER(blank_number)
            ).
        RELEASE machtran.
    END. /* frame {&frame-name} */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Job_Data_Collection_2 s-object
PROCEDURE Job_Data_Collection_2:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdtToday AS DATE NO-UNDO.
    
    DEFINE VARIABLE iJobQty      AS INTEGER NO-UNDO.
    DEFINE VARIABLE iRunQty      AS INTEGER NO-UNDO.
    DEFINE VARIABLE lCompleted   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lLastMachine AS LOGICAL NO-UNDO.
    
    FIND FIRST job-hdr NO-LOCK
         WHERE job-hdr.company EQ company_code
           AND job-hdr.job-no  EQ job_number
           AND job-hdr.job-no2 EQ INTEGER(job_sub)
        NO-ERROR.
    ASSIGN
        iJobQty    = IF AVAILABLE job-hdr THEN job-hdr.qty ELSE 0
        lCompleted = YES
        iRunQty    = 0
        .
    FOR EACH bMachTran NO-LOCK
        WHERE bMachTran.company       EQ company_code
          AND bMachTran.machine       EQ machine_code
          AND bMachTran.job_number    EQ job_number
          AND bMachTran.job_sub       EQ INTEGER(job_sub)
          AND bMachTran.form_number   EQ INTEGER(form_number)
          AND bMachTran.blank_number  EQ INTEGER(blank_number)
          AND bMachTran.pass_sequence EQ INTEGER(pass_sequence)
        :        
        iRunQty = iRunQty + bMachTran.RUN_qty.
    END. /* each bmachtran */
    IF charge_code EQ "RUN" THEN DO:
        IF fgrecpt-char EQ "TSPARTS" THEN
            RUN proc-tsparts (ipdtToday).
        ELSE DO:
            FIND LAST job-mch NO-LOCK
                WHERE job-mch.company EQ company_code
                  AND job-mch.job-no  EQ job_number
                  AND job-mch.job-no2 EQ INTEGER(job_sub)
                  AND job-mch.frm     EQ integer(form_number)                                 
                USE-INDEX line-idx NO-ERROR.
            IF AVAILABLE job-mch AND
                LOOKUP(job-mch.m-code,tspostfg-char) GT 0 AND
                tspostfg-int EQ 1 THEN DO:
                FIND PREV job-mch NO-LOCK
                    WHERE job-mch.company EQ company_code
                      AND job-mch.job-no  EQ job_number
                      AND job-mch.job-no2 EQ INTEGER(job_sub)
                      AND job-mch.frm     EQ INTEGER(form_number)                                 
                    USE-INDEX line-idx NO-ERROR.
                IF AVAILABLE job-mch AND job-mch.m-code EQ machine_code THEN DO:
                    IF tspostfg-log THEN RUN proc-form-cmplt (ipdtToday).
                END. /* avail job-mch */
            END. /* avail / lookup */

            /* form closing procedure only for last machine */
            FIND LAST job-mch NO-LOCK
                WHERE job-mch.company EQ company_code
                  AND job-mch.job-no  EQ job_number
                  AND job-mch.job-no2 EQ INTEGER(job_sub)
                  AND job-mch.frm     EQ integer(form_number)                                 
                USE-INDEX line-idx NO-ERROR.
            IF AVAILABLE job-mch AND job-mch.m-code EQ machine_code THEN DO:
                IF LOOKUP(machine_code,tspostfg-char) GT 0 AND tspostfg-int EQ 1 THEN DO:
                    IF tspostfg-log THEN RUN proc-set-cmplt (ipdtToday).
                END. /* if lookup */
                ELSE DO:
                    IF tspostfg-log THEN RUN proc-form-cmplt (ipdtToday).
                END. /* components receoipt */
            END. /* avail job-mch */
        END. /* else,  no TSPARTS*/
    END. /* charge code run */

    FIND LAST jobseq NO-LOCK NO-ERROR.
    IF AVAILABLE jobseq AND jobseq.charge_code EQ charge_code THEN DO:
        IF v-tsfinish-char-val EQ "Last Machine" AND lCompleted THEN
        RUN touch-finish.

        FOR EACH bMachTran FIELDS(complete) NO-LOCK
            WHERE bMachTran.company       EQ company_code
              AND bMachTran.machine       EQ machine_code
              AND bMachTran.job_number    EQ job_number
              AND bMachTran.job_sub       EQ INTEGER(job_sub)
              AND bMachTran.form_number   EQ INTEGER(form_number)
              AND bMachTran.blank_number  EQ INTEGER(blank_number)
              AND bMachTran.pass_sequence EQ INTEGER(pass_sequence)
            :
            lCompleted = IF NOT lCompleted THEN lCompleted
                         ELSE bMachTran.complete.
        END. /* for each */
 
        IF NOT lCompleted AND tscomplete-log THEN
            MESSAGE "IS OPERATION FOR MACHINE" CAPS(machine_code) "COMPLETE?" SKIP
                "Job Qty:" iJobQty "  Total Run Qty:" iRunQty  
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                UPDATE lCompleted.
        IF lCompleted THEN DO:
            CREATE cmpltjob.
            ASSIGN 
                cmpltjob.company       = company_code
                cmpltjob.machine       = machine_code
                cmpltjob.job_number    = job_number
                cmpltjob.job_sub       = INTEGER(job_sub)
                cmpltjob.form_number   = INTEGER(form_number)
                cmpltjob.blank_number  = INTEGER(blank_number)
                cmpltjob.pass_sequence = INTEGER(pass_sequence)
                .
            RELEASE cmpltjob.

            /* update job-mch.run-complete , mr-complete for scheduling */
            /* completeJobMch created b/c this procedure exceeded action code
               seqment size of 63k */
            RUN completeJobMch (
                company_code,
                machine_code,
                job_number,
                INTEGER(job_sub),
                INTEGER(form_number),
                INTEGER(blank_number)
                ).
            RUN updateRouting (
                company_code,
                machine_code,
                job_number,
                INTEGER(job_sub),
                INTEGER(form_number),
                INTEGER(blank_number)
                ).
            /* task# 06200526*/
            FIND LAST job-mch NO-LOCK
                WHERE job-mch.company EQ company_code
                  AND job-mch.job-no  EQ job_number
                  AND job-mch.job-no2 EQ INTEGER(job_sub)
                  AND job-mch.frm     EQ integer(form_number)                                 
                USE-INDEX line-idx NO-ERROR.
            lLastMachine = AVAILABLE job-mch AND job-mch.m-code EQ machine_code.
            IF fgrecpt-char = "TSPARTS" AND
                CAN-FIND(FIRST est
                         WHERE est.company  EQ company_code
                           AND est.est-no   EQ job-hdr.est-no
                           AND est.est-type GT 5) THEN .
            ELSE IF lLastMachine THEN RUN close-job (company_code,job_number,job_sub).
            RELEASE machtran.
        END. /* if lcompleted */
    END.  /* last seq */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Key_Stroke s-object 
PROCEDURE Key_Stroke :
/*------------------------------------------------------------------------------
  Purpose:     Apply keystroke to field with focus
  Parameters:  Input Keystroke
  Notes:       
------------------------------------------------------------------------------*/
  IF btn_hour:SENSITIVE IN FRAME {&FRAME-NAME} = NO
     AND (h_field:NAME = "time-hour" OR h_field:NAME = "time-minute")
  THEN RETURN.

  {touch/keystrok.i}
  
  IF h_field:NAME = "time-hour" OR h_field:NAME = "time-minute" THEN DO:
     lv-timer = int(time-hour:SCREEN-VALUE) * 3600 +
               INT(time-minute:SCREEN-VALUE) * 60.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize s-object 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN pCreateINIObjects ("HomeSmall,ResetTime,SetTime,AcceptEntry,Back").

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view s-object 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {touch/localview.i}
  ASSIGN
    v-time-clock-off = NO
    timerStatus:SCREEN-VALUE = setTimerStatus(NO).

  {methods/run_link.i "CONTAINER" "Get_Value" "('machine_list',OUTPUT machine_list)"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClick s-object 
PROCEDURE pClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcClick AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE ampm AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-time-hour AS INT NO-UNDO.
    DEFINE VARIABLE v-gang-jobs AS LOG NO-UNDO.
    DEFINE VARIABLE lv-starts AS CHAR EXTENT 2 NO-UNDO.
    DEFINE VARIABLE lv-stopps AS CHAR EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-today AS DATE NO-UNDO.
    DEFINE VARIABLE v-index AS INT NO-UNDO.
    DEFINE VARIABLE v-start AS INT INIT 1 NO-UNDO.
    DEFINE VARIABLE v-entered-start-date AS DATE NO-UNDO.
    DEFINE VARIABLE v-valid AS LOG INIT TRUE NO-UNDO.
  
    DEF BUFFER bf-machtran FOR machtran.
    DEF BUFFER bf2-machtran FOR machtran.
    DEF BUFFER bf-mach FOR mach.
    
    CASE ipcClick:
        WHEN "HomeSmall" THEN DO WITH FRAME {&FRAME-NAME}:
            RELEASE machtran.
            RELEASE machemp.
            {methods/run_link.i "CONTAINER" "Change_Page" "(2)"}
        END.
        WHEN "ResetTime" THEN DO:
            ASSIGN
                time-hour:SCREEN-VALUE = SUBSTR(STRING(TIME,'HH:MM AM'),1,2)
                time-minute:SCREEN-VALUE = SUBSTR(STRING(TIME,'HH:MM AM'),4,2)
                Btn_AMPM:LABEL = SUBSTR(STRING(TIME,'HH:MM AM'),7,2)
                .
        END.
        WHEN "SetTime" THEN DO:
            {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
            {methods/run_link.i "CONTAINER" "Get_Value" "('machine_code',OUTPUT machine_code)"}
            {methods/run_link.i "CONTAINER" "Get_Value" "('job_number',OUTPUT job_number)"}
            {methods/run_link.i "CONTAINER" "Get_Value" "('job_sub',OUTPUT job_sub)"}
            {methods/run_link.i "CONTAINER" "Get_Value" "('form_number',OUTPUT form_number)"}
            {methods/run_link.i "CONTAINER" "Get_Value" "('blank_number',OUTPUT blank_number)"}
            {methods/run_link.i "CONTAINER" "Get_Value" "('pass_sequence',OUTPUT pass_sequence)"}
            FIND LAST machtran NO-LOCK
                 WHERE machtran.company       EQ company_code
                   AND machtran.machine       EQ machine_code
                   AND machtran.job_number    EQ job_number
                   AND machtran.job_sub       EQ INTEGER(job_sub)
                   AND machtran.form_number   EQ INTEGER(form_number)
                   AND machtran.blank_number  EQ INTEGER(blank_number)
                   AND machtran.pass_sequence EQ INTEGER(pass_sequence)
                   AND (machtran.end_time     NE 0
                    OR machtran.total_time    NE 0)
                 NO-ERROR.
            IF AVAILABLE machtran THEN
            ASSIGN 
              time-hour:SCREEN-VALUE = SUBSTR(STRING(machtran.end_time,'HH:MM AM'),1,2)
              time-minute:SCREEN-VALUE = SUBSTR(STRING(machtran.end_time,'HH:MM AM'),4,2)
              Btn_AMPM:LABEL = SUBSTR(STRING(machtran.end_time,'HH:MM AM'),7,2)
              ampm = IF Btn_AMPM:LABEL = 'PM' AND time-hour NE 12 THEN 43200 ELSE 0
              lv-timer = int(time-hour:SCREEN-VALUE) * 3600
                       + INT(time-minute:SCREEN-VALUE) * 60
                       + ampm /* ampm adds in 12 hours for a PM time */
                       .             
            ELSE
            RUN pSetSensitive ("ResetTime",NO).
        END.
        WHEN "AcceptEntry" THEN DO:          
            ASSIGN
               time-hour time-minute run-qty waste-qty
               v-time-hour = time-hour
               v-today = TODAY.
          
            IF time-hour LT 1 OR time-hour GT 12 THEN DO:
              MESSAGE 'INVALID HOUR - ENTER BETWEEN 1 AND 12' VIEW-AS ALERT-BOX ERROR.
              APPLY 'CHOOSE' TO Btn_Hour.
              RETURN NO-APPLY.
            END.
            IF time-minute GT 59 THEN DO:
              MESSAGE 'INVALID MINUTE - ENTER BETWEEN 0 AND 59' VIEW-AS ALERT-BOX ERROR.
              APPLY 'CHOOSE' TO Btn_Minute.
              RETURN NO-APPLY.
            END.  
            IF job_sequence BEGINS 'END' THEN DO: /* task 10050516*/
               RUN check-tsqty NO-ERROR.
               IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            END.
          
            ASSIGN ampm = IF Btn_AMPM:LABEL = 'PM' AND time-hour NE 12 THEN 43200 ELSE 0
                   v-time-hour = IF Btn_AMPM:LABEL = 'AM' AND time-hour = 12 THEN 0 ELSE v-time-hour.
          
            /* check machine's running. If yes, don't create new transaction. End it first */
            {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
            {methods/run_link.i "CONTAINER" "Get_Value" "('machine_code',OUTPUT machine_code)"}     
            {methods/run_link.i "CONTAINER" "Get_Value" "('machine_list',OUTPUT machine_list)"}
           
            /* task# 10110517 allow duplicate time if gang jobs is yes*/
            FIND FIRST bf-mach WHERE
                 bf-mach.company EQ company_code AND
                 bf-mach.m-code  EQ machine_code
                 NO-LOCK NO-ERROR.
          
            v-gang-jobs = IF AVAIL bf-mach THEN bf-mach.gang-jobs ELSE NO.
            
            IF NOT v-gang-jobs THEN DO:
               IF job_sequence BEGINS 'START' THEN DO:
          
                  FIND FIRST bf-machtran WHERE
                       bf-machtran.company EQ company_code AND
                       bf-machtran.machine EQ machine_code AND
                       bf-machtran.end_date   EQ ? AND
                       bf-machtran.end_time   EQ 0 AND
                       bf-machtran.TOTAL_time EQ 0
                       NO-LOCK NO-ERROR.
                 
                  IF AVAIL bf-machtran THEN DO:
                     MESSAGE "Machine " + "is running for a job " +
                             TRIM(bf-machtran.job_number) +
                             "-" + TRIM(STRING(bf-machtran.job_sub,"99")) +  
                             ".   Must end data collection for the job " +
                             TRIM(bf-machtran.job_number)
                         VIEW-AS ALERT-BOX ERROR.
                     RETURN NO-APPLY.
                  END.
               END.
              
               ELSE DO:
                 {methods/run_link.i "CONTAINER" "Get_MachTran_Rowid" "(OUTPUT machtran-rowid)"}
                 FIND FIRST bf2-machtran NO-LOCK
                     WHERE ROWID(bf2-machtran) EQ machtran-rowid
                     NO-ERROR.
               END.
              
               ASSIGN
                lv-stopps[1] = STRING(YEAR(v-today),"9999") +
                               STRING(MONTH(v-today),"99")  +
                               STRING(DAY(v-today),"99")    +
                               STRING(v-time-hour * 3600 + time-minute * 60 + ampm,"99999")
                lv-starts[1] = IF AVAIL bf2-machtran THEN
                                 (STRING(YEAR(bf2-machtran.start_date),"9999") +
                                  STRING(MONTH(bf2-machtran.start_date),"99")  +
                                  STRING(DAY(bf2-machtran.start_date),"99")    +
                                  STRING(bf2-machtran.start_time,"99999"))
                               ELSE lv-stopps[1]
               v-entered-start-date = IF AVAIL bf2-machtran THEN bf2-machtran.start_date ELSE
                                      v-today.
               
               EMPTY TEMP-TABLE tt-mach.
               
               CREATE tt-mach.
               ASSIGN tt-mach.machine = machine_code.
               RELEASE tt-mach.
               
               DO v-index = 1 TO LENGTH(machine_list):
               
                  IF SUBSTRING(machine_list,v-index,1) EQ "," AND
                     machine_code NE SUBSTRING(machine_list,v-start,v-index - v-start) THEN
                  DO:
                     CREATE tt-mach.
                     ASSIGN tt-mach.machine = SUBSTRING(machine_list,v-start,v-index - v-start)
                            v-start = v-index + 1.
                     RELEASE tt-mach.
                  END.
               END.
               
               FOR EACH tt-mach,
                   EACH bf-machtran FIELDS(start_date start_time end_date end_time
                        charge_code job_number job_sub form_number blank_number ) NO-LOCK
                   WHERE bf-machtran.company  EQ company_code
                     AND bf-machtran.machine EQ tt-mach.machine
                     AND bf-machtran.end_date NE ?
                     AND bf-machtran.end_date GE v-entered-start-date
                     AND (NOT AVAIL bf2-machtran OR
                          ROWID(bf-machtran)  NE ROWID(bf2-machtran))
                   USE-INDEX menddate:
               
                   ASSIGN
                    lv-starts[2] = STRING(YEAR(bf-machtran.start_date),"9999") +
                                   STRING(MONTH(bf-machtran.start_date),"99")  +
                                   STRING(DAY(bf-machtran.start_date),"99")    +
                                   STRING(bf-machtran.start_time,"99999")
                    lv-stopps[2] = STRING(YEAR(bf-machtran.end_date),"9999") +
                                   STRING(MONTH(bf-machtran.end_date),"99")  +
                                   STRING(DAY(bf-machtran.end_date),"99") +
                                   STRING(bf-machtran.end_time,"99999").
                    
                   IF NOT(job_sequence BEGINS 'START') THEN
                   DO:
                      IF lv-starts[1] EQ lv-starts[2] THEN
                         v-valid = FALSE.
                      ELSE IF lv-starts[1] GT lv-starts[2] THEN
                      DO:
                          IF lv-starts[1] LT lv-stopps[2] THEN
                             v-valid = FALSE.
                      END.
                      ELSE IF lv-starts[2] LT lv-stopps[1] THEN
                          v-valid = FALSE.
                   END.
                   ELSE /*start*/
                      IF lv-starts[2] LE lv-starts[1] AND
                         lv-starts[1] LT lv-stopps[2] THEN
                         v-valid = FALSE.
                        
                   IF NOT v-valid THEN
                   DO:
                      MESSAGE "Machine transaction " + TRIM(bf-machtran.charge_code) +
                              " exists for Job#: " + TRIM(bf-machtran.job_number) + "-" +
                              TRIM(STRING(bf-machtran.job_sub,"99")) +
                              " Form#: " + STRING(bf-machtran.form_number) +
                              " Blank#: " + STRING(bf-machtran.blank_number) +
                              " From " + STRING(bf-machtran.start_date,"99/99/99") + "@" +
                                         STRING(bf-machtran.start_time,"hh:mm") +
                              " To " + STRING(bf-machtran.end_date,"99/99/99") + "@" +
                                       STRING(bf-machtran.end_time,"hh:mm")
                              SKIP
                              "Validate operation's start date and time."
                          VIEW-AS ALERT-BOX ERROR.
                      RETURN NO-APPLY.
                   END.
               END.
            END.
          
            RUN Job_Data_Collection(INPUT v-today).
            RELEASE machemp.
            RELEASE machtran.
          
            IF RETURN-VALUE = 'COMPLETED' THEN DO:   
              IF getNumForms() GT 1 THEN DO:
              {methods/run_link.i "CONTAINER" "Change_Page" "(10)"}
              END.
              ELSE DO:
                  {methods/run_link.i "CONTAINER" "Change_Page" "(9)"} /* Tried 15, but no effect */        
              END.
              RETURN NO-APPLY.
            END.
          
            {methods/run_link.i "CONTAINER" "Run_Get_Job_Sequence"}
            {methods/run_link.i "CONTAINER" "Change_Page" "(13)"}
        END.
        WHEN "Back" THEN DO:
            {methods/run_link.i "CONTAINER" "Change_Page" "(13)"}
        END.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-form-cmplt s-object 
PROCEDURE proc-form-cmplt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* from pc/pcprdd3u.p pcprdd4u.p */
   DEFINE INPUT PARAM v-today AS DATE NO-UNDO.
    
   DEF VAR v-est-type           LIKE est.est-type NO-UNDO.
   DEF VAR v-loc                LIKE fg-bin.loc NO-UNDO.
   DEF VAR v-loc-bin            LIKE fg-bin.loc-bin NO-UNDO.
   DEF VAR v-qty                AS   INT NO-UNDO.
   DEF VAR choice               AS   LOG NO-UNDO.
   DEF VAR v-assembled AS   LOG NO-UNDO.
   DEF VAR v-runqty AS INT NO-UNDO.
   DEF VAR X AS INT NO-UNDO.
   DEF VAR v-up AS INT NO-UNDO.
   DEF VAR v-out AS INT NO-UNDO.
   DEF VAR v-up-hs     LIKE eb.num-up NO-UNDO.
   DEF VAR v-on        LIKE eb.num-up NO-UNDO.
   DEF VAR h_updbin AS HANDLE NO-UNDO.
   

   DEF BUFFER bf-machtran FOR machtran.
   DEF BUFFER b-reftable FOR reftable.

   cocode = company_code.

   FIND FIRST job WHERE job.company EQ company_code
        AND job.job-no  EQ job_number
        AND job.job-no2 EQ int(job_sub)
      USE-INDEX job-no NO-LOCK NO-ERROR.

  FIND FIRST est
      WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
      NO-LOCK NO-ERROR.
  v-est-type = IF AVAIL est THEN est.est-type ELSE 1.

  IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

  v-assembled = NO.
/*
  IF v-est-type EQ 2 THEN
  FOR FIRST job-hdr
      WHERE job-hdr.company  EQ company_code
        AND job-hdr.job      EQ job.job
        AND job-hdr.job-no   EQ job.job-no
        AND job-hdr.job-no2  EQ job.job-no2
      NO-LOCK,
      FIRST itemfg
      WHERE itemfg.company   EQ company_code
        AND itemfg.i-no      EQ job-hdr.i-no
        AND itemfg.isaset
        AND itemfg.alloc     NE YES
      NO-LOCK:

    ASSIGN
     v-assembled = YES
     v-est-type  = 4.
  END.
??? */

 /* IF v-assembled THEN do for both assembled or unassembled */
  FOR EACH reftable
      WHERE reftable.reftable EQ "jc/jc-calc.p"
        AND reftable.company  EQ job-mch.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ STRING(job-mch.job,"999999999")
        AND reftable.val[12]  EQ job-mch.frm
        AND (reftable.val[13] EQ job-mch.blank-no OR
             job-mch.blank-no EQ 0),
      EACH job-hdr
      WHERE job-hdr.company   EQ company_code
        AND job-hdr.job-no    EQ job-mch.job-no
        AND job-hdr.job-no2   EQ job-mch.job-no2
        AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
        AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ),

      FIRST itemfg
      WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no    EQ reftable.code2 NO-LOCK:

    /*IF itemfg.isaset AND itemfg.alloc NE YES THEN DO:
      ASSIGN
       v-set  = itemfg.i-no
       v-qty  = pc-prdd.qty.
            
      RUN fg/checkset.p (RECID(itemfg), ?, INPUT-OUTPUT v-qty).
          
      IF v-qty LT pc-prdd.qty THEN DO:
        choice = NO.
        MESSAGE "Insufficient components for AUTOPOST, process anyway?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE choice.
        IF NOT choice THEN RETURN ERROR.
      END.
    END.*/    

    RUN fg/autopost.p (ROWID(itemfg), job-hdr.job-no, job-hdr.job-no2,
                       OUTPUT v-loc, OUTPUT v-loc-bin).

    FIND FIRST fg-bin
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
          AND fg-bin.loc     EQ v-loc
          AND fg-bin.loc-bin EQ v-loc-bin  
          AND fg-bin.tag     EQ ""
          AND fg-bin.job-no  EQ job-hdr.job-no
          AND fg-bin.job-no2 EQ job-hdr.job-no2
        NO-ERROR.
    IF NOT AVAIL fg-bin THEN DO:
      CREATE fg-bin.
      ASSIGN
       fg-bin.company      = itemfg.company
       fg-bin.loc          = v-loc
       fg-bin.loc-bin      = v-loc-bin
       fg-bin.i-no         = reftable.code2
       fg-bin.tag          = ""
       fg-bin.job-no       = job-hdr.job-no
       fg-bin.job-no2      = job-hdr.job-no2
       fg-bin.std-mat-cost = reftable.val[2]
       fg-bin.std-lab-cost = reftable.val[1]
       fg-bin.std-fix-cost = reftable.val[4]
       fg-bin.std-var-cost = reftable.val[3]
       fg-bin.std-tot-cost = reftable.val[5]
       fg-bin.last-cost    = job-hdr.std-tot-cost
       fg-bin.unit-count   = itemfg.case-count.
    END.
      
    IF fg-bin.cases-unit   LE 0 THEN fg-bin.cases-unit   = 1.
    IF fg-bin.units-pallet LE 0 THEN fg-bin.units-pallet = 1.
    
    FIND FIRST b-reftable
        WHERE b-reftable.reftable EQ "ts/jobdata.p"
          AND b-reftable.company  EQ company_code
          AND b-reftable.code     EQ job-hdr.rec_key
        EXCLUSIVE NO-ERROR.
    IF AVAIL b-reftable THEN DELETE b-reftable.
    CREATE b-reftable.
    ASSIGN
     b-reftable.reftable = "ts/jobdata.p"
     b-reftable.company  = company_code
     b-reftable.code     = job-hdr.rec_key
     b-reftable.code2    = fg-bin.rec_key.
    
    v-runqty = 0. 
    FOR EACH bf-machtran FIELDS(RUN_qty) WHERE
        bf-machtran.company = company_code AND
        bf-machtran.machine = machine_code AND
        bf-machtran.job_number = job_number AND
        bf-machtran.job_sub = INTEGER(job_sub) AND
        bf-machtran.form_number = INTEGER(form_number) AND
        bf-machtran.blank_number = INTEGER(blank_number) AND
        bf-machtran.pass_sequence = INTEGER(pass_sequence) NO-LOCK:
        v-runqty = v-runqty + bf-machtran.run_qty.
    END.
    RUN touch/d-updbin.w  (ROWID(fg-bin), v-runqty,employee_code,company_code). /* pc-prdd.qty*/
  END.  /* v-assembled */
/* === NEED more code later
  ELSE DO:     /* for unassembled sets
       THIS CODE WILL POST BOTH COMPONENTS AND SETS ON EVERY FORM, WHICH IS A BUG. 
       ADDITIONAL CODE MUST BE WRITTEN TO ONLY POST ON LAST OPERATION OF LAST FORM 
               */     
        {addon/touch/jobbin.i}
  END.
===*/

  /*=========== create fg receipt : from pc/r-wippst.w */
  /*FOR EACH bf-machtran WHERE bf-machtran.company = company_code AND
                                      bf-machtran.machine = machine_code AND
                                      bf-machtran.job_number = job_number AND
                                      bf-machtran.job_sub = INTEGER(job_sub) AND
                                      bf-machtran.form_number = INTEGER(form_number) AND
                                      bf-machtran.blank_number = INTEGER(blank_number) AND
                                      bf-machtran.pass_sequence = INTEGER(pass_sequence) NO-LOCK:
                                      */
    FIND FIRST job WHERE job.company EQ company_code
                        AND job.job-no  EQ job_number
                        AND job.job-no2 EQ int(job_sub)
                        USE-INDEX job-no NO-ERROR.

    ASSIGN v-up  = 1
           v-out = 1
           v-on  = 1.

    FIND FIRST est WHERE est.company EQ job.company
                     AND est.est-no  EQ job.est-no
                     NO-LOCK NO-ERROR.
    v-est-type = IF AVAIL est THEN est.est-type ELSE 1.
    IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

    FIND FIRST mach WHERE mach.company = job-mch.company
                       AND mach.m-code = job-mch.m-code NO-LOCK NO-ERROR.
    /*
    IF mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR
       mach.dept[3] EQ "PR" OR mach.dept[4] EQ "PR" THEN
       RUN update-plate-die (ROWID(machtran), "P", v-est-type).

    IF mach.dept[1] EQ "DC" OR mach.dept[2] EQ "DC" OR
       mach.dept[3] EQ "DC" OR mach.dept[4] EQ "DC" THEN
       RUN update-plate-die (ROWID(machtran), "D", v-est-type). */

    IF INDEX("AP",mach.p-type) GT 0 THEN
      ASSIGN
       v-on  = 1
       v-up  = 1
       v-out = 1.

    ELSE
    IF AVAIL est THEN DO:
      RUN sys/inc/numup.p (est.company, est.est-no, job-mch.frm, OUTPUT v-up).

      FIND FIRST ef
          WHERE ef.company EQ est.company
            AND ef.est-no  EQ est.est-no
            AND ef.form-no EQ job-mch.frm
          NO-LOCK NO-ERROR.

      IF AVAIL ef THEN DO:
        RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).
        v-on = v-up * v-on.
      END.
                      
      FIND FIRST est-op
          WHERE est-op.company EQ est.company
            AND est-op.est-no  EQ est.est-no
            AND est-op.s-num   EQ job-mch.frm
            AND (est-op.b-num  EQ job-mch.blank-no OR job-mch.blank-no EQ 0)
            AND est-op.m-code  EQ job-mch.m-code
            AND est-op.op-pass EQ job-mch.pass
            AND est-op.dept    EQ job-mch.dept
            AND est-op.line    LT 500
          NO-LOCK NO-ERROR.

      IF ((AVAIL est-op) AND est-op.op-sb)           OR
         ((NOT AVAIL est-op) AND mach.p-type NE "B") THEN DO:

        IF AVAIL est-op THEN RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-out).
        ELSE v-out = 1.
        v-up = v-up * v-out.
      END.
      ELSE v-up = 1.

      v-on = v-on / v-up.
    END.
           
    v-up-hs = 1.

    IF job-mch.dept EQ "HS" AND
       AVAIL est            AND
       mach.therm           AND
       mach.p-type EQ "S"   THEN
      RUN sys/inc/numup.p (est.company, est.est-no, job-mch.frm, OUTPUT v-up-hs).

 /* Don't create wip
    {touch/pcmchact.i}  /* from {pc/pcmchact.i}  mch-act creatation */
 */
 
 /* IF v-assembled THEN */
    IF machtran.run_qty > 0 OR v-runqty > 0 THEN
    FOR EACH reftable
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job-mch.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job-mch.job,"999999999")
          AND reftable.val[12]  EQ job-mch.frm
          AND (reftable.val[13] EQ job-mch.blank-no OR
               job-mch.blank-no EQ 0),
        EACH job-hdr
      WHERE job-hdr.company   EQ company_code
        AND job-hdr.job-no    EQ job-mch.job-no
        AND job-hdr.job-no2   EQ job-mch.job-no2
        AND (job-hdr.frm      EQ job-mch.frm OR v-est-type <> 4)
        AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 OR v-est-type <> 4),
        FIRST itemfg
        WHERE itemfg.company    EQ cocode
          AND itemfg.i-no       EQ reftable.code2
          AND itemfg.case-count GT 0 NO-LOCK:

      x = 1.
      FOR EACH fg-rctd NO-LOCK BY fg-rctd.r-no DESC:
        LEAVE.
      END.
      IF AVAIL fg-rctd THEN x = fg-rctd.r-no.

      FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
      IF AVAIL fg-rcpth AND fg-rcpth.r-no GT x THEN x = fg-rcpth.r-no.

      CREATE fg-rctd.
      ASSIGN
       fg-rctd.r-no       = X + 1
       fg-rctd.rct-date   = v-today /*c-prdd.op-date*/
       fg-rctd.trans-time = TIME
       fg-rctd.company    = job-hdr.company
       fg-rctd.rita-code  = "R"
       fg-rctd.i-name     = itemfg.i-name
       fg-rctd.i-no       = reftable.code2
       fg-rctd.job-no     = job-hdr.job-no
       fg-rctd.job-no2    = job-hdr.job-no2.
                 
      ASSIGN
       v-up  = 1
       v-out = 1.
      
      IF AVAIL est AND NOT CAN-DO("A,P,B",mach.p-type) THEN DO:
        FIND FIRST eb NO-LOCK
            WHERE eb.company  EQ est.company
              AND eb.est-no   EQ est.est-no
              AND eb.form-no  EQ reftable.val[12]
              AND eb.blank-no EQ reftable.val[13]
            NO-ERROR.
        IF AVAIL eb THEN v-up = eb.num-up.
                 
        FIND FIRST est-op
            WHERE est-op.company EQ est.company
              AND est-op.est-no  EQ est.est-no
              AND est-op.s-num   EQ job-hdr.frm
              AND (est-op.b-num  EQ job-hdr.blank-no OR
                   job-hdr.blank-no EQ 0)
              AND est-op.m-code  EQ job-mch.m-code
              AND est-op.op-pass EQ job-mch.pass
              AND est-op.dept    EQ job-mch.dept
              AND est-op.line    LT 500
            NO-LOCK NO-ERROR.
        IF AVAIL est-op AND est-op.n-out NE 0 THEN v-out = est-op.n-out.
      END.

      ASSIGN
       fg-rctd.b-num      = reftable.val[13]
       fg-rctd.s-num      = reftable.val[12]
       fg-rctd.t-qty      = (IF machtran.run_qty = 0 THEN v-runqty ELSE machtran.run_qty)
                                 / v-up-hs * v-out * v-up  /*v-runqty*/
       fg-rctd.pur-uom    = itemfg.prod-uom
       fg-rctd.cost-uom   = itemfg.prod-uom
       fg-rctd.std-cost   = reftable.val[5]
       fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
       fg-rctd.qty-case   = itemfg.case-count
       fg-rctd.partial    = fg-rctd.t-qty MODULO itemfg.case-count
       fg-rctd.cases      = trunc(fg-rctd.t-qty / itemfg.case-count,0)
       fg-rctd.cases-unit = 1.

      IF fg-rctd.t-qty LE 0 THEN fg-rctd.cases = 0.

      RELEASE fg-bin.
      
      FIND FIRST b-reftable
          WHERE b-reftable.reftable EQ "ts/jobdata.p"
            AND b-reftable.company  EQ company_code
            AND b-reftable.code     EQ job-hdr.rec_key
          NO-LOCK NO-ERROR.

      IF AVAIL b-reftable THEN 
      FIND FIRST fg-bin WHERE fg-bin.rec_key EQ b-reftable.code2 NO-LOCK NO-ERROR.
      
      IF AVAIL fg-bin THEN
        ASSIGN
         v-loc       = fg-bin.loc
         v-loc-bin   = fg-bin.loc-bin
         fg-rctd.tag = fg-bin.tag.
                
      ELSE
        RUN fg/autopost.p (ROWID(itemfg), fg-rctd.job-no, fg-rctd.job-no2,
                           OUTPUT v-loc, OUTPUT v-loc-bin).

      ASSIGN
       fg-rctd.loc     = v-loc
       fg-rctd.loc-bin = v-loc-bin.

      FIND FIRST fg-bin
          WHERE fg-bin.company EQ fg-rctd.company
            AND fg-bin.i-no    EQ fg-rctd.i-no
            AND fg-bin.job-no  EQ job-hdr.job-no
            AND fg-bin.job-no2 EQ job-hdr.job-no2
            AND fg-bin.loc     EQ fg-rctd.loc
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin
            AND fg-bin.tag     EQ fg-rctd.tag
          NO-LOCK NO-ERROR.

      IF AVAIL fg-bin AND fg-bin.cases-unit <> 0 THEN fg-rctd.cases-unit = fg-bin.cases-unit.

      RUN fg/comprcpt.p (ROWID(fg-rctd)).
    END. /* v-assembled */
/*====
    ELSE DO:
       FOR EACH job-hdr WHERE job-hdr.company   EQ company_code
          AND job-hdr.job-no    EQ job-mch.job-no
          AND job-hdr.job-no2   EQ job-mch.job-no2         
          AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
          AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ) :

          FIND first itemfg where itemfg.company    eq company_code
                        and itemfg.i-no       eq job-hdr.i-no
                        and itemfg.case-count gt 0
                        NO-LOCK NO-ERROR.
          IF NOT AVAIL itemfg THEN NEXT.
          {addon/touch/jobrcpt.i}
===    END.
  END.  /* for each job-hdr */
===    */

   /* end of fg receipt creation */
 
  RELEASE fg-rctd.
  RELEASE job.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-set-cmplt s-object 
PROCEDURE proc-set-cmplt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* from pc/pcprdd3u.p pcprdd4u.p */
   DEFINE INPUT PARAM v-today AS DATE NO-UNDO.

   DEF VAR v-est-type           LIKE est.est-type NO-UNDO.
   DEF VAR v-loc                LIKE fg-bin.loc NO-UNDO.
   DEF VAR v-loc-bin            LIKE fg-bin.loc-bin NO-UNDO.
   DEF VAR v-qty                AS   INT NO-UNDO.
   DEF VAR choice               AS   LOG NO-UNDO.
   DEF VAR v-assembled AS   LOG NO-UNDO.
   DEF VAR v-runqty AS INT NO-UNDO.
   DEF VAR X AS INT NO-UNDO.
   DEF VAR v-up AS INT NO-UNDO.
   DEF VAR v-out AS INT NO-UNDO.
   DEF VAR v-up-hs     LIKE eb.num-up NO-UNDO.
   DEF VAR v-on        LIKE eb.num-up NO-UNDO.
   DEF VAR h_updbin AS HANDLE NO-UNDO.
   
   DEF BUFFER bf-machtran FOR machtran.
   DEF BUFFER b-reftable FOR reftable.

   cocode = company_code.

   FIND FIRST job WHERE job.company EQ company_code
        AND job.job-no  EQ job_number
        AND job.job-no2 EQ int(job_sub)
      USE-INDEX job-no NO-LOCK NO-ERROR.

  FIND FIRST est
      WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
      NO-LOCK NO-ERROR.
  v-est-type = IF AVAIL est THEN est.est-type ELSE 1.

  IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

  v-assembled = NO.
/*
  IF v-est-type EQ 2 THEN
  FOR FIRST job-hdr
      WHERE job-hdr.company  EQ company_code
        AND job-hdr.job      EQ job.job
        AND job-hdr.job-no   EQ job.job-no
        AND job-hdr.job-no2  EQ job.job-no2
      NO-LOCK,
      FIRST itemfg
      WHERE itemfg.company   EQ company_code
        AND itemfg.i-no      EQ job-hdr.i-no
        AND itemfg.isaset
        AND itemfg.alloc     NE YES
      NO-LOCK:

    ASSIGN
     v-assembled = YES
     v-est-type  = 4.
  END.
*/

 /* IF v-assembled THEN do for both assembled or unassembled */
/* FOR EACH reftable
      WHERE reftable.reftable EQ "jc/jc-calc.p"
        AND reftable.company  EQ job-mch.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ STRING(job-mch.job,"999999999")
        AND reftable.val[12]  EQ job-mch.frm
        AND (reftable.val[13] EQ job-mch.blank-no OR
             job-mch.blank-no EQ 0),
    */         

  FOR EACH job-hdr
      WHERE job-hdr.company   EQ company_code
        AND job-hdr.job-no    EQ job-mch.job-no
        AND job-hdr.job-no2   EQ job-mch.job-no2
        AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
        AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ),

      FIRST itemfg
      WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no    EQ job-hdr.i-no NO-LOCK:

    /*IF itemfg.isaset AND itemfg.alloc NE YES THEN DO:
      ASSIGN
       v-set  = itemfg.i-no
       v-qty  = pc-prdd.qty.
            
      RUN fg/checkset.p (RECID(itemfg), ?, INPUT-OUTPUT v-qty).
          
      IF v-qty LT pc-prdd.qty THEN DO:
        choice = NO.
        MESSAGE "Insufficient components for AUTOPOST, process anyway?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE choice.
        IF NOT choice THEN RETURN ERROR.
      END.
    END.*/    

    RUN fg/autopost.p (ROWID(itemfg), job-hdr.job-no, job-hdr.job-no2,
                       OUTPUT v-loc, OUTPUT v-loc-bin).

    FIND FIRST fg-bin
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
          AND fg-bin.loc     EQ v-loc
          AND fg-bin.loc-bin EQ v-loc-bin  
          AND fg-bin.tag     EQ ""
          AND fg-bin.job-no  EQ job-hdr.job-no
          AND fg-bin.job-no2 EQ job-hdr.job-no2
        NO-ERROR.
    IF NOT AVAIL fg-bin THEN DO:
      CREATE fg-bin.
      ASSIGN
       fg-bin.company      = itemfg.company
       fg-bin.loc          = v-loc
       fg-bin.loc-bin      = v-loc-bin
       fg-bin.i-no         = job-hdr.i-no 
       fg-bin.tag          = ""
       fg-bin.job-no       = job-hdr.job-no
       fg-bin.job-no2      = job-hdr.job-no2
       fg-bin.std-mat-cost = job-hdr.std-mat-cost
       fg-bin.std-lab-cost = job-hdr.std-lab-cost
       fg-bin.std-fix-cost = job-hdr.std-fix-cost
       fg-bin.std-var-cost = job-hdr.std-var-cost
       fg-bin.std-tot-cost = job-hdr.std-tot-cost
       fg-bin.last-cost    = job-hdr.std-tot-cost
       fg-bin.unit-count   = itemfg.case-count.
    END.
      
    IF fg-bin.cases-unit   LE 0 THEN fg-bin.cases-unit   = 1.
    IF fg-bin.units-pallet LE 0 THEN fg-bin.units-pallet = 1.
    
    FIND FIRST b-reftable
        WHERE b-reftable.reftable EQ "ts/jobdata.p"
          AND b-reftable.company  EQ company_code
          AND b-reftable.code     EQ job-hdr.rec_key
        EXCLUSIVE NO-ERROR.
    IF AVAIL b-reftable THEN DELETE b-reftable.
    CREATE b-reftable.
    ASSIGN
     b-reftable.reftable = "ts/jobdata.p"
     b-reftable.company  = company_code
     b-reftable.code     = job-hdr.rec_key
     b-reftable.code2    = fg-bin.rec_key
     v-runqty = 0.

    FOR EACH bf-machtran FIELDS(RUN_qty) WHERE
        bf-machtran.company = company_code AND
        bf-machtran.machine = machine_code AND
        bf-machtran.job_number = job_number AND
        bf-machtran.job_sub = INTEGER(job_sub) AND
        bf-machtran.form_number = INTEGER(form_number) AND
        bf-machtran.blank_number = INTEGER(blank_number) AND
        bf-machtran.pass_sequence = INTEGER(pass_sequence) NO-LOCK:
        v-runqty = v-runqty + bf-machtran.run_qty.
    END.

    RUN touch/d-updbin.w  (ROWID(fg-bin), v-runqty,employee_code,company_code). /* pc-prdd.qty*/
  END.  /* v-assembled */

/* === NEED more code later
  ELSE DO:     /* for unassembled sets
       THIS CODE WILL POST BOTH COMPONENTS AND SETS ON EVERY FORM, WHICH IS A BUG. 
       ADDITIONAL CODE MUST BE WRITTEN TO ONLY POST ON LAST OPERATION OF LAST FORM 
               */     
        {addon/touch/jobbin.i}
  END.
===*/

  /*=========== create fg receipt : from pc/r-wippst.w */
  /*FOR EACH bf-machtran WHERE bf-machtran.company = company_code AND
                                      bf-machtran.machine = machine_code AND
                                      bf-machtran.job_number = job_number AND
                                      bf-machtran.job_sub = INTEGER(job_sub) AND
                                      bf-machtran.form_number = INTEGER(form_number) AND
                                      bf-machtran.blank_number = INTEGER(blank_number) AND
                                      bf-machtran.pass_sequence = INTEGER(pass_sequence) NO-LOCK:
                                      */
    FIND FIRST job WHERE job.company EQ company_code
                        AND job.job-no  EQ job_number
                        AND job.job-no2 EQ int(job_sub)
                        USE-INDEX job-no NO-ERROR.

    ASSIGN v-up  = 1
           v-out = 1
           v-on  = 1.

    FIND FIRST est WHERE est.company EQ job.company
                     AND est.est-no  EQ job.est-no
                     NO-LOCK NO-ERROR.
    v-est-type = IF AVAIL est THEN est.est-type ELSE 1.
    IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

    FIND FIRST mach WHERE mach.company = job-mch.company
                       AND mach.m-code = job-mch.m-code NO-LOCK NO-ERROR.

    /*
    IF mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR
       mach.dept[3] EQ "PR" OR mach.dept[4] EQ "PR" THEN
       RUN update-plate-die (ROWID(machtran), "P", v-est-type).

    IF mach.dept[1] EQ "DC" OR mach.dept[2] EQ "DC" OR
       mach.dept[3] EQ "DC" OR mach.dept[4] EQ "DC" THEN
       RUN update-plate-die (ROWID(machtran), "D", v-est-type). */

    IF AVAIL est THEN DO:
       RUN sys/inc/numup.p (est.company, est.est-no, job-mch.frm, OUTPUT v-up).

    FIND FIRST ef
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ job-mch.frm
        NO-LOCK NO-ERROR.

    IF AVAIL ef THEN
      v-on = v-up *
             (IF ef.n-out   EQ 0 THEN 1 ELSE ef.n-out) *
             (IF ef.n-out-l EQ 0 THEN 1 ELSE ef.n-out-l) *
             (IF ef.n-out-d EQ 0 THEN 1 ELSE ef.n-out-d).
                      
    FIND FIRST est-op
        WHERE est-op.company EQ est.company
          AND est-op.est-no  EQ est.est-no
          AND est-op.s-num   EQ job-mch.frm
          AND (est-op.b-num  EQ job-mch.blank-no OR job-mch.blank-no EQ 0)
          AND est-op.m-code  EQ job-mch.m-code
          AND est-op.op-pass EQ job-mch.pass
          AND est-op.dept    EQ job-mch.dept
          AND est-op.line    LT 500
        NO-LOCK NO-ERROR.

    IF ((AVAIL est-op) AND est-op.op-sb)           OR
       ((NOT AVAIL est-op) AND mach.p-type NE "B") THEN DO:

      IF AVAIL est-op THEN RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-out).
      ELSE v-out = 1.
      v-up = v-up * v-out.
    END.
    ELSE v-up = 1.

    v-on = v-on / v-up.
  END.
           
  v-up-hs = 1.

  IF job-mch.dept EQ "HS" AND
     AVAIL est            AND
     mach.therm           AND
     mach.p-type EQ "S"   THEN
    RUN sys/inc/numup.p (est.company, est.est-no, job-mch.frm, OUTPUT v-up-hs).

 /* Don't create wip
    {touch/pcmchact.i}  /* from {pc/pcmchact.i}  mch-act creatation */
 */

 /* IF v-assembled THEN */
    IF machtran.run_qty > 0 OR v-runqty > 0 THEN
    /*FOR EACH reftable
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job-mch.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job-mch.job,"999999999")
          AND reftable.val[12]  EQ job-mch.frm
          AND (reftable.val[13] EQ job-mch.blank-no OR
               job-mch.blank-no EQ 0), */
     FOR EACH job-hdr
      WHERE job-hdr.company   EQ company_code
        AND job-hdr.job-no    EQ job-mch.job-no
        AND job-hdr.job-no2   EQ job-mch.job-no2
        AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
        AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ),
        FIRST itemfg
        WHERE itemfg.company    EQ cocode
          AND itemfg.i-no       EQ job-hdr.i-no
          AND itemfg.case-count GT 0 NO-LOCK:

      x = 1.
      FOR EACH fg-rctd NO-LOCK BY fg-rctd.r-no DESC:
        LEAVE.
      END.
      IF AVAIL fg-rctd THEN x = fg-rctd.r-no.

      FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
      IF AVAIL fg-rcpth AND fg-rcpth.r-no GT x THEN x = fg-rcpth.r-no.

      CREATE fg-rctd.
      ASSIGN
       fg-rctd.r-no       = X + 1
       fg-rctd.rct-date   = v-today /*c-prdd.op-date*/
       fg-rctd.trans-time = TIME
       fg-rctd.company    = job-hdr.company
       fg-rctd.rita-code  = "R"
       fg-rctd.i-name     = itemfg.i-name
       fg-rctd.i-no       = job-hdr.i-no
       fg-rctd.job-no     = job-hdr.job-no
       fg-rctd.job-no2    = job-hdr.job-no2
       v-up  = 1
       v-out = 1.
      
      IF AVAIL est AND mach.p-type NE "B" THEN DO:
        RUN sys/inc/numup.p (est.company, est.est-no, job-mch.frm, OUTPUT v-up).
                 
        FIND FIRST est-op
            WHERE est-op.company EQ est.company
              AND est-op.est-no  EQ est.est-no
              AND est-op.s-num   EQ job-hdr.frm
              AND (est-op.b-num  EQ job-hdr.blank-no OR
                   job-hdr.blank-no EQ 0)
              AND est-op.m-code  EQ job-mch.m-code
              AND est-op.op-pass EQ job-mch.pass
              AND est-op.dept    EQ job-mch.dept
              AND est-op.line    LT 500
            NO-LOCK NO-ERROR.
        IF AVAIL est-op AND est-op.n-out NE 0 THEN v-out = est-op.n-out.
      END.

      ASSIGN
       fg-rctd.b-num      = job-mch.blank-no
       fg-rctd.s-num      = job-mch.frm
       fg-rctd.t-qty      = (IF machtran.run_qty = 0 THEN v-runqty ELSE machtran.run_qty) 
                               / v-up-hs * v-out * v-up  /*v-runqty*/
       fg-rctd.pur-uom    = itemfg.prod-uom
       fg-rctd.cost-uom   = itemfg.prod-uom
       fg-rctd.std-cost   = job-hdr.std-tot-cost
       fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
       fg-rctd.qty-case   = itemfg.case-count
       fg-rctd.partial    = fg-rctd.t-qty MODULO itemfg.case-count
       fg-rctd.cases      = trunc(fg-rctd.t-qty / itemfg.case-count,0)
       fg-rctd.cases-unit = 1.

      IF fg-rctd.t-qty LE 0 THEN fg-rctd.cases = 0.

      RELEASE fg-bin.
      
      FIND FIRST b-reftable
          WHERE b-reftable.reftable EQ "ts/jobdata.p"
            AND b-reftable.company  EQ company_code
            AND b-reftable.code     EQ STRING(RECID(job-hdr))
          NO-LOCK NO-ERROR.

      IF AVAIL b-reftable THEN 
      FIND FIRST fg-bin WHERE RECID(fg-bin) EQ INT(b-reftable.code2) NO-LOCK NO-ERROR.
      
      IF AVAIL fg-bin THEN
        ASSIGN
         v-loc       = fg-bin.loc
         v-loc-bin   = fg-bin.loc-bin
         fg-rctd.tag = fg-bin.tag.
                
      ELSE
        RUN fg/autopost.p (ROWID(itemfg), fg-rctd.job-no, fg-rctd.job-no2,
                           OUTPUT v-loc, OUTPUT v-loc-bin).

      ASSIGN
       fg-rctd.loc     = v-loc
       fg-rctd.loc-bin = v-loc-bin.

      FIND FIRST fg-bin
          WHERE fg-bin.company EQ fg-rctd.company
            AND fg-bin.i-no    EQ fg-rctd.i-no
            AND fg-bin.job-no  EQ job-hdr.job-no
            AND fg-bin.job-no2 EQ job-hdr.job-no2
            AND fg-bin.loc     EQ fg-rctd.loc
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin
            AND fg-bin.tag     EQ fg-rctd.tag
          NO-LOCK NO-ERROR.

      IF AVAIL fg-bin AND fg-bin.cases-unit <> 0 THEN fg-rctd.cases-unit = fg-bin.cases-unit.

      RUN fg/comprcpt.p (ROWID(fg-rctd)).
    END. /* v-assembled */
/*====
    ELSE DO:
       FOR EACH job-hdr WHERE job-hdr.company   EQ company_code
          AND job-hdr.job-no    EQ job-mch.job-no
          AND job-hdr.job-no2   EQ job-mch.job-no2         
          AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
          AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ) :

          FIND first itemfg where itemfg.company    eq company_code
                        and itemfg.i-no       eq job-hdr.i-no
                        and itemfg.case-count gt 0
                        NO-LOCK NO-ERROR.
          IF NOT AVAIL itemfg THEN NEXT.
          {addon/touch/jobrcpt.i}
===    END.
  END.  /* for each job-hdr */
===    */

   /* end of fg receipt creation */
 
  RELEASE fg-rctd.
  RELEASE job.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-tsparts s-object 
PROCEDURE proc-tsparts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE INPUT PARAM v-today AS DATE NO-UNDO. 

     DEF BUFFER b-eb FOR eb.
     FIND FIRST job-hdr WHERE job-hdr.company EQ company_code
                              AND job-hdr.job-no  EQ job_number
                              AND job-hdr.job-no2 EQ INTEGER(job_sub) NO-LOCK NO-ERROR.
     IF AVAIL job-hdr THEN 
        FIND FIRST est WHERE est.company = company_code
                         AND est.est-no = job-hdr.est-no NO-LOCK NO-ERROR.
     IF AVAIL est AND est.est-type = 6 THEN DO: /* TSPARTS is only for Corrugated set estimate*/
         FIND FIRST eb WHERE eb.company = est.company
                         AND eb.est-no = est.est-no 
                         AND eb.form-no > 0 NO-LOCK NO-ERROR.
         IF AVAIL eb AND 
            CAN-FIND(FIRST b-eb WHERE b-eb.company = est.company
                                  AND b-eb.est-no = est.est-no 
                                  AND b-eb.form-no > 0 
                                  AND RECID(b-eb) <> RECID(eb))
         THEN DO: /* don't create fg-rctd if it's not a real set*/
             FIND LAST job-mch WHERE job-mch.company EQ company_code
                                    /*AND job-mch.job     EQ pc-prdd.job*/
                                    AND job-mch.job-no  EQ job_number
                                    AND job-mch.job-no2 EQ INTEGER(job_sub)
                                    AND job-mch.frm    EQ integer(form_number)                                 
                     USE-INDEX line-idx NO-LOCK NO-ERROR.
             IF AVAIL job-mch AND lookup(job-mch.m-code,tspostfg-char) > 0 /*AND tspostfg-int = 1 */
             THEN DO:  /* task for setheader receipt */
                FIND PREV job-mch WHERE job-mch.company EQ company_code
                                    /*AND job-mch.job     EQ pc-prdd.job*/
                                    AND job-mch.job-no  EQ job_number
                                    AND job-mch.job-no2 EQ INTEGER(job_sub)
                                    AND job-mch.frm    EQ integer(form_number)                                 
                        USE-INDEX line-idx NO-LOCK NO-ERROR.
                IF AVAIL job-mch AND job-mch.m-code = machine_code THEN DO:
                   IF tspostfg-log THEN RUN proc-form-cmplt(INPUT v-today).
                END.
             END.
    
             /* form closing procedure only for last machine */
             FIND LAST job-mch WHERE job-mch.company EQ company_code
                                    /*AND job-mch.job     EQ pc-prdd.job*/
                                    AND job-mch.job-no  EQ job_number
                                    AND job-mch.job-no2 EQ INTEGER(job_sub)
                                    AND job-mch.frm    EQ integer(form_number)                                 
                     USE-INDEX line-idx NO-LOCK NO-ERROR.
             IF AVAIL job-mch AND job-mch.m-code = machine_code THEN DO:
                IF LOOKUP(machine_code,tspostfg-char) > 0 /*AND tspostfg-int = 1*/ THEN DO:  /* mods for set header receipts */
                     /*IF tspostfg-log THEN run proc-set-cmplt. */
                END.
                ELSE DO:
                     IF tspostfg-log THEN RUN proc-form-cmplt(INPUT v-today).
                END.  /* components receipt */
             END.      
         END.
     END.  /* est.est-type = 6*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reset_Field_Colors s-object 
PROCEDURE Reset_Field_Colors :
/*------------------------------------------------------------------------------
  Purpose:     Reset all data field colors to initial settings
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      h_field = time-hour:HANDLE
      time-hour:FGCOLOR = 15
      time-hour:BGCOLOR = 0
      h_field = time-minute:HANDLE
      time-minute:FGCOLOR = 15
      time-minute:BGCOLOR = 0
      h_field = run-qty:HANDLE
      run-qty:FGCOLOR = 15
      run-qty:BGCOLOR = 0
      h_field = waste-qty:HANDLE
      waste-qty:FGCOLOR = 15
      waste-qty:BGCOLOR = 0
      .
    IF tskey-log EQ NO THEN
       ASSIGN time-hour:SENSITIVE = NO
              time-minute:SENSITIVE = NO
              run-qty:SENSITIVE = NO
              waste-qty:SENSITIVE = NO
              .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set_Field_Colors s-object 
PROCEDURE Set_Field_Colors :
/*------------------------------------------------------------------------------
  Purpose:     Set field colors to show field has focus
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
        field_value = ''
        h_field:FGCOLOR = 15
        h_field:BGCOLOR = 4 /* dark red */
        h_field:BGCOLOR = 3 /* tiel */
        .
    
     IF tskey-log EQ NO THEN
     DO:
        h_field:SENSITIVE = YES.
        APPLY "ENTRY":U TO h_field.
     END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE touch-finish s-object 
PROCEDURE touch-finish :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER bf-machtran FOR machtran .

    {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
    {methods/run_link.i "CONTAINER" "Get_Value" "('machine_code',OUTPUT machine_code)"}
    {methods/run_link.i "CONTAINER" "Get_Value" "('job_number',OUTPUT job_number)"}
    {methods/run_link.i "CONTAINER" "Get_Value" "('job_sub',OUTPUT job_sub)"}
    {methods/run_link.i "CONTAINER" "Get_Value" "('form_number',OUTPUT form_number)"}
    {methods/run_link.i "CONTAINER" "Get_Value" "('blank_number',OUTPUT blank_number)"}
    {methods/run_link.i "CONTAINER" "Get_Value" "('pass_sequence',OUTPUT pass_sequence)"}
    {methods/run_link.i "CONTAINER" "Get_Value" "('charge_code',OUTPUT charge_code)"}    

    FOR EACH bf-machtran
        WHERE bf-machtran.company       EQ company_code
          AND bf-machtran.machine       EQ machine_code
          AND bf-machtran.job_number    EQ job_number
          AND bf-machtran.job_sub       EQ INTEGER(job_sub)
          AND bf-machtran.form_number   EQ INTEGER(form_number)
          AND bf-machtran.blank_number  EQ INTEGER(blank_number)
          AND bf-machtran.pass_sequence EQ INTEGER(pass_sequence)
        :
        bf-machtran.complete = v-completed.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-plate-die s-object 
PROCEDURE update-plate-die :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid    AS   ROWID NO-UNDO.
  DEF INPUT PARAM ip-upd-type AS   CHAR NO-UNDO.
  DEF INPUT PARAM ip-est-type LIKE est.est-type NO-UNDO.
  DEF BUFFER b-machtran FOR machtran.

  

  ASSIGN cocode = company_code
         locode = job.loc.
  FIND b-machtran WHERE ROWID(b-machtran) = ip-rowid NO-LOCK NO-ERROR.
  IF AVAIL b-machtran THEN DO:

  /*
  DEF BUFFER b-pc-prdd FOR pc-prdd.
  DEF BUFFER bf-job FOR job.
  
  FIND b-pc-prdd WHERE ROWID(b-pc-prdd) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-pc-prdd THEN
  FOR FIRST bf-job
      WHERE bf-job.company eq cocode
        AND bf-job.job     eq b-pc-prdd.job
        AND bf-job.job-no  eq b-pc-prdd.job-no
        AND bf-job.job-no2 eq b-pc-prdd.job-no2
      NO-LOCK,

      FIRST job-hdr
      WHERE job-hdr.company   EQ cocode
        AND job-hdr.job       EQ b-pc-prdd.job
        AND job-hdr.job-no    EQ b-pc-prdd.job-no
        AND job-hdr.job-no2   EQ b-pc-prdd.job-no2
        AND (job-mch.frm      EQ b-pc-prdd.frm OR
             ip-est-type      EQ 2)
      NO-LOCK:

    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ job-hdr.i-no
        NO-LOCK NO-ERROR.

    IF ip-est-type EQ 2 AND job.est-no NE "" AND
       AVAIL itemfg AND itemfg.isaset        THEN
    FOR EACH eb
        WHERE eb.company EQ cocode
          AND eb.est-no  EQ bf-job.est-no
          AND eb.form-no EQ b-pc-prdd.frm
        NO-LOCK,
        FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ eb.stock-no
        NO-LOCK:
      LEAVE.
    END.
*/
    IF AVAIL itemfg THEN DO:
      IF ip-upd-type EQ "P" AND itemfg.plate-no NE "" THEN
      FIND FIRST prep
          WHERE prep.company EQ cocode
            AND prep.code    EQ itemfg.plate-no
          NO-ERROR.

      ELSE
      IF ip-upd-type EQ "D" AND itemfg.die-no NE "" THEN
      FIND FIRST prep
          WHERE prep.company EQ cocode
            AND prep.code    EQ itemfg.die-no
          NO-ERROR.

      IF AVAIL prep THEN prep.no-of-impressions = prep.no-of-impressions +
                                                  b-machtran.run_qty +
                                                  b-machtran.waste_qty.        
        RELEASE prep.
      
    END.
  END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRouting s-object 
PROCEDURE updateRouting :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  {touch/updateRouting.i}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fTotalTime s-object
FUNCTION fTotalTime RETURNS INTEGER 
  (ipdtStartDate AS DATE,
    ipdtEndDate AS DATE,
    ipiStartTime AS INTEGER,
    ipiEndTime AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iTotalTime AS INTEGER NO-UNDO.
    
    IF ipdtStartDate EQ ipdtEndDate THEN
        iTotalTime = ipiEndTime - ipiStartTime.
    ELSE
        iTotalTime = (86400 - ipiStartTime)
                   + (ipdtEndDate - ipdtStartDate - 1) * 86400
                   +  ipiEndTime
                   .
    /*if end_date is blank, set total_time to 0*/
    IF iTotalTime LT 0 OR iTotalTime EQ ? THEN
        iTotalTime = 0.
    
    RETURN iTotalTime.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNumForms s-object 
FUNCTION getNumForms RETURNS INTEGER
  (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
 DEFINE VARIABLE iFormCnt AS INTEGER NO-UNDO.
 DEFINE VARIABLE itemlist AS CHARACTER NO-UNDO.
{methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
{methods/run_link.i "CONTAINER" "Get_Value" "('job_number',OUTPUT job_number)"}
{methods/run_link.i "CONTAINER" "Get_Value" "('job_sub',OUTPUT job_sub)"}
{methods/run_link.i "CONTAINER" "Get_Value" "('machine_code',OUTPUT machine_code)"}
{methods/run_link.i "CONTAINER" "Get_Value" "('machine_list',OUTPUT machine_list)"}

    iFormCnt = 0.
    itemlist = "".

  FIND FIRST job WHERE job.company = company_code
                   AND job.job-no = job_number
                   AND job.job-no2 = INTEGER(job_sub)
                 NO-LOCK NO-ERROR.
  IF AVAILABLE job THEN
  FOR EACH job-mch NO-LOCK WHERE job-mch.company = company_code
                             AND (job-mch.m-code = machine_code OR
                                  LOOKUP(job-mch.m-code,machine_list) > 0)
                             AND job-mch.job = job.job
                  BREAK BY job-mch.frm BY job-mch.blank-no:

              IF FIRST-OF(job-mch.blank-no) THEN DO:
                 IF NOT CAN-FIND(FIRST cmpltjob WHERE cmpltjob.company = company_code
                                  AND cmpltjob.machine = machine_code
                                  AND cmpltjob.job_number = job-mch.job-no
                                  AND cmpltjob.job_sub = job-mch.job-no2
                                  AND cmpltjob.form_number = job-mch.frm
                                  AND cmpltjob.blank_number = job-mch.blank-no)
                 THEN DO:
                    IF INDEX(itemlist,STRING(job-mch.frm)) = 0 THEN 
                       ASSIGN itemlist = IF itemlist = '' THEN 'FORM: ' + STRING(job-mch.frm)
                                  ELSE itemlist + '@' + 'FORM: ' + STRING(job-mch.frm)
                              iFormCnt = iFormCnt + 1.                 
                 END.
              END.
  END.    

    RETURN iFormCnt.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTimerStatus s-object 
FUNCTION setTimerStatus RETURNS CHARACTER
  (ipStatus AS LOGICAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN 'Auto Timer ' + STRING(ipStatus,'Off/On').

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ftsDockSec s-object
FUNCTION ftsDockSec RETURNS LOGICAL 
  (ipiTime AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN tsdocksec-log AND SUBSTRING(STRING(ipiTime,"HH:MM:SS"),7,2) EQ "59".

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


