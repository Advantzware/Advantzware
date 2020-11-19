/* dmiTran.i - rstark - 9.9.2019 */

/* used in AOA/dynBL/dmiTran.p and schedule/objects/loads/ASI/ProdAce.w */

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE iJobMchID         AS INTEGER   NO-UNDO.
DEFINE VARIABLE jobMchRowID       AS ROWID     NO-UNDO.
DEFINE VARIABLE lvProdAceOperator AS CHARACTER NO-UNDO EXTENT 10.

DEFINE BUFFER buffProdAce FOR ttblProdAce.

/* **********************  Internal Procedures  *********************** */

PROCEDURE completeMR :
  FIND LAST machtran EXCLUSIVE-LOCK
       WHERE machtran.company       EQ job-mch.company
         AND machtran.machine       EQ ttblProdAce.prodAceResource
         AND machtran.job_number    EQ job-mch.job-no
         AND machtran.job_sub       EQ job-mch.job-no2
         AND machtran.form_number   EQ job-mch.frm
         AND machtran.blank_number  EQ job-mch.blank-no
         AND machtran.pass_sequence EQ job-mch.pass
         AND machtran.charge_code   EQ 'MR'
       NO-ERROR.
  IF AVAILABLE machtran THEN
  machtran.completed = YES.
END PROCEDURE.

PROCEDURE createEmpLogin :
  DEFINE PARAMETER BUFFER ttblProdAce FOR ttblProdAce.
  DEFINE PARAMETER BUFFER machtran FOR machtran.

  DEFINE VARIABLE idx AS INTEGER NO-UNDO.

  DO idx = 1 TO EXTENT(lvProdAceOperator):
      IF ttblProdAce.prodAceOperator[idx] EQ '' THEN NEXT.
      FIND FIRST employee NO-LOCK
           WHERE employee.employee EQ STRING(ttblProdAce.prodAceOperator[idx])
           NO-ERROR.
      IF AVAILABLE employee THEN DO:
        IF CAN-FIND(FIRST emplogin
                    WHERE emplogin.company    EQ employee.company
                      AND emplogin.employee   EQ employee.employee
                      AND emplogin.start_date EQ machtran.start_date
                      AND emplogin.start_time EQ machtran.start_time
                      AND emplogin.machine    EQ ttblProdAce.prodAceResource) THEN
        IF idx EQ 1 THEN
        ASSIGN
          machtran.start_time = machtran.start_time + 1
          machtran.total_time = machtran.total_time + 1
          .
        CREATE empLogin.
        ASSIGN
          emplogin.company    = employee.company
          emplogin.employee   = employee.employee
          emplogin.machine    = ttblProdAce.prodAceResource
          emplogin.start_date = machtran.start_date
          emplogin.start_time = machtran.start_time
          emplogin.end_date   = machtran.end_date
          emplogin.end_time   = machtran.end_time
          emplogin.total_time = machtran.total_time
          emplogin.shift      = ttblProdAce.prodAceShift
          .
        RUN createMachEmp (BUFFER machtran, INPUT ROWID(emplogin)).
      END. /* avail employee */
  END. /* do idx */
END PROCEDURE.

PROCEDURE createMachEmp :
  DEFINE PARAMETER BUFFER machtran FOR machtran.

  DEFINE INPUT PARAMETER ipEmpLoginRowID AS ROWID NO-UNDO.

  FIND FIRST emplogin NO-LOCK
       WHERE ROWID(emplogin) EQ ipEmpLoginRowID NO-ERROR.
  IF NOT AVAILABLE emplogin THEN RETURN.

  FIND FIRST employee NO-LOCK
       WHERE employee.company  EQ emplogin.company
         AND employee.employee EQ emplogin.employee
       NO-ERROR.
  IF NOT AVAILABLE employee THEN RETURN.

  CREATE machemp.
  ASSIGN
    machemp.table_rec_key = machtran.rec_key
    machemp.employee      = emplogin.employee
    machemp.start_date    = IF machtran.start_date GT emplogin.start_date THEN machtran.start_date ELSE emplogin.start_date
    machemp.start_time    = IF machtran.start_date GT emplogin.start_date THEN machtran.start_time ELSE emplogin.start_time
    machemp.shift         = machtran.shift
    machemp.ratetype      = 'Standard' 
    machemp.rate_usage    = employee.rate_usage
    machemp.end_date      = IF machtran.end_date LT emplogin.end_date OR emplogin.end_date EQ ? THEN machtran.end_date ELSE emplogin.end_date
    machemp.end_time      = IF machtran.end_date LT emplogin.end_date OR emplogin.end_date EQ ? THEN machtran.end_time ELSE emplogin.end_time
    .
  RUN employeeRate(machtran.company,machemp.employee,machemp.shift,machtran.machine,
                   machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
  IF machemp.start_date EQ machemp.end_date THEN
  machemp.total_time = machemp.end_time - machemp.start_time.
  ELSE
  machemp.total_time = (86400 - machemp.start_time)
                     + (machemp.end_date - machemp.start_date - 1) * 86400
                     +  machemp.end_time
                     .
  /*if end_date is blank, set total_time to 0*/
  IF machemp.total_time LT 0 OR machemp.total_time EQ ? THEN
     machemp.total_time = 0.
END PROCEDURE.

PROCEDURE employeeRate :
  DEFINE INPUT PARAMETER ipCompany    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipEmployee   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipShift      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMachine    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipRate_usage AS LOGICAL   NO-UNDO.
  DEFINE INPUT PARAMETER ipRatetype   AS CHARACTER NO-UNDO.

  DEFINE OUTPUT PARAMETER opRate AS DECIMAL NO-UNDO.

  DEFINE BUFFER bRate FOR rate.

  IF ipRate_usage THEN
  ipMachine = ''.
  FIND bRate NO-LOCK
       WHERE bRate.company  EQ ipCompany
         AND bRate.employee EQ ipEmployee
         AND bRate.shift    EQ ipShift
         AND bRate.machine  EQ ipMachine
         AND bRate.ratetype EQ 'Standard'
       NO-ERROR.
  IF NOT AVAILABLE bRate THEN
  RETURN.
  opRate = bRate.rate.
  FIND bRate NO-LOCK
       WHERE bRate.company  EQ ipCompany
         AND bRate.employee EQ ipEmployee
         AND bRate.shift    EQ ipShift
         AND bRate.machine  EQ ipMachine
         AND bRate.ratetype EQ ipRatetype
       NO-ERROR.
  IF AVAILABLE bRate THEN
  CASE bRate.factortype:
    WHEN 'Straight' THEN
    opRate = bRate.rate.
    WHEN 'Additional' THEN
    opRate = opRate + bRate.rate.
    WHEN 'Multiply' THEN
    opRate = opRate * bRate.rate.
    OTHERWISE
    opRate = 0.
  END CASE.
END PROCEDURE.

PROCEDURE newEnd :
  DEFINE INPUT PARAMETER ipTimeSpan   AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER newStartDate AS DATE    NO-UNDO.
  DEFINE INPUT PARAMETER newStartTime AS INTEGER NO-UNDO.
  
  DEFINE OUTPUT PARAMETER newEndDate AS DATE    NO-UNDO.
  DEFINE OUTPUT PARAMETER newEndTime AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE days AS INTEGER NO-UNDO.
  
  ASSIGN
    newEndTime = newStartTime + ipTimeSpan
    days       = TRUNCATE(newEndTime / 86400,0)
    newEndDate = newStartDate + days
    newEndTime = newEndTime - days * 86400
    .
END PROCEDURE.

PROCEDURE prodAceDetail :
  /* split records that span midnight */
  FOR EACH ttblProdAce
      WHERE ttblProdAce.prodAceSelected EQ YES
      :
    IF ttblProdAce.prodAceStartDate NE ttblProdAce.prodAceEndDate THEN DO:
      CREATE buffProdAce.
      BUFFER-COPY ttblProdAce TO buffProdAce.
      ASSIGN
        buffProdAce.prodAceStartDate     = ttblProdAce.prodAceEndDate
        buffProdAce.prodAceStartTime     = 0
        buffProdAce.prodAceDuration      = buffProdAce.prodAceEndTime - buffProdAce.prodAceStartTime
        ttblProdAce.prodAceEndDate       = ttblProdAce.prodAceStartDate
        ttblProdAce.prodAceEndTime       = 86340
        ttblProdAce.prodAceDuration      = ttblProdAce.prodAceEndTime - ttblProdAce.prodAceStartTime
        ttblProdAce.prodAceTranRunQty    = 0
        ttblProdAce.prodAceTranRejectQty = 0
        .
    END. /* spans midnight */
  END. /* each ttblprodace */
  
  FOR EACH ttblProdAce
      WHERE ttblProdAce.prodAceSelected EQ YES
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceSeq
      :
    IF FIRST-OF(ttblProdAce.prodAceJob) THEN DO:
      &IF DEFINED(dmiTran) EQ 0 &THEN
      FIND FIRST ttblJob
           WHERE ttblJob.resource EQ ttblProdAce.prodAceResource
             AND ttblJob.job      EQ ttblProdAce.prodAceJob
           NO-ERROR.
      IF AVAILABLE ttblJob THEN
      jobMchRowID = TO-ROWID(ENTRY(2,ttblJob.rowIDs)).
      ELSE DO:
        FIND FIRST pendingJob
             WHERE pendingJob.resource EQ ttblProdAce.prodAceResource
               AND pendingJob.job      EQ ttblProdAce.prodAceJob
             NO-ERROR.
        IF NOT AVAILABLE pendingJob THEN NEXT.
        jobMchRowID = TO-ROWID(ENTRY(2,pendingJob.rowIDs)).
      END. /* not avail ttbljob */
      FIND job-mch NO-LOCK WHERE ROWID(job-mch) EQ jobMchRowID NO-ERROR.
      &ELSE
      iJobMchID = INTEGER(ENTRY(2,ttblProdAce.prodAceJob,".")).
      FIND FIRST job-mch NO-LOCK
           WHERE job-mch.job-mchID EQ iJobMchID
           NO-ERROR.
      &ENDIF
    END. /* first-of job */
    IF NOT AVAILABLE job-mch THEN NEXT.
    CREATE machtran.
    ASSIGN
      machtran.company       = job-mch.company
      machtran.machine       = ttblProdAce.prodAceResource
      machtran.job_number    = job-mch.job-no
      machtran.job_sub       = job-mch.job-no2
      machtran.form_number   = job-mch.frm
      machtran.blank_number  = job-mch.blank-no
      machtran.pass_sequence = job-mch.pass
      machtran.charge_code   = ttblProdAce.prodAceState
      machtran.completed     = ttblProdAce.prodAceRunComplete
      machtran.start_date    = ttblProdAce.prodAceStartDate
      machtran.start_time    = ttblProdAce.prodAceStartTime
      machtran.end_date      = ttblProdAce.prodAceEndDate
      machtran.end_time      = ttblProdAce.prodAceEndTime
      machtran.run_qty       = ttblProdAce.prodAceTranRunQty
      machtran.waste_qty     = ttblProdAce.prodAceTranRejectQty
      machtran.shift         = ttblProdAce.prodAceShift
      machtran.total_time    = machtran.end_time - machtran.start_time
      .
    IF machtran.total_time LT 0 OR machtran.total_time EQ ? THEN
    machtran.total_time = 0.

    RUN setRecKey (BUFFER machtran).
    &IF DEFINED(dmiTran) EQ 0 &THEN
    IF lvEmpLogin EQ 'ProdAce' THEN
    &ENDIF
/*    RUN createMachEmp (BUFFER machtran).*/
    RUN createEmpLogin (BUFFER ttblProdAce,BUFFER machtran).
    IF ttblProdAce.prodAceRunComplete THEN
    RUN completeMR.
  END. /* each ttblProdAce */
END PROCEDURE.

PROCEDURE prodAceSummary :
  DEFINE VARIABLE empLoginRowID AS ROWID     NO-UNDO.
  DEFINE VARIABLE lvState       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvTime        AS INTEGER   NO-UNDO.
  
  /* Pass 1: consolidate prodAce transactions */
  FOR EACH ttblProdAce
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceShift
            BY ttblProdAce.prodAceStartDate
            BY ttblProdAce.prodAceSeq
            BY ttblProdAce.prodAceState
      :
    IF FIRST-OF(ttblProdAce.prodAceShift)     OR
       FIRST-OF(ttblProdAce.prodAceStartDate) OR
       lvState NE ttblProdAce.prodAceState THEN DO:
      FIND buffProdAce WHERE ROWID(buffProdAce) EQ ROWID(ttblProdAce).
      lvState = ttblProdAce.prodAceState.
    END. /* first-of */
    IF AVAILABLE buffProdAce AND (ROWID(buffProdAce) NE ROWID(ttblProdAce)) THEN DO:
      ASSIGN
        buffProdAce.prodAceTranRunQty    = buffProdAce.prodAceTranRunQty + ttblProdAce.prodAceTranRunQty
        buffProdAce.prodAceTranRejectQty = buffProdAce.prodAceTranRejectQty + ttblProdAce.prodAceTranRejectQty
        buffProdAce.prodAceDuration      = buffProdAce.prodAceDuration + ttblProdAce.prodAceDuration
/*        buffProdAce.prodAceEndDate       = ttblProdAce.prodAceEndDate*/
/*        buffProdAce.prodAceEndTime       = ttblProdAce.prodAceEndTime*/
        ttblProdAce.deleteFlag           = YES
        .
      RUN newEnd (buffProdAce.prodAceDuration, buffProdAce.prodAceStartDate, buffProdAce.prodAceStartTime,
                  OUTPUT buffProdAce.prodAceEndDate, OUTPUT buffProdAce.prodAceEndTime).
    END. /* avail buffprodAce */
  END. /* each ttblProdAce */

  /* Pass 2: move non-RUN qty values to RUN transaction */
  FOR EACH ttblProdAce
      WHERE ttblProdAce.prodAceState         NE 'RUN'
       AND  ttblProdAce.deleteFlag           EQ NO
       AND (ttblProdAce.prodAceTranRunQty    NE 0
        OR  ttblProdAce.prodAceTranRejectQty NE 0)
      :
    FIND FIRST buffProdAce
         WHERE buffProdAce.prodAceResource EQ ttblProdAce.prodAceResource
           AND buffProdAce.prodAceJob      EQ ttblProdAce.prodAceJob
           AND buffProdAce.prodAceItem     EQ ttblProdAce.prodAceItem
           AND buffProdAce.prodAceShift    EQ ttblProdAce.prodAceShift
           AND buffProdAce.prodAceState    EQ 'RUN'
           AND ttblProdAce.deleteFlag      EQ NO
         NO-ERROR.
    IF AVAILABLE buffProdAce THEN
    ASSIGN
      buffProdAce.prodAceTranRunQty    = buffProdAce.prodAceTranRunQty + ttblProdAce.prodAceTranRunQty
      buffProdAce.prodAceTranRejectQty = buffProdAce.prodAceTranRejectQty + ttblProdAce.prodAceTranRejectQty
      ttblProdAce.prodAceTranRunQty    = 0
      ttblProdAce.prodAceTranRejectQty = 0
      .
  END. /* each ttblProdAce */

  /* Pass 3: flag records as run completed if necessary */
  FOR EACH ttblProdAce
      WHERE ttblProdAce.prodAceState       EQ 'RUN'
        AND ttblProdAce.prodAceRunComplete EQ YES
        AND ttblProdAce.deleteFlag         EQ YES
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceShift
            BY ttblProdAce.prodAceStartDate
            BY ttblProdAce.prodAceSeq
            BY ttblProdAce.prodAceState
      :
    FIND LAST buffProdAce
         WHERE buffProdAce.prodAceResource    EQ ttblProdAce.prodAceResource
           AND buffProdAce.prodAceJob         EQ ttblProdAce.prodAceJob
           AND buffProdAce.prodAceItem        EQ ttblProdAce.prodAceItem
           AND buffProdAce.prodAceShift       EQ ttblProdAce.prodAceShift
           AND buffProdAce.prodAceState       EQ 'RUN'
           AND ttblProdAce.prodAceRunComplete EQ NO
           AND buffProdAce.deleteFlag         EQ NO
         NO-ERROR.
    IF AVAILABLE buffProdAce THEN
    buffProdAce.prodAceRunComplete = YES.
  END. /* each ttblprodAce */

  /* Pass 4: remove deleted flag records */
  FOR EACH ttblProdAce:
    IF ttblProdAce.deleteFlag           EQ YES OR
      (ttblProdAce.prodAceState         EQ 'NC' AND
       ttblProdAce.prodAceDuration      LE 60 AND
       ttblProdAce.prodAceTranRunQty    EQ 0 AND
       ttblProdAce.prodAceTranRejectQty EQ 0 AND
       ttblProdAce.prodAceRunComplete   EQ NO
       ) THEN
    DELETE ttblProdAce.
  END. /* each ttblprodAce */

  /* Pass 5: split records that span midnight */
  FOR EACH ttblProdAce
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceShift
            BY ttblProdAce.prodAceStartDate
            BY ttblProdAce.prodAceSeq
            BY ttblProdAce.prodAceState
      :
    IF ttblProdAce.deleteFlag THEN NEXT.
    IF ttblProdAce.prodAceStartDate NE ttblProdAce.prodAceEndDate THEN DO:
      CREATE buffProdAce.
      BUFFER-COPY ttblProdAce TO buffProdAce.
      ASSIGN
        buffProdAce.prodAceStartDate     = ttblProdAce.prodAceEndDate
        buffProdAce.prodAceStartTime     = 0
        buffProdAce.prodAceDuration      = buffProdAce.prodAceEndTime - buffProdAce.prodAceStartTime
        ttblProdAce.prodAceEndDate       = ttblProdAce.prodAceStartDate
        ttblProdAce.prodAceEndTime       = 86340
        ttblProdAce.prodAceDuration      = ttblProdAce.prodAceEndTime - ttblProdAce.prodAceStartTime
        ttblProdAce.prodAceTranRunQty    = 0
        ttblProdAce.prodAceTranRejectQty = 0
        .
    END. /* spans midnight */
  END. /* each ttblProdAce */

  /* Pass 6: consolidate prodAce transactions again */
  RELEASE buffProdAce.
  lvState = ''.
  FOR EACH ttblProdAce
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceShift
            BY ttblProdAce.prodAceStartDate
            BY ttblProdAce.prodAceSeq
            BY ttblProdAce.prodAceState
      :
    IF FIRST-OF(ttblProdAce.prodAceShift)     OR
       FIRST-OF(ttblProdAce.prodAceStartDate) OR
       lvState NE ttblProdAce.prodAceState THEN DO:
      FIND buffProdAce WHERE ROWID(buffProdAce) EQ ROWID(ttblProdAce).
      lvState = ttblProdAce.prodAceState.
    END. /* first-of */
    IF AVAILABLE buffProdAce AND (ROWID(buffProdAce) NE ROWID(ttblProdAce)) THEN DO:
      ASSIGN
        buffProdAce.prodAceTranRunQty    = buffProdAce.prodAceTranRunQty + ttblProdAce.prodAceTranRunQty
        buffProdAce.prodAceTranRejectQty = buffProdAce.prodAceTranRejectQty + ttblProdAce.prodAceTranRejectQty
        buffProdAce.prodAceDuration      = buffProdAce.prodAceDuration + ttblProdAce.prodAceDuration
/*        buffProdAce.prodAceEndDate       = ttblProdAce.prodAceEndDate*/
/*        buffProdAce.prodAceEndTime       = ttblProdAce.prodAceEndTime*/
        buffProdAce.prodAceRunComplete   = ttblProdAce.prodAceRunComplete
        ttblProdAce.deleteFlag           = YES
        .
      RUN newEnd (buffProdAce.prodAceDuration, buffProdAce.prodAceStartDate, buffProdAce.prodAceStartTime,
                  OUTPUT buffProdAce.prodAceEndDate, OUTPUT buffProdAce.prodAceEndTime).
    END. /* avail buffprodAce */
  END. /* each ttblProdAce */

  /* Pass 7: move RUN qty values to last RUN transaction */
  FOR EACH ttblProdAce
      WHERE ttblProdAce.prodAceState         EQ 'RUN'
       AND  ttblProdAce.deleteFlag           EQ NO
       AND (ttblProdAce.prodAceTranRunQty    NE 0
        OR  ttblProdAce.prodAceTranRejectQty NE 0)
      :
    FIND LAST buffProdAce
         WHERE buffProdAce.prodAceResource EQ ttblProdAce.prodAceResource
           AND buffProdAce.prodAceJob      EQ ttblProdAce.prodAceJob
           AND buffProdAce.prodAceItem     EQ ttblProdAce.prodAceItem
           AND buffProdAce.prodAceShift    EQ ttblProdAce.prodAceShift
           AND buffProdAce.prodAceState    EQ 'RUN'
           AND buffProdAce.deleteFlag      EQ NO
           AND buffProdAce.prodAceSeq      GT ttblProdAce.prodAceSeq
           AND ROWID(buffProdAce)          NE ROWID(ttblProdAce)
         NO-ERROR.
    IF AVAILABLE buffProdAce THEN
    ASSIGN
      buffProdAce.prodAceTranRunQty    = buffProdAce.prodAceTranRunQty + ttblProdAce.prodAceTranRunQty
      buffProdAce.prodAceTranRejectQty = buffProdAce.prodAceTranRejectQty + ttblProdAce.prodAceTranRejectQty
      ttblProdAce.prodAceTranRunQty    = 0
      ttblProdAce.prodAceTranRejectQty = 0
      .
  END. /* each ttblProdAce */

  /* Pass 8: create machtran records */
  FOR EACH ttblProdAce
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceShift
            BY ttblProdAce.prodAceStartDate
            BY ttblProdAce.prodAceSeq
            BY ttblProdAce.prodAceState
      :
    IF ttblProdAce.deleteFlag THEN NEXT.

    IF FIRST-OF(ttblProdAce.prodAceJob) THEN DO:
      lvTime = ?.
      &IF DEFINED(dmiTran) EQ 0 &THEN
      FIND FIRST ttblJob
           WHERE ttblJob.resource EQ ttblProdAce.prodAceResource
             AND ttblJob.job EQ ttblProdAce.prodAceJob NO-ERROR.
      IF AVAILABLE ttblJob THEN
      jobMchRowID = TO-ROWID(ENTRY(2,ttblJob.rowIDs)).
      ELSE DO:
        FIND FIRST pendingJob
             WHERE pendingJob.resource EQ ttblProdAce.prodAceResource
               AND pendingJob.job      EQ ttblProdAce.prodAceJob NO-ERROR.
        IF NOT AVAILABLE pendingJob THEN NEXT.
        jobMchRowID = TO-ROWID(ENTRY(2,pendingJob.rowIDs)).
      END. /* not avail ttbljob */
      FIND job-mch NO-LOCK WHERE ROWID(job-mch) EQ jobMchRowID NO-ERROR.
      &ELSE
      iJobMchID = INTEGER(ENTRY(2,ttblProdAce.prodAceJob,".")).
      FIND FIRST job-mch NO-LOCK
           WHERE job-mch.job-mchID EQ iJobMchID
           NO-ERROR.
      &ENDIF
      IF NOT AVAILABLE job-mch THEN NEXT.
    END. /* first-of job */

    IF lvTime EQ ? THEN
    lvTime = ttblProdAce.prodAceStartTime.

    CREATE machtran.
    ASSIGN
      machtran.company       = job-mch.company
      machtran.machine       = ttblProdAce.prodAceResource
      machtran.job_number    = job-mch.job-no
      machtran.job_sub       = job-mch.job-no2
      machtran.form_number   = job-mch.frm
      machtran.blank_number  = job-mch.blank-no
      machtran.pass_sequence = job-mch.pass
      machtran.charge_code   = ttblProdAce.prodAceState
      machtran.completed     = ttblProdAce.prodAceRunComplete
      machtran.start_date    = ttblProdAce.prodAceStartDate
      machtran.start_time    = ttblProdAce.prodAceStartTime
      machtran.end_date      = ttblProdAce.prodAceEndDate
      machtran.end_time      = ttblProdAce.prodAceEndTime
      machtran.run_qty       = ttblProdAce.prodAceTranRunQty
      machtran.waste_qty     = ttblProdAce.prodAceTranRejectQty
      machtran.shift         = ttblProdAce.prodAceShift
      machtran.total_time    = machtran.end_time - machtran.start_time
      .
    IF machtran.total_time LT 0 OR machtran.total_time EQ ? THEN
    machtran.total_time = 0.

    RUN setRecKey (BUFFER machtran).
    &IF DEFINED(dmiTran) EQ 0 &THEN
    IF lvEmpLogin EQ 'ProdAce' THEN
    &ENDIF
    RUN createEmpLogin (BUFFER ttblProdAce,BUFFER machtran).
    IF ttblProdAce.prodAceRunComplete THEN
    RUN completeMR.
  END. /* each ttblProdAce */
END PROCEDURE.

PROCEDURE setRecKey :
  DEFINE PARAMETER BUFFER machtran FOR machtran.

  {custom/rec_key.i "machtran"}
END PROCEDURE.
