{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/{&Board}/calcEnd.i}

DEFINE VARIABLE lvVorneDat AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneFile AS CHARACTER NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE lvVorneData AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneResource AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneJob AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneRun AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneForm AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneStart AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneStartDate AS DATE NO-UNDO.
DEFINE VARIABLE lvVorneStartTime AS INTEGER NO-UNDO.
DEFINE VARIABLE lvVorneEnd AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneEndDate AS DATE NO-UNDO.
DEFINE VARIABLE lvVorneEndTime AS INTEGER NO-UNDO.
DEFINE VARIABLE lvVorneDuration AS DECIMAL NO-UNDO.
DEFINE VARIABLE lvVorneMRRunQty AS INTEGER NO-UNDO.
DEFINE VARIABLE lvVorneRunQty AS INTEGER NO-UNDO.
DEFINE VARIABLE lvVorneRejectQty AS INTEGER NO-UNDO.
DEFINE VARIABLE lvVorneState AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneTranRunQty AS INTEGER NO-UNDO.
DEFINE VARIABLE lvVorneTranRejectQty AS INTEGER NO-UNDO.
DEFINE VARIABLE lvVorneLastName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneFirstName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvAttrList AS CHARACTER NO-UNDO FORMAT 'x(4)'.
DEFINE VARIABLE lvFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvTemp AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProcessed AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobMchRowID AS ROWID NO-UNDO.
DEFINE VARIABLE lvShifts AS CHARACTER NO-UNDO INIT 'First,Second,Third,Fourth,Fifth,Sixth'.
DEFINE VARIABLE lvResourceList AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvEmpLogin AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvTotalTime AS INTEGER NO-UNDO.
DEFINE VARIABLE lvDateLoop AS DATE NO-UNDO.

DEFINE TEMP-TABLE ttblMachTran LIKE machtran.

DEFINE TEMP-TABLE ttblVorne NO-UNDO
  FIELD vorneResource AS CHAR
  FIELD vorneJob AS CHAR
  FIELD vorneItem AS CHAR
  FIELD vorneSeq AS INT
  FIELD vorneStartDate AS DATE
  FIELD vorneStartTime AS INT
  FIELD vorneEndDate AS DATE
  FIELD vorneEndTime AS INT
  FIELD vorneDuration AS DEC
  FIELD vorneReason AS CHAR
  FIELD vorneState# AS INT
  FIELD vorneState AS CHAR
  FIELD vorneRunQty AS INT
  FIELD vorneRejectQty AS INT
  FIELD vorneEmployee AS CHAR
  FIELD vorneShift AS CHAR
  FIELD vorneTranRunQty AS INT
  FIELD vorneTranRejectQty AS INT
    INDEX ttblVorneDetail IS PRIMARY
          vorneResource
          vorneJob
          vorneItem
          vorneSeq
    INDEX ttblVorneSummary
          vorneResource
          vorneJob
          vorneItem
          vorneState#
          vorneShift
          vorneSeq
    INDEX vorneSeq
          vorneSeq
    INDEX ttblEmpLogin
          vorneResource
          vorneShift
          vorneEmployee
          vorneStartDate
          vorneStartTime
          vorneEndDate
          vorneEndTime
          .

DEFINE STREAM sVorne.

DEFINE TEMP-TABLE buffMachTran LIKE machtran.
DEFINE TEMP-TABLE ttblEmpLogin LIKE emplogin.
DEFINE TEMP-TABLE ttblEmployee LIKE employee.
CREATE ttblEmployee.
ASSIGN
  ttblEmployee.company = '001'
  ttblEmployee.employee = '900'
  ttblEmployee.first_name = 'Ryan'
  ttblEmployee.last_name = 'Thomas'
  .
CREATE ttblEmployee.
ASSIGN
  ttblEmployee.company = '001'
  ttblEmployee.employee = '912'
  ttblEmployee.first_name = 'Craig'
  ttblEmployee.last_name = 'Weston'
  .
CREATE ttblEmployee.
ASSIGN
  ttblEmployee.company = '001'
  ttblEmployee.employee = '544'
  ttblEmployee.first_name = 'Elton'
  ttblEmployee.last_name = 'Schafer'
  .
CREATE ttblEmployee.
ASSIGN
  ttblEmployee.company = '001'
  ttblEmployee.employee = '222'
  ttblEmployee.first_name = 'Lawrence'
  ttblEmployee.last_name = 'Bishop'
  .
CREATE ttblEmployee.
ASSIGN
  ttblEmployee.company = '001'
  ttblEmployee.employee = '333'
  ttblEmployee.first_name = 'Mark'
  ttblEmployee.last_name = 'Stevens'
  .

lvVorneDat = 'C:\scheduler\schedule\data\ASI\Folding\Vorne.dat'.

INPUT FROM VALUE(lvVorneDat) NO-ECHO.
IMPORT lvVorneDir.
IMPORT lvVorneType.
IMPORT lvEmpLogin.
IMPORT lvResourceList.
INPUT CLOSE.

INPUT FROM OS-DIR(lvVorneDir) NO-ECHO.
REPEAT:
  SET lvVorneFile ^ lvAttrList.
  IF lvAttrList NE 'f' OR INDEX(lvVorneFile,'.psv') EQ 0 THEN NEXT.
  /* create temp-table vorne records */
  RUN createTtblVorne.
  /* calculate qty values for each transaction */
  RUN calcQtyPerTrans.
  /* manually create ttblEmpLogin records if needed */
  /*
  IF lvEmpLogin EQ 'Vorne' THEN
  RUN createEmpLogin.
  */
  /* run Detail or Summary */
  RUN VALUE('vorne' + lvVorneType).
END. /* repeat */
INPUT CLOSE.

PROCEDURE createEmpLogin:
  DEFINE PARAMETER BUFFER ttblVorne FOR ttblVorne.
  DEFINE PARAMETER BUFFER ttblMachTran FOR ttblMachTran.

  DEFINE OUTPUT PARAMETER opEmpLoginRowID AS ROWID NO-UNDO.

  IF ttblVorne.vorneEmployee EQ '' OR
     NUM-ENTRIES(ttblVorne.vorneEmployee,' ') LT 2 THEN RETURN.
  ASSIGN
    lvVorneFirstName = ENTRY(1,ttblVorne.vorneEmployee,' ')
    lvVorneLastName = ENTRY(2,ttblVorne.vorneEmployee,' ')
    .
  FIND FIRST ttblEmployee NO-LOCK
       WHERE ttblEmployee.last_name EQ lvVorneLastName
         AND ttblEmployee.first_name EQ lvVorneFirstName
       NO-ERROR.
  IF AVAILABLE ttblEmployee THEN DO:
    CREATE ttblEmpLogin.
    ASSIGN
      ttblEmpLogin.company = ttblEmployee.company
      ttblEmpLogin.employee = ttblEmployee.employee
      ttblEmpLogin.machine = ttblVorne.vorneResource
      ttblEmpLogin.start_date = ttblMachTran.start_date
      ttblEmpLogin.start_time = ttblMachTran.start_time
      ttblEmpLogin.end_date = ttblMachTran.end_date
      ttblEmpLogin.end_time = ttblMachTran.end_time
      ttblEmpLogin.total_time = ttblMachTran.total_time
      ttblEmpLogin.shift = ttblVorne.vorneShift
      opEmpLoginRowID = ROWID(ttblEmpLogin)
      .
  END. /* avail employee */
END PROCEDURE.

PROCEDURE createEmpLogin_x:
  DEFINE PARAMETER BUFFER ttblVorne FOR ttblVorne.
  DEFINE PARAMETER BUFFER machtran FOR machtran.

  DEFINE VARIABLE duration AS DECIMAL NO-UNDO.

  FOR EACH ttblVorne USE-INDEX ttblEmpLogin
      WHERE ttblVorne.vorneEmployee NE ''
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneShift
            BY ttblVorne.vorneEmployee
      :
    IF FIRST-OF(ttblVorne.vorneEmployee) THEN DO:
      ASSIGN
        lvVorneFirstName = ENTRY(1,ttblVorne.vorneEmployee,' ')
        lvVorneLastName = ENTRY(2,ttblVorne.vorneEmployee,' ')
        .
      FIND FIRST ttblEmployee NO-LOCK
           WHERE ttblEmployee.last_name EQ lvVorneLastName
             AND ttblEmployee.first_name EQ lvVorneFirstName
           NO-ERROR.
      IF AVAILABLE ttblEmployee THEN DO:
          FIND FIRST ttblEmpLogin
               WHERE ttblEmpLogin.company EQ ttblEmployee.company
                 AND ttblEmpLogin.employee EQ ttblEmployee.employee
                 AND ttblEmpLogin.start_date EQ ttblVorne.vorneStartDate
                 AND ttblEmpLogin.machine EQ ttblVorne.vorneResource
                 AND ttblEmpLogin.shift EQ ttblVorne.vorneShift NO-ERROR.
          IF NOT AVAILABLE ttblEmpLogin THEN DO:
            CREATE ttblEmpLogin.
            ASSIGN
              ttblEmpLogin.company = ttblEmployee.company
              ttblEmpLogin.employee = ttblEmployee.employee
              ttblEmpLogin.machine = ttblVorne.vorneResource
              ttblEmpLogin.start_date = ttblVorne.vorneStartDate
              ttblEmpLogin.start_time = ttblVorne.vorneStartTime
              ttblEmpLogin.shift = ttblVorne.vorneShift
              .
          END. /* not avail */
      END. /* avail employee */
    END. /* first-of */
    /*
    IF LAST-OF(ttblVorne.vorneEmployee) THEN DO:
      IF AVAILABLE emplogin THEN
      ASSIGN
        emplogin.end_date = ttblVorne.vorneEndDate
        emplogin.end_time = ttblVorne.vorneEndTime
        emplogin.total_time = IF emplogin.end_time GT emplogin.start_time THEN 
                              emplogin.end_time - emplogin.start_time
                              ELSE 86400 - emplogin.start_time + emplogin.end_time
        .
    END. /* last-of */
    */
    duration = duration + ttblVorne.vorneDuration.
    IF LAST-OF(ttblVorne.vorneEmployee) AND AVAILABLE ttblEmpLogin THEN DO:
      duration = duration / 3600.
      RUN calcEnd (ttblEmpLogin.start_date,ttblEmpLogin.start_time,0,duration,
                   OUTPUT ttblEmpLogin.end_date,OUTPUT ttblEmpLogin.end_time).
      ttblEmpLogin.total_time = duration.
    END. /* last-of */
  END. /* each ttblvorne */
END PROCEDURE.

PROCEDURE createTtblVorne:
  EMPTY TEMP-TABLE ttblVorne.
  ASSIGN
    lvFile = lvVorneDir + '/' + lvVorneFile
    lvTemp = REPLACE(lvFile,'.psv','.tmp')
    lvProcessed = lvVorneDir + '/processed/'
                + REPLACE(lvVorneFile,'.psv','.'
                + STRING(YEAR(TODAY),'9999')
                + STRING(MONTH(TODAY),'99')
                + STRING(DAY(TODAY),'99')
                + '.' + STRING(TIME,'99999')
                + '.psv')
    .
  lvTemp = lvFile.
  INPUT STREAM sVorne FROM VALUE(lvTemp).
  REPEAT:
    IMPORT STREAM sVorne UNFORMATTED lvVorneData.
    IF ENTRY(2,lvVorneData,'|') EQ 'null' THEN NEXT.
    IF lvResourceList NE '' AND NOT CAN-DO(lvResourceList,ENTRY(1,lvVorneData,'|')) THEN NEXT.
    ASSIGN
      lvVorneResource = ENTRY(1,lvVorneData,'|')
      lvVorneJob = LEFT-TRIM(ENTRY(2,lvVorneData,'|'))
      lvVorneRun = ENTRY(2,lvVorneJob,'-')
      lvVorneForm = ENTRY(3,lvVorneJob,'-')
      lvVorneJob = ENTRY(1,lvVorneJob,'-')
      lvVorneJob = lvVorneJob + '-'
                 + STRING(INT(lvVorneRun)) + '.'
                 + STRING(INT(lvVorneForm))
      .
    CREATE ttblVorne.
    ASSIGN
      ttblVorne.vorneResource = lvVorneResource
      ttblVorne.vorneJob = lvVorneJob
      ttblVorne.vorneItem = ENTRY(3,lvVorneData,'|')
      ttblVorne.vorneSeq = INT(ENTRY(4,lvVorneData,'|'))
      lvVorneStart = ENTRY(5,lvVorneData,'|')
      lvVorneStart = SUBSTR(lvVorneStart,1,10)
      ttblVorne.vorneStartDate = DATE(INT(ENTRY(2,lvVorneStart,'-')),
                                      INT(ENTRY(3,lvVorneStart,'-')),
                                      INT(ENTRY(1,lvVorneStart,'-')))
      lvVorneStart = ENTRY(5,lvVorneData,'|')
      lvVorneStart = SUBSTR(lvVorneStart,12,8)
      ttblVorne.vorneStartTime = INT(ENTRY(1,lvVorneStart,':')) * 3600
                               + INT(ENTRY(2,lvVorneStart,':')) * 60
                               + INT(ENTRY(3,lvVorneStart,':'))
      lvVorneEnd = ENTRY(6,lvVorneData,'|')
      lvVorneEnd = SUBSTR(lvVorneEnd,1,10)
      ttblVorne.vorneEndDate = DATE(INT(ENTRY(2,lvVorneEnd,'-')),
                                    INT(ENTRY(3,lvVorneEnd,'-')),
                                    INT(ENTRY(1,lvVorneEnd,'-')))
      lvVorneEnd = ENTRY(6,lvVorneData,'|')
      lvVorneEnd = SUBSTR(lvVorneEnd,12,8)
      ttblVorne.vorneEndTime = INT(ENTRY(1,lvVorneEnd,':')) * 3600
                             + INT(ENTRY(2,lvVorneEnd,':')) * 60
                             + INT(ENTRY(3,lvVorneEnd,':'))
      ttblVorne.vorneDuration = DEC(ENTRY(7,lvVorneData,'|'))
      ttblVorne.vorneReason = ENTRY(8,lvVorneData,'|')
      lvVorneState = ENTRY(9,lvVorneData,'|')
      lvVorneState = REPLACE(lvVorneState,'_enum','')
      ttblVorne.vorneState# = IF CAN-DO('run,down',lvVorneState) THEN 2
                         ELSE IF lvVorneState EQ 'setup' THEN 1
                         ELSE 3
      ttblVorne.vorneState = IF CAN-DO('run,down',lvVorneState) THEN 'RUN'
                        ELSE IF lvVorneState EQ 'setup' THEN 'MR'
                        ELSE 'NC'
      ttblVorne.vorneRunQty = INT(ENTRY(10,lvVorneData,'|'))
      ttblVorne.vorneRejectQty = INT(ENTRY(11,lvVorneData,'|'))
      ttblVorne.vorneEmployee = ENTRY(12,lvVorneData,'|')
      ttblVorne.vorneShift = ENTRY(1,ENTRY(13,lvVorneData,'|'),' ')
      ttblVorne.vorneShift = STRING(LOOKUP(ttblVorne.vorneShift,lvShifts))
      .
    /* cannot have a midnight ending time */
    IF ttblVorne.vorneEndTime EQ 0 THEN
    ttblVorne.vorneEndTime = 86340.
  END. /* repeat */
  INPUT STREAM sVorne CLOSE.
END PROCEDURE.

PROCEDURE vorneSummary:
  DEFINE VARIABLE empLoginRowID AS ROWID NO-UNDO.

  lvVorneStartDate = ?.
  FOR EACH ttblVorne
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneStartDate
            BY ttblVorne.vorneShift
            BY ttblVorne.vorneEmployee
            BY ttblVorne.vorneJob
            BY ttblVorne.vorneItem
            BY ttblVorne.vorneState#
            BY ttblVorne.vorneSeq
            :
    IF lvVorneStartDate EQ ? THEN
    ASSIGN
      lvVorneStartDate = ttblVorne.vorneStartDate
      lvVorneStartTime = ttblVorne.vorneStartTime
      .
    IF FIRST-OF(ttblVorne.vorneJob) THEN DO:
      /*
      FIND FIRST ttblJob
           WHERE ttblJob.resource EQ ttblVorne.vorneResource
             AND ttblJob.job EQ ttblVorne.vorneJob NO-ERROR.
      IF AVAILABLE ttblJob THEN
      jobMchRowID = TO-ROWID(ENTRY(2,ttblJob.rowIDs)).
      ELSE DO:
        FIND FIRST pendingJob
             WHERE pendingJob.resource EQ ttblVorne.vorneResource
               AND pendingJob.job EQ ttblVorne.vorneJob NO-ERROR.
        IF NOT AVAILABLE pendingJob THEN NEXT.
        jobMchRowID = TO-ROWID(ENTRY(2,pendingJob.rowIDs)).
      END. /* not avail ttbljob */
      FIND job-mch NO-LOCK WHERE ROWID(job-mch) EQ jobMchRowID NO-ERROR.
      IF NOT AVAILABLE job-mch THEN NEXT.
      */
    END. /* first-of job */
    IF FIRST-OF(ttblVorne.vorneState#) OR FIRST-OF(ttblVorne.vorneShift) THEN DO:
      ASSIGN
        lvVorneDuration = 0
        lvVorneTranRunQty = 0
        lvVorneTranRejectQty = 0
        /*
        lvVorneStartDate = ttblVorne.vorneStartDate
        lvVorneStartTime = ttblVorne.vorneStartTime
        */
        .
    END. /* first-of state or shift */

    IF ttblVorne.vorneState NE 'RUN' THEN
    ASSIGN
      lvVorneMRRunQty = lvVorneMRRunQty + ttblVorne.vorneTranRunQty
      ttblVorne.vorneTranRunQty = 0
      .
    ASSIGN
      lvVorneTranRunQty = lvVorneTranRunQty + ttblVorne.vorneTranRunQty
      lvVorneTranRejectQty = lvVorneTranRejectQty + ttblVorne.vorneTranRejectQty
      lvVorneDuration = lvVorneDuration + ttblVorne.vorneDuration
      .
    IF LAST-OF(ttblVorne.vorneState#) OR LAST-OF(ttblVorne.vorneShift) THEN DO:
      IF lvVorneDuration LE 60 AND
         lvVorneTranRunQty EQ 0 AND
         lvVorneTranRejectQty EQ 0 THEN NEXT.
      lvVorneDuration = lvVorneDuration / 3600.
      RUN calcEnd (lvVorneStartDate,lvVorneStartTime,0,lvVorneDuration,
                   OUTPUT lvVorneEndDate,OUTPUT lvVorneEndTime).
      /*
      IF lvVorneTranRunQty EQ 0 AND
         lvVorneTranRejectQty EQ 0 AND
         lvVorneStartDate EQ lvVorneEndDate AND
        (lvVorneStartTime EQ lvVorneEndTime OR
         lvVorneStartTime + lvVorneEndTime LE 60) AND
        (lvVorneMRRunQty EQ 0 OR
         lvVorneMRRunQty EQ ?) THEN NEXT.
      */
      DO lvDateLoop = lvVorneStartDate TO lvVorneEndDate:
        CREATE ttblMachTran.
        ASSIGN
          lvVorneJob = LEFT-TRIM(ENTRY(1,ttblVorne.vorneJob,'-'))
          lvVorneRun = ENTRY(2,ttblVorne.vorneJob,'-')
          lvVorneForm = ENTRY(2,ttblVorne.vorneJob,'.')
          ttblMachTran.company = '001'
          ttblMachTran.machine = ttblVorne.vorneResource
          ttblMachTran.job_number = lvVorneJob
          ttblMachTran.job_sub = INT(lvVorneRun)
          ttblMachTran.form_number = INT(lvVorneForm)
          ttblMachTran.blank_number = 0
          ttblMachTran.pass_sequence = 1
          ttblMachTran.charge_code = ttblVorne.vorneState
          ttblMachTran.completed = lvDateLoop EQ lvVorneEndDate AND
                               ttblVorne.vorneReason EQ 'Run Completed'
          ttblMachTran.start_date = lvDateLoop
          ttblMachTran.start_time = lvVorneStartTime WHEN lvDateLoop EQ lvVorneStartDate
          ttblMachTran.end_date = lvDateLoop
          ttblMachTran.end_time = IF lvDateLoop EQ lvVorneEndDate THEN lvVorneEndTime
                                  ELSE 86340
          ttblMachTran.run_qty = lvVorneTranRunQty WHEN lvDateLoop EQ lvVorneEndDate
          ttblMachTran.run_qty = ttblMachTran.run_qty + (IF ttblVorne.vorneState EQ 'RUN' THEN lvVorneMRRunQty ELSE 0)
          ttblMachTran.waste_qty = lvVorneTranRejectQty WHEN lvDateLoop EQ lvVorneEndDate
          ttblMachTran.shift = ttblVorne.vorneShift
          lvVorneMRRunQty = 0 WHEN lvDateLoop EQ lvVorneEndDate
                              AND ttblVorne.vorneState EQ 'RUN'
          .
        IF ttblMachTran.start_date = ttblMachTran.end_date THEN
        ttblMachTran.total_time = ttblMachTran.end_time - ttblMachTran.start_time.
        ELSE
        ttblMachTran.total_time = (86400 - ttblMachTran.start_time) +
                                  (ttblMachTran.end_date - ttblMachTran.start_date - 1) * 86400 +
                                   ttblMachTran.end_time.
        IF ttblMachTran.total_time LT 0 OR ttblMachTran.total_time EQ ? THEN
        ttblMachTran.total_time = 0.
      END. /* do lvdateloop */
      ASSIGN
        lvVorneStartDate = ttblMachTran.end_date
        lvVorneStartTime = ttblMachTran.end_time + 1
        .
    END. /* last-of state or shift */
  END. /* each ttblVorne */
  
  OUTPUT TO '/vorne/results.txt'.
  FOR EACH ttblMachTran
     BREAK BY ttblMachTran.company
           BY ttblMachTran.machine
           BY ttblMachTran.start_date
           BY ttblMachTran.start_time
           BY ttblMachTran.job_number
           BY ttblMachTran.job_sub
           BY ttblMachTran.form_number
           BY ttblMachTran.blank_number
           BY ttblMachTran.pass_sequence
           BY ttblMachTran.shift
           BY ttblMachTran.charge_code
           :
    DISP
      ttblMachTran.company
      ttblMachTran.machine
      ttblMachTran.start_date
      STRING(ttblMachTran.start_time,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'Start Time'
      ttblMachTran.job_number
      ttblMachTran.job_sub
      ttblMachTran.form_number
      ttblMachTran.blank_number
      ttblMachTran.pass_sequence
      ttblMachTran.shift
      ttblMachTran.charge_code
      ttblMachTran.total_time
        WITH WIDTH 200 STREAM-IO.
  END.
  FOR EACH ttblMachTran
     BREAK BY ttblMachTran.company
           BY ttblMachTran.machine
           BY ttblMachTran.start_date
           BY ttblMachTran.start_time
           BY ttblMachTran.job_number
           BY ttblMachTran.job_sub
           BY ttblMachTran.form_number
           BY ttblMachTran.blank_number
           BY ttblMachTran.pass_sequence
           BY ttblMachTran.shift
           BY ttblMachTran.charge_code
           :
    IF FIRST-OF(ttblMachTran.shift) THEN
    ASSIGN
      lvVorneStartDate = ttblMachTran.start_date
      lvVorneStartTime = ttblMachTran.start_time
      .
    IF ttblMachTran.total_time LE 60 AND
       ttblMachTran.run_qty EQ 0 AND
       ttblMachTran.waste_qty EQ 0 THEN NEXT.
    RUN calcEnd (lvVorneStartDate,lvVorneStartTime,0,ttblMachTran.total_time / 3600,
                 OUTPUT ttblMachTran.end_date,OUTPUT ttblMachTran.end_time).
    /*
    IF lvVorneStartDate EQ ttblMachTran.end_date AND
      (lvVorneStartTime EQ ttblMachTran.end_time OR
       lvVorneStartTime + ttblMachTran.end_time LE 60) AND
      (ttblMachTran.run_qty EQ 0 OR
       ttblMachTran.run_qty EQ ?) THEN NEXT.
    */
    CREATE buffMachTran.
    BUFFER-COPY ttblMachTran TO buffMachTran.
    /*
    MESSAGE
      ttblMachTran.shift ttblMachTran.charge_code
      STRING(buffMachTran.total_time,'hh:mm:ss') SKIP
      lvVorneStartDate STRING(lvVorneStartTime,'hh:mm:ss am') SKIP
      buffMachTran.start_date STRING(buffMachTran.start_time,'hh:mm:ss am') SKIP
      buffMachTran.end_date STRING(buffMachTran.end_time,'hh:mm:ss am')
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */
    ASSIGN
      lvVorneStartDate = ttblMachTran.end_date
      lvVorneStartTime = ttblMachTran.end_time
      .
    IF lvEmpLogin EQ 'Vorne' THEN
    RUN createEmpLogin (BUFFER ttblVorne,BUFFER ttblMachTran,OUTPUT empLoginRowID).
    /*
    RUN setRecKey (BUFFER machtran).
    RUN createMachEmp (BUFFER machtran, INPUT empLoginRowID).
    IF machtran.completed THEN
    RUN completeMR.
    */
  END. /* each ttblmachtran */
  OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE calcQtyPerTrans:
  /* calculate qty values for each transaction */
  FOR EACH ttblVorne
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneJob
            BY ttblVorne.vorneItem
            BY ttblVorne.vorneSeq:
    IF FIRST-OF(ttblVorne.vorneJob) THEN DO:
      /*
      FIND FIRST ttblJob
           WHERE ttblJob.resource EQ ttblVorne.vorneResource
             AND ttblJob.job EQ ttblVorne.vorneJob NO-ERROR.
      IF AVAILABLE ttblJob THEN
      jobMchRowID = TO-ROWID(ENTRY(2,ttblJob.rowIDs)).
      ELSE DO:
        FIND FIRST pendingJob
             WHERE pendingJob.resource EQ ttblVorne.vorneResource
               AND pendingJob.job EQ ttblVorne.vorneJob NO-ERROR.
        IF NOT AVAILABLE pendingJob THEN NEXT.
        jobMchRowID = TO-ROWID(ENTRY(2,pendingJob.rowIDs)).
      END. /* not avail ttbljob */
      FIND job-mch NO-LOCK WHERE ROWID(job-mch) EQ jobMchRowID NO-ERROR.
      IF NOT AVAILABLE job-mch THEN NEXT.
      */
      ASSIGN
        lvVorneRunQty = 0
        lvVorneRejectQty = 0
        .
      /*
      FOR EACH machtran NO-LOCK
          WHERE machtran.company EQ job-mch.company
            AND machtran.machine EQ ttblVorne.vorneResource
            AND machtran.job_number EQ job-mch.job-no
            AND machtran.job_sub EQ job-mch.job-no2
            AND machtran.form_number EQ job-mch.frm
            AND machtran.blank_number EQ job-mch.blank-no
            AND machtran.pass_sequence EQ job-mch.pass
            AND machtran.jobseq EQ 0:
        ASSIGN
          lvVorneRunQty = lvVorneRunQty + machtran.run_qty
          lvVorneRejectQty = lvVorneRejectQty + machtran.waste_qty
          .
      END. /* each machtran */
      */
    END. /* first-of job */

    ASSIGN
      ttblVorne.vorneTranRunQty = ttblVorne.vorneRunQty - lvVorneRunQty
      ttblVorne.vorneTranRejectQty = ttblVorne.vorneRejectQty - lvVorneRejectQty
      lvVorneRunQty = ttblVorne.vorneRunQty
      lvVorneRejectQty = ttblVorne.vorneRejectQty
      .
  END. /* each ttblvorne */
END PROCEDURE.

OUTPUT TO '/vorne/results.txt' APPEND.
FOR EACH buffMachTran NO-LOCK
      BY buffMachTran.start_date
      BY buffMachTran.start_time
  :
  DISP
    buffMachTran.shift
    buffMachTran.job_number
    buffMachTran.job_sub
    buffMachTran.form_number
    buffMachTran.blank_number
    buffMachTran.charge_code
    buffMachTran.start_date
    STRING(buffMachTran.start_time,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'Start Time'
    buffMachTran.end_date
    STRING(buffMachTran.end_time,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'End Time'
    buffMachTran.run_qty
    buffMachTran.waste_qty
    buffMachTran.total_time
      WITH WIDTH 200 STREAM-IO.
END.
FOR EACH ttblEmpLogin NO-LOCK
   ,FIRST ttblEmployee NO-LOCK
    WHERE ttblEmployee.company EQ ttblEmpLogin.company
      AND ttblEmployee.employee EQ ttblEmpLogin.employee
  BY ttblEmpLogin.start_date
  BY ttblEmpLogin.start_time
    :
  DISP
    ttblEmpLogin.shift
    ttblEmpLogin.employee
    ttblEmployee.first_name
    ttblEmployee.last_name
    ttblEmpLogin.start_date
    STRING(ttblEmpLogin.start_time,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'Start Time'
    ttblEmpLogin.end_date
    STRING(ttblEmpLogin.end_time,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'End Time'
      WITH WIDTH 200 STREAM-IO.
END.
OUTPUT CLOSE.
OS-COMMAND NO-WAIT notepad.exe \vorne\results.txt.
