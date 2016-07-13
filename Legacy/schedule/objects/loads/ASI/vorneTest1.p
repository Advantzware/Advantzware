/* vorneTest1.p */


/*
&SCOPED-DEFINE sort1
&SCOPED-DEFINE sort2
&SCOPED-DEFINE sort3
&SCOPED-DEFINE setSBJobs
*/
&SCOPED-DEFINE results

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/ttblJob.i NEW}
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
DEFINE VARIABLE lvVorneBlankEmployee AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvVorneEmployee AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE lvRunComplete AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvPostVorne AS LOGICAL NO-UNDO.

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
  FIELD deleteFlag AS LOG
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

DEFINE BUFFER buffVorne FOR ttblVorne.
  
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
CREATE ttblEmployee.
ASSIGN
  ttblEmployee.company = '001'
  ttblEmployee.employee = '999'
  ttblEmployee.first_name = 'No'
  ttblEmployee.last_name = 'IDScan'
  .

lvVorneDat = 'C:\scheduler\schedule\data\ASI\Folding\Vorne.dat'.

INPUT FROM VALUE(lvVorneDat) NO-ECHO.
IMPORT UNFORMATTED lvVorneDir.           /* location of vorne trans file   */
IMPORT UNFORMATTED lvVorneType.          /* Summary or Detail              */
IMPORT UNFORMATTED lvEmpLogin.           /* Vorne or TS                    */
IMPORT UNFORMATTED lvVorneBlankEmployee. /* default employee if blank      */
IMPORT UNFORMATTED lvRunComplete.        /* value to indicate run complete */
IMPORT UNFORMATTED lvResourceList.       /* comma delimited list, or blank */
INPUT CLOSE.

MESSAGE 'Post Vorne Transactions to Touch Screen?'
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lvPostVorne.

INPUT FROM OS-DIR(lvVorneDir) NO-ECHO.
REPEAT:
  SET lvVorneFile ^ lvAttrList.
  IF lvAttrList NE 'f' OR INDEX(lvVorneFile,'.psv') EQ 0 THEN NEXT.
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
  /*
  IF lvPostVorne THEN
  OS-RENAME VALUE(lvFile) VALUE(lvTemp).
  ELSE */ lvTemp = lvFile.
  /* create temp-table vorne records */
  RUN createTtblVorne (lvTemp).
  /* post to create machtran, or simply to reflect onto SB */
  IF lvPostVorne THEN DO:
    /* if posting, move vorne file to processed */
    /* OS-RENAME VALUE(lvTemp) VALUE(lvProcessed). */
    RUN setSBJobs.
    /* calculate qty values for each transaction */
    RUN calcQtyPerTrans.
    /* run Detail or Summary */
    RUN VALUE('vorne' + lvVorneType).
  END. /* if lvportvorne */
  /* not posting, simply reflect trans on SB jobs */
  ELSE RUN setSBJobs.
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
    IF CAN-FIND(FIRST ttblEmpLogin
                WHERE ttblEmpLogin.company EQ ttblEmployee.company
                  AND ttblEmpLogin.employee EQ ttblEmployee.employee
                  AND ttblEmpLogin.start_date EQ ttblMachTran.start_date
                  AND ttblEmpLogin.start_time EQ ttblMachTran.start_time
                  AND ttblEmpLogin.machine EQ ttblVorne.vorneResource) THEN
    ASSIGN
      ttblMachTran.start_time = ttblMachTran.start_time + 1
      ttblMachTran.total_time = ttblMachTran.total_time + 1
      .
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

PROCEDURE createTtblVorne:
  DEFINE INPUT PARAMETER ipVorneFile AS CHARACTER NO-UNDO.

  EMPTY TEMP-TABLE ttblVorne.
  INPUT STREAM sVorne FROM VALUE(ipVorneFile).
  REPEAT:
    IMPORT STREAM sVorne UNFORMATTED lvVorneData.
    IF ENTRY(2,lvVorneData,'|') EQ 'null' THEN NEXT.
  /*IF ENTRY(12,lvVorneData,'|') EQ '' THEN NEXT.*/
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
      lvVorneEmployee = IF ENTRY(12,lvVorneData,'|') EQ '' THEN lvVorneBlankEmployee
                        ELSE ENTRY(12,lvVorneData,'|')
      .
    /*
    IF NOT CAN-FIND(FIRST ttblJob
                    WHERE ttblJob.resource EQ lvVorneResource
                      AND ttblJob.job EQ lvVorneJob) AND
       NOT CAN-FIND(FIRST pendingJob
                    WHERE pendingJob.resource EQ lvVorneResource
                      AND pendingJob.job EQ lvVorneJob) THEN NEXT.
    */
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
      ttblVorne.vorneEmployee = lvVorneEmployee
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
  DEFINE VARIABLE lvState AS CHARACTER NO-UNDO.

  &IF DEFINED(sort1) NE 0 &THEN
  OUTPUT TO \vorne\sort1.txt.
  &ENDIF
  /* consolidate vorne transactions */
  FOR EACH ttblVorne
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneJob
            BY ttblVorne.vorneItem
            BY ttblVorne.vorneEmployee
            BY ttblVorne.vorneShift
            BY ttblVorne.vorneStartDate
            BY ttblVorne.vorneSeq
            BY ttblVorne.vorneState
      :
    IF FIRST-OF(ttblVorne.vorneShift) OR
       FIRST-OF(ttblVorne.vorneStartDate) OR
       lvState NE ttblVorne.vorneState THEN DO:
      FIND buffVorne WHERE ROWID(buffVorne) EQ ROWID(ttblVorne).
      lvState = ttblVorne.vorneState.
    END. /* first-of */

    IF AVAILABLE buffVorne AND (ROWID(buffVorne) NE ROWID(ttblVorne)) THEN DO:
      ASSIGN
        buffVorne.vorneTranRunQty = buffVorne.vorneTranRunQty + ttblVorne.vorneTranRunQty
        buffVorne.vorneTranRejectQty = buffVorne.vorneTranRejectQty + ttblVorne.vorneTranRejectQty
        buffVorne.vorneDuration = buffVorne.vorneDuration + ttblVorne.vorneDuration
        buffVorne.vorneEndDate = ttblVorne.vorneEndDate
        buffVorne.vorneEndTime = ttblVorne.vorneEndTime
        ttblVorne.deleteFlag = YES
        .
    END. /* avail buffvorne */
    &IF DEFINED(sort1) NE 0 &THEN
    DISP
      ttblVorne.vorneJob LABEL 'Job'
      ttblVorne.vorneItem LABEL 'Item'
      ttblVorne.vorneStartDate LABEL 'Start'
      STRING(ttblVorne.vorneStartTime,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'Time'
      ttblVorne.vorneEndDate LABEL 'End'
      STRING(ttblVorne.vorneEndTime,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'Time'
      ttblVorne.vorneShift LABEL 'Shift'
      ttblVorne.vorneEmployee FORMAT 'x(20)' LABEL 'Employee'
      ttblVorne.vorneSeq LABEL 'Seq'
      ttblVorne.vorneState LABEL 'State'
      ttblVorne.vorneDuration LABEL 'Duration'
      ttblVorne.vorneTranRunQty LABEL 'Run'
      ttblVorne.vorneTranRejectQty LABEL 'Reject'
      ttblVorne.vorneReason EQ lvRunComplete LABEL 'Completed'
      ttblVorne.deleteFlag LABEL 'Deleted'
        WITH WIDTH 200 STREAM-IO.
    &ENDIF
  END. /* each ttblVorne */
  &IF DEFINED(sort1) NE 0 &THEN
  OUTPUT CLOSE.
  OS-COMMAND NO-WAIT notepad.exe \vorne\sort1.txt.
  &ENDIF
  
  /* move non-RUN qty values to RUN transaction */
  FOR EACH ttblVorne
      WHERE ttblVorne.vorneState NE 'RUN'
       AND  ttblVorne.deleteFlag EQ NO
       AND (ttblVorne.vorneTranRunQty NE 0
        OR  ttblVorne.vorneTranRejectQty NE 0)
      :
    FIND FIRST buffVorne
         WHERE buffVorne.vorneResource EQ ttblVorne.vorneResource
           AND buffVorne.vorneJob EQ ttblVorne.vorneJob
           AND buffVorne.vorneItem EQ ttblVorne.vorneItem
           AND buffVorne.vorneShift EQ ttblVorne.vorneShift
           AND buffVorne.vorneEmployee EQ ttblVorne.vorneEmployee
           AND buffVorne.vorneState EQ 'RUN'
           AND ttblVorne.deleteFlag EQ NO
         NO-ERROR.
    IF AVAILABLE buffVorne THEN
    ASSIGN
      buffVorne.vorneTranRunQty = buffVorne.vorneTranRunQty + ttblVorne.vorneTranRunQty
      buffVorne.vorneTranRejectQty = buffVorne.vorneTranRejectQty + ttblVorne.vorneTranRejectQty
      ttblVorne.vorneTranRunQty = 0
      ttblVorne.vorneTranRejectQty = 0
      .
    /*
    ELSE DO:
      ASSIGN
        ttblVorne.vorneEndTime = ttblVorne.vorneEndTime - 60
        ttblVorne.vorneDuration = ttblVorne.vorneDuration - 60
        .
      CREATE buffVorne.
      BUFFER-COPY ttblVorne EXCEPT vorneState vorneStartTime vorneDuration
               TO buffVorne
        ASSIGN
          buffVorne.vorneState = 'RUN'
          buffVorne.vorneStartTime = ttblVorne.vorneEndTime
          buffVorne.vorneEndTime = buffVorne.vorneStartTime + 60
          buffVorne.vorneDuration = 60
          .
      ASSIGN
        ttblVorne.vorneTranRunQty = 0
        ttblVorne.vorneTranRejectQty = 0
        .
    END. /* else */
    */
  END. /* each ttblVorne */

  /* flag records as run completed if necessary */
  FOR EACH ttblVorne
      WHERE ttblVorne.vorneState EQ 'RUN'
        AND ttblVorne.vorneReason EQ lvRunComplete
        AND ttblVorne.deleteFlag EQ YES
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneJob
            BY ttblVorne.vorneItem
            BY ttblVorne.vorneEmployee
            BY ttblVorne.vorneShift
            BY ttblVorne.vorneStartDate
            BY ttblVorne.vorneSeq
            BY ttblVorne.vorneState
      :
    FIND LAST buffVorne
         WHERE buffVorne.vorneResource EQ ttblVorne.vorneResource
           AND buffVorne.vorneJob EQ ttblVorne.vorneJob
           AND buffVorne.vorneItem EQ ttblVorne.vorneItem
           AND buffVorne.vorneEmployee EQ ttblVorne.vorneEmployee
           AND buffVorne.vorneShift EQ ttblVorne.vorneShift
           AND buffVorne.vorneState EQ 'RUN'
           AND buffVorne.vorneReason NE lvRunComplete
           AND buffVorne.deleteFlag EQ NO
         NO-ERROR.
    IF AVAILABLE buffVorne THEN
    buffVorne.vorneReason = lvRunComplete.
  END. /* each ttblvorne */
  
  /* remove deleted flag records */
  FOR EACH ttblVorne:
    IF ttblVorne.deleteFlag EQ YES OR
      (ttblVorne.vorneDuration LE 60 AND
       ttblVorne.vorneTranRunQty EQ 0 AND
       ttblVorne.vorneTranRejectQty EQ 0 AND
       ttblVorne.vorneReason NE lvRunComplete) THEN
    DELETE ttblVorne.
  END. /* each ttblvorne */

  &IF DEFINED(sort2) NE 0 &THEN
  OUTPUT TO \vorne\sort2.txt.
  &ENDIF
  /* consolidate vorne transactions again */
  RELEASE buffVorne.
  lvState = ''.
  FOR EACH ttblVorne
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneJob
            BY ttblVorne.vorneItem
            BY ttblVorne.vorneEmployee
            BY ttblVorne.vorneShift
            BY ttblVorne.vorneStartDate
            BY ttblVorne.vorneSeq
            BY ttblVorne.vorneState
      :
    IF FIRST-OF(ttblVorne.vorneShift) OR
       FIRST-OF(ttblVorne.vorneStartDate) OR
       lvState NE ttblVorne.vorneState THEN DO:
      FIND buffVorne WHERE ROWID(buffVorne) EQ ROWID(ttblVorne).
      lvState = ttblVorne.vorneState.
    END. /* first-of */

    IF AVAILABLE buffVorne AND (ROWID(buffVorne) NE ROWID(ttblVorne)) THEN DO:
      ASSIGN
        buffVorne.vorneTranRunQty = buffVorne.vorneTranRunQty + ttblVorne.vorneTranRunQty
        buffVorne.vorneTranRejectQty = buffVorne.vorneTranRejectQty + ttblVorne.vorneTranRejectQty
        buffVorne.vorneDuration = buffVorne.vorneDuration + ttblVorne.vorneDuration
        buffVorne.vorneEndDate = ttblVorne.vorneEndDate
        buffVorne.vorneEndTime = ttblVorne.vorneEndTime
        buffVorne.vorneReason = ttblVorne.vorneReason
        ttblVorne.deleteFlag = YES
        .
    END. /* avail buffvorne */
    &IF DEFINED(sort2) NE 0 &THEN
    DISP
      ttblVorne.vorneJob LABEL 'Job'
      ttblVorne.vorneItem LABEL 'Item'
      ttblVorne.vorneStartDate LABEL 'Start'
      STRING(ttblVorne.vorneStartTime,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'Time'
      ttblVorne.vorneEndDate LABEL 'End'
      STRING(ttblVorne.vorneEndTime,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'Time'
      ttblVorne.vorneShift LABEL 'Shift'
      ttblVorne.vorneEmployee FORMAT 'x(20)' LABEL 'Employee'
      ttblVorne.vorneSeq LABEL 'Seq'
      ttblVorne.vorneState LABEL 'State'
      ttblVorne.vorneDuration LABEL 'Duration'
      ttblVorne.vorneTranRunQty LABEL 'Run'
      ttblVorne.vorneTranRejectQty LABEL 'Reject'
      ttblVorne.vorneReason EQ lvRunComplete LABEL 'Completed'
      ttblVorne.deleteFlag LABEL 'Deleted'
        WITH WIDTH 200 STREAM-IO.
    &ENDIF
  END. /* each ttblVorne */
  &IF DEFINED(sort2) NE 0 &THEN
  OUTPUT CLOSE.
  OS-COMMAND NO-WAIT notepad.exe \vorne\sort2.txt.
  &ENDIF
  
  /*
  &IF DEFINED(sort3) NE 0 &THEN
  OUTPUT TO \vorne\sort3.txt.
  &ENDIF
  /* total RUN qty values to end of job/shift */
  FOR EACH ttblVorne
      WHERE ttblVorne.vorneState EQ 'RUN'
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneJob
            BY ttblVorne.vorneItem
            BY ttblVorne.vorneShift
            BY ttblVorne.vorneSeq
            BY ttblVorne.vorneState
      :
    IF FIRST-OF(ttblVorne.vorneShift)THEN
    ASSIGN
      lvVorneTranRunQty = 0
      lvVorneTranRejectQty = 0
      .
    ASSIGN
      lvVorneTranRunQty = lvVorneTranRunQty + ttblVorne.vorneTranRunQty
      lvVorneTranRejectQty = lvVorneTranRejectQty + ttblVorne.vorneTranRejectQty
      ttblVorne.vorneTranRunQty = 0
      ttblVorne.vorneTranRejectQty = 0
      .
    IF LAST-OF(ttblVorne.vorneShift)THEN
    ASSIGN
      ttblVorne.vorneTranRunQty = lvVorneTranRunQty
      ttblVorne.vorneTranRejectQty = lvVorneTranRejectQty
      .
    &IF DEFINED(sort3) NE 0 &THEN
    DISP
      ttblVorne.vorneJob LABEL 'Job'
      ttblVorne.vorneItem LABEL 'Item'
      ttblVorne.vorneStartDate LABEL 'Start'
      STRING(ttblVorne.vorneStartTime,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'Time'
      ttblVorne.vorneEndDate LABEL 'End'
      STRING(ttblVorne.vorneEndTime,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'Time'
      ttblVorne.vorneEmployee FORMAT 'x(20)' LABEL 'Employee'
      ttblVorne.vorneShift LABEL 'Shift'
      ttblVorne.vorneSeq LABEL 'Seq'
      ttblVorne.vorneState LABEL 'State'
      ttblVorne.vorneDuration LABEL 'Duration'
      ttblVorne.vorneTranRunQty LABEL 'Run'
      ttblVorne.vorneTranRejectQty LABEL 'Reject'
      ttblVorne.vorneReason EQ lvRunComplete LABEL 'Completed'
      ttblVorne.deleteFlag LABEL 'Deleted'
        WITH WIDTH 200 STREAM-IO.
    &ENDIF
  END. /* each ttblVorne */
  &IF DEFINED(sort3) NE 0 &THEN
  OUTPUT CLOSE.
  OS-COMMAND NO-WAIT notepad.exe \vorne\sort3.txt.
  &ENDIF
  */
  
  /* calc ending values and create machtran records */
  FOR EACH ttblVorne
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneJob
            BY ttblVorne.vorneItem
            BY ttblVorne.vorneEmployee
            BY ttblVorne.vorneShift
            BY ttblVorne.vorneStartDate
            BY ttblVorne.vorneSeq
            BY ttblVorne.vorneState
      :
    IF ttblVorne.deleteFlag THEN NEXT.
    
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
    
    /*
    RUN calcEnd (ttblVorne.vorneStartDate,ttblVorne.vorneStartTime,0,ttblVorne.vorneDuration / 3600,
                 OUTPUT ttblVorne.vorneEndDate,OUTPUT ttblVorne.vorneEndTime).
    ASSIGN
      lvVorneStartDate = ttblVorne.vorneStartDate
      lvVorneStartTime = ttblVorne.vorneStartTime
      lvVorneEndDate = ttblVorne.vorneEndDate
      lvVorneEndTime = ttblVorne.vorneEndTime
      .
    */
    ttblVorne.vorneDuration = IF ttblVorne.vorneStartDate EQ ttblVorne.vorneEndDate THEN ttblVorne.vorneEndTime - ttblVorne.vorneStartTime
                              ELSE (86400 - ttblVorne.vorneStartTime)
                                 + (ttblVorne.vorneEndDate - ttblVorne.vorneStartDate - 1)
                                 *  86400 + ttblVorne.vorneEndTime.
    /* loop in case of date change, break transaction into two */
    DO lvDateLoop = ttblVorne.vorneStartDate TO ttblVorne.vorneEndDate:
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
        ttblMachTran.completed = lvDateLoop EQ ttblVorne.vorneEndDate AND
                                 ttblVorne.vorneReason EQ lvRunComplete
        ttblMachTran.start_date = lvDateLoop
        ttblMachTran.start_time = ttblVorne.vorneStartTime WHEN lvDateLoop EQ ttblVorne.vorneStartDate
        ttblMachTran.end_date = lvDateLoop
        ttblMachTran.end_time = IF lvDateLoop EQ ttblVorne.vorneEndDate THEN ttblVorne.vorneEndTime
                                ELSE 86340
        ttblMachTran.run_qty = ttblVorne.vorneTranRunQty WHEN lvDateLoop EQ ttblVorne.vorneEndDate
        ttblMachTran.waste_qty = ttblVorne.vorneTranRejectQty WHEN lvDateLoop EQ ttblVorne.vorneEndDate
        ttblMachTran.shift = ttblVorne.vorneShift
        .
      IF ttblMachTran.start_date = ttblMachTran.end_date THEN
      ttblMachTran.total_time = ttblMachTran.end_time - ttblMachTran.start_time.
      ELSE
      ttblMachTran.total_time = (86400 - ttblMachTran.start_time) +
                                (ttblMachTran.end_date - ttblMachTran.start_date - 1) * 86400 +
                                 ttblMachTran.end_time.
      IF ttblMachTran.total_time LT 0 OR ttblMachTran.total_time EQ ? THEN
      ttblMachTran.total_time = 0.
      
      IF lvEmpLogin EQ 'Vorne' THEN
      RUN createEmpLogin (BUFFER ttblVorne,BUFFER ttblMachTran,OUTPUT empLoginRowID).
      /*
      RUN setRecKey (BUFFER machtran).
      RUN createMachEmp (BUFFER machtran, INPUT empLoginRowID).
      IF machtran.completed THEN
      RUN completeMR.
      */
    END. /* do lvdataloop */
  END. /* each ttblVorne */

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

PROCEDURE setSBJobs:
  &IF DEFINED(setSBJobs) NE 0 &THEN
  OUTPUT TO \vorne\setsbjobs.txt.
  &ENDIF
  FOR EACH ttblVorne
      BREAK BY ttblVorne.vorneResource
            BY ttblVorne.vorneJob
            BY ttblVorne.vorneItem
            BY ttblVorne.vorneSeq
      :
    IF FIRST-OF(ttblVorne.vorneJob) THEN DO:
      &IF DEFINED(setSBJobs) NE 0 &THEN
      DISP
        ttblVorne.vorneSeq
        ttblVorne.vorneJob FORMAT 'x(20)' LABEL 'Job'
        ttblVorne.vorneItem LABEL 'Item'
        ttblVorne.vorneStartDate LABEL 'Start'
        STRING(ttblVorne.vorneStartTime,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'Time'
        ttblVorne.vorneEndDate LABEL 'End'
        STRING(ttblVorne.vorneEndTime,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'Time'
        ttblVorne.vorneShift LABEL 'Shift'
        ttblVorne.vorneEmployee FORMAT 'x(20)' LABEL 'Employee'
        ttblVorne.vorneSeq LABEL 'Seq'
        ttblVorne.vorneState LABEL 'State'
        ttblVorne.vorneDuration LABEL 'Duration'
        ttblVorne.vorneTranRunQty LABEL 'Run'
        ttblVorne.vorneTranRejectQty LABEL 'Reject'
        ttblVorne.vorneReason EQ lvRunComplete LABEL 'Completed'
        ttblVorne.deleteFlag LABEL 'Deleted'
          WITH WIDTH 200 STREAM-IO.
      &ENDIF
      FIND FIRST ttblJob
           WHERE ttblJob.resource EQ ttblVorne.vorneResource
             AND ttblJob.job EQ ttblVorne.vorneJob NO-ERROR.
      IF AVAILABLE ttblJob THEN DO:
        IF ttblJob.jobLocked THEN NEXT.
      END. /* avail ttbljob */
      ELSE DO:
        FIND FIRST pendingJob
             WHERE pendingJob.resource EQ ttblVorne.vorneResource
               AND pendingJob.job EQ ttblVorne.vorneJob NO-ERROR.
        IF NOT AVAILABLE pendingJob THEN NEXT.
        CREATE ttblJob.
        BUFFER-COPY pendingJob TO ttblJob.
        DELETE pendingJob.
      END. /* not avail ttbljob */
      ASSIGN
        ttblJob.startDate = ttblVorne.vorneStartDate
        ttblJob.startTime = ttblVorne.vorneStartTime
        .
      RUN calcEnd (ttblJob.startDate,ttblJob.startTime,0,ttblJob.timeSpan,
                   OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).
    END. /* first-of job */
  END. /* each ttblvorne */
  &IF DEFINED(setSBJobs) NE 0 &THEN
  OUTPUT CLOSE.
  OS-COMMAND NO-WAIT notepad.exe \vorne\setsbjobs.txt.
  &ENDIF
END PROCEDURE.

/*******************************************************************************/
DEFINE VARIABLE lvJobNumber AS CHARACTER NO-UNDO.

&IF DEFINED(results) NE 0 &THEN
OUTPUT TO '/vorne/results.txt'.
FOR EACH ttblMachTran NO-LOCK
      BY ttblMachTran.machine
      BY ttblMachTran.start_date
      BY ttblMachTran.start_time
    :
  IF lvJobNumber EQ '' THEN
    lvJobNumber = ttblMachTran.job_number.
  IF lvJobNumber NE ttblMachTran.job_number THEN DO:
    DOWN 1.
    lvJobNumber = ttblMachTran.job_number.
  END.
  DISP
    ttblMachTran.machine
    ttblMachTran.shift
    ttblMachTran.job_number
    ttblMachTran.job_sub
    ttblMachTran.form_number
    ttblMachTran.blank_number
    ttblMachTran.charge_code
    ttblMachTran.start_date
    STRING(ttblMachTran.start_time,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'Start Time'
    ttblMachTran.end_date
    STRING(ttblMachTran.end_time,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'End Time'
    ttblMachTran.run_qty
    ttblMachTran.waste_qty
    ttblMachTran.total_time
    ttblMachTran.completed
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
    ttblEmpLogin.machine
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
&ENDIF
