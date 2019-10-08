/* MultiCell.p */

DEFINE VARIABLE mCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE transDateTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE transDate AS DATE NO-UNDO.
DEFINE VARIABLE transTime AS INTEGER NO-UNDO.
DEFINE VARIABLE jobNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobNo2 AS INTEGER NO-UNDO.
DEFINE VARIABLE frm AS INTEGER NO-UNDO.
DEFINE VARIABLE qty AS INTEGER NO-UNDO.
DEFINE VARIABLE jobState AS INTEGER NO-UNDO.
DEFINE VARIABLE shift AS INTEGER NO-UNDO.
DEFINE VARIABLE csvName AS CHARACTER NO-UNDO.

csvName = '31ASM_Results.csv'.
mCode = ENTRY(1,csvName,'_').
csvName = '/asi_gui10/pco1010/MultiCell/' + csvName.

INPUT FROM VALUE(csvName) NO-ECHO.
REPEAT:
  IMPORT DELIMITER ',' transDateTime qty jobNo jobState shift.
  ASSIGN
    transDate = DATE(ENTRY(1,transDateTime,' '))
    transTime = INT(SUBSTR(ENTRY(2,transDateTime,' '),1,2)) * 3600 +
                INT(SUBSTR(ENTRY(2,transDateTime,' '),4,2)).
  MESSAGE mCode transDateTime transDate transTime qty jobNo jobState shift
    VIEW-AS ALERT-BOX.
END. /* repeat */
INPUT CLOSE.

PROCEDURE pc-prdhCreate:
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMCode AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipShift AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipTransDate AS DATE NO-UNDO.
  
  IF NOT CAN-FIND(pc-prdh
            WHERE pc-prdh.company EQ ipCompany
              AND pc-prdh.m-code EQ ipMCode
              AND pc-prdh.shift EQ ipShift
              AND pc-prdh.trans-date EQ ipTransDate) THEN DO:
    FIND FIRST mach NO-LOCK WHERE mach.company EQ ipCompany
                              AND mach.m-code EQ ipMCode NO-ERROR.
    CREATE pc-prdh.
    ASSIGN
      pc-prdh.company = ipCompany
      pc-prdh.m-code = ipMCode
      pc-prdh.shift = ipShift
      pc-prdh.trans-date = ipTransDate
      pc-prdh.user-id = USERID('NOSWEAT')
      pc-prdh.dept = mach.dept[1].
  END.
END PROCEDURE.

PROCEDURE pc-prddCreate:
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMCode AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo2 AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipFrm AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipBlankNo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipPass AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipCode AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipShift AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipOPDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipStart AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipStopp AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipDept AS CHARACTER NO-UNDO.

  DEFINE VARIABLE runComplete AS LOGICAL NO-UNDO.
  DEFINE VARIABLE runHours AS DECIMAL NO-UNDO.
  
  FIND FIRST mach NO-LOCK WHERE mach.company EQ ipCompany
                            AND mach.m-code EQ ipMCode NO-ERROR.
  FIND FIRST job NO-LOCK WHERE job.company EQ ipCompany
                           AND job.job-no EQ ipJobNo
                           AND job.job-no2 EQ ipJobNo2 NO-ERROR.
  FIND FIRST job-hdr NO-LOCK WHERE job-hdr.company EQ ipCompany
                               AND job-hdr.job-no EQ ipJobNo
                               AND job-hdr.job-no2 EQ ipJobNo2
                               AND job-hdr.frm EQ ipFrm
                               AND job-hdr.blank-no EQ ipBlankNo NO-ERROR.
  IF NOT AVAIL job-hdr THEN
     FIND FIRST job-hdr NO-LOCK WHERE job-hdr.company EQ ipCompany
                                  AND job-hdr.job-no EQ ipJobNo
                                  AND job-hdr.job-no2 EQ ipJobNo2
                                  AND job-hdr.frm EQ ipFrm NO-ERROR.
  IF AVAILABLE job-hdr THEN DO:
    FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ ipCompany
                                AND itemfg.i-no EQ job-hdr.i-no NO-ERROR.
    FIND FIRST job-mch NO-LOCK WHERE job-mch.company EQ ipCompany
                                 AND job-mch.j-no EQ job-hdr.j-no
                                 AND job-mch.i-no EQ job-hdr.i-no
                                 AND job-mch.m-code EQ ipMCode NO-ERROR.
  END.
  FIND FIRST pc-prdd EXCLUSIVE-LOCK
       WHERE pc-prdd.company EQ ipCompany
         AND pc-prdd.m-code EQ ipMCode
         AND pc-prdd.job-no EQ ipJobNo
         AND pc-prdd.job-no2 EQ ipJobNo2
         AND pc-prdd.frm EQ ipFrm
         AND pc-prdd.blank-no EQ ipBlankNo
         AND pc-prdd.pass EQ ipPass
         AND pc-prdd.i-no EQ (IF AVAILABLE job-hdr THEN job-hdr.i-no ELSE '')
         AND pc-prdd.code EQ ipCode
         AND pc-prdd.op-date EQ ipOPDate
         AND pc-prdd.start EQ ipStart
         AND pc-prdd.stopp EQ ipStopp
         AND pc-prdd.shift EQ ipShift NO-ERROR.
  IF NOT AVAILABLE pc-prdd THEN DO:
    CREATE pc-prdd.
    ASSIGN
      pc-prdd.company = ipCompany
      pc-prdd.m-code = ipMCode
      pc-prdd.job-no = ipJobNo
      pc-prdd.job-no2 = ipJobNo2
      pc-prdd.frm = ipFrm
      pc-prdd.blank-no = ipBlankNo
      pc-prdd.pass = ipPass
      pc-prdd.i-no = IF AVAILABLE job-hdr THEN job-hdr.i-no ELSE ''
      pc-prdd.code = ipCode
      pc-prdd.op-date = ipOPDate
      pc-prdd.start = ipStart
      pc-prdd.stopp = ipStopp
      pc-prdd.startx = SUBSTRING(STRING(pc-prdd.start,"hh:mm"),1,2) +
                       SUBSTRING(STRING(pc-prdd.start,"hh:mm"),4,2)
      pc-prdd.stopx =  SUBSTRING(STRING(pc-prdd.stopp,"hh:mm"),1,2) +
                       SUBSTRING(STRING(pc-prdd.stopp,"hh:mm"),4,2)
      pc-prdd.shift = ipShift
      pc-prdd.complete = runComplete
      pc-prdd.dept = mach.dept[1]
      pc-prdd.hours = runHours / 3600
      pc-prdd.i-name = IF AVAILABLE itemfg THEN itemfg.i-name ELSE ''
      pc-prdd.j-no = IF AVAILABLE job-hdr THEN job-hdr.j-no ELSE 0
      pc-prdd.job = job.job
      pc-prdd.speed = IF AVAILABLE job-mch THEN job-mch.speed ELSE 0
      pc-prdd.crew = 1
      .
    FIND job-code NO-LOCK WHERE job-code.code EQ pc-prdd.code NO-ERROR.
    IF AVAILABLE job-code AND job-code.cat EQ "MR" THEN
      pc-prdd.rate[INTEGER(pc-prdd.crew)] = mach.mr-rate.
    ELSE IF AVAILABLE job-code AND job-code.cat EQ "RUN" THEN
      pc-prdd.rate[INTEGER(pc-prdd.crew)] = mach.run-rate.
  END.
END PROCEDURE.
