/* rptLayout.i */

{{&print}/includes/printDefs.i}
{{&includes}/specialTime.i}

DEFINE VARIABLE colors AS CHARACTER NO-UNDO.
DEFINE VARIABLE dashLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelCol# AS INTEGER NO-UNDO.
DEFINE VARIABLE excelCnt AS INTEGER NO-UNDO.
DEFINE VARIABLE excelHeader AS LOGICAL NO-UNDO INITIAL YES.
DEFINE VARIABLE fieldLine AS CHARACTER NO-UNDO EXTENT 6.
DEFINE VARIABLE hiRptLine AS INTEGER NO-UNDO.
DEFINE VARIABLE labelLine AS CHARACTER NO-UNDO EXTENT 6.
DEFINE VARIABLE noteFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE noteCnt AS INTEGER NO-UNDO.
DEFINE VARIABLE skipLine AS LOGICAL NO-UNDO.

DEFINE WORKFILE wrkExcel NO-UNDO
  FIELD excelColumn AS INTEGER
  FIELD excelLabel AS CHARACTER
  FIELD excelField AS CHARACTER.

DEFINE WORKFILE wrkRpt NO-UNDO
  FIELD wrkColumn AS INTEGER
  FIELD wrkLine AS INTEGER
  FIELD excelColumn AS INTEGER.

DEFINE TEMP-TABLE ttblSortBy LIKE ttblJob.

DEFINE BUFFER buffUseTable FOR {&useTable}.

{{&includes}/jobStatusFunc.i}

FUNCTION resourceSequence RETURNS CHARACTER (ipValue AS INTEGER):
  FIND FIRST buffUseTable NO-LOCK
       WHERE buffUseTable.job EQ {&useTable}.job
         AND buffUseTable.resourceSequence EQ {&useTable}.resourceSequence + ipValue
       NO-ERROR.
  RETURN IF AVAILABLE buffUseTable THEN buffUseTable.resource ELSE ''.
END FUNCTION.

FUNCTION routing RETURNS CHARACTER ():
  DEFINE VARIABLE cRouting AS CHARACTER NO-UNDO.

  FOR EACH buffUseTable NO-LOCK
      WHERE buffUseTable.job EQ {&useTable}.job
      BY buffUseTable.resourceSequence
      :
      cRouting = cRouting + buffUseTable.resource + ','.
  END.
  cRouting = TRIM(cRouting,',').
  RETURN cRouting.
END FUNCTION.

FUNCTION sortBy RETURNS CHARACTER:
  CASE sortByValue:
    WHEN 'Due Date' THEN
    RETURN STRING(YEAR({&useTable}.dueDate),'9999') +
           STRING(MONTH({&useTable}.dueDate),'99') +
           STRING(DAY({&useTable}.dueDate),'99').
    WHEN 'Job' THEN
    RETURN {&useTable}.jobSort.
    WHEN 'Job Sequence' THEN
    RETURN STRING({&useTable}.jobSequence,'9999').
    WHEN 'Prod Date' THEN
    RETURN STRING(YEAR({&useTable}.prodDate),'9999') +
           STRING(MONTH({&useTable}.prodDate),'99') +
           STRING(DAY({&useTable}.prodDate),'99').
    WHEN 'Resource' THEN
    RETURN {&useTable}.resource.
    OTHERWISE
    RETURN ''.
  END CASE.
END FUNCTION.

PROCEDURE buildLines:
  DEFINE INPUT PARAMETER ipHeaderLines AS LOGICAL NO-UNDO.

  DEFINE VARIABLE lvFieldFormat AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cText AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN
    labelLine = ''
    dashLine = ''
    fieldLine = ''.
  FOR EACH rptLayout NO-LOCK WHERE rptLayout.rptName EQ '{&printProgramName}'
                               AND rptLayout.rptFormat EQ ipFormat:
    IF (NOT ipExcel AND (rptLayout.rptLine EQ 0 OR rptLayout.rptColumn EQ 0)) OR
       (ipExcel AND rptLayout.excelColumn EQ 0) THEN NEXT.
    FIND FIRST rptFields NO-LOCK
         WHERE rptFields.fieldName EQ rptLayout.fieldName
           AND rptFields.fieldLabel EQ rptLayout.fieldLabel NO-ERROR.
    IF NOT AVAILABLE rptFields THEN NEXT.
    lvFieldFormat = rptFields.fieldFormat.
    IF ipExcel THEN
    lvFieldFormat = REPLACE(lvFieldFormat,'99.99.9999','99/99/9999').
    CASE rptLayout.fieldName:
      WHEN 'dueDate' THEN
      RUN loadField (STRING({&useTable}.dueDate,lvFieldFormat)).
      WHEN 'endDate' THEN
      RUN loadField (STRING({&useTable}.endDate,lvFieldFormat)).
      WHEN 'jobSequence' THEN
      RUN loadField (STRING({&useTable}.jobSequence,lvFieldFormat)).
      WHEN 'job' THEN
      RUN loadField ({&useTable}.job).
      WHEN 'jobCompleted' THEN
      RUN loadField (STRING({&useTable}.jobCompleted,lvFieldFormat)).
      WHEN 'jobDescription' THEN DO:
          cText = REPLACE({&useTable}.jobDescription,'|',' ').
          RUN jobText (cText, OUTPUT cText).
          RUN loadField (cText).
      END.
      WHEN 'jobLocked' THEN
      RUN loadField (STRING({&useTable}.jobLocked,lvFieldFormat)).
      WHEN 'jobSort' THEN
      RUN loadField ({&useTable}.jobSort).
      WHEN 'jobType' THEN
      RUN loadField ({&useTable}.jobType).
      WHEN 'nextRes' THEN
      RUN loadField (resourceSequence(1)).
      WHEN 'prevRes' THEN
      RUN loadField (resourceSequence(-1)).
      WHEN 'prodDate' THEN
      RUN loadField (STRING({&useTable}.prodDate,lvFieldFormat)).
      WHEN 'resource' THEN
      RUN loadField ({&useTable}.resource).
      WHEN 'resourceDescription' THEN
      RUN loadField ({&useTable}.resourceDescription).
      WHEN 'resourceSequence' THEN
      RUN loadField (STRING({&useTable}.resourceSequence,lvFieldFormat)).
      WHEN 'routing' THEN
      RUN loadField (routing()).
      WHEN 'skipLine' THEN DO:
        RUN loadField ('skipLine').
        skipLine = YES.
      END.
      WHEN 'startDate' THEN
      RUN loadField (STRING({&useTable}.startDate,lvFieldFormat)).
      WHEN 'status' THEN
      RUN loadField (jobStatus()).
      WHEN 'calcTimeField' THEN
      CASE rptLayout.fieldLabel:
        WHEN 'Downtime' THEN
        RUN loadField (STRING(specialTime({&useTable}.downtimeSpan),lvFieldFormat)).
        WHEN 'Due Time' THEN
        RUN loadField (STRING({&useTable}.dueTime,lvFieldFormat)).
        WHEN 'End Time' THEN
        RUN loadField (STRING({&useTable}.endTime,lvFieldFormat)).
        WHEN 'Job Time' THEN
        RUN loadField (STRING(specialTime({&useTable}.timeSpan),lvFieldFormat)).
        WHEN 'Start Time' THEN
        RUN loadField (STRING({&useTable}.startTime,lvFieldFormat)).
        WHEN 'Total Time' THEN
        RUN loadField (STRING(specialTime({&useTable}.timeSpan + {&useTable}.downtimeSpan),lvFieldFormat)).
      END CASE.
      {{&includes}/jobStatus.i 1}
      {{&includes}/jobStatus.i 2}
      {{&includes}/jobStatus.i 3}
      {{&includes}/jobStatus.i 4}
      {{&includes}/jobStatus.i 5}
      {{&includes}/jobStatus.i 6}
      {{&includes}/jobStatus.i 7}
      {{&includes}/jobStatus.i 8}
      {{&includes}/jobStatus.i 9}
      {{&includes}/jobStatus.i 10}
      {{&includes}/jobStatus.i 11}
      {{&includes}/jobStatus.i 12}
      {{&includes}/jobStatus.i 13}
      {{&includes}/jobStatus.i 14}
      {{&includes}/udfField.i 01}
      {{&includes}/udfField.i 02}
      {{&includes}/udfField.i 03}
      {{&includes}/udfField.i 04}
      {{&includes}/udfField.i 05}
      {{&includes}/udfField.i 06}
      {{&includes}/udfField.i 07}
      {{&includes}/udfField.i 08}
      {{&includes}/udfField.i 09}
      {{&includes}/udfField.i 10}
      {{&includes}/udfField.i 11}
      {{&includes}/udfField.i 12}
      {{&includes}/udfField.i 13}
      {{&includes}/udfField.i 14}
      {{&includes}/udfField.i 15}
      {{&includes}/udfField.i 16}
      {{&includes}/udfField.i 17}
      {{&includes}/udfField.i 18}
      {{&includes}/udfField.i 19}
      {{&includes}/udfField.i 20}
      {{&includes}/userField.i 01}
      {{&includes}/userField.i 02}
      {{&includes}/userField.i 03}
      {{&includes}/userField.i 04}
      {{&includes}/userField.i 05}
      {{&includes}/userField.i 06}
      {{&includes}/userField.i 07}
      {{&includes}/userField.i 08}
      {{&includes}/userField.i 09}
      {{&includes}/userField.i 10}
      {{&includes}/userField.i 11}
      {{&includes}/userField.i 12}
      {{&includes}/userField.i 13}
      {{&includes}/userField.i 14}
      {{&includes}/userField.i 15}
      {{&includes}/userField.i 16}
      {{&includes}/userField.i 17}
      {{&includes}/userField.i 18}
      {{&includes}/userField.i 19}
      {{&includes}/userField.i 20}
      {{&includes}/userField.i 21}
      {{&includes}/userField.i 22}
      {{&includes}/userField.i 23}
      {{&includes}/userField.i 24}
      {{&includes}/userField.i 25}
      {{&includes}/userField.i 26}
      {{&includes}/userField.i 27}
      {{&includes}/userField.i 28}
      {{&includes}/userField.i 29}
      {{&includes}/userField.i 30}
      {{&includes}/userField.i 31}
      {{&includes}/userField.i 32}
      {{&includes}/userField.i 33}
      {{&includes}/userField.i 34}
      {{&includes}/userField.i 35}
      {{&includes}/userField.i 36}
      {{&includes}/userField.i 37}
      {{&includes}/userField.i 38}
      {{&includes}/userField.i 39}
      {{&includes}/userField.i 40}
      {{&includes}/userField.i 41}
      {{&includes}/userField.i 42}
      {{&includes}/userField.i 43}
      {{&includes}/userField.i 44}
      {{&includes}/userField.i 45}
      {{&includes}/userField.i 46}
      {{&includes}/userField.i 47}
      {{&includes}/userField.i 48}
      {{&includes}/userField.i 49}
      {{&includes}/userField.i 50}
      {{&includes}/userField.i 51}
      {{&includes}/userField.i 52}
      {{&includes}/userField.i 53}
      {{&includes}/userField.i 54}
      {{&includes}/userField.i 55}
      {{&includes}/userField.i 56}
      {{&includes}/userField.i 57}
      {{&includes}/userField.i 58}
      {{&includes}/userField.i 59}
      {{&includes}/userField.i 60}
      {{&includes}/userField.i 61}
      {{&includes}/userField.i 62}
      {{&includes}/userField.i 63}
      {{&includes}/userField.i 64}
      {{&includes}/userField.i 65}
      {{&includes}/userField.i 66}
      {{&includes}/userField.i 67}
      {{&includes}/userField.i 68}
      {{&includes}/userField.i 69}
      {{&includes}/userField.i 70}
      {{&includes}/userField.i 71}
      {{&includes}/userField.i 72}
      {{&includes}/userField.i 73}
      {{&includes}/userField.i 74}
      {{&includes}/userField.i 75}
      {{&includes}/userField.i 76}
      {{&includes}/userField.i 77}
      {{&includes}/userField.i 78}
      {{&includes}/userField.i 79}
      {{&includes}/userField.i 80}
      {{&includes}/userField.i 81}
      {{&includes}/userField.i 82}
      {{&includes}/userField.i 83}
      {{&includes}/userField.i 84}
      {{&includes}/userField.i 85}
      {{&includes}/userField.i 86}
      {{&includes}/userField.i 87}
      {{&includes}/userField.i 88}
      {{&includes}/userField.i 89}
      {{&includes}/userField.i 90}
      {{&includes}/userField.i 91}
      {{&includes}/userField.i 92}
      {{&includes}/userField.i 93}
      {{&includes}/userField.i 94}
      {{&includes}/userField.i 95}
      {{&includes}/userField.i 96}
      {{&includes}/userField.i 97}
      {{&includes}/userField.i 98}
      {{&includes}/userField.i 99}
      {{&includes}/userField.i 100}
      {{&includes}/userField.i 101}
      {{&includes}/userField.i 102}
      {{&includes}/userField.i 103}
      {{&includes}/userField.i 104}
      {{&includes}/userField.i 105}
      {{&includes}/userField.i 106}
      {{&includes}/userField.i 107}
      {{&includes}/userField.i 108}
      {{&includes}/userField.i 109}
      {{&includes}/userField.i 110}
      {{&includes}/userField.i 111}
      {{&includes}/userField.i 112}
      {{&includes}/userField.i 113}
      {{&includes}/userField.i 114}
      {{&includes}/userField.i 115}
      {{&includes}/userField.i 116}
      {{&includes}/userField.i 117}
      {{&includes}/userField.i 118}
      {{&includes}/userField.i 119}
      {{&includes}/userField.i 120}
      {{&includes}/userField.i 121}
      {{&includes}/userField.i 122}
      {{&includes}/userField.i 123}
    END CASE.
  END. /* each rptLayout */
  IF ipExcel THEN DO:
    IF excelHeader THEN DO:
      FOR EACH wrkExcel EXCLUSIVE-LOCK BY wrkExcel.excelColumn:
        PUT UNFORMATTED '"' wrkExcel.excelLabel '",'.
      END.
      /* manually add note headers */
      PUT UNFORMATTED '"Note Date","Note Time","Note",' SKIP.
      excelHeader = NO.
    END. /* if excelheader */
    excelCnt = 0. /* initialize counter for # of columns used */
    FOR EACH wrkExcel EXCLUSIVE-LOCK BY wrkExcel.excelColumn:
      PUT UNFORMATTED '"' wrkExcel.excelField '",'.
      excelCnt = excelCnt + 1.
      DELETE wrkExcel.
    END. /* each wrkexcel */
  END. /* if ipexcel */
  ELSE DO:
    IF ipHeaderLines THEN DO:
      PUT UNFORMATTED SKIP(1).
      DO i = 1 TO EXTENT(labelLine):
        IF labelLine[i] NE '' THEN
        PUT UNFORMATTED labelLine[i] AT 1.
      END. /* do i */
      PUT UNFORMATTED dashLine AT 1.
    END. /* if ipheaderlines */
    DO i = 1 TO EXTENT(fieldLine):
      PUT UNFORMATTED fieldLine[i] AT 1.
    END. /* do i */
  END. /* else */
END PROCEDURE.

PROCEDURE buildWrkRpt:
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  FOR EACH rptLayout NO-LOCK WHERE rptLayout.rptName EQ '{&printProgramName}'
                               AND rptLayout.rptFormat EQ ipFormat:
    IF (NOT ipExcel AND (rptLayout.rptLine EQ 0 OR rptLayout.rptColumn EQ 0)) OR
       (ipExcel AND rptLayout.excelColumn EQ 0) THEN NEXT.
    IF hiRptLine LT rptLayout.rptLine AND
       rptLayout.fieldName NE 'skipLine' THEN
    hiRptLine = rptLayout.rptLine.
    FIND FIRST wrkRpt
         WHERE wrkRpt.wrkColumn EQ rptLayout.rptColumn
           AND wrkRpt.excelColumn EQ rptLayout.excelColumn NO-ERROR.
    IF NOT AVAILABLE wrkRpt THEN DO:
      CREATE wrkRpt.
      ASSIGN
        wrkRpt.wrkColumn = rptLayout.rptColumn
        wrkRpt.excelColumn = rptLayout.excelColumn.
    END. /* if not avail */
    IF wrkRpt.wrkLine LT rptLayout.rptLine AND
       rptLayout.fieldName NE 'skipLine' THEN
    wrkRpt.wrkLine = rptLayout.rptLine.
  END. /* each rptlayout */
END PROCEDURE.

PROCEDURE getConfiguration:
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/config.dat')) NO-ECHO.
  IMPORT version.
  INPUT CLOSE.
  RUN VALUE('get' + version).
END PROCEDURE.

PROCEDURE jobText:
    {{&loads}/jobText.i}
END PROCEDURE.

PROCEDURE loadField:
  DEFINE INPUT PARAMETER ipValue AS CHARACTER NO-UNDO.

  DEFINE VARIABLE lvFieldLabel AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvLength AS INTEGER NO-UNDO.
  DEFINE VARIABLE idx AS INTEGER NO-UNDO.

  FIND FIRST wrkRpt
       WHERE wrkRpt.wrkColumn EQ rptLayout.rptColumn
         AND wrkRpt.excelColumn EQ rptLayout.excelColumn NO-ERROR.
  IF NOT AVAILABLE wrkRpt THEN RETURN.
  ASSIGN
    idx = IF rptLayout.fieldLabel BEGINS 'Status' THEN
          INTEGER(SUBSTR(rptLayout.fieldLabel,8)) ELSE 0
    lvFieldLabel = IF idx NE 0 AND customLabel[idx] NE '' THEN customLabel[idx]
                   ELSE rptLayout.fieldLabel.
  IF ipExcel THEN DO:
    CREATE wrkExcel.
    ASSIGN
      wrkExcel.excelColumn = wrkRpt.excelColumn + excelCol#
      wrkExcel.excelLabel = lvFieldLabel
      wrkExcel.excelField = ipValue.
  END. /* if ipexcel */
  ELSE
  IF ipValue NE 'skipLine' THEN
  ASSIGN
    lvLength = MAX(LENGTH(lvFieldLabel),LENGTH(rptFields.fieldFormat))
    idx = rptLayout.rptLine + (hiRptLine - wrkRpt.wrkLine)
    SUBSTR(dashLine,rptLayout.rptColumn,lvLength) = FILL('-',lvLength)
    SUBSTR(labelLine[idx],rptLayout.rptColumn,LENGTH(lvFieldLabel)) = lvFieldLabel
    SUBSTR(fieldLine[rptLayout.rptLine],rptLayout.rptColumn,LENGTH(ipValue)) = ipValue.
  ELSE
  fieldLine[rptLayout.rptLine] = ' '.
END PROCEDURE.
