/* loadProEnd.i */

IF traceON THEN
OUTPUT CLOSE.

OUTPUT TO 'schedule/load.log' APPEND.
PUT UNFORMATTED '  End Load: ' STRING(TODAY,'99.99.9999') ' @ ' STRING(TIME,'hh:mm:ss') ' for ' ID ' by ' sbUser SKIP(1).
OUTPUT CLOSE.

OUTPUT STREAM sCapacity CLOSE.
OUTPUT STREAM sJobNotes CLOSE.
OUTPUT STREAM sPending CLOSE.
OUTPUT STREAM sResource CLOSE.
OUTPUT STREAM sScenario CLOSE.

RUN updateColumns. /* add newly added columns incase they are missing */

PROCEDURE loadRptLayout:
  DEFINE INPUT PARAMETER ipRptName AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opTextFields AS CHARACTER NO-UNDO.

  DEFINE VARIABLE inputFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvID AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE hiFormat AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFieldLabel AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iFieldLabel AS INTEGER NO-UNDO.

  EMPTY TEMP-TABLE rptLayout.
  DO i = 2 TO 1 BY -1:
    ASSIGN
      lvID = IF i EQ 1 THEN '' ELSE ID
      inputFile = findProgram('{&data}/',lvID,'/rptLayout.dat').
    INPUT FROM VALUE(inputFile) NO-ECHO.
    REPEAT:
      IMPORT ttblRptLayout.
      IF ttblRptLayout.rptName NE ipRptName THEN NEXT.
      IF ttblRptLayout.rptID EQ '' OR ID BEGINS ttblRptLayout.rptID THEN DO:
        FIND FIRST rptLayout EXCLUSIVE-LOCK
             WHERE rptLayout.rptName EQ ttblRptLayout.rptName
               AND rptLayout.rptFormat EQ ttblRptLayout.rptFormat
               AND rptLayout.fieldLabel EQ ttblRptLayout.fieldLabel
               AND rptLayout.fieldName EQ ttblRptLayout.fieldName NO-ERROR.
        IF NOT AVAILABLE rptLayout THEN DO:
          CREATE rptLayout.
          BUFFER-COPY ttblRptLayout TO rptLayout.
        END. /* not avail */
        ELSE
        ASSIGN
          rptLayout.rptLine = ttblRptLayout.rptLine
          rptLayout.rptColumn = ttblRptLayout.rptColumn
          rptLayout.excelColumn = ttblRptLayout.excelColumn.
        IF hiFormat LT ttblRptLayout.rptFormat THEN
        hiFormat = ttblRptLayout.rptFormat.
      END. /* if */
    END. /* repeat */
    INPUT CLOSE.
  END. /* do i */
  FOR EACH rptLayout WHERE rptLayout.rptFormat EQ hiFormat
      BREAK BY rptLayout.rptLine BY rptLayout.rptColumn:
    IF ipRptName EQ 'jobToolTip' THEN DO:
      ASSIGN
        cFieldLabel = rptLayout.fieldLabel + ': '
        cFieldLabel = REPLACE(cFieldLabel,'Skip Line: ','')
        .
      IF cFieldLabel BEGINS 'Status-' THEN
      ASSIGN
        cFieldLabel = REPLACE(cFieldLabel,'Status-','')
        cFieldLabel = REPLACE(cFieldLabel,':','')
        iFieldLabel = INT(TRIM(cFieldLabel))
        cFieldLabel = ENTRY(iFieldLabel,customLabelList) + ': '
        .
    END. /* if tooltip */
    opTextFields = opTextFields +
      (IF rptLayout.rptColumn NE 1 THEN ',' ELSE '') +
      (IF ipRptName EQ 'jobToolTip' THEN cFieldLabel ELSE '') + '@' +
      (IF rptLayout.fieldName NE 'calcTimeField' THEN rptLayout.fieldName ELSE rptLayout.fieldLabel) + '@'
      .
    IF LAST-OF(rptLayout.rptLine) AND NOT LAST(rptLayout.rptLine) THEN
    opTextFields = opTextFields + '|'.
  END. /* each rptlayout */
END PROCEDURE.

PROCEDURE updateColumns:
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  RUN loadUserFieldLabelWidth.
  RUN colCheck.
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/columns.dat')) NO-ECHO.
  CREATE bBrowseColumn.
  REPEAT:
    IMPORT bBrowseColumn.
    FIND FIRST colCheck EXCLUSIVE-LOCK
         WHERE (colCheck.fld1 EQ bBrowseColumn.colLabel
           AND  colCheck.fld2 EQ bBrowseColumn.colName)
            OR (colCheck.fld3 EQ bBrowseColumn.colLabel
           AND  colCheck.fld4 EQ bBrowseColumn.colName) NO-ERROR.
    IF NOT AVAILABLE colCheck OR colCheck.found THEN NEXT.
    IF bBrowseColumn.colLabel NE colCheck.fld3 OR
       bBrowseColumn.colName NE colCheck.fld4 THEN
    ASSIGN
      bBrowseColumn.colLabel = colCheck.fld3
      bBrowseColumn.colName = colCheck.fld4.
    ASSIGN
      colCheck.found = YES
      i = i + 1
      bBrowseColumn.colOrder = i.
    CREATE browseColumn.
    BUFFER-COPY bBrowseColumn TO browseColumn.
  END. /* repeat */
  INPUT CLOSE.
  
  OUTPUT TO VALUE(SEARCH('{&data}/' + ID + '/columns.dat')).
  FOR EACH browseColumn NO-LOCK BY browseColumn.colOrder:
    EXPORT browseColumn.
  END. /* each browsecolumn */
  FOR EACH colCheck NO-LOCK WHERE colCheck.found EQ NO:
    IF CAN-FIND(FIRST bColCheck WHERE bColCheck.fld3 EQ colCheck.fld3
                                  AND bColCheck.fld4 EQ colCheck.fld4
                                  AND bColCheck.found EQ YES) THEN NEXT.
    i = i + 1.
    EXPORT i YES colCheck.fld3 colCheck.fld4 0 0 0 NO 0.
  END. /* each colcheck */
  OUTPUT CLOSE.
END PROCEDURE.
