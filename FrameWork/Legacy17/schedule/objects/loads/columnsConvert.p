/* columnsConvert.p */

MESSAGE 'Convert Columns.dat to New Format 1 Layout?' VIEW-AS ALERT-BOX
  QUESTION BUTTONS YES-NO UPDATE convertColumns AS LOGICAL.
IF NOT convertColumns THEN RETURN.

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&viewers}/includes/sharedVars.i}
{{&includes}/rptTables.i}

&SCOPED-DEFINE rptName 'jobByResource'
&SCOPED-DEFINE rptFormat '1'

CREATE browseColumn.
IF NOT CAN-FIND(FIRST rptFormat
                WHERE rptFormat.rptID EQ ID
                  AND rptFormat.rptName EQ {&rptName}
                  AND rptFormat.rptFormat EQ {&rptFormat}) THEN DO:
  CREATE rptFormat.
  ASSIGN
    rptFormat.rptID = ID
    rptFormat.rptName = {&rptName}
    rptFormat.rptFormat = {&rptFormat}.
END. /* if not can-find */
INPUT FROM VALUE(findProgram('{&data}/',ID,'/columns.dat')) NO-ECHO.
REPEAT:
  IMPORT browseColumn.
  IF CAN-FIND(FIRST rptLayout
              WHERE rptLayout.rptID EQ ID
                AND rptLayout.rptName EQ {&rptName}
                AND rptLayout.rptFormat EQ {&rptFormat}
                AND rptLayout.fieldLabel EQ browseColumn.colLabel
                AND rptLayout.fieldName EQ browseColumn.colName) THEN NEXT.
  CREATE rptLayout.
  ASSIGN
    rptLayout.rptID = ID
    rptLayout.rptName = {&rptName}
    rptLayout.rptFormat = {&rptFormat}
    rptLayout.fieldLabel = browseColumn.colLabel
    rptLayout.fieldName = browseColumn.colName
    rptLayout.rptLine = browseColumn.rptLine
    rptLayout.rptColumn = browseColumn.rptCol
    rptLayout.excelColumn = browseColumn.excelCol.
END. /* repeat */
INPUT CLOSE.
RUN saveRptLayout.
RUN saveRptFormat.
MESSAGE 'Conversion Complete, Close & Re-Open' SKIP
        'to Access Job by Resource Format 1' VIEW-AS ALERT-BOX.
