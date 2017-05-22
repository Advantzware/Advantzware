/* outputTo.i */

runTitle = IF ipTitle NE '' THEN ipTitle ELSE '{&printProgramTitle} ' + ipFormat.

FORM HEADER 'Scheduler{&Board} ({&version}) - ' + runTitle +
            ' [{&printProgramName}.p]' FORMAT 'X(78)'
    WITH FRAME topFrame PAGE-TOP NO-LABELS STREAM-IO.

FORMAT HEADER SKIP(1) 'Page:' STRING(PAGE-NUMBER)
  STRING(TODAY,'99.99.9999') + ' @ ' + STRING(TIME,'HH:MM:SSam') FORMAT 'X(23)'
    WITH FRAME bottomFrame PAGE-BOTTOM NO-LABELS NO-BOX STREAM-IO.

SESSION:SET-WAIT-STATE('GENERAL').
RUN getConfiguration.

&SCOPED-DEFINE useTtbl {&jobTable}
{{&viewers}/includes/setFilterFlag.i "print"}

printFile = clientDat + '{&print}/' + ID + '/{&printProgramName}.' +
             (IF ipExcel THEN USERID('NoSweat') + '.csv' ELSE 'txt').

IF ipExcel THEN linesPerPageValue = 0.

OUTPUT TO VALUE(printFile) PAGE-SIZE VALUE(linesPerPageValue).

&IF DEFINED(hideTopFrame) EQ 0 &THEN
IF NOT ipExcel THEN
VIEW FRAME topFrame.
&ENDIF

&IF DEFINED(hideBottomFrame) EQ 0 &THEN
IF NOT ipExcel THEN
VIEW FRAME bottomFrame.
&ENDIF
