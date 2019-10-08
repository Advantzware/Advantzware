/* tooltip.i */

pWidget:TOOLTIP = 'Job: ' + ttblJob.job + ' - Seq: ' + STRING(ttblJob.jobSequence).
IF ttblJob.resourceSequence NE 0 THEN
pWidget:TOOLTIP = pWidget:TOOLTIP + ' - Routing: ' + STRING(ttblJob.resourceSequence).
pWidget:TOOLTIP = pWidget:TOOLTIP + '~n~nStart: ' +
  STRING(ttblJob.startDate,'99.99.9999') + ' @ ' +
  LEFT-TRIM(STRING(ttblJob.startTime,'HH:MM:SSam')) + '~nEnd: ' +
  STRING(ttblJob.endDate,'99.99.9999') + ' @ ' +
  LEFT-TRIM(STRING(ttblJob.endTime,'HH:MM:SSam')) + '~nDue: ' +
  STRING(ttblJob.dueDate,'99.99.9999') + '~nProd: ' +
  STRING(ttblJob.prodDate,'99.99.9999') +
  '~n~nStatus: ' + jobStatus().
