/* job.i - export format for ttblJob */

&IF DEFINED(resource) EQ 0 &THEN
&SCOPED-DEFINE resource ''
&ENDIF

&IF DEFINED(resourceSequence) EQ 0 &THEN
&SCOPED-DEFINE resourceSequence 0
&ENDIF

&IF DEFINED(altResSeq) EQ 0 &THEN
&SCOPED-DEFINE altResSeq 0
&ENDIF

&IF DEFINED(job) EQ 0 &THEN
&SCOPED-DEFINE job ''
&ENDIF

&IF DEFINED(jobSort) EQ 0 &THEN
&SCOPED-DEFINE jobSort {&job}
&ENDIF

&IF DEFINED(jobDescription) EQ 0 &THEN
&SCOPED-DEFINE jobDescription ''
&ENDIF

&IF DEFINED(jobToolTip) EQ 0 &THEN
&SCOPED-DEFINE jobToolTip ''
&ENDIF

&IF DEFINED(jobSequence) EQ 0 &THEN
&SCOPED-DEFINE jobSequence 0
&ENDIF

&IF DEFINED(resourceDescription) EQ 0 &THEN
&SCOPED-DEFINE resourceDescription ''
&ENDIF

&IF DEFINED(altResource) EQ 0 &THEN
&SCOPED-DEFINE altResource ''
&ENDIF

&IF DEFINED(department) EQ 0 &THEN
&SCOPED-DEFINE department '0000'
&ENDIF

&IF DEFINED(startDate) EQ 0 &THEN
&SCOPED-DEFINE startDate ?
&ENDIF

&IF DEFINED(startTime) EQ 0 &THEN
&SCOPED-DEFINE startTime 0
&ENDIF

&IF DEFINED(endDate) EQ 0 &THEN
&SCOPED-DEFINE endDate ?
&ENDIF

&IF DEFINED(endTime) EQ 0 &THEN
&SCOPED-DEFINE endTime 0
&ENDIF

&IF DEFINED(timeSpan) EQ 0 &THEN
&SCOPED-DEFINE timeSpan 0
&ENDIF
&IF DEFINED(jobLocked) EQ 0 &THEN
&SCOPED-DEFINE jobLocked NO
&ENDIF

&IF DEFINED(dueDate) EQ 0 &THEN
&SCOPED-DEFINE dueDate {{&includes}/lastDate.i}
&ENDIF

&IF DEFINED(dueTime) EQ 0 &THEN
&SCOPED-DEFINE dueTime 86400
&ENDIF

&IF DEFINED(prodDate) EQ 0 &THEN
&SCOPED-DEFINE prodDate {{&includes}/lastDate.i}
&ENDIF

&IF DEFINED(customValue) EQ 0 &THEN
&SCOPED-DEFINE customValue ''
&ENDIF

&IF DEFINED(completed) EQ 0 &THEN
&SCOPED-DEFINE completed NO
&ENDIF

&IF DEFINED(strRowID) EQ 0 &THEN
&SCOPED-DEFINE strRowID ''
&ENDIF

&IF DEFINED(keyValue) EQ 0 &THEN
&SCOPED-DEFINE keyValue ''
&ENDIF

&IF DEFINED(udfField) EQ 0 &THEN
&SCOPED-DEFINE udfField udfField
&ENDIF

&IF DEFINED(userField) EQ 0 &THEN
&SCOPED-DEFINE userField userField
&ENDIF

&IF DEFINED(jobStatus) EQ 0 &THEN
&SCOPED-DEFINE jobStatus jobStatus
&ENDIF

&IF DEFINED(statusTimeStamp) EQ 0 &THEN
&SCOPED-DEFINE statusTimeStamp statusTimeStamp
&ENDIF

&IF DEFINED(liveUpdate) EQ 0 &THEN
&SCOPED-DEFINE liveUpdate liveUpdate
&ENDIF

&IF DEFINED(lagTime) EQ 0 &THEN
&SCOPED-DEFINE lagTime lagTime
&ENDIF

EXPORT STREAM {&streamName}
  {&resource}
  {&jobSequence}
  {&job}
  {&jobSort}
  {&jobDescription}
  {&resourceSequence}
  {&resourceSequence}
  {&altResSeq}
  {&resourceDescription}
  {&altResource}
  {&department}
  {&startDate}
  {&startTime}
  {&endDate}
  {&endTime}
  {&timeSpan}
  {&jobLocked}
  {&dueDate}
  {&dueTime}
  {&prodDate}
  {&customValue}
  {&completed}
  {&strRowID}
  {&keyValue}
  {&udfField}
  {&userField}
  {&jobStatus}
  {&statusTimeStamp}
  {&liveUpdate}
  {&lagTime}
  {&jobToolTip}
  .
