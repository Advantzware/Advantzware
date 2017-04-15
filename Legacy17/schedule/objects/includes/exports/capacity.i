/* capacity.i - export format for ttblDowntime */

&IF DEFINED(dayID) EQ 0 &THEN
&SCOPED-DEFINE dayID 0
&ENDIF

&IF DEFINED(resource) EQ 0 &THEN
&SCOPED-DEFINE resource ''
&ENDIF

&IF DEFINED(startDate) EQ 0 &THEN
&SCOPED-DEFINE startDate ?
&ENDIF

&IF DEFINED(startTime) EQ 0 &THEN
&SCOPED-DEFINE startTime 0
&ENDIF

&IF DEFINED(endTime) EQ 0 &THEN
&SCOPED-DEFINE endTime 0
&ENDIF

EXPORT STREAM {&streamName}
  {&dayID}
  {&resource}
  {&startDate}
  {&startTime}
  {&endTime}.
