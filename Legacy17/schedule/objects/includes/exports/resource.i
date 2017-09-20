/* resource.i - export format for ttblResource */

&IF DEFINED(resource) EQ 0 &THEN
&SCOPED-DEFINE resource ''
&ENDIF

&IF DEFINED(resourceDescription) EQ 0 &THEN
&SCOPED-DEFINE resourceDescription ''
&ENDIF

&IF DEFINED(sortOrder) EQ 0 &THEN
&SCOPED-DEFINE sortOrder 0
&ENDIF

&IF DEFINED(department) EQ 0 &THEN
&SCOPED-DEFINE department ''
&ENDIF

&IF DEFINED(kicks) EQ 0 &THEN
&SCOPED-DEFINE kicks 0
&ENDIF

&IF DEFINED(dmiID) EQ 0 &THEN
&SCOPED-DEFINE dmiID 0
&ENDIF

EXPORT STREAM {&streamName} 0
  {&resource}
  {&resourceDescription}
  {&sortOrder}
  {&sortOrder}
  {&department}
  {&kicks}
  {&dmiID}
  .
