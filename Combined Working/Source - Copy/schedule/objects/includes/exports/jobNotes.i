/* jobNotes.i - export format for jobNotes */

&IF DEFINED(noteDate) EQ 0 &THEN
&SCOPED-DEFINE noteDate TODAY
&ENDIF

&IF DEFINED(noteTime) EQ 0 &THEN
&SCOPED-DEFINE noteTime TIME
&ENDIF

&IF DEFINED(noteText) EQ 0 &THEN
&SCOPED-DEFINE noteText ''
&ENDIF

&IF DEFINED(noteKey) EQ 0 &THEN
&SCOPED-DEFINE noteKey ''
&ENDIF

EXPORT STREAM {&streamName}
  {&jobRowID}
  {&noteDate}
  {&noteTime}
  {&noteText}
  {&noteKey}.
