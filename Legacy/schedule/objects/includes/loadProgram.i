/* loadProgram.i - used in procedure setSize in board and main block of sbReport.p */

&IF '{&Board}' NE 'Basic' &THEN
    EMPTY TEMP-TABLE jobNotes.
&ENDIF
    RUN VALUE(findProgram('{&loads}/',ID,'/load{&Board}.p')) (containerHandle).
&IF '{&Board}' EQ 'Pro' AND DEFINED(sbExternal) EQ 0 &THEN
    OUTPUT TO VALUE(clientDat + '{&data}/' + ID + '/inUse.' + loginID + '.dat').
    EXPORT STRING(TODAY,'99.99.9999') STRING(TIME,'hh:mm:ss am') ENTRY(1,loginID,'.').
    OUTPUT CLOSE.
&ENDIF

