/* jobseq.i */


DEFINE BUFFER bf-jobseq FOR jobseq.
DEFINE VARIABLE iRowCount AS INTEGER NO-UNDO.

&IF '{&jobseq-delete}' NE '' &THEN

   FOR EACH bf-jobseq NO-LOCK:
       iRowCount = iRowCount + 1.
   END.
   IF iRowCount = 1 THEN DO:
       MESSAGE 'Cannot delete this job sequence. At least one job sequence is required.' VIEW-AS ALERT-BOX INFORMATION.
       RETURN ERROR.
   END.
   
&ENDIF
