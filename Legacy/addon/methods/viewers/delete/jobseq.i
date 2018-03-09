/* jobseq.i */


DEFINE BUFFER bf-jobseq FOR jobseq.
DEFINE VARIABLE iRowCount AS INTEGER NO-UNDO.

&IF '{&jobseq-delete}' NE '' &THEN

   FOR EACH bf-jobseq NO-LOCK:
       iRowCount = iRowCount + 1.
   END.
   IF iRowCount = 1 THEN DO:
       MESSAGE 'Cannot delete job sequence. Need atleast one job sequence' VIEW-AS ALERT-BOX INFORMATION.
       RETURN ERROR.
   END.
   
&ENDIF
