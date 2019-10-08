/* touch/copynote.p  copy notes from asi's notes database for job/order/estimate*/

DEF INPUT PARAMETER ip_rec_key LIKE nosweat.notes.rec_key.
DISABLE TRIGGERS FOR LOAD OF nosweat.notes.

IF CONNECTED("asinos") THEN DO:
   
   FOR EACH nosweat.notes WHERE nosweat.notes.rec_key = ip_rec_key :
       DELETE nosweat.notes.
   END.

   FOR EACH asinos.notes WHERE asinos.notes.rec_key = ip_rec_key NO-LOCK:
       
       CREATE nosweat.notes.
       BUFFER-COPY asinos.notes TO nosweat.notes. 
   END.
   
END.


