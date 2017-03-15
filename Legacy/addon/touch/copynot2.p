/* touch/copynot2.p  copy notes to asi's notes database for job/order/estimate*/

DEF INPUT PARAMETER ip_rec_key LIKE nosweat.notes.rec_key.
DISABLE TRIGGERS FOR LOAD OF nosweat.notes.


IF CONNECTED("asinos") THEN DO:
   
   FOR EACH asinos.notes WHERE asinos.notes.rec_key = ip_rec_key :
       DELETE asinos.notes.
   END.

   FOR EACH nosweat.notes WHERE nosweat.notes.rec_key = ip_rec_key NO-LOCK:
       
       CREATE asinos.notes.
       BUFFER-COPY nosweat.notes TO asinos.notes. 
   END.
   
END.


