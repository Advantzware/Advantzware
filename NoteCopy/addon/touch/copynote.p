/* touch/copynote.p  copy notes from asi's notes database for job/order/estimate*/

DEF INPUT PARAMETER ip_rec_key LIKE nosweat.notes.rec_key.
DISABLE TRIGGERS FOR LOAD OF nosweat.notes.

IF CONNECTED("nosweat-asi") THEN DO:
   
   FOR EACH nosweat.notes WHERE nosweat.notes.rec_key = ip_rec_key :
       DELETE nosweat.notes.
   END.

   FOR EACH nosweat-asi.notes WHERE nosweat-asi.notes.rec_key = ip_rec_key NO-LOCK:
       
       CREATE nosweat.notes.
       BUFFER-COPY nosweat-asi.notes TO nosweat.notes. 
   END.
   
END.


