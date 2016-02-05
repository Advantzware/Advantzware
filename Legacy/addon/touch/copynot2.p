/* touch/copynot2.p  copy notes to asi's notes database for job/order/estimate*/

DEF INPUT PARAMETER ip_rec_key LIKE nosweat.notes.rec_key.
DISABLE TRIGGERS FOR LOAD OF nosweat.notes.


IF CONNECTED("nosweat-asi") THEN DO:
   
   FOR EACH nosweat-asi.notes WHERE nosweat-asi.notes.rec_key = ip_rec_key :
       DELETE nosweat-asi.notes.
   END.

   FOR EACH nosweat.notes WHERE nosweat.notes.rec_key = ip_rec_key NO-LOCK:
       
       CREATE nosweat-asi.notes.
       BUFFER-COPY nosweat.notes TO nosweat-asi.notes. 
   END.
   
END.


