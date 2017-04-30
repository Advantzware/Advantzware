/* touch/copynoteA.p  copy notes from asi's notes database for job/order/estimate*/

DEF INPUT PARAMETER ip_rec_key LIKE nosweat.notes.rec_key.
DEF INPUT PARAM ip-header_value AS cha.

DISABLE TRIGGERS FOR LOAD OF nosweat.notes.

IF CONNECTED("asinos") THEN DO:
   
   FOR EACH nosweat.notes WHERE nosweat.notes.rec_key = ip_rec_key
                            AND nosweat.notes.note_group = ip-header_value :
       DELETE nosweat.notes.
   END.

   FOR EACH asinos.notes WHERE asinos.notes.rec_key = ip_rec_key
                                AND asinos.notes.note_group = ip-header_value  NO-LOCK:
       
       CREATE nosweat.notes.
       BUFFER-COPY asinos.notes TO nosweat.notes. 
   END.
   
END.


