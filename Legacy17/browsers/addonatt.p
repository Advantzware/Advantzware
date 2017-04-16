   DEFINE INPUT PARAMETER ip-rec_key AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER v-est-no AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER v-i-no AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER v-rec-key-list AS CHAR NO-UNDO.

   FIND FIRST machtran WHERE machtran.rec_key = ip-rec_key NO-LOCK NO-ERROR.
   
   IF AVAIL machtran THEN DO:
   
     FIND FIRST job WHERE
          job.company = machtran.company AND
          job.job-no = machtran.job_number AND
          job.job-no2 = machtran.job_sub
          NO-LOCK NO-ERROR.
   
     IF AVAIL job THEN DO:
        FIND FIRST est WHERE
             est.company EQ job.company AND
             est.est-no EQ job.est-no
             NO-LOCK NO-ERROR.
       
        IF AVAIL est THEN DO:
        
           ASSIGN
              v-est-no = est.est-no.
              v-rec-key-list = est.rec_key.

           FOR EACH eb FIELD(stock-no) WHERE
               eb.company = est.company AND
               eb.est-no = est.est-no
               NO-LOCK:
               IF eb.stock-no <> "" THEN
               DO:
                  v-i-no = v-i-no + eb.stock-no + ",".

                  FIND FIRST itemfg WHERE
                       itemfg.company EQ est.company AND
                       itemfg.i-no EQ eb.stock-no
                       NO-LOCK NO-ERROR.

                  IF AVAIL itemfg AND LOOKUP(itemfg.rec_key,v-rec-key-list) EQ 0 THEN
                     v-rec-key-list = v-rec-key-list + "," + itemfg.rec_key.
               END.
           END.
        END.
     END.
   END.
