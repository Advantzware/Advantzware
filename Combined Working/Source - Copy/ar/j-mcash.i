/* ar/j-mcash.i */

     CLOSE QUERY {&browse-name}.

     EMPTY TEMP-TABLE tt-mcash.

     IF browse-order = 1 THEN
        v-posted = NO.
     ELSE
        v-posted = YES.
   
     FOR EACH ar-mcash WHERE 
         ar-mcash.company = g_company AND
         ar-mcash.posted EQ v-posted AND
         ar-mcash.payer BEGINS auto_find
         NO-LOCK
         BY ar-mcash.payer:
    

         FIND FIRST reftable WHERE
              reftable.reftable = "AR-MCASH" AND
              reftable.company  = ar-mcash.company AND
              reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9") AND
              reftable.code     = ar-mcash.rec_key
              NO-LOCK NO-ERROR.

         IF AVAIL reftable THEN
            v-check-no = reftable.code2.
         ELSE
            v-check-no = FILL("0",10).

         FIND FIRST tt-mcash WHERE
              tt-mcash.company = ar-mcash.company AND
              tt-mcash.posted = ar-mcash.posted AND
              tt-mcash.payer = ar-mcash.payer AND
              tt-mcash.check-date = ar-mcash.check-date AND
              tt-mcash.bank-code = ar-mcash.bank-code AND
              tt-mcash.curr-code[1] = ar-mcash.curr-code[1] AND
              tt-mcash.check-no = v-check-no
              NO-ERROR.
         
         IF NOT AVAIL tt-mcash THEN
            DO:
               CREATE tt-mcash.
               BUFFER-COPY ar-mcash EXCEPT check-amt TO tt-mcash
                  ASSIGN tt-mcash.check-no = v-check-no.
            END.
    
         tt-mcash.check-amt = tt-mcash.check-amt + ar-mcash.check-amt.
    
         RELEASE tt-mcash.
     END.
  
CASE browse-order :
    when 1 THEN DO:
         OPEN QUERY {&browse-name}  FOR EACH tt-mcash /*WHERE
              tt-mcash.posted EQ NO AND
              tt-mcash.payer BEGINS auto_find*/ ,
              FIRST ar-mcash WHERE ar-mcash.rec_key = tt-mcash.rec_key
                    NO-LOCK,
              FIRST reftable OUTER-JOIN WHERE
                    reftable.reftable = "AR-MCASH" AND
                    reftable.company  = ar-mcash.company AND
                    reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9") AND
                    reftable.code     = ar-mcash.rec_key
                    NO-LOCK
                    BY tt-mcash.payer.
    END.
    when 2 THEN DO:
        OPEN QUERY {&browse-name}  FOR EACH tt-mcash /*WHERE
             tt-mcash.posted EQ YES AND
             tt-mcash.payer BEGINS auto_find*/ ,
             FIRST ar-mcash WHERE ar-mcash.rec_key = tt-mcash.rec_key
                   NO-LOCK,
            FIRST reftable OUTER-JOIN WHERE
                  reftable.reftable = "AR-MCASH" AND
                  reftable.company  = ar-mcash.company AND
                  reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9") AND
                  reftable.code     = ar-mcash.rec_key
                  NO-LOCK
                  BY tt-mcash.payer.
    END.
    
END CASE.

