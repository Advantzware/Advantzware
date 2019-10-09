     /* porep\r-rschrp.i */
     FOR EACH tt-fgs WHERE tt-fgs.sched-rowid = ROWID(tt-sched)
         WITH FRAME {1}
         BREAK BY tt-fgs.i-no
               BY tt-fgs.due-date:

         DISP
             tt-fgs.i-no WHEN FIRST-OF(tt-fgs.i-no) @ tt-sched.i-no 
             tt-fgs.cust-no @ tt-sched.vend-no
             tt-fgs.ord-no      @ tt-sched.ord-no
             tt-fgs.ord-qty     @ tt-sched.cons-qty
             tt-fgs.due-date    @ tt-sched.due-date
             "  " + tt-fgs.cust-name   @ tt-sched.i-name  
             tt-fgs.job-no      @ lv-job-no
             WITH FRAME {1}.
         DOWN WITH FRAME {1}.

         IF tb_excel AND v-sort = "V" THEN
            PUT STREAM excel UNFORMATTED
              '"' tt-fgs.cust-no                             '",'
              '"' ""                                         '",'
              '"' tt-fgs.i-no                                '",'
              '"' ""                                         '",'
              '"' "  " + tt-fgs.cust-name                    '",'
              '"' ""                                         '",'
              '"' tt-fgs.ord-no                              '",'
              '"'  ""                                        '",'
              '"' STRING(tt-fgs.ord-qty,"->>>,>>>,>>9.99")   '",'
              '"' ""                                         '",'
              '"' (IF tt-fgs.due-date NE ? THEN              
                      STRING(tt-fgs.due-date) ELSE "")       '",'
              '"' ""                                         '",'
              '"' ""                                         '",'
              '"' tt-fgs.job-no                              '",'
              SKIP.
         
         IF tb_excel AND v-sort = "I" THEN
            PUT STREAM excel UNFORMATTED
              '"' tt-fgs.i-no                                '",'
              '"' ""                                         '",'
              '"' "  " + tt-fgs.cust-name                    '",'
              '"' tt-fgs.cust-no                             '",'
              '"' ""                                         '",'
              '"' STRING(tt-fgs.ord-no)                       '",'
              '"' ""                                         '",'
              '"' ""                                         '",'
              '"' STRING(tt-fgs.ord-qty,"->>>,>>>,>>9.99")   '",'
              '"' ""                                         '",'
              '"' (IF tt-fgs.due-date NE ? THEN
                      STRING(tt-fgs.due-date) ELSE "")       '",'
              '"' ""                                         '",'
              '"' tt-fgs.job-no                              '",'
              SKIP.

        
     IF tb_excel AND v-sort = "J" THEN
        PUT STREAM excel UNFORMATTED
          '"' tt-fgs.job-no                                '",'
          '"' tt-fgs.i-no                                  '",'
          '"' ""                                           '",'
          '"' "  " + tt-fgs.cust-name                      '",'
          '"' tt-fgs.cust-no                               '",'
          '"' ""                                           '",'
          '"' ""                                           '",'
          '"' ""                                           '",'
          '"' ""                                           '",'
          '"' STRING(tt-fgs.ord-qty,"->>>,>>>,>>9.99")     '",'
          '"' ""                                           '",'
          '"' (IF tt-fgs.due-date NE ? THEN
                  STRING(tt-fgs.due-date) ELSE "")         '",'
          '"' ""                                           '",'
          SKIP.

    END.
