
      assign
       v-sman-no = ""
       v-prodc   = "MISC".

      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq ar-invl.i-no
          no-lock no-error.
      if avail itemfg and itemfg.procat ne "" then v-prodc = itemfg.procat.

      else do:
        find first fgcat
            where fgcat.company eq cocode
              and fgcat.glacc   eq ar-invl.actnum
            no-lock no-error.
        if avail fgcat then v-prodc = fgcat.procat.
      end.
      
      cp-part-no = "".
      cp-i-no = "" .
      IF AVAIL itemfg /*AND rd_fg-cp EQ "Cust Part#"*/ THEN DO:
        cp-rowid = ROWID(itemfg).
        RUN custom/getcpart.p (ar-inv.company, ar-inv.cust-no,
                               INPUT-OUTPUT cp-part-no, INPUT-OUTPUT cp-rowid).
        IF TRIM(cp-part-no) EQ "" THEN cp-part-no = itemfg.part-no.
      END.
     /* IF TRIM(cp-part-no) EQ "" THEN cp-part-no = ar-invl.i-no. */
      cp-i-no = ar-invl.i-no.

      if v-prodc ge fcat and
         v-prodc le tcat then
      do i = 1 to 3:
        v-sman-no = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                    else ar-invl.sman[i].

        if v-sman-no   lt fsman                         or
           v-sman-no   gt tsman                         or
           (i ne 1 and
            (v-sman-no eq "" or ar-invl.s-pct[i] eq 0)) then next.

        create xreport.

        assign
         xreport.rec-id  = recid({1})
         xreport.key-01  = "1"
         xreport.key-02  = v-prodc
         xreport.key-03  = v-sman-no
         xreport.key-04  = cust.cust-no
         xreport.key-05  = cp-part-no
         xreport.key-06  = cp-i-no   .

        if v-prodc eq "MISC" and i eq 1 then do:
          create xreport.

          assign
           xreport.rec-id  = recid({1})
           xreport.key-01  = "2"
           xreport.key-02  = "MISC"
           xreport.key-03  = v-sman-no
           xreport.key-04  = cust.cust-no.
        end.
      end.
