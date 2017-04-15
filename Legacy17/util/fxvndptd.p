
   DEF BUFFER b-per FOR period.


   FOR EACH b-per WHERE b-per.pstat EQ YES
       BREAK BY b-per.company BY b-per.pnum:

     IF FIRST-OF(b-per.company) THEN
     FOR EACH period
         WHERE period.company EQ b-per.company
           AND period.yr      EQ b-per.yr
         no-lock,
         each vend where vend.company eq period.company transaction:

       status default "Please Wait...Updating Vendor/Year: " +
                      trim(vend.vend-no) + "/" + TRIM(STRING(period.yr,"9999")).

       assign
        vend.purch[period.pnum]   = 0
        vend.ptd-msf[period.pnum] = 0.

       for each ap-ledger where ap-ledger.company eq period.company and
                                ap-ledger.tr-date ge period.pst and
                                ap-ledger.tr-date le period.pend and
                                ap-ledger.vend-no eq vend.vend-no and
                                ap-ledger.refnum begins "INV#" no-lock:

         find first ap-inv where ap-inv.company eq period.company and
                                 ap-inv.vend-no eq vend.vend-no and
                                 ap-inv.posted  eq yes and
                                 ap-inv.inv-no eq substr(ap-ledger.refnum,6,length(ap-ledger.refnum))
                           USE-INDEX ap-inv no-lock no-error.

         if avail ap-inv THEN do:
           FOR each ap-invl where ap-invl.company eq period.company and
                                  ap-invl.inv-no eq ap-inv.inv-no and
                                  ap-invl.i-no eq ap-inv.i-no
                            use-index i-no no-lock:
             if ap-invl.amt-msf ne 0 then
               vend.ptd-msf[period.pnum] = vend.ptd-msf[period.pnum] + ap-invl.amt-msf.
             else do:
               find first itemfg where itemfg.company eq period.company and
                                       itemfg.i-no eq string(ap-invl.i-no)
                                 use-index i-no no-lock no-error.
               if avail itemfg then
                 vend.ptd-msf[period.pnum] = vend.ptd-msf[period.pnum] +
                                            ((ap-invl.qty / 1000) * itemfg.t-sqft).
             end.
           end.
         end. /* if avail ap-inv */

         assign vend.purch[period.pnum] = vend.purch[period.pnum] + ap-ledger.amt.
       end. /* for each ap-ledger INV */

       for each ap-ledger where ap-ledger.company eq period.company and
                                ap-ledger.tr-date ge period.pst and
                                ap-ledger.tr-date le period.pend and
                                ap-ledger.vend-no eq vend.vend-no and
                                (ap-ledger.refnum begins "Memo#" or
                                 ap-ledger.refnum begins "Chk#") no-lock:

         vend.purch[period.pnum] = vend.purch[period.pnum] + ap-ledger.amt.
       end. /* for each ap-ledger MEMO */
     END.
   END.
