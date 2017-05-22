/* fg/fgemails.i fgemails = "NONE,Underrun,Receipts"
                     None: don't email 
                     underrun: email only qty is underrun
                     receipts: email all the time */

IF fgemails <> "NONE" THEN DO:
   FIND FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id NO-ERROR.
   IF AVAIL fg-rctd THEN DO:
       FIND FIRST cust WHERE cust.company = fg-rctd.company
                         AND cust.cust-no = itemfg.cust-no NO-LOCK NO-ERROR.
      FIND FIRST job-hdr WHERE job-hdr.company = fg-rctd.company
                           AND job-hdr.job-no = fg-rctd.job-no
                           AND job-hdr.job-no2 = fg-rctd.job-no2
                           AND job-hdr.i-no = fg-rctd.i-no NO-LOCK NO-ERROR.
      IF AVAIL job-hdr THEN DO:
         FIND FIRST oe-ord WHERE oe-ord.company = job-hdr.company
                             AND oe-ord.ord-no = job-hdr.ord-no NO-LOCK NO-ERROR.
         FIND FIRST oe-ordl WHERE oe-ordl.company = job-hdr.company
                             AND oe-ordl.ord-no = job-hdr.ord-no
                             AND oe-ordl.i-no = job-hdr.i-no NO-LOCK NO-ERROR.
      END.
      v-underrun = IF AVAIL oe-ordl THEN oe-ordl.under-pct ELSE
                   IF AVAIL oe-ord  THEN oe-ord.under-pct  ELSE
                   IF AVAIL cust    THEN cust.under-pct    ELSE 0.

      IF fgemails = "RECEIPTS" THEN DO:
        /* PUT STREAM st-email 
             fg-rctd.job-no + "-" + string(fg-rctd.job-no2,"99") FORM "x(10)"
             " " fg-rctd.i-no " " fg-rctd.t-qty FORM "->>>,>>>,>>9" SKIP. */
          CREATE tt-email.
          ASSIGN tt-email.job-no = fg-rctd.job-no
                 tt-email.job-no2 = fg-rctd.job-no2
                 tt-email.i-no = fg-rctd.i-no
                 tt-email.qty = fg-rctd.t-qty
                 tt-email.cust-no = IF AVAIL oe-ord THEN oe-ord.cust-no
                                    ELSE IF AVAIL job-hdr THEN job-hdr.cust-no
                                    ELSE itemfg.cust-no.          
         v-got-fgemail = YES.
      END.
      ELSE IF fgemails = "Underrun" THEN DO:
          v-qty-received = 0 /*fg-rctd.t-qty*/ .
          FOR each fg-rcpth WHERE fg-rcpth.company EQ cocode
                              AND fg-rcpth.i-no    EQ fg-rctd.i-no
                              AND fg-rcpth.rita-code = "R"
                              AND fg-rcpth.job-no  EQ fg-rctd.job-no
                              AND fg-rcpth.job-no2 EQ fg-rctd.job-no2
                              USE-INDEX i-no NO-LOCK,
              FIRST fg-rdtlh WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                               AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code NO-LOCK:

              v-qty-received = v-qty-received + fg-rdtlh.qty.
          END.
          IF AVAIL job-hdr AND v-qty-received < job-hdr.qty * v-underrun / 100 THEN DO:
             v-got-fgemail = YES.
             CREATE tt-email.
             ASSIGN tt-email.job-no = fg-rctd.job-no
                 tt-email.job-no2 = fg-rctd.job-no2
                 tt-email.i-no = fg-rctd.i-no
                 tt-email.qty = fg-rctd.t-qty
                 tt-email.cust-no = IF AVAIL oe-ord THEN oe-ord.cust-no
                                    ELSE IF AVAIL job-hdr THEN job-hdr.cust-no
                                    ELSE itemfg.cust-no.
          /*
             PUT STREAM st-email UNFORMATTED
                 fg-rctd.job-no + "-" + string(fg-rctd.job-no2,"99") FORM "x(10)"
                 " " fg-rctd.i-no " " fg-rctd.t-qty FORM "->>>,>>>,>>9" SKIP.
          */       
/*MESSAGE fg-rctd.t-qty v-qty-received job-hdr.qty v-underrun job-hdr.qty * v-underrun VIEW-AS ALERT-BOX.*/
          END.
             
      END.

   END. /* avail fg-rctd */
   
END.
