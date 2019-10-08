/* ---------------------------------------------- oe/rep/boltrilx.i */
/* PRINT Xprint Trilakes BOL                                        */
/* ---------------------------------------------------------------- */

assign
   v-tot-wt = 0
   v-tot-units = 0.

for each report where report.term-id eq v-term-id,

    first oe-boll where recid(oe-boll) eq report.rec-id,

    first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,

    first itemfg
    where itemfg.company eq oe-boll.company
      and itemfg.i-no    eq oe-boll.i-no
    no-lock
    BREAK by report.key-01
          by report.key-02:

    IF FIRST-OF(report.key-02) THEN
       ASSIGN v-ship-qty = 0
              v-weight   = 0.
    
    v-tot-pkgs = v-tot-pkgs + oe-boll.cases +
                 if oe-boll.partial gt 0 then 1 else 0.

    /* gdm - */
    ASSIGN v-p-c = "P".

    if oe-boll.qty-case ne 0 and oe-boll.cases ne 0 then do:
       find first w2 where w2.cas-cnt eq oe-boll.qty-case no-error.
       if not avail w2 then create w2.
       assign
         w2.cas-cnt = oe-boll.qty-case
         w2.cases   = w2.cases + oe-boll.cases.
    end.
   
    if oe-boll.partial ne 0 then do:
       find first w2 where w2.cas-cnt eq oe-boll.partial no-error.
       if not avail w2 then create w2.
       assign
         w2.cas-cnt = oe-boll.partial
         w2.cases   = w2.cases + 1.
    end.
   
    find first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-boll.ord-no
          and oe-ordl.i-no    eq oe-boll.i-no
        no-lock no-error.
   
    ASSIGN
       v-job-no = ""
       v-ship-qty = v-ship-qty + oe-boll.qty
       v-weight   = v-weight + oe-boll.weight
       v-tot-wt = v-tot-wt + oe-boll.weight.
       
    if oe-boll.weight eq 0 then
       v-tot-wt = v-tot-wt + (oe-boll.qty / 100 * itemfg.weight-100).

    if avail oe-ordl and oe-ordl.job-no ne "" then
       v-job-no = fill(" ",6 - length(trim(oe-ordl.job-no))) +
                  trim(oe-ordl.job-no) + "-" + trim(string(oe-ordl.job-no2,"99")).
   
    IF v-printline >= 39 THEN DO:
       v-printline = 0.
       PAGE {1}.
       {oe/rep/boltrilx2.i}     
    END.
            
    /*show summary per item - did not incorporate non-summary*/
   
    IF LAST-OF(report.key-02) THEN DO:
          FOR EACH w2 BREAK BY w2.cases * w2.cas-cnt DESC:

          IF FIRST(w2.cases * w2.cas-cnt) THEN
          DO:
              /* gdm - */
             IF oe-boll.p-c THEN v-p-c = "C".

             DISPLAY  {1}
                oe-ordl.i-no WHEN AVAIL oe-ordl 
                oe-boll.ord-no AT 18 
                v-job-no AT 34 FORM "x(10)"
                oe-ordl.i-name AT 44 FORM "x(22)"
                w2.cases AT 66 FORM "->>>9"
                " @ " AT 71
                w2.cas-cnt AT 75 FORM "->>>>>9"
                v-p-c AT 84
                with frame bol-mid1 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.

             down {1} with frame bol-mid1.

             v-printline = v-printline + 2.
          END.
          ELSE

          DO:
             PUT {1}
                 oe-ordl.part-no
                 IF lv-print-lot THEN oe-boll.lot-no ELSE oe-boll.po-no AT 18 FORM "x(15)"                 
                 oe-ordl.part-dscr1 AT 44 FORM "x(22)"
                 w2.cases AT 66 FORM "->>>9"
                 " @ " AT 71
                 w2.cas-cnt AT 75 FORM "->>>>>9".

             PUT {1} SKIP.

             v-printline = v-printline + 1.
          END.
         
          IF LAST(w2.cases * w2.cas-cnt) THEN DO:
         
             IF FIRST(w2.cases * w2.cas-cnt) THEN DO:
                PUT {1}
                    oe-ordl.part-no
                     IF lv-print-lot THEN oe-boll.lot-no ELSE oe-boll.po-no AT 18 FORM "x(15)"
                    oe-ordl.part-dscr1 AT 44 FORM "x(22)"
                    SKIP.
                v-printline = v-printline + 1.
             END.
         
             PUT {1}  "=======================" AT 63 SKIP
                      v-tot-pkgs FORM "->>>>" AT 66
                      " = " AT 71
                      v-ship-qty AT 75 FORM "->>>>>9" SPACE(10)
                      v-weight SKIP.
                        
             put {1} skip(1).

             assign
                v-printline = v-printline + 2
                v-tot-units = v-tot-units + v-tot-pkgs
                v-tot-pkgs = 0.
          END. /*LAST(w2.cases * w2.cas-cnt)*/

          DELETE w2.
       END. /*each w2*/
    END. /*last-of report.key2*/
  
end. /* for each report */

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
