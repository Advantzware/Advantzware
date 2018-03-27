

    IF FIRST(fg-rcpth.trans-date) THEN tt-fg-bin.aging-date = fg-rcpth.trans-date.

    IF fg-rcpth.rita-code EQ "R" THEN DO:
      IF tt-fg-bin.case-count   LE 0 AND fg-rdtlh.qty-case     GT 0 THEN
        tt-fg-bin.case-count   = fg-rdtlh.qty-case.
      IF tt-fg-bin.units-pallet LE 0 AND fg-rdtlh.units-pallet GT 0 THEN
        tt-fg-bin.units-pallet = fg-rdtlh.units-pallet.
      IF tt-fg-bin.cases-unit   LE 0 AND fg-rdtlh.stacks-unit  GT 0 THEN
        tt-fg-bin.cases-unit   = fg-rdtlh.stacks-unit.
    END.

    IF INDEX("RTA",fg-rcpth.rita-code) GT 0                       OR
       (fg-rcpth.rita-code EQ "C" AND FIRST(fg-rcpth.trans-date)) THEN DO:
      FIND FIRST fg-rctd WHERE fg-rctd.r-no EQ fg-rcpth.r-no USE-INDEX fg-rctd NO-LOCK NO-ERROR.

      /* CREATE tt-fg-rctd. */

      IF AVAIL fg-rctd         AND
         fg-rctd.t-qty NE 0    AND
         fg-rctd.ext-cost NE 0 AND
         fg-rctd.ext-cost NE ? THEN
          ASSIGN
            lvdRctdT-qty     = fg-rctd.t-qty     
            lvdRctdPartial   = fg-rctd.partial   
            lvcRctdJob-no    = fg-rctd.job-no    
            lviRctdJob-no2   = fg-rctd.job-no2
            lvcRctdCompany   = fg-rctd.company   
            lvcRctdI-no      = fg-rctd.i-no      
            lvcRctdRita-code = fg-rcpth.rita-code
            lvdExtCost       = fg-rctd.ext-cost .

      ELSE DO:

        ASSIGN
           lvdRctdPartial    = fg-rdtlh.partial
           lvcRctdRita-code  = fg-rcpth.rita-code           
           lvcRctdCompany    = fg-rdtlh.company 
           lvcRctdJob-no     = fg-rdtlh.job-no   
           lviRctdJob-no2    = fg-rdtlh.job-no2 
           lvcRctdI-no       = fg-rcpth.i-no

           lvdRctdT-qty      = IF fg-rdtlh.qty = 0 THEN 1
                               ELSE fg-rdtlh.qty                     
           lvdExtCost        = fg-rdtlh.cost *
                                IF fg-rdtlh.qty <> 0 THEN
                               (fg-rdtlh.qty / IF fg-rcpth.pur-uom EQ "M" THEN 1000 ELSE 1)
                               ELSE (1 / IF fg-rcpth.pur-uom EQ "M" THEN 1000 ELSE 1).
         
      END.

      IF fg-rcpth.rita-code EQ "R" AND
         lvdRctdT-qty NE 0     AND
         lvdExtCost NE 0  AND
         lvdExtCost NE ?  THEN
        ASSIGN
         lv-uom      = "M"
         ld-cvt-cost = lvdExtCost / lvdRctdT-qty * 1000.
      ELSE DO:
          
         ASSIGN lv-uom      = fg-rcpth.pur-uom /*itemfg.prod-uom*/
                ld-cvt-cost = fg-rdtlh.cost.
         IF lv-uom EQ "" THEN DO:
             lv-uom = tt-fg-bin.pur-uom.
             IF lv-uom EQ "" THEN
                 lv-uom = itemfg.prod-uom.
         END.
      END.

      IF ld-cvt-cost NE 0 AND ld-cvt-cost NE ?                  AND
         (lvdRctdT-qty NE 0 OR lvcRctdRita-code EQ "A") AND
         lvdRctdT-qty NE ?                                  THEN DO:
        find first job-hdr where job-hdr.company eq cocode
                          and job-hdr.job-no  eq tt-fg-bin.job-no
                          and job-hdr.job-no2 eq tt-fg-bin.job-no2
                          and job-hdr.i-no    eq tt-fg-bin.i-no
                no-lock no-error.
        
        /* {fg/upd-bin.i "tt-fg-bin" "lv-uom" "ld-cvt-cost" tt-fg-rctd} */
        /* upd-bin defined in upfgbinc.p, no need for tt-fg-rctd */
        RUN upd-bin      (INPUT-OUTPUT    tt-fg-bin.pur-uom,
                          INPUT-OUTPUT    tt-fg-bin.qty ,
                          INPUT-OUTPUT    tt-fg-bin.partial-count ,
                          INPUT-OUTPUT    tt-fg-bin.std-tot-cost ,
                          INPUT-OUTPUT    tt-fg-bin.std-mat-cost ,
                          INPUT-OUTPUT    tt-fg-bin.std-lab-cost ,
                          INPUT-OUTPUT    tt-fg-bin.std-var-cost ,
                          INPUT-OUTPUT    tt-fg-bin.std-fix-cost ,
                          INPUT           itemfg.prod-uom ,
                          INPUT           lvdRctdT-qty ,
                          INPUT           lvdRctdPartial ,
                          INPUT           lvcRctdJob-no ,
                          INPUT           lvcRctdCompany ,
                          INPUT           lviRctdJob-no2 ,
                          INPUT           lvcRctdI-no ,
                          INPUT           lvcRctdRita-code ,
                          INPUT           lv-uom  ,
                          INPUT           ld-cvt-cost ,
                          OUTPUT          v-cost     ,
                          OUTPUT          v-binqty,
                          OUTPUT          v-qty,
                          OUTPUT          v-tagcost).
/*
        MESSAGE          "loc" tt-fg-bin.loc "bin" tt-fg-bin.loc-bin "tag" tt-fg-bin.tag 
                          "job" tt-fg-bin.job-no "po" tt-fg-bin.po-no "job2" tt-fg-bin.job-no2 SKIP
                         "tt-fg-bin.pur-uom"    tt-fg-bin.pur-uom skip
                         "tt-fg-bin.qty"    tt-fg-bin.qty skip
                         "tt-fg-bin.partial-count"    tt-fg-bin.partial-count skip
                         "tt-fg-bin.std-tot-cost"    tt-fg-bin.std-tot-cost skip
                         "tt-fg-bin.std-mat-cost"    tt-fg-bin.std-mat-cost skip
                         "tt-fg-bin.std-lab-cost"    tt-fg-bin.std-lab-cost skip
                         "tt-fg-bin.std-var-cost"  tt-fg-bin.std-var-cost skip
                         "tt-fg-bin.std-fix-cost"   tt-fg-bin.std-fix-cost skip
                         "itemfg.prod-uom"           itemfg.prod-uom skip
                         "lvdRctdT-qty"         lvdRctdT-qty skip
                         "lvdRctdPartial"         lvdRctdPartial skip
                        "lvcRctdJob-no"        lvcRctdJob-no skip
                        "lvcRctdCompany"         lvcRctdCompany skip
                        "lviRctdJob-no2"        lviRctdJob-no2 skip
                        "lvcRctdI-no"       lvcRctdI-no skip
                         "lvcRctdRita-code"        lvcRctdRita-code skip
                         "lv-uom"       lv-uom  skip
                       "ld-cvt-cost"          ld-cvt-cost skip
                        "v-cost"           v-cost     skip
                         "v-binqty"        v-binqty skip
                          "v-qty"         v-qty skip
                          "v-tagcost"         v-tagcost skip
            VIEW-AS ALERT-BOX INFO BUTTONS OK. */


      END.

      ELSE DO:
        {fg/fg-mkbin.i tt-}
      END.

      /* DELETE tt-fg-rctd. */
    END.

    ELSE DO:
      IF fg-rcpth.rita-code EQ "S"   AND
         tt-fg-bin.std-tot-cost NE ? AND
         tt-fg-bin.std-tot-cost NE 0 THEN DO:
        /* WFK 04071501 no longer updating history here per Joe */
        FIND b-fg-rcpth WHERE ROWID(b-fg-rcpth) EQ ROWID(fg-rcpth) NO-LOCK NO-ERROR.
        FIND b-fg-rdtlh WHERE ROWID(b-fg-rdtlh) EQ ROWID(fg-rdtlh) NO-LOCK NO-ERROR.
      END.

      {fg/fg-mkbin.i tt-}
    END.
