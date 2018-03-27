/* addon/touch/jobrcpt.i */

      x = 1.
      FOR EACH fg-rctd no-lock BY fg-rctd.r-no DESC:
        LEAVE.
      END.
      if avail fg-rctd then x = fg-rctd.r-no.

      find last fg-rcpth use-index r-no no-lock no-error.
      if avail fg-rcpth and fg-rcpth.r-no GT x then x = fg-rcpth.r-no.

      create fg-rctd.
      assign
       fg-rctd.r-no       = X + 1
       fg-rctd.rct-date   = TODAY /*c-prdd.op-date*/
       fg-rctd.trans-time = TIME
       fg-rctd.company    = job-hdr.company
       fg-rctd.rita-code  = "R"
       fg-rctd.i-name     = itemfg.i-name
       fg-rctd.i-no       = job-hdr.i-no
       fg-rctd.job-no     = job-hdr.job-no
       fg-rctd.job-no2    = job-hdr.job-no2.
                 
      assign
       v-up  = 1
       v-out = 1.

      if avail est and index("APB",mach.p-type) le 0 then do:
        run sys/inc/numup.p (est.company, est.est-no, job-mch.frm, output v-up).
                 
        find first est-op
            where est-op.company eq est.company
              and est-op.est-no  eq est.est-no
              and est-op.s-num   eq job-hdr.frm
              and (est-op.b-num  eq job-hdr.blank-no or
                   job-hdr.blank-no eq 0)
              and est-op.m-code  eq job-mch.m-code
              and est-op.op-pass eq job-mch.pass
              and est-op.dept    eq job-mch.dept
              and est-op.line    lt 500
            no-lock no-error.
        if avail est-op and est-op.n-out ne 0 then v-out = est-op.n-out.
      end.

      ASSIGN
       fg-rctd.b-num      = job-hdr.blank-no
       fg-rctd.s-num      = job-hdr.frm
       fg-rctd.t-qty      = machtran.run_qty / v-up-hs * v-out * v-up  /*v-runqty*/
       fg-rctd.pur-uom    = itemfg.prod-uom
       fg-rctd.cost-uom   = itemfg.prod-uom
       fg-rctd.std-cost   = job-hdr.std-tot-cost
       fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
       fg-rctd.qty-case   = itemfg.case-count
       fg-rctd.partial    = fg-rctd.t-qty modulo itemfg.case-count
       fg-rctd.cases      = trunc(fg-rctd.t-qty / itemfg.case-count,0)
       fg-rctd.cases-unit = 1.

      if fg-rctd.t-qty le 0 then fg-rctd.cases = 0.
      release fg-bin.

      FIND FIRST reftable NO-LOCK
          WHERE reftable.reftable EQ "pc/pcprddu3.p"
            AND reftable.company  EQ pc-prdd.company
            AND reftable.code     EQ /*pc-prdd.rec_key*/ STRING(RECID(pc-prdd))
          NO-ERROR.

      IF AVAIL reftable THEN DO:
        ASSIGN
         fg-rctd.cases      = reftable.val[1]
         fg-rctd.qty-case   = reftable.val[2]
         fg-rctd.cases-unit = reftable.val[3]
         fg-rctd.partial    = fg-rctd.t-qty - (fg-rctd.cases * fg-rctd.qty-case).
        
        FIND FIRST fg-bin 
            WHERE fg-bin.rec_key EQ reftable.code2 /*RECID(fg-bin) EQ INT(reftable.code2)*/ 
            NO-LOCK NO-ERROR.
      END.
      IF AVAIL fg-bin THEN
        ASSIGN
         v-loc       = fg-bin.loc
         v-loc-bin   = fg-bin.loc-bin
         fg-rctd.tag = fg-bin.tag.
                
      else
      if v-auto-bin eq "ShipTo" then do:
        /*get estimate blank file from finished goods item file*/
        find first eb
            where eb.company  eq company_code
              and eb.est-no   eq itemfg.est-no
              and eb.stock-no eq itemfg.i-no
            use-index est-no no-lock no-error.

        if avail eb then do:
          /*get customer file from estimate blank file*/
          find first cust
              where cust.company eq company_code
                and cust.cust-no eq eb.cust-no
              no-lock no-error.
          if avail cust then do:              
            find first shipto
                where shipto.company = company_code
                  and shipto.cust-no = cust.cust-no 
                no-lock no-error.
            if avail shipto then do:
              find first fg-bin
                  where fg-bin.company eq company_code
                    and fg-bin.loc     eq shipto.loc
                    and fg-bin.loc-bin eq shipto.loc-bin
                    and fg-bin.i-no    eq ""
                  no-lock no-error.
              if avail fg-bin then 
                ASSIGN
                 v-loc     = shipto.loc
                 v-loc-bin = shipto.loc-bin.
            end.
                          
            if v-loc eq "" and v-loc-bin eq "" then do:
              find first fg-bin
                  where fg-bin.company eq company_code
                    and fg-bin.loc     eq itemfg.def-loc
                    and fg-bin.loc-bin eq itemfg.def-loc-bin
                    and fg-bin.i-no    eq ""
                  no-lock no-error.
              if avail fg-bin then 
                assign 
                 v-loc     = itemfg.def-loc
                 v-loc-bin = itemfg.def-loc-bin.
            end. /*if avail shipto*/
          end. /*if avail cust*/
        end. /*if avail eb*/
      end. /*if system default is shipto*/
      /*else if "FGFILE" then get from finished goods file*/
      else do:
        find first fg-bin
            where fg-bin.company eq company_code
              and fg-bin.loc     eq itemfg.def-loc
              and fg-bin.loc-bin eq itemfg.def-loc-bin
              and fg-bin.i-no    eq ""
            no-lock no-error.
        if avail fg-bin then 
          ASSIGN
           v-loc     = itemfg.def-loc
           v-loc-bin = itemfg.def-loc-bin.
      end. /*else FGFILE*/
          
      /*if bin and warehouse are blank, goto cust "X" shipto file*/
      if v-loc eq "" and v-loc-bin eq "" then do:
        find first cust
            where cust.company eq company_code
              and cust.active  eq "X"
            no-lock no-error.
                                
        if avail cust then do:
          find first shipto
              where shipto.company eq company_code
                and shipto.cust-no eq cust.cust-no  
              no-lock no-error.
          if avail shipto then do:
            find first fg-bin
                where fg-bin.company eq company_code
                  and fg-bin.loc     eq shipto.loc
                  and fg-bin.loc-bin eq shipto.loc-bin
                  and fg-bin.i-no    eq ""
                no-lock no-error.
             ASSIGN
              v-loc     = shipto.loc
              v-loc-bin = shipto.loc-bin.
          end.                                  
        end.
      end.

      ASSIGN
       fg-rctd.loc     = v-loc
       fg-rctd.loc-bin = v-loc-bin.

      FIND FIRST fg-bin
          WHERE fg-bin.company EQ fg-rctd.company
            AND fg-bin.i-no    EQ fg-rctd.i-no
            AND fg-bin.job-no  EQ job-hdr.job-no
            AND fg-bin.job-no2 EQ job-hdr.job-no2
            AND fg-bin.loc     EQ fg-rctd.loc
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin
            AND fg-bin.tag     EQ fg-rctd.tag
          NO-LOCK NO-ERROR.

      IF AVAIL fg-bin THEN fg-rctd.cases-unit = fg-bin.cases-unit.

      RUN fg/comprcpt.p (ROWID(fg-rctd)).
