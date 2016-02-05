/* fg/fgpstwip.i*/
     
     create fg-rctd.
      assign
       fg-rctd.r-no       = X + 1
       fg-rctd.rct-date   = pc-prdd.op-date
       fg-rctd.trans-time = pc-prdd.op-time
       fg-rctd.company    = cocode
       fg-rctd.rita-code  = "R"
       fg-rctd.i-name     = {1}.i-name  /* itemfg*/
       fg-rctd.i-no       = {1}.i-no   /*tt-job-hdr*/
       fg-rctd.job-no     = pc-prdd.job-no
       fg-rctd.job-no2    = pc-prdd.job-no2.
                 
      assign
       v-up  = 1
       v-out = 1.
                 
      if avail est and index("APB",mach.p-type) le 0 then do:
        run sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, output v-up).                 
        find first est-op
            where est-op.company eq est.company
              and est-op.est-no  eq est.est-no
              and est-op.s-num   eq pc-prdd.frm
              and (est-op.b-num  eq pc-prdd.blank-no or
                   pc-prdd.blank-no eq 0)
              and est-op.m-code  eq pc-prdd.m-code
              and est-op.op-pass eq pc-prdd.pass
              and est-op.dept    eq pc-prdd.dept
              and est-op.line    lt 500
            no-lock no-error.
        if avail est-op and est-op.n-out ne 0 then v-out = est-op.n-out.
      end.
                 
      ASSIGN
       fg-rctd.b-num      = pc-prdd.blank-no
       fg-rctd.s-num      = pc-prdd.frm
       fg-rctd.t-qty      = pc-prdd.qty / v-up-hs * v-out * v-up
       fg-rctd.pur-uom    = {1}.prod-uom
       fg-rctd.cost-uom   = {1}.prod-uom
       fg-rctd.std-cost   = tt-job-hdr.std-tot-cost
       fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
       fg-rctd.qty-case   = {1}.case-count
       fg-rctd.partial    = fg-rctd.t-qty modulo {1}.case-count
       fg-rctd.cases      = trunc(fg-rctd.t-qty / {1}.case-count,0)
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
         fg-rctd.tag = fg-bin.tag
         .
                
      else
      if v-auto-bin eq "ShipTo" then do:
        /*get estimate blank file from finished goods item file*/
        find first eb
            where eb.company  eq cocode
              and eb.est-no   eq {1}.est-no
              and eb.stock-no eq {1}.i-no
            use-index est-no no-lock no-error.
        if avail eb then do:
          /*get customer file from estimate blank file*/
          find first cust
              where cust.company eq cocode
                and cust.cust-no eq eb.cust-no
              no-lock no-error.
          if avail cust then do:              
            find first shipto
                where shipto.company = cocode
                  and shipto.cust-no = cust.cust-no 
                no-lock no-error.
            if avail shipto then do:
              find first fg-bin
                  where fg-bin.company eq cocode
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
                  where fg-bin.company eq cocode
                    and fg-bin.loc     eq {1}.def-loc
                    and fg-bin.loc-bin eq {1}.def-loc-bin
                    and fg-bin.i-no    eq ""
                  no-lock no-error.
              if avail fg-bin then 
                assign 
                 v-loc     = {1}.def-loc
                 v-loc-bin = {1}.def-loc-bin.
            end. /*if avail shipto*/
          end. /*if avail cust*/
        end. /*if avail eb*/
      end. /*if system default is shipto*/
      /*else if "FGFILE" then get from finished goods file*/
      else do:
        find first fg-bin
            where fg-bin.company eq cocode
              and fg-bin.loc     eq {1}.def-loc
              and fg-bin.loc-bin eq {1}.def-loc-bin
              and fg-bin.i-no    eq ""
            no-lock no-error.
        if avail fg-bin then 
          ASSIGN
           v-loc     = {1}.def-loc
           v-loc-bin = {1}.def-loc-bin.
      end. /*else FGFILE*/
          
      /*if bin and warehouse are blank, goto cust "X" shipto file*/
      if v-loc eq "" and v-loc-bin eq "" then do:
        find first cust
            where cust.company eq cocode
              and cust.active  eq "X"
            no-lock no-error.
                                
        if avail cust then do:
          find first shipto
              where shipto.company eq cocode
                and shipto.cust-no eq cust.cust-no  
              no-lock no-error.
          if avail shipto then do:
            find first fg-bin
                where fg-bin.company eq cocode
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
            AND fg-bin.job-no  EQ pc-prdd.job-no
            AND fg-bin.job-no2 EQ pc-prdd.job-no2
            AND fg-bin.loc     EQ fg-rctd.loc
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin
            AND fg-bin.tag     EQ fg-rctd.tag
          NO-LOCK NO-ERROR.

      IF AVAIL fg-bin THEN fg-rctd.cases-unit = fg-bin.cases-unit.
      RUN fg/comprcpt.p (ROWID(fg-rctd)).
