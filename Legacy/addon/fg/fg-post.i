/* fg/fg-post.i   in GUI  copied and changed */ 

  v-newhdr = no. 
  release po-ord.
  if {1}.rita-code ne "E" then
      find first po-ord   where po-ord.company eq cocode
                            and po-ord.po-no   eq int({1}.po-no)
          no-error.
          
 /*     for each {2} where {2}.r-no eq {1}.r-no:  */
  release po-ordl.
  if avail po-ord then
        find first po-ordl where po-ordl.company   eq cocode
                             and po-ordl.po-no     eq po-ord.po-no
                             and po-ordl.i-no      eq {1}.i-no
                             and po-ordl.deleted   eq no
                             and po-ordl.item-type eq no
                             AND po-ordl.stat NE "C"
             no-error.
        IF NOT AVAIL po-ordl THEN
        find first po-ordl  where po-ordl.company   eq cocode
                              and po-ordl.po-no     eq po-ord.po-no
                              and po-ordl.i-no      eq {1}.i-no
                              and po-ordl.deleted   eq no
                              and po-ordl.item-type eq no
                        no-error.
                                /** Adjusting the Finish Good item quantitys **/
  if {1}.rita-code eq "R" then do:           /** RECEIPTS **/
         v-reduce-qty = 0.
         if avail po-ordl then do:
            if po-ordl.cons-uom eq "EA" then v-reduce-qty = po-ordl.cons-qty.
            else  if po-ordl.cons-qty ne 0 then
              run sys/ref/convquom.p(input po-ordl.cons-uom,
                                     input "EA", input 0,
                                     input po-ordl.s-len,
                                     input po-ordl.s-wid,
                                     input 0,
                                     input po-ordl.cons-qty,
                                     output v-reduce-qty).

            assign v-overrun-qty     = v-reduce-qty * (1 - (po-ordl.over-pct / 100))
                   v-underrun-qty    = v-reduce-qty * (1 - (po-ordl.under-pct / 100))
                   po-ordl.t-rec-qty = po-ordl.t-rec-qty + {1}.t-qty.

            if po-ordl.t-rec-qty ge v-underrun-qty then do:
               assign  v-reduce-qty = min(v-reduce-qty,v-reduce-qty - po-ordl.t-rec-qty + {1}.t-qty)
                       po-ordl.stat = "C".       
               find first b-po-ordl  where b-po-ordl.company eq po-ord.company
                                       and b-po-ordl.po-no   eq po-ord.po-no
                                        and b-po-ordl.stat    ne "C"
                                        and b-po-ordl.deleted eq no
                                        and recid(b-po-ordl)  ne recid(po-ordl)
                              no-lock no-error.
               if not avail b-po-ordl then po-ord.stat = "C".
            end.
            else po-ordl.stat = "P".
            if v-reduce-qty lt 0 then v-reduce-qty = 0.
            if po-ordl.stat ne "C" then  v-reduce-qty = min({1}.t-qty,v-reduce-qty).
            if {2}.ext-cost ne 0 then do:
               release prod.
               find first prodl where prodl.company eq cocode
                                and prodl.procat  eq itemfg.procat
                      no-lock no-error.
              if avail prodl then
                 find first prod  where prod.company eq cocode
                                    and prod.prolin  eq prodl.prolin
                          no-lock no-error.
              if avail prod            and  prod.fg-mat ne ""     and
                 prod.wip-mat ne ""    and {2}.ext-cost ne ? 
              then do:                          
                 /* Debit FG Material */
                 find first work-gl where work-gl.actnum eq prod.fg-mat
                    no-lock no-error.      
                 if not avail work-gl then do:
                    create work-gl.
                    work-gl.actnum = prod.fg-mat.
                 end.
                 work-gl.debits = work-gl.debits + {2}.ext-cost.             
                 /* Credit WIP Material */
                 find first work-gl where work-gl.actnum eq prod.wip-mat
                      no-lock no-error.      
                 if not avail work-gl then do:
                    create work-gl.
                    work-gl.actnum = prod.wip-mat.
                 end.
                 work-gl.credits = work-gl.credits + {2}.ext-cost.
              end.
            end.  /* if {2}.ext-cost */ 
          end.   /* avail po-ordl */

          else  if {1}.job-no ne "" then do:
            for each job-hdr
                where job-hdr.company eq cocode
                  and job-hdr.job-no  eq {1}.job-no
                  and job-hdr.job-no2 eq {1}.job-no2
                  and job-hdr.i-no    eq {1}.i-no
                  and can-find(first job where job.company eq cocode
                                           and job.job     eq job-hdr.job
                                           and job.job-no  eq job-hdr.job-no
                                           and job.job-no2 eq job-hdr.job-no2
                                           and job.opened
                  no-lock:
                accumulate job-hdr.qty (total).
                v-est-no = job-hdr.est-no.
            end.
            
            if (accum total job-hdr.qty) gt 0 then do:
              for each fg-act
                  where fg-act.company eq cocode
                    and fg-act.job-no  eq {1}.job-no
                    and fg-act.job-no2 eq {1}.job-no2
                    and fg-act.i-no    eq {1}.i-no
                  no-lock:
                accumulate fg-act.qty (total).
              end.
            
              v-reduce-qty = (accum total fg-act.qty) + {2}.t-qty.
            
              if v-reduce-qty gt (accum total job-hdr.qty) then
                v-reduce-qty = {2}.t-qty -
                               (v-reduce-qty - (accum total job-hdr.qty)).
              else             
                v-reduce-qty = v-reduce-qty - (accum total fg-act.qty).
            end.    
          end.   /* else */

          RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).

          FIND FIRST itemfg-loc 
              WHERE itemfg-loc.company EQ itemfg.company
                AND itemfg-loc.i-no    EQ itemfg.i-no
                AND itemfg-loc.loc     EQ {2}.loc
              EXCLUSIVE-LOCK NO-ERROR.

          assign
           itemfg.q-prod     = itemfg.q-prod     + {2}.t-qty
           itemfg.q-prod-ptd = itemfg.q-prod-ptd + {2}.t-qty
           itemfg.q-prod-ytd = itemfg.q-prod-ytd + {2}.t-qty
           itemfg.q-onh      = itemfg.q-onh      + {2}.t-qty
           itemfg.q-ono      = itemfg.q-ono      - v-reduce-qty.
          
          if itemfg.def-loc eq "" and itemfg.def-loc-bin eq "" then
            assign itemfg.def-loc     = {2}.loc
                   itemfg.def-loc-bin = {2}.loc-bin.          
          if itemfg.q-ono lt 0 then DO: 
              itemfg.q-ono = 0.             
              IF AVAIL(itemfg-loc) THEN
                itemfg-loc.q-ono = 0.
          END.

          run fg/comp-upd.p (recid(itemfg), v-reduce-qty * -1, "q-ono",v-est-no).
          run sys/ref/convcuom.p({2}.pur-uom, itemfg.prod-uom, 0, 0, 0, 0,
                                 itemfg.last-cost, output itemfg.last-cost).

         IF AVAIL(itemfg-loc) THEN
          assign
           itemfg-loc.q-prod     = itemfg-loc.q-prod     + {2}.t-qty
           itemfg-loc.q-prod-ptd = itemfg-loc.q-prod-ptd + {2}.t-qty
           itemfg-loc.q-prod-ytd = itemfg-loc.q-prod-ytd + {2}.t-qty
           itemfg-loc.q-onh      = itemfg-loc.q-onh      + {2}.t-qty
           itemfg-loc.q-ono      = itemfg-loc.q-ono      - v-reduce-qty.
                  
          

  end.  /* rita-code = "r" */
  else if {1}.rita-code eq "S" then DO:      /** SHIPPMENTS **/
  
       assign
           itemfg.q-ytd      = itemfg.q-ytd - {2}.t-qty
           itemfg.q-ship     = itemfg.q-ship + {2}.t-qty
           itemfg.q-ship-ptd = itemfg.q-ship-ptd + {2}.t-qty
           itemfg.q-ship-ytd = itemfg.q-ship-ytd + {2}.t-qty
           itemfg.q-onh      = itemfg.q-onh - {2}.t-qty.
       RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).

       FIND FIRST itemfg-loc 
           WHERE itemfg-loc.company EQ itemfg.company
             AND itemfg-loc.i-no    EQ itemfg.i-no
             AND itemfg-loc.loc     EQ {2}.loc
           EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL(itemfg-loc) THEN
       ASSIGN
           itemfg-loc.q-ytd      = itemfg-loc.q-ytd - {2}.t-qty
           itemfg-loc.q-ship     = itemfg-loc.q-ship + {2}.t-qty
           itemfg-loc.q-ship-ptd = itemfg-loc.q-ship-ptd + {2}.t-qty
           itemfg-loc.q-ship-ytd = itemfg-loc.q-ship-ytd + {2}.t-qty
           itemfg-loc.q-onh      = itemfg-loc.q-onh - {2}.t-qty.   

  END.
  else if {1}.rita-code eq "T" then DO:        /** TRANSFERS **/

       assign
           itemfg.q-tran     = itemfg.q-tran + {2}.t-qty
           itemfg.q-tran-ptd = itemfg.q-tran-ptd + {2}.t-qty.
       RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).

       FIND FIRST itemfg-loc 
           WHERE itemfg-loc.company EQ itemfg.company
             AND itemfg-loc.i-no    EQ itemfg.i-no
             AND itemfg-loc.loc     EQ {2}.loc
           EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL(itemfg-loc) THEN
       ASSIGN
           itemfg-loc.q-tran     = itemfg-loc.q-tran + {2}.t-qty
           itemfg-loc.q-tran-ptd = itemfg-loc.q-tran-ptd + {2}.t-qty. 

  END.
  else if {1}.rita-code eq "A" then DO:       /** ADJUSTMENTS **/

       assign
           itemfg.q-adj     = itemfg.q-adj  + {2}.t-qty
           itemfg.q-adj-ytd = itemfg.q-adj-ytd  + {2}.t-qty
           itemfg.q-adj-ptd = itemfg.q-adj-ptd  + {2}.t-qty
           itemfg.q-onh     = itemfg.q-onh + {2}.t-qty.

       RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).
       FIND FIRST itemfg-loc 
           WHERE itemfg-loc.company EQ itemfg.company
             AND itemfg-loc.i-no    EQ itemfg.i-no
             AND itemfg-loc.loc     EQ {2}.loc
           EXCLUSIVE-LOCK NO-ERROR.
       ASSIGN
           itemfg-loc.q-adj     = itemfg-loc.q-adj  + {2}.t-qty
           itemfg-loc.q-adj-ytd = itemfg-loc.q-adj-ytd  + {2}.t-qty
           itemfg-loc.q-adj-ptd = itemfg-loc.q-adj-ptd  + {2}.t-qty
           itemfg-loc.q-onh     = itemfg-loc.q-onh + {2}.t-qty.   

  END.
  else if {1}.rita-code eq "E" then DO:      /** CREDIT RETURNS **/

       assign
           itemfg.q-ytd      = itemfg.q-ytd + {2}.t-qty
           itemfg.q-ship     = itemfg.q-ship - {2}.t-qty
           itemfg.q-ship-ptd = itemfg.q-ship-ptd - {2}.t-qty
           itemfg.q-ship-ytd = itemfg.q-ship-ytd - {2}.t-qty
           itemfg.q-onh      = itemfg.q-onh + {2}.t-qty.

       RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).
       FIND FIRST itemfg-loc 
           WHERE itemfg-loc.company EQ itemfg.company
             AND itemfg-loc.i-no    EQ itemfg.i-no
             AND itemfg-loc.loc     EQ {2}.loc
           EXCLUSIVE-LOCK NO-ERROR.

       ASSIGN
           itemfg-loc.q-ytd      = itemfg-loc.q-ytd + {2}.t-qty
           itemfg-loc.q-ship     = itemfg-loc.q-ship - {2}.t-qty
           itemfg-loc.q-ship-ptd = itemfg-loc.q-ship-ptd - {2}.t-qty
           itemfg-loc.q-ship-ytd = itemfg-loc.q-ship-ytd - {2}.t-qty
           itemfg-loc.q-onh      = itemfg-loc.q-onh + {2}.t-qty.

  END.           
                                         /** Re-Calculate Qty Available **/
  itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.
  if itemfg.q-ship-ptd lt 0 then itemfg.q-ship-ptd = 0.
  if itemfg.q-ship-ytd lt 0 then itemfg.q-ship-ytd = 0.
  find first job-hdr where job-hdr.company eq cocode
                          and job-hdr.job-no  eq {1}.job-no
                          and job-hdr.job-no2 eq {1}.job-no2
                          and job-hdr.i-no    eq {1}.i-no
                no-lock no-error.
  find first fg-bin where fg-bin.company eq cocode
                      and fg-bin.i-no    eq {1}.i-no
                      and fg-bin.job-no  eq {1}.job-no
                      and fg-bin.job-no2 eq {1}.job-no2
                      and fg-bin.loc     eq {2}.loc
                      and fg-bin.loc-bin eq {2}.loc-bin
                      and fg-bin.tag     eq {2}.tag
            no-error.

  if not avail fg-bin then
         if {1}.rita-code eq "R" then do:
            create fg-bin.
            assign
             fg-bin.company    = cocode
             fg-bin.i-no       = {1}.i-no
             fg-bin.job-no     = {1}.job-no
             fg-bin.job-no2    = {1}.job-no2
             fg-bin.loc        = {2}.loc
             fg-bin.loc-bin    = {2}.loc-bin
             fg-bin.tag        = {2}.tag
             fg-bin.case-count = {2}.qty-case.
         end.
         else return error.  /*undo {3}, next {3}. */
        
  /* For Transfers from & Shipments decrease the quantity in the BIN */
  if index("TS",{1}.rita-code) ne 0 then
          fg-bin.qty = fg-bin.qty - {2}.t-qty.

  /* For Receipts increase the quantity in the BIN */
  else if {1}.rita-code eq "R" then do:
          {fg/upd-bin.i "fg-bin" "{2}.pur-uom" "{2}.std-cost" {2}}
  end.
  else if {1}.rita-code eq "A" or {1}.rita-code eq "E" then
          fg-bin.qty = fg-bin.qty + {2}.t-qty.
  
  ASSIGN fg-bin.units-pallet =  {1}.units-pallet
         fg-bin.CASEs-unit = {1}.cases-unit.
         
  /* This code is to handle the Transfer quantity to increase the BIN */
  if {1}.rita-code eq "T" then do:
          find first b-fg-bin
              where b-fg-bin.company eq {2}.company
                and b-fg-bin.job-no  eq {1}.job-no
                and b-fg-bin.job-no2 eq {1}.job-no2
                and b-fg-bin.i-no    eq {1}.i-no
                and b-fg-bin.loc     eq {2}.loc2
                and b-fg-bin.loc-bin eq {2}.loc-bin2
                and b-fg-bin.tag     eq {2}.tag2
              no-error.

          IF NOT AVAIL b-fg-bin THEN DO:
            CREATE b-fg-bin.
            ASSIGN
             b-fg-bin.company    = {2}.company
             b-fg-bin.i-no       = {1}.i-no
             b-fg-bin.job-no     = {1}.job-no
             b-fg-bin.job-no2    = {1}.job-no2
             b-fg-bin.loc        = {2}.loc2
             b-fg-bin.loc-bin    = {2}.loc-bin2
             b-fg-bin.tag        = {2}.tag2
             b-fg-bin.case-count = {2}.qty-case.
          END.

          {fg/upd-bin.i "b-fg-bin" "fg-bin.pur-uom" "fg-bin.std-tot-cost" {2}}

       /* Move the inventory between the two itemfg-loc records */
       RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc).
       FIND FIRST itemfg-loc 
           WHERE itemfg-loc.company EQ itemfg.company
             AND itemfg-loc.i-no    EQ itemfg.i-no
             AND itemfg-loc.loc     EQ {2}.loc
           EXCLUSIVE-LOCK NO-ERROR.
       ASSIGN
         itemfg-loc.q-onh = itemfg-loc.q-onh - {2}.t-qty
         itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.

       RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT {2}.loc2).
       FIND FIRST itemfg-loc 
           WHERE itemfg-loc.company EQ itemfg.company
             AND itemfg-loc.i-no    EQ itemfg.i-no
             AND itemfg-loc.loc     EQ {2}.loc2
           EXCLUSIVE-LOCK NO-ERROR.
       ASSIGN
         itemfg-loc.q-onh = itemfg-loc.q-onh + {2}.t-qty
         itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.

  end. /* if rita-code eq "T" */
       
  
  create fg-rdtlh.
  {fg/fg-rdtl.i fg-rdtlh {2}} /* Create Detail History Records */
  {fg/fg-fgact.i {1} {2}}         /* Create Job Costing F/G WIP Record */
      
  create fg-rcpth.
  {fg/fg-rcpts.i fg-rcpth {1}}  /* Create Header History Records */

        run fg/updfgcs1.p (recid(itemfg), NO).
        find b-itemfg where recid(b-itemfg) eq recid(itemfg).
        for each oe-ordl where oe-ordl.company eq cocode
                          and oe-ordl.i-no    eq {1}.i-no
                          and oe-ordl.job-no  eq ""
                          and oe-ordl.cost    eq 0
                          use-index item,
            first oe-ord    where oe-ord.company           eq cocode
                            and oe-ord.ord-no            eq oe-ordl.ord-no
                            and index("CDZ",oe-ord.stat) eq 0 no-lock:

            run sys/ref/convcuom.p(b-itemfg.prod-uom, "M", 0, 0, 0, 0,
                                    b-itemfg.total-std-cost, output oe-ordl.cost).
        end.

