  
  for each w-fg-rctd

      break by w-fg-rctd.loc
            by w-fg-rctd.i-no
            by w-fg-rctd.loc-bin
            by w-fg-rctd.tag:
     v-time = STRING(w-fg-rctd.trans-time, "HH:MM").
            
    if first-of(w-fg-rctd.loc) then do:
      v-whse = w-fg-rctd.loc.
    
      if first(w-fg-rctd.loc) then do:
        
        
        

              end.
      
      ELSE DO:
        
      END.
    end.
    
    ASSIGN v-cstprt = "".

    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq w-fg-rctd.i-no
        no-lock no-error.

    release prod.
    /* gdm - 06150904 */
    IF AVAIL itemfg THEN ASSIGN v-cstprt = itemfg.part-no.

    if avail itemfg then
    find first prodl
        where prodl.company eq cocode
           and prodl.procat  eq itemfg.procat
           and can-find(first prod
                        where prod.company eq cocode
                          and prod.prolin  eq prodl.prolin)
        no-lock no-error.

    if avail prodl then
    find first prod
        where prod.company eq cocode
          and prod.prolin  eq prodl.prolin
        no-lock no-error.

    assign
     /*v-fg-qty   = w-fg-rctd.t-qty*/
     /*v-fg-cost  = w-fg-rctd.ext-cost*/
     v-fg-value = 0
     v-msf[1]   = 0
     v-msf[2]   = 0.

    release job-mat.
    
    if w-fg-rctd.rita-code eq "R" then do:
      find first job-hdr
          where job-hdr.company eq cocode
            and job-hdr.job-no  eq w-fg-rctd.job-no
            and job-hdr.job-no2 eq w-fg-rctd.job-no2
            and job-hdr.i-no    eq w-fg-rctd.i-no
          use-index job-no no-lock no-error.

      ll-wip = NO.

      if avail job-hdr then do:
        /* For calculating the quantity per pallet. */
        find first fg-bin
            where fg-bin.company eq w-fg-rctd.company
              and fg-bin.job-no  eq job-hdr.job-no
              and fg-bin.job-no2 eq job-hdr.job-no2
              and fg-bin.i-no    eq job-hdr.i-no
              and fg-bin.loc-bin eq w-fg-rctd.loc-bin
              and fg-bin.tag     eq w-fg-rctd.tag
            no-lock no-error.

        v-qty-pallet = w-fg-rctd.cases *
                       if avail fg-bin then fg-bin.cases-unit else 1.

        find first job
            where job.company eq cocode
              and job.job     eq job-hdr.job
              and job.job-no  eq job-hdr.job-no
              and job.job-no2 eq job-hdr.job-no2
            no-lock no-error.

        IF AVAIL job AND INT(w-fg-rctd.po-no) EQ 0 AND
           v-fgpostgl EQ "AllItems" AND AVAIL prod THEN DO:
          ASSIGN
           wip-amt = w-fg-rctd.t-qty / 1000 * job-hdr.std-mat-cost
           wip-lab = w-fg-rctd.t-qty / 1000 * job-hdr.std-lab-cost
           wip-foh = w-fg-rctd.t-qty / 1000 * job-hdr.std-fix-cost
           wip-voh = w-fg-rctd.t-qty / 1000 * job-hdr.std-var-cost.

          IF wip-amt NE ? AND wip-lab NE ? AND wip-foh NE ? AND wip-voh NE ? THEN DO:
            {jc/jcglcrt.i prod.fg-mat 0 wip-amt}    /* Finished Goods Material */
            {jc/jcglcrt.i prod.fg-lab 0 wip-lab}    /* Finished Goods Direct Labor */
            {jc/jcglcrt.i prod.fg-fo  0 wip-foh}    /* Finished Goods Fixed Ovrhd */
            {jc/jcglcrt.i prod.fg-vo  0 wip-voh}    /* Finished Goods Variable O/H */
            {jc/jcglcrt.i prod.wip-mat wip-amt 0}   /* Work in Process Material */
            {jc/jcglcrt.i prod.wip-lab wip-lab 0}   /* WIP Direct Labor */
            {jc/jcglcrt.i prod.wip-fo  wip-foh 0}   /* WIP Fixed Overhead */
            {jc/jcglcrt.i prod.wip-vo  wip-voh 0}   /* WIP Variable Overhead */
            ll-wip = YES.
          END.
        END.
      END.
      
      if w-fg-rctd.ext-cost ne 0 AND NOT ll-wip then do:
        release po-ord.
        release po-ordl.
        if int(w-fg-rctd.po-no) ne 0 then
        find first po-ord
            where po-ord.company eq cocode
              and po-ord.po-no   eq int(w-fg-rctd.po-no)
            no-lock no-error.
        if avail po-ord then
        find first po-ordl
            where po-ordl.company   eq cocode
              and po-ordl.po-no     eq po-ord.po-no
              and po-ordl.i-no      eq w-fg-rctd.i-no
              and po-ordl.deleted   eq no
              and po-ordl.item-type eq no
            no-lock no-error.
        IF AVAIL itemfg AND v-fgpostgl NE "None"       AND
           (AVAIL po-ordl OR v-fgpostgl EQ "AllItems") THEN DO:
          if w-fg-rctd.ext-cost ne 0  and
             w-fg-rctd.ext-cost ne ?  and
             avail prod         and  
             prod.fg-mat ne ""  and
             prod.wip-mat ne "" then do:                          
            /* Debit FG Material */
            find first work-gl where work-gl.actnum eq prod.fg-mat no-lock no-error.      
            if not avail work-gl then do:
              create work-gl.
              work-gl.actnum = prod.fg-mat.
            end.
            work-gl.debits = work-gl.debits + w-fg-rctd.ext-cost.             
            /* Credit WIP Material */
            find first work-gl where work-gl.actnum eq prod.wip-mat no-lock no-error.      
            if not avail work-gl then do:
              create work-gl.
              work-gl.actnum = prod.wip-mat.
            end.
            work-gl.credits = work-gl.credits + w-fg-rctd.ext-cost.
          end.  /* if w-fg-rctd.ext-cost */
        end.  
      end.

      IF AVAIL job-hdr THEN DO:
        if last-of(w-fg-rctd.i-no) then
        for each mch-act FIELDS(waste)
            where mch-act.company  eq cocode
              and mch-act.job      eq job-hdr.job
              and mch-act.job-no   eq job-hdr.job-no
              and mch-act.job-no2  eq job-hdr.job-no2
              and mch-act.frm      eq job-hdr.frm
            use-index job no-lock:
          v-msf[2] = v-msf[2] + mch-act.waste.
        end.

        for each job-mat
            where job-mat.company eq cocode
              and job-mat.job     eq job-hdr.job
              and job-mat.job-no  eq job-hdr.job-no
              and job-mat.job-no2 eq job-hdr.job-no2
              and job-mat.frm     eq job-hdr.frm
            no-lock,
            first item
            where item.company    eq cocode
              and item.i-no       eq job-mat.i-no
              and lookup(item.mat-type,"B,1,2,3,4") gt 0
            no-lock:
          leave.
        end.

        if avail job-mat then do:
          assign 
           v-msf[1] = w-fg-rctd.t-qty *
                      (job-mat.len * job-mat.wid * (job-hdr.sq-in / 100))
           v-msf[2] = v-msf[2]        *
                      (job-mat.len * job-mat.wid * (job-hdr.sq-in / 100)).

          if v-corr then
            assign
             v-msf[1] = v-msf[1] * .007
             v-msf[2] = v-msf[2] * .007.
          else
            assign
             v-msf[1] = v-msf[1] / 144
             v-msf[2] = v-msf[2] / 144.
        end.
      end.
    end.
    
    else
    if w-fg-rctd.rita-code eq "A" and v-adjustgl then do:
      find first fg-bin
          where fg-bin.company eq cocode
            and fg-bin.i-no    eq w-fg-rctd.i-no
            and fg-bin.job-no  eq w-fg-rctd.job-no
            and fg-bin.job-no2 eq w-fg-rctd.job-no2
            and fg-bin.loc     eq w-fg-rctd.loc
            and fg-bin.loc-bin eq w-fg-rctd.loc-bin
            and fg-bin.tag     eq w-fg-rctd.tag
          no-lock no-error.
        
      if avail fg-bin then
        run oe/invposty.p (0, fg-bin.i-no, w-fg-rctd.t-qty * -1, fg-bin.pur-uom,
                           fg-bin.std-lab-cost, fg-bin.std-fix-cost,
                           fg-bin.std-var-cost, fg-bin.std-mat-cost).
    end.

    /*procedure code was originally here*/

    if index("RTASEI",w-fg-rctd.rita-code) ne 0 then
      v-tran-type = entry(index("RTASEI",w-fg-rctd.rita-code),v-entrytype).
    else v-tran-type = "".

    if w-fg-rctd.po-no ne " " then
      find po-ord 
         where po-ord.company EQ w-fg-rctd.company
           AND po-ord.po-no = int(w-fg-rctd.po-no)            
      NO-LOCK.
    else
      release po-ord.
      
    /*djk: get the total quantity (not including the partial)*/
    assign
     v-fg-qty  = w-fg-rctd.cases * w-fg-rctd.qty-case
     v-fg-cost = w-fg-rctd.ext-cost / w-fg-rctd.t-qty * v-fg-qty.

    /*if w-fg-rctd.pur-uom ne "EA" then
        v-fg-cost = (v-fg-qty / 1000) * w-fg-rctd.std-cost.
    else
        v-fg-cost = v-fg-qty * w-fg-rctd.std-cost.*/
    run calc-total.

    IF rd-Itm#Cst# = 1
      THEN ASSIGN v-cstprt = w-fg-rctd.i-no.


    IF rd-ItmPo EQ 1
      THEN DO:        

    END.
    ELSE DO:

    END.




    IF rd-ItmPo EQ 1
      THEN DO:    
        

    END.
    ELSE DO:


    END.

    /*djk: get the total quantity for the partial*/
    assign
     v-fg-qty  = w-fg-rctd.partial
     v-fg-cost = w-fg-rctd.ext-cost / w-fg-rctd.t-qty * v-fg-qty.

    /*if w-fg-rctd.cost-uom ne "EA" then
        v-fg-cost = (v-fg-qty / 1000) * w-fg-rctd.std-cost.
    else
        v-fg-cost = v-fg-qty * w-fg-rctd.std-cost.*/
    run calc-partial.

    if v-fg-qty <> 0 then do:

      IF rd-ItmPo EQ 1
        THEN DO:
          

      END.
      ELSE DO:
      

      END.

 

      IF rd-ItmPo EQ 1 
        THEN DO:

      END.
      ELSE DO:
      

      END.
    end.  

   assign
     v-fg-qty  = w-fg-rctd.t-qty
     v-fg-cost = w-fg-rctd.ext-cost.

    /*if w-fg-rctd.cost-uom ne "EA" then
        v-fg-cost = (v-fg-qty / 1000) * w-fg-rctd.std-cost.
    else
        v-fg-cost = v-fg-qty * w-fg-rctd.std-cost.*/
    run orig. 

    IF w-fg-rctd.rita-code eq "T" THEN DO:
     END.

    if v-pr-tots OR v-pr-tots2  then do:

         v-tot-qty = v-tot-qty + v-fg-qty.
         v-grd-tot-qty = v-grd-tot-qty + v-fg-qty.
         v-tot-cost = v-tot-cost + v-fg-cost.
         v-grd-tot-cost  = v-grd-tot-cost  + v-tot-cost.         
         v-tot-value = v-tot-value + round(v-fg-value,2).
         
         v-grd-tot-value = v-grd-tot-value + v-fg-value.   
         
         v-msf[3] = v-msf[3] + v-msf[1].
         v-msf[4] = v-msf[4] + v-msf[2].

      if w-fg-rctd.rita-code eq "R" or
         w-fg-rctd.rita-code eq "A" or
         w-fg-rctd.rita-code eq "E" then
        v-cum-tot  = v-cum-tot + v-fg-cost.

      else
      if w-fg-rctd.rita-code eq "S" then v-cum-tot  = v-cum-tot - v-fg-cost.
    end.

    if last-of(w-fg-rctd.i-no) then do:
      if v-pr-tots2 then do:                      

      end.
      
      assign
       v-msf[5]    = v-msf[5] + v-msf[3]
       v-msf[6]    = v-msf[6] + v-msf[4]
       v-tot-qty   = 0
       v-tot-cost  = 0
       v-tot-value = 0
       v-msf[3]    = 0
       v-msf[4]    = 0.
    end.  /* if last-of(w-fg-rctd.i-no) */        
  end. /*for each*/
  
