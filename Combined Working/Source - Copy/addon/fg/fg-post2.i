        
        find first w-fg-rcpts
            where w-fg-rcpts.company eq cocode
              and w-fg-rcpts.i-no    eq b-itemfg.i-no   
              and w-fg-rcpts.job-no  eq fg-bin.job-no
              and w-fg-rcpts.job-no2 eq fg-bin.job-no2
            no-lock no-error.
            
        if not avail w-fg-rcpts then do:
          x = 1.
          find last b-fg-rcpts use-index r-no no-lock no-error.
          if avail b-fg-rcpts then x = b-fg-rcpts.r-no + 1.
          for each w-fg-rcpts by w-fg-rcpts.r-no desc:
            leave.
          end.
          if avail w-fg-rcpts and w-fg-rcpts.r-no ge x then
            x = w-fg-rcpts.r-no + 1.
          find last fg-rcpth use-index r-no no-lock no-error.
          if avail fg-rcpth and fg-rcpth.r-no ge x then
            x = fg-rcpth.r-no + 1.

          create w-fg-rcpts.
          buffer-copy fg-rcpts to w-fg-rcpts
          assign
           w-fg-rcpts.r-no    = x
           w-fg-rcpts.i-no    = b-itemfg.i-no
           w-fg-rcpts.i-name  = b-itemfg.i-name
           w-fg-rcpts.job-no  = fg-bin.job-no
           w-fg-rcpts.job-no2 = fg-bin.job-no2
           w-fg-rcpts.pur-uom = fg-bin.pur-uom.
        end.
        
        create w-fg-rdtl.
        buffer-copy fg-rdtl to w-fg-rdtl
        assign
         w-fg-rdtl.r-no      = w-fg-rcpts.r-no
         w-fg-rdtl.loc       = fg-bin.loc
         w-fg-rdtl.loc-bin   = fg-bin.loc-bin
         w-fg-rdtl.tag       = fg-bin.tag  
         w-fg-rdtl.t-qty     = min(v-set-qty,fg-bin.qty) * -1
         w-fg-rdtl.rita-code = w-fg-rcpts.rita-code
         w-fg-rdtl.uom       = w-fg-rcpts.pur-uom
         w-fg-rdtl.std-cost  = fg-bin.std-tot-cost
         w-fg-rdtl.ext-cost  = 0.
        
        if w-fg-rdtl.uom eq "EA" then
          v-cost = w-fg-rdtl.std-cost.
        else
          run sys/ref/convcuom.p(w-fg-rdtl.uom, "EA", 0, 0, 0, 0,
                                 w-fg-rdtl.std-cost, output v-cost).
                                 
        assign
         w-fg-rdtl.ext-cost = w-fg-rdtl.t-qty * v-cost
         fg-rdtl.ext-cost   = fg-rdtl.ext-cost + (w-fg-rdtl.ext-cost * -1)
         
         v-set-qty = v-set-qty - min(v-set-qty,fg-bin.qty).
         
        if v-set-qty le 0 then leave.
        
