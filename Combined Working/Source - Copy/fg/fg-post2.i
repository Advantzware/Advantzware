        
        x = 0.
        FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
        IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT x THEN x = b-fg-rctd.r-no.

        IF "{1}" EQ "" THEN DO:
          FIND LAST {1}w-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
          IF AVAIL {1}w-fg-rctd AND {1}w-fg-rctd.r-no GT x THEN x = {1}w-fg-rctd.r-no.
        END.

        FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAIL fg-rcpth AND fg-rcpth.r-no GT x THEN x = fg-rcpth.r-no + 1.

        IF "{1}" EQ "b-" THEN DO:
          CREATE fg-rcpts.
          ASSIGN
           fg-rcpts.r-no       = x + 1
           fg-rcpts.company    = cocode
           fg-rcpts.i-no       = b-itemfg.i-no
           fg-rcpts.i-name     = b-itemfg.i-name
           fg-rcpts.trans-date = fg-rctd.rct-date
           fg-rcpts.linker     = "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")
           fg-rcpts.rita-code  = "set".

          RELEASE fg-rcpts.
        END.

        CREATE {1}w-fg-rctd.
        BUFFER-COPY fg-rctd TO {1}w-fg-rctd
        ASSIGN
         {1}w-fg-rctd.r-no         = x + 1 
         {1}w-fg-rctd.i-no         = b-itemfg.i-no
         {1}w-fg-rctd.i-name       = b-itemfg.i-name
         {1}w-fg-rctd.job-no       = fg-bin.job-no
         {1}w-fg-rctd.job-no2      = fg-bin.job-no2
         {1}w-fg-rctd.pur-uom      = fg-bin.pur-uom
         {1}w-fg-rctd.loc          = fg-bin.loc
         {1}w-fg-rctd.loc-bin      = fg-bin.loc-bin
         {1}w-fg-rctd.tag          = fg-bin.tag  
         {1}w-fg-rctd.t-qty        = MIN(v-set-qty,(IF {2} EQ 0 THEN 999999999 ELSE {2})) * -1
         {1}w-fg-rctd.cost-uom     = {1}w-fg-rctd.pur-uom
         {1}w-fg-rctd.std-cost     = fg-bin.std-tot-cost
         {1}w-fg-rctd.ext-cost     = 0
         {1}w-fg-rctd.qty-case     = fg-bin.case-count
         {1}w-fg-rctd.units-pallet = fg-bin.units-pallet
         {1}w-fg-rctd.cases-unit   = fg-bin.cases-unit
         {1}w-fg-rctd.cases        = TRUNC({1}w-fg-rctd.t-qty / {1}w-fg-rctd.qty-case,0)
         {1}w-fg-rctd.partial      = {1}w-fg-rctd.t-qty -
                                     ({1}w-fg-rctd.cases * {1}w-fg-rctd.qty-case)
         {1}w-fg-rctd.SetHeaderRno = fg-rctd.r-no
        .
        /* Instead of 0 at 500 with -300 partial, make it */
        /* 1 at -500 with 0 partial 12101418 */
        IF {1}w-fg-rctd.cases EQ 0 AND {1}w-fg-rctd.partial NE 0 THEN
          ASSIGN 
              {1}w-fg-rctd.cases = (IF  {1}w-fg-rctd.partial LT 0 THEN -1 ELSE 1)
              {1}w-fg-rctd.qty-case = (IF {1}w-fg-rctd.partial LT 0 THEN - {1}w-fg-rctd.partial ELSE {1}w-fg-rctd.partial)
              {1}w-fg-rctd.partial = 0
              .
        IF {1}w-fg-rctd.cost-uom EQ "EA" THEN
          v-cost = {1}w-fg-rctd.std-cost.
        ELSE
          RUN sys/ref/convcuom.p({1}w-fg-rctd.cost-uom, "EA", 0, 0, 0, 0,
                                 {1}w-fg-rctd.std-cost, OUTPUT v-cost).
                                 
        ASSIGN
         {1}w-fg-rctd.ext-cost = {1}w-fg-rctd.t-qty * v-cost
         fg-rctd.ext-cost      = fg-rctd.ext-cost + ({1}w-fg-rctd.ext-cost * -1).
         
        RELEASE {1}w-fg-rctd.

         v-set-qty = v-set-qty - MIN(v-set-qty,{2}).
         
        IF v-set-qty LE 0 THEN LEAVE.
        
