        
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
         {1}w-fg-rctd.job-no       = bf-fg-rctd.job-no
         {1}w-fg-rctd.job-no2      = bf-fg-rctd.job-no2
         {1}w-fg-rctd.pur-uom      = bf-fg-rctd.pur-uom
         {1}w-fg-rctd.loc          = bf-fg-rctd.loc
         {1}w-fg-rctd.loc-bin      = bf-fg-rctd.loc-bin
         {1}w-fg-rctd.tag          = bf-fg-rctd.tag  
         {1}w-fg-rctd.t-qty        = MIN(v-set-qty,(IF bf-fg-rctd.qty EQ 0 THEN 999999999 ELSE bf-fg-rctd.qty)) * -1
         {1}w-fg-rctd.cost-uom     = {1}w-fg-rctd.pur-uom
         {1}w-fg-rctd.std-cost     = bf-fg-rctd.std-cost
         {1}w-fg-rctd.ext-cost     = 0
         {1}w-fg-rctd.qty-case     = bf-fg-rctd.qty-case
         {1}w-fg-rctd.units-pallet = bf-fg-rctd.units-pallet
         {1}w-fg-rctd.cases-unit   = bf-fg-rctd.cases-unit
         {1}w-fg-rctd.cases        = bf-fg-rctd.cases
         {1}w-fg-rctd.partial      = {1}w-fg-rctd.t-qty -
                                     ({1}w-fg-rctd.cases * {1}w-fg-rctd.qty-case)
        {1}w-fg-rctd.setHeaderRno  = bf-fg-rctd.r-no
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

         v-set-qty = v-set-qty - MIN(v-set-qty,bf-fg-rctd.qty).
         
        IF v-set-qty LE 0 THEN LEAVE.
        
