/* -------------------------------------------------- cec/pr4-spe.i 07/96 JLF */

   /* special */
   do i = 1 to 8 with frame ac4  down no-labels no-box:
      if xef.spec-no[i] ne "" then do:
         find first item {sys/look/itemW.i} and item.i-no = xef.spec-no[i]
         no-lock no-error.
         if not available item then next.
         find first e-item of item no-lock no-error.

         /*s-qty[i] = xef.spec-qty[i] * {1}.*/
         RUN custom/extradec.p (.0001, (xef.spec-qty[i] * {1}),
                                OUTPUT s-qty[i]).

         FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

         b-uom = IF AVAIL e-item AND e-item.std-uom NE "" THEN e-item.std-uom
                                                          ELSE item.cons-uom.

         IF b-uom NE "EA" THEN
           RUN sys/ref/convquom.p("EA", b-uom,
                                  item.basis-w, xeb.t-len, xeb.t-wid, xeb.t-dep,
                                  s-qty[i], OUTPUT s-qty[i]).

         {est/matcost.i s-qty[i] s-cost[i] spe}

         s-cost[i] = (s-cost[i] * s-qty[i]) + lv-setup-spe.

         find first brd where brd.form-no = xef.form-no and
                              brd.i-no    = xef.spec-no[i]
                              no-error.
         if not available brd then
         do:
            create brd.
            assign brd.form-no = xef.form-no
                   brd.blank-no = int(xest.est-type eq 5)
                   brd.i-no    = xef.spec-no[i]
                   brd.dscr    = xef.spec-dscr[i]
                   brd.basis-w = item.basis-w.
         end.
         brd.qty = s-qty[i].
         if available e-item then
            ASSIGN
               brd.qty-uom = e-item.std-uom
               brd.sc-uom  = e-item.std-uom.
         else
            ASSIGN
               brd.qty-uom = item.cons-uom
               brd.sc-uom  = item.cons-uom.
         
         ASSIGN
         brd.cost = s-cost[i] / s-qty[i]
         brd.cost-m = s-cost[i] / ({1} / 1000)
         dm-tot[5] = dm-tot[5] + s-cost[i].

/* end ---------------------------------- copr. 1996  advanced software, inc. */
