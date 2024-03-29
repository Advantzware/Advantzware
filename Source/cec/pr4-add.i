/* -------------------------------------------------- cec/pr4-add.i 07/96 JLF */

find first est-op
    where est-op.company eq xest.company
      and est-op.est-no  eq xest.est-no
      and est-op.s-num   eq xef.form-no
      and est-op.line    ge 500
    NO-LOCK NO-ERROR.

assign
 v-ssqft   = if v-corr then (xef.gsh-len * xef.gsh-wid) * .007
                       else (xef.gsh-len * xef.gsh-wid) / 144.

IF AVAIL est-op THEN
   brd-sf[4] = (v-ssqft * /*est-op.num-sh*/ xef.gsh-qty) / 1000.

do i = 1 to 6 with frame adders down no-labels no-box:
  assign
   a-cost  = 0
   v-setup = 0
   v-cost  = 0
   v-qty   = brd-sf[4].
   
  if xef.adder[i] ne "" then do:
    find first xitem
        where xitem.company eq cocode
          and xitem.i-no    eq xef.adder[i]
        no-lock no-error.
    if available xitem then do:
      find xe-item of xitem no-lock no-error.
      find first xe-item-vend of xe-item
          where xe-item-vend.item-type eq yes
            and xe-item-vend.vend-no   eq v-vend-no
          no-lock no-error.

      b-uom = xitem.cons-uom.
          
      if avail xe-item then do:
        IF lNewVendorItemCost THEN
        DO: 
            /* If Old vendor logic then apply the conversion. For new, conversion logic is in Vendor Cost proc */     
            RUN est/getVendorCostinQtyUOM.p(xitem.company, 
                xitem.i-no, 
                "RM", 
                v-vend-no,
                xeb.est-no,
                xeb.form-no,
                xeb.blank-no,
                xef.gsh-qty,
                "EA",
                xef.gsh-len, 
                xef.gsh-wid, 
                xef.gsh-dep,
                xitem.basis-w,
                OUTPUT v-cost,
                OUTPUT v-setup,
                OUTPUT a-cost).
                              
            RUN Conv_ValueFromUOMtoUOM (xitem.company, xitem.i-no, "RM",
                v-cost, "EA", "MSF",
                xitem.basis-w, xef.gsh-len, xef.gsh-wid, xef.gsh-dep, 0,
                OUTPUT v-cost, OUTPUT lError, OUTPUT cMessage).
            b-uom = "MSF". 
                
        END.
        ELSE
        DO:   
          IF xe-item.std-uom NE "" THEN b-uom = xe-item.std-uom.

          if b-uom ne "MSF" then
            run sys/ref/convquom.p("MSF", b-uom, xitem.basis-w,
                                   xef.gsh-len, xef.gsh-wid, xef.gsh-dep,
                                   brd-sf[4], output v-qty).

          EMPTY TEMP-TABLE tt-eiv.
          EMPTY TEMP-TABLE tt-ei.

          IF AVAIL xe-item-vend THEN
          DO:
             CREATE tt-eiv.
             DO j = 1 TO 10:
                ASSIGN
                   tt-eiv.run-qty[j] = xe-item-vend.run-qty[j]
                   tt-eiv.run-cost[j] = xe-item-vend.run-cost[j]
                   tt-eiv.setups[j] = xe-item-vend.setups[j].
             END.

             IF AVAIL xe-item-vend THEN
             DO:

                DO j = 1 TO 10:
                   ASSIGN
                      tt-eiv.run-qty[j + 10] = xe-item-vend.runQtyXtra[j]
                      tt-eiv.run-cost[j + 10] = xe-item-vend.runCostXtra[j]
                      tt-eiv.setups[j + 10] = xe-item-vend.setupsXtra[j].
                END.
             END.
          END.
          ELSE
          DO:
             CREATE tt-ei.
             DO j = 1 TO 10:
                ASSIGN
                   tt-ei.run-qty[j] = xe-item.run-qty[j]
                   tt-ei.run-cost[j] = xe-item.run-cost[j].
             END.

           
                DO j = 1 TO 10:
                   ASSIGN
                      tt-ei.run-qty[j + 10] = xe-item.runQty[j]
                      tt-ei.run-cost[j + 10] = xe-item.runCost[j].
                END.
          END.

          do j = 1 to 20:
            if avail xe-item-vend then do:
              if tt-eiv.run-qty[j] ge v-qty then do:
                assign
                v-cost  = tt-eiv.run-cost[j]
                v-setup = tt-eiv.setups[j].
                leave.
              end.
            end.
   
            else
            if tt-ei.run-qty[j] ge v-qty then do:
              v-cost = tt-ei.run-cost[j].
              leave.
            end.
          end.
        end.
      END.

      ELSE DO:
        /* If Old vendor logic then apply the conversion. For new, conversion logic is in Vendor Cost proc */
        IF lNewVendorItemCost THEN
        DO:      
            RUN est/getVendorCostinQtyUOM.p(xitem.company, 
                xitem.i-no, 
                "RM", 
                v-vend-no,
                xeb.est-no,
                xeb.form-no,
                xeb.blank-no,
                xef.gsh-qty,
                "EA",
                xef.gsh-len, 
                xef.gsh-wid, 
                xef.gsh-dep,
                xitem.basis-w,
                OUTPUT v-cost,
                OUTPUT v-setup,
                OUTPUT a-cost).
                            
            RUN Conv_ValueFromUOMtoUOM (xitem.company, xitem.i-no, "RM",
                v-cost, "EA", "MSF",
                xitem.basis-w, xef.gsh-len, xef.gsh-wid, xef.gsh-dep, 0,
                OUTPUT v-cost, OUTPUT lError, OUTPUT cMessage).
            b-uom = "MSF".                
        END.
        ELSE
        DO:         
          v-cost = if ce-ctrl.r-cost then xitem.avg-cost else xitem.last-cost.

          if b-uom ne "MSF" then
            run sys/ref/convquom.p("MSF", b-uom, xitem.basis-w,
                                   xef.gsh-len, xef.gsh-wid, xef.gsh-dep,
                                   brd-sf[4], output v-qty).
         END.
      END.
      IF NOT lNewVendorItemCost THEN
      a-cost = (v-cost * v-qty) + v-setup.

      find first brd
          where brd.form-no  eq xef.form-no
            and brd.blank-no eq int(xest.est-type eq 5)
            and brd.i-no     eq xef.adder[i]
          no-error.
      if not available brd then do:
        create brd.
        assign
         brd.form-no  = xef.form-no
         brd.blank-no = int(xest.est-type eq 5)
         brd.i-no     = xef.adder[i]
         brd.dscr     = xef.adder[i + 6]
         brd.basis-w  = xitem.basis-w.
      end.

      assign
       brd.qty     = v-qty
       brd.qty-uom = b-uom
       brd.sc-uom  = b-uom
       brd.cost    = v-cost
       brd.len     = xef.gsh-len
       brd.wid     = xef.gsh-wid
       brd.cost-m  = a-cost / qm.
    end.
  end.
  else next.

  assign
   dm-tot[3] = dm-tot[3] + v-setup
   dm-tot[4] = dm-tot[4] + (a-cost / qm)
   dm-tot[5] = dm-tot[5] + a-cost.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
