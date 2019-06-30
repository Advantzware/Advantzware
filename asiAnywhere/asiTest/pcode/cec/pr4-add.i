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

           FIND FIRST b-qty WHERE
                b-qty.reftable = "vend-qty" AND
                b-qty.company = xe-item-vend.company AND
		        b-qty.CODE    = xe-item-vend.i-no AND
                b-qty.code2   = xe-item-vend.vend-no
                NO-LOCK NO-ERROR.

           IF AVAIL b-qty THEN
           DO:
              FIND FIRST b-cost WHERE
                   b-cost.reftable = "vend-cost" AND
                   b-cost.company = xe-item-vend.company AND
		           b-cost.CODE    = xe-item-vend.i-no AND
                   b-cost.code2   = xe-item-vend.vend-no
                   NO-LOCK NO-ERROR.
             
              FIND FIRST b-setup WHERE
                   b-setup.reftable = "vend-setup" AND
                   b-setup.company = xe-item-vend.company AND
		           b-setup.CODE    = xe-item-vend.i-no AND
                   b-setup.code2   = xe-item-vend.vend-no
                   NO-LOCK NO-ERROR.

              DO j = 1 TO 10:
                 ASSIGN
                    tt-eiv.run-qty[j + 10] = b-qty.val[j]
                    tt-eiv.run-cost[j + 10] = b-cost.val[j]
                    tt-eiv.setups[j + 10] = b-setup.val[j].
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

           FIND FIRST b-qty WHERE
                b-qty.reftable = "blank-vend-qty" AND
                b-qty.company = xe-item.company AND
		        b-qty.CODE    = xe-item.i-no
                NO-LOCK NO-ERROR.

           IF AVAIL b-qty THEN
           DO:
              FIND FIRST b-cost WHERE
                   b-cost.reftable = "blank-vend-cost" AND
                   b-cost.company = xe-item.company AND
		           b-cost.CODE    = xe-item.i-no
                   NO-LOCK NO-ERROR.

              DO j = 1 TO 10:
                 ASSIGN
                    tt-ei.run-qty[j + 10] = b-qty.val[j]
                    tt-ei.run-cost[j + 10] = b-cost.val[j].
              END.
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

      ELSE DO:
        v-cost = if ce-ctrl.r-cost then xitem.avg-cost else xitem.last-cost.

        if b-uom ne "MSF" then
          run sys/ref/convquom.p("MSF", b-uom, xitem.basis-w,
                                 xef.gsh-len, xef.gsh-wid, xef.gsh-dep,
                                 brd-sf[4], output v-qty).
      END.

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
