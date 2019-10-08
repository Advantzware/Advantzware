/* ---------------------------------------------------- cec/pr4-flm.p 4/92 cd */

def input parameter v-vend-no like e-item-vend.vend-no.

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def var i as int no-undo.
def var j as int no-undo.
def shared var qty as int NO-UNDO.

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

def buffer b-i for item.

def var v-sqin-lb   like item.sqin-lb NO-UNDO.
def var v-item-cost like item.avg-cost NO-UNDO.
def var v-cons-uom  like item.cons-uom NO-UNDO.
def var v-setup like e-item-vend.setup NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.
DEF VAR lv-deptb LIKE est-op.dept NO-UNDO.
DEF VAR lv-deptf LIKE est-op.dept NO-UNDO.

DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-setup FOR reftable.

DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20
    FIELD setups AS DECIMAL DECIMALS 2 EXTENT 20.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

&SCOPED-DEFINE valid-est-op                                            ~
    WHERE est-op.company EQ xest.company                               ~
      AND est-op.est-no  EQ xest.est-no                                ~
      AND est-op.qty     EQ v-op-qty                                   ~
      AND est-op.line    GT 500                                        ~
      AND est-op.s-num   EQ xef.form-no                                ~
      AND CAN-FIND(FIRST mach                                          ~
                   WHERE mach.company       EQ est-op.company          ~
                     AND mach.m-code        EQ est-op.m-code           ~
                     AND ((est-op.op-sb     EQ NO               AND    ~
                           est-op.b-num     EQ xef.leaf-bnum[i] AND    ~
                           xef.leaf-bnum[i] GT 0                AND    ~
                           (mach.dept[1]    EQ lv-deptb OR             ~
                            mach.dept[2]    EQ lv-deptb OR             ~
                            mach.dept[3]    EQ lv-deptb OR             ~
                            mach.dept[4]    EQ lv-deptb))           OR ~
                          ((est-op.op-sb    EQ YES OR                  ~
                            xef.leaf-bnum[i] LE 0)              AND    ~
                           (mach.dept[1]    EQ lv-deptf OR             ~
                            mach.dept[2]    EQ lv-deptf OR             ~
                            mach.dept[3]    EQ lv-deptf OR             ~
                            mach.dept[4]    EQ lv-deptf))))

{cec/print4.i shared shared}

{cec/msfcalc.i}

{cec/rollfac.i}


save-qty = qty.
find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

/* films */
do i = 1 to 2 with frame ac0  down no-labels no-box:
   v-setup = 0.

   if xef.leaf[i] ne "" then do:
      find first item
          where item.company eq cocode
            and item.i-no    eq xef.leaf[i]
          no-lock no-error.
      if available item then find first e-item of item no-lock no-error.
      else next.
      v-cons-uom = item.cons-uom.

      if item.mat-type eq "W" then do:
         find first b-i
             where b-i.company eq cocode
               and b-i.i-no    eq xef.board
             no-lock no-error.
         if not avail b-i then leave.

         ASSIGN
          lv-deptb = "WN"
          lv-deptf = "WS".

         FIND FIRST est-op {&valid-est-op} NO-LOCK NO-ERROR.

         if not avail est-op then leave.

         assign
          f-qty[i]   = (est-op.num-sh * xeb.num-up * v-n-out *
                          (if v-corr then
                             (xef.leaf-l[i] * xef.leaf-w[i] * .007) else
                             (xef.leaf-l[i] * xef.leaf-w[i] / 144))) /
                       1000 * b-i.basis-w
          v-cons-uom = "LB"
          v-sqin-lb  = 1.

        do j = 1 to 6:
          if xef.adder[j] ne "" then do:
            find first b-i
                where b-i.company eq cocode
                  and b-i.i-no    eq xef.adder[j]
                no-lock no-error.
            if not avail b-i then leave.
            f-qty[i] = f-qty[i] +
                       ((est-op.num-sh * xeb.num-up * v-n-out *
                           (if v-corr then
                              (xef.leaf-l[i] * xef.leaf-w[i] * .007) else
                              (xef.leaf-l[i] * xef.leaf-w[i] / 144))) /
                        1000 * b-i.basis-w).
          end.
        end.
        
        f-qty[i] = f-qty[i] * item.shrink.
      end.

      else
      if item.mat-type eq "F" then do:
         ASSIGN
          lv-deptb = "FB"
          lv-deptf = "FS".

         FIND FIRST est-op {&valid-est-op} NO-LOCK NO-ERROR.

         if not available est-op then leave.

         FIND FIRST mach WHERE
              mach.company EQ est-op.company AND
              mach.m-code EQ est-op.m-code
              NO-LOCK NO-ERROR.

         assign
          f-qty[i]  = (xef.leaf-w[i]  * xef.leaf-l[i]) *
                       (est-op.num-sh *
                        (IF AVAIL mach AND mach.p-type EQ "S" THEN 1
                         ELSE xeb.num-up) *
                       v-n-out)
          v-sqin-lb = item.sqin-lb.
      end.

      IF AVAIL e-item THEN DO:
         find first e-item-vend of e-item
             where e-item-vend.item-type eq yes
               and e-item-vend.vend-no   eq ""
             no-lock no-error.

         f-qty[i] = f-qty[i] / (if e-item.std-uom eq "MSI" then 1000
                                else
                                if e-item.std-uom eq "LB"  then v-sqin-lb
                                else 1).

         EMPTY TEMP-TABLE tt-eiv.
         EMPTY TEMP-TABLE tt-ei.

         IF AVAIL e-item-vend THEN
         DO:
            CREATE tt-eiv.
            DO j = 1 TO 10:
               ASSIGN
                  tt-eiv.run-qty[j] = e-item-vend.run-qty[j]
                  tt-eiv.run-cost[j] = e-item-vend.run-cost[j]
                  tt-eiv.setups[j] = e-item-vend.setups[j].
            END.

            FIND FIRST b-qty WHERE
                 b-qty.reftable = "vend-qty" AND
                 b-qty.company = e-item-vend.company AND
		         b-qty.CODE    = e-item-vend.i-no AND
                 b-qty.code2   = e-item-vend.vend-no
                 NO-LOCK NO-ERROR.

            IF AVAIL b-qty THEN
            DO:
               FIND FIRST b-cost WHERE
                    b-cost.reftable = "vend-cost" AND
                    b-cost.company = e-item-vend.company AND
		            b-cost.CODE    = e-item-vend.i-no AND
                    b-cost.code2   = e-item-vend.vend-no
                    NO-LOCK NO-ERROR.
              
               FIND FIRST b-setup WHERE
                    b-setup.reftable = "vend-setup" AND
                    b-setup.company = e-item-vend.company AND
		            b-setup.CODE    = e-item-vend.i-no AND
                    b-setup.code2   = e-item-vend.vend-no
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
            EMPTY TEMP-TABLE tt-ei.
            CREATE tt-ei.
            DO j = 1 TO 10:
               ASSIGN
                  tt-ei.run-qty[j] = e-item.run-qty[j]
                  tt-ei.run-cost[j] = e-item.run-cost[j].
            END.

            FIND FIRST b-qty WHERE
                 b-qty.reftable = "blank-vend-qty" AND
                 b-qty.company = e-item.company AND
		         b-qty.CODE    = e-item.i-no
                 NO-LOCK NO-ERROR.

            IF AVAIL b-qty THEN
            DO:
               FIND FIRST b-cost WHERE
                    b-cost.reftable = "blank-vend-cost" AND
                    b-cost.company = e-item.company AND
		            b-cost.CODE    = e-item.i-no
                    NO-LOCK NO-ERROR.

               DO j = 1 TO 10:
                  ASSIGN
                     tt-ei.run-qty[j + 10] = b-qty.val[j]
                     tt-ei.run-cost[j + 10] = b-cost.val[j].
               END.
            END.
         END.

         do j = 1 to 20:
            if avail e-item-vend then do:
               if tt-eiv.run-qty[j] ge f-qty[i] then do:
                  assign
                   f-cost[i] = (f-qty[i] * tt-eiv.run-cost[j])
                   v-setup   = tt-eiv.setups[j].
                  leave.
               end.
            end.
   
            else
            if tt-ei.run-qty[j] ge b-qty then do:
               f-cost[i] = (f-qty[i] * tt-ei.run-cost[j]).
               leave.
            end.
         end.
      end.

      else do:
         v-item-cost = if ce-ctrl.r-cost then item.avg-cost else item.last-cost.
         if v-cons-uom eq "MSI" then
            f-cost[i] = (f-qty[i] / 1000) * v-item-cost.
         else
         if v-cons-uom eq "LB" then
            f-cost[i] = (f-qty[i] / v-sqin-lb) * v-item-cost.

         f-qty[i] = f-qty[i] / v-sqin-lb.
      end.

      ASSIGN
      dm-tot[4] = dm-tot[4] + ((f-cost[i] + v-setup) / (save-qty / 1000))
      dm-tot[5] = dm-tot[5] + (f-cost[i] + v-setup).

      find first BRD where BRD.form-no eq xef.form-no and
                           BRD.blank-no eq 1 and
                           BRD.i-no    eq xef.leaf[i]
                           no-error.
      if not available BRD then
      do:
         create BRD.
         assign BRD.form-no = xef.form-no
                BRD.blank-no = 1
                BRD.i-no    = xef.leaf[i]
                BRD.dscr    = xef.leaf-dscr[i]
                BRD.basis-w = item.basis-w.
      end.

      ASSIGN
         BRD.qty = f-qty[i]
         BRD.qty-uom = "Lb"
         BRD.sc-uom = "Lb"
         BRD.cost = (f-cost[i] + v-setup) / f-qty[i]
         BRD.cost-m = (f-cost[i] + v-setup) / (save-qty / 1000).

      display item.i-name f-qty[i]  to 48 space(0) " Lb"
              v-setup when v-setup ne 0 format ">>>9.99" to 59
              (f-cost[i] + v-setup) / (save-qty / 1000) / v-sqft-fac format ">>>>9.99" to 68
              (f-cost[i] + v-setup) format ">>>>,>>9.99" to 80 skip
          with stream-io.
   end.
end.
qty = save-qty.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
