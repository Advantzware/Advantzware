/* --------------------------------------------- cec/box/pr42-flm.p 2/93 cd */

def input parameter v-vend-no like e-item-vend.vend-no.

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def var i as int no-undo.
def var j as int no-undo.
def shared var qty as INT NO-UNDO .

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

def buffer b-i for item.
DEF BUFFER b-setup FOR reftable.

def var v-sqin-lb   like item.sqin-lb no-undo.
def var v-item-cost like item.avg-cost no-undo.
def var v-setup like e-item-vend.setup no-undo.
DEF VAR v-n-out AS INT NO-UNDO.

DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20
    FIELD setups AS DECIMAL DECIMALS 2 EXTENT 20.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

{cec/print4.i shared shared}
{cec/print42.i shared}

DEF TEMP-TABLE w-flm NO-UNDO LIKE flm
    FIELD flm-rowid AS ROWID FIELD setup LIKE v-setup.

def var qqq as de no-undo.
def var f-uom as ch no-undo.
def var rm-wt$ as de no-undo.
def var rm-wt% as de no-undo.
def var rm-wt  as de no-undo.
def var mrg as de no-undo.
def var v-yld as dec no-undo.
def var fup like eb.num-up NO-UNDO.
DEF VAR lv-deptb LIKE est-op.dept NO-UNDO.
DEF VAR lv-deptf LIKE est-op.dept NO-UNDO.
DEF VAR ld-rm-rate AS DEC NO-UNDO.

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

{cec/msfcalc.i}

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.
mrg = ce-ctrl.win-margin.

/* films */

for each xef where xef.company = xest.company and
                   xef.est-no    eq xest.est-no
               and (xef.form-no eq v-form-no or (not vmclean2)):

   RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

   do i = 1 to 4 with frame ac0  down no-labels no-box stream-io:
     v-setup = 0.

     if xef.leaf[i] = "" then next.
     find first item where item.company = cocode and
                           item.i-no    = xef.leaf[i] no-lock no-error.
     if avail item then find first e-item of item no-lock no-error.
     else next.
     if not avail e-item then f-uom = item.cons-uom.
     else f-uom = e-item.std-uom.

     find first eb
         where eb.company eq xest.company 
           and eb.est-no  eq xest.est-no
           and eb.form-no eq xef.form-no
           and (eb.blank-no eq xef.leaf-bnum[i] or xef.leaf-bnum[i] eq 0)
         no-lock no-error.

     fup = if xef.leaf-bnum[i] eq 0 then 1 else eb.num-up.

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
         f-qty[i]  = (est-op.num-sh * fup * v-n-out *
                           (if v-corr then
                              (xef.leaf-l[i] * xef.leaf-w[i] * .007) else
                              (xef.leaf-l[i] * xef.leaf-w[i] / 144))) /
                     1000 * b-i.basis-w
         f-uom     = "LB"
         v-sqin-lb = 1.

        do j = 1 to 6:
          if xef.adder[j] ne "" then do:
            find first b-i
                where b-i.company eq cocode
                  and b-i.i-no    eq xef.adder[j]
                no-lock no-error.
            if not avail b-i then leave.
            f-qty[i] = f-qty[i] +
                       ((est-op.num-sh * fup * v-n-out *
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

        if not avail est-op then leave.

        FIND FIRST mach WHERE
             mach.company EQ est-op.company AND
             mach.m-code EQ est-op.m-code
             NO-LOCK NO-ERROR.

        assign
         f-qty[i]  = (xef.leaf-w[i] * xef.leaf-l[i]) *
                     (est-op.num-sh *
                      (IF AVAIL mach AND mach.p-type EQ "S" THEN 1
                       ELSE fup) * v-n-out)
         v-sqin-lb = item.sqin-lb.
     end.

     IF AVAIL e-item THEN do:
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


            IF AVAIL e-item-vend THEN
            DO:


               DO j = 1 TO 10:
                  ASSIGN
                     tt-eiv.run-qty[j + 10] = e-item-vend.runQtyXtra[j]
                     tt-eiv.run-cost[j + 10] = e-item-vend.runCostXtra[j]
                     tt-eiv.setups[j + 10] = e-item-vend.setupsXtra[j].
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

            
               DO j = 1 TO 10:
                  ASSIGN
                     tt-ei.run-qty[j + 10] = e-item.runQty[j]
                     tt-ei.run-cost[j + 10] = e-item.runCost[j].
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
        if f-uom eq "MSI" then
           f-cost[i] = (f-qty[i] / 1000) * v-item-cost.
        else
        if f-uom eq "LB" then
           f-cost[i] = (f-qty[i] / v-sqin-lb) * v-item-cost.

        f-qty[i] = f-qty[i] / v-sqin-lb.
     end.

     f-cost[i] = f-cost[i] + v-setup.

     IF xef.leaf-bnum[i] NE 0 THEN DO:
        FIND FIRST blk
            WHERE blk.snum EQ xef.form-no
              AND blk.bnum EQ xef.leaf-bnum[i]
            NO-ERROR.

        ASSIGN
           blk.cost = blk.cost + f-cost[i]
           ld-rm-rate = IF blk.pur-man THEN rm-rate-f ELSE ctrl[3].

        IF ld-rm-rate GT 0 THEN
           ASSIGN
              blk.cost = blk.cost + (rm-wt / 100 * ld-rm-rate)
              blk.lab  = blk.lab  + (rm-wt / 100 * ld-rm-rate)
              ctrl2[3] = ctrl2[3] + (rm-wt / 100 * ld-rm-rate).
     END.

     ELSE
     FOR EACH blk WHERE blk.snum EQ xef.form-no:

        ASSIGN
           blk.cost = blk.cost + (f-cost[i] * blk.pct)
           ld-rm-rate = IF blk.pur-man THEN rm-rate-f ELSE ctrl[3].

        IF ld-rm-rate NE 0 THEN
           ASSIGN
              blk.cost = blk.cost + (rm-wt / 100 * ld-rm-rate * blk.pct)
              blk.lab  = blk.lab  + (rm-wt / 100 * ld-rm-rate * blk.pct)
              ctrl2[3] = ctrl2[3] + (rm-wt / 100 * ld-rm-rate * blk.pct).
     END.

     dm-tot[5] = dm-tot[5] + f-cost[i].

     find first flm where flm.id = item.i-no and
                         flm.snum = xef.form-no and
                         flm.bnum = xef.leaf-bnum[i] no-error.
     if not avail flm then
     do:
        create flm.
        assign
        flm.snum = xef.form-no
        flm.bnum = xef.leaf-bnum[i].
     end.

     FIND FIRST w-flm WHERE w-flm.flm-rowid EQ ROWID(flm) NO-ERROR.
     IF NOT AVAIL w-flm THEN DO:
        CREATE w-flm.
        w-flm.flm-rowid = ROWID(flm).
     END.

     assign
        flm.i-no    = item.i-no
        flm.dscr    = item.est-dscr
        flm.qty     = flm.qty + f-qty[i]
        flm.uom     = f-uom
        flm.cost    = flm.cost + f-cost[i]
        w-flm.setup = w-flm.setup + v-setup.

     if avail eb then flm.id = eb.part-no.
     if flm.bnum gt 0 then
       assign
        v-yld    = if eb.quantityPerSet lt 0 then -1 / eb.quantityPerSet else eb.quantityPerSet
        flm.cosm = f-cost[i] / (tt-blk * v-yld / 1000).
     else flm.cosm = f-cost[i] / ( t-blkqty[xef.form-no] / 1000).
        
   end.
end.

for each flm,
    FIRST w-flm WHERE w-flm.flm-rowid EQ ROWID(flm)
    by flm.snum by flm.bnum
    with no-labels NO-BOX stream-io:

  DISPLAY flm.snum format "99" space(0) "-" space(0) flm.bnum format "9"
          item.i-name flm.qty to 50
          flm.uom at 52 when flm.uom = "MSI"
          "Lbs" when flm.uom = "LB" @ flm.uom
          w-flm.setup when w-flm.setup ne 0 format ">>>9.99" to 63
          flm.cosm to 71
          flm.cost to 80 format ">>>>>9.99" skip.
   lin-count = lin-count + 1.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
