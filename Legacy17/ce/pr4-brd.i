/* --------------------------------------------------- ce/pr4-brd.i 03/97 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF VAR v-new-uom LIKE b-uom NO-UNDO.
DEF VAR v-hld-b-qty LIKE b-qty NO-UNDO.
DEF VAR v-hld-save-qty LIKE save-qty NO-UNDO.
DEF VAR v-hld-hld-qty LIKE save-qty NO-UNDO.
DEF VAR hld-qty LIKE save-qty NO-UNDO.
DEF VAR dQtyOtherForms AS DEC NO-UNDO.
DEF VAR v-out AS INT NO-UNDO.

{rm/bestvend.i NEW}

RUN rm/bestvnd1.p (ROWID(xeb)).  /* create temp-tables tt-ei, tt-eiv */

FIND FIRST tt-ei NO-ERROR.

ASSIGN
 hld-qty = qty
 b-uom   = item.cons-uom
 b-msh   = 0.

IF NOT vprint THEN
  ASSIGN
   v-vend-no    = xef.vend-no
   lv-save-vend = v-vend-no.

IF AVAIL tt-ei THEN DO:
  IF tt-ei.std-uom NE "" THEN b-uom = tt-ei.std-uom.

  FIND FIRST tt-eiv WHERE
      tt-eiv.company EQ tt-ei.company AND
      tt-eiv.i-no EQ tt-ei.i-no AND
      tt-eiv.item-type EQ YES
      AND tt-eiv.vend-no   EQ (IF v-vend-no EQ "bestvendor" THEN ""
                               ELSE v-vend-no)
      NO-ERROR.
END.          

ELSE b-msh = IF ce-ctrl.r-cost THEN item.avg-cost ELSE item.last-cost.

IF NOT xeb.pur-man AND (ceboard-log EQ NO OR v-vend-no EQ "") AND xef.cost-msh GT 0 THEN
   b-uom = xef.cost-uom.

RUN sys/ref/convquom.p("MSF", "EA", item.basis-w,
                       xef.gsh-len, xef.gsh-wid, xef.gsh-dep,
                       b-qty, OUTPUT save-qty).

RUN sys/inc/numup.p (xef.company, xef.est-no, xef.form-no, OUTPUT v-num-up).

RUN est/ef-#out.p (ROWID(xef), OUTPUT v-out).

ASSIGN
 v-hld-b-qty    = b-qty
 v-hld-save-qty = save-qty
 v-hld-hld-qty  = hld-qty.

/*IF NOT xeb.pur-man THEN
  RUN cec/groupcst.p (ROWID(xeb), INPUT-OUTPUT save-qty, INPUT-OUTPUT b-qty).*/

if xeb.pur-man and b-uom eq "C" then
  ASSIGN
   b-qty = save-qty / 100
   v-hld-b-qty = v-hld-save-qty / 100.
      
else
if b-uom eq "MSH" or b-uom eq "M" then
  ASSIGN
   b-qty = save-qty / 1000
   v-hld-b-qty = v-hld-save-qty / 1000.
      
else
if b-uom eq "EA"                                                     or
   (lookup(b-uom,"LB,DRM,ROL,PKG,SET,DOZ,BDL") gt 0 and xeb.pur-man) then
  ASSIGN
   b-qty = save-qty
   v-hld-b-qty = v-hld-save-qty.
      
else
if b-uom eq "TON" then
  ASSIGN
   b-qty = (b-qty * xef.weight) / 2000
   v-hld-b-qty = (v-hld-b-qty * xef.weight) / 2000.
  
else
if b-uom eq "LB" then
   ASSIGN
      b-qty = b-qty * xef.weight
      v-hld-b-qty = v-hld-b-qty * xef.weight.

else
if b-uom eq "REM" then do:
  ASSIGN
   b-qty = (if v-corr then (b-qty / .007) else (b-qty * 144)) * 1000 /
           (xef.gsh-wid * 10000)
   v-hld-b-qty = (if v-corr then (v-hld-b-qty / .007) else (v-hld-b-qty * 144)) * 1000 /
           (xef.gsh-wid * 10000).
  {sys/inc/roundup.i b-qty}
  {sys/inc/roundup.i v-hld-b-qty}
end.

ELSE
IF b-uom eq "LF" THEN
  ASSIGN
   b-qty = b-qty * 1000 / (xef.gsh-wid / 12)
   v-hld-b-qty = v-hld-b-qty * 1000 / (xef.gsh-wid / 12).

IF AVAIL tt-ei THEN DO:
  IF lv-save-vend EQ "bestvendor" OR xeb.pur-man THEN DO:
    RUN rm/bestvend.p (ROWID(xef), b-qty, OUTPUT lv-bestvend-id).
    IF lv-bestvend-id NE ? THEN
    FIND tt-eiv WHERE tt-eiv.row-id EQ lv-bestvend-id NO-LOCK NO-ERROR.
  END.
  dQtyOtherForms = 0.
  
  /* Total Quantity for other forms with same board, len, wid */
  IF xest.est-type GT 2 THEN 
      RUN est/calcTotalBrdQty.p 
        (INPUT cocode,
         INPUT xef.board,
         INPUT xef.gsh-len,
         INPUT xef.gsh-wid,
         INPUT ROWID(xef),
         INPUT ceBoard-log,
         INPUT v-vend-no,
         INPUT v-corr,         
         OUTPUT dQtyOtherForms).  
  
  IF AVAIL tt-eiv THEN v-vend-no = tt-eiv.vend-no.
     
  IF vprint THEN
    xef.vend-no = IF lv-save-vend EQ "bestvendor" THEN lv-save-vend
                                                  ELSE v-vend-no.

  do j = 1 to 20:
    if avail tt-eiv then do:
      if tt-eiv.run-qty[j] ge b-qty + dQtyOtherForms then do:
        assign
         b-msh   = tt-eiv.run-cost[j]
         v-setup = tt-eiv.setups[j].
        leave.
      end.
    end.
   
    else
    if tt-ei.run-qty[j] ge b-qty + dQtyOtherForms then do:
      b-msh = tt-ei.run-cost[j].
      leave.
    end.
  end.
end.

if (ceboard-log EQ NO OR v-vend-no EQ "") AND xef.cost-msh gt 0 then
  assign
   b-msh   = xef.cost-msh
   v-setup = 0.

b-cost = b-qty * b-msh.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
