/* ----------------------------------------------------- cec/kxy1.i 08/98 JLF */
/* note: qty must be set to sheets or blanks before entering kxy.i            */
/* -------------------------------------------------------------------------- */

def input parameter v-recid as recid.

{sys/inc/var.i shared}

def shared var call_id as recid no-undo.

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

def shared var qty      as INT NO-UNDO.
def shared var maxco    as int no-undo.
def shared var sh-wid   as de no-undo.
def shared var sh-len   as de no-undo.

def var li as int no-undo.
def var v-across as INT NO-UNDO.
def var v-up     as INT NO-UNDO.
def var v-bsqft as DEC NO-UNDO.
def var v-ssqft as DEC NO-UNDO.
def var v-len like ef.gsh-len NO-UNDO.
def var v-wid like ef.gsh-wid NO-UNDO.
def var v-dep like ef.gsh-dep NO-UNDO.
def var v-nsh like ef.nsh-wid NO-UNDO.
def var v-out like ef.n-out NO-UNDO.
def var v-cut like ef.n-cuts NO-UNDO.
def var v-hed like ef.n-cuts NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.
DEF VAR v-yld AS DEC NO-UNDO.
def var ld-parts as dec extent 2 no-undo.
def var ld-ink-blk as dec no-undo.
def var ld-ink-frm as dec no-undo.
DEF VAR v-long-qty-set AS DEC NO-UNDO.
DEF VAR v-short-qty-set AS DEC NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEF VARIABLE dBlankLen AS DECIMAL NO-UNDO.
DEF VARIABLE dBlankWid AS DECIMAL NO-UNDO.

assign
 v-len = sh-len
 v-wid = sh-wid
 v-dep = xef.cal
 v-nsh = xef.nsh-wid
 v-out = xef.n-out
 v-hed = 0.

find est-op where recid(est-op) eq v-recid no-lock no-error.
find first mstd where recid(mstd) eq call_id no-lock no-error.
find first {1} of mstd where {2} no-lock no-error.
find first mach of mstd no-lock no-error.
if avail est-op then do:
  if est-op.n-out ne 0 then v-out = est-op.n-out.
  
  {sys/inc/outstrPL.i est-op no}
  if avail reftable then do:
    if reftable.val[4] ne 0 then v-len = reftable.val[4].
    if reftable.val[5] ne 0 then v-wid = reftable.val[5].
    if reftable.val[6] ne 0 then v-dep = reftable.val[6].
    
    if reftable.val[2] ne 0 then
      if reftable.val[2] eq 1 then
        assign
         v-hed = xef.n-cuts - (xef.n-out   - 1)
         v-nsh = xef.nsh-len.
      else
      if reftable.val[2] eq 2 then
        assign
         v-hed = xef.n-cuts - (xef.n-out-l - 1)
         v-nsh = xef.nsh-wid.
      else
        assign
         v-hed = 0
         v-nsh = xef.nsh-dep.
  end.
  
  if v-hed eq 0 and xef.n-cuts gt (xef.n-out - 1) + (xef.n-out-l - 1) then
    v-hed = xef.n-cuts - (xef.n-out-l - 1).

  if v-hed eq ? then v-hed = 0.
end.

v-cut = if xef.n-cuts eq (xef.n-out - 1) + (xef.n-out-l - 1) then
          max(v-out - if v-nsh * v-out lt v-wid then 0 else 1,v-hed)
        else xef.n-cuts.
{cec/msfcalc.i}

if v-corr then
  assign
   v-bsqft = xeb.t-sqin * .007
   v-ssqft = (xef.nsh-len * xef.nsh-wid) * .007.

else
  assign
   v-bsqft = xeb.t-sqin / 144
   v-ssqft = (xef.nsh-len * xef.nsh-wid) / 144.
   
ASSIGN
   call_id = ?
   ld-parts = 0.

FOR EACH eb NO-LOCK
    WHERE eb.company EQ xef.company
      AND eb.est-no  EQ xef.est-no
      AND eb.form-no NE 0
      USE-INDEX est-qty:

  IF xef.est-type EQ 6 THEN
  DO:
     ASSIGN
        v-yld = IF eb.quantityPerSet GT 0 THEN eb.quantityPerSet ELSE (-1 / eb.quantityPerSet)
        ld-parts[1] = ld-parts[1] + v-yld
        v-count = v-count + 1.

     IF v-count EQ 1 THEN
        v-long-qty-set = v-yld.
     ELSE IF v-count EQ 2 THEN
        v-short-qty-set = v-yld.
  END.

  IF eb.form-no EQ xef.form-no THEN DO:
     IF xef.est-type EQ 6 THEN ld-parts[2] = ld-parts[2] + v-yld.
    
     ld-ink-blk = 0.
     DO li = 1 TO EXTENT(eb.i-code):
       IF eb.i-code[li] NE "" AND eb.i-%[li] NE 0 THEN DO:
         FIND FIRST item
             WHERE item.company   EQ eb.company
               AND item.i-no      EQ eb.i-code[li]
               AND INDEX("IV",item.mat-type) GT 0
               AND item.ink-type  NE "A" 
             NO-LOCK NO-ERROR. 
         IF AVAIL item THEN
            ld-ink-blk = ld-ink-blk + ((eb.t-sqin - eb.t-win) *
                                       eb.num-up * (eb.i-%[li] / 100)).
       END.
    
       IF ld-ink-blk GT (eb.t-sqin - eb.t-win) * eb.num-up THEN DO:
          ld-ink-blk = (eb.t-sqin - eb.t-win) * eb.num-up.
          LEAVE.
       END.
     END.
     ld-ink-frm = ld-ink-frm + ld-ink-blk.
  END.
END.


ld-ink-frm = ld-ink-frm / (xef.nsh-wid * xef.nsh-len) * 100.
IF ld-ink-frm GT 100 THEN ld-ink-frm = 100.

IF INDEX("AP",mach.p-type) GT 0 THEN DO:
  ASSIGN
   v-up    = 1
   v-n-out = 1.

  IF mach.p-type EQ "A" THEN v-yld = 1.

  ELSE
  FOR EACH eb FIELDS(quantityPerSet)
      WHERE eb.company EQ xest.company
        AND eb.est-no  EQ xest.est-no
        AND eb.form-no NE 0
      NO-LOCK:
    v-yld = v-yld +
            (IF eb.quantityPerSet LT 0 THEN (-1 / eb.quantityPerSet) ELSE eb.quantityPerSet).
  END.
END.

ELSE DO:
  IF mach.p-type EQ "B" THEN v-up = xeb.num-up.
  ELSE
    RUN sys/inc/numup.p (xef.company, xef.est-no, xef.form-no, OUTPUT v-up).

  RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

  v-yld = IF xeb.est-type EQ 8 THEN 1
          ELSE
          IF xeb.quantityPerSet LT 0 THEN (-1 / xeb.quantityPerSet) ELSE xeb.quantityPerSet.
END.
IF AVAIL est-op AND est-op.spare-char-1 = "R" THEN
    ASSIGN
    dBlankLen  = xeb.t-wid
    dBlankWid  = xeb.t-len .
ELSE 
   ASSIGN
    dBlankLen  = xeb.t-len
    dBlankWid  = xeb.t-wid .


/* --------------------------------- c o l  ----- */
x = 0.

do while avail {1} and x eq 0:
  if mstd.{3}-x eq 1 then
    {cec/kxy2.i "{1}" col maxco}

  else
  if mstd.{3}-x eq 2 then
    {cec/kxy2.i "{1}" col xeb.len}

  else
  if mstd.{3}-x eq 3 then
    {cec/kxy2.i "{1}" col xeb.wid}

  else
  if mstd.{3}-x eq 4 then
    {cec/kxy2.i "{1}" COL dBlankLen /*xeb.t-len*/  }

  else
  if mstd.{3}-x eq 5 then
    {cec/kxy2.i "{1}" COL dBlankWid  /*xeb.t-wid*/ }

  else
  if mstd.{3}-x eq 6 then
    {cec/kxy2.i "{1}" col xeb.lin-in}

  else
  if mstd.{3}-x eq 7 then
    {cec/kxy2.i "{1}" col v-bsqft}

  else
  if mstd.{3}-x eq 8 then
    {cec/kxy2.i "{1}" col "v-dep * 1000"}

  else
  if mstd.{3}-x eq 9 then
    {cec/kxy2.i "{1}" col xef.weight}

  else
  if mstd.{3}-x eq 10 then
    {cec/kxy2.i "{1}" col xef.roll-wid}

  else
  if mstd.{3}-x eq 11 then
    {cec/kxy2.i "{1}" COL xef.nsh-wid }

  else
  if mstd.{3}-x eq 12 then
    {cec/kxy2.i "{1}" COL xef.nsh-len  }

  else
  if mstd.{3}-x eq 13 then
    {cec/kxy2.i "{1}" col v-up}

  else
  if mstd.{3}-x eq 14 then
    {cec/kxy2.i "{1}" col xef.leaf-l[1]}

  else
  if mstd.{3}-x eq 15 then
    {cec/kxy2.i "{1}" col xef.leaf-w[1]}

  else
  if mstd.{3}-x eq 16 then
    {cec/kxy2.i "{1}" col v-len}

  else
  if mstd.{3}-x eq 17 then
    {cec/kxy2.i "{1}" col v-wid}

  else
  if mstd.{3}-x eq 18 then
    {cec/kxy2.i "{1}" col qty}

  else
  if mstd.{3}-x eq 19 then
    {cec/kxy2.i "{1}" col xef.die-in}

  else
  if mstd.{3}-x eq 20 then
    {cec/kxy2.i "{1}" col "(qty * v-yld / xeb.num-up / v-n-out)"}

  else
  if mstd.{3}-x eq 21 then
    {cec/kxy2.i "{1}" col v-ssqft}

  else
  if mstd.{3}-x eq 22 then
    {cec/kxy2.i "{1}" col "(xef.f-col + xef.f-coat)"}

  else
  if mstd.{3}-x eq 23 then
    {cec/kxy2.i "{1}" col v-cut}

  else
  if mstd.{3}-x eq 24 then
    {cec/kxy2.i "{1}" col xef.blank-qty}

  else
  if mstd.{3}-x eq 25 then
    {cec/kxy2.i "{1}" col v-out}

  else
  if mstd.{3}-x eq 26 then
    {cec/kxy2.i "{1}" col xef.n-out-l}

  else
  if mstd.{3}-x eq 27 then
    {cec/kxy2.i "{1}" COL ld-parts[2]}

  else
  if mstd.{3}-x eq 28 then
    {cec/kxy2.i "{1}" COL ld-ink-frm}

  else
  if mstd.{3}-x eq 29 then
    {cec/kxy2.i "{1}" COL ld-parts[1]}

  else
  if mstd.{3}-x eq 31 then
    {cec/kxy2.i "{1}" COL v-long-qty-set}

  else
  if mstd.{3}-x eq 32 then
    {cec/kxy2.i "{1}" COL v-short-qty-set}

  else
  if mstd.{3}-x eq 33 then
    {cec/kxy2.i "{1}" COL xeb.num-wid}

  else
  if mstd.{3}-x eq 34 then
    {cec/kxy2.i "{1}" COL xeb.num-len}

  else
  if mstd.{3}-x eq 98 then i = 1.

  if i gt 0 and i lt 11 then
    assign
     x        = i
     v-across = {1}.across-no.

  else
    find next {1} of mstd
        where {2}
          and {1}.page-no eq 0
        no-lock no-error.
end.

/* --------------------------------- r o w   ----- */
y = 0.
do while avail {1} and y eq 0:
  if mstd.{3}-y eq 1 then
    {cec/kxy2.i "{1}" row maxco}

  else
  if mstd.{3}-y eq 2 then
    {cec/kxy2.i "{1}" row xeb.len}

  else
  if mstd.{3}-y eq 3 then
    {cec/kxy2.i "{1}" row xeb.wid}

  else
  if mstd.{3}-y eq 4 then
    {cec/kxy2.i "{1}" ROW dBlankLen /*xeb.t-len*/ }

  else
  if mstd.{3}-y eq 5 then
    {cec/kxy2.i "{1}" ROW dBlankWid /*xeb.t-wid*/ }

  else
  if mstd.{3}-y eq 6 then
    {cec/kxy2.i "{1}" row xeb.lin-in}

  else
  if mstd.{3}-y eq 7 then
    {cec/kxy2.i "{1}" row v-bsqft}

  else
  if mstd.{3}-y eq 8 then
    {cec/kxy2.i "{1}" row v-dep}

  else
  if mstd.{3}-y eq 9 then
    {cec/kxy2.i "{1}" row xef.weight}

  else
  if mstd.{3}-y eq 10 then
    {cec/kxy2.i "{1}" row xef.roll-wid}

  else
  if mstd.{3}-y eq 11 then
    {cec/kxy2.i "{1}" ROW xef.nsh-wid  }

  else
  if mstd.{3}-y eq 12 then
    {cec/kxy2.i "{1}" ROW xef.nsh-len  }

  else
  if mstd.{3}-y eq 13 then
    {cec/kxy2.i "{1}" row v-up}

  else
  if mstd.{3}-y eq 14 then
    {cec/kxy2.i "{1}" row xef.leaf-l[1]}

  else
  if mstd.{3}-y eq 15 then
    {cec/kxy2.i "{1}" row xef.leaf-w[1]}

  else
  if mstd.{3}-y eq 16 then
    {cec/kxy2.i "{1}" row v-len}

  else
  if mstd.{3}-y eq 17 then
    {cec/kxy2.i "{1}" row v-wid}

  else
  if mstd.{3}-y eq 18 then
    {cec/kxy2.i "{1}" row qty}

  else
  if mstd.{3}-y eq 19 then
    {cec/kxy2.i "{1}" row xef.die-in}

  else
  if mstd.{3}-y eq 20 then
    {cec/kxy2.i "{1}" row "(qty * v-yld / xeb.num-up / v-n-out)"}
  else
  if mstd.{3}-y eq 21 then
    {cec/kxy2.i "{1}" row v-ssqft}

  else
  if mstd.{3}-y eq 22 then
    {cec/kxy2.i "{1}" row "(xef.f-col + xef.f-coat)"}

  else
  if mstd.{3}-y eq 23 then
    {cec/kxy2.i "{1}" row v-cut}

  else
  if mstd.{3}-y eq 24 then
    {cec/kxy2.i "{1}" row xef.blank-qty}

  else
  if mstd.{3}-y eq 25 then
    {cec/kxy2.i "{1}" row v-out}

  else
  if mstd.{3}-y eq 26 then
    {cec/kxy2.i "{1}" row xef.n-out-l}

  else
  if mstd.{3}-y eq 27 then
    {cec/kxy2.i "{1}" row ld-parts[2]}

  else
  if mstd.{3}-y eq 28 then
    {cec/kxy2.i "{1}" row ld-ink-frm}

  else
  if mstd.{3}-y eq 29 then
    {cec/kxy2.i "{1}" row ld-parts[1]}

  else
  if mstd.{3}-y eq 31 then
    {cec/kxy2.i "{1}" row v-long-qty-set}

  else
  if mstd.{3}-y eq 32 then
    {cec/kxy2.i "{1}" row v-short-qty-set}

  else
   if mstd.{3}-y eq 33 then
    {cec/kxy2.i "{1}" row xeb.num-wid}

  else
  if mstd.{3}-y eq 34 then
    {cec/kxy2.i "{1}" row xeb.num-len}

  else
  if mstd.{3}-y eq 98 then i = 1.

  if i gt 0 and i lt 16 then
    assign
     call_id = recid({1})
     y       = i.

  else
    find next {1} of mstd
        where {2}
          and {1}.across-no eq v-across
        no-lock no-error.
end.

/* end ---------------------------------- copr. 1998  advanced software, inc. */
