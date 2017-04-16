/* ------------------------------------------------------ ce/kxy1.i 08/98 JLF */
/* note: qty must be set to sheets or blanks before entering kxy.i            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

DEF SHARED VAR qty AS INT NO-UNDO.

def shared var maxco    as int no-undo.
def shared var sh-wid   as dec no-undo.
def shared var sh-len   as dec no-undo.
def shared var xcal     as dec no-undo.
def shared var v-n-out  as int no-undo.
def shared var call_id AS RECID NO-UNDO.

def var li as int no-undo.
def var v-across as int no-undo.
def var v-up as int no-undo.
def var v-qty as dec no-undo.
def var ld-parts like eb.yld-qty no-undo.
def var ld-ink-blk as dec no-undo.
def var ld-ink-frm as dec no-undo.
DEF VAR v-t-win AS DEC DECIMALS 4 NO-UNDO.
DEF BUFFER b-item2 FOR ITEM.

{ce/msfcalc.i}

find first mstd where recid(mstd) eq call_id no-lock no-error.
find first {1} of mstd where {2} no-lock no-error.
find first mach of mstd no-lock.

IF LOOKUP(mach.dept[1],"RC,CR") GT 0 THEN
ASSIGN
 sh-len = xef.gsh-wid
 sh-wid = xef.gsh-len.

ELSE
IF LOOKUP(mach.dept[1],"PR,GU,LM") GT 0 OR xef.n-out-l LE 1 THEN
  ASSIGN
   sh-len = xef.nsh-wid
   sh-wid = xef.nsh-len.
ELSE
  ASSIGN
   sh-len = xef.trim-w
   sh-wid = xef.trim-l.

assign
 call_id = ?
 v-qty   = qty
 ld-parts = 0.

FOR EACH eb
    WHERE eb.company EQ xef.company
      AND eb.est-no  EQ xef.est-no
      AND eb.form-no EQ xef.form-no
    NO-LOCK:

  ASSIGN
    ld-ink-blk = 0
    v-t-win = 0.

  IF eb.est-type EQ 1 THEN
     do li = 1 TO 4:
        find first b-item2 WHERE
             b-item2.company EQ xef.company and
             b-item2.i-no eq xef.leaf[li]
             no-lock no-error.
       
        if avail b-item2 and b-item2.mat-type eq "W" and
           xef.leaf-l[li] ne 0 and xef.leaf-w[li] ne 0 then
           DO:
              /*sheet fed windowing*/
              IF xef.leaf-bnum[li] NE 0 THEN
                 v-t-win = v-t-win + (xef.leaf-l[li] * xef.leaf-w[li]).
              ELSE
                 v-t-win = v-t-win + (xef.leaf-l[li] * xef.leaf-w[li] / eb.num-up).
           END.
     end.
  ELSE
     v-t-win = eb.t-win.

  DO li = 1 TO EXTENT(eb.i-code2):
    IF eb.i-code2[li] NE "" AND eb.i-%2[li] NE 0 THEN DO:
      FIND FIRST item
          WHERE item.company   EQ eb.company
            AND item.i-no      EQ eb.i-code2[li]
            AND INDEX("IV",item.mat-type) GT 0
            AND item.ink-type  NE "A" 
          NO-LOCK NO-ERROR.

      IF AVAIL item THEN
        ld-ink-blk = ld-ink-blk + ((eb.t-sqin - v-t-win) *
                                   eb.num-up * (eb.i-%2[li] / 100)).
    END.

    IF ld-ink-blk GT (eb.t-sqin - v-t-win) * eb.num-up THEN DO:
      ld-ink-blk = (eb.t-sqin - v-t-win) * eb.num-up.
      LEAVE.
    END.
  END.
  ld-ink-frm = ld-ink-frm + ld-ink-blk.

  IF eb.est-type EQ 2 THEN
    ld-parts = ld-parts + (IF eb.cust-% GT 0 THEN eb.cust-%
                                             ELSE (-1 / eb.cust-%)).
END.
ld-ink-frm = ld-ink-frm / (xef.nsh-wid * xef.nsh-len) * 100.
IF ld-ink-frm GT 100 THEN ld-ink-frm = 100.

if mach.p-type eq "B" then v-up = xeb.num-up.

else do:
  run sys/inc/numup.p (xef.company,xef.est-no, xef.form-no, output v-up).
  
  /*if xest.est-type eq 4 then do:
    qty = 0.
    
    for each eb where eb.company = xest.company 
                  and eb.est-no   eq xest.est-no
                  and eb.form-no eq xeb.form-no
                  no-lock:
        
        qty = qty + eb.yld-qty.  
    end.
  end.*/
end.

/* --------------------------------- c o l  ----- */
x = 0.

find first est where est.company = xef.company 
                 and est.est-no eq xef.est-no no-lock.

do while avail {1} and x eq 0:
  if mstd.{3}-x eq 1 then
    {ce/kxy2.i "{1}" col maxco}

  else
  if mstd.{3}-x eq 2 then
    {ce/kxy2.i "{1}" col xeb.len}

  else
  if mstd.{3}-x eq 3 then
    {ce/kxy2.i "{1}" col xeb.wid}

  else
  if mstd.{3}-x eq 4 then
    {ce/kxy2.i "{1}" col xeb.t-len}

  else
  if mstd.{3}-x eq 5 then
    {ce/kxy2.i "{1}" col xeb.t-wid}

  else
  if mstd.{3}-x eq 6 then
    {ce/kxy2.i "{1}" col xeb.lin-in}

  else
  if mstd.{3}-x eq 7 then
    {ce/kxy2.i "{1}" col xeb.t-sqin}

  else
  if mstd.{3}-x eq 8 then
    {ce/kxy2.i "{1}" col "xef.cal * 1000"}

  else
  if mstd.{3}-x eq 9 then
    {ce/kxy2.i "{1}" col xef.weight}

  else
  if mstd.{3}-x eq 10 then
    {ce/kxy2.i "{1}" col xef.roll-wid}

  else
  if mstd.{3}-x eq 11 then
    {ce/kxy2.i "{1}" col sh-len}

  else
  if mstd.{3}-x eq 12 then
    {ce/kxy2.i "{1}" col sh-wid}

  else
  if mstd.{3}-x eq 13 then
    {ce/kxy2.i "{1}" col v-up}

  else
  if mstd.{3}-x eq 14 then
    {ce/kxy2.i "{1}" col xef.leaf-l[1]}

  else
  if mstd.{3}-x eq 15 then
    {ce/kxy2.i "{1}" col xef.leaf-w[1]}

  else
  if mstd.{3}-x eq 16 then
    {ce/kxy2.i "{1}" col xef.gsh-len}

  else
  if mstd.{3}-x eq 17 then
    {ce/kxy2.i "{1}" col xef.gsh-wid}

  else
  if mstd.{3}-x eq 18 then
    {ce/kxy2.i "{1}" col qty}

  else
  if mstd.{3}-x eq 19 then
    {ce/kxy2.i "{1}" col xef.die-in}

  else
  if mstd.{3}-x eq 20 then
    {ce/kxy2.i "{1}" col "(qty / (v-up * v-n-out))"}

  else
  if mstd.{3}-x eq 21 then
    /*if v-corr then
      {ce/kxy2.i "{1}" row "(sh-wid * sh-len) * .007"}
    else
      {ce/kxy2.i "{1}" row "(sh-wid * sh-len) / 144"}*/
    {ce/kxy2.i "{1}" col "sh-wid * sh-len"}

  else
  if mstd.{3}-x eq 22 then
    if est.est-type eq 1 then
      {ce/kxy2.i "{1}" col "(xeb.i-col + xeb.i-coat)"}
    else
      {ce/kxy2.i "{1}" col "(xef.f-col + xef.f-coat)"}

  else
  if mstd.{3}-x eq 23 then
    {ce/kxy2.i "{1}" col xef.n-cuts}

  else
  if mstd.{3}-x eq 24 then
    {ce/kxy2.i "{1}" col xef.blank-qty}

  else
  if mstd.{3}-x eq 25 then
    {ce/kxy2.i "{1}" col xef.n-out}

  else
  if mstd.{3}-x eq 26 then
    {cec/kxy2.i "{1}" col xef.n-out-l}

  else
  if mstd.{3}-x eq 27 then
    {cec/kxy2.i "{1}" col ld-parts}

  else
  if mstd.{3}-x eq 28 then
    {cec/kxy2.i "{1}" col ld-ink-frm}

  else
  if mstd.{3}-x eq 30 AND INDEX("GL,DC",ip-est-op-dept) > 0 then
    {cec/kxy2.i "{1}" col ip-est-op-n-out}

  else
  if mstd.{3}-x eq 99 then i = 1.

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
    {ce/kxy2.i "{1}" row maxco}

  else
  if mstd.{3}-y eq 2 then
    {ce/kxy2.i "{1}" row xeb.len}

  else
  if mstd.{3}-y eq 3 then
    {ce/kxy2.i "{1}" row xeb.wid}

  else
  if mstd.{3}-y eq 4 then
    {ce/kxy2.i "{1}" row xeb.t-len}

  else
  if mstd.{3}-y eq 5 then
    {ce/kxy2.i "{1}" row xeb.t-wid}

  else
  if mstd.{3}-y eq 6 then
    {ce/kxy2.i "{1}" row xeb.lin-in}

  else
  if mstd.{3}-y eq 7 then
    {ce/kxy2.i "{1}" row xeb.t-sqin}

  else
  if mstd.{3}-y eq 8 then
    {ce/kxy2.i "{1}" row xef.cal}

  else
  if mstd.{3}-y eq 9 then
    {ce/kxy2.i "{1}" row xef.weight}

  else
  if mstd.{3}-y eq 10 then
    {ce/kxy2.i "{1}" row xef.roll-wid}

  else
  if mstd.{3}-y eq 11 then
    {ce/kxy2.i "{1}" row sh-len}

  else
  if mstd.{3}-y eq 12 then
    {ce/kxy2.i "{1}" row sh-wid}

  else
  if mstd.{3}-y eq 13 then
    {ce/kxy2.i "{1}" row v-up}

  else
  if mstd.{3}-y eq 14 then
    {ce/kxy2.i "{1}" row xef.leaf-l[1]}

  else
  if mstd.{3}-y eq 15 then
    {ce/kxy2.i "{1}" row xef.leaf-w[1]}

  else
  if mstd.{3}-y eq 16 then
    {ce/kxy2.i "{1}" row xef.gsh-len}

  else
  if mstd.{3}-y eq 17 then
    {ce/kxy2.i "{1}" row xef.gsh-wid}

  else
  if mstd.{3}-y eq 18 then
    {ce/kxy2.i "{1}" row qty}

  else
  if mstd.{3}-y eq 19 then
    {ce/kxy2.i "{1}" row xef.die-in}

  else
  if mstd.{3}-y eq 20 then
    {ce/kxy2.i "{1}" row "(qty / (v-up * v-n-out))"}

  else
  if mstd.{3}-y eq 21 then
    /*if v-corr then
      {ce/kxy2.i "{1}" row "(sh-wid * sh-len) * .007"}
    else
      {ce/kxy2.i "{1}" row "(sh-wid * sh-len) / 144"}*/
    {ce/kxy2.i "{1}" row "sh-wid * sh-len"}

  else
  if mstd.{3}-y eq 22 then
    if est.est-type eq 1 then
      {ce/kxy2.i "{1}" row "(xeb.i-col + xeb.i-coat)"}
    else
      {ce/kxy2.i "{1}" row "(xef.f-col + xef.f-coat)"}

  else
  if mstd.{3}-y eq 23 then
    {ce/kxy2.i "{1}" row xef.n-cuts}

  else
  if mstd.{3}-y eq 24 then
    {ce/kxy2.i "{1}" row xef.blank-qty}

  else
  if mstd.{3}-y eq 25 then
    {ce/kxy2.i "{1}" row xef.n-out}

  else
  if mstd.{3}-y eq 26 then
    {cec/kxy2.i "{1}" row xef.n-out-l}

  else
  if mstd.{3}-y eq 27 then
    {cec/kxy2.i "{1}" row ld-parts}

  else
  if mstd.{3}-y eq 28 then
    {cec/kxy2.i "{1}" row ld-ink-frm}

  else
  if mstd.{3}-Y eq 30 AND INDEX("GL,DC",ip-est-op-dept) > 0 then
    {cec/kxy2.i "{1}" row ip-est-op-n-out}

  else
  if mstd.{3}-y eq 99 then i = 1.

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

qty = v-qty.

/* end ---------------------------------- copr. 1998  advanced software, inc. */
