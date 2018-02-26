/* -------------------------------------------------- cec/mach-sq2.p 4/92 cd  */
/* create machine routing sequence    part-2                                  */
/* -------------------------------------------------------------------------- */

def input parameter v-def-rout as LOG NO-UNDO.
DEF INPUT PARAMETER ip-build-combo AS LOG NO-UNDO.

{sys/inc/var.i shared}


def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{cec/print4.i "new shared" "new shared"}

def new shared var call_id as recid no-undo.

def shared var qty    as INT NO-UNDO.
def shared var maxco  as int no-undo.

DEF VAR ll-foam AS LOG NO-UNDO.
DEF VAR ld-sqft AS DEC NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR gotAttAdder AS LOG NO-UNDO.
{ce/mach-lst.i}

DEF BUFFER bf-eb FOR eb. 
DEF VAR iDieQty AS INT NO-UNDO.
DEF VAR machAttachRec AS RECID NO-UNDO.
def var k_frac as dec init 6.25 no-undo.

RUN ce/mach-chk.p (v-def-rout).

j = 1.
for each m-lst by m-lst.f-no by m-lst.seq by m-lst.b-no by m-lst.pass-no:
  find first mach
      {sys/look/machW.i}
        and mach.m-code eq m-lst.m-code
      no-lock no-error.
      
  create est-op.
  assign
   est-op.company    = xest.company
   est-op.e-num      = xest.e-num
   est-op.est-no     = xest.est-no
   est-op.line       = j
   est-op.qty        = IF xest.est-type EQ 8 THEN 0 ELSE qty
   est-op.s-num      = m-lst.f-no
   est-op.b-num      = if xest.est-type eq 5 then 1 else m-lst.b-no
   est-op.op-pass    = m-lst.pass-no
   est-op.op-sb      = yes
   est-op.m-code     = mach.m-code
   est-op.m-dscr     = mach.m-dscr
   est-op.dept       = m-lst.dept
   est-op.d-seq      = mach.d-seq
   est-op.n-out      = m-lst.n-out
   est-op.op-spoil   = mach.run-spoil
   est-op.op-crew[1] = mach.mr-crusiz
   est-op.op-crew[2] = mach.run-crusiz.

  gotattadder = NO.
  FIND FIRST eb NO-LOCK
      WHERE eb.company   EQ xest.company
        AND eb.est-no    EQ xest.est-no
        AND eb.form-no   EQ m-lst.f-no
        AND (eb.blank-no EQ m-lst.b-no OR m-lst.b-no EQ 0)
      NO-ERROR.
      
  IF AVAIL eb THEN DO:

    FOR EACH bf-eb WHERE bf-eb.company = xest.company
                     AND bf-eb.est-no = eb.est-no
                     AND bf-eb.form-no = eb.form-no
                     AND bf-eb.blank-no > 0 :
    
    li = 0.
    
    /*FOR EACH mach-attach NO-LOCK
        WHERE mach-attach.company  EQ est-op.company
          AND mach-attach.m-code   EQ est-op.m-code
          AND mach-attach.style    EQ eb.style:
      li = li + 1.
      IF li LE EXTENT(est-op.att-type) THEN
        ASSIGN
         est-op.att-type[li] = mach-attach.att-type
         est-op.att-qty[li]  = mach-attach.qty.
    END.
    */
    FIND FIRST style WHERE style.company = eb.company
                 AND style.style = bf-eb.style
                 AND style.flute = bf-eb.flute
                 AND style.test = bf-eb.test 
                 AND (style.TYPE = "p" OR style.TYPE = "R") 
        NO-LOCK NO-ERROR.
    IF NOT AVAIL style THEN
       FIND FIRST style WHERE style.company = eb.company
                 AND style.style = bf-eb.style
                 AND style.flute = ""
                 AND style.test = ""
                 AND (style.TYPE = "p" OR style.TYPE = "R") 
        NO-LOCK NO-ERROR.
    IF AVAIL style THEN DO:
       ASSIGN gotattadder = NO
              li = 0
              machAttachRec = ?.
       mach-attach-style:
       FOR EACH mach-attach-pat NO-LOCK
            WHERE mach-attach-pat.company  EQ est-op.company
              AND mach-attach-pat.m-code   EQ est-op.m-code
              AND mach-attach-pat.style EQ bf-eb.style
              AND mach-attach-pat.priority > 0
              BY mach-attach-pat.priority:
           FIND ef OF eb NO-LOCK NO-ERROR.
           FIND mach-attach OF mach-attach-pat NO-LOCK NO-ERROR.
           iDieQty = IF mach-attach.qty > 0 THEN mach-attach.qty ELSE style.dim-df.
           
           IF ef.cal >= mach-attach-pat.caliperMin AND
              ef.cal <= mach-attach-pat.caliperMax AND
              bf-eb.t-wid >= mach-attach-pat.blankwidthmin AND
              bf-eb.t-wid <= mach-attach-pat.blankwidthmax AND
              iDieQty <= mach-attach-pat.onHandDieQty  /* AND
              ((eb.k-len-array2[1] <> 0 AND 
               eb.k-len-array2[1] >= mach-attach-pat.internalCellMin AND
               eb.k-len-array2[1] <= mach-attach-pat.internalCellMax) 
               OR eb.k-len-array2[1] = 0)  */
              
           THEN DO:
                DO i = 1 TO 30:
                  IF bf-eb.k-len-array2[i] <> 0  AND
                     (bf-eb.k-len-array2[i] < mach-attach-pat.internalCellMin OR
                     bf-eb.k-len-array2[i] > mach-attach-pat.internalCellMax)
                     THEN NEXT mach-attach-style.
               END.
              ASSIGN gotAttAdder = YES.
              li = li + 1.
              IF li LE EXTENT(est-op.att-type) THEN DO:
                 IF li = 1 THEN machAttachRec = RECID(mach-attach-pat).
                 IF bf-eb.blank-no = 1 THEN
                    ASSIGN est-op.att-type[li] = mach-attach-pat.att-type
                           est-op.att-qty[li]  = iDieQty /*style.dim-df ,mach-attach-pat.qty*/.
                 ELSE DO:
                   ASSIGN est-op.att-type[li] = mach-attach-pat.att-type
                          est-op.att-qty[li]  = iDieQty /*style.dim-df , mach-attach-pat.qty*/.
                 END.
                 IF est-op.att-type[li] <> "" THEN LEAVE mach-attach-style.
              END.
           END.
            
       END.
       
       IF NOT gotAttAdder THEN
       mach-attach-nostyle:
       FOR EACH mach-attach-pat NO-LOCK
            WHERE mach-attach-pat.company  EQ est-op.company
              AND mach-attach-pat.m-code   EQ est-op.m-code
              AND mach-attach-pat.style EQ ""
              AND mach-attach-pat.priority > 0
              BY mach-attach-pat.priority  :
           FIND ef OF eb NO-LOCK NO-ERROR.
           FIND mach-attach OF mach-attach-pat NO-LOCK NO-ERROR.
           iDieQty = IF mach-attach.qty > 0 THEN mach-attach.qty ELSE style.dim-df.

           IF ef.cal >= mach-attach-pat.caliperMin AND
              ef.cal <= mach-attach-pat.caliperMax AND
              bf-eb.t-wid >= mach-attach-pat.blankwidthmin AND
              bf-eb.t-wid <= mach-attach-pat.blankwidthmax AND
              iDieQty <= mach-attach-pat.onHandDieQty /*  AND
              ((eb.k-len-array2[1] <> 0 AND 
               eb.k-len-array2[1] >= mach-attach-pat.internalCellMin AND
               eb.k-len-array2[1] <= mach-attach-pat.internalCellMax) 
               OR eb.k-len-array2[1] = 0)  */
           THEN DO: 
               DO i = 1 TO 30:
                  IF bf-eb.k-len-array2[i] <> 0  AND
                     (bf-eb.k-len-array2[i] < mach-attach-pat.internalCellMin OR
                     bf-eb.k-len-array2[i] > mach-attach-pat.internalCellMax)
                     THEN NEXT mach-attach-nostyle.
               END.
              ASSIGN gotAttAdder = YES
              li = li + 1.
              IF li LE EXTENT(est-op.att-type) THEN DO:
                 IF li = 1 THEN machAttachRec = RECID(mach-attach-pat).
                 IF bf-eb.blank-no = 1 THEN
                 ASSIGN est-op.att-type[li] = mach-attach-pat.att-type
                        est-op.att-qty[li]  = iDieQty /*style.dim-df ,mach-attach-pat.qty*/.
                 ELSE DO:
                     IF est-op.att-type[li] = mach-attach-pat.att-type THEN
                         est-op.att-qty[li] = est-op.att-qty[li] + iDieQty.
                 END.
                 IF est-op.att-type[li] <> "" THEN LEAVE.
              END.
           END.            
       END.      
       IF est-op.att-type[1] <> "" THEN DO:
          find first sys-ctrl where sys-ctrl.company eq cocode
                                and sys-ctrl.name    eq "CEPDIES"
                                no-lock no-error.
          if avail sys-ctrl AND can-do(sys-ctrl.char-fld,est-op.m-code) THEN DO:
             FIND mach-attach-pat WHERE RECID(mach-attach-pat) = machAttachRec NO-LOCK NO-ERROR.
             IF AVAIL mach-attach-pat AND bf-eb.gluelap = 0 THEN
                 bf-eb.gluelap = mach-attach-pat.slotWidth.
                 /*{/k16bb.i bf-eb.gluelap}*/
          END.
                 
       END.
    END.  /* avail style - mach-attach-pat */

    END.  /* bf-eb*/

    IF AVAIL mach THEN DO:
      RUN est/getcrusz.p (ROWID(mach), ROWID(eb), est-op.dept, "M R",
                          INPUT-OUTPUT est-op.op-crew[1]).
      RUN est/getcrusz.p (ROWID(mach), ROWID(eb), est-op.dept, "RUN",
                          INPUT-OUTPUT est-op.op-crew[2]).
    END.
  END.

  ASSIGN
   est-op.op-rate[1] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[1]) + 
                       mach.mr-varoh  + mach.mr-fixoh
   est-op.op-rate[2] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[2]) + 
                       mach.run-varoh + mach.run-fixoh.
  
  ASSIGN
      est-op.len-pos = integer(index("LWD",substr(m-lst.dim-pos,1,1))) .
      est-op.len     =  m-lst.dim[1] .
      est-op.wid-pos =  index("LWD",substr(m-lst.dim-pos,2,1)) .
      est-op.wid     =  m-lst.dim[2] .
      est-op.dep-pos =   index("LWD",substr(m-lst.dim-pos,3,1)) .
      est-op.dep     =  m-lst.dim[3].
  
  if mach.p-type eq "B" then est-op.op-sb = no.
  j = j + 1.
end.

FOR EACH xef
    WHERE xef.company EQ xest.company
      AND xef.est-no  EQ xest.est-no
    NO-LOCK:

  RUN cec/isitfoam.p (ROWID(xef), OUTPUT ll-foam).

  FOR EACH est-op
      WHERE est-op.company EQ xest.company
        AND est-op.est-no  EQ xest.est-no
        AND est-op.s-num   EQ xef.form-no
        AND est-op.qty     EQ qty
        AND est-op.line    LT 500
        AND (NOT ll-foam OR est-op.n-out EQ 0)
      BREAK BY est-op.s-num
            BY est-op.b-num
            BY est-op.dept
            BY est-op.line:
            
    IF FIRST-OF(est-op.dept) THEN j = 0.
    
    ASSIGN
     j              = j + 1
     est-op.op-pass = j.
  END.
END.

j = 0.
for each est-op
    where est-op.company eq xest.company
      and est-op.est-no  eq xest.est-no
      and est-op.qty     eq qty
      and est-op.line    lt 500
    by est-op.qty
    by est-op.s-num
    by est-op.b-num
    by est-op.d-seq
    by est-op.op-pass
    by est-op.rec_key:
  
  assign
   j           = j + 1
   est-op.line = j.
  
end.

maxco = 0.
for each xef
    where xef.company eq xest.company
      and xef.est-no  eq xest.est-no
    BREAK BY xef.company
          BY xef.est-no:
  xef.op-lock = NO.
  IF xest.est-type EQ 8 THEN
  DO:
    RUN cec/com/localk.p (0, ?,ip-build-combo).
    IF ip-build-combo AND LAST(xef.est-no) THEN
     DO:

          FIND CURRENT xest EXCLUSIVE-LOCK NO-ERROR.
             ASSIGN
               xest.recalc    = NO
               xest.recalc-mr = NO.
          FIND CURRENT xest NO-LOCK.     
     END.
  END.
  ELSE
    RUN cec/localk.p (0).
end. /* for each ef */

for each ef
    where ef.company eq xest.company
      and ef.est-no  eq xest.est-no:
  ef.op-lock = yes.
end.
find first xef
    where xef.company eq xest.company
      and xef.est-no  eq xest.est-no
    no-error.
call_id = recid(xef).

for each est-op
    where est-op.company eq xest.company
      and est-op.est-no  eq xest.est-no
      and (est-op.qty    eq qty OR xest.est-type GE 7)
    break by est-op.s-num:

  if first-of(est-op.s-num) then do:
     find first ef
         where ef.company eq est-op.company
           and ef.est-no  eq est-op.est-no
           and ef.form-no eq est-op.s-num
         no-error.
     if avail ef then
       assign
        ef.op-lock = yes
        ef.gsh-qty = est-op.num-sh.
  end.
end.

find xef where recid(xef) eq call_id no-lock no-error.

qty = xest.est-qty[1].



/* end ---------------------------------- copr. 1992  advanced software, inc. */
