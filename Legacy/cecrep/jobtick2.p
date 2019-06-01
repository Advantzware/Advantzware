/* --------------------------------------------- cec/rep/jobtick2.p 04/97 JLF */
/*  factory ticket                                                            */
/* -------------------------------------------------------------------------- */

def input parameter v-recid1 as recid.
def input parameter v-recid2 as recid.
{sys/inc/VAR.i SHARED}
{cecrep/jobtick.i "shared"}

{cec/descalc.i NEW}

def var xg-flag  as log init no no-undo.
def var v-space  as log init yes no-undo.
def var v-dec    as dec no-undo.
def var v-op-qty as int no-undo.
def var v-on-f   as dec no-undo.
DEF VAR v-len LIKE xef.gsh-len NO-UNDO.
DEF VAR v-yld-qty LIKE eb.quantityPerSet NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.
def workfile w-rec field w-recdate as date field w-rec-qty as dec.

/****************************************************************************/
PROCEDURE PR-ship:
def var v-cas-cnt like eb.cas-cnt.
def var v-oecount as   log.
def var v-rec-alf as   char extent 8.
def var v-date    as   date init ?.
def var v-qty     as   dec.
def var v         as   int.

FOR EACH w-rec:
    DELETE w-rec.
END.

find job-hdr where recid(job-hdr) eq v-recid2 no-lock no-error.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OECOUNT"
    no-lock no-error.
v-oecount = avail sys-ctrl and sys-ctrl.log-fld.

/*if lookup(v-format,"Brick,TriState,RFC") gt 0 then do:
*/
  if avail job-hdr and avail xeb then
  for each rm-rcpth
      where rm-rcpth.company   eq cocode
        and rm-rcpth.job-no    eq job-hdr.job-no
        and rm-rcpth.job-no2   eq job-hdr.job-no2
        and rm-rcpth.rita-code eq "R"
        and can-find(first item where item.company eq cocode
                                  and item.i-no    eq rm-rcpth.i-no
                                  and index("BPR1234",item.mat-type) gt 0)
      no-lock,
      
      first job-mat
      where job-mat.company eq cocode
        and job-mat.rm-i-no eq rm-rcpth.i-no
        and job-mat.job-no  eq job-hdr.job-no
        and job-mat.job-no2 eq job-hdr.job-no2
        and job-mat.frm     eq xeb.form-no
      no-lock,
      
      each rm-rdtlh
      where rm-rdtlh.r-no   eq rm-rcpth.r-no
        and (rm-rdtlh.s-num eq xeb.form-no or rm-rdtlh.s-num eq 0)
      no-lock
      
      by rm-rcpth.trans-date
      by rm-rdtlh.qty desc:
            
    if rm-rcpth.pur-uom eq "EA" then
      v-qty = rm-rdtlh.qty.
    else
      run sys/ref/convquom.p(rm-rcpth.pur-uom, "EA",
                             job-mat.basis-w, job-mat.len,
                             job-mat.wid, job-mat.dep,
                             rm-rdtlh.qty, output v-qty).
                             
    if v-qty le 0 then
      find prev w-rec no-error.
    else
      find first w-rec where w-recdate eq rm-rcpth.trans-date no-error.
    
    if not avail w-rec then do:
      create w-rec.
      w-recdate = rm-rcpth.trans-date.
    end.
                             
    w-rec-qty = w-rec-qty + v-qty.
  end.
  
  assign
   v     = 0
   v-qty = 0.
   
  for each w-rec by w-recdate:
    if w-rec-qty ne 0 then do:
      {sys/inc/roundup.i w-rec-qty}
      
      assign
       v     = v + 1
       v-qty = v-qty + w-rec-qty.
      
      if v le 3 then
        assign
         v-rec-alf[v]     = string(w-recdate,"99/99/9999")
         v-rec-alf[v + 4] = string(w-rec-qty,"->,>>>,>>>,>>>").
    end.  
  end.
  
  v-rec-alf[8] = string(v-qty,">,>>>,>>>,>>>").
  
  do v = 1 to 8:
    v-rec-alf[v] = trim(v-rec-alf[v]) + fill("_",100).
  end.
END PROCEDURE.  

/****************************************************************************/

{sys/inc/notes.i}

find first w-ef where recid(w-ef) eq v-recid1.

find first job-hdr where recid(job-hdr) eq v-recid2 no-lock.

assign
 v-form-hdr   = ""
 v-set-hdr    = ""
 v-adders     = ""
 v-sht-qty    = 0
 v-form-code  = ""
 v-form-dscr  = ""
 v-form-len   = 0
 v-form-wid   = 0
 v-joint-dscr = ""
 v-out        = 1
 v-stackcode  = ""
 v-len-score  = ""
 v-board-dscr = ""
 v-board-code = "".

if avail xest then do:
  if xest.est-type eq 6 then do:
    find first xeb
        where xeb.company = xest.company 
          AND xeb.est-no   eq xest.est-no
          and xeb.form-no eq 0
        no-lock no-error.
        
    assign
     v-form-hdr = "Form #: " + string(w-ef.frm,"99") +
                      " of " + string(xest.form-qty,"99")
     v-set-hdr  = "Set Item: " + if avail xeb then xeb.stock-no else "Unknown".
     
    release xeb. 
  end.   

  find first xef
      where xef.company = xest.company AND xef.est-no   eq xest.est-no
        and xef.form-no eq w-ef.frm
      no-lock no-error.

  if avail xef then do:
    find first item
        where item.company eq cocode
          and item.i-no    eq xef.board
        no-lock no-error.

    v-adders = xef.adder[1] + " " + xef.adder[2] + " " + xef.adder[3] + " " +
               xef.adder[4] + " " + xef.adder[5] + " " + xef.adder[6].

    assign
     v-form-code = caps(item.i-no)
     v-form-dscr = if avail item then item.i-name else ""
     v-board-code = v-form-code
     v-board-dscr = v-form-dscr
     v-form-len  = xef.gsh-len
     v-form-wid  = xef.gsh-wid.

    find first xeb
        where xeb.company = xest.company AND xeb.est-no   eq xest.est-no
          and xeb.form-no eq w-ef.frm
        no-lock no-error.
  end.

  v-inst = "".
  
  for each est-inst
      where est-inst.company = xest.company and
            est-inst.est-no    eq xest.est-no
        and (est-inst.line-no eq 0 or
             est-inst.line-no eq w-ef.frm)
        and lookup(est-inst.dept,v-exc-depts) eq 0
      no-lock,

      first dept where dept.code eq est-inst.dept no-lock
            
      by dept.fc
      by est-inst.line-no:

    if est-inst.inst[1] ne "" or
       est-inst.inst[2] ne "" or
       est-inst.inst[3] ne "" then do:
      v-inst = trim(v-inst) + "  " + est-inst.dept + ":".
      if est-inst.inst[1] ne "" then
        v-inst = trim(v-inst) + " " + est-inst.inst[1].
      if est-inst.inst[2] ne "" then
        v-inst = trim(v-inst) + " " + est-inst.inst[2].
      if est-inst.inst[3] ne "" then
        v-inst = trim(v-inst) + " " + est-inst.inst[3].
    end.
  end.
        
  assign
   v-inst = trim(v-ship + " " + v-inst)
   v-inst = v-inst + fill("_",471 - length(trim(v-inst))).

  if avail xeb then do:
    find first item
        where item.company eq cocode
          and item.i-no    eq xeb.adhesive
        no-lock no-error.
    assign
     v-outw       = if xef.n-out   ne 0 then xef.n-out   else 1
     v-outl       = if xef.n-out-l ne 0 then xef.n-out-l else 1
     v-upw        = if xeb.num-wid ne 0 then xeb.num-wid else 1
     v-upl        = if xeb.num-len ne 0 then xeb.num-len else 1
     v-joint-dscr = if avail item then item.i-name else ""
     v-joint-dscr = v-joint-dscr +
                    (if v-joint-dscr ne ""      and
                     xeb.adhesive ne "NO JOINT" and
                     xeb.tab-in ne ?            then
                       " - TAB " + string(xeb.tab-in,"IN/OUT") else "")
     v-joint-dscr = trim(v-joint-dscr).

    RUN est/ef-#out.p (ROWID(xef), OUTPUT v-out).
    v-out = xeb.num-up * v-out.

    if xeb.tr-no eq "" then v-error = yes.
    
    else
      run cec/kpallet.p(input  recid(xest),
                        output v-cas-pal,
                        output v-tr-cnt,
                        output v-numstacks,
                        output v-stackcode,
                        output v-error).

    if v-error then
      assign
       v-numstacks = 0
       v-stackcode = "".
                      
    if xeb.stack-code ne "" then v-stackcode = xeb.stack-code.
    

    if v-stackcode ne "" then v-stackcode = "Pattern:" + trim(v-stackcode).
    
    if xeb.stacks eq 0 then do:
      if xeb.tr-cas ne 0 then do:
        v-dec = xeb.cas-pal / xeb.tr-cas.
        {sys/inc/roundup.i v-dec}
        v-numstacks = v-dec.
      end.
    end.
    else v-numstacks = xeb.stacks.

    if v-numstacks ne 0 and xeb.tr-cas ne 0 then
      v-stackcode = (IF v-stackcode <> "" THEN (v-stackcode + ",") ELSE "") +
                    trim(string(v-numstacks,">>9")) + " Stack/" +
                    trim(string(xeb.tr-cas,">>9")) + " Layer".

    xg-flag = xef.xgrain eq "S" or xef.xgrain eq "B".

    if xg-flag then
      assign
       v-dec = v-upw
       v-upw = v-upl
       v-upl = v-dec.

    find first xstyle
        where xstyle.company eq xeb.company
          and xstyle.style   eq xeb.style
        no-lock no-error.
    if avail xstyle and xstyle.type ne "d" then do:
      find first box-design-hdr
          where box-design-hdr.design-no eq xstyle.design-no
          no-lock no-error.

      if avail box-design-hdr then do:
        run cec/descalc.p (recid(xest), recid(xeb)).

        v-lscore-c = trim(v-lscore-c).

        if xg-flag then do x = 1 to length(v-lscore-c):
          if substring(v-lscore-c,x,1) ne "" then
            assign
             v-len-score = v-len-score + trim(substr(v-lscore-c,x,1))
             v-space     = yes.

          else
          if v-space then
            assign
             v-len-score = v-len-score + " "
             v-space     = no.
        end.

        else
        for each box-design-line of box-design-hdr no-lock:
          find first w-box-design-line
              where w-box-design-line.line-no eq box-design-line.line-no
              no-error.
          if avail w-box-design-line then do:
            v-len-score = trim(v-len-score) + " " + 
                          trim(w-box-design-line.wscore-c).
            delete w-box-design-line.
          end.
        end.

        v-len-score = trim(v-len-score).
      end.    
    end.

    find first xxprep
        where xxprep.company eq cocode
          and xxprep.code eq xeb.die-no
        no-lock no-error.
  end.
end.

if v-standards then do:
  for each w-i:
    delete w-i.
  end.

  for each job-mat
      where job-mat.company eq cocode
        and job-mat.job     eq job-hdr.job
        and job-mat.job-no  eq job-hdr.job-no
        and job-mat.job-no2 eq job-hdr.job-no2
        and job-mat.frm     eq w-ef.frm
      no-lock,

      first item
      where item.company  eq cocode
        and item.i-no     eq job-mat.i-no
        and index("B,P,1,2,3,4",item.mat-type) > 0
      no-lock
      
      break by job-mat.i-no:

    if first(job-mat.i-no) then do:
      assign
       v-form-code = caps(item.i-no)
       v-form-dscr = item.i-name
       v-form-len  = job-mat.len
       v-form-wid  = job-mat.wid.

      if v-form-len eq 0 then v-form-len = item.s-len.
      if v-form-wid eq 0 then v-form-wid =
                            if item.r-wid ne 0 then item.r-wid else item.s-wid.
    end.
    
    v-sht-qty = v-sht-qty + job-mat.qty.
  end.

  for each job-mat
      where job-mat.company eq cocode
        and job-mat.job     eq job-hdr.job
        and job-mat.job-no  eq job-hdr.job-no
        and job-mat.job-no2 eq job-hdr.job-no2
        and job-mat.frm     eq w-ef.frm
      no-lock,

      first item
      where item.company  eq cocode
        and item.i-no     eq job-mat.i-no
        and item.mat-type eq "I"
      no-lock:

    create w-i.
    assign
     w-i.i-code = job-mat.i-no
     w-i.i-dscr = item.i-name
     w-i.i-qty  = job-mat.qty.
  end.
end.

else
if avail xest then do:
  v-op-qty = 0.
  
  for each est-op
      where est-op.company = xest.company
        AND est-op.est-no eq xest.est-no
        and est-op.s-num eq w-ef.frm
        and est-op.line  gt 500
     no-lock
     
     break by est-op.qty:
     
    if first-of(est-op.qty) then do:
      v-op-qty = est-op.qty.
      if est-op.qty ge job-hdr.qty then leave.
    end.
  end.
  
  for each est-op
      where est-op.company = xest.company
        AND est-op.est-no eq xest.est-no
        and est-op.s-num eq w-ef.frm
        and est-op.qty   eq v-op-qty
        and est-op.line  gt 500
      no-lock:
      
    v-sht-qty = est-op.num-sh.
    leave.
  end.
end.

assign
 v-dc-qty = v-sht-qty * v-out
 v-1st-dc = yes.

for each w-m:
  delete w-m.
end.

if v-standards then
for each job-mch
    where job-mch.company eq cocode
      and job-mch.job     eq job-hdr.job
      and job-mch.job-no  eq job-hdr.job-no
      and job-mch.job-no2 eq job-hdr.job-no2
      and job-mch.frm     eq w-ef.frm
    use-index seq-idx no-lock,

    first mach
    where mach.company eq cocode
      and mach.m-code  eq job-mch.m-code
    no-lock:

  create w-m.
  assign
   w-m.m-code = mach.m-code
   w-m.dseq = mach.d-seq
   w-m.dscr = mach.m-dscr
   w-m.s-hr = job-mch.mr-hr
   w-m.r-sp = job-mch.speed
   w-m.r-hr = job-mch.run-hr.

  if v-1st-dc                                           and
     (mach.dept[1] eq "DC" or mach.dept[2] eq "DC" or
      mach.dept[3] eq "DC" or mach.dept[4] eq "DC")     then
    assign
     v-dc-qty = job-mch.run-qty
     v-1st-dc = no.
end.

else
if avail xest then
for each est-op
   where est-op.company = xest.company
        AND est-op.est-no eq xest.est-no
     and est-op.s-num eq w-ef.frm
     and est-op.qty   eq v-op-qty
     and est-op.line  gt 500
   no-lock,
   
   first mach
   where mach.company eq cocode
     and mach.m-code  eq est-op.m-code
   no-lock:
   
  IF INDEX("AP",mach.p-type) GT 0 THEN
    ASSIGN
     v-on-f  = 1
     v-n-out = 1.

  ELSE DO:
    RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

    IF (mach.therm AND mach.p-type EQ "R") OR est-op.op-sb THEN
      RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f).
    ELSE
      v-on-f = xeb.num-up * v-n-out.
  END.

  create w-m.
  assign
   w-m.m-code = est-op.m-code
   w-m.dseq = est-op.d-seq
   w-m.dscr = est-op.m-dscr
   w-m.s-hr = est-op.op-mr
   w-m.r-sp = est-op.op-speed
   v-len    = IF est-op.dept EQ "LM" THEN xef.nsh-len ELSE xef.gsh-len.
   
  if est-op.op-speed ne 0 then
    assign
     w-m.r-hr = ((est-op.num-sh * v-on-f) - est-op.op-waste) *
                (if mach.therm and (mach.p-type eq "R" OR est-op.dept EQ "LM") then (v-len / 12)
                 else 1) / est-op.op-speed.

  if v-1st-dc            and
     est-op.dept eq "DC" then
    assign
     v-dc-qty = est-op.num-sh * v-out
     v-1st-dc = no.
end.

IF v-net-shts THEN 
  ASSIGN
   v-sht-qty = job-hdr.qty / v-out
   v-yld-qty = IF AVAIL xeb THEN
                 IF xeb.est-type EQ 2 THEN xeb.cust-%
                 ELSE
                 IF xeb.est-type EQ 6 THEN xeb.quantityPerSet
                 ELSE 1
               ELSE 1
   v-sht-qty = v-sht-qty *
               (IF v-yld-qty LT 0 THEN (-1 / v-yld-qty)
                ELSE v-yld-qty).

/* end ---------------------------------- copr. 1997  advanced software, inc. */
