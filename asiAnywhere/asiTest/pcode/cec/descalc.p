/* -------------------------------------------------- cec/descalc.p 11/95 BSM */
/* Box Design Calculation                                                     */
/* -------------------------------------------------------------------------- */

def input parameter v-recid1 as recid no-undo.
def input parameter v-recid2 as recid no-undo.

{sys/inc/var.i shared}

def var K_FRAC as dec init 6.25 no-undo.

{sys/inc/f16to32.i}
{cec/descalc.i}

def var v-i    as int                  no-undo.
def var v-j    as int                  no-undo.
def var v-l    as int                  no-undo.
def var v-k    as int                  no-undo.
def var v-lscore-d     as dec extent 30 format "999.99" no-undo.
def var v-lscor2-d     as dec extent 30 format "999.99" no-undo.
def var v-wscore-d     as dec extent 50 format "999.99" no-undo.
def var v-wscor2-d     as dec extent 50 format "999.99" no-undo.
def var v-lcum-score-d as dec extent 30 format "999.99" no-undo.
def var v-lcum-scor2-d as dec extent 30 format "999.99" no-undo.
def var v-wcum-score-d as dec extent 50 format "999.99" no-undo.
def var v-lscore-fld-loc as int extent 30 no-undo.
def var v-lscore-fld-id  as int extent 30 no-undo.
def var v-lscore-fld-num as int init 1 no-undo.
def var v-wscore-fld-num as int init 0 no-undo.
def var v-lscore-width as int init 6 no-undo.
def var v-lscore-ttl-width as int init 65 no-undo.
def var v-code as char no-undo.
def var v-formula as char no-undo.
def var op as char extent 40.
def var nextop as int.
def var num as dec extent 40.
def var curnum as char.
def var kar as char format "x".  /* style formula kalk vars */
def var v-in-paren as log init no no-undo.
def var v-score as dec extent 30 NO-UNDO.

def var v-sq-box  as   dec.
def var v-16-s    as   dec.

def var v-round   like sys-ctrl.log-fld.
DEF VAR lv-max-loc AS INT NO-UNDO. /* not to have error if v-lscore-fld-num >= 10 */

DEF BUFFER reftable2 FOR reftable.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "ROUND"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "ROUND"
   sys-ctrl.descrip = "Round Up Scoring Allowances?".
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
v-round = sys-ctrl.log-fld.

find est where recid(est) eq v-recid1 no-lock.

{cec/boxdesu.i}

find eb where recid(eb) eq v-recid2 NO-LOCK NO-ERROR.

IF AVAIL eb THEN
  find first ef
      where ef.company eq eb.company
        and ef.est-no  eq eb.est-no
        and ef.form-no eq eb.form-no
      no-lock no-error.

ELSE DO:
  find ef where recid(ef) eq v-recid2 no-lock.
  find first eb
      where eb.company eq ef.company
        and eb.est-no  eq ef.est-no
        and eb.form-no eq ef.form-no
      no-lock no-error.
END.

if avail eb then
find first item
    where (item.company = cocode and
	 index("GTS",item.mat-type) > 0 )
      and item.i-no eq eb.adhesive
    no-lock no-error.
if avail eb then
find first style
    where style.company eq eb.company
      and style.style   eq eb.style
    no-lock no-error.
if avail style then
find box-design-hdr
    where box-design-hdr.company = style.company
      AND box-design-hdr.design-no eq style.design-no
    no-lock no-error.
if avail eb                           and
   eb.len eq eb.wid                   and 
   ( program-name(2) matches "*b-estitm*" or
     program-name(2) matches "*v-est*")
then do:
    
  find first reftable
      where reftable.reftable eq "STYFLU"
        and reftable.company  eq eb.style
        and reftable.loc      eq eb.flute
        and reftable.code     eq "DIM-FIT"
      no-lock no-error.
  if avail reftable then v-sq-box = reftable.val[1] / 6.25 * k_frac.
end.

if avail box-design-hdr then do:
  assign
   v-lscore-c     = box-design-hdr.lscore
   v-lcum-score-c = "".
  select count(*) into v-num-lines from box-design-line
      where box-design-line.design-no = box-design-hdr.design-no.
  v-num-lines = v-num-lines + 4.
end.

else do:
  v-lscore-c = "No Design exists for this Estimate/Form.".
  return.
end.

for each w-box-design-line:
  delete w-box-design-line.
end.

v-lscore-ttl-width = LENGTH(v-lscore-c).

/** find field locations in length Score var **/
do i = 1 to v-lscore-ttl-width:
  if substr(v-lscore-c, i, 1) eq "[" then
    assign
     v-lscore-fld-loc[v-lscore-fld-num] = i
     v-lscore-fld-id[v-lscore-fld-num]  = if substring(v-lscore-c,i + 1,1) = "" then int(substr(v-lscore-c, i + 2, 2))
                                          else int(substr(v-lscore-c, i + 1, 2)) 
                                    /* YSK before [ 02 ] (i + 2) -> now before + [02] (i + 1) */
     v-lscore-fld-num = v-lscore-fld-num + 1.
end.
v-lscore-fld-num = v-lscore-fld-num - 1.

i = 0.
for each box-design-line of box-design-hdr no-lock:
  if box-design-line.wcum-score ne "" then do:
    i = i + 1.
    create w-box-design-line.
    assign
     w-box-design-line.line-no       = box-design-line.line-no
     w-box-design-line.wscore-c      = box-design-line.wscore
     w-box-design-line.wcum-score-c  = box-design-line.wcum-score
     w-box-design-line.wscore-fld-id = if substring(trim(box-design-line.wscore),2,1) = "" then int(substr(trim(box-design-line.wscore), 3, 2))
                                       else int(substr(trim(box-design-line.wscore), 2, 2))   /* ysk not need space */
     v-wscore-fld-num                = v-wscore-fld-num + 1.

  end.
end.

do v-j = 1 to 2:
  v-score = 0.

  /**************************************************************
   Find Length Scores
   **************************************************************/
  if v-j eq 1 then do:
    release reftable.
    v-code = "7".
    if avail item then
      if item.mat-type eq "G" then v-code = if eb.tab-in then "3" else "4".
      else
      if item.mat-type eq "S" then v-code = if eb.tab-in then "5" else "6".

    find first reftable
        where reftable.reftable eq "STYSCORE"
          and reftable.company  eq eb.style
          and reftable.loc      eq eb.flute
          and reftable.code     eq v-code
          AND reftable.code2    EQ ""
        no-lock no-error.

    if avail reftable then
    do i = 1 to EXTENT(lv-k-len-scr-type):
      lv-k-len-scr-type[i] = substr(reftable.dscr,i,1).
    end.

    find first reftable
        where reftable.reftable eq "STYFLU"
          and reftable.company  eq eb.style
          and reftable.loc      eq eb.flute
          and reftable.code     eq v-code
          AND reftable.code2    EQ ""
        no-lock no-error.

    find first reftable2
        where reftable2.reftable eq "STYFLU"
          and reftable2.company  eq eb.style
          and reftable2.loc      eq eb.flute
          and reftable2.code     eq v-code
          AND reftable2.code2    EQ "1"
        no-lock no-error.

    v-formula = style.formula[2].
  end.

  else do:
  /**************************************************************
   Find Width Scores
   **************************************************************/

    find first reftable
        where reftable.reftable eq "STYSCORE"
          and reftable.company  eq eb.style
          and reftable.loc      eq eb.flute
          and reftable.code     eq "2"
          AND reftable.code2    EQ ""
        no-lock no-error.

    if avail reftable then
    do i = 1 to EXTENT(lv-k-wid-scr-type):
      lv-k-wid-scr-type[i] = substr(reftable.dscr,i,1).
    end.

    find first reftable
        where reftable.reftable eq "STYFLU"
          and reftable.company  eq eb.style
          and reftable.loc      eq eb.flute
          and reftable.code     eq "2"
        no-lock no-error.

    find first reftable2
        where reftable2.reftable eq "STYFLU"
          and reftable2.company  eq eb.style
          and reftable2.loc      eq eb.flute
          and reftable2.code     eq "2"
          AND reftable2.code2    EQ "1"
        no-lock no-error.
    v-formula = style.formula[1].
  end.

  if avail reftable then
  do i = 1 to 12: /* must be 12, total is in 13 */
    v-score[i] = reftable.val[i].
  end.

  if avail reftable2 then
  do i = 1 to 8: /* must be 8 (12 + 8 = 20) */
    v-score[12 + i] = reftable2.val[i].
  end.

  /**************************************************************
   Build Dimension Formula
   **************************************************************/
  /* get rid of any blank/space or invalid char */
  do i = 1 to length(v-formula):
   if substr(v-formula, i, 1) eq ""                                   or
      index("0123456789.+-LWDTFJBOSI()",substr(v-formula, i ,1)) eq 0 then
     v-formula = substr(v-formula, 1, i - 1) + substr(v-formula, i + 1).
  end.

  assign
   num    = 0     /* zero arrays */
   op     = ""
   nextop = 1.

  loop:
  do i = 1 to length(v-formula):
    kar = substr(v-formula, i, 1).
    if index("+-()",kar) gt 0 then do:
      op[nextop] = kar.
    end.

    else
    if index("LWDJTSFBOI",kar) gt 0 then do:
      if kar eq "L" then num[nextop] = eb.len.
      if kar eq "W" then num[nextop] = eb.wid.
      if kar eq "D" then num[nextop] = eb.dep.
      if kar eq "J" then num[nextop] = eb.gluelap.
      if kar eq "T" then num[nextop] = eb.tuck.
      if kar eq "F" then num[nextop] = eb.dust.
      if kar eq "B" then num[nextop] = eb.fpanel.
      if kar eq "O" then num[nextop] = eb.lock.
      if kar eq "I" then num[nextop] = style.dim-fit.
      if nextop gt 1 and num[nextop - 1] ne 0 then
        assign
         num[nextop - 1] = num[nextop - 1] * num[nextop]
         num[nextop]     = 0
         nextop          = nextop - 1.
    end.
    else do:
      curnum = "".
      do while (keycode(kar) ge 48 and keycode(kar) le 57) or
               keycode(kar) eq 46:
        assign
         curnum = curnum + kar
         i      = i + 1
         kar    = substr(v-formula,i,1).
      end.
      assign
       i           = i - 1
       num[nextop] = dec(curnum).
      if nextop gt 1 and num[nextop - 1] ne 0 then
        assign
         num[nextop - 1] = num[nextop - 1] * num[nextop]
         num[nextop]     = 0
         nextop          = nextop - 1.
    end.
    nextop = nextop + 1.
  end.

  /**************************************************************
   Calculate Length Dimensions
  **************************************************************/
  if v-j eq 1 then do:
    assign
     v-in-paren = no
     v-i        = 1.
    do i = 1 to 40:
      if op[i] ne "" then do:
        if op[i] eq "(" then v-in-paren = yes.
        if op[i] eq ")" then v-in-paren = no.
        if op[i] eq "+" and not v-in-paren then v-i = v-i + 1.
      end.
      else
      if num[i] ne 0 then do:
        if v-in-paren and v-lscore-d[v-i] ne 0 then do:
          if op[i - 1] eq "+" then v-lscore-d[v-i] = v-lscore-d[v-i] + num[i].
          if op[i - 1] eq "-" then v-lscore-d[v-i] = v-lscore-d[v-i] - num[i].
        end.
        else v-lscore-d[v-i] = num[i].
      end.
    end.
    do i = 1 to extent(eb.k-len-array2):
      if eb.k-len-array2[i] eq 0 then leave.

      if i eq 1 then
        assign
         v-lscore-d = 0
         v-score    = 0.

      assign
       v-lscore-fld-num   = i
       v-lscore-d[i]      = eb.k-len-array2[i]
       v-lscore-fld-id[i] = i.
    end.

    if v-lscore-fld-num + 1 ne v-i then do:
      assign
       v-i              = v-lscore-fld-num + 1
       v-lscore-c       = ""
       v-lscore-fld-loc = 0.

      lv-max-loc = IF v-lscore-fld-num >= 20 THEN 210
                   ELSE IF v-lscore-fld-num >= 12 THEN 137  
                   ELSE IF v-lscore-fld-num >= 10 THEN 83
                   ELSE 65.

      do i = 1 to 30:      /*  was ((  ( 65 - ((60))  ) / 9 ) * 1)  error if v-lscore-fld-num >= 10 */
        v-lscore-fld-loc[i] = round(((lv-max-loc - ((1 + v-lscore-fld-num) * 6)) / v-lscore-fld-num) * i,0) 
                              + ((i - 1) * 6).
      end.
    end.
    do i = 1 to v-lscore-fld-num:
      assign
       v-lscore-d[i] = v-lscore-d[i] + {sys/inc/k16bv.i v-score[i]}
       v-lscore-d[i] = v-lscore-d[i] * li-16-32.

      if v-round then do:
        {sys/inc/roundup.i v-lscore-d[i]}
      end.
      else
        v-lscore-d[i] = truncate(v-lscore-d[i],0).

      assign
       v-lscore-d[i]     = v-lscore-d[i] / li-16-32
       v-lcum-score-d[i] = (if i gt 1 then v-lcum-score-d[i - 1] else 0) +
                            v-lscore-d[i].

    end.
    do i = 1 to v-lscore-fld-num:
      do j = 1 to v-lscore-fld-num:
        if v-lscore-fld-id[j] eq i then do:
          assign
           v-lscor2-d[i]     = v-lscore-d[j]
           v-lcum-scor2-d[i] = v-lcum-score-d[j].
        end.
      end.
    end.
    do i = 1 to v-lscore-fld-num:
      if v-box-uom eq "Inches"                      or
         (v-box-uom eq "Both" and (not est.metric)) then
        assign
         v-lscore-d[i]     = {sys/inc/k16v.i v-lscor2-d[i]}
         v-lcum-score-d[i] = {sys/inc/k16v.i v-lcum-scor2-d[i]}.
      else
        assign
         v-lscore-d[i]     = round(v-lscor2-d[i]     * 25.4,0)
         v-lcum-score-d[i] = round(v-lcum-scor2-d[i] * 25.4,0).        

    end.

    do i = 1 to v-lscore-fld-num:
      if i gt v-i then
        assign
         overlay(v-lscore-c,    v-lscore-fld-loc[i],6) = fill(" ",6)
         overlay(v-lcum-score-c,v-lscore-fld-loc[i],6) = fill(" ",6).
      else
        assign
         overlay(v-lscore-c,    v-lscore-fld-loc[i],6) =
                                             string(v-lscore-d[i],v-sc-fmt)
         overlay(v-lcum-score-c,v-lscore-fld-loc[i],6) =
                                             string(v-lcum-score-d[i],v-sc-fmt).
    end.    
  end.
  /**************************************************************
   Calculate Width Dimensions
  **************************************************************/
  else do:
    v-in-paren = no.
    v-i = 1.
    do i = 1 to 40:
      if op[i] ne "" then do:
        if op[i] eq "(" then v-in-paren = yes.
        if op[i] eq ")" then v-in-paren = no.
        if op[i] eq "+" and not v-in-paren then v-i = v-i + 1.
      end.
      else
      if num[i] ne 0 then do:
        if v-in-paren and v-wscore-d[v-i] ne 0 then do:
          if op[i - 1] eq "+" then v-wscore-d[v-i] = v-wscore-d[v-i] + num[i].
          if op[i - 1] eq "-" then v-wscore-d[v-i] = v-wscore-d[v-i] - num[i].
        end.
        else v-wscore-d[v-i] = num[i].
      end.
    end.

    do i = 1 to extent(eb.k-wid-array2):
      if eb.k-wid-array2[i] eq 0 then leave.

      if i eq 1 then
        assign
         v-wscore-d = 0
         v-score    = 0.

      assign
       v-wscore-fld-num = i
       v-wscore-d[i]    = eb.k-wid-array2[i].
    end.
    v-wscore-d[1] = v-wscore-d[1] - v-sq-box.

    do i = 30 to 1 by -1:
      if v-wscore-d[i] ne 0 then do:
        v-wscore-d[i] = v-wscore-d[i] - v-sq-box.
        leave.
      end.
    end.
    if v-wscore-fld-num + 1 ne v-i then do:
      v-i = v-wscore-fld-num + 1.
      find last box-design-line of box-design-hdr no-lock.
      do i = 1 to 30:
        find first w-box-design-line
            where w-box-design-line.wscore-fld-id eq i
            no-error.
        if not avail w-box-design-line then do:
          create w-box-design-line.
          w-box-design-line.wscore-fld-id = i.
        end.
        w-box-design-line.line-no =
                           round((box-design-line.line-no - 1) / v-i * i,0) + 1.
      end.
    end.

    do i = 1 to v-wscore-fld-num:
      assign
       v-wscore-d[i] = {sys/inc/k16v.i  v-wscore-d[i]}
       v-wscore-d[i] = {sys/inc/k16bv.i v-wscore-d[i]}
       v-wscore-d[i] = v-wscore-d[i] + ({sys/inc/k16bv.i v-score[i]})
       v-wscore-d[i] = v-wscore-d[i] * li-16-32.
      
      if v-round then do:
        {sys/inc/roundup.i v-wscore-d[i]}
      end.
      else
        v-wscore-d[i] = trunc(v-wscore-d[i],0).
        
      assign
       v-wscore-d[i]     = v-wscore-d[i] / li-16-32
       v-wcum-score-d[i] = (if i gt 1 then v-wcum-score-d[i - 1] else 0) +
                            v-wscore-d[i].

      find first w-box-design-line
          where w-box-design-line.wscore-fld-id eq i
          no-error.
      if avail w-box-design-line then
        if v-box-uom eq "Inches"                      or
           (v-box-uom eq "Both" and (not est.metric)) then
          assign
           w-box-design-line.wscore-d     = {sys/inc/k16v.i v-wscore-d[i]}
           w-box-design-line.wcum-score-d = {sys/inc/k16v.i v-wcum-score-d[i]}.
        else
          assign
           w-box-design-line.wscore-d     = round(v-wscore-d[i]     * 25.4,0)
           w-box-design-line.wcum-score-d = round(v-wcum-score-d[i] * 25.4,0).
    end.

    find first w-box-design-line no-error.
    if avail w-box-design-line then
    do i = 1 to v-wscore-fld-num:
      if i gt v-i then
        assign
         w-box-design-line.wscore-c     = ""
         w-box-design-line.wcum-score-c = "".
      else
        assign
         w-box-design-line.wscore-c     =
                                string(w-box-design-line.wscore-d,v-sc-fmt)
         w-box-design-line.wcum-score-c =
                                string(w-box-design-line.wcum-score-d,v-sc-fmt).

      find next w-box-design-line no-error.
      if not avail w-box-design-line then leave.
    end.
  end.
end.

/* end ---------------------------------- copr. 1995  advanced software, inc. */
