/* ------------------------------------------------- cec/mach-sq1.p 08/96 JLF */
/* create machine routing sequence                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def var save_id as recid.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

DEF BUFFER bf-eb FOR eb.

def shared var qty     as int no-undo.
def shared var maxco   as int no-undo.
def shared var v-2     as logical init no.
def shared var xcal    as de no-undo.
def shared var sh-wid  as de no-undo.
def shared var sh-len  as de no-undo.
def shared var ll-corr as log no-undo.

{ce/mach-lst.i}

def var vn-out as int no-undo.
def var v-outw like xef.n-out no-undo.
def var v-outl like xef.n-out-l no-undo.
def var v-on-f as int no-undo.
def var sh-tmp like sh-len no-undo.
def var v-widp as log no-undo.
def var v-1stp as log no-undo.
def var v-run as dec no-undo.
def var v-rc-seq as int init 99 no-undo.
def var v-dept like dept.code no-undo.
DEF VAR ll-foam AS LOG NO-UNDO.


&SCOPED-DEFINE where-machine                                                     ~
               WHERE mach.company EQ cocode                                  ~
                 AND mach.obsolete EQ YES                                       



{est/d-machex.i}

/*{sys/inc/cepanel.i} - Deprecated with 17756*/

find first dept where dept.code eq "RC" no-lock no-error.
if avail dept then v-rc-seq = dept.fc.

find first style
    {sys/ref/styleW.i}
      and style.style eq xeb.style
    no-lock no-error.

RUN cec/isitfoam.p (ROWID(xef), OUTPUT ll-foam).

IF NOT ll-foam AND NOT xeb.pur-man THEN DO: /* 1 */

/* laminating */
if xef.medium ne "" /*or xef.lam-code ne ""*/ then lamin:
do on error undo:
   if xef.trim-pen ne 0 then do:
      xcal = xef.trim-pen / 1000. /* trim-pen stored as integer */
      if xef.cal ne 0 and xef.cal ne .014 then xcal = xcal - .014 + xef.cal.
   end.
   else xcal = xef.cal.
   /* find laminator entered in style file, if any */
   do i = 1 to 7:
      find first mach {&where-machine}            and
                 mach.m-code  = style.m-code[i] no-lock no-error.
      if avail mach and (mach.dept[1] ne "LM") and
                            (mach.dept[2] ne "LM") and
                            (mach.dept[3] ne "LM") and
                            (mach.dept[4] ne "LM")
      then release mach.
      {cec/mach-seq.i sh-len sh-wid xcal}
   end.

   if not avail mach then
   for each mach
       {&where-machine}
         and mach.dept[1] eq "LM"
       no-lock:
      {cec/mach-seq.i sh-len sh-wid xcal}
   end.
   if avail mach then do:
      create m-lst.
      assign
       m-lst.f-no   = xef.form-no
       m-lst.seq    = 10 * mach.d-seq
       m-lst.dept   = "LM"
       m-lst.bl     = no
       m-lst.m-code = mach.m-code.
   end.
end.

/* get guillotine (rc dept) */
IF xef.n-cuts GT 0                                                 AND
   (NOT ll-corr OR xef.n-out-l GT 1 OR xef.gsh-len GT xef.nsh-len) THEN DO:
  k = (if ll-corr then 0 else int(xef.n-out gt 1)) + int(xef.n-out-l gt 1).

  assign
   j      = 0
   vn-out = 0
   v-1stp = yes
   v-widp = (xef.n-out le xef.n-out-l or xef.n-out-l le 1) and not ll-corr
   v-on-f = 1.

  if v-widp then
    assign
     sh-len = xef.gsh-wid
     v-outl = xef.n-out
     sh-wid = xef.gsh-len
     v-outw = xef.n-out-l.
  else
    assign
     sh-len = xef.gsh-len
     v-outl = xef.n-out-l
     sh-wid = xef.gsh-wid
     v-outw = xef.n-out.

  reamcut:
  do while true:
    /* find ream cutter for # on width entered in style file, if any */
    if avail mach then release mach.
    do i = 1 to 7:
      if style.m-code[i] = "" then next.
      find first mach
          {&where-machine}
            and mach.m-code eq style.m-code[i]
          no-lock no-error.
      if avail mach and (mach.dept[1] ne "RC") and
                        (mach.dept[2] ne "RC") and
                        (mach.dept[3] ne "RC") and
                        (mach.dept[4] ne "RC") then release mach.
      RUN cec/rc-mach.p (BUFFER mach, v-on-f, NO).
      IF AVAIL mach THEN LEAVE.
    end.

    /* find layout machine in mach file */
    if not avail mach then do:
      find first mach {&where-machine} and mach.m-code = xef.m-code
      no-lock no-error.
      if avail mach and mach.dept[1] ne "RC" then release mach.
    end.

    if not avail mach then
      if v-widp then
      for each mach
          {&where-machine}
            and mach.dept[1] eq "RC"
          no-lock by mach.num-wid desc
                  by mach.d-seq
                  by mach.m-seq:
         RUN cec/rc-mach.p (BUFFER mach, v-on-f, NO).
         IF AVAIL mach THEN LEAVE.
      end.

      else
      for each mach
         {&where-machine}
           and mach.dept[1] eq "RC"
         no-lock by mach.num-len desc
                 by mach.d-seq
                 by mach.m-seq:
        RUN cec/rc-mach.p (BUFFER mach, v-on-f, NO).
        IF AVAIL mach THEN LEAVE.
      end.

    if not avail mach then leave reamcut.

    if avail mach and mach.dept[1] ne "PR" then do:
      create m-lst.
      assign
       m-lst.f-no    = xef.form-no
       m-lst.seq     = 10 * mach.d-seq
       m-lst.dept    = "RC"
       m-lst.bl      = no
       m-lst.m-code  = mach.m-code
       j             = j + 1
       m-lst.pass-no = j
       m-lst.n-out   = minimum(v-outl,if v-widp then mach.num-wid
                                                else mach.num-len)
       vn-out        = vn-out + m-lst.n-out
       v-outl        = v-outl - m-lst.n-out.
    end.

    if v-outl lt 1 then
      if not v-1stp                        or
         (v-widp and xef.n-out-l lt 2)     or
         ((not v-widp) and xef.n-out lt 2) or
         ll-corr                           then leave reamcut.

      else do:
        assign
         v-1stp = no
         v-widp = not v-widp.

        if v-widp then
          assign
           sh-wid = xef.gsh-len / xef.n-out-l
           v-outw = xef.n-out-l
           sh-len = xef.gsh-wid
           v-outl = xef.n-out
           v-on-f = xef.n-out-l.
        else
          assign
           sh-wid = xef.gsh-wid / xef.n-out
           v-outw = xef.n-out
           sh-len = xef.gsh-len
           v-outl = xef.n-out-l
           v-on-f = xef.n-out.
      end.

    else if avail m-lst then sh-len = sh-len - (sh-wid * m-lst.n-out).
  end.

  if xef.lam-dscr = "R" then
    assign
     sh-wid = xef.nsh-wid
     sh-len = xef.nsh-len.
  else
    assign
     sh-wid = xef.nsh-len
     sh-len = xef.nsh-wid.
end.

END.  /* NOT ll-foam AND NOT xeb.pur-man 1 */

/* get die cutter */
IF NOT xeb.pur-man                                                 AND
   (NOT ll-foam         OR
    (xeb.num-up GT 1 AND
     NOT CAN-FIND(FIRST ef-nsh OF xef WHERE ef-nsh.dept EQ "DC"))) THEN diecut:
do on error undo:
   IF ll-foam THEN
   FOR EACH ef-nsh OF xef NO-LOCK BREAK BY ef-nsh.sheet-no BY ef-nsh.pass-no:
     IF FIRST(ef-nsh.sheet-no) THEN
       ASSIGN
        sh-len = 0
        sh-wid = 0
        xcal   = 0.

     IF LAST-OF(ef-nsh.sheet-no) THEN DO:
       IF ef-nsh.wid-out GT sh-len THEN sh-len = ef-nsh.wid-out.
       IF ef-nsh.len-out GT sh-wid THEN sh-wid = ef-nsh.len-out.
       IF ef-nsh.dep-out GT xcal   THEN xcal   = ef-nsh.dep-out.
     END.
   END.

   /* find die cutter entered in style file, if any */
   if avail mach then release mach.
   do i = 1 to 7:
      if style.m-code[i] = "" then next.
      find first mach {&where-machine}            and
                 mach.m-code  = style.m-code[i] no-lock no-error.
      if avail mach and mach.dept[1] ne "DC" and
                        mach.dept[2] ne "DC" and
                        mach.dept[3] ne "DC" and
                        mach.dept[4] ne "DC" then release mach.
      {cec/mach-seq.i sh-len sh-wid xcal}
   end.
   if not avail mach then
   for each mach
       {&where-machine}
         and mach.dept[1] eq "DC"
       no-lock:
      {cec/mach-seq.i sh-len sh-wid xcal}
   end.
   if avail mach then do:
      create m-lst.
      assign m-lst.f-no    = xef.form-no
             m-lst.seq     = 10 * mach.d-seq
             m-lst.dept    = "DC"
             m-lst.bl      = no
             m-lst.m-code  = mach.m-code
             m-lst.dim-pos = "WLD"
             m-lst.dim[1]  = sh-wid
             m-lst.dim[2]  = sh-len
             m-lst.dim[3]  = xcal.
   end.
end.

IF NOT ll-foam AND NOT xeb.pur-man THEN DO:  /* NOT ll-foam 2 */

/* get auto strip */
hstrip:
do on error undo:
   if avail mach then release mach.
   do i = 1 to 7:
      if style.m-code[i] = "" then next.
      find first mach {&where-machine} and
                 mach.m-code  = style.m-code[i] no-lock no-error.
      if avail mach and (mach.dept[1] ne "HS" ) and
                            (mach.dept[2] ne "HS" ) and
                            (mach.dept[3] ne "HS" ) and
                            (mach.dept[4] ne "HS" )
      then release mach.
      {cec/mach-seq.i sh-len sh-wid xcal}
   end.
   if not avail mach then
   for each mach
       {&where-machine}
         and mach.dept[1] eq "HS"
       no-lock:
      {cec/mach-seq.i sh-len sh-wid xcal}
   end.
   if avail mach then do:
      create m-lst.
      assign
             m-lst.f-no   = xef.form-no
             m-lst.seq    = 10 * mach.d-seq
             m-lst.dept   = "HS"
             m-lst.bl     = no
             m-lst.m-code = mach.m-code.
   end.
end.

/* get window */
windo:
do on error undo:
   if avail mach then release mach.
   do i = 1 to 7:
      if style.m-code[i] = "" then next.
      find first mach {&where-machine} and
                            mach.m-code  = style.m-code[i] no-lock no-error.
      if avail mach and not (mach.dept[1] = "WN" or mach.dept[2] = "WN" or
                            mach.dept[3] = "WN" or mach.dept[4] = "WN")
      then release mach.
     {cec/mach-seq.i xeb.t-wid xeb.t-len xcal}
   end.
   do i = 1 to 4:
      if xef.leaf[i] ne "" then do:
         find first item {sys/look/itemlwW.i} and
                    item.i-no = xef.leaf[i] no-lock no-error.
         if avail item then find first e-item of item no-lock no-error.
         if item.mat-type = "W" and not avail mach then
         for each mach
             {&where-machine}
               and mach.dept[1] eq "WN"
             no-lock:
            {cec/mach-seq.i xeb.t-wid xeb.t-len xcal}
         end.
         if avail mach and item.mat-type = "W" then do:
            create m-lst.
            assign
               m-lst.f-no = xef.form-no
               m-lst.seq = (10 * mach.d-seq) + i
               m-lst.bl  = no
               m-lst.dept   = "WN"
               m-lst.m-code = mach.m-code
               m-lst.dscr = mach.m-dscr.
            if mach.p-type = "B" then m-lst.bl = yes.
            if m-lst.bl then m-lst.b-no = xeb.blank-no.
         end.
      end.
   end. /* 1 to 2 */
end.

END. /* NOT ll-foam AND NOT xeb.pur-man 2 */

/* end ---------------------------------- copr. 1992  advanced software, inc. */
