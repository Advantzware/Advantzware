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

&SCOPED-DEFINE where-machine                                                     ~
               WHERE (mach.company EQ cocode                                  ~
                 AND  NOT CAN-FIND(FIRST reftable                             ~
                                   WHERE reftable.reftable EQ "mach.obsolete" ~
                                     AND reftable.company  EQ mach.company    ~
                                     AND reftable.loc      EQ mach.loc        ~
                                     AND reftable.code     EQ mach.m-code     ~
                                     AND reftable.val[1]   EQ 1)) 

{est/d-machex.i}

/*{sys/inc/cepanel.i} - Deprecated with 17756*/

find first dept where dept.code eq "RC" no-lock no-error.
if avail dept then v-rc-seq = dept.fc.

find first style
    {sys/ref/styleW.i}
      and style.style eq xeb.style
    no-lock no-error.

/* get window or foil */
  window-foil:
  for each est-flm
      where est-flm.company = xest.company
        AND est-flm.est-no eq xest.est-no
        and est-flm.snum  eq xef.form-no
      no-lock,
      
      first item
      {sys/look/itemlwW.i}
        and item.i-no eq est-flm.i-no
      no-lock:
      
    release mach.
  
    v-dept = if item.mat-type eq "W" then
               if est-flm.bnum eq 0 then "WS" else "WN"
             else
             if item.mat-type eq "F" then
               if est-flm.bnum eq 0 then "FS" else "FB"
             else "".
               
    if v-dept eq "" then next.         
    
    FIND FIRST bf-eb
        WHERE bf-eb.company   EQ est-flm.company
          AND bf-eb.est-no    EQ est-flm.est-no
          AND bf-eb.form-no   EQ est-flm.snum
          AND (bf-eb.blank-no EQ est-flm.bnum or est-flm.bnum eq 0)
          AND bf-eb.pur-man   EQ NO
        NO-LOCK NO-ERROR.
          
    release style.
    
    if avail bf-eb then
    find first style
        {sys/ref/styleW.i}
          and style.style eq bf-eb.style
        no-lock no-error.

    if avail style then
    do i = 1 to 7:
      if style.m-code[i] eq "" then next.
    
      find first mach
          {&where-machine}
          and mach.m-code eq style.m-code[i]
        no-lock no-error.
      if avail mach and
         not (mach.dept[1] eq v-dept or mach.dept[2] eq v-dept  or
              mach.dept[3] eq v-dept or mach.dept[4] eq v-dept) then do:
        release mach.
        next.
      end.
      IF mach.p-type EQ "B" THEN DO:
        {cec/mach-seq.i xeb.t-wid xeb.t-len xcal}
      END.
      ELSE DO:
        {cec/mach-seq.i sh-len sh-wid xcal}
      END.
    end.

    if not avail mach then
    for each mach
        {&where-machine}
          and mach.dept[1] eq v-dept
        no-lock:
      IF mach.p-type EQ "B" THEN DO:
        {cec/mach-seq.i xeb.t-wid xeb.t-len xcal}
      END.
      ELSE DO:
        {cec/mach-seq.i sh-len sh-wid xcal}
      END.
    end.
      
    if avail mach then do:
      create m-lst.
      assign
       m-lst.f-no   = xef.form-no
       m-lst.b-no   = est-flm.bnum
       m-lst.seq    = (10 * mach.d-seq) + i
       m-lst.dept   = v-dept
       m-lst.m-code = mach.m-code
       m-lst.dscr   = mach.m-dscr
       m-lst.bl     = est-flm.bnum gt 0.
    end.
  end.

/* get foil */
foyle:
do on error undo:
   if avail mach then release mach.
   do i = 1 to 7:
      if style.m-code[i] = "" then next.
      find first mach {&where-machine} and
                            mach.m-code  = style.m-code[i] no-lock no-error.
      if avail mach and not (mach.dept[1] = "FS" or mach.dept[2] = "FS" or
                             mach.dept[3] = "FS" or mach.dept[4] = "FS")
      then release mach.
      {cec/mach-seq.i xeb.t-wid xeb.t-len xcal}
   end.
   do i = 1 to 4:
      if xef.leaf[i] ne "" then
      do:
         find first item {sys/look/itemlwW.i} and
                    item.i-no = xef.leaf[i] no-lock no-error.
         v-dept = "".
         if avail item THEN DO:
            find first e-item where e-item.company = item.company and
                                  /*  e-item.loc     = item.loc     and */
                                    e-item.i-no    = item.i-no no-lock no-error.

           v-dept = if item.mat-type eq "W" then
                      if xef.leaf-bnum[i] eq 0 then "WS" else "WN"
                    else
                    if item.mat-type eq "F" then
                      if xef.leaf-bnum[i] eq 0 then "FS" else "FB"
                    else "".
         END.
               
         if v-dept eq "" then next.

         for each mach
             {&where-machine}
               and mach.dept[1] eq v-dept
             no-lock:
            IF mach.p-type EQ "B" THEN DO:
              {cec/mach-seq.i xeb.t-wid xeb.t-len xcal}
            END.
            ELSE DO:
              {cec/mach-seq.i sh-len sh-wid xcal}
            END.
         end.
         if avail mach then do:
            create m-lst.
            assign
               m-lst.f-no = xef.form-no
               m-lst.seq = (10 * mach.d-seq) + i
               m-lst.bl  = no
               m-lst.dept   = v-dept
               m-lst.m-code = mach.m-code
               m-lst.dscr = mach.m-dscr.
            if mach.p-type = "B" then m-lst.bl = yes.
            if m-lst.bl then m-lst.b-no = xeb.blank-no.
         end.
      end.
   end. /* 1 to 4 */
end.

/* get gluer */
glu:
do on error undo:
  FOR EACH xeb
      WHERE xeb.company EQ xest.company
        AND xeb.est-no  EQ xest.est-no
        AND xeb.form-no EQ xef.form-no
        AND xeb.pur-man EQ NO
      NO-LOCK:
   if (xeb.adhesive ne "" and xeb.adhesive ne "NO JOINT") or
      xeb.gluelap  ne 0  then
   do:
      find first style {sys/ref/styleW.i} and
        style.style   = xeb.style no-lock no-error.
      if avail mach then release mach.
      do i = 1 to 7:
         if style.m-code[i] = "" then next.
         find first mach {&where-machine} and
                    mach.m-code  = style.m-code[i] no-lock no-error.
         if avail mach then do j = 1 to 4:
            if mach.dept[j] = "GL" then leave.
            if j = 4 then release mach.
         end.
         {cec/mach-seq.i xeb.t-wid xeb.t-len xcal}
      end.
      if not avail mach then
      for each mach
          {&where-machine}
            and mach.dept[1] eq "GL"
          no-lock:
         {cec/mach-seq.i xeb.t-wid xeb.t-len xcal}
      end.
      if avail mach then do:
         create m-lst.
         assign
             m-lst.f-no = xef.form-no
             m-lst.seq = 10 * mach.d-seq
             m-lst.bl  = no
             m-lst.dept   = "GL"
             m-lst.m-code = mach.m-code
             m-lst.dscr = mach.m-dscr.
         if mach.p-type = "B" then m-lst.bl = yes.
         if m-lst.bl then m-lst.b-no = xeb.blank-no.
      end.
   end.
  end.
end.

FOR EACH xeb
    WHERE xeb.company EQ xest.company
      AND xeb.est-no  EQ xest.est-no
      AND xeb.form-no EQ xef.form-no
      AND xeb.pur-man EQ NO
    no-lock,

    first style
    where style.company eq xeb.company
      and style.style   eq xeb.style
    no-lock:

/* get any other mach in style file */
sty:
do on error undo:
   if avail mach then release mach.
   do i = 1 to 7:
      if style.m-code[i] = "" then next.
      find first m-lst where m-lst.m-code eq style.m-code[i]
                         and m-lst.f-no   eq xef.form-no no-lock no-error.
      if avail m-lst then do:
         if avail mach then release mach. next. end.
      else
      for each mach
          {&where-machine}
            and mach.m-code eq style.m-code[i]
          no-lock:
         if mach.p-type eq "B" then do:
           {cec/mach-seq.i xeb.t-wid xeb.t-len xcal}
         end.
         else do:
           {cec/mach-seq.i sh-len sh-wid xcal}
         end.
      end.
      if avail mach then do:
         find first dept where dept.code = "LM" no-lock no-error.
         if avail dept then x = dept.fc. else x = 1001.
         find first dept where dept.code = mach.dept[1] no-lock no-error.
         if avail dept then
            if (dept.fc ge x and xef.trim-pen ne 0 and
                 mach.min-cal gt xcal and mach.max-cal lt xcal) or
                ((dept.fc lt x or xef.trim-pen = 0) and
                 mach.min-cal gt xef.cal and mach.max-cal lt xef.cal) then next.

         create m-lst.
         assign
             m-lst.f-no   = xef.form-no
             m-lst.seq    = 10 * mach.d-seq
             m-lst.bl     = no
             m-lst.dept   = mach.dept[1]
             m-lst.m-code = mach.m-code
             m-lst.dscr   = mach.m-dscr.
         if mach.p-type = "B" then m-lst.bl = yes.
         if m-lst.bl then m-lst.b-no = xeb.blank-no.
      end.
   end.
end.

end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
