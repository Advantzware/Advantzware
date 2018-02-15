/* --------------------------------------------------- ce/mach-seq.p 10/94 gb */
/* create machine routing sequence                                            */
/* -------------------------------------------------------------------------- */

def input parameter v-qty like est-op.qty.

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
DEF NEW SHARED VAR qty AS INT NO-UNDO.

def        var save_id as RECID NO-UNDO.
def new shared var maxco   as int   no-undo.
def new shared var v-2 as log init FALSE NO-UNDO.
def shared var xcal    as de NO-UNDO.
def new shared var sh-wid  as de NO-UNDO.
def new shared var sh-len  as de NO-UNDO.
def new shared var v-chk-qty as dec no-undo.
def new shared var v-rc-seq as int init 9999 no-undo.
def new shared var v-sht-qty as dec no-undo.
def var v-mach_id as recid no-undo.
def var v-int as int no-undo.
DEF VAR ll-style AS LOG NO-UNDO.


&SCOPED-DEFINE where-machine                                                     ~
               WHERE mach.company EQ cocode                                  ~
                 AND mach.obsolete EQ YES                                       



{ce/mach-lst.i new}

{ce/mach-ink.i new}

run ce/mach-ink.p.

find first style
    {sys/ref/styleW.i}
      and style.style eq xeb.style
    no-lock no-error.

save_id = recid(xest).
find xest where recid(xest) = save_id no-lock no-error.
save_id = recid(xef).
find xef where recid(xef) = save_id no-error.

assign
xcal = xef.cal
sh-len = if xef.roll then xef.gsh-wid else xef.nsh-wid
sh-wid = if xef.roll then xef.gsh-len else xef.nsh-len
qty = if v-qty eq 0 then xest.est-qty[1] else v-qty
v-sht-qty = qty / (xeb.num-up * xef.n-out).

if xef.gsh-qty ne 0 then v-sht-qty = xef.gsh-qty.

IF xeb.pur-man THEN DO:      /* Purchased FG */
  RELEASE mach.
  DO i = 1 TO 7:
    IF style.m-code[i] EQ "" THEN NEXT.
    FIND FIRST mach
        {&where-machine}
          AND mach.m-code EQ style.m-code[i]
        NO-LOCK NO-ERROR.
    IF AVAIL mach AND (mach.dept[1] NE "FO") AND
                      (mach.dept[2] NE "FO") AND
                      (mach.dept[3] NE "FO") AND
                      (mach.dept[4] NE "FO")
    THEN RELEASE mach.
    IF AVAIL mach THEN LEAVE.
  END.
  IF NOT AVAIL mach THEN
  FOR EACH mach
      {&where-machine}
        AND mach.dept[1] EQ "FO"
      NO-LOCK
      BY mach.d-seq
      BY mach.m-seq:
    LEAVE.
  END.
  IF AVAIL mach THEN DO:
    CREATE m-lst.
    ASSIGN
     m-lst.seq    = 10 * mach.d-seq
     m-lst.dept   = "FO"
     m-lst.bl     = YES
     m-lst.m-code = mach.m-code.
  END.
END.

ELSE DO:                   /* Manufactured FG */
/* need sheeter? */
if xef.roll then do:
  /*  {sys/inc/debug.i "xef.roll is yes, in block"}  */
  /* find sheeter entered in style file, if any */
  release mach.
  do i = 1 to 7:
    if style.m-code[i] = "" then next.
    FIND FIRST mach NO-LOCK
        {&where-machine}
          AND mach.m-code EQ style.m-code[i]
        NO-ERROR.
    if avail mach and (mach.dept[1] ne "RS") and
      (mach.dept[2] ne "RS") and
      (mach.dept[3] ne "RS") and
      (mach.dept[4] ne "RS") then do:
      release mach.
      next.
    end.

    if avail mach then
      run ce/mach-qty.p (ROWID(mach)).

    if avail mach and mach.min-len le sh-len  and
      mach.min-wid le sh-wid  and
      mach.min-cal le xef.cal and
      mach.max-len ge sh-len  and
      mach.max-wid ge sh-wid  and
      mach.max-cal ge xef.cal and
      mach.min-run le v-chk-qty and
      mach.max-run ge v-chk-qty
      then leave.
    release mach.
  end.
  if not avail mach then do:
    assign v-mach_id = ?.
    
    for each mach
        {&where-machine}           and
        mach.dept[1] EQ  "RS"   and
        mach.min-len le sh-len  and
        mach.min-wid le sh-wid  and
        mach.max-len ge sh-len  and
        mach.max-wid ge sh-wid  and
        mach.min-cal le xef.cal and
        mach.max-cal ge xef.cal 
        no-lock
        BY mach.d-seq
        BY mach.m-seq:  
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty THEN next.
      v-mach_id = recid(mach).
      leave.
    end.
    if v-mach_id ne ? then
    find mach where recid(mach) = v-mach_id no-lock.
    else release mach.
  end.
  if avail mach then do:
    create m-lst.
    assign
     m-lst.seq    = 10 * mach.d-seq
     m-lst.dept   = "RS"
     m-lst.bl     = no
     m-lst.m-code = mach.m-code.
  end.
end.

/* get guillotine (rc dept) */
if xef.n-cuts gt 0 OR xef.roll then
reamcut:
do on error undo:
  /*{sys/inc/debug.i "xef.n-cuts gt 0, in reamcut block"} */
  release mach.
  /* find ream cutter entered in style file, if any */
  do i = 1 to 7:
    if style.m-code[i] = "" then next.
    FIND FIRST mach NO-LOCK
        {&where-machine}
          AND mach.m-code EQ style.m-code[i]
        NO-ERROR.
    if avail mach and (mach.dept[1] ne "RC") and
      (mach.dept[2] ne "RC") and
      (mach.dept[3] ne "RC") and
      (mach.dept[4] ne "RC") then do:
      release mach.
      next.
    end.
    if avail mach then do:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
      release mach.
    end.
    if avail mach 
            and mach.min-cal le xef.cal
            and mach.max-cal ge xef.cal
            and (mach.p-type EQ "R" OR xef.n-cuts NE 0)
            and /*((xef.xgrain eq "S"                    and
                  mach.min-len             le sh-wid   and
                  mach.min-wid             le sh-len   and
                  mach.max-len             ge sh-wid   and
                  mach.max-wid             ge sh-len)       or
                 (xef.xgrain ne "S"                    and*/
                  mach.min-len             le sh-len   and
                  mach.min-wid             le sh-wid   and
                  mach.max-len             ge sh-len   and
                  mach.max-wid             ge sh-wid /*))*/
      then leave.
  end.
  if not avail mach then do:
    for each mach  no-lock
        {&where-machine}
            and mach.dept[1] eq "RC"   
            and mach.min-cal le xef.cal
            and mach.max-cal ge xef.cal
            and (mach.p-type EQ "R" OR xef.n-cuts NE 0)
            and /*((xef.xgrain eq "S"                    and
                  mach.min-len             le sh-wid   and
                  mach.min-wid             le sh-len   and
                  mach.max-len             ge sh-wid   and
                  mach.max-wid             ge sh-len)       or
                 (xef.xgrain ne "S"                    and*/
                  mach.min-len             le sh-len   and
                  mach.min-wid             le sh-wid   and
                  mach.max-len             ge sh-len   and
                  mach.max-wid             ge sh-wid /*))*/
        BY INT(xef.roll EQ (mach.p-type EQ "R")) DESC
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      else leave.
    end.
  end.
  if avail mach then do:
    create m-lst.
    assign m-lst.seq    = 10 * mach.d-seq
      m-lst.dept   = "RC"
      m-lst.bl     = no
      m-lst.m-code = mach.m-code
      m-lst.n-out  = xef.n-out
      v-rc-seq     = 10 * mach.d-seq.
    if mach.p-type eq "R" or xef.roll eq no then do:
      find first m-lst
          where m-lst.f-no eq xef.form-no
            and m-lst.dept eq "RS"
          no-error.
      if avail m-lst then delete m-lst.
    end.
  end.
end.

assign
 sh-len = xef.nsh-wid
 sh-wid = xef.nsh-len.

find first w-ink where w-ink.form-no eq xef.form-no no-error.

if avail w-ink then run ce/mach-pr.p.

/* find ink & coater */
else
if xeb.i-pass gt 0 then do:
  find first item {sys/look/itemivW.i} and
    item.i-no    = xeb.i-code2[1] no-lock no-error.
  if avail item and item.i-code = "E" then
  find first e-item OF item no-lock no-error.
  maxco = (xeb.i-col + xeb.i-coat ) / xeb.i-pass.
  {sys/inc/roundup.i maxco}

  if xeb.i-coat gt 0 then
  do k = 1 to xeb.i-coat:
    /* find coater entered in style file, if any */
    release mach.
    do i = 1 to 7:
      if style.m-code[i] = "" then
      next.
      FIND FIRST mach NO-LOCK
          {&where-machine}
            AND mach.m-code EQ style.m-code[i]
            AND mach.dept[1] EQ "CT"            
            and mach.min-cal le xef.cal
            and mach.max-cal ge xef.cal
            and /*((xef.xgrain eq "S"                    and
                  mach.min-len             le sh-wid   and
                  mach.min-wid             le sh-len   and
                  mach.max-len             ge sh-wid   and
                  mach.max-wid             ge sh-len)       or
                 (xef.xgrain ne "S"                    and*/
                  mach.min-len             le sh-len   and
                  mach.min-wid             le sh-wid   and
                  mach.max-len             ge sh-len   and
                  mach.max-wid             ge sh-wid /*))*/
          NO-ERROR.
      if avail mach then do:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      end.
      if avail mach then leave.
    end.
    /* find 1st valid machine in mach file */
    if not avail mach then
    for each mach
        {&where-machine}
            AND mach.dept[1]  = "CT"         
            and mach.min-cal le xef.cal
            and mach.max-cal ge xef.cal
            and /*((xef.xgrain eq "S"                    and
                  mach.min-len             le sh-wid   and
                  mach.min-wid             le sh-len   and
                  mach.max-len             ge sh-wid   and
                  mach.max-wid             ge sh-len)       or
                 (xef.xgrain ne "S"                    and*/
                  mach.min-len             le sh-len   and
                  mach.min-wid             le sh-wid   and
                  mach.max-len             ge sh-len   and
                  mach.max-wid             ge sh-wid /*))*/
        NO-LOCK
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      else leave.
    end.
    if avail mach then do:
      create m-lst.
      assign m-lst.seq = (10 * mach.d-seq) + k
        m-lst.bl  = no
        m-lst.dept   = "CT"
        m-lst.m-code = mach.m-code
        m-lst.dscr = mach.m-dscr
        m-lst.pass-no = k.

      maxco = xeb.i-col / xeb.i-pass.
      {sys/inc/roundup.i maxco}
    end.
  end.
  /** end. /* avail item... */ **/

  /* find press */
  if avail item then k = 0.
  prez:
  repeat:
    k = k + 1.
    /* find machine entered in style file, if any */
    if k = 1 and avail mach then release mach.
    do i = 1 to 7:
      if style.m-code[i] eq "" then next.
      FIND FIRST mach NO-LOCK
          {&where-machine}
            AND mach.m-code EQ style.m-code[i]
            AND mach.pr-type EQ item.press-type
            AND mach.max-color ge maxco 
            and mach.min-cal le xef.cal
            and mach.max-cal ge xef.cal
            and /*((xef.xgrain eq "S"                    and
                  mach.min-len             le sh-wid   and
                  mach.min-wid             le sh-len   and
                  mach.max-len             ge sh-wid   and
                  mach.max-wid             ge sh-len)       or
                 (xef.xgrain ne "S"                    and*/
                  mach.min-len             le sh-len   and
                  mach.min-wid             le sh-wid   and
                  mach.max-len             ge sh-len   and
                  mach.max-wid             ge sh-wid /*))*/
          NO-ERROR.

      if avail mach then do:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      end.

      if avail mach then leave.
    end.
    /* find layout machine in mach file */
    if not avail mach then do:
      find first mach
          {&where-machine}
            and mach.m-code EQ xef.m-code
          no-lock no-error.
      if avail mach and mach.dept[1] ne "PR" then release mach.
      /*
      for each mach {&where-machine}                             and
      mach.pr-type =  item.press-type            and
      mach.max-col ge maxco 
            and mach.min-cal le xef.cal
            and mach.max-cal ge xef.cal
            and /*((xef.xgrain eq "S"                    and
                  mach.min-len             le sh-wid   and
                  mach.min-wid             le sh-len   and
                  mach.max-len             ge sh-wid   and
                  mach.max-wid             ge sh-len)       or
                 (xef.xgrain ne "S"                    and*/
                  mach.min-len             le sh-len   and
                  mach.min-wid             le sh-wid   and
                  mach.max-len             ge sh-len   and
                  mach.max-wid             ge sh-wid /*))*/
      by mach.max-col
      BY mach.d-seq
      BY mach.m-seq:
      if avail mach then leave.
      end.
      */
      if avail mach then do:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      end.
    end.
    /* find 1st valid machine in mach file */
    if not avail mach then
    for each mach
        {&where-machine}
            AND mach.pr-type EQ item.press-type
            AND mach.max-color ge maxco 
            and mach.min-cal le xef.cal
            and mach.max-cal ge xef.cal
            and /*((xef.xgrain eq "S"                    and
                  mach.min-len             le sh-wid   and
                  mach.min-wid             le sh-len   and
                  mach.max-len             ge sh-wid   and
                  mach.max-wid             ge sh-len)       or
                 (xef.xgrain ne "S"                    and*/
                  mach.min-len             le sh-len   and
                  mach.min-wid             le sh-wid   and
                  mach.max-len             ge sh-len   and
                  mach.max-wid             ge sh-wid /*))*/
        BY mach.max-color
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      else leave.
    end.

    if avail mach then do:
      if mach.p-type eq "R" or xef.roll eq no then
      do:
        find first m-lst where m-lst.dept eq "RS" no-error.
        if avail m-lst then
        delete m-lst.
      end.
      if k = 1 and mach.coater = yes and mach.max-color gt maxco then
      do:
        find first m-lst where m-lst.seq gt 30 and m-lst.seq lt 40 no-error.
        if avail m-lst then
        do:
          delete m-lst.
          k = k - 1.
          maxco = maxco + 1.
          next prez.
        end.
      end.
      create m-lst.
      assign
        m-lst.seq     = (10 * mach.d-seq) + k
        m-lst.bl      = no
        m-lst.dept    = "PR"
        m-lst.m-code  = mach.m-code
        m-lst.dscr    = mach.m-dscr
        m-lst.pass-no = k.
    end.
    if k ge xeb.i-pass then
    leave.
  end. /* avail item... */
end. /* if i-pass gt 0 */

else do:
  k = 1.
  /* find machine entered in style file, if any */
  release mach.
  do i = 1 to 7:
    find first mach
        {&where-machine}
          and mach.m-code = style.m-code[i]
        no-lock no-error.
    if avail mach then do:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
      release mach.
    end.
    if avail mach then leave.
  end.
  /* find 1st valid machine in mach file */
  if not avail mach then
  find first mach
      {&where-machine}
        and mach.m-code = xef.m-code
      no-lock no-error.
  if avail mach then do:
    run ce/mach-qty.p (ROWID(mach)).
    if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
    release mach.
  end.
  if avail mach and can-do("RC,PR",mach.dept[1]) then do:
    if mach.p-type = "R" or xef.roll = no then do:
      find first m-lst where m-lst.dept = "RS" no-error.
      if avail m-lst then
      delete m-lst.
    end.

    create m-lst.
    assign
      m-lst.seq     = (10 * mach.d-seq) + k
      m-lst.bl      = mach.p-type eq "B"
      m-lst.m-code  = mach.m-code
      m-lst.dscr    = mach.m-dscr
      m-lst.pass-no = k.
    do i = 1 to 4 :
      if mach.dept[i] ne "" and mach.dept[i] ne "PR" then
      do:
        m-lst.dept  = mach.dept[i].
        leave.
      end.
    end.

    IF m-lst.dept EQ "" AND mach.dept[1] EQ "PR" THEN
       m-lst.dept = "PR".
  end.
end.

/* get guillotine (gu dept after printing) */
if xef.n-out-l gt 1 then
do on error undo:
  release mach.
  /* find guillotine entered in style file, if any */
  do i = 1 to 7:
    if style.m-code[i] = "" then next.
    FIND FIRST mach
        {&where-machine}
          AND mach.m-code EQ style.m-code[i]
        NO-LOCK NO-ERROR.
    if avail mach and (mach.dept[1] ne "GU") and
      (mach.dept[2] ne "GU") and
      (mach.dept[3] ne "GU") and
      (mach.dept[4] ne "GU") then do:
      release mach.
      next.
    end.
    if avail mach then do:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
      release mach.
    end.
    if avail mach
            and mach.min-cal le xef.cal
            and mach.max-cal ge xef.cal
            and /*((xef.xgrain eq "S"                    and
                  mach.min-len             le sh-wid   and
                  mach.min-wid             le sh-len   and
                  mach.max-len             ge sh-wid   and
                  mach.max-wid             ge sh-len)       or
                 (xef.xgrain ne "S"                    and*/
                  mach.min-len             le sh-len   and
                  mach.min-wid             le sh-wid   and
                  mach.max-len             ge sh-len   and
                  mach.max-wid             ge sh-wid /*))*/
      then
    leave.
  end.
  if not avail mach then do:
    for each mach
        {&where-machine}
          and mach.dept[1] eq "GU"
          and mach.min-cal le xef.cal
          and mach.max-cal ge xef.cal
          and /*((xef.xgrain eq "S"                    and
                  mach.min-len             le sh-wid   and
                  mach.min-wid             le sh-len   and
                  mach.max-len             ge sh-wid   and
                  mach.max-wid             ge sh-len)       or
                 (xef.xgrain ne "S"                    and*/
                  mach.min-len             le sh-len   and
                  mach.min-wid             le sh-wid   and
                  mach.max-len             ge sh-len   and
                  mach.max-wid             ge sh-wid /*))*/
        no-lock
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      else leave.
    end.
  end.
  if avail mach then do:
    create m-lst.
    assign m-lst.seq    = 10 * mach.d-seq
      m-lst.dept   = "GU"
      m-lst.bl     = no
      m-lst.m-code = mach.m-code
      m-lst.dscr   = mach.m-dscr
      m-lst.n-out  = xef.n-out-l.
  end.
end.

/* laminating */
if xef.medium ne "" or xef.flute ne "" or xef.lam-code ne "" then
lamin:
do on error undo:
  if xef.trim-pen ne 0 then
  do:
    xcal = xef.trim-pen / 1000. /* trim-pen stored as int */
    if xef.cal ne 0 and xef.cal ne .014 then
    xcal = xcal - .014 + xef.cal.
  end.
  else
  xcal = xef.cal.
  release mach.
  /* find laminator entered in style file, if any */
  do i = 1 to 7:
    FIND FIRST mach
        {&where-machine}
          AND mach.m-code EQ style.m-code[i]
        NO-LOCK NO-ERROR.
    if avail mach and (mach.dept[1] ne "LM") and
      (mach.dept[2] ne "LM") and
      (mach.dept[3] ne "LM") and
      (mach.dept[4] ne "LM") then do:
      release mach.
      next.
    end.
    if avail mach then do:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
      release mach.
    end.
    if avail mach 
            and mach.min-cal le xef.cal
            and mach.max-cal ge xef.cal
            and /*((xef.xgrain eq "S"                    and
                  mach.min-len             le sh-wid   and
                  mach.min-wid             le sh-len   and
                  mach.max-len             ge sh-wid   and
                  mach.max-wid             ge sh-len)       or
                 (xef.xgrain ne "S"                    and*/
                  mach.min-len             le sh-len   and
                  mach.min-wid             le sh-wid   and
                  mach.max-len             ge sh-len   and
                  mach.max-wid             ge sh-wid /*))*/
      then
    leave.
  end.
  if not avail mach then
  do:
    for each mach
        {&where-machine}
            AND mach.dept[1] EQ "LM"         
            and mach.min-cal le xef.cal
            and mach.max-cal ge xef.cal
            and /*((xef.xgrain eq "S"                    and
                  mach.min-len             le sh-wid   and
                  mach.min-wid             le sh-len   and
                  mach.max-len             ge sh-wid   and
                  mach.max-wid             ge sh-len)       or
                 (xef.xgrain ne "S"                    and*/
                  mach.min-len             le sh-len   and
                  mach.min-wid             le sh-wid   and
                  mach.max-len             ge sh-len   and
                  mach.max-wid             ge sh-wid /*))*/
        no-lock
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      else leave.
    end.
  end.
  if avail mach then do:
    create m-lst.
    assign m-lst.seq    = 10 * mach.d-seq
      m-lst.dept   = "LM"
      m-lst.bl     = no
      m-lst.m-code = mach.m-code.
  end.
end.

IF xef.n-out-l GT 1 THEN
  assign
   sh-len = xef.trim-w
   sh-wid = xef.trim-l.

/* get die cutter */
diecut:
do on error undo:
  /* find die cutter entered in style file, if any */
  release mach.
  do i = 1 to 7:
    if style.m-code[i] = "" then
    next.
    FIND FIRST mach
        {&where-machine}
          AND mach.m-code EQ style.m-code[i]
        NO-LOCK NO-ERROR.
    if avail mach and (mach.dept[1] ne "DC" ) and
      (mach.dept[2] ne "DC" ) and
      (mach.dept[3] ne "DC" ) and
      (mach.dept[4] ne "DC" ) then do:
      release mach.
      next.
    end.
    if avail mach then do:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
      release mach.
    end.

    if avail mach 
            and mach.min-cal le xef.cal
            and mach.max-cal ge xef.cal
            and /*((xef.xgrain eq "S"                    and
                  mach.min-len             le sh-wid   and
                  mach.min-wid             le sh-len   and
                  mach.max-len             ge sh-wid   and
                  mach.max-wid             ge sh-len)       or
                 (xef.xgrain ne "S"                    and*/
                  mach.min-len             le sh-len   and
                  mach.min-wid             le sh-wid   and
                  mach.max-len             ge sh-len   and
                  mach.max-wid             ge sh-wid /*))*/
      then
    leave.
  end.
  if not avail mach then
  do:
    for each mach
        {&where-machine}
            AND mach.dept[1] EQ "DC"        
            and mach.min-cal le xef.cal
            and mach.max-cal ge xef.cal
            and /*((xef.xgrain eq "S"                    and
                  mach.min-len             le sh-wid   and
                  mach.min-wid             le sh-len   and
                  mach.max-len             ge sh-wid   and
                  mach.max-wid             ge sh-len)       or
                 (xef.xgrain ne "S"                    and*/
                  mach.min-len             le sh-len   and
                  mach.min-wid             le sh-wid   and
                  mach.max-len             ge sh-len   and
                  mach.max-wid             ge sh-wid /*))*/
        NO-LOCK
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      else leave.
    end.
  end.
  if avail mach then do:
    create m-lst.
   /* {sys/inc/debug.i "In ce/mach-seq.p.  Creating DC Dept..."} */
    assign
      m-lst.seq    = 10 * mach.d-seq
      m-lst.dept   = "DC"
      m-lst.bl     = no
      m-lst.m-code = mach.m-code.
  end.
end.

/* get auto strip */
hstrip:
do on error undo:
  release mach.
  do i = 1 to 7:
    if style.m-code[i] = "" then next.
    FIND FIRST mach
        {&where-machine}
          AND mach.m-code EQ style.m-code[i]
        NO-LOCK NO-ERROR.
    if avail mach then do:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
      release mach.
    end.
    if avail mach then do:
      if mach.dept[1] = "HS" or mach.dept[2] = "HS" or
        mach.dept[3] = "HS" or mach.dept[4] = "HS" then leave.
      release mach.
    end.
  end.

  if not avail mach then do:
    for each mach
        {&where-machine}
            AND mach.dept[1] EQ "HS"        
            and mach.min-cal le xef.cal
            and mach.max-cal ge xef.cal
            and /*((xef.xgrain eq "S"                    and
                  mach.min-len             le sh-wid   and
                  mach.min-wid             le sh-len   and
                  mach.max-len             ge sh-wid   and
                  mach.max-wid             ge sh-len)       or
                 (xef.xgrain ne "S"                    and*/
                  mach.min-len             le sh-len   and
                  mach.min-wid             le sh-wid   and
                  mach.max-len             ge sh-len   and
                  mach.max-wid             ge sh-wid /*))*/
        no-lock
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      else leave.
    end.
  end.
  if avail mach then do:
    create m-lst.
    assign
      m-lst.seq    = 10 * mach.d-seq
      m-lst.dept   = "HS"
      m-lst.bl     = no
      m-lst.m-code = mach.m-code.
  end.
end.

/* get window or foil */
{ce/mach-wof.i}

/* get gluer */
glu:
do on error undo:
  if xeb.adhesive ne "" and xeb.gluelap  ne 0  then
  do:
    ll-style = NO.
    find first item
        where item.company eq cocode
          and item.i-no    eq xeb.adhesive
        no-lock no-error.
    if avail item and item.mat-type eq "T" then leave glu.
    release mach.
    do i = 1 to 7:
      if style.m-code[i] = "" then next.
      FIND FIRST mach
          {&where-machine}
            AND mach.m-code EQ style.m-code[i]
          NO-LOCK NO-ERROR.
      if avail mach then do:
        if mach.dept[1] ne "GL" and mach.dept[2] ne "GL" and
          mach.dept[3] ne "GL" and mach.dept[4] ne "GL" then do:
          release mach.
          next.
        end.
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      end.
      IF AVAIL mach THEN DO:
        ll-style = YES.
        RUN create-m-lst ("GL", YES).
      END.
    end.
    IF NOT ll-style THEN
    for each mach
        {&where-machine}              and
          mach.dept[1] EQ "GL"        and
          mach.min-len le xeb.t-len   and
          mach.min-wid le xeb.t-wid   and
          mach.min-cal le xcal        and
          mach.max-len ge xeb.t-len   and
          mach.max-wid ge xeb.t-wid   and
          mach.max-cal ge xcal
          no-lock
          BY mach.d-seq
          BY mach.m-seq:

      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then do:
        release mach.
      end.
      ELSE DO:
        RUN create-m-lst ("GL", NO).
        LEAVE.
      END.
    end.
  end.
end.

/* get Quad Stayer */
quadstayer:
do on error undo:
  if xeb.adhesive ne "" then
  do:
    find first item
        where item.company eq cocode
          and item.i-no    eq xeb.adhesive
        no-lock no-error.

    if not avail item or item.mat-type ne "T" then leave quadstayer.

    release mach.
    do i = 1 to 7:
      if style.m-code[i] = "" then next.
      FIND FIRST mach
          {&where-machine}
            AND mach.m-code EQ style.m-code[i]
          NO-LOCK NO-ERROR.
      if avail mach then do:
        if mach.dept[1] ne "QS" and mach.dept[2] ne "QS" and
          mach.dept[3] ne "QS" and mach.dept[4] ne "QS" then do:
          release mach.
          next.
        end.
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      end.
      if avail mach then leave.
    end.
    if not avail mach then do:
      for each mach
          {&where-machine}               and
          mach.dept[1] EQ "QS"        and
          mach.min-len le xeb.t-len   and
          mach.min-wid le xeb.t-wid   and
          mach.min-cal le xcal        and
          mach.max-len ge xeb.t-len   and
          mach.max-wid ge xeb.t-wid   and
          mach.max-cal ge xcal
          no-lock
          BY mach.d-seq
          BY mach.m-seq:

        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
          release mach.
        else leave.
      end.
    end.
    if avail mach then do:
      create m-lst.
      assign m-lst.seq = 10 * mach.d-seq
        m-lst.bl  = no
        m-lst.dept   = "QS"
        m-lst.m-code = mach.m-code
        m-lst.dscr = mach.m-dscr.
      if mach.p-type = "B" then m-lst.bl = yes.
    end.
  end.
end.
/*
if debug-mode-on then do:
  for each m-lst with frame f-debug:
    display m-lst.
  end.
  hide frame f-debug.
end.
*/
/* get any other mach in style file */
sty:
do on error undo:
  release mach.
  do i = 1 to 7:
    if style.m-code[i] = "" then next.
    RELEASE m-lst.
    FIND FIRST mach
          {&where-machine}
            AND mach.m-code EQ style.m-code[i] AND
      mach.min-cal le xcal     and
      mach.max-cal ge xcal     and
      NOT CAN-FIND(first m-lst
                   where ((m-lst.dept  eq mach.dept[1] AND NOT m-lst.styl) OR
                          m-lst.m-code EQ style.m-code[i])) AND
      ((mach.p-type = "B" and
      mach.min-len le xeb.t-len   and
      mach.min-wid le xeb.t-wid   and
      mach.max-len ge xeb.t-len   and
      mach.max-wid ge xeb.t-wid)
      OR
      (mach.p-type NE "B" and
                 /*((xef.xgrain eq "S"                    and
                  mach.min-len             le sh-wid   and
                  mach.min-wid             le sh-len   and
                  mach.max-len             ge sh-wid   and
                  mach.max-wid             ge sh-len)       or
                 (xef.xgrain ne "S"                    and*/
                  mach.min-len             le sh-len   and
                  mach.min-wid             le sh-wid   and
                  mach.max-len             ge sh-len   and
                  mach.max-wid             ge sh-wid /*))*/
      ))
      no-lock no-error.
    if avail mach then do:
      find first m-lst where m-lst.dept eq mach.dept[1] no-lock no-error.
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty or
         (avail m-lst and m-lst.dept ne "AW")                   then
        release mach.
    end.
    
    if avail mach then do:
      find first dept where dept.code = "LM" no-lock no-error.
      if avail dept then
      x = dept.fc.
      else
      x = 1001.
      find first dept where dept.code = mach.dept[1] no-lock no-error.
      if avail dept then
        if (dept.fc ge x and xef.trim-pen ne 0 and
          mach.min-cal gt xcal and mach.max-cal lt xcal) or
          ((dept.fc lt x or xef.trim-pen = 0) and
          mach.min-cal gt xef.cal and mach.max-cal lt xef.cal) then next.
      
      create m-lst.
      assign
        m-lst.seq    = 10 * mach.d-seq
        m-lst.dept   = mach.dept[1]
        m-lst.m-code = mach.m-code
        m-lst.dscr   = mach.m-dscr
        m-lst.bl     = mach.p-type EQ "B"
        m-lst.styl   = YES.
    end.
  end.
end.
/*
if debug-mode-on then do:
  for each m-lst with frame f-debug2:
    display m-lst.
  end.
  hide frame f-debug2.
end.
*/
END.  /* Manufactured FG */

run ce/mach-sq2.p.

hide frame kalk no-pause.

RETURN.

PROCEDURE create-m-lst.
  
  DEF INPUT PARAM ip-dept  LIKE m-lst.dept NO-UNDO.
  DEF INPUT PARAM ip-style AS LOG          NO-UNDO.


  CREATE m-lst.
  ASSIGN
   m-lst.seq    = 10 * mach.d-seq
   m-lst.dept   = ip-dept
   m-lst.m-code = mach.m-code
   m-lst.dscr   = mach.m-dscr
   m-lst.bl     = mach.p-type EQ "B"
   m-lst.styl   = ip-style.

END PROCEDURE.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
