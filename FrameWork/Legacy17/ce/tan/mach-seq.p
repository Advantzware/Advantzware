/* --------------------------------------------------- ce/mach-seq.p 10/94 gb */
/* create machine routing sequence                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def buffer xeb2 for eb.
DEF BUFFER bf-eb FOR eb.
def buffer alt-xeb for eb.
def buffer xstyle for style.

def new shared var sh-wid as DEC NO-UNDO.
def new shared var sh-len as dec NO-UNDO.
def new shared var maxco as int no-undo.
def new shared var v-2 as log init false NO-UNDO.
def new shared var v-chk-qty as dec no-undo.
def new shared var v-rc-seq as int init 9999 no-undo.
def new shared var v-sht-qty as dec no-undo.
DEF NEW SHARED VAR qty AS INT NO-UNDO.
def shared var xcal as de NO-UNDO.
def var save_id as recid no-undo.
def var v-dept like dept.code no-undo.
DEF VAR ll-style AS LOG NO-UNDO.

&SCOPED-DEFINE where-machine                                                     ~
               WHERE (mach.company EQ cocode                                  ~
                 AND  NOT CAN-FIND(FIRST reftable                             ~
                                   WHERE reftable.reftable EQ "mach.obsolete" ~
                                     AND reftable.company  EQ mach.company    ~
                                     AND reftable.loc      EQ mach.loc        ~
                                     AND reftable.code     EQ mach.m-code     ~
                                     AND reftable.val[1]   EQ 1)) 

{ce/mach-lst.i new}

{ce/mach-ink.i new}

/*
display "                P l e a s e   W a i t !     C A L C U L A T I N G              "
  with frame kalk row lorow overlay no-box no-labels width 81
  color value("blink-" + col-look).
*/
SESSION:SET-WAIT-STATE("general").
run ce/mach-ink.p.

find first style
    {sys/ref/styleW.i}
      and style.style eq xeb.style
    no-lock no-error.

assign
 xcal   = xef.cal
 sh-len = if xef.roll then xef.gsh-wid else xef.nsh-wid
 sh-wid = if xef.roll then xef.gsh-len else xef.nsh-len.

v-sht-qty = if xef.gsh-qty ne 0 then xef.gsh-qty
            else (xeb.bl-qty / (xeb.num-up * xef.n-out)).

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
if xef.roll eq true then do:
  /* find sheeter entered in style file, if any */
  if avail mach then
  release mach.
  do i = 1 to 7:
    find first mach
        {&where-machine}
          and mach.m-code eq style.m-code[i]
        no-lock no-error.
    if avail mach and (mach.dept[1] ne "RS") and
      (mach.dept[2] ne "RS") and
      (mach.dept[3] ne "RS") and
      (mach.dept[4] ne "RS") then do:
      release mach.
      next.
    end.
    if avail mach then
      run ce/mach-qty.p (ROWID(mach)).
    if avail mach and
      mach.min-len le sh-len  and
      mach.min-wid le sh-wid  and
      mach.min-cal le xef.cal      and
      mach.max-len ge sh-len  and
      mach.max-wid ge sh-wid  and
      mach.max-cal ge xef.cal and
      mach.min-run le v-chk-qty and
      mach.max-run ge v-chk-qty
      then
    leave.
  end.
  if not avail mach then do:
    for each mach {&where-machine}             and
        mach.dept[1]  eq "RS"    and
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
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then next.
      leave.
    end.
  end.

  if avail mach then do:
    create m-lst.
    assign
      m-lst.seq    = 10 * mach.d-seq
      m-lst.f-no   = xef.form-no
      m-lst.b-no   = 0
      m-lst.dept   = "RS"
      m-lst.bl     = no
      m-lst.m-code = mach.m-code.
  end.
end.

/* get guillotine (rc dept) */
if xef.n-cuts gt 0 OR xef.roll then
reamcut:
do on error undo:
  /* find ream cutter entered in style file, if any */
  if avail mach then
  release mach.
  do i = 1 to 7:
    find first mach {&where-machine}            and
      mach.m-code  eq style.m-code[i] no-lock no-error.
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
    if avail mach and
      (mach.p-type EQ "R" OR xef.n-cuts NE 0) AND
      ((xef.roll eq yes  and xef.lam-dscr eq "S" and
      (mach.min-len le xef.gsh-wid  and
      mach.min-wid le xef.gsh-len  and
      mach.min-cal le xcal      and
      mach.max-cal ge xcal      and
      mach.max-len ge xef.gsh-wid  and
      mach.max-wid ge xef.gsh-len     ))   or
      (mach.min-len le xef.gsh-len  and
      mach.min-wid le xef.gsh-wid  and
      mach.min-cal le xcal      and
      mach.max-cal ge xcal      and
      mach.max-len ge xef.gsh-len  and
      mach.max-wid ge xef.gsh-wid     ))
      then
    leave.
  end.
  if not avail mach then do:
    for each mach {&where-machine}                  and
        (mach.p-type EQ "R" OR xef.n-cuts NE 0) AND
        mach.dept[1]  eq "RC"         and
        ((xef.roll eq yes  and xef.lam-dscr eq "S" and
        (mach.min-len le xef.gsh-wid  and
        mach.min-wid le xef.gsh-len  and
        mach.min-cal le xcal      and
        mach.max-cal ge xcal      and
        mach.max-len ge xef.gsh-wid  and
        mach.max-wid ge xef.gsh-len     ))   or
        (mach.min-len le xef.gsh-len  and
        mach.min-wid le xef.gsh-wid  and
        mach.min-cal le xcal      and
        mach.max-cal ge xcal      and
        mach.max-len ge xef.gsh-len  and
        mach.max-wid ge xef.gsh-wid     ))
        no-lock
        BY INT(xef.roll EQ (mach.p-type EQ "R")) DESC
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
      next.
      leave.
    end.
  end.
  if avail mach then do:
    create m-lst.
    assign
      m-lst.seq    = 10 * mach.d-seq
      m-lst.f-no   = xef.form-no
      m-lst.b-no   = 0
      m-lst.dept   = "RC"
      m-lst.bl     = no
      m-lst.m-code = mach.m-code
      m-lst.n-out  = xef.n-out
      v-rc-seq     = m-lst.seq
      sh-wid       = xef.nsh-len
      sh-len       = xef.nsh-wid.
    if mach.p-type eq "R" or xef.roll eq no then do:
      find first m-lst
          where m-lst.f-no eq xef.form-no
            and m-lst.dept eq "RS"
          no-error.
      if avail m-lst then delete m-lst.
    end.
  end.
end.

find first w-ink where w-ink.form-no eq xef.form-no no-error.

if avail w-ink then run ce/mach-pr.p.

/* find ink & coater */
else
if xeb.i-pass gt 0 then do:
  find first item {sys/look/itemivW.i} and
    item.i-no    eq xeb.i-code2[1] no-lock no-error.
  if avail item and item.i-code eq "E" then
  find first e-item where e-item.company eq item.company and
    e-item.loc     eq item.loc     and
    e-item.i-no    eq item.i-no no-lock no-error.

  if xef.f-coat gt 0 then
  do k = 1 to xef.f-coat:
    /* find coater entered in style file, if any */
    if avail mach then
    release mach.
    do i = 1 to 7:
      find first mach {&where-machine} and mach.m-code  eq style.m-code[i]
        and mach.dept[1]  eq "CT"         and
        ((xef.roll eq yes  and xef.lam-dscr eq "S" and
        (mach.min-len le sh-wid  and
        mach.min-wid le sh-len  and
        mach.min-cal le xef.cal      and
        mach.max-cal ge xef.cal      and
        mach.max-len ge sh-wid  and
        mach.max-wid ge sh-len     ))   or

        (mach.min-len le sh-len  and
        mach.min-wid le sh-wid  and
        mach.min-cal le xef.cal      and
        mach.max-cal ge xef.cal      and
        mach.max-len ge sh-len  and
        mach.max-wid ge sh-wid     ))
        no-lock no-error.
      if avail mach then do:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      end.
      if avail mach then
      leave.
    end.
    /* find 1st valid machine in mach file */
    if not avail mach then
    for each mach {&where-machine}                  and
        mach.dept[1]  eq "CT"         and
        ((xef.roll eq yes  and xef.lam-dscr eq "S" and
        (mach.min-len le sh-wid  and
        mach.min-wid le sh-len  and
        mach.min-cal le xef.cal      and
        mach.max-cal ge xef.cal      and
        mach.max-len ge sh-wid  and
        mach.max-wid ge sh-len     ))   or
        (mach.min-len le sh-len  and
        mach.min-wid le sh-wid  and
        mach.min-cal le xef.cal      and
        mach.max-cal ge xef.cal      and
        mach.max-len ge sh-len  and
        mach.max-wid ge sh-wid     ))
        NO-LOCK
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
      next.
      leave.
    end.
    if avail mach then do:
      create m-lst.
      assign
        m-lst.seq = (10 * mach.d-seq) + k
        m-lst.f-no   = xef.form-no
        m-lst.bl  = no
        m-lst.dept = "CT"
        m-lst.m-code = mach.m-code
        m-lst.dscr = mach.m-dscr
        m-lst.pass-no = k.
    end.
  end. /* avail item... */

  /* find press */
  if avail item then
  prez:
  repeat:
    find first m-lst where m-lst.dept eq "CT" no-error.
    if not avail m-lst then do:
      if int((xef.f-col + xef.f-coat ) / xef.f-pass) lt
        ((xef.f-col + xef.f-coat ) / xef.f-pass) then
      maxco = int((xef.f-col + xef.f-coat ) / xef.f-pass) + 1.
      else
      maxco = int((xef.f-col + xef.f-coat ) / xef.f-pass).
    end.
    else do:
      if int(xef.f-col / xef.f-pass) lt (xef.f-col / xef.f-pass) then
      maxco = int(xef.f-col / xef.f-pass) + 1.
      else
      maxco = int(xef.f-col / xef.f-pass).
    end.
    if avail mach then release mach.

    /* find machine entered in style file, if any */
    do i = 1 to 7:
      find first mach {&where-machine}                                and
        mach.m-code  eq style.m-code[i]             and
        mach.pr-type eq  item.press-type            and
        mach.max-color ge maxco                      and
        ((xef.roll eq yes  and xef.lam-dscr eq "S" and
        (mach.min-len le sh-wid  and
        mach.min-wid le sh-len  and
        mach.min-cal le xef.cal      and
        mach.max-cal ge xef.cal      and
        mach.max-len ge sh-wid  and
        mach.max-wid ge sh-len     ))   or
        (mach.min-len le sh-len  and
        mach.min-wid le sh-wid  and
        mach.min-cal le xef.cal      and
        mach.max-cal ge xef.cal      and
        mach.max-len ge sh-len  and
        mach.max-wid ge sh-wid     ))
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
    for each mach {&where-machine}                                and
        mach.pr-type eq  item.press-type            and
        mach.max-color ge maxco                      and
        ((xef.roll eq yes  and xef.lam-dscr eq "S" and
        (mach.min-len le sh-wid  and
        mach.min-wid le sh-len  and
        mach.min-cal le xef.cal      and
        mach.max-cal ge xef.cal      and
        mach.max-len ge sh-wid  and
        mach.max-wid ge sh-len     ))   or
        (mach.min-len le sh-len  and
        mach.min-wid le sh-wid  and
        mach.min-cal le xef.cal      and
        mach.max-cal ge xef.cal      and
        mach.max-len ge sh-len  and
        mach.max-wid ge sh-wid     ))
        BY mach.max-color
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
      next.
      leave.
    end.

    if avail mach then do:
      if mach.p-type eq "R" then do:
        find first m-lst where m-lst.seq ge 10 and m-lst.seq lt 20 no-error.
        if avail m-lst then
        delete m-lst.
      end.
      if mach.coater eq true and mach.max-color * xef.f-pass gt xef.f-col then do:
        find first m-lst where m-lst.dept eq "CT"  no-error.
        if avail m-lst then
        delete m-lst.
        /* delete only 1 coater... */
      end.

      prez2:
      for each xeb2 where xeb2.company = xest.company
                      AND xeb2.est-no eq xest.est-no :
        do k = 1 to xeb2.i-pass:
          create m-lst.
          assign
            m-lst.seq     = (10 * mach.d-seq) + xeb2.blank-no
            m-lst.f-no    = xef.form-no
            m-lst.bl      = no
            m-lst.b-no    = xeb2.blank-no
            m-lst.dept    = "PR"
            m-lst.m-code  = mach.m-code
            m-lst.dscr    = mach.m-dscr
            m-lst.pass-no = k.
        end.
      end.

    end.
    leave.    /* fake repeat */
  end. /* avail item... */
end. /* if i-pass gt 0 */

else do:
  k = 1.
  if avail mach then
  release mach.
  /* find machine entered in style file, if any */
  do i = 1 to 7:
    find first mach {&where-machine} and mach.m-code eq style.m-code[i]
      no-lock no-error.
    if avail mach then do:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
      release mach.
    end.
    if avail mach then
    leave.
  end.
  /* find 1st valid machine in mach file */
  if not avail mach then
  find first mach {&where-machine} and mach.m-code eq xef.m-code
    no-lock no-error.
  if avail mach then do:
    if mach.p-type eq "R" then do:
      find first m-lst where m-lst.seq eq 10 no-error.
      if avail m-lst then
      delete m-lst.
    end.
    create m-lst.
    assign
      m-lst.seq     = (10 * mach.d-seq) + k
      m-lst.f-no    = xef.form-no
      m-lst.bl      = no
      m-lst.m-code  = mach.m-code
      m-lst.dscr    = mach.m-dscr
      m-lst.pass-no = k.
    do i = 1 to 4 :
      if mach.dept[i] ne "" or mach.dept[i] ne "PR" then do:
        m-lst.dept    = mach.dept[i].
        leave.
      end.
    end.
  end.
end.

/* get guillotine (gu dept after printing) */
if xef.n-out-l gt 1 then
do on error undo:
  release mach.
  /* find guillotineentered in style file, if any */
  do i = 1 to 7:
    if style.m-code[i] = "" then next.
    find first mach {&where-machine}            and
      mach.m-code  = style.m-code[i] no-lock no-error.
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
            and ((xef.xgrain eq "S"                    and
                  mach.min-len             le sh-wid   and
                  mach.min-wid             le sh-len   and
                  mach.max-len             ge sh-wid   and
                  mach.max-wid             ge sh-len)       or
                 (xef.xgrain ne "S"                    and
                  mach.min-len             le sh-len   and
                  mach.min-wid             le sh-wid   and
                  mach.max-len             ge sh-len   and
                  mach.max-wid             ge sh-wid))
      then
    leave.
  end.
  if not avail mach then do:
    for each mach
        {&where-machine}
          and mach.dept[1] eq "GU"
          and mach.min-cal le xef.cal
          and mach.max-cal ge xef.cal
          and ((xef.xgrain eq "S"                    and
                mach.min-len             le sh-wid   and
                mach.min-wid             le sh-len   and
                mach.max-len             ge sh-wid   and
                mach.max-wid             ge sh-len)       or
               (xef.xgrain ne "S"                    and
                mach.min-len             le sh-len   and
                mach.min-wid             le sh-wid   and
                mach.max-len             ge sh-len   and
                mach.max-wid             ge sh-wid))
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
      m-lst.f-no    = xef.form-no
      m-lst.seq    = 10 * mach.d-seq
      m-lst.b-no   = 0
      m-lst.dept   = "GU"
      m-lst.bl     = no
      m-lst.m-code = mach.m-code
      m-lst.dscr   = mach.m-dscr
      m-lst.n-out  = xef.n-out-l.
  end.
end.

assign
 sh-len = xef.trim-w
 sh-wid = xef.trim-l.

/* laminating */
if xef.medium ne "" or xef.flute ne "" or xef.lam-code ne "" then
lamin:
do on error undo:
  if xef.trim-pen ne 0 then do:
    xcal = xef.trim-pen / 1000. /* trim-pen stored as int */
    if xef.cal ne 0 and xef.cal ne .014 then
    xcal = xcal - .014 + xef.cal.
  end.
  else
  xcal = xef.cal.
  /* find laminator entered in style file, if any */
  if avail mach then
  release mach.
  do i = 1 to 7:
    find first mach {&where-machine}            and
      mach.m-code  eq style.m-code[i] no-lock no-error.
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
    if avail mach and
      ((xef.roll eq yes  and xef.lam-dscr eq "S" and
      (mach.min-len le sh-wid  and
      mach.min-wid le sh-len  and
      mach.min-cal le xcal      and
      mach.max-cal ge xcal      and
      mach.max-len ge sh-wid  and
      mach.max-wid ge sh-len     ))   or
      (mach.min-len le sh-len  and
      mach.min-wid le sh-wid  and
      mach.min-cal le xcal      and
      mach.max-cal ge xcal      and
      mach.max-len ge sh-len  and
      mach.max-wid ge sh-wid     ))
      then
    leave.
  end.

  if not avail mach then do:
    for each mach {&where-machine}                  and
        mach.dept[1]  eq "LM"         and
        ((xef.roll eq yes  and xef.lam-dscr eq "S" and
        (mach.min-len le sh-wid  and
        mach.min-wid le sh-len  and
        mach.min-cal le xcal      and
        mach.max-cal ge xcal     and
        mach.max-len ge sh-wid  and
        mach.max-wid ge sh-len     ))   or
        (mach.min-len le sh-len  and
        mach.min-wid le sh-wid  and
        mach.min-cal le xcal      and
        mach.max-cal ge xcal      and
        mach.max-len ge sh-len  and
        mach.max-wid ge sh-wid     ))
        no-lock
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
      next.
      leave.
    end.
  end.
  if avail mach then do:
    create m-lst.
    assign
      m-lst.seq    = 10 * mach.d-seq
      m-lst.f-no   = xef.form-no
      m-lst.b-no   = 0
      m-lst.dept   = "LM"
      m-lst.bl     = no
      m-lst.m-code = mach.m-code.
  end.
end.

/* get die cutter */
diecut:
do on error undo:
  /* find die cutter entered in style file, if any */
  if avail mach then release mach.
  do i = 1 to 7:
    if style.m-code[i] eq "" then next.
    find first mach {&where-machine}            and
      mach.m-code  eq style.m-code[i] no-lock no-error.
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
    if avail mach and
      ((xef.lam-dscr eq "S" and
      (mach.min-len le sh-wid  and
      mach.min-wid le sh-len  and
      mach.min-cal le xcal      and
      mach.max-cal ge xcal      and
      mach.max-len ge sh-wid  and
      mach.max-wid ge sh-len     ))   or
      (mach.min-len le sh-len  and
      mach.min-wid le sh-wid  and
      mach.min-cal le xcal      and
      mach.max-cal ge xcal      and
      mach.max-len ge sh-len  and
      mach.max-wid ge sh-wid     ))
      then
    leave.
  end.
  if not avail mach then do:
    for each mach {&where-machine}                 and
        mach.dept[1]  eq "DC"        and
        ((xef.lam-dscr eq "S" and
        (mach.min-len le sh-wid  and
        mach.min-wid le sh-len  and
        mach.min-cal le xcal      and
        mach.max-cal ge xcal      and
        mach.max-len ge sh-wid  and
        mach.max-wid ge sh-len     ))   or
        (mach.min-len le sh-len  and
        mach.min-wid le sh-wid  and
        mach.min-cal le xcal      and
        mach.max-cal ge xcal      and
        mach.max-len ge sh-len  and
        mach.max-wid ge sh-wid     ))
        no-lock
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
      next.
      leave.
    end.
  end.
  if avail mach then do:
    create m-lst.
    assign
      m-lst.seq    = 10 * mach.d-seq
      m-lst.f-no   = xef.form-no
      m-lst.b-no   = 0
      m-lst.dept   = "DC"
      m-lst.bl     = no
      m-lst.m-code = mach.m-code.
  end.
end.

/* get auto strip */
hstrip:
do on error undo:
  do i = 1 to 7:
    find first mach {&where-machine} and
      mach.m-code  eq style.m-code[i] no-lock no-error.
    if avail mach then do:
      if mach.dept[1] ne "HS" and mach.dept[2] ne "HS" and
        mach.dept[3] ne "HS" and mach.dept[4] ne "HS" then do:
        release mach.
        next.
      end.
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
      release mach.
      if avail mach then leave.
    end.
  end.
  if not avail mach then do:
    for each mach {&where-machine}                 and
        mach.dept[1]  eq "HS"        and
        ((xef.roll eq yes  and xef.lam-dscr eq "S" and
        (mach.min-len le sh-wid  and
        mach.min-wid le sh-len  and
        mach.min-cal le xcal      and
        mach.max-cal ge xcal      and
        mach.max-len ge sh-wid  and
        mach.max-wid ge sh-len     ))   or
        (mach.min-len le sh-len  and
        mach.min-wid le sh-wid  and
        mach.min-cal le xcal      and
        mach.max-cal ge xcal      and
        mach.max-len ge sh-len  and
        mach.max-wid ge sh-wid     ))
        no-lock
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
      next.
      leave.
    end.
  end.
  if avail mach then do:
    create m-lst.
    assign
      m-lst.seq    = 10 * mach.d-seq
      m-lst.f-no   = xef.form-no
      m-lst.b-no   = 0
      m-lst.dept   = "HS"
      m-lst.bl     = no
      m-lst.m-code = mach.m-code.
  end.
end.

/* get window or foil */
window-foil:
do on error undo:
  do i = 1 to 4:
    if xef.leaf[i] eq "" then next.
    
    release mach.
    
    find first item
        {sys/look/itemlwW.i}
          and item.i-no eq xef.leaf[i]
        no-lock no-error.
                                                           
    if avail item then do:
      v-dept = if item.mat-type eq "W" then
                 if xef.leaf-bnum[i] eq 0 then "WS" else "WN"
               else
               if item.mat-type eq "F" then
                 if xef.leaf-bnum[i] eq 0 then "FS" else "FB"
               else "".
               
      if v-dept eq "" then next.         
      
      find first bf-eb
          where bf-eb.company = xest.company
            AND bf-eb.est-no   eq xef.est-no
            and bf-eb.form-no  eq xef.leaf-snum[i]
            and bf-eb.blank-no eq xef.leaf-bnum[i]
          no-lock no-error.
          
      release xstyle.
    
      if avail bf-eb then
      find first xstyle
          where xstyle.company eq cocode
            and xstyle.style   eq bf-eb.style
          no-lock no-error.
        
      if avail bf-eb and avail xstyle then
      do j = 1 to 7:
        if xstyle.m-code[i] eq "" then next.
    
        find first mach
            {&where-machine}
              and mach.m-code eq xstyle.m-code[j]
            no-lock no-error.
        if avail mach and
           not (mach.dept[1] eq v-dept or mach.dept[2] eq v-dept  or
                mach.dept[3] eq v-dept or mach.dept[4] eq v-dept) then do:
          release mach.
        end.
        if avail mach then do:
          run ce/mach-qty.p (ROWID(mach)).
          if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
            release mach.
        end.
        if avail mach then leave.
      end.
      
      if not avail mach then
      for each mach
          {&where-machine}
            and mach.dept[1] eq v-dept
            and ((xef.leaf-bnum[i] gt 0     and
                  mach.min-len le xeb.t-len and
                  mach.min-wid le xeb.t-wid and
                  mach.max-len ge xeb.t-len and
                  mach.max-wid ge xeb.t-wid) or
                 (xef.leaf-bnum[i] eq 0     and
                  mach.min-len le sh-len  and
                  mach.min-wid le sh-wid  and
                  mach.max-len ge sh-len  and
                  mach.max-wid ge sh-wid))
            and mach.min-cal le xcal
            and mach.max-cal ge xcal
          no-lock
          BY mach.d-seq
          BY mach.m-seq:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
          release mach.
        else leave.
      end.
      
      if avail mach then
      for each alt-xeb
          where alt-xeb.company = xest.company
            AND alt-xeb.est-no   eq xest.est-no
            and alt-xeb.form-no eq xef.form-no
          no-lock:
        create m-lst.
        assign
         m-lst.f-no   = xef.form-no
         m-lst.b-no   = if xef.leaf-bnum[i] gt 0 then alt-xeb.blank-no else 0
         m-lst.seq    = (10 * mach.d-seq) + i + alt-xeb.blank-no
         m-lst.dept   = v-dept
         m-lst.m-code = mach.m-code
         m-lst.dscr   = mach.m-dscr
         m-lst.bl     = xef.leaf-bnum[i] gt 0.
        
        if xef.leaf-bnum[i] eq 0 then leave. 
      end.
    end.
  end. /* 1 to 4 */
end.

/* get gluer */
glu:
do on error undo:
  if xeb.adhesive ne "" or xeb.gluelap  ne 0  then do:
    ll-style = NO.
    if avail mach then release mach.
    do i = 1 to 7:
      find first mach {&where-machine} and
        mach.m-code  eq style.m-code[i] no-lock no-error.
      if avail mach then do:
        if mach.dept[1] ne "GL" and mach.dept[2] ne "GL" and
          mach.dept[3] ne "GL" and mach.dept[4] ne "GL" then do:
          release mach.
          next.
        end.
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
        IF AVAIL mach THEN
        FOR EACH alt-xeb
            WHERE alt-xeb.company EQ xef.company
              AND alt-xeb.est-no  EQ xef.est-no
            NO-LOCK:
          ll-style = YES.
          RUN create-m-lst ("GL", xef.form-no, alt-xeb.blank-no, YES).
        END.
      end.
    end.
    IF NOT ll-style THEN
    for each mach {&where-machine}                 and
          mach.dept[1]  eq "GL"        and
          mach.min-len le xeb.t-len   and
          mach.min-wid le xeb.t-wid   and
          mach.min-cal le xcal     and
          mach.max-len ge xeb.t-len   and
          mach.max-wid ge xeb.t-wid   and
          mach.max-cal ge xcal
          no-lock
          BY mach.d-seq
          BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then next.
      ELSE DO:
        FOR EACH alt-xeb
            WHERE alt-xeb.company EQ xef.company
              AND alt-xeb.est-no  EQ xef.est-no
            NO-LOCK:
          RUN create-m-lst ("GL", xef.form-no, alt-xeb.blank-no, NO).
        END.
        LEAVE.
      END.
    end.
  end.
end.

for each xef where xef.company = xest.company
               AND xef.est-no eq xest.est-no:
  for each xeb
      where xeb.company = xest.company
        AND xeb.est-no   eq xest.est-no
        and xeb.form-no eq xef.form-no
      no-lock:
    find first style
        where style.company eq xeb.company
          and style.style   eq xeb.style
        no-lock no-error.
    if not avail style then next.

    /* get any other mach in style file */
    sty:
    do on error undo:
      if avail mach then release mach.
      do i = 1 to 7:
        if style.m-code[i] eq "" then next.
        find first mach {&where-machine}                     and
          mach.m-code   eq style.m-code[i] and
          ((mach.p-type eq "B" and
          mach.min-len le xeb.t-len   and
          mach.min-wid le xeb.t-wid   and
          mach.min-cal le xcal     and
          NOT CAN-FIND(first m-lst
                       where ((m-lst.dept  eq mach.dept[1] AND NOT m-lst.styl) OR
                              m-lst.m-code EQ style.m-code[i])
                         and m-lst.f-no    eq xef.form-no
                         and ((mach.p-type eq "B" and m-lst.b-no eq xeb.blank-no) or
                               mach.p-type ne "B")) AND
          mach.max-len ge xeb.t-len   and
          mach.max-wid ge xeb.t-wid   and
          mach.max-cal ge xcal)
          or
          ((xef.roll eq yes  and xef.lam-dscr eq "S" and
          (mach.min-len le sh-wid  and
          mach.min-wid le sh-len  and
          mach.max-len ge sh-wid  and
          mach.max-wid ge sh-len     ))
          or
          (mach.min-len le sh-len  and
          mach.min-wid le sh-wid  and
          mach.max-len ge sh-len  and
          mach.max-wid ge sh-wid     ))
          )
          no-lock no-error.

        if avail mach then do:
          find first m-lst
              where m-lst.m-code eq style.m-code[i]
                and m-lst.f-no   eq xef.form-no
                and (if mach.p-type eq "B" or mach.dept[1] eq "PR" then
                       m-lst.b-no eq xeb.blank-no else m-lst.b-no eq 0)
              no-lock no-error.
          if avail m-lst then next.

          find first m-lst
              where mach.dept[1] eq "PR"
                and m-lst.dept   eq "PR"
                and m-lst.f-no   eq xef.form-no
                and m-lst.b-no   eq xeb.blank-no
              no-lock no-error.
          if avail m-lst then next.

          run ce/mach-qty.p (ROWID(mach)).
          if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then next.
          create m-lst.
          assign
           m-lst.f-no   = xef.form-no
           m-lst.b-no   = if mach.p-type eq "B" or mach.dept[1] eq "PR" then
                            xeb.blank-no else 0
           m-lst.seq    = 10 * mach.d-seq
           m-lst.bl     = no
           m-lst.dept   = mach.dept[1]
           m-lst.m-code = style.m-code[i]
           m-lst.dscr   = mach.m-dscr
           m-lst.bl     = mach.p-type eq "B"
           m-lst.styl   = YES.
        end.
      end.
    end.
  end.
end.
END.

run ce/tan/mach-sq2.p.

hide frame kalk no-pause.

RETURN.

PROCEDURE create-m-lst.
  
  DEF INPUT PARAM ip-dept  LIKE m-lst.dept NO-UNDO.
  DEF INPUT PARAM ip-f-no  LIKE m-lst.f-no NO-UNDO.
  DEF INPUT PARAM ip-b-no  LIKE m-lst.b-no NO-UNDO.
  DEF INPUT PARAM ip-style AS LOG          NO-UNDO.


  CREATE m-lst.
  ASSIGN
   m-lst.seq    = 10 * mach.d-seq
   m-lst.dept   = ip-dept
   m-lst.m-code = mach.m-code
   m-lst.dscr   = mach.m-dscr
   m-lst.f-no   = ip-f-no
   m-lst.b-no   = ip-b-no
   m-lst.bl     = mach.p-type EQ "B"
   m-lst.styl   = ip-style.

END PROCEDURE.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
