/* --------------------------------------------------- ce/box/mach-seq.p  */
/* create machine routing sequence                                        */
/* ---------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

DEF BUFFER xeb-2 FOR eb.

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

for each xef
    where xef.company eq xest.company
      and xef.est-no  eq xest.est-no:

  xcal = xef.cal.
  assign
   sh-len = if xef.roll then xef.gsh-wid else xef.nsh-wid
   sh-wid = if xef.roll then xef.gsh-len else xef.nsh-len.

  if xef.gsh-qty ne 0 then
  assign v-sht-qty = xef.gsh-qty.
  else
  v-sht-qty = xeb.bl-qty / (xeb.num-up * xef.n-out).

  find first xeb
      where xeb.company eq xef.company
        and xeb.est-no  eq xef.est-no
        and xeb.form-no eq xef.form-no
      no-lock no-error.

  find first style {sys/ref/styleW.i} and
    style.style   = xeb.style no-lock no-error.

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
       m-lst.f-no   = xef.form-no
       m-lst.seq    = 10 * mach.d-seq
       m-lst.dept   = "FO"
       m-lst.bl     = YES
       m-lst.m-code = mach.m-code.

      RELEASE m-lst.
    END.
  END.

  ELSE DO:                    /* Manufactured FG */
  /* need sheeter? */
  if xef.roll = yes then
  do:
    /* find sheeter entered in style file, if any */
    if avail mach then
    release mach.
    do i = 1 to 7:
      find first mach {&where-machine}            and
        mach.m-code  = style.m-code[i] no-lock no-error.
      if avail mach and (mach.dept[1] ne "RS") and
        (mach.dept[2] ne "RS") and
        (mach.dept[3] ne "RS") and
        (mach.dept[4] ne "RS")
        then
      do:
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
      else release mach.
    end.
    if not avail mach then
    do:
      for each mach {&where-machine}             and
          mach.dept[1]  = "RS"    and
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
        if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
          release mach.
        else leave.
      end.
    end.
    if avail mach then
    do:
      create m-lst.
      assign m-lst.f-no   = xef.form-no
        m-lst.seq    = 10 * mach.d-seq
        m-lst.dept   = "RS"
        m-lst.bl     = NO
        m-lst.m-code = mach.m-code.

      RELEASE m-lst.
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
        mach.m-code  = style.m-code[i] no-lock no-error.
      if avail mach and (mach.dept[1] ne "RC") and
        (mach.dept[2] ne "RC") and
        (mach.dept[3] ne "RC") and
        (mach.dept[4] ne "RC")
        then
      do:
        release mach.
        next.
      end.
      if avail mach then
      do:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
        release mach.
      end.
      if avail mach and
        (mach.p-type EQ "R" OR xef.n-cuts NE 0) AND
        ((xef.roll = yes  and xef.lam-dscr = "S" and
        (mach.min-len le xef.gsh-wid  and
        mach.min-wid le xef.gsh-len  and
        mach.min-cal le xcal      and
        mach.max-cal ge xcal      and
        mach.max-len ge xef.gsh-wid  and
        mach.max-wid ge xef.gsh-len     ))   OR
        (mach.min-len le xef.gsh-len  and
        mach.min-wid le xef.gsh-wid  and
        mach.min-cal le xcal      and
        mach.max-cal ge xcal      and
        mach.max-len ge xef.gsh-len  and
        mach.max-wid ge xef.gsh-wid     ))
        then
      leave.
    end.
    if not avail mach then
    do:
      for each mach {&where-machine}                  and
          mach.dept[1]  = "RC"         and
          (mach.p-type EQ "R" OR xef.n-cuts NE 0) AND
          ((xef.roll = yes  and xef.lam-dscr = "S" and
          (mach.min-len le xef.gsh-wid  and
          mach.min-wid le xef.gsh-len  and
          mach.min-cal le xcal      and
          mach.max-cal ge xcal      and
          mach.max-len ge xef.gsh-wid  and
          mach.max-wid ge xef.gsh-len     ))   OR
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
        if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
          release mach.
        else leave.
      end.
    end.
    if avail mach then
    do:
      create m-lst.
      assign
        m-lst.f-no   = xef.form-no
        m-lst.seq    = 10 * mach.d-seq
        m-lst.dept   = "RC"
        m-lst.bl     = NO
        m-lst.m-code = mach.m-code
        m-lst.n-out  = xef.n-out
        v-rc-seq     = m-lst.seq.

      RELEASE m-lst.

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
  if xef.f-pass gt 0 then
  for each eb
      where eb.company eq xef.company
        and eb.est-no  eq xef.est-no
        and eb.form-no eq xef.form-no:

    find first item {sys/look/itemivW.i} and
         item.i-no    = eb.i-code2[1] no-lock no-error.
    if avail item then
       find first e-item of item no-lock no-error.
    else
    next.
    /*
    if int((xef.f-col + xef.f-coat ) / xef.f-pass) lt
    ((xef.f-col + xef.f-coat ) / xef.f-pass)
    then maxco = int((xef.f-col + xef.f-coat ) / xef.f-pass) + 1.
    else maxco = int((xef.f-col + xef.f-coat ) / xef.f-pass).
    */
    if xef.f-coat gt 0 then
    do k = 1 to xef.f-coat:
      /* find coater entered in style file, if any */
      if avail mach then
      release mach.
      do i = 1 to 7:
        find first mach {&where-machine} and mach.m-code = style.m-code[i]
          and mach.dept[1]  = "CT"         and
          ((xef.roll = yes  and xef.lam-dscr = "S" and
          (mach.min-len le sh-wid  and
          mach.min-wid le sh-len  and
          mach.min-cal le xef.cal      and
          mach.max-cal ge xef.cal      and
          mach.max-len ge sh-wid  and
          mach.max-wid ge sh-len     ))   OR
          (mach.min-len le sh-len  and
          mach.min-wid le sh-wid  and
          mach.min-cal le xef.cal      and
          mach.max-cal ge xef.cal      and
          mach.max-len ge sh-len  and
          mach.max-wid ge sh-wid     ))
          no-lock no-error.
        if avail mach then
        do:
          run ce/mach-qty.p (ROWID(mach)).
          if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
          release mach.
        end.
        if avail mach then
        leave.
      end.
      /* find 1st valid machine in mach file */
      if not avail mach then
      for each mach {&where-machine}                  and
          mach.dept[1]  = "CT"         and
          ((xef.roll = yes  and xef.lam-dscr = "S" and
          (mach.min-len le sh-wid  and
          mach.min-wid le sh-len  and
          mach.min-cal le xef.cal      and
          mach.max-cal ge xef.cal      and
          mach.max-len ge sh-wid  and
          mach.max-wid ge sh-len     ))   OR
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
        if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
          release mach.
        else leave.
      end.
      if avail mach then
      do:
        create m-lst.
        assign
          m-lst.f-no = xef.form-no
          m-lst.seq = (10 * mach.d-seq) + k
          m-lst.bl  = NO
          m-lst.dept = "CT"
          m-lst.m-code = mach.m-code
          m-lst.dscr = mach.m-dscr
          m-lst.pass-no = k.

        RELEASE m-lst.
      end.
    end. /* avail item... */
    /* find press */
    if avail item then
    prez:
    REPEAT:
      find first m-lst where m-lst.f-no = xef.form-no and
                             m-lst.dept = "CT" no-error.
      if not avail m-lst then
      do:
        if int((xef.f-col + xef.f-coat ) / xef.f-pass) lt
          ((xef.f-col + xef.f-coat ) / xef.f-pass)
          then
        maxco = int((xef.f-col + xef.f-coat ) / xef.f-pass) + 1.
        else
        maxco = int((xef.f-col + xef.f-coat ) / xef.f-pass).
      end.
      else
      do:
        if int(xef.f-col / xef.f-pass) lt (xef.f-col / xef.f-pass)
          then
        maxco = int(xef.f-col / xef.f-pass) + 1.
        else
        maxco = int(xef.f-col / xef.f-pass).
      end.
      if avail mach then
      release mach.
      /* find machine entered in style file, if any */
      do i = 1 to 7:
        find first mach {&where-machine}                                and
          mach.m-code  = style.m-code[i]             and
          mach.pr-type =  item.press-type            and
          mach.max-color ge maxco                    and
          ((xef.roll = yes  and xef.lam-dscr = "S" and
          (mach.min-len le sh-wid  and
          mach.min-wid le sh-len  and
          mach.min-cal le xef.cal      and
          mach.max-cal ge xef.cal      and
          mach.max-len ge sh-wid  and
          mach.max-wid ge sh-len     ))   OR
          (mach.min-len le sh-len  and
          mach.min-wid le sh-wid  and
          mach.min-cal le xef.cal      and
          mach.max-cal ge xef.cal      and
          mach.max-len ge sh-len  and
          mach.max-wid ge sh-wid     ))
          no-lock no-error.
        if avail mach then
        do:
          run ce/mach-qty.p (ROWID(mach)).
          if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
          release mach.
        end.
        if avail mach then
        leave.
      end.
      /* find 1st valid machine in mach file */
      if not avail mach then
      for each mach {&where-machine}                                and
          mach.pr-type =  item.press-type            and
          mach.max-color ge maxco                    and
          ((xef.roll = yes  and xef.lam-dscr = "S" and
          (mach.min-len le sh-wid  and
          mach.min-wid le sh-len  and
          mach.min-cal le xef.cal      and
          mach.max-cal ge xef.cal      and
          mach.max-len ge sh-wid  and
          mach.max-wid ge sh-len     ))   OR
          (mach.min-len le sh-len  and
          mach.min-wid le sh-wid  and
          mach.min-cal le xef.cal      and
          mach.max-cal ge xef.cal      and
          mach.max-len ge sh-len  and
          mach.max-wid ge sh-wid     ))
          NO-LOCK
          BY mach.max-color
          BY mach.d-seq
          BY mach.m-seq:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
          release mach.
        else leave.
      end.

      if avail mach then
      do:
        if mach.p-type = "R" then
        do:
          find first m-lst where m-lst.f-no = xef.form-no and
                                 m-lst.dept = "RS" no-error.
          if avail m-lst then
          delete m-lst.
        end.
        if mach.coater = yes and mach.max-color * xef.f-pass gt xef.f-col then
        do:
          find last m-lst where m-lst.dept = "CT"  no-error.
          if avail m-lst then
          delete m-lst.
          /* delete only 1 coater... */
        end.

        do k = 1 to xef.f-pass:
          create m-lst.
          assign
            m-lst.f-no    = xef.form-no
            m-lst.seq     = (10 * mach.d-seq) + k
            m-lst.bl      = NO
            m-lst.dept    = "PR"
            m-lst.m-code  = mach.m-code
            m-lst.dscr    = mach.m-dscr
            m-lst.pass-no = k.

          RELEASE m-lst.
        end.
      end.
      leave.    /* fake repeat */
    end. /* avail item... */
    leave.
  end. /* if f-pass gt 0 */
  else
  do:
    k = 1.
    /* find machine entered in style file, if any */
    do i = 1 to 7:
      find first mach {&where-machine} and mach.m-code = style.m-code[i]
        no-lock no-error.
      if avail mach then
      do:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
        release mach.
      end.
      if avail mach then
      leave.
    end.
    /* find 1st valid machine in mach file */
    if not avail mach then
    find first mach {&where-machine} and mach.m-code = xef.m-code
      no-lock no-error.
    if avail mach and can-do("RC,PR",mach.dept[1]) then do:
      if mach.p-type = "R" or xef.roll = no then do:
        find first m-lst where m-lst.dept = "RS" no-error.
        if avail m-lst then
        delete m-lst.
      end.
      create m-lst.
      assign
        m-lst.f-no    = xef.form-no
        m-lst.seq     = (10 * mach.d-seq) + k
        m-lst.bl      = mach.p-type eq "B"
        m-lst.b-no    = if m-lst.bl then xeb.blank-no else 0
        m-lst.m-code  = mach.m-code
        m-lst.dscr    = mach.m-dscr
        m-lst.pass-no = k.
      do i = 1 to 4 :
         if mach.dept[i] ne "" and mach.dept[i] ne "PR" then
         do:
           m-lst.dept    = mach.dept[i].
           leave.
         end.
      end.

      IF m-lst.dept EQ "" AND mach.dept[1] EQ "PR" THEN
         m-lst.dept = "PR".

      RELEASE m-lst.
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
         m-lst.f-no   = xef.form-no
         m-lst.seq    = 10 * mach.d-seq
         m-lst.dept   = "GU"
         m-lst.bl     = no
         m-lst.m-code = mach.m-code
         m-lst.dscr   = mach.m-dscr
         m-lst.n-out  = xef.n-out-l.
      
       RELEASE m-lst.
    end.
  end.

  /* laminating */
  if xef.medium ne "" OR xef.flute ne "" OR xef.lam-code ne "" then
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
    if avail mach then
    release mach.
    /* find laminator entered in style file, if any */
    do i = 1 to 7:
      find first mach {&where-machine}            and
        mach.m-code  = style.m-code[i] no-lock no-error.
      if avail mach and (mach.dept[1] ne "LM") and
        (mach.dept[2] ne "LM") and
        (mach.dept[3] ne "LM") and
        (mach.dept[4] ne "LM")
        then
      do:
        release mach.
        next.
      end.
      if avail mach then
      do:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
        release mach.
      end.
      if avail mach and
        ((xef.roll = yes  and xef.lam-dscr = "S" and
        (mach.min-len le sh-wid  and
        mach.min-wid le sh-len  and
        mach.min-cal le xcal      and
        mach.max-cal ge xcal      and
        mach.max-len ge sh-wid  and
        mach.max-wid ge sh-len     ))   OR
        (mach.min-len le sh-len  and
        mach.min-wid le sh-wid  and
        mach.min-cal le xcal      and
        mach.max-cal ge xcal      and
        mach.max-len ge sh-len  and
        mach.max-wid ge sh-wid     ))
        then
      leave.
    end.

    if not avail mach then
    do:
      for each mach {&where-machine}                  and
          mach.dept[1]  = "LM"         and
          ((xef.roll = yes  and xef.lam-dscr = "S" and
          (mach.min-len le sh-wid  and
          mach.min-wid le sh-len  and
          mach.min-cal le xcal      and
          mach.max-cal ge xcal      and
          mach.max-len ge sh-wid  and
          mach.max-wid ge sh-len     ))   OR
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
        if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
          release mach.
        else leave.
      end.
    end.
    if avail mach then
    do:
      create m-lst.
      assign
        m-lst.f-no   = xef.form-no
        m-lst.seq    = 10 * mach.d-seq
        m-lst.dept   = "LM"
        m-lst.bl     = NO
        m-lst.m-code = mach.m-code.

      RELEASE m-lst.
    end.
  end.

  assign
   sh-len = xef.trim-w
   sh-wid = xef.trim-l.

  /* get die cutter */
  diecut:
  do on error undo:
    release mach.
    /* find die cutter entered in style file, if any */
    do i = 1 to 7:
      find first mach {&where-machine}            and
        mach.m-code  = style.m-code[i] no-lock no-error.
      if avail mach and (mach.dept[1] ne "DC" ) and
        (mach.dept[2] ne "DC" ) and
        (mach.dept[3] ne "DC" ) and
        (mach.dept[4] ne "DC" )
        then
      do:
        release mach.
        next.
      end.
      if avail mach then
      do:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
        release mach.
      end.
      if avail mach and
        /*((xef.roll = yes  and xef.lam-dscr = "S" and
        (mach.min-len le sh-wid  and
        mach.min-wid le sh-len  and
        mach.min-cal le xcal      and
        mach.max-cal ge xcal     and
        mach.max-len ge sh-wid  and
        mach.max-wid ge sh-len     ))   OR
        (*/ mach.min-len le sh-len  and
        mach.min-wid le sh-wid  and
        mach.min-cal le xcal      and
        mach.max-cal ge xcal      and
        mach.max-len ge sh-len  and
        mach.max-wid ge sh-wid     /*))*/
        then
      leave.
    end.
    if not avail mach THEN
      for each mach {&where-machine}                 and
          mach.dept[1]  = "DC"        and
          ((xef.roll = yes  and xef.lam-dscr = "S" and
          (mach.min-len le sh-wid  and
          mach.min-wid le sh-len  and
          mach.min-cal le xcal      and
          mach.max-cal ge xcal      and
          mach.max-len ge sh-wid  and
          mach.max-wid ge sh-len     ))   OR
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
        if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
          release mach.
        else leave.
    end.
    if avail mach then
    do:
      create m-lst.
      assign
        m-lst.f-no   = xef.form-no
        m-lst.seq    = 10 * mach.d-seq
        m-lst.dept   = "DC"
        m-lst.bl     = NO
        m-lst.m-code = mach.m-code.

      RELEASE m-lst.
    end.
  end.

  /* get auto strip */
  if xef.board ne "" then
  hstrip:
  do on error undo:
    if avail mach then
    release mach.
    do i = 1 to 7:
      find first mach {&where-machine} and
        mach.m-code  = style.m-code[i] no-lock no-error.
      if avail mach then
      do:
        if mach.dept[1] ne "HS" and mach.dept[2] ne "HS" and
          mach.dept[3] ne "HS" and mach.dept[4] ne "HS"
          then
        do:
          release mach.
          next.
        end.
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
        release mach.
        if avail mach then
        leave.
      end.
    end.
    if not avail mach then
    do:
      for each mach {&where-machine}                 and
          mach.dept[1]  = "HS"        and
          ((xef.roll = yes  and xef.lam-dscr = "S" and
          (mach.min-len le sh-wid  and
          mach.min-wid le sh-len  and
          mach.min-cal le xcal      and
          mach.max-cal ge xcal      and
          mach.max-len ge sh-wid  and
          mach.max-wid ge sh-len     ))   OR
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
        if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
          release mach.
        else leave.
      end.
    end.
    if avail mach then
    do:
      create m-lst.
      assign
        m-lst.f-no   = xef.form-no
        m-lst.seq    = 10 * mach.d-seq
        m-lst.dept   = "HS"
        m-lst.bl     = NO
        m-lst.m-code = mach.m-code.

      RELEASE m-lst.
    end.
  end.

  /* get window or foil */
  {ce/mach-wof.i}
     
  /* get gluer */
  glu:
  for each xeb
      where xeb.company eq xef.company
        and xeb.est-no  eq xef.est-no
        and xeb.form-no eq xef.form-no
      NO-LOCK:

    find first style {sys/ref/styleW.i} and
      style.style   = xeb.style no-lock no-error.

    if xeb.adhesive ne "" OR
      xeb.gluelap  ne 0  OR
      xeb.lin-in   ne 0  then
    do:

/* JLF added 03/25/96 */
      find first item
          where item.company eq cocode
            and item.i-no    eq xeb.adhesive
          no-lock no-error.

      if avail item and item.mat-type eq "T" then next glu.
/* JLF added 03/26/96 */

      ll-style = NO.
      if avail mach then
      release mach.
      do i = 1 to 7:
        find first mach {&where-machine} and
          mach.m-code  = style.m-code[i] no-lock no-error.
        if avail mach then
        do:
          if mach.dept[1] ne "GL" and mach.dept[2] ne "GL" and
            mach.dept[3] ne "GL" and mach.dept[4] ne "GL" then
          do:
            release mach.
            next.
          end.
          run ce/mach-qty.p (ROWID(mach)).
          if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
          release mach.
          IF AVAIL mach THEN DO:
            ll-style = YES.
            RUN create-m-lst ("GL", xef.form-no, xeb.blank-no, YES).
          END.
        end.
      end.
      IF NOT ll-style THEN
      for each mach {&where-machine}                and
            mach.dept[1]  = "GL"        and
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
        if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
          release mach.
        ELSE DO:
          RUN create-m-lst ("GL", xef.form-no, xeb.blank-no, NO).
          LEAVE.
        END.
      end.
    end.
  end.

/* JLF added 03/26/96 */
  /* get Quad Stayer */
  quadstayer:
  for each xeb
      where xeb.company eq xef.company
        and xeb.est-no  eq xef.est-no
        and xeb.form-no eq xef.form-no
      NO-LOCK:

    find first style {sys/ref/styleW.i} and
      style.style   = xeb.style no-lock no-error.

    if xeb.adhesive ne "" then
    do:
      find first item
          where item.company eq cocode
            and item.i-no    eq xeb.adhesive
          no-lock no-error.

      if not avail item or item.mat-type ne "T" then next quadstayer.

      if avail mach then
      release mach.
      do i = 1 to 7:
        find first mach {&where-machine} and
          mach.m-code  = style.m-code[i] no-lock no-error.
        if avail mach then
        do:
          if mach.dept[1] ne "QS" and mach.dept[2] ne "QS" and
            mach.dept[3] ne "QS" and mach.dept[4] ne "QS" then
          do:
            release mach.
            next.
          end.
          run ce/mach-qty.p (ROWID(mach)).
          if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
          release mach.
          if avail mach then
          leave.
        end.
      end.
      if not avail mach then
      do:
        for each mach {&where-machine}                and
            mach.dept[1]  = "QS"        and
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
          if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
            release mach.
          else leave.
        end.
      end.

      if avail mach then
      do:
        create m-lst.
        assign
          m-lst.f-no = xef.form-no
          m-lst.b-no = xeb.blank-no
          m-lst.seq = (10 * mach.d-seq) + xeb.blank-no
          m-lst.bl  = yes
          m-lst.dept = "QS"
          m-lst.m-code = mach.m-code
          m-lst.dscr = mach.m-dscr.

        RELEASE m-lst.
      end.
    end.
  end.
/* JLF added 03/26/96 */

  find first xeb
      where xeb.company eq xef.company
        and xeb.est-no  eq xef.est-no
        and xeb.form-no eq xef.form-no
      no-lock no-error.

  FOR EACH xeb-2 FIELDS(style blank-no t-len t-wid)
      where xeb-2.company eq xef.company
        and xeb-2.est-no  eq xef.est-no
        and xeb-2.form-no eq xef.form-no
      no-lock:
  
  /* get any other mach in style file */
  sty:
  do on error undo:

     find first style where
          style.company eq cocode and
          style.style   = xeb-2.style
          no-lock no-error.
    
     do i = 1 to 7:
        if style.m-code[i] = "" then next.
        find first mach {&where-machine}  and
          mach.m-code   = style.m-code[i] and
          NOT CAN-FIND(first m-lst
                       where ((m-lst.dept  eq mach.dept[1] AND NOT m-lst.styl) OR
                              m-lst.m-code EQ style.m-code[i])
                         and m-lst.f-no    eq xef.form-no
                         and ((mach.p-type eq "B" and m-lst.b-no eq xeb-2.blank-no) or
                               mach.p-type ne "B")) AND
          ((mach.p-type = "B" and
          mach.min-len le xeb-2.t-len   and
          mach.min-wid le xeb-2.t-wid   and
          mach.min-cal le xcal     and
          mach.max-len ge xeb-2.t-len   and
          mach.max-wid ge xeb-2.t-wid   and
          mach.max-cal ge xcal)
          OR
        ((xef.roll = yes  and xef.lam-dscr = "S" and
        (mach.min-len le sh-wid  and
        mach.min-wid le sh-len  and
        mach.max-len ge sh-wid  and
        mach.max-wid ge sh-len     ))
        OR
        (mach.min-len le sh-len  and
        mach.min-wid le sh-wid  and
        mach.max-len ge sh-len  and
        mach.max-wid ge sh-wid     ))
        )
          /* CTS end */
          no-lock no-error.
        if avail mach then
        do:
           run ce/mach-qty.p (ROWID(mach)).
           if mach.min-run gt v-chk-qty OR mach.max-run lt v-chk-qty then
              release mach.
        end.
       
        if avail mach then
        do:
           create m-lst.
           assign
             m-lst.f-no   = xef.form-no
             m-lst.seq    = 10 * mach.d-seq
             m-lst.bl     = mach.p-type eq "B"
             m-lst.b-no   = if m-lst.bl then xeb-2.blank-no else 0
             m-lst.dept   = mach.dept[1]
             m-lst.m-code = mach.m-code
             m-lst.dscr   = mach.m-dscr
             m-lst.styl   = YES.

           if mach.p-type = "B" then
              m-lst.bl = yes.
           if mach.dept[1] = "AS" then
              m-lst.b-no = 1.

           RELEASE m-lst.
        end.
     end.
  end.
  END. /*each xeb-2*/
  END.  /* Manufactured FG */
end.

run ce/box/mach-sq2.p .

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

  RELEASE m-lst.

END PROCEDURE.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
