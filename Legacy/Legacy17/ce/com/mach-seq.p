/* ------------------------------------------------ce/com/mach-seq.p 9/94 gb  */
/* create machine routing sequence                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-form-no LIKE ef.form-no NO-UNDO.

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def        buffer alt-xeb for eb.
DEF BUFFER bf-eb FOR eb.
def new shared var sh-wid as de NO-UNDO.
def new shared var sh-len as de NO-UNDO.
DEF VAR sh-hld LIKE sh-len NO-UNDO.
def new shared var maxco as int no-undo.
def new shared var v-2 as log init false NO-UNDO.
def new shared var v-chk-qty as dec no-undo.
def new shared var v-rc-seq as int init 9999 no-undo.
def new shared var v-sht-qty as dec no-undo.
def shared var xcal as de NO-UNDO.
def var v-lin-ft as de no-undo.
def var v-dept like dept.code no-undo.
DEF VAR ll-style AS LOG NO-UNDO.

DEF NEW SHARED VAR qty AS INT NO-UNDO.

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


SESSION:SET-WAIT-STATE("general").

run ce/mach-ink.p.

for each xef where xef.company = xest.company
               AND xef.est-no = xest.est-no
               AND (xef.form-no = ip-form-no OR ip-form-no = 0):

  assign
   maxco  = (xef.f-col / xef.f-pass) + (xef.f-coat / xef.f-pass)
   xcal   = xef.cal
   sh-len = if xef.roll then xef.gsh-wid else xef.nsh-wid
   sh-wid = if xef.roll then xef.gsh-len else xef.nsh-len.

  find first xeb where xeb.company = xest.company and
                       xeb.est-no = xest.est-no  and
                       xeb.form-no = xef.form-no and
                       xeb.i-col gt 0 no-lock no-error.
  if not avail xeb then
     find first xeb where xeb.company = xest.company and
                          xeb.est-no = xest.est-no  and
                          xeb.form-no = xef.form-no no-lock no-error.

  if xef.gsh-qty ne 0 then   assign v-sht-qty = xef.gsh-qty.
  ELSE if xef.est-type = 1 THEN v-sht-qty = xest.est-qty[1] / (xeb.num-up * xef.n-out).
  ELSE v-sht-qty = xeb.yld-qty / (xeb.num-up * xef.n-out).

  v-lin-ft = xef.gsh-qty * (sh-len / 12).

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
    END.
  END.

  ELSE DO:                    /* Manufactured FG */
  /* need sheeter? */
  if xef.roll = true then do:
    /* find sheeter entered in style file, if any */
    if avail mach then release mach.
    do i = 1 to 7:
      find first mach {&where-machine}            and
        mach.m-code  = style.m-code[i] no-lock no-error.
      if avail mach and (mach.dept[1] ne "RS") and
        (mach.dept[2] ne "RS") and
        (mach.dept[3] ne "RS") and
        (mach.dept[4] ne "RS") then do:
        release mach.
        next.
      end.
      
      if avail mach then
      run ce/mach-qty.p (ROWID(mach)).
      if avail mach                and
         mach.min-len le sh-len    and
         mach.min-wid le sh-wid    and
         mach.min-cal le xcal      and
         mach.max-len ge sh-len    and
         mach.max-wid ge sh-wid    and
         mach.max-cal ge xcal      and
         mach.min-run le v-chk-qty and
         mach.max-run ge v-chk-qty then leave.
      if avail mach then
      release mach.
    end.
    if not avail mach then
    do:
      for each mach
          {&where-machine}
            and mach.dept[1] eq "RS"
            and mach.min-len le sh-len
            and mach.min-wid le sh-wid
            and mach.min-cal le xcal
            and mach.max-len ge sh-len
            and mach.max-wid ge sh-wid
            and mach.max-cal ge xcal
          no-lock
          BY mach.d-seq
          BY mach.m-seq:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        next.
        leave.
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
    end.
  end.
  
  /* get guillotine (rc dept) */
  if xef.n-cuts gt 0 OR xef.roll then
  reamcut:
  do on error undo:
    if avail mach then
    release mach.
    /* find ream cutter entered in style file, if any */
    do i = 1 to 7:
      find first mach {&where-machine}            and
        mach.m-code  = style.m-code[i] no-lock no-error.
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
      if avail mach                and
         (mach.p-type EQ "R" OR xef.n-cuts NE 0) AND
         mach.min-len le sh-len    and
         mach.min-wid le sh-wid    and
         mach.min-cal le xcal      and
         mach.max-len ge sh-len    and
         mach.max-wid ge sh-wid    and
         mach.max-cal ge xcal      and
         mach.min-run le v-chk-qty and
         mach.max-run ge v-chk-qty then leave.
    end.
    if not avail mach then do:
      for each mach
          {&where-machine}
            and mach.dept[1] eq "RC"
            and (mach.p-type EQ "R" OR xef.n-cuts NE 0)
            and mach.min-len le sh-len
            and mach.min-wid le sh-wid
            and mach.min-cal le xcal
            and mach.max-len ge sh-len
            and mach.max-wid ge sh-wid
            and mach.max-cal ge xcal
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
      assign m-lst.f-no   = xef.form-no
        m-lst.seq    = 10 * mach.d-seq
        m-lst.dept   = "RC"
        m-lst.bl     = NO
        m-lst.m-code = mach.m-code
        m-lst.n-out  = xef.n-out
        v-rc-seq     = m-lst.seq.
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
  if xef.f-pass + xef.f-coat-p gt 0 then do:
    find first item {sys/look/itemivW.i} and
      item.i-no    = xeb.i-code2[1] no-lock no-error.
    if avail item and item.i-code = "E" then
    find first e-item of item no-lock no-error.
    if avail mach then release mach.
    if xef.f-coat gt 0 then
    do k = 1 to xef.f-coat:
      /* find coater entered in style file, if any */
      do i = 1 to 7:
        find first mach
            {&where-machine}
              and mach.m-code  eq style.m-code[i]
              and mach.dept[1] eq "CT"
              and mach.min-cal le xcal
              and mach.max-cal ge xcal
              and mach.min-len le sh-len
              and mach.max-len ge sh-len
              and mach.min-wid le sh-wid
              and mach.max-wid ge sh-wid
            no-lock no-error.
        if avail mach then do:
          run ce/mach-qty.p (ROWID(mach)).
          if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
          release mach.
          if avail mach then
          leave.
        end.
      end.
      /* find 1st valid machine in mach file */
      if not avail mach then
      for each mach
          {&where-machine}
            and mach.dept[1] eq "CT"
            and mach.min-cal le xcal
            and mach.max-cal ge xcal
            and mach.min-len le sh-len
            and mach.max-len ge sh-len
            and mach.min-wid le sh-wid
            and mach.max-wid ge sh-wid
          no-lock
          BY mach.d-seq
          BY mach.m-seq:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then next.
        if avail mach then leave.
      end.
      if avail mach then do:
        create m-lst.
        assign
          m-lst.f-no = xef.form-no
          m-lst.seq = (10 * mach.d-seq) + k
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
      {sys/inc/roundup.i maxco}
      if avail mach then release mach.
      /* find machine entered in style file, if any */
      do i = 1 to 7:
        find first mach
            {&where-machine}
              and mach.m-code  eq style.m-code[i]
              and mach.pr-type eq item.press-type
              and mach.max-color + int(mach.coater and xef.f-coat gt 0) ge maxco
              and mach.min-cal le xcal
              and mach.max-cal ge xcal
              and mach.min-len le sh-len
              and mach.max-len ge sh-len
              and mach.min-wid le sh-wid
              and mach.max-wid ge sh-wid
            no-lock no-error.
        if avail mach then do:
          run ce/mach-qty.p (ROWID(mach)).
          if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
          release mach.
          if avail mach then leave.
        end.
      end.

      /* find 1st valid machine in mach file */
      if not avail mach then
      for each mach
          {&where-machine}
            and mach.pr-type eq item.press-type
            and mach.max-color + int(mach.coater and xef.f-coat gt 0) ge maxco
            and mach.min-cal le xcal
            and mach.max-cal ge xcal
            and mach.min-len le sh-len
            and mach.max-len ge sh-len
            and mach.min-wid le sh-wid
            and mach.max-wid ge sh-wid
          by mach.max-color
          BY mach.d-seq
          BY mach.m-seq:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then next.
        if avail mach then leave.
      end.
      
      if avail mach then do:
        if mach.p-type eq "R" then do:
          find first m-lst where m-lst.seq ge 10 and m-lst.seq lt 20 no-error.
          if avail m-lst then delete m-lst.
        end.
        if mach.coater and mach.max-color * xef.f-pass ge xef.f-col then do:
          for each m-lst
              where m-lst.dept    eq "CT"
                and m-lst.f-no    eq xef.form-no:
            delete m-lst.
          end.
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
        end.
      end.
      leave.    /* fake repeat */
    end. /* avail item... */
  end. /* if i-pass gt 0 */
  else do:
    k = 1.
    /* find machine entered in style file, if any */
    do i = 1 to 7:
      find first mach {&where-machine} and mach.m-code = style.m-code[i]
        no-lock no-error.
      if avail mach then
      do:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      end.
      if avail mach then
      leave.
    end.
    /* find 1st valid machine in mach file */
    if not avail mach then
    find first mach {&where-machine} and mach.m-code = xef.m-code
      no-lock no-error.
    if avail mach and mach.dept[1] = "PR" then do:
      if mach.p-type = "R" or xef.roll = no then do:
        find first m-lst where m-lst.dept = "RS" no-error.
        if avail m-lst then
        delete m-lst.
      end.
      create m-lst.
      assign
        m-lst.f-no    = xef.form-no
        m-lst.seq     = (10 * mach.d-seq) + k
        m-lst.bl      = NO
        m-lst.m-code  = mach.m-code
        m-lst.dscr    = mach.m-dscr
        m-lst.pass-no = k.
      do i = 1 to 4 :
        if mach.dept[i] ne "" and mach.dept[i] ne "PR" then do:
          m-lst.dept = mach.dept[i].
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
      if avail mach             and
         mach.min-cal le xcal   and
         mach.max-cal ge xcal   and
         mach.min-len le sh-len and
         mach.max-len ge sh-len and
         mach.min-wid le sh-wid and
         mach.max-wid ge sh-wid then leave.
    end.
    if not avail mach then do:
      for each mach
          {&where-machine}
            and mach.dept[1] eq "GU"
            and mach.min-cal le xcal
            and mach.max-cal ge xcal
            and mach.min-len le sh-len
            and mach.max-len ge sh-len
            and mach.min-wid le sh-wid
            and mach.max-wid ge sh-wid
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
    end.
  end.

  /* laminating */
  if xef.medium ne "" or xef.flute ne "" or xef.lam-code ne "" then
  lamin:
  do on error undo:
    if xef.trim-pen ne 0 then
    do:
      xcal = xef.trim-pen / 1000. /* trim-pen stored as int */
      if xcal ne 0 and xcal ne .014 then
      xcal = xcal - .014 + xcal.
    end.
    else
    xcal = xcal.
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
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      end.
      if avail mach             and
         mach.min-cal le xcal   and
         mach.max-cal ge xcal   and
         mach.min-len le sh-len and
         mach.max-len ge sh-len and
         mach.min-wid le sh-wid and
         mach.max-wid ge sh-wid then leave.
    end.

    if not avail mach then
    for each mach
        {&where-machine}
          and mach.dept[1] eq "LM"
          and mach.min-cal le xcal
          and mach.max-cal ge xcal 
          and mach.min-len le sh-len
          and mach.max-len ge sh-len
          and mach.min-wid le sh-wid
          and mach.max-wid ge sh-wid
        no-lock
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then next.
      leave.
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
    end.
  end.

  assign
   sh-len = xef.trim-w
   sh-wid = xef.trim-l.

  /* get die cutter */
  diecut:
  do on error undo:
    if avail mach then
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
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      end.
      if avail mach             and
         mach.min-cal le xcal   and
         mach.max-cal ge xcal   and
         mach.min-len le sh-len and
         mach.max-len ge sh-len and
         mach.min-wid le sh-wid and
         mach.max-wid ge sh-wid then leave.
    end.

    if not avail mach then
    for each mach
        {&where-machine}
          and mach.dept[1] eq "DC"
          and mach.min-cal le xcal
          and mach.max-cal ge xcal
          and mach.min-len le sh-len
          and mach.max-len ge sh-len
          and mach.min-wid le sh-wid
          and mach.max-wid ge sh-wid
        no-lock
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
      next.
      leave.
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
    end.
  end.

  /* get auto strip */
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
          mach.dept[3] ne "HS" and mach.dept[4] ne "HS" then
        release mach.
        else
        do:
          run ce/mach-qty.p (ROWID(mach)).
          if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
          release mach.
        end.
        if avail mach then
        leave.
      end.
    end.
    if not avail mach then
    for each mach {&where-machine}                 and
        mach.dept[1]  = "HS"
        no-lock
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
      next.
      leave.
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
    end.
  end.

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
    
    find first bf-eb
        where bf-eb.company  eq est-flm.company
          AND bf-eb.est-no   eq est-flm.est-no
          and bf-eb.form-no  eq est-flm.snum
          and bf-eb.blank-no eq est-flm.bnum
        no-lock no-error.
          
    release style.
    
    if avail bf-eb then
    find first style
        {sys/ref/styleW.i}
          and style.style eq bf-eb.style
        no-lock no-error.

    if avail style then
    do j = 1 to 7:
      if style.m-code[j] eq "" then next.
    
      find first mach
          {&where-machine}
          and mach.m-code eq style.m-code[j]
        no-lock no-error.
      if avail mach and
         mach.dept[1] NE v-dept AND mach.dept[2] NE v-dept AND
         mach.dept[3] NE v-dept AND mach.dept[4] NE v-dept then do:
        release mach.
      end.
      if avail mach then do:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
          release mach.
      end.
      if avail mach                             and
         mach.min-cal le xcal                   and
         mach.max-cal ge xcal                   and
         ((est-flm.bnum gt 0           and
           mach.min-len le bf-eb.t-len and
           mach.max-len ge bf-eb.t-len and
           mach.min-wid le bf-eb.t-wid and
           mach.max-wid ge bf-eb.t-wid)     or
          (est-flm.bnum le 0       and
           mach.min-len le sh-len  and
           mach.max-len ge sh-len  and
           mach.min-wid le sh-wid  and
           mach.max-wid ge sh-wid))             then leave.
    end.

    if not avail mach then
    for each mach
        {&where-machine}
          and mach.dept[1] eq v-dept
          and mach.min-cal le xcal
          and mach.max-cal ge xcal
          and ((est-flm.bnum gt 0           and
                mach.min-len le bf-eb.t-len and
                mach.max-len ge bf-eb.t-len and
                mach.min-wid le bf-eb.t-wid and
                mach.max-wid ge bf-eb.t-wid)    or
               (est-flm.bnum le 0       and
                mach.min-len le sh-len  and
                mach.max-len ge sh-len  and
                mach.min-wid le sh-wid  and
                mach.max-wid ge sh-wid))
        no-lock
        BY mach.d-seq
        BY mach.m-seq:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then
        release mach.
      else leave.
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
 
  /* get gluer */
  glu:
  for each xeb where xeb.company = xest.company and
      xeb.est-no = xest.est-no and
      xeb.form-no = xef.form-no:

    find first style {sys/ref/styleW.i} and
      style.style   = xeb.style no-lock no-error.

    if avail mach then release mach.

    if xeb.adhesive ne "" and xeb.gluelap  ne 0  then do:
      find first item
          where item.company eq cocode
            and item.i-no    eq xeb.adhesive
          no-lock no-error.
      if avail item and item.mat-type eq "T" then leave glu.

      ll-style = NO.
      if avail mach then
      release mach.
      do i = 1 to 7:
        if style.m-code[i] = "" then
        next.
        find first mach {&where-machine} and
          mach.m-code  = style.m-code[i] no-lock no-error.
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
          RUN create-m-lst ("GL", xef.form-no, xeb.blank-no, YES).
        END.
      end.
      IF NOT ll-style THEN
      for each mach {&where-machine}                 and
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
        if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then do:
          release mach.
        end.
        ELSE DO:
          RUN create-m-lst ("GL", xef.form-no, xeb.blank-no, NO).
          LEAVE.
        END.
      end.
    end.
  end.
  
  for each xeb
      where xeb.company = xest.company
        AND xeb.est-no   eq xest.est-no
        and xeb.form-no eq xef.form-no
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
        find first mach {&where-machine}                     and
          mach.m-code   = style.m-code[i] and
          mach.min-cal le xcal      and
          mach.max-cal ge xcal      and
          NOT CAN-FIND(first m-lst
                       where ((m-lst.dept  eq mach.dept[1] AND NOT m-lst.styl) OR
                              m-lst.m-code EQ style.m-code[i])
                         and m-lst.f-no    eq xef.form-no
                         and ((mach.p-type eq "B" and m-lst.b-no eq xeb.blank-no) or
                               mach.p-type ne "B")) 
          no-lock no-error.

        IF AVAIL mach THEN
          IF LOOKUP(mach.dept[1],"RS,RC") GT 0 OR mach.p-type EQ "R" THEN
            ASSIGN
             sh-len = IF xef.roll THEN xef.gsh-wid ELSE xef.nsh-wid
             sh-wid = IF xef.roll THEN xef.gsh-len ELSE xef.nsh-len.
          ELSE
          IF LOOKUP(mach.dept[1],"PR,GU,LM") GT 0 OR xef.n-out-l LE 1 THEN
            ASSIGN
             sh-len = xef.nsh-wid
             sh-wid = xef.nsh-len.
          ELSE
            ASSIGN
             sh-len = xef.trim-w
             sh-wid = xef.trim-l.

        if avail mach                           and
           ((mach.p-type  eq "B"       and
             mach.min-len le xeb.t-len and
             mach.max-len ge xeb.t-len and
             mach.min-wid le xeb.t-wid and
             mach.max-wid ge xeb.t-wid)     or
            (mach.p-type  ne "B"       and
             mach.max-len ge sh-len    and
             mach.min-len le sh-len    and
             mach.min-wid le sh-wid    and
             mach.max-wid ge sh-wid))           then do:

          if mach.dept[1] eq "PR" and
             mach.max-color + int(mach.coater and xef.f-coat gt 0) lt maxco then
            next.

          run ce/mach-qty.p (ROWID(mach)).
          if mach.min-run gt v-chk-qty or mach.max-run lt v-chk-qty then next.

          create m-lst.
          assign
            m-lst.f-no   = xef.form-no
            m-lst.seq    = 10 * mach.d-seq
            m-lst.bl     = NO
            m-lst.dept   = mach.dept[1]
            m-lst.m-code = style.m-code[i]
            m-lst.dscr   = mach.m-dscr
            m-lst.bl     = mach.p-type eq "B"
            m-lst.b-no   = if m-lst.bl then xeb.blank-no else 0
            m-lst.styl   = YES.
        end.
      end.
    end.
  end. /* CTS */
  END. /* Manufactured FG */
end. /* each ef */

run ce/com/mach-sq2.p (ip-form-no).

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

/* end ---------------------------------- copr. 1994  advanced software, inc. */
