/* --------------------------------------------------- ce/mach-pr.p 02/99 JLF */
/* create machine routing sequence - printing PR & coating CT departments     */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def buffer xeb2        for eb.

def shared var sh-wid    as DEC NO-UNDO.
def shared var sh-len    as DEC NO-UNDO.
def shared var v-chk-qty as dec no-undo.

&SCOPED-DEFINE where-machine                                                     ~
               WHERE (mach.company EQ cocode                                  ~
                 AND  NOT CAN-FIND(FIRST reftable                             ~
                                   WHERE reftable.reftable EQ "mach.obsolete" ~
                                     AND reftable.company  EQ mach.company    ~
                                     AND reftable.loc      EQ mach.loc        ~
                                     AND reftable.code     EQ mach.m-code     ~
                                     AND reftable.val[1]   EQ 1)) 

{ce/mach-lst.i}

{ce/mach-ink.i}


find first style
    {sys/ref/styleW.i}
      and style.style eq xeb.style
    no-lock no-error.

release w-ink.

for each w-ink where w-ink.form-no eq xef.form-no:
  /* find ink & coater */
  if w-ink.coat then do:
    release mach.

    /* find coater entered in style file, if any */
    do i = 1 to 7:
      if style.m-code[i] eq "" then next.
      find first mach
          {&where-machine}
            and mach.m-code  eq style.m-code[i]
            and mach.dept[1] eq "CT"
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
          no-lock no-error.

      if avail mach then do:
        run ce/mach-qty.p (ROWID(mach)).
        if mach.min-run gt v-chk-qty or
           mach.max-run lt v-chk-qty then release mach.
      end.

      if avail mach then leave.
    end.

    /* find 1st valid machine in mach file */
    if not avail mach then
    for each mach
        {&where-machine}
          and mach.dept[1] eq "CT"
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

      if mach.min-run gt v-chk-qty or
         mach.max-run lt v-chk-qty then release mach. else leave.
    end.

    if avail mach then do:
      create m-lst.
      assign
       m-lst.f-no    = w-ink.form-no
       m-lst.b-no    = if xest.est-type eq 1 then 1 else 0
       m-lst.seq     = (10 * mach.d-seq) + k
       m-lst.bl      = no
       m-lst.dept    = "CT"
       m-lst.m-code  = mach.m-code
       m-lst.dscr    = mach.m-dscr
       m-lst.pass-no = w-ink.pass.
    end.
  end.

  /* find press */
  /* find machine entered in style file, if any */
  release mach.
  do i = 1 to 7:
    if style.m-code[i] eq "" then next.
    find first mach
        {&where-machine}
          and mach.m-code                 eq style.m-code[i]
          and mach.pr-type                eq w-ink.press
          and mach.max-color              ge w-ink.inks + w-ink.varn
          and (mach.coater or not w-ink.coat)
          and mach.min-cal                le xef.cal
          and mach.max-cal                ge xef.cal
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
        no-lock no-error.

    if avail mach then do:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or
         mach.max-run lt v-chk-qty then release mach.
    end.

    if avail mach then leave.
  end.

  /* find layout machine in mach file */
  if not avail mach then do:
    find first mach no-lock
        {&where-machine}
          and mach.m-code  eq xef.m-code
          and mach.pr-type eq w-ink.press
          and mach.max-color ge w-ink.inks + w-ink.varn
          and (mach.coater or not w-ink.coat)
          and mach.min-cal                le xef.cal
          and mach.max-cal                ge xef.cal
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
        no-error.
    if avail mach and mach.dept[1] ne "PR" then release mach.
    if avail mach then do:
      run ce/mach-qty.p (ROWID(mach)).
      if mach.min-run gt v-chk-qty or
         mach.max-run lt v-chk-qty then release mach.
    end.
  end.

  /* find 1st valid machine in mach file */
  if not avail mach then
  for each mach
      {&where-machine}
        and mach.pr-type                eq w-ink.press
        and mach.max-color              ge w-ink.inks + w-ink.varn
        and (mach.coater or not w-ink.coat)
        and mach.min-cal                le xef.cal
        and mach.max-cal                ge xef.cal
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
      by mach.max-color + int(mach.coater and w-ink.coat)
      BY mach.d-seq
      BY mach.m-seq:

    run ce/mach-qty.p (ROWID(mach)).

    if mach.min-run gt v-chk-qty or
       mach.max-run lt v-chk-qty then release mach.
    else leave.
  end.

  if avail mach then do:
    if mach.p-type eq "R" or xef.roll eq no then do:
      find first m-lst
          where m-lst.f-no eq w-ink.form-no
            and m-lst.dept eq "RS"
          no-error.
      if avail m-lst then delete m-lst.
    end.
    if w-ink.coat then do:
      find first m-lst
          where m-lst.f-no    eq w-ink.form-no
            and m-lst.dept    eq "CT"
            and m-lst.pass-no eq w-ink.pass
          no-error.
      if avail m-lst then delete m-lst.
    end.

    if xest.est-type eq 3 then
    for each xeb2 where xeb.company = xest.company and
                        xeb2.est-no eq xest.est-no no-lock:
      do i = 1 to 20:
        if xeb2.i-code2[i] gt "" and xeb2.i-ps2[i] eq w-ink.pass then do:
          create m-lst.
          assign
           m-lst.f-no    = w-ink.form-no
           m-lst.b-no    = xeb2.blank-no
           m-lst.seq     = (10 * mach.d-seq) + w-ink.pass
           m-lst.bl      = no
           m-lst.dept    = "PR"
           m-lst.m-code  = mach.m-code
           m-lst.dscr    = mach.m-dscr
           m-lst.pass-no = w-ink.pass.
          leave.
        end.
      end.
    end.

    else do:
      create m-lst.
      assign
       m-lst.f-no    = w-ink.form-no
       m-lst.b-no    = if xest.est-type eq 1 then 1 else 0
       m-lst.seq     = (10 * mach.d-seq) + w-ink.pass
       m-lst.bl      = no
       m-lst.dept    = "PR"
       m-lst.m-code  = mach.m-code
       m-lst.dscr    = mach.m-dscr
       m-lst.pass-no = w-ink.pass.
    end.
  end.
end. /* for each w-ink */

/* end ---------------------------------- copr. 1999  advanced software, inc. */
