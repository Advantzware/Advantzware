/* --------------------------------------------------- ce/pr4-flm.i 08/96 JLF */

DEF VAR lv-deptb LIKE est-op.dept NO-UNDO.
DEF VAR lv-deptf LIKE est-op.dept NO-UNDO.

&SCOPED-DEFINE valid-est-op                                            ~
    WHERE est-op.company EQ xest.company                               ~
      AND est-op.est-no  EQ xest.est-no                                ~
      AND (est-op.qty    EQ v-op-qty OR xest.est-type NE 1)            ~
      AND est-op.line    GT 500                                        ~
      AND est-op.s-num   EQ xef.form-no                                ~
      AND CAN-FIND(FIRST mach                                          ~
                   WHERE mach.company       EQ est-op.company          ~
                     AND mach.m-code        EQ est-op.m-code           ~
                     AND ((est-op.op-sb     EQ NO               AND    ~
                           est-op.b-num     EQ xef.leaf-bnum[i] AND    ~
                           xef.leaf-bnum[i] GT 0                AND    ~
                           (mach.dept[1]    EQ lv-deptb OR             ~
                            mach.dept[2]    EQ lv-deptb OR             ~
                            mach.dept[3]    EQ lv-deptb OR             ~
                            mach.dept[4]    EQ lv-deptb))           OR ~
                          ((est-op.op-sb    EQ YES OR                  ~
                            xef.leaf-bnum[i] LE 0)               AND   ~
                           (mach.dept[1]    EQ lv-deptf OR             ~
                            mach.dept[2]    EQ lv-deptf OR             ~
                            mach.dept[3]    EQ lv-deptf OR             ~
                            mach.dept[4]    EQ lv-deptf))))

do i = 1 to 4:
  if xef.leaf[i] ne "" then do:
    find first item
        where item.company eq cocode
          and item.i-no    eq xef.leaf[i]
        no-lock no-error.
    if avail item then find first e-item of item no-lock no-error.
    else next.

    fuom = if avail e-item then e-item.std-uom else item.cons-uom.

    release eb.
    if xef.leaf-bnum[i] gt 0 then
    find first eb
        where eb.company  eq xest.company 
          AND eb.est-no   eq xest.est-no
          and eb.form-no  eq xef.form-no
          and eb.blank-no eq xef.leaf-bnum[i]
        no-lock no-error.

    IF item.mat-type EQ "W" THEN
      ASSIGN
       lv-deptb = "WN"
       lv-deptf = "WS".
    
    ELSE
      ASSIGN
       lv-deptb = "FB"
       lv-deptf = "FS".

    FIND FIRST est-op {&valid-est-op} NO-LOCK NO-ERROR.

    if not avail est-op then next.

    if avail eb and xest.est-type ne 4 then vup = eb.num-up.
    
    else
    run sys/inc/numup.p (xest.company,xest.est-no, xef.form-no, output vup).
    
    fup = if xef.leaf-bnum[i] eq 0 then vup else 1.
    
    if item.mat-type eq "W" then
      fqty = (xef.leaf-w[i] + fup) * (xef.leaf-l[i] + fup).
    else
      fqty = xef.leaf-w[i] * xef.leaf-l[i].
    
    ASSIGN
    fup = if xef.leaf-bnum[i] eq 0 then 1 else vup
    fup = fup * (if xef.n-out   gt 0 then xef.n-out   else 1) *
                (if xef.n-out-l gt 0 then xef.n-out-l else 1)
    fqty = fqty * fup * est-op.num-sh
    fqty = fqty / (if fuom eq "MSI" then 1000 else item.sqin-lb).

    find first flm
        where flm.id   eq item.i-no
          and flm.snum eq xef.form-no
          and flm.bnum eq xef.leaf-bnum[i]
        no-error.
    if not avail flm then do:
      create flm.
      assign
       flm.id   = item.i-no
       flm.i-no = item.i-no
       flm.snum = xef.form-no
       flm.bnum = xef.leaf-bnum[i].
    end.
    
    assign
     flm.dscr = item.est-dscr
     flm.qty  = flm.qty + fqty
     flm.uom  = fuom.
  end.
end.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
