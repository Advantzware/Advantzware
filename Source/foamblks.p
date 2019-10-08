
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-blk AS INT INIT 0 NO-UNDO.

DEF VAR li-up AS INT NO-UNDO.
DEF VAR li-on AS INT NO-UNDO.
DEF VAR li-out AS INT NO-UNDO.


FIND ef WHERE ROWID(ef) EQ ip-rowid NO-LOCK NO-ERROR.
IF NOT AVAIL ef THEN RETURN.

RUN sys/inc/numup.p (ef.company, ef.est-no, ef.form-no, OUTPUT li-up).

FIND FIRST eb OF ef NO-LOCK NO-ERROR.

IF AVAIL eb THEN
  IF CAN-FIND(FIRST ef-nsh OF ef WHERE ef-nsh.dept EQ "DC") THEN
  FOR EACH ef-nsh OF ef NO-LOCK
      BREAK BY ef-nsh.sheet-no
            BY ef-nsh.pass-no DESC:

    IF FIRST-OF(ef-nsh.sheet-no) THEN li-out = 1.

    li-out = li-out * ef-nsh.n-out-l * ef-nsh.n-out-w * ef-nsh.n-out-d.

    IF ef-nsh.dept EQ "DC" THEN DO:
      IF ef-nsh.wid-out LT ef.trim-w THEN
        li-out = li-out * (ef-nsh.wid-out / ef.trim-w).

      IF ef-nsh.len-out LT ef.trim-l THEN
        li-out = li-out * (ef-nsh.len-out / ef.trim-l).

      IF ef-nsh.dep-out LT ef.trim-d THEN
        li-out = li-out * (ef-nsh.dep-out / ef.trim-d).

      RUN cec/foamplus.p (ROWID(ef-nsh), OUTPUT li-on).

      op-blk = op-blk + li-on.
    END.

    IF LAST-OF(ef-nsh.sheet-no) THEN
      ASSIGN
       li-out = li-out * li-up
       op-blk = op-blk + li-out.
  END.

  ELSE
  FOR EACH ef-nsh OF ef NO-LOCK
      BREAK BY ef-nsh.sheet-no
            BY ef-nsh.pass-no:

    IF FIRST-OF(ef-nsh.sheet-no) THEN li-out = 1.

    li-out = li-out * ef-nsh.n-out-l * ef-nsh.n-out-w * ef-nsh.n-out-d.
   
    IF LAST-OF(ef-nsh.sheet-no) THEN DO:
      li-out = li-out * li-up.

      IF ef-nsh.sheet-no NE 1 THEN
        ASSIGN
         li-out = li-out * (ef-nsh.wid-out / ef.trim-w).
         li-out = li-out * (ef-nsh.len-out / ef.trim-l).
         li-out = li-out * (ef-nsh.dep-out / ef.trim-d).

      op-blk = op-blk + li-out.
    END.
  END.
