
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-is-a-tandem-est AS LOG NO-UNDO.

DEF VAR lv-company LIKE est.company NO-UNDO.
DEF VAR lv-est-no LIKE est.est-no NO-UNDO.
DEF VAR lv-form-no LIKE ef.form-no INIT 0 NO-UNDO.
DEF VAR lv-blank-no LIKE eb.blank-no INIT 0 NO-UNDO.
DEF VAR li AS INT NO-UNDO.

DEF BUFFER bf-eb FOR eb.
DEF BUFFER bf-ef FOR ef.


FIND FIRST eb
    WHERE ROWID(eb)    EQ ip-rowid
      AND (eb.est-type EQ 4 OR
           eb.est-type EQ 8)
    NO-LOCK NO-ERROR.

IF AVAIL eb THEN
  ASSIGN
   lv-company  = eb.company
   lv-est-no   = eb.est-no
   lv-form-no  = eb.form-no
   lv-blank-no = eb.blank-no.

ELSE
FIND FIRST ef
    WHERE ROWID(ef)    EQ ip-rowid
      AND (ef.est-type EQ 4 OR
           ef.est-type EQ 8)
    NO-LOCK NO-ERROR.

IF AVAIL ef THEN
  ASSIGN
   lv-company = ef.company
   lv-est-no  = ef.est-no
   lv-form-no = ef.form-no.

ELSE
FIND FIRST est
    WHERE ROWID(est)    EQ ip-rowid
      AND (est.est-type EQ 4 OR
           est.est-type EQ 8)
    NO-LOCK NO-ERROR.

IF NOT AVAIL est THEN
FIND FIRST est
    WHERE est.company EQ lv-company
      AND est.est-no  EQ lv-est-no
    NO-LOCK NO-ERROR.

RELEASE ef.
RELEASE eb.

IF AVAIL est THEN
FIND FIRST ef
    WHERE ef.company  EQ est.company 
      AND ef.est-no   EQ est.est-no
      AND (ef.form-no NE lv-form-no OR
           lv-form-no EQ 0)
    NO-LOCK NO-ERROR.

IF AVAIL ef THEN
FIND FIRST eb
    WHERE eb.company EQ ef.company 
      AND eb.est-no  EQ ef.est-no
      AND eb.form-no EQ ef.form-no
    NO-LOCK NO-ERROR.

IF AVAIL eb THEN DO:
  op-is-a-tandem-est = /*CAN-FIND(FIRST bf-ef
                                WHERE bf-ef.company  EQ ef.company
                                  AND bf-ef.est-no   EQ ef.est-no
                                  AND ROWID(bf-ef)   NE ROWID(ef))
                       AND*/
                       NOT CAN-FIND(FIRST bf-ef
                                    WHERE bf-ef.company  EQ ef.company
                                      AND bf-ef.est-no   EQ ef.est-no
                                      AND (bf-ef.form-no NE lv-form-no OR
                                           lv-form-no    EQ 0)
                                      AND (bf-ef.trim-w  NE ef.trim-w  OR
                                           bf-ef.trim-l  NE ef.trim-l  /*OR
                                           bf-ef.board   NE ef.board   OR
                                           bf-ef.gsh-wid NE ef.gsh-wid OR
                                           bf-ef.gsh-len NE ef.gsh-len OR
                                           bf-ef.nsh-wid NE ef.nsh-wid OR
                                           bf-ef.nsh-len NE ef.nsh-len OR
                                           bf-ef.n-out   NE ef.n-out   OR
                                           bf-ef.n-out-l NE ef.n-out-l*/))
                       AND
                       NOT CAN-FIND(FIRST bf-eb
                                    WHERE bf-eb.company   EQ eb.company
                                      AND bf-eb.est-no    EQ eb.est-no
                                      AND (bf-eb.form-no  NE lv-form-no OR
                                           lv-form-no     EQ 0)
                                      AND (bf-eb.blank-no NE lv-blank-no OR
                                           lv-blank-no    EQ 0)
                                      AND (bf-eb.style    NE eb.style OR
                                           bf-eb.len      NE eb.len   OR
                                           bf-eb.wid      NE eb.wid   OR
                                           bf-eb.dep      NE eb.dep)).

  IF op-is-a-tandem-est THEN
  FOR EACH bf-ef
      WHERE bf-ef.company EQ ef.company
        AND bf-ef.est-no  EQ ef.est-no
      NO-LOCK:
    li = 0.
    FOR EACH bf-eb OF bf-ef NO-LOCK:
      li = li + 1.
    END.
    IF li GT 1 THEN DO:
      op-is-a-tandem-est = NO.
      LEAVE.
    END.
  END.
END.

