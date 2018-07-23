
DEF PARAM BUFFER io-po-ordl FOR po-ordl.

DEF BUFFER b-po-ordl FOR po-ordl.
DEF BUFFER b-ref1 FOR reftable.
DEF BUFFER b-ref2 FOR reftable.

DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.

DEF VAR v-factor AS INT INIT -1 NO-UNDO.
DEF VAR v-reopen as LOG INIT YES NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.


ASSIGN
 cocode = io-po-ordl.company
 ll     = NO.

IF io-po-ordl.t-rec-qty NE 0 OR io-po-ordl.t-inv-qty NE 0 THEN ll = YES.

IF NOT ll THEN
FOR EACH ap-inv
    WHERE ap-inv.company EQ io-po-ordl.company
      AND ap-inv.posted  EQ NO
    NO-LOCK,
    EACH ap-invl
    WHERE ap-invl.i-no EQ ap-inv.i-no
      AND ap-invl.line EQ ((io-po-ordl.po-no * 1000) + io-po-ordl.line)
    NO-LOCK:
  ll = YES.
  LEAVE.
END.

IF NOT ll THEN
  IF io-po-ordl.item-type THEN
    ll = CAN-FIND(FIRST rm-rctd
                  WHERE rm-rctd.company    EQ io-po-ordl.company
                    AND rm-rctd.rita-code  EQ "R"
                    AND rm-rctd.i-no       EQ io-po-ordl.i-no
                    AND INT(rm-rctd.po-no) EQ io-po-ordl.po-no
                  USE-INDEX rita-code) NO-ERROR.
  ELSE
    ll = CAN-FIND(FIRST fg-rctd
                  WHERE fg-rctd.company    EQ io-po-ordl.company
                    AND fg-rctd.rita-code  EQ "R"
                    AND fg-rctd.i-no       EQ io-po-ordl.i-no
                    AND INT(fg-rctd.po-no) EQ io-po-ordl.po-no
                  USE-INDEX rita-code) NO-ERROR.

IF ll THEN DO:
  MESSAGE "Receipts and/or invoice have been entered against this item, cannot delete..."
      VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

{po/po-ordls.i io-}
IF AVAIL b-ref1 THEN DELETE b-ref1.
IF AVAIL b-ref2 THEN DELETE b-ref2.

{po/poordlup.i io-}
    FIND po-ordl WHERE ROWID(po-ordl) EQ ROWID(io-po-ordl) NO-LOCK NO-ERROR.
    
RUN po/deleteItemAttach.p (INPUT io-po-ordl.company, INPUT io-po-ordl.po-no,
                           INPUT io-po-ordl.LINE, INPUT io-po-ordl.i-no).

FIND FIRST po-ord
    WHERE po-ord.company EQ io-po-ordl.company
      AND po-ord.po-no   EQ io-po-ordl.po-no
    NO-LOCK NO-ERROR.
IF AVAIL po-ord THEN RUN po/po-total.p (RECID(po-ord)).

FOR EACH job NO-LOCK
    WHERE job.company  EQ io-po-ordl.company
      AND job.job-no   EQ io-po-ordl.job-no
      AND job.job-no2  EQ io-po-ordl.job-no2,
    EACH job-mat NO-LOCK
    WHERE job-mat.company  EQ job.company
      AND job-mat.job      EQ job.job
      AND job-mat.job-no   EQ job.job-no
      AND job-mat.job-no2  EQ job.job-no2
      AND job-mat.rm-i-no  EQ io-po-ordl.i-no
      AND job-mat.frm      EQ io-po-ordl.s-num
      AND job-mat.po-no    EQ io-po-ordl.po-no
    USE-INDEX job:

    FIND FIRST job-hdr
        WHERE job-hdr.company   EQ job-mat.company
          AND job-hdr.job-no    EQ job-mat.job-no
          AND job-hdr.job-no2   EQ job-mat.job-no2
          AND job-hdr.frm       EQ job-mat.frm
          AND (job-hdr.blank-no EQ job-mat.blank-no OR job-mat.blank-no EQ 0)
        NO-LOCK NO-ERROR.

    IF NOT AVAIL job-hdr THEN
    FIND FIRST job-hdr
        WHERE job-hdr.company   EQ job-mat.company
          AND job-hdr.job-no    EQ job-mat.job-no
          AND job-hdr.job-no2   EQ job-mat.job-no2
        NO-LOCK NO-ERROR.

    IF AVAIL job-hdr AND io-po-ordl.ord-no NE 0 THEN
    FOR EACH oe-ordl
        WHERE oe-ordl.company  EQ job-hdr.company
          AND oe-ordl.ord-no   EQ io-po-ordl.ord-no
          AND oe-ordl.i-no     EQ job-hdr.i-no
          AND oe-ordl.job-no   EQ job-hdr.job-no
          AND oe-ordl.job-no2  EQ job-hdr.job-no2
          AND oe-ordl.po-no-po EQ io-po-ordl.po-no
        EXCLUSIVE:
      ASSIGN
       oe-ordl.po-no-po = 0
       oe-ordl.vend-no  = "".
    END.

/*    DELETE reftable.*/
/*  END.              */
END.

IF trim(io-po-ordl.job-no) = "" THEN DO:
  FOR EACH oe-ordl
      WHERE oe-ordl.company  EQ io-po-ordl.company
        AND oe-ordl.po-no-po EQ io-po-ordl.po-no:
                  
    ASSIGN
     oe-ordl.vend-no  = ""
     oe-ordl.po-no-po = 0.

  END.
END.

FOR EACH loadtag
    WHERE loadtag.company   EQ io-po-ordl.company
      AND loadtag.item-type EQ YES
      AND loadtag.tag-no    BEGINS STRING(io-po-ordl.po-no,'9999999') +
                                   STRING(io-po-ordl.line,'999'):
  DELETE loadtag.
END.

FOR EACH reftable
    WHERE reftable.rec_key  EQ io-po-ordl.rec_key
      AND reftable.reftable EQ "LAYOUT UPDATED"
      AND reftable.company  EQ "io-po-ordl"
    USE-INDEX rec_key:

  DELETE reftable.
END.

FIND FIRST reftable WHERE
     reftable.reftable EQ "POORDLDEPTH" AND
     reftable.company  EQ io-po-ordl.company AND
     reftable.loc      EQ STRING(io-po-ordl.po-no) AND
     reftable.code     EQ STRING(io-po-ordl.LINE)
     EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL reftable THEN DELETE reftable.
