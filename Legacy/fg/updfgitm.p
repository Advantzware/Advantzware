
DEF INPUT PARAM v-recid    AS RECID.
DEF INPUT PARAM v-new-item AS CHAR.

{sys/inc/var.i SHARED}

DEF BUFFER b-itemfg FOR itemfg.

DEF VAR lv AS CHAR NO-UNDO.


FIND itemfg WHERE RECID(itemfg) EQ v-recid NO-ERROR.
IF NOT AVAIL itemfg THEN RETURN.

IF v-new-item EQ "!" THEN
  v-new-item = CAPS(itemfg.i-no).
ELSE
  FIND FIRST b-itemfg
      WHERE b-itemfg.company EQ itemfg.company
        AND b-itemfg.i-no    EQ v-new-item
      NO-LOCK NO-ERROR.

FOR EACH ar-invl
    WHERE ar-invl.company EQ itemfg.company
      AND ar-invl.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i ar-invl i-no NO}
END.

FOR EACH itemfg-loc
    WHERE itemfg-loc.company EQ itemfg.company
      AND itemfg-loc.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i itemfg-loc i-no NO}
END.

FOR EACH ap-invl
    WHERE ap-invl.company   EQ itemfg.company
      AND ap-invl.item-no   EQ itemfg.i-no
      AND ap-invl.item-type EQ NO
    NO-LOCK:

  {fg/updfgitm.i ap-invl item-no NO}
END.

FOR EACH inv-line
    WHERE inv-line.company EQ itemfg.company
      AND inv-line.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i inv-line i-no NO}
END.

FOR EACH e-itemfg
    WHERE e-itemfg.company EQ itemfg.company
      AND e-itemfg.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i e-itemfg i-no NO}
END.

FOR EACH e-itemfg-vend
    WHERE e-itemfg-vend.company EQ itemfg.company
      AND e-itemfg-vend.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i e-itemfg-vend i-no NO}
END.

FOR EACH eb
    WHERE eb.company  EQ itemfg.company
      AND eb.stock-no EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i eb stock-no NO}
END.

FOR EACH fg-act
    WHERE fg-act.company EQ itemfg.company
      AND fg-act.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i fg-act i-no NO}
END.

IF TRIM(itemfg.i-no) NE "" THEN
FOR EACH fg-bin
    WHERE fg-bin.company EQ itemfg.company
      AND fg-bin.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i fg-bin i-no YES}
END.

FOR EACH loadtag
    WHERE loadtag.company   EQ itemfg.company
      AND loadtag.item-type EQ no
      AND loadtag.i-no      EQ itemfg.i-no
    USE-INDEX i-no NO-LOCK:

  {fg/updfgitm.i loadtag i-no NO}
END.

FOR EACH wiptag WHERE
    wiptag.company EQ itemfg.company AND
    wiptag.fg-i-no EQ itemfg.i-no
    NO-LOCK:
  
    {fg/updfgitm.i wiptag fg-i-no NO}

END.

FOR EACH fg-set
    WHERE fg-set.company EQ itemfg.company
      AND fg-set.set-no  EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i fg-set set-no YES 1}
END.

FOR EACH fg-set
    WHERE fg-set.company EQ itemfg.company
      AND fg-set.part-no EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i fg-set part-no NO 2}
END.

FOR EACH fg-hist
    WHERE fg-hist.company EQ itemfg.company
      AND fg-hist.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i fg-hist i-no NO}
END.

FOR EACH fg-rcpth
    WHERE fg-rcpth.company EQ itemfg.company
      AND fg-rcpth.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i fg-rcpth i-no NO}
END.

FOR EACH fg-rdtlh
    WHERE fg-rdtlh.company EQ itemfg.company
      AND fg-rdtlh.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i fg-rdtlh i-no NO}
END.

FOR EACH fg-rcpts
    WHERE fg-rcpts.company EQ itemfg.company
      AND fg-rcpts.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i fg-rcpts i-no NO}
END.

FOR EACH fg-rctd
    WHERE fg-rctd.company EQ itemfg.company
      AND fg-rctd.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i fg-rctd i-no NO}
END.

FOR EACH item-bom
    WHERE item-bom.company  EQ itemfg.company
      AND item-bom.parent-i EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i item-bom parent-i YES 1}
END.

FOR EACH item-bom
    WHERE item-bom.company EQ itemfg.company
      AND item-bom.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i item-bom i-no NO 2}
END.

FOR EACH itemfgdtl
    WHERE itemfgdtl.company EQ itemfg.company
      AND itemfgdtl.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i itemfgdtl i-no NO}
END.

FOR EACH job-hdr
    WHERE job-hdr.company EQ itemfg.company
      AND job-hdr.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i job-hdr i-no NO}
END.

FOR EACH job-mch
    WHERE job-mch.company EQ itemfg.company
      AND job-mch.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i job-mch i-no NO}
END.

FOR EACH mch-act
    WHERE mch-act.company EQ itemfg.company
      AND mch-act.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i mch-act i-no NO}
END.

FOR EACH misc-act
    WHERE misc-act.company EQ itemfg.company
      AND misc-act.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i misc-act i-no NO}
END.

FOR EACH oe-boll
    WHERE oe-boll.company EQ itemfg.company
      AND oe-boll.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i oe-boll i-no NO}
END.

FOR EACH oe-ordl
    WHERE oe-ordl.company EQ itemfg.company
      AND oe-ordl.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i oe-ordl i-no NO}
END.

FOR EACH oe-prmtx
    WHERE oe-prmtx.company EQ itemfg.company
      AND oe-prmtx.i-no    BEGINS itemfg.i-no
      AND SUBSTR(oe-prmtx.i-no,01,100) EQ itemfg.i-no
    NO-LOCK:

/*   lv = SUBSTR(oe-prmtx.i-no,101,8). */

  {fg/updfgitm.i oe-prmtx i-no YES}

/*   IF AVAIL b-oe-prmtx THEN                                    */
/*      b-oe-prmtx.i-no = STRING(b-oe-prmtx.i-no,"x(100)") + lv. */
END.

FOR EACH oe-rel
    WHERE oe-rel.company EQ itemfg.company
      AND oe-rel.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i oe-rel i-no NO}
END.

FOR EACH oe-rell
    WHERE oe-rell.company EQ itemfg.company
      AND oe-rell.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i oe-rell i-no NO}
END.

FOR EACH oe-retl
    WHERE oe-retl.company EQ itemfg.company
      AND oe-retl.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i oe-retl i-no NO}
END.

FOR EACH oe-ship
    WHERE oe-ship.company EQ itemfg.company
      AND oe-ship.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i oe-ship i-no NO}
END.

FOR EACH pc-prdd
    WHERE pc-prdd.company EQ itemfg.company
      AND pc-prdd.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i pc-prdd i-no NO}
END.

FOR EACH pc-prdd-wip
    WHERE pc-prdd-wip.company EQ itemfg.company
      AND pc-prdd-wip.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i pc-prdd-wip i-no NO}
END.

FOR EACH job-set
    WHERE job-set.company EQ itemfg.company
      AND job-set.i-no    EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i job-set i-no NO}
END.

FOR EACH po-ordl
    WHERE po-ordl.company   EQ itemfg.company
      AND po-ordl.i-no      EQ itemfg.i-no
      AND po-ordl.item-type EQ NO
    NO-LOCK:

  FOR EACH po-all
      WHERE po-all.company EQ itemfg.company
        AND po-all.po-no   EQ po-ordl.po-no
        AND po-all.line    EQ po-ordl.line
        AND po-all.i-no    EQ po-ordl.i-no:

    {fg/updfgitm.i po-all i-no NO}
  END.

  FOR EACH po-rcpts
      WHERE po-rcpts.company EQ itemfg.company
        AND po-rcpts.po-no   EQ trim(string(po-ordl.po-no,">>>>>>>>>9"))
        AND po-rcpts.line    EQ po-ordl.line
        AND po-rcpts.i-no    EQ po-ordl.i-no:

    {fg/updfgitm.i po-rcpts i-no NO}
  END.

  {fg/updfgitm.i po-ordl i-no NO}
END.

FOR EACH EDICXref
    WHERE EDICXref.company EQ itemfg.company
      AND EDICXref.Item-no EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i EDICXref Item-no NO}
END.

FOR EACH EDIVLine
    WHERE EDIVLine.company EQ itemfg.company
      AND EDIVLine.Item-no EQ itemfg.i-no
    NO-LOCK:

  {fg/updfgitm.i EDIVLine Item-no NO}
END.

FOR EACH EDPD WHERE EDPD.Item-no EQ itemfg.i-no NO-LOCK:
  {fg/updfgitm.i EDPD Item-no NO}
END.

FOR EACH EDPOLine WHERE EDPOLine.Item-no EQ itemfg.i-no NO-LOCK:
  {fg/updfgitm.i EDPOLine Item-no NO}
END.

FOR EACH EDSHLine WHERE EDSHLine.Item-no EQ itemfg.i-no NO-LOCK:
  {fg/updfgitm.i EDSHLine Item-no NO}
END.

FOR EACH pdd WHERE pdd.Item-no EQ itemfg.i-no NO-LOCK:
  {fg/updfgitm.i pdd Item-no NO}
END.

FOR EACH truck-run-print WHERE truck-run-print.i-no EQ itemfg.i-no NO-LOCK:
  {fg/updfgitm.i truck-run-print i-no NO}
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "jc/jc-calc.p"
      AND reftable.company  EQ itemfg.company
      AND reftable.loc      EQ ""
      AND reftable.code2    EQ itemfg.i-no
    USE-INDEX code2 NO-LOCK:

  {fg/updfgitm.i reftable code2 NO}
END.

/*FOR EACH reftable                            */
/*    WHERE reftable.reftable EQ "FGSTATUS"    */
/*      AND reftable.company  EQ itemfg.company*/
/*      AND reftable.loc      EQ ""            */
/*      AND reftable.CODE     EQ itemfg.i-no:  */
/*                                             */
/*    reftable.CODE = v-new-item.              */
/*END.                                         */
 




IF CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part") THEN
  RUN fg/updfgprt.p (ROWID(itemfg), v-new-item).

DO TRANSACTION:
  IF AVAIL b-itemfg THEN DELETE itemfg.
  
  ELSE DO:
    DISABLE TRIGGERS FOR LOAD OF itemfg.
    itemfg.i-no = v-new-item.
  END.
END.

IF AVAIL b-itemfg THEN
  IF PROGRAM-NAME(2) BEGINS "util/mergbycp." THEN
    RUN fg/fg-reset.p (RECID(b-itemfg)).
  ELSE
    RUN fg/d-reqtys.w (ROWID(b-itemfg), YES).
