
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

DEF VAR lv-est-no LIKE est.est-no NO-UNDO.
DEF VAR ll AS LOG INIT YES NO-UNDO.


locode = g_loc.

MESSAGE "Complete run?"
    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
    UPDATE ll.

IF NOT ll THEN
FIND LAST est
    WHERE company EQ cocode
      AND est-no  BEGINS FILL(" ",5 - LENGTH(TRIM(est-no)))
    NO-LOCK NO-ERROR.

IF AVAIL est OR ll THEN
FOR EACH company WHERE company.company EQ g_company OR ll:
  IF AVAIL est THEN lv-est-no = est-no.

  cocode = company.company.

  {util/bldest#8.i ar-inv est-no}
  {util/bldest#8.i ar-invl est-no}
  {util/bldest#8.i ar-invm est-no}
  {util/bldest#8.i box-design-hdr est-no}
  {util/bldest#8.i box-design-line est-no}
  {util/bldest#8.i e-item-vend est-no}
  {util/bldest#8.i eb est-no}
  {util/bldest#8.i eb master-est-no}
  {util/bldest#8.i ef est-no}
  {util/bldest#8.i ef-nsh est-no}
  {util/bldest#8.i est est-no}
  {util/bldest#8.i est-flm est-no}
  {util/bldest#8.i est-inst est-no}
  {util/bldest#8.i est-op est-no}
  {util/bldest#8.i est-pf est-no}
  {util/bldest#8.i est-prep est-no}
  {util/bldest#8.i est-qty est-no}
  {util/bldest#8.i est-summ est-no}
  {util/bldest#8.i inv-line est-no}
  {util/bldest#8.i inv-misc est-no}
  {util/bldest#8.i itemfg est-no}
  {util/bldest#8.i itemfgdtl est-no}
  {util/bldest#8.i job est-no}
  {util/bldest#8.i job-hdr est-no}
  {util/bldest#8.i oe-ord est-no}
  {util/bldest#8.i oe-ordl est-no}
  {util/bldest#8.i oe-ordm est-no}
  {util/bldest#8.i oe-retl est-no}
  {util/bldest#8.i probe est-no}
  {util/bldest#8.i probeit est-no}
  {util/bldest#8.i probeit-price est-no}
  {util/bldest#8.i quote est-no}
  {util/bldest#8.i quotehd est-no}
  {util/bldest#8.i quoteit est-no}
  {util/bldest#8.i quoteitm est-no}

  DISABLE TRIGGERS FOR LOAD OF reftable.

  FOR EACH reftable
      WHERE reftable.reftable   EQ "est/getqty.w"
        AND TRIM(reftable.code) NE ""
      TRANSACTION:
    RUN fix (INPUT-OUTPUT reftable.code).
  END.
END.

RETURN.

PROCEDURE fix.
  DEF INPUT-OUTPUT PARAM io-est-no LIKE est.est-no NO-UNDO.

  io-est-no = FILL(" ",8 - LENGTH(TRIM(io-est-no))) + TRIM(io-est-no).
END.
