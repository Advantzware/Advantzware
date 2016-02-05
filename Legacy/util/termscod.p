/* util/termscod.p */

{custom/patchBegin.i 1 20050426 1 "util/termscod.r"}
FOR EACH terms NO-LOCK:
  IF CAN-FIND(FIRST reftable WHERE reftable.reftable EQ 'terms.cod'
                               AND reftable.company EQ terms.company
                               AND reftable.loc EQ ''
                               AND reftable.code EQ terms.t-code) THEN NEXT.
  CREATE reftable.
  ASSIGN
    reftable.reftable = 'terms.cod'
    reftable.company = terms.company
    reftable.code = terms.t-code.
END.
{custom/patchEnd.i}
MESSAGE 'Terms COD Reftable Create Complete!' VIEW-AS ALERT-BOX.
