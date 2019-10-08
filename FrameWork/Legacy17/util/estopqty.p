{custom/patchBegin.i 1 20050426 1 "util/EstOPQty.r"}
FOR EACH est WHERE est-type GE 5,
    EACH est-op WHERE est-op.company EQ est.company
                  AND est-op.est-no  EQ est.est-no:
  est-op.qty = est.est-qty[1].
END.
{custom/patchEnd.i}
