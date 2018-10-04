&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME est

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FOR EACH est-qty WHERE est-qty.company = est.company
                   AND est-qty.est-no = est.est-no:
    DELETE est-qty.
END.
FOR EACH ef WHERE ef.company = est.company
              AND ef.est-no = est.est-no:
    DELETE ef.
END.
FOR EACH eb WHERE eb.company = est.company
              AND eb.est-no = est.est-no:
    DELETE eb.
END.
FOR EACH est-op WHERE est-op.company = est.company
                  AND est-op.est-no = est.est-no :
    DELETE est-op.
END.
FOR EACH est-prep WHERE est-prep.company = est.company
                     AND est-prep.est-no = est.est-no:
    DELETE est-prep.
END.
FOR EACH est-inst WHERE est-inst.company = est.company
                     AND est-inst.est-no = est.est-no:
    DELETE est-inst.
END.
 FOR EACH est-flm WHERE est-flm.company = est.company
                     AND est-flm.est-no = est.est-no:
    DELETE est-flm.
END.

FOR EACH box-design-hdr WHERE box-design-hdr.company EQ est.company
                          AND box-design-hdr.est-no  EQ est.est-no:
  DELETE box-design-hdr.
END. 

FOR EACH notes where notes.rec_key   eq est.rec_key :
    DELETE notes.
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "est/getqty.w"
      AND reftable.company  EQ est.company
      AND reftable.loc      EQ est.loc
      AND reftable.code     EQ est.est-no:
  DELETE reftable.
END.

FOR EACH reftable WHERE
    reftable.reftable EQ "gsa-fm" AND
    reftable.company  EQ est.company AND
    reftable.loc      EQ "" AND
    reftable.code     EQ est.est-no:

    DELETE reftable.
END.

FOR EACH probe
    WHERE probe.company EQ est.company
      AND probe.est-no  EQ est.est-no:
  DELETE probe.
END.

FOR EACH e-itemfg-vend
    WHERE e-itemfg-vend.company  EQ {&TABLENAME}.company
      AND e-itemfg-vend.est-no   EQ {&TABLENAME}.est-no:

  DELETE e-itemfg-vend.
END.
