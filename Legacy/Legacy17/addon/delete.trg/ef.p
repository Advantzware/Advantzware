&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME ef

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FOR EACH eb WHERE eb.company EQ ef.company
              AND eb.est-no  EQ ef.est-no
              AND eb.form-no EQ ef.form-no:
    DELETE eb.
END.
FOR EACH est-op WHERE est-op.company EQ ef.company
                  AND est-op.est-no  EQ ef.est-no
                  AND est-op.s-num   EQ ef.form-no:
    DELETE est-op.
END.
FOR EACH est-prep WHERE est-prep.company EQ ef.company
                    AND est-prep.est-no  EQ ef.est-no
                    AND est-prep.s-num   EQ ef.form-no:
    DELETE est-prep.
END.
FOR EACH est-inst WHERE est-inst.company EQ ef.company
                    AND est-inst.est-no  EQ ef.est-no
                    AND est-inst.LINE    EQ ef.form-no:
    DELETE est-inst.
END.
 FOR EACH est-flm WHERE est-flm.company EQ ef.company
                    AND est-flm.est-no  EQ ef.est-no
                    AND est-flm.snum    EQ ef.form-no:
    DELETE est-flm.
END.
FOR EACH box-design-hdr WHERE box-design-hdr.company EQ ef.company
                          AND box-design-hdr.est-no  EQ ef.est-no
                          AND box-design-hdr.form-no EQ ef.form-no:
    DELETE box-design-hdr.
END.
FOR EACH reftable {ce/est-mrpl.i ef}:
    DELETE reftable.
END.
