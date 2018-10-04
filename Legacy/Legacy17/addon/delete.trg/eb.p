&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME eb

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FOR EACH est-op WHERE est-op.company EQ eb.company
                  AND est-op.est-no  EQ eb.est-no
                  AND est-op.s-num   EQ eb.form-no
                  AND est-op.b-num   EQ eb.blank-no:
    DELETE est-op.
END.
FOR EACH est-prep WHERE est-prep.company EQ eb.company
                    AND est-prep.est-no  EQ eb.est-no
                    AND est-prep.s-num   EQ eb.form-no
                    AND est-prep.b-num   EQ eb.blank-no:
    DELETE est-prep.
END.
FOR EACH est-flm WHERE est-flm.company EQ eb.company
                   AND est-flm.est-no  EQ eb.est-no
                   AND est-flm.snum    EQ eb.form-no
                   AND est-flm.bnum    EQ eb.blank-no:
    DELETE est-flm.
END.
FOR EACH box-design-hdr WHERE box-design-hdr.company  EQ eb.company
                          AND box-design-hdr.est-no   EQ eb.est-no
                          AND box-design-hdr.form-no  EQ eb.form-no
                          AND box-design-hdr.blank-no EQ eb.blank-no:
  DELETE box-design-hdr.
END.
