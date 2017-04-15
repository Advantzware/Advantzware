&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME eb

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF VAR li AS INT NO-UNDO.


FOR EACH est-op
    WHERE est-op.company EQ {&TABLENAME}.company
      AND est-op.est-no  EQ {&TABLENAME}.est-no
      AND est-op.s-num   EQ {&TABLENAME}.form-no
      AND est-op.b-num   EQ {&TABLENAME}.blank-no:
  DELETE est-op.
END.

FOR EACH est-prep
    WHERE est-prep.company EQ {&TABLENAME}.company
      AND est-prep.est-no  EQ {&TABLENAME}.est-no
      AND est-prep.s-num   EQ {&TABLENAME}.form-no
      AND est-prep.b-num   EQ {&TABLENAME}.blank-no:
  DELETE est-prep.
END.

FOR EACH est-flm
    WHERE est-flm.company EQ {&TABLENAME}.company
      AND est-flm.est-no  EQ {&TABLENAME}.est-no
      AND est-flm.snum    EQ {&TABLENAME}.form-no
      AND est-flm.bnum    EQ {&TABLENAME}.blank-no:
  DELETE est-flm.
END.

FOR EACH box-design-hdr
    WHERE box-design-hdr.design-no EQ 0
      AND box-design-hdr.company   EQ {&TABLENAME}.company
      AND box-design-hdr.est-no    EQ {&TABLENAME}.est-no
      AND box-design-hdr.form-no   EQ {&TABLENAME}.form-no
      AND box-design-hdr.blank-no  EQ {&TABLENAME}.blank-no:
  DELETE box-design-hdr.
END.

FOR EACH {est/plfoun-w.i reftable {&TABLENAME}}:
  DELETE reftable.
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "ce/v-est3.w Unit#"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ {&TABLENAME}.est-no
      AND reftable.code     EQ STRING({&TABLENAME}.form-no,"9999999999")
      AND reftable.code2    EQ STRING({&TABLENAME}.blank-no,"9999999999"):
  DELETE reftable.
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "cedepth"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ {&TABLENAME}.est-no
      AND reftable.code     EQ STRING({&TABLENAME}.form-no,"9999999999")
      AND reftable.code2    EQ STRING({&TABLENAME}.blank-no,"9999999999"):
  DELETE reftable.
END.

FOR EACH e-itemfg-vend
    WHERE e-itemfg-vend.company  EQ {&TABLENAME}.company
      AND e-itemfg-vend.est-no   EQ {&TABLENAME}.est-no
      AND e-itemfg-vend.form-no  EQ {&TABLENAME}.form-no
      AND e-itemfg-vend.blank-no EQ {&TABLENAME}.blank-no:

  DELETE e-itemfg-vend.
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "ce/com/selwhif1.w"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ {&TABLENAME}.est-no
      AND reftable.code     EQ STRING({&TABLENAME}.form-no,"9999999999")
      AND reftable.code2    EQ STRING({&TABLENAME}.blank-no,"9999999999"):
  DELETE reftable.
END.

FOR EACH ef OF {&TABLENAME}:
  DO li = 1 TO EXTENT(ef.leaf):
    IF ef.leaf-bnum[li] EQ {&TABLENAME}.blank-no THEN ef.leaf[li] = "".
  END.
  DO li = 1 TO EXTENT(ef.mis-cost):
    IF ef.mis-bnum[li] EQ {&TABLENAME}.blank-no THEN ef.mis-cost[li] = "".
  END.

  RUN est/updefdie.p (ROWID(ef)).
  ef.blank-qty = 0.
  LEAVE.
END.

FOR EACH est
    WHERE est.company EQ {&TABLENAME}.company
      AND est.est-no  EQ {&TABLENAME}.est-no:
  ASSIGN
   est.updated-date = TODAY
   est.updated-id   = USERID("nosweat").
  LEAVE.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
