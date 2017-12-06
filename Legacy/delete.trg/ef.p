&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME ef

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

FOR EACH ef-nsh
    WHERE ef-nsh.company EQ {&TABLENAME}.company
      AND ef-nsh.est-no  EQ {&TABLENAME}.est-no
      AND ef-nsh.form-no EQ {&TABLENAME}.form-no:
  DELETE ef-nsh.
END.

FOR EACH est-op
    WHERE est-op.company EQ {&TABLENAME}.company
      AND est-op.est-no  EQ {&TABLENAME}.est-no
      AND est-op.s-num   EQ {&TABLENAME}.form-no:
  DELETE est-op.
END.

FOR EACH est-prep
    WHERE est-prep.company EQ {&TABLENAME}.company
      AND est-prep.est-no  EQ {&TABLENAME}.est-no
      AND est-prep.s-num   EQ {&TABLENAME}.form-no:
  DELETE est-prep.
END.

FOR EACH est-inst
    WHERE est-inst.company EQ {&TABLENAME}.company
      AND est-inst.est-no  EQ {&TABLENAME}.est-no
      AND est-inst.line    EQ {&TABLENAME}.form-no:
  DELETE est-inst.
END.

FOR EACH est-flm
    WHERE est-flm.company EQ {&TABLENAME}.company
      AND est-flm.est-no  EQ {&TABLENAME}.est-no
      AND est-flm.snum    EQ {&TABLENAME}.form-no:
  DELETE est-flm.
END.

FOR EACH box-design-hdr
    WHERE box-design-hdr.design-no EQ 0
      AND box-design-hdr.company   EQ {&TABLENAME}.company
      AND box-design-hdr.est-no    EQ {&TABLENAME}.est-no
      AND box-design-hdr.form-no   EQ {&TABLENAME}.form-no:
  DELETE box-design-hdr.
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "EST-MISC"
	  AND reftable.company  EQ {&TABLENAME}.company
	  AND reftable.loc      EQ {&TABLENAME}.loc
      AND reftable.code     EQ TRIM({&TABLENAME}.est-no) + STRING({&TABLENAME}.form-no,"/99"):
  DELETE reftable.
END.

FOR EACH reftable {ce/est-mrpl.i {&TABLENAME}}:
  DELETE reftable.
END.

FIND FIRST est OF {&TABLENAME} NO-LOCK NO-ERROR.
IF AVAIL est THEN
FOR EACH notes
    WHERE notes.rec_key      EQ est.rec_key
      AND notes.note_form_no EQ {&TABLENAME}.form-no:
  DELETE notes.
END.

FOR EACH eb
    WHERE eb.company EQ {&TABLENAME}.company
      AND eb.est-no  EQ {&TABLENAME}.est-no
      AND eb.form-no EQ {&TABLENAME}.form-no:
  DELETE eb.
END.

FOR EACH e-itemfg-vend
    WHERE e-itemfg-vend.company  EQ {&TABLENAME}.company
      AND e-itemfg-vend.est-no   EQ {&TABLENAME}.est-no
      AND e-itemfg-vend.form-no  EQ {&TABLENAME}.form-no:

  DELETE e-itemfg-vend.
END.

FOR EACH reftable
    WHERE reftable.rec_key  EQ {&TABLENAME}.rec_key
      AND reftable.reftable EQ "LAYOUT UPDATED"
      AND reftable.company  EQ "{&TABLENAME}"
    USE-INDEX rec_key:

  DELETE reftable.
END.

FOR EACH est
    WHERE est.company EQ {&TABLENAME}.company
      AND est.est-no  EQ {&TABLENAME}.est-no:
  ASSIGN
   est.updated-date = TODAY
   est.updated-id   = USERID("nosweat")
   est.form-qty     = 0.
  LEAVE.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
