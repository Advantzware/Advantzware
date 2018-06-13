
FOR EACH est-flm
    WHERE est-flm.company EQ {1}company
      AND est-flm.est-no  EQ {1}est-no
      AND est-flm.snum    EQ {1}form-no
      AND est-flm.bnum    EQ {2}:
  ASSIGN
   est-flm.snum = {3}
   est-flm.bnum = {4}.
END.

FOR EACH xeb-form-ef
    WHERE xeb-form-ef.company EQ {1}company
      AND xeb-form-ef.est-no  EQ {1}est-no
      AND xeb-form-ef.form-no EQ {1}form-no:
  IF {4} NE 0 THEN DO:
    DO li = 1 TO EXTENT(xeb-form-ef.leaf):
      IF xeb-form-ef.leaf-bnum[li] EQ {2} THEN
        xeb-form-ef.leaf-bnum[li] = {4}.
    END.
    DO li = 1 TO EXTENT(xeb-form-ef.mis-cost):
      IF xeb-form-ef.mis-bnum[li] EQ {2} THEN
        xeb-form-ef.mis-bnum[li] = {4}.
    END.
  END.
  DO li = 1 TO EXTENT(xeb-form-ef.leaf):
    IF xeb-form-ef.leaf[li] NE "" THEN
      xeb-form-ef.leaf-snum[li] = {3}.
  END.
  DO li = 1 TO EXTENT(xeb-form-ef.mis-cost):
    IF xeb-form-ef.mis-cost[li] NE "" THEN
      xeb-form-ef.mis-snum[li] = {3}.
  END.
END.

FOR EACH est-prep
    WHERE est-prep.company EQ {1}company
      AND est-prep.est-no  EQ {1}est-no
      AND est-prep.s-num   EQ {1}form-no
      AND est-prep.b-num   EQ {2}:
  ASSIGN
   est-prep.s-num = {3}
   est-prep.b-num = {4}.
END.

FOR EACH est-op
    WHERE est-op.company EQ {1}company
      AND est-op.est-no  EQ {1}est-no
      AND (est-op.qty    EQ {1}eqty OR {1}est-type GT 1)
      AND est-op.s-num   EQ {1}form-no
      AND est-op.b-num   EQ {2}:
  ASSIGN
   est-op.s-num = {3}
   est-op.b-num = {4}.
END.

IF {4} EQ 0 THEN
FOR EACH est-inst
    WHERE est-inst.company EQ {1}company
      AND est-inst.est-no  EQ {1}est-no
      AND est-inst.line    EQ {1}form-no:
  est-inst.line = {3}.
END.

FOR EACH box-design-line
    WHERE box-design-line.design-no EQ 0
      AND box-design-line.company   EQ {1}company
      AND box-design-line.est-no    EQ {1}est-no
      AND box-design-line.form-no   EQ {1}form-no
      AND box-design-line.blank-no  EQ {2}:

  ASSIGN
   box-design-line.form-no  = {3}
   box-design-line.blank-no = {4}.
END.

FOR EACH box-design-hdr
    WHERE box-design-hdr.design-no EQ 0
      AND box-design-hdr.company   EQ {1}company
      AND box-design-hdr.est-no    EQ {1}est-no
      AND box-design-hdr.form-no   EQ {1}form-no
      AND box-design-hdr.blank-no  EQ {2}:

  ASSIGN
   box-design-hdr.form-no  = {3}
   box-design-hdr.blank-no = {4}.
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "PLATE/FOUNTAIN"
      AND reftable.company  EQ {1}company
      AND reftable.loc      EQ {1}est-no
      AND reftable.code2    EQ STRING({1}form-no,"9999999999") +
                               STRING({2},"9999999999"):
  reftable.code2 = STRING({3},"9999999999") + STRING({4},"9999999999").
END.

IF {4} EQ 0 THEN
FOR EACH reftable
    WHERE reftable.reftable EQ "EST-MISC"
      AND reftable.company  EQ {1}company
      AND reftable.loc      EQ {1}loc
      AND reftable.code     EQ TRIM({1}est-no) + STRING({1}form-no,"/99"):
  reftable.code = TRIM({1}est-no) + STRING({3},"/99").
END.

IF NOT AVAIL est THEN
FIND FIRST est
    WHERE est.company EQ {1}company
      AND est.est-no  EQ {1}est-no
    NO-LOCK NO-ERROR.

IF AVAIL est AND {4} EQ 0 THEN
FOR EACH notes
    WHERE notes.rec_key      EQ est.rec_key
      AND notes.note_form_no EQ {1}form-no
      AND notes.note_form_no NE 0:
  notes.note_form_no = {3}.
END.
   
FOR EACH reftable
    WHERE reftable.reftable EQ "ce/v-est3.w Unit#"
      AND reftable.company  EQ {1}company
      AND reftable.loc      EQ {1}est-no
      AND reftable.code     EQ STRING({1}form-no,"9999999999")
      AND reftable.code2    EQ STRING({2},"9999999999"):
  ASSIGN
   reftable.code  = STRING({3},"9999999999")
   reftable.code2 = STRING({4},"9999999999").
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "cedepth"
      AND reftable.company  EQ {1}company
      AND reftable.loc      EQ {1}est-no
      AND reftable.code     EQ STRING({1}form-no,"9999999999")
      AND reftable.code2    EQ STRING({2},"9999999999"):
  ASSIGN
   reftable.code  = STRING({3},"9999999999")
   reftable.code2 = STRING({4},"9999999999").
END.

IF {4} EQ 0 THEN
FOR EACH ef-nsh
    WHERE ef-nsh.company EQ {1}company
      and ef-nsh.est-no  EQ {1}est-no
      AND ef-nsh.form-no EQ {1}form-no:
  ef-nsh.form-no = {3}.
END.

FOR EACH e-itemfg-vend
    WHERE e-itemfg-vend.company   EQ {1}company
      AND e-itemfg-vend.est-no    EQ {1}est-no
      AND e-itemfg-vend.form-no   EQ {1}form-no
      AND e-itemfg-vend.blank-no  EQ {2}:

  ASSIGN
   e-itemfg-vend.form-no  = {3}
   e-itemfg-vend.blank-no = {4}.
END.



/* end ---------------------------------- copr. 1999  advanced software, inc. */
