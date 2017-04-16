
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i NEW SHARED}

def NEW shared buffer xest for est.
def NEW shared buffer xef  for ef.
def NEW shared buffer xeb  for eb.

{ce/print4.i "new shared" "new shared"}

def NEW shared var save_id as recid NO-UNDO.
def NEW shared var chosen as logical format "y/n" NO-UNDO.
DEF NEW SHARED VAR CALL_id AS RECID NO-UNDO. 
DEF NEW SHARED VAR qty AS INT NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.

DEF BUFFER b-ef FOR ef.
DEF BUFFER b-est-op FOR est-op.
DEF BUFFER b-est-prep FOR est-prep.
DEF BUFFER b-est-inst FOR est-inst.
DEF BUFFER b-est-flm FOR est-flm.
DEF BUFFER b-est-ref FOR reftable.

DEF VAR li AS INT NO-UNDO.

{ce/mach-ink.i NEW}

                     
SESSION:SET-WAIT-STATE ("general").

FIND est WHERE ROWID(est) EQ ip-rowid NO-ERROR.

IF AVAIL est THEN DO:
  ASSIGN
   cocode = est.company
   locode = est.loc
   est.est-type = 4.

  FIND FIRST ef OF est NO-ERROR.
  IF AVAIL ef THEN DO:
    ef.est-type = 4.

    FOR EACH eb OF est WHERE eb.blank-no NE 1 NO-LOCK:
      FOR EACH est-op
          WHERE est-op.company EQ ef.company
            AND est-op.est-no  EQ ef.est-no
            AND est-op.s-num   EQ ef.form-no
            AND est-op.b-num   EQ 0
            AND est-op.dept    NE "DM"
            AND est-op.dept    NE "PD"
          NO-LOCK
          BY est-op.line:

        FOR EACH b-est-op
            WHERE b-est-op.company EQ est.company
              AND b-est-op.est-no  EQ est.est-no
              AND b-est-op.line    LT 500
            BY est-op.line DESC:
          LEAVE.
        END.
        li = (IF AVAIL b-est-op THEN b-est-op.line ELSE 0) + 1.

        CREATE b-est-op.
        BUFFER-COPY est-op EXCEPT rec_key TO b-est-op
        ASSIGN
         b-est-op.s-num = eb.blank-no
         b-est-op.line  = li.
      END.

      /*FOR EACH est-prep
          WHERE est-prep.company  EQ ef.company
            AND est-prep.est-no   EQ ef.est-no
            AND est-prep.s-num    EQ ef.form-no
            AND est-prep.mat-type NE "R"
            AND est-prep.b-num    EQ 0:

        FOR EACH b-est-prep
            WHERE b-est-prep.company EQ est.company 
              AND b-est-prep.est-no  EQ est.est-no                      
            USE-INDEX est-qty NO-LOCK
            BY b-est-prep.line DESC:
          LEAVE.
        END.
        li = (IF AVAIL b-est-prep THEN b-est-prep.line ELSE 0) + 1.

        CREATE b-est-prep.
        BUFFER-COPY est-prep EXCEPT rec_key TO b-est-prep
        ASSIGN
         b-est-prep.s-num = eb.blank-no
         b-est-prep.line  = li.
      END.*/

      FOR EACH est-inst
          WHERE est-inst.company EQ ef.company
            AND est-inst.est-no  EQ ef.est-no
            AND est-inst.line    EQ ef.form-no:
        CREATE b-est-inst.
        BUFFER-COPY est-inst EXCEPT rec_key TO b-est-inst
        ASSIGN
         b-est-inst.line = eb.blank-no.
      END.

      FOR EACH est-flm
          WHERE est-flm.company EQ ef.company
            AND est-flm.est-no  EQ ef.est-no
            AND est-flm.snum    EQ ef.form-no
            AND est-flm.bnum    EQ 0:
        CREATE b-est-flm.
        BUFFER-COPY est-flm EXCEPT rec_key TO b-est-flm
        ASSIGN
         b-est-flm.snum = eb.blank-no.
      END.

      FOR EACH reftable {ce/est-mrpl.i ef}:
        CREATE b-est-ref.
        BUFFER-COPY reftable EXCEPT rec_key TO b-est-ref
        ASSIGN
         b-est-ref.code2 = STRING(eb.blank-no,"9999999999") +
                           SUBSTR(reftable.code2,11,10).
      END.

      CREATE b-ef.
      BUFFER-COPY ef EXCEPT rec_key TO b-ef
      ASSIGN
       b-ef.form-no = eb.blank-no.
    END.
  END.

  FOR EACH eb OF est:
    ASSIGN
     eb.est-type = 4
     eb.yld-qty  = eb.bl-qty
     eb.form-no  = eb.blank-no
     eb.blank-no = 1.

    FIND FIRST ef OF eb NO-ERROR.
    IF AVAIL ef THEN DO:
      ASSIGN
       ef.f-col    = eb.i-col
       ef.f-pass   = eb.i-pass
       ef.f-coat   = eb.i-coat
       ef.f-coat-p = eb.i-coat-p.

      IF ef.f-col + ef.f-coat EQ 0 THEN DO:
        {ce/delplate.i}
      END.
    END.
  END.

  FOR EACH ef OF est NO-LOCK:
    {sys/inc/flm-prep.i}
  END.

  FIND xest WHERE ROWID(xest) EQ ROWID(est) NO-ERROR.
  IF AVAIL xest THEN DO:
    FOR EACH est-op
        WHERE est-op.company EQ xest.company
          AND est-op.est-no  EQ xest.est-no
          AND est-op.line    LT 500
          AND est-op.op-sb   EQ YES:
      est-op.b-num = 0.
    END.
        
    li = 0.
    FOR EACH est-op
        WHERE est-op.company EQ xest.company
          AND est-op.est-no  EQ xest.est-no
          AND est-op.line    LT 500
        BY est-op.s-num
        BY est-op.b-num
        BY est-op.line:
      li = li + 1.
      est-op.line = li.
    END.

    FOR EACH xef
        WHERE xef.company EQ xest.company
          AND xef.est-no  EQ xest.est-no:
      xef.op-lock = YES.
      RUN ce/com/localk.p (0).
    END.

    for each est-op where est-op.company = xest.company and
                          est-op.est-no = xest.est-no break by est-op.s-num:
      if first-of(est-op.s-num) then do:
        find b-ef where b-ef.company = xest.company and
                         b-ef.est-no = xest.est-no and b-ef.form-no = est-op.s-num no-error.
        if available b-ef then assign b-ef.op-lock = true
                                       b-ef.gsh-qty = est-op.num-sh.
      end.
    end.
  END.

  RELEASE xeb.
  RELEASE xef.
  RELEASE xest.
END.

SESSION:SET-WAIT-STATE ("").
