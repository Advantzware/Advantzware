&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME reftable

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}
                         
DEF VAR lv-est-no LIKE est.est-no NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-rec-type AS CHAR NO-UNDO.
DEF VAR lv-val-type AS CHAR NO-UNDO.

DEF BUFFER b-ref FOR {&TABLENAME}.

DEF TEMP-TABLE w-ref FIELD w-qty AS DEC FIELD w-cst AS DEC.

DISABLE TRIGGERS FOR LOAD OF est.
DISABLE TRIGGERS FOR LOAD OF b-ref.
DISABLE TRIGGERS FOR LOAD OF po-ord.
DISABLE TRIGGERS FOR LOAD OF po-ordl.


IF LOOKUP({&TABLENAME}.reftable,"EST-MISC,MACH-CREW") GT 0 THEN DO:
  IF {&TABLENAME}.reftable EQ "EST-MISC" THEN DO:
    lv-est-no = "".
    DO li = 1 TO LENGTH({&TABLENAME}.code):
      IF SUBSTR({&TABLENAME}.code,1,1) EQ "/" THEN LEAVE.
      lv-est-no = lv-est-no + SUBSTR({&TABLENAME}.code,1,1).
    END.
    lv-est-no = FILL(" ",8 - LENGTH(TRIM(lv-est-no))) + TRIM(lv-est-no).

    IF TRIM(lv-est-no) NE "" THEN RUN update-est (lv-est-no).
  END.

  ASSIGN
   lv-rec-type = SUBSTR({&TABLENAME}.code2,1,3)
   lv-val-type = SUBSTR({&TABLENAME}.code2,5,3)
   li          = INT(SUBSTR({&TABLENAME}.code2,8,2)).

  IF lv-val-type EQ "QTY" THEN DO:
    FIND FIRST b-ref
        WHERE b-ref.reftable EQ {&TABLENAME}.reftable
          AND b-ref.company  EQ {&TABLENAME}.company
          AND b-ref.loc      EQ {&TABLENAME}.loc
          AND b-ref.code     EQ {&TABLENAME}.code
          AND b-ref.code2    EQ lv-rec-type + "-CST" +
                                (IF li GT 0 THEN STRING(li,"99") ELSE "")
        NO-ERROR.

    DO li = 1 TO EXTENT({&TABLENAME}.val):
      CREATE w-ref.
      w-qty = {&TABLENAME}.val[li].
      IF AVAIL b-ref THEN w-cst = b-ref.val[li].
    END.
    FOR EACH w-ref WHERE BREAK BY w-qty BY w-cst:
      IF (w-qty EQ 0 OR NOT LAST-OF(w-qty)) AND
         NOT LAST(w-qty)                    THEN DELETE w-ref.
    END.

    {&TABLENAME}.val = 0.
    IF AVAIL b-ref THEN b-ref.val = 0.

    li = 0.
    FOR EACH w-ref BREAK BY w-qty BY w-cst:
      IF LAST-OF(w-qty) THEN DO:
        li = li + 1.
        IF li LE EXTENT({&TABLENAME}.val) THEN DO:
          {&TABLENAME}.val[li] = IF LAST(w-qty) THEN 99999999 ELSE w-qty.
          IF AVAIL b-ref THEN b-ref.val[li] = w-cst.
        END.
      END.
    END.
  END.
END.

ELSE
IF {&TABLENAME}.reftable EQ "PLATE/FOUNTAIN"    OR
   {&TABLENAME}.reftable EQ "ce/v-est3.w Unit#" OR
   {&TABLENAME}.reftable EQ "cedepth" THEN RUN update-est ({&TABLENAME}.loc).

ELSE
IF {&TABLENAME}.reftable EQ "est/getqty.w" THEN RUN update-est ({&TABLENAME}.code).

ELSE
IF {&TABLENAME}.reftable EQ "POLSCORE" THEN DO:
  IF old-{&TABLENAME}.reftable NE "" THEN
  FOR EACH po-ordl
      WHERE po-ordl.company EQ reftable.company
       AND po-ordl.po-no    EQ INT(reftable.code)
       AND po-ordl.line     EQ INT(reftable.code2),
      FIRST po-ord
      WHERE po-ord.company EQ po-ordl.company
        AND po-ord.po-no   EQ po-ordl.po-no:

    IF po-ordl.stat NE "U" AND
       po-ordl.stat NE "C" AND
       po-ord.printed      AND
       po-ord.opened       THEN po-ordl.stat = "U".
    
    IF po-ordl.stat EQ "U" AND
       po-ord.stat NE "H"  THEN po-ord.stat = "U".
  END.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

RETURN.

/* Procedures */
PROCEDURE update-est.
  DEF INPUT PARAM ip-est-no LIKE est.est-no NO-UNDO.

    
  FIND FIRST est
      WHERE est.company EQ {&TABLENAME}.company
        AND est.est-no  EQ ip-est-no
      NO-ERROR.

  IF AVAIL est THEN DO:
    ASSIGN
     est.updated-date = TODAY
     est.updated-id   = USERID("nosweat")
     est.mod-date     = est.updated-date.
  END.

END PROCEDURE.
