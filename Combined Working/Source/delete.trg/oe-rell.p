&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME oe-rell

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-itemfg-loc FOR itemfg-loc.

DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-q-alloc LIKE b-itemfg.q-alloc NO-UNDO.
DEF VAR lv-q-back LIKE b-itemfg.q-back NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.
DISABLE TRIGGERS FOR LOAD OF itemfg.
DISABLE TRIGGERS FOR LOAD OF oe-rel.


cocode = {&TABLENAME}.company.
{sys/inc/oereordr.i}

FOR EACH oe-ordl
    WHERE oe-ordl.company EQ {&TABLENAME}.company
      AND oe-ordl.ord-no  EQ {&TABLENAME}.ord-no
      AND oe-ordl.i-no    EQ {&TABLENAME}.i-no
      AND oe-ordl.line    EQ {&TABLENAME}.line
    USE-INDEX item-ord NO-LOCK:

  RUN oe/rell-qty.p (ROWID(oe-ordl), OUTPUT li).

  IF oe-ordl.t-rel-qty NE li THEN DO:
    FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl)
        EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF AVAIL b-oe-ordl THEN b-oe-ordl.t-rel-qty = li.
  END.

  LEAVE.
END.

IF {&TABLENAME}.b-ord-no GE 0 THEN DO:

    FOR EACH itemfg
        WHERE itemfg.company EQ {&TABLENAME}.company
          AND itemfg.i-no    EQ {&TABLENAME}.i-no
        NO-LOCK:
    
      RUN fg/calcqa&b.p (ROWID(itemfg), OUTPUT lv-q-alloc, OUTPUT lv-q-back).
    
      IF itemfg.q-alloc NE lv-q-alloc OR
         itemfg.q-back  NE lv-q-back  THEN DO:
        FIND b-itemfg WHERE ROWID(b-itemfg) EQ ROWID(itemfg)
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF AVAIL b-itemfg THEN
          ASSIGN
           b-itemfg.q-alloc = lv-q-alloc
           b-itemfg.q-back  = lv-q-back
           b-itemfg.q-avail = b-itemfg.q-onh +
                              (IF oereordr-cha EQ "XOnOrder" THEN 0
                                                             ELSE b-itemfg.q-ono) -
                              b-itemfg.q-alloc.
      END.
    
      LEAVE.
    END.
    RUN fg/chkfgloc.p (INPUT {&TABLENAME}.i-no, INPUT {&TABLENAME}.loc).
    FOR EACH itemfg-loc
        WHERE itemfg-loc.company EQ {&TABLENAME}.company
          AND itemfg-loc.i-no    EQ {&TABLENAME}.i-no
          AND itemfg-loc.loc     EQ {&TABLENAME}.loc
        NO-LOCK:
    
      RUN fg/calcqabl.p (ROWID(itemfg), itemfg-loc.loc, OUTPUT lv-q-alloc, OUTPUT lv-q-back).
    
      IF itemfg-loc.q-alloc NE lv-q-alloc OR
         itemfg-loc.q-back  NE lv-q-back  THEN DO:
        FIND b-itemfg-loc WHERE ROWID(b-itemfg-loc) EQ ROWID(itemfg-loc)
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF AVAIL b-itemfg-loc THEN
          ASSIGN
           b-itemfg-loc.q-alloc = lv-q-alloc
           b-itemfg-loc.q-back  = lv-q-back
           b-itemfg-loc.q-avail = b-itemfg-loc.q-onh +
                              (IF oereordr-cha EQ "XOnOrder" THEN 0
                                                             ELSE b-itemfg-loc.q-ono) -
                              b-itemfg-loc.q-alloc.
      END.
    
      LEAVE.
    END.
END.
IF NOT CAN-FIND(FIRST b-{&TABLENAME}
                WHERE b-{&TABLENAME}.company  EQ {&TABLENAME}.company
                  AND b-{&TABLENAME}.ord-no   EQ {&TABLENAME}.ord-no
                  AND b-{&TABLENAME}.i-no     EQ {&TABLENAME}.i-no
                  AND b-{&TABLENAME}.line     EQ {&TABLENAME}.line
                  AND b-{&TABLENAME}.r-no     EQ {&TABLENAME}.r-no
                  AND b-{&TABLENAME}.rel-no   EQ {&TABLENAME}.rel-no
                  AND b-{&TABLENAME}.b-ord-no EQ {&TABLENAME}.b-ord-no
                  AND ROWID(b-{&TABLENAME})   NE ROWID({&TABLENAME})
                USE-INDEX ord-no) THEN DO:

  FOR EACH oe-ordl
      WHERE oe-ordl.company EQ {&TABLENAME}.company
        AND oe-ordl.ord-no  EQ {&TABLENAME}.ord-no
        AND oe-ordl.i-no    EQ {&TABLENAME}.i-no
        AND oe-ordl.line    EQ {&TABLENAME}.line
      USE-INDEX item-ord NO-LOCK:
    FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl)
        EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF AVAIL b-oe-ordl THEN b-oe-ordl.rel-stat = NO.
  END.

  FOR each oe-rel WHERE oe-rel.r-no = oe-rell.link-no 
      /*oe-rel.link-no EQ {&TABLENAME}.r-no USE-INDEX link*/ :
    ASSIGN
     oe-rel.rel-no   = 0
     oe-rel.b-ord-no = 0
     oe-rel.link-no  = 0.

    FIND FIRST truck-run-print WHERE
         truck-run-print.company EQ {&TABLENAME}.company AND
         truck-run-print.oe-rel-r-no EQ {&TABLENAME}.r-no
         EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL truck-run-print THEN
    DO:
       ASSIGN
       truck-run-print.rel-no = 0
       truck-run-print.b-ord-no = 0
       truck-run-print.link-no = 0.

       RELEASE truck-run-print.
    END.
         
END.
  END.

  FOR EACH oe-rel WHERE oe-rel.r-no EQ {&TABLENAME}.link-no USE-INDEX seq-no:
    ASSIGN
     oe-rel.rel-no   = 0
     oe-rel.b-ord-no = 0
     oe-rel.link-no  = 0
     oe-rel.qty = oe-rel.qty - oe-rell.Qty.  
     IF oe-rel.qty < 0 THEN oe-rel.qty = 0.

    FIND FIRST truck-run-print WHERE
         truck-run-print.company EQ {&TABLENAME}.company AND
         truck-run-print.oe-rel-r-no EQ {&TABLENAME}.r-no
         EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL truck-run-print THEN
    DO:
       ASSIGN
       truck-run-print.rel-no = 0
       truck-run-print.b-ord-no = 0
       truck-run-print.link-no = 0.

       RELEASE truck-run-print.
    END.
  END.

  FOR EACH oe-rel
      WHERE oe-rel.company  EQ {&TABLENAME}.company
        AND oe-rel.ord-no   EQ {&TABLENAME}.ord-no
        AND oe-rel.rel-no   EQ {&TABLENAME}.rel-no
        AND oe-rel.b-ord-no EQ {&TABLENAME}.b-ord-no
        AND oe-rel.i-no     EQ {&TABLENAME}.i-no
        AND oe-rel.line     EQ {&TABLENAME}.line
      USE-INDEX ord-item:
    ASSIGN
     oe-rel.rel-no   = 0
     oe-rel.b-ord-no = 0
     oe-rel.link-no  = 0.

    FIND FIRST truck-run-print WHERE
         truck-run-print.company EQ {&TABLENAME}.company AND
         truck-run-print.oe-rel-r-no EQ {&TABLENAME}.r-no
         EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL truck-run-print THEN
    DO:
       ASSIGN
       truck-run-print.rel-no = 0
       truck-run-print.b-ord-no = 0
       truck-run-print.link-no = 0.

       RELEASE truck-run-print.
    END.
  END.

IF TRIM({&TABLENAME}.rec_key) NE "" THEN
FOR EACH reftable WHERE reftable.rec_key EQ {&TABLENAME}.rec_key:
  DELETE reftable.
END.

IF {&TABLENAME}.ord-no NE 0 THEN
FOR EACH oe-rel
    WHERE oe-rel.company EQ {&TABLENAME}.company
      AND oe-rel.ord-no  EQ {&TABLENAME}.ord-no:
  RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).
END.

