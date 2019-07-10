
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF BUFFER b-oe-ordl FOR oe-ordl.

DEF VAR cocode AS CHAR NO-UNDO.

FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

DEF TEMP-TABLE tt-oe-rel NO-UNDO
    FIELD ROWID AS ROWID.

IF AVAIL oe-ordl THEN DO:
  cocode = oe-ordl.company.

  {sys/ref/relpost.i}

  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
        AND oe-rel.link-no GT 0
        AND NOT CAN-FIND(FIRST oe-boll
                         WHERE oe-boll.company  EQ oe-rel.company
                           AND oe-boll.r-no     EQ oe-rel.link-no
                           AND oe-boll.ord-no   EQ oe-rel.ord-no
                           AND oe-boll.i-no     EQ oe-rel.i-no
                           AND oe-boll.line     EQ oe-rel.line
                           AND oe-boll.rel-no   EQ oe-rel.rel-no
                           AND oe-boll.b-ord-no EQ oe-rel.b-ord-no
                           AND oe-boll.po-no    EQ oe-rel.po-no
                         USE-INDEX ord-no)
      USE-INDEX ord-item
      /*TRANSACTION*/ :

    IF relpost-chr NE "Nothing"     OR
       NOT CAN-FIND(FIRST oe-rell  
                    WHERE oe-rell.company  EQ oe-rel.company
                      AND oe-rell.r-no     EQ oe-rel.link-no
                      AND oe-rell.ord-no   EQ oe-rel.ord-no
                      AND oe-rell.i-no     EQ oe-rel.i-no
                      AND oe-rell.line     EQ oe-rel.line
                      AND oe-rell.rel-no   EQ oe-rel.rel-no
                      AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                      AND oe-rell.po-no    EQ oe-rel.po-no
                    USE-INDEX r-no) OR
       NOT CAN-FIND(FIRST oe-rell
                    WHERE oe-rell.company  EQ oe-rel.company
                      AND oe-rell.r-no     EQ oe-rel.link-no
                      AND oe-rell.link-no  EQ oe-rel.r-no
                    USE-INDEX r-no) THEN
    DO:
       CREATE tt-oe-rel.
       tt-oe-rel.ROWID = ROWID(oe-rel).
       RELEASE tt-oe-rel.
      /* DELETE oe-rel. */
    END.

    ELSE
    FOR EACH oe-rell
        WHERE oe-rell.company EQ oe-rel.company
          AND oe-rell.r-no    EQ oe-rel.link-no
          AND CAN-FIND(FIRST oe-boll
                       WHERE oe-boll.company  EQ oe-rell.company
                         AND oe-boll.ord-no   EQ oe-rell.ord-no
                         AND oe-boll.i-no     EQ oe-rell.i-no
                         AND oe-boll.line     EQ oe-rell.line
                         AND oe-boll.r-no     EQ oe-rell.r-no
                         AND oe-boll.rel-no   EQ oe-rell.rel-no
                         AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
                         AND oe-boll.po-no    EQ oe-rell.po-no
                       USE-INDEX ord-no)
        NO-LOCK:
      
       CREATE tt-oe-rel.
       tt-oe-rel.ROWID = ROWID(oe-rel).
       RELEASE tt-oe-rel.
       /* DELETE oe-rel. */
      
      LEAVE.
    END.
  END.

  FOR EACH tt-oe-rel,
      FIRST oe-rel WHERE
            ROWID(oe-rel) EQ tt-oe-rel.ROWID
      TRANSACTION:

      DELETE oe-rel.
  END.

  EMPTY TEMP-TABLE tt-oe-rel.

  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
        AND oe-rel.rel-no  GT 0
      USE-INDEX ord-item
      
      BREAK BY oe-rel.rel-no
            BY oe-rel.b-ord-no
            BY oe-rel.link-no DESC

      /*TRANSACTION*/ :
    
    IF NOT FIRST-OF(oe-rel.b-ord-no) AND oe-rel.link-no EQ 0 THEN
    DO:
       CREATE tt-oe-rel.
       tt-oe-rel.ROWID = ROWID(oe-rel).
       RELEASE tt-oe-rel.
       /* DELETE oe-rel. */
    END.
  END.

  FOR EACH tt-oe-rel,
      FIRST oe-rel WHERE
            ROWID(oe-rel) EQ tt-oe-rel.ROWID
            TRANSACTION:

      DELETE oe-rel.
  END.

  EMPTY TEMP-TABLE tt-oe-rel.

  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
      USE-INDEX ord-item
      
      BREAK BY oe-rel.rel-no
            BY oe-rel.b-ord-no
            BY oe-rel.po-no

      /*TRANSACTION*/ :
  
    IF CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}) OR
       (NOT LAST-OF(oe-rel.po-no) AND oe-rel.rel-no NE 0)               THEN
       DO:
          CREATE tt-oe-rel.
          tt-oe-rel.ROWID = ROWID(oe-rel).
          RELEASE tt-oe-rel.
          /* DELETE oe-rel. */
       END.
  END.
  
  FOR EACH tt-oe-rel,
      FIRST oe-rel WHERE
            ROWID(oe-rel) EQ tt-oe-rel.ROWID
            TRANSACTION:

      DELETE oe-rel.
  END.
END.
