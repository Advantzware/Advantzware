/* ------------------------------------------------ util/prgunrel.i 07/01 JLF */
/*  Purge Unshipped Releases for Closed Orders                                */
/* -------------------------------------------------------------------------- */

FOR EACH oe-relh
    WHERE oe-relh.company EQ cocode
      AND oe-relh.posted  EQ YES
      AND oe-relh.printed EQ YES
      AND oe-relh.deleted EQ NO
                                  
      AND NOT CAN-FIND(FIRST oe-bolh WHERE oe-bolh.company  EQ cocode
                                       AND oe-bolh.release# EQ oe-relh.release#)
     USE-INDEX post
     
     TRANSACTION:
  
  FIND FIRST oe-rell
      WHERE oe-rell.company EQ cocode
        AND oe-rell.r-no    EQ oe-relh.r-no

        AND CAN-FIND(FIRST oe-ord WHERE oe-ord.company EQ cocode
                                    AND oe-ord.ord-no  EQ oe-rell.ord-no
                                    AND oe-ord.opened  EQ YES)
      NO-LOCK NO-ERROR.

  IF NOT AVAIL oe-rell THEN DO:
    oe-relh.posted = NO.  /* So trigger will delete lines */
    DELETE oe-relh.
  END.
END.

/* end ---------------------------------- copr. 2001  advanced software, inc. */
