
DEF INPUT  PARAM ip-rowid     AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-exception AS LOG   NO-UNDO.

{oe/oe-relp1.i}

DEF BUFFER upd-oe-relh FOR oe-relh.
DEF BUFFER upd-oe-rell FOR oe-rell.

{sys/inc/var.i NEW SHARED}


FIND oe-relh WHERE ROWID(oe-relh) EQ ip-rowid NO-LOCK NO-ERROR.
                                  
IF AVAIL oe-relh THEN DO:
  cocode = oe-relh.company.

  {sys/ref/relpost.i}

  FIND upd-oe-relh WHERE ROWID(upd-oe-relh) EQ ROWID(oe-relh)
      EXCLUSIVE NO-WAIT NO-ERROR.

  FOR EACH oe-rell NO-LOCK
      WHERE oe-rell.company EQ oe-relh.company
        AND oe-rell.r-no    EQ oe-relh.r-no
      USE-INDEX r-no:
    FIND upd-oe-rell WHERE ROWID(upd-oe-rell) EQ ROWID(oe-rell) EXCLUSIVE NO-WAIT NO-ERROR.
    IF NOT AVAIL upd-oe-relh OR NOT AVAIL upd-oe-rell THEN DO:
      CREATE tt-except.
      BUFFER-COPY oe-rell TO tt-except.
      tt-except.reason = 1.
      LEAVE.
    END.
    FIND upd-oe-rell WHERE ROWID(upd-oe-rell) EQ ROWID(oe-rell) NO-LOCK NO-ERROR.
  END.
  FIND upd-oe-relh WHERE ROWID(upd-oe-relh) EQ ROWID(oe-relh) NO-LOCK NO-ERROR.

  IF relpost-int EQ 1                                      AND
     NOT CAN-FIND(FIRST tt-except
                  WHERE tt-except.company EQ oe-relh.company
                    AND tt-except.r-no    EQ oe-relh.r-no) THEN DO:
       FOR EACH tt-fg-bin:
            tt-fg-bin.qty = 0.
       END.

       FOR EACH oe-rell
          WHERE oe-rell.company EQ oe-relh.company
            AND oe-rell.r-no    EQ oe-relh.r-no
          USE-INDEX r-no NO-LOCK,
    
          FIRST oe-ord
          WHERE oe-ord.company EQ oe-rell.company
            AND oe-ord.ord-no  EQ oe-rell.ord-no
            AND oe-ord.type    NE "T"
          NO-LOCK
    
          BREAK BY oe-rell.i-no
                BY oe-rell.job-no
                BY oe-rell.job-no2
                BY oe-rell.loc
                BY oe-rell.loc-bin
                BY oe-rell.tag:
                 
        FIND FIRST tt-fg-bin
            WHERE tt-fg-bin.company EQ oe-rell.company
              AND tt-fg-bin.i-no    EQ oe-rell.i-no
              AND tt-fg-bin.job-no  EQ oe-rell.job-no
              AND tt-fg-bin.job-no2 EQ oe-rell.job-no2
              AND tt-fg-bin.loc     EQ oe-rell.loc
              AND tt-fg-bin.loc-bin EQ oe-rell.loc-bin
              AND tt-fg-bin.tag     EQ oe-rell.tag
            NO-ERROR.
        IF NOT AVAIL tt-fg-bin THEN DO:
          CREATE tt-fg-bin.
          ASSIGN
           tt-fg-bin.company = oe-rell.company
           tt-fg-bin.i-no    = oe-rell.i-no
           tt-fg-bin.job-no  = oe-rell.job-no
           tt-fg-bin.job-no2 = oe-rell.job-no2
           tt-fg-bin.loc     = oe-rell.loc
           tt-fg-bin.loc-bin = oe-rell.loc-bin
           tt-fg-bin.tag     = oe-rell.tag.
    
          FOR EACH oe-bolh NO-LOCK
              WHERE oe-bolh.company EQ tt-fg-bin.company
                AND oe-bolh.posted  EQ NO
                AND oe-bolh.deleted EQ NO
              USE-INDEX post,
              EACH oe-boll NO-LOCK
              WHERE oe-boll.b-no    EQ oe-bolh.b-no
                AND oe-boll.i-no    EQ tt-fg-bin.i-no
                AND oe-boll.job-no  EQ tt-fg-bin.job-no
                AND oe-boll.job-no2 EQ tt-fg-bin.job-no2
                AND oe-boll.loc     EQ tt-fg-bin.loc
                AND oe-boll.loc-bin EQ tt-fg-bin.loc-bin
                AND oe-boll.tag     EQ tt-fg-bin.tag
              USE-INDEX b-no2:
            tt-fg-bin.qty = tt-fg-bin.qty + oe-boll.qty.
          END.
        END.
        tt-fg-bin.qty = tt-fg-bin.qty + oe-rell.qty.
                  
        IF LAST-OF(oe-rell.tag) THEN DO:
          FIND FIRST fg-bin
              WHERE fg-bin.company EQ oe-rell.company
                AND fg-bin.i-no    EQ oe-rell.i-no
                AND fg-bin.job-no  EQ oe-rell.job-no
                AND fg-bin.job-no2 EQ oe-rell.job-no2
                AND fg-bin.loc     EQ oe-rell.loc
                AND fg-bin.loc-bin EQ oe-rell.loc-bin
                AND fg-bin.tag     EQ oe-rell.tag
              NO-LOCK NO-ERROR.
         
          IF (NOT AVAIL fg-bin OR fg-bin.qty LT tt-fg-bin.qty) AND
              oe-rell.tag EQ "" THEN DO:
            CREATE tt-except.
            BUFFER-COPY oe-rell TO tt-except.
            tt-except.reason = 2.
          END.
        END.
      END.
  END.
  op-exception = CAN-FIND(FIRST tt-except
                          WHERE tt-except.company EQ oe-relh.company
                            AND tt-except.r-no    EQ oe-relh.r-no).
END.
