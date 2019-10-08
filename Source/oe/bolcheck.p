
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{oe/bolcheck.i}


FIND oe-bolh WHERE ROWID(oe-bolh) EQ ip-rowid NO-LOCK NO-ERROR.
                                  
IF AVAIL oe-bolh THEN
FOR EACH oe-boll NO-LOCK
    WHERE oe-boll.company EQ oe-bolh.company
      AND oe-boll.b-no    EQ oe-bolh.b-no,

    FIRST oe-ord NO-LOCK
    WHERE oe-ord.company EQ oe-bolh.company
      AND oe-ord.ord-no  EQ oe-boll.ord-no

    BREAK BY oe-boll.i-no
          BY oe-boll.job-no
          BY oe-boll.job-no2
          BY oe-boll.loc
          BY oe-boll.loc-bin
          BY oe-boll.tag:
             
  FIND FIRST tt-fg-bin
      WHERE tt-fg-bin.company EQ oe-boll.company
        AND tt-fg-bin.i-no    EQ oe-boll.i-no
        AND tt-fg-bin.job-no  EQ oe-boll.job-no
        AND tt-fg-bin.job-no2 EQ oe-boll.job-no2
        AND tt-fg-bin.loc     EQ oe-boll.loc
        AND tt-fg-bin.loc-bin EQ oe-boll.loc-bin
        AND tt-fg-bin.tag     EQ oe-boll.tag
      NO-ERROR.
  IF NOT AVAIL tt-fg-bin THEN DO:
    CREATE tt-fg-bin.
    ASSIGN
     tt-fg-bin.company = oe-boll.company
     tt-fg-bin.i-no    = oe-boll.i-no
     tt-fg-bin.job-no  = oe-boll.job-no
     tt-fg-bin.job-no2 = oe-boll.job-no2
     tt-fg-bin.loc     = oe-boll.loc
     tt-fg-bin.loc-bin = oe-boll.loc-bin
     tt-fg-bin.tag     = oe-boll.tag.
  END.
  tt-fg-bin.qty = tt-fg-bin.qty + oe-boll.qty.
              
  IF LAST-OF(oe-boll.tag) THEN DO:
    FOR EACH fg-bin NO-LOCK
        WHERE fg-bin.company EQ oe-boll.company
          AND fg-bin.i-no    EQ oe-boll.i-no
          AND fg-bin.job-no  EQ oe-boll.job-no
          AND fg-bin.job-no2 EQ oe-boll.job-no2
          AND fg-bin.loc     EQ oe-boll.loc
          AND fg-bin.loc-bin EQ oe-boll.loc-bin
          AND fg-bin.tag     EQ oe-boll.tag
          AND fg-bin.cust-no EQ oe-boll.cust-no:
      ACCUMULATE fg-bin.qty (TOTAL).
    END.
    IF tt-fg-bin.qty GT 0                        AND
       (ACCUM TOTAL fg-bin.qty) LT tt-fg-bin.qty THEN DO:
      CREATE w-except.
      BUFFER-COPY oe-boll TO w-except
      ASSIGN
       w-except.bol-no = oe-bolh.bol-no
       w-except.dOnhQty = (ACCUM TOTAL fg-bin.qty) .
    END.
  END.
END.
