
DEFINE INPUT PARAMETER ip-rowid1 AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ip-rowid2 AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ip-which  AS INTEGER NO-UNDO.  /* 1 is inks,
                                              2 is inks & units,
                                              3 is packing
                                              4 is freight
                                              ? is all */

DEFINE BUFFER b-eb  FOR eb.
DEFINE BUFFER b-eb1 FOR eb.
DEFINE BUFFER b-ef  FOR ef.
DEFINE BUFFER b-ref FOR reftable.

DEFINE VARIABLE ll            AS LOG       NO-UNDO.
DEFINE VARIABLE lj            AS INTEGER   NO-UNDO.
DEFINE VARIABLE li            AS INTEGER   NO-UNDO.
DEFINE VARIABLE lv-list       AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-side-count  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lEFPackUpdate AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE cEFPackUpdate AS CHARACTER NO-UNDO.


FIND eb WHERE ROWID(eb) EQ ip-rowid1 NO-LOCK NO-ERROR.


IF AVAILABLE eb THEN 
DO:

    RUN sys/ref/nk1look.p (INPUT eb.company, "CEPackUpdate", "L" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cRtnChar, OUTPUT lRecFound).
    lEFPackUpdate = LOGICAL(cRtnChar) NO-ERROR.

    RUN sys/ref/nk1look.p (INPUT eb.company, "CEPackUpdate", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cRtnChar, OUTPUT lRecFound).
    cEFPackUpdate = cRtnChar .


    lv-list = "Ink,"                                          +
        (IF ip-which EQ ? THEN " " ELSE "Ink & ")       +
        (IF eb.est-type LE 4 THEN "Unit" ELSE "") + "," +
        "Packing,"                                      +
        (IF ip-which EQ ? THEN " & " ELSE "")           +
        "Freight".

    IF ip-which NE ? THEN lv-list = ENTRY(ip-which,lv-list).

    IF lEFPackUpdate AND ip-which EQ 3 THEN 
    DO:

        IF cEFPackUpdate EQ "FGItem#" THEN 
        DO:

            IF eb.stock-no NE ""                          AND
                CAN-FIND(FIRST b-eb
                WHERE b-eb.company  EQ eb.company
                AND b-eb.stock-no EQ eb.stock-no
                AND ROWID(b-eb)   NE ip-rowid1
                AND ROWID(b-eb)   NE ip-rowid2) THEN
                MESSAGE "Update all other estimates with FG# " +
                    TRIM(eb.stock-no) +
                    " with these " + TRIM(lv-list) + " values?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE ll.
        END.

        ELSE IF cEFPackUpdate EQ "CAD#" THEN 
            DO:
                IF eb.cad-no NE ""                          AND
                    CAN-FIND(FIRST b-eb
                    WHERE b-eb.company  EQ eb.company
                    AND b-eb.cad-no EQ eb.cad-no
                    AND ROWID(b-eb)   NE ip-rowid1
                    AND ROWID(b-eb)   NE ip-rowid2) THEN
                    MESSAGE "Update all other estimates with Cad# " +
                        TRIM(eb.cad-no) +
                        " with these " + TRIM(lv-list) + " values?"
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                        UPDATE ll.

            END. /* ip-format EQ "CAD#" */
            ELSE IF cEFPackUpdate EQ "CustomerPart#" THEN 
                DO:
                    IF eb.part-no NE ""                          AND
                        CAN-FIND(FIRST b-eb
                        WHERE b-eb.company  EQ eb.company
                        AND b-eb.part-no EQ eb.part-no
                        AND ROWID(b-eb)   NE ip-rowid1
                        AND ROWID(b-eb)   NE ip-rowid2) THEN
                        MESSAGE "Update all other estimates with Cust Part# " +
                            TRIM(eb.part-no) +
                            " with these " + TRIM(lv-list) + " values?"
                            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                            UPDATE ll.
                END. /* ip-format EQ "customer part" */
                ELSE IF cEFPackUpdate EQ "CAD#andFGItem#" THEN 
                    DO:
                        IF eb.stock-no NE ""   AND eb.cad-no NE ""     AND
                            CAN-FIND(FIRST b-eb
                            WHERE b-eb.company  EQ eb.company
                            AND b-eb.stock-no EQ eb.stock-no
                            AND b-eb.cad-no EQ eb.cad-no
                            AND ROWID(b-eb)   NE ip-rowid1
                            AND ROWID(b-eb)   NE ip-rowid2) THEN
                            MESSAGE "Update all other estimates with FG# " +
                                TRIM(eb.part-no) + " and Cad# " + trim(eb.cad-no)
                                " with these " + TRIM(lv-list) + " values?"
                                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                                UPDATE ll.

                    END. /* ip-format EQ "CAD#andFGItem#" */

        IF ll THEN
            FOR EACH b-eb
                WHERE b-eb.company  EQ eb.company
                AND ( (b-eb.stock-no EQ eb.stock-no AND cEFPackUpdate EQ "FGItem#") 
                OR (b-eb.cad-no EQ eb.cad-no AND cEFPackUpdate EQ "CAD#")
                OR (b-eb.part-no EQ eb.part-no AND cEFPackUpdate EQ "CustomerPart#")
                OR (b-eb.part-no EQ eb.part-no AND b-eb.cad-no EQ eb.cad-no AND cEFPackUpdate EQ "CAD#andFGItem#") )
                AND ROWID(b-eb)   NE ip-rowid1
                AND ROWID(b-eb)   NE ip-rowid2:
                
                IF ip-which EQ 3 OR ip-which EQ ? THEN 
                DO:
                    {est/copypack.i}
                END.
            END.
    END.  /* lEFPackUpdate */
    ELSE do:

        IF eb.stock-no NE ""                          AND
            CAN-FIND(FIRST b-eb
                     WHERE b-eb.company  EQ eb.company
                     AND b-eb.stock-no EQ eb.stock-no
                     AND ROWID(b-eb)   NE ip-rowid1
                     AND ROWID(b-eb)   NE ip-rowid2) THEN
            MESSAGE "Update all other estimates with FG# " +
            TRIM(eb.stock-no) +
            " with these " + TRIM(lv-list) + " values?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll.

      IF ll THEN
          FOR EACH b-eb
          WHERE b-eb.company  EQ eb.company
          AND b-eb.stock-no EQ eb.stock-no
          AND ROWID(b-eb)   NE ip-rowid1
          AND ROWID(b-eb)   NE ip-rowid2:

          IF ip-which LE 2 OR ip-which EQ ? THEN DO:
              {est/copyinks.i}
          END.

          IF ip-which EQ 4 OR ip-which EQ ? THEN DO:
            {est/copyfrat.i}
          END.
      END.
    END. 
END.  /* if avail eb */
