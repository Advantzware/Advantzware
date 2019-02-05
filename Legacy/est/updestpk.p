
DEFINE INPUT PARAMETER ip-rowid1 AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ip-rowid2 AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ip-which  AS INTEGER NO-UNDO.  /* 1 is inks,
                                              2 is inks & units,
                                              3 is packing
                                              4 is freight
                                              ? is all */
DEFINE INPUT PARAMETER ip-format  AS CHARACTER NO-UNDO.

DEFINE BUFFER b-eb  FOR eb.
DEFINE BUFFER b-eb1 FOR eb.
DEFINE BUFFER b-ef  FOR ef.
DEFINE BUFFER b-ref FOR reftable.

DEFINE VARIABLE ll           AS LOG       NO-UNDO.
DEFINE VARIABLE lj           AS INTEGER   NO-UNDO.
DEFINE VARIABLE li           AS INTEGER   NO-UNDO.
DEFINE VARIABLE lv-list      AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-side-count AS INTEGER   NO-UNDO.

FIND eb WHERE ROWID(eb) EQ ip-rowid1 NO-LOCK NO-ERROR.

IF AVAILABLE eb THEN 
DO:
    lv-list = "Ink,"                                          +
        (IF ip-which EQ ? THEN " " ELSE "Ink & ")       +
        (IF eb.est-type LE 4 THEN "Unit" ELSE "") + "," +
        "Packing,"                                      +
        (IF ip-which EQ ? THEN " & " ELSE "")           +
        "Freight".

    IF ip-which NE ? THEN lv-list = ENTRY(ip-which,lv-list).
    IF ip-format EQ "FGItem#" THEN 
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
    END. /* ip-format = fgitem*/
    ELSE IF ip-format EQ "CAD#" THEN 
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
        ELSE IF ip-format EQ "CustomerPart#" THEN 
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
            ELSE IF ip-format EQ "CAD#andFGItem#" THEN 
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
            AND b-eb.stock-no EQ eb.stock-no
            AND ROWID(b-eb)   NE ip-rowid1
            AND ROWID(b-eb)   NE ip-rowid2:
  
            IF ip-which LE 2 OR ip-which EQ ? THEN 
            DO:
                {est/copyinks.i}
            END.

            IF ip-which EQ 3 OR ip-which EQ ? THEN 
            DO:
                {est/copypack.i}
            END.

            IF ip-which EQ 4 OR ip-which EQ ? THEN 
            DO:
                {est/copyfrat.i}
            END.
        END.
END.
