
DEF INPUT PARAM ip-rowid1 AS ROWID NO-UNDO.
DEF INPUT PARAM ip-rowid2 AS ROWID NO-UNDO.
DEF INPUT PARAM ip-which  AS INT NO-UNDO.  /* 1 is inks,
                                              2 is inks & units,
                                              3 is packing
                                              4 is freight
                                              ? is all */

DEF BUFFER b-eb FOR eb.
DEF BUFFER b-eb1 FOR eb.
DEF BUFFER b-ef FOR ef.
DEF BUFFER b-ref FOR reftable.

DEF VAR ll AS LOG NO-UNDO.
DEF VAR lj AS INT NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-list AS CHAR NO-UNDO.
DEF VAR v-side-count AS INT NO-UNDO.

FIND eb WHERE ROWID(eb) EQ ip-rowid1 NO-LOCK NO-ERROR.

IF AVAIL eb THEN DO:
  lv-list = "Ink,"                                          +
            (IF ip-which EQ ? THEN " " ELSE "Ink & ")       +
            (IF eb.est-type LE 4 THEN "Unit" ELSE "") + "," +
            "Packing,"                                      +
            (IF ip-which EQ ? THEN " & " ELSE "")           +
            "Freight".

  IF ip-which NE ? THEN lv-list = ENTRY(ip-which,lv-list).

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

    IF ip-which EQ 2 OR ip-which EQ ? THEN DO:
      {est/copyunit.i}
    END.

    IF ip-which EQ 3 OR ip-which EQ ? THEN DO:
      {est/copypack.i}
    END.

    IF ip-which EQ 4 OR ip-which EQ ? THEN DO:
      {est/copyfrat.i}
    END.
  END.
END.
