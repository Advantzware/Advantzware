/* ltfixbin.p */

MESSAGE 'Run Live Update?' VIEW-AS ALERT-BOX
  QUESTION BUTTONS YES-NO UPDATE liveUpdate AS LOGICAL.
OUTPUT TO 'util/ltfixbin.log'.
FOR EACH loadtag WHERE loadtag.item-type EQ NO
                   AND loadtag.is-case-tag EQ NO:
  IF NOT CAN-FIND(FIRST fg-bin NO-LOCK
      WHERE fg-bin.company EQ loadtag.company
        AND fg-bin.tag EQ loadtag.tag-no
        AND fg-bin.i-no EQ loadtag.i-no
        AND fg-bin.loc EQ loadtag.loc
        AND fg-bin.loc-bin EQ loadtag.loc-bin) THEN
  FOR EACH fg-bin NO-LOCK
      WHERE fg-bin.company EQ loadtag.company
        AND fg-bin.tag EQ loadtag.tag-no
        AND fg-bin.i-no EQ loadtag.i-no
      BREAK BY fg-bin.qty DESCENDING:
    IF fg-bin.loc NE loadtag.loc OR
       fg-bin.loc-bin NE loadtag.loc-bin THEN DO:
      EXPORT loadtag.tag-no loadtag.loc loadtag.loc-bin
        fg-bin.loc fg-bin.loc-bin fg-bin.qty.
      IF liveUpdate THEN
      ASSIGN
        loadtag.loc = fg-bin.loc
        loadtag.loc-bin = fg-bin.loc-bin.
    END.
    LEAVE.
  END. /* each fg-bin */
END. /* each loadtag */
OUTPUT CLOSE.
MESSAGE 'Loadtag Bin Fix Complete!' VIEW-AS ALERT-BOX.
