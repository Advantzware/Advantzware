/* loadtag.i */

DEFINE VARIABLE recUsed AS LOGICAL NO-UNDO.

recUsed = (loadtag.item-type EQ YES AND /* rm */
           loadtag.tag-no <> "" AND
          (CAN-FIND(FIRST rm-bin WHERE rm-bin.company EQ loadtag.company
                                   AND rm-bin.tag EQ loadtag.tag-no) OR
           CAN-FIND(FIRST rm-rctd WHERE rm-rctd.company EQ loadtag.company
                                    AND rm-rctd.tag EQ loadtag.tag-no))) OR
          (loadtag.item-type EQ NO AND /* fg */
           loadtag.tag-no <> "" AND
          (CAN-FIND(FIRST fg-bin WHERE fg-bin.company EQ loadtag.company
                                   AND fg-bin.tag EQ loadtag.tag-no) OR
           CAN-FIND(FIRST fg-rctd WHERE fg-rctd.company EQ loadtag.company
                                    AND fg-rctd.tag EQ loadtag.tag-no))).
IF recUsed THEN
DO:
  MESSAGE 'Loadtag Exists in Bin/Receipts Tables, Delete Not Allowed' VIEW-AS ALERT-BOX.
  RETURN 'ADM-ERROR':U.
END.
