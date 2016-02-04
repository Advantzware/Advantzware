/* t-oebolh.p  Load oe-relh record and assign release# */
DISABLE TRIGGERS FOR LOAD OF oe-relh.

DEF VAR li-next-release AS INT NO-UNDO.
/*
FOR EACH oe-relh:
    DELETE oe-relh.
END.
*/

INPUT FROM d:\sonoco\dump\oe-relh.d NO-ECHO.

FIND LAST oe-relh USE-INDEX release# NO-LOCK NO-ERROR.
IF AVAIL oe-relh THEN li-next-release = oe-relh.release# + 1.
ELSE li-next-release = 1.

REPEAT :
    INSERT oe-relh.
    oe-relh.RELEASE# = li-next-release.
    li-next-release = li-next-release + 1.

    DISP oe-relh.release# oe-relh.r-no oe-relh.rel-no WITH FRAME rrr DOWN.
    PAUSE 0.
END.

message li-next-release view-as alert-box.
