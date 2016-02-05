
DEF VAR li-num AS INT NO-UNDO.

li-num = 10000.
FOR EACH oe-bolh.
    oe-bolh.release# = li-num.
    li-num = li-num + 1.
END.

FOR EACH oe-bolh.
    FIND FIRST oe-relh WHERE oe-relh.r-no = oe-bolh.r-no NO-LOCK NO-ERROR.
    IF avai oe-relh THEN oe-bolh.release# = oe-relh.release#.
END.
