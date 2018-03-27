
SESSION:SET-WAIT-STATE ("general").

FOR EACH ar-invl WHERE actnum EQ "" AND i-no NE "",
    FIRST itemfg
    WHERE itemfg.company EQ ar-invl.company
      AND itemfg.i-no    EQ ar-invl.i-no
    NO-LOCK,
    FIRST ar-ctrl WHERE ar-ctrl.company EQ ar-invl.company NO-LOCK:

  FIND FIRST fgcat
      WHERE fgcat.company eq itemfg.company
        AND fgcat.procat  eq itemfg.procat
      NO-LOCK NO-ERROR.

  ar-invl.actnum = IF AVAIL fgcat AND fgcat.glacc NE ""
                   THEN fgcat.glacc ELSE ar-ctrl.sales.
END.

SESSION:SET-WAIT-STATE ("").
