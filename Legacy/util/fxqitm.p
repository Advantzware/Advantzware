/*util/fxqitm.p */

FOR EACH quoteitm WHERE quoteitm.i-no = "".
    FIND FIRST itemfg WHERE itemfg.company = quoteitm.company
                        AND itemfg.part-no = quoteitm.part-no NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN quoteitm.i-no = itemfg.i-no.
    DISP quoteitm.i-no quoteitm.part-no.
    PAUSE 0.
END.
MESSAGE "Quote FG Item# update is completed" view-as alert-box information.
HIDE ALL NO-PAUSE.

