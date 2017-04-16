/* util/updefcal.p  update ef.cal from item */

FOR EACH ef.
    FIND FIRST ITEM WHERE ITEM.company = ef.company AND
                          ITEM.i-no = ef.board NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN ef.cal = ITEM.cal.

END.
