
SESSION:SET-WAIT-STATE ("general").

FOR EACH oe-ord EXCLUSIVE-LOCK
    :
    ASSIGN
        oe-ord.opened    = LOOKUP(oe-ord.stat,"C,D,Z") EQ 0
        oe-ord.closeDate = IF oe-ord.opened THEN ?  ELSE TODAY
        oe-ord.closeTime = IF oe-ord.opened THEN "" ELSE STRING(TIME,"hh:mm:ss")        
        .
    FOR EACH oe-ordl EXCLUSIVE-LOCK
        WHERE oe-ordl.company EQ oe-ord.company
          AND oe-ordl.ord-no  EQ oe-ord.ord-no
        :    
        oe-ordl.opened = oe-ord.opened.
    END.
END.

SESSION:SET-WAIT-STATE ("").
