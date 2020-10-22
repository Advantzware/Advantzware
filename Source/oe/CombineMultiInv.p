DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO.

{oe/ttCombInv.i}

DEFINE BUFFER bf-inv-line FOR inv-line.
DEFINE BUFFER bf-inv-head FOR inv-head.
DEFINE BUFFER bf-inv-misc FOR inv-misc.
       
FIND FIRST inv-head NO-LOCK
WHERE ROWID(inv-head) EQ ipriRowid NO-ERROR .
FIND FIRST ttCombInv NO-LOCK NO-ERROR.     

IF AVAIL inv-head AND AVAIL ttCombInv THEN
DO:              
    FOR EACH ttCombInv:
      FIND FIRST bf-inv-head EXCLUSIVE-LOCK
           WHERE bf-inv-head.company EQ ttCombInv.company
           AND bf-inv-head.r-no EQ ttCombInv.r-no NO-ERROR .
        IF avail bf-inv-head THEN
        DO:            
            FOR EACH bf-inv-line EXCLUSIVE-LOCK
                WHERE bf-inv-line.company EQ bf-inv-head.company
                AND bf-inv-line.r-no EQ ttCombInv.r-no:
                
                ASSIGN
                   bf-inv-line.r-no = inv-head.r-no
                    .                        
            END.
            FOR EACH bf-inv-misc EXCLUSIVE-LOCK
                WHERE bf-inv-misc.r-no eq ttCombInv.r-no:
                ASSIGN
                    bf-inv-misc.r-n = inv-head.r-no
                    .
            END.       
            DELETE bf-inv-head.
        END.     
    END.             
END.     

