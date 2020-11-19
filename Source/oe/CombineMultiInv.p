DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO.

{oe/ttCombInv.i}

DEFINE BUFFER bf-inv-line FOR inv-line.
DEFINE BUFFER bf-inv-head FOR inv-head.
DEFINE BUFFER bf-inv-misc FOR inv-misc.
DEFINE BUFFER bf-master-inv-head FOR inv-head.

DEFINE VARIABLE dComm AS DECIMAL NO-UNDO.
DEFINE VARIABLE dFuel AS DECIMAL NO-UNDO.
DEFINE VARIABLE dCost AS DECIMAL NO-UNDO.
DEFINE VARIABLE dWeight AS DECIMAL NO-UNDO.
DEFINE VARIABLE dFreight AS DECIMAL NO-UNDO.
       
FIND FIRST bf-master-inv-head NO-LOCK
WHERE ROWID(bf-master-inv-head) EQ ipriRowid NO-ERROR .
FIND FIRST ttCombInv NO-LOCK NO-ERROR.     

IF AVAIL bf-master-inv-head AND AVAIL ttCombInv THEN
DO:    
    ASSIGN 
        dComm = bf-master-inv-head.t-comm
        dFuel = bf-master-inv-head.t-inv-fuel
        dCost = bf-master-inv-head.t-inv-cost
        dWeight = bf-master-inv-head.t-inv-weight
        dFreight = bf-master-inv-head.t-inv-freight
        .
     
    FOR EACH ttCombInv:
      FIND FIRST bf-inv-head EXCLUSIVE-LOCK
           WHERE bf-inv-head.company EQ ttCombInv.company
           AND bf-inv-head.r-no EQ ttCombInv.r-no NO-ERROR .
        IF AVAILABLE bf-inv-head THEN
        DO:            
            FOR EACH bf-inv-line EXCLUSIVE-LOCK
                WHERE bf-inv-line.company EQ bf-inv-head.company
                AND bf-inv-line.r-no EQ ttCombInv.r-no:
                
                ASSIGN
                   bf-inv-line.r-no = bf-master-inv-head.r-no
                    .                        
            END.
            FOR EACH bf-inv-misc EXCLUSIVE-LOCK
                WHERE bf-inv-misc.r-no EQ ttCombInv.r-no:
                ASSIGN
                    bf-inv-misc.r-n = bf-master-inv-head.r-no
                    .
            END.   
            ASSIGN
            dComm        = dComm        + bf-inv-head.t-comm
            dFuel    = dFuel    + bf-inv-head.t-inv-fuel
            dCost    = dCost    + bf-inv-head.t-inv-cost
            dWeight  = dWeight  + bf-inv-head.t-inv-weight
            dFreight = dFreight +  bf-inv-head.t-inv-freight .                         
            
            DELETE bf-inv-head.
        END.     
    END. 
    FIND CURRENT bf-master-inv-head EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN
        bf-master-inv-head.t-comm = dComm
        bf-master-inv-head.t-inv-fuel = dFuel
        bf-master-inv-head.t-inv-cost = dCost
        bf-master-inv-head.t-inv-weight = dWeight
        bf-master-inv-head.t-inv-freight = dFreight
        bf-master-inv-head.spare-int-1 = 1
        .
    FIND CURRENT bf-master-inv-head NO-LOCK NO-ERROR.    
END.     
RELEASE bf-master-inv-head.
RELEASE bf-inv-head.
RELEASE bf-inv-line.
RELEASE bf-inv-misc.
