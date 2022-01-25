DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiBeginPo AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipiEndPo AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipcEndDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipcQtyUsed AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipExecute AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipFilePath AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttPoLineChange
    FIELD poNO            AS INTEGER  LABEL "PO#"
    FIELD vendor          AS CHARACTER LABEL "Vendor"
    FIELD poDate          AS DATE  LABEL "Po Date"
    FIELD shipID          AS CHARACTER LABEL "Ship To"
    FIELD shipName        AS CHARACTER LABEL "Ship Name"
    FIELD job             AS CHARACTER LABEL "Job"
    FIELD job2            AS INTEGER LABEL "Job2"
    FIELD iform           AS INTEGER LABEL  "Form"
    FIELD itemCode        AS CHARACTER LABEL "Item"
    FIELD itemType        AS CHARACTER LABEL "Type"
    FIELD poStatus        AS CHARACTER LABEL "PO# Status"
    FIELD poStatusNew     AS CHARACTER LABEL "PO# Status New"
    FIELD poLineStatus    AS CHARACTER LABEL "PO# Line Status"    
    FIELD poLineStatusNew AS CHARACTER LABEL "PO# Line Status New"
    FIELD note            AS CHARACTER
    .

DEFINE VARIABLE hdOutput     AS HANDLE    NO-UNDO.
DEFINE VARIABLE lError       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.    
DEFINE VARIABLE cStatusDesc  AS CHARACTER NO-UNDO.

RUN system\OutputProcs.p PERSISTENT SET hdOutput.  

DEFINE BUFFER bf-po-ord  FOR po-ord.
DEFINE BUFFER bf-po-ordl FOR po-ordl. 
    
MAIN-LOOP:
FOR EACH bf-po-ord NO-LOCK
    WHERE bf-po-ord.company EQ ipcCompany
    AND bf-po-ord.po-no   GE ipiBeginPo
    AND bf-po-ord.po-no   LE ipiEndPo
    AND bf-po-ord.po-date GE ipcBeginDate
    AND bf-po-ord.po-date LE ipcEndDate
    AND bf-po-ord.stat    NE "C" 
    :         
    FOR EACH bf-po-ordl NO-LOCK 
        WHERE bf-po-ordl.company EQ bf-po-ord.company 
        AND bf-po-ordl.po-no   EQ bf-po-ord.po-no:  
        
        IF ipcQtyUsed EQ "F" THEN
        DO:
           IF bf-po-ordl.t-rec-qty LT bf-po-ordl.ord-qty THEN NEXT MAIN-LOOP.
              
        END.             
        ELSE IF ipcQtyUsed EQ "R" THEN
        DO:
           IF bf-po-ordl.t-rec-qty LT (bf-po-ordl.ord-qty * (1 - bf-po-ordl.under-pct / 100)) THEN NEXT MAIN-LOOP.
              
        END.
        ELSE IF ipcQtyUsed EQ "I" THEN
        DO:
            IF bf-po-ordl.t-rec-qty LT (bf-po-ordl.ord-qty * (1 - bf-po-ordl.under-pct / 100))
            OR bf-po-ordl.t-inv-qty LT bf-po-ordl.t-rec-qty THEN NEXT MAIN-LOOP.          
        END.
                  
                  
        CREATE ttPoLineChange.
        ASSIGN 
            ttPoLineChange.poNO     = bf-po-ordl.po-no
            ttPoLineChange.vendor   = bf-po-ord.vend-no
            ttPoLineChange.poDate   = bf-po-ord.po-date
            ttPoLineChange.shipID   = bf-po-ord.ship-id
            ttPoLineChange.shipName = bf-po-ord.ship-name
            ttPoLineChange.job      = bf-po-ordl.job-no
            ttPoLineChange.job2     = bf-po-ordl.job-no2
            ttPoLineChange.iForm    = bf-po-ordl.s-num
            ttPoLineChange.itemCode = bf-po-ordl.i-no 
            ttPoLineChange.itemType = IF bf-po-ordl.item-type THEN "RM" ELSE "FG".
            
        RUN oe/getStatusDesc.p( INPUT bf-po-ord.stat, OUTPUT cStatusDesc) .
            
        ttPoLineChange.poStatus        = cStatusDesc.
            
        RUN oe/getStatusDesc.p( INPUT bf-po-ordl.stat, OUTPUT cStatusDesc) .
            
        ttPoLineChange.poLineStatus    = cStatusDesc.
            
        RUN oe/getStatusDesc.p( INPUT "C", OUTPUT cStatusDesc) .
        ASSIGN
            ttPoLineChange.poStatusNew     = cStatusDesc
            ttPoLineChange.poLineStatusNew = cStatusDesc.            
                              
        IF ipExecute THEN
        DO: 
           DO TRANSACTION:
                FIND CURRENT bf-po-ordl EXCLUSIVE-LOCK NO-ERROR.
                bf-po-ordl.stat = "C".
                ttPoLineChange.note = "PO# status changed to closed" .
                FIND CURRENT bf-po-ordl NO-LOCK NO-ERROR.
           END. 
        END.        
       
    END. /*bf-oe-rel*/
    
    IF ipExecute THEN
    DO:
        DO TRANSACTION:
          FIND CURRENT bf-po-ord EXCLUSIVE-LOCK NO-ERROR.
          bf-po-ord.stat = "C".  
          FIND CURRENT bf-po-ord NO-LOCK NO-ERROR.
        END.   
    END.
   
END.
        
RUN Output_TempTableToCSV IN hdOutput (TEMP-TABLE ttPoLineChange:HANDLE, ipFilePath ,YES,YES, OUTPUT lError, OUTPUT cMessage).

IF VALID-HANDLE(hdOutput) THEN
    DELETE OBJECT hdOutput.
