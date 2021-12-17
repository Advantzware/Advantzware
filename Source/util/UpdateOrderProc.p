DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginCustomer AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEndCustomer AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginRelDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipcEndRelDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipiBeginOrder AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipiEndOrder AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER iplUpdRelDate AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipdtRelDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER iplRelItem AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcFilterStatus AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcChangeStatus AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipExecute AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipFilePath AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttOrderLineChange
    FIELD customerID  AS CHARACTER
    FIELD shipID      AS CHARACTER
    FIELD orderNo     AS INTEGER
    FIELD orderline   AS INTEGER
    FIELD itemCode    AS CHARACTER
    FIELD orderQty    AS DECIMAL
    FIELD shippedQty  AS DECIMAL
    FIELD releaseQty  AS DECIMAL
    FIELD qtyUOM      AS CHARACTER
    FIELD price       AS DECIMAL     
    FIELD priceUom    AS CHARACTER
    FIELD totalPrice  AS DECIMAL     
    FIELD orderStatus AS CHARACTER
    FIELD orderDate   AS DATE
    FIELD relDateOld  AS DATE 
    FIELD relDateNew  AS DATE
    FIELD note        AS CHARACTER
    .

DEFINE VARIABLE hdOutput     AS HANDLE    NO-UNDO.
DEFINE VARIABLE lError       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.    
DEFINE VARIABLE dtRelDateOld AS DATE      NO-UNDO.  
DEFINE VARIABLE cStatus      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrompt      AS CHARACTER NO-UNDO.

DEFINE NEW SHARED VARIABLE relh-recid   AS RECID     NO-UNDO.
DEFINE NEW SHARED VARIABLE v-auto       AS LOG       INIT YES NO-UNDO.

RUN system\OutputProcs.p PERSISTENT SET hdOutput.

{oe/chkordl.i NEW}
{oe/relemail.i NEW}
{sys/inc/var.i new shared }

DEFINE BUFFER bf-oe-rel  FOR oe-rel.
DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
DEFINE BUFFER bf-oe-ord  FOR oe-ord.

ASSIGN
    cocode = ipcCompany.

FOR EACH oe-ordl NO-LOCK
    WHERE oe-ordl.company EQ ipcCompany    
    AND oe-ordl.cust-no GE ipcBeginCustomer
    AND oe-ordl.cust-no LE ipcEndCustomer
    AND oe-ordl.ord-no GE ipiBeginOrder
    AND oe-ordl.ord-no LE ipiEndOrder
    ,
    FIRST oe-ord NO-LOCK
    WHERE oe-ord.company EQ oe-ordl.company
    AND (oe-ord.stat EQ ipcFilterStatus OR ipcFilterStatus EQ "")
    AND oe-ord.ord-no EQ oe-ordl.ord-no      
    BREAK BY oe-ordl.ord-no:
    
    MAIN-LOOP:
    FOR EACH bf-oe-rel EXCLUSIVE-LOCK 
        WHERE bf-oe-rel.company EQ oe-ordl.company
        AND bf-oe-rel.ord-no EQ oe-ordl.ord-no
        AND bf-oe-rel.i-no EQ oe-ordl.i-no
        AND bf-oe-rel.LINE EQ oe-ordl.LINE:         
     
        RELEASE oe-rell.
            
        FIND FIRST oe-rell NO-LOCK
            WHERE oe-rell.company  EQ bf-oe-rel.company
            AND oe-rell.ord-no   EQ bf-oe-rel.ord-no
            AND oe-rell.i-no     EQ bf-oe-rel.i-no
            AND oe-rell.line     EQ bf-oe-rel.line
            AND oe-rell.rel-no   EQ bf-oe-rel.rel-no
            AND oe-rell.b-ord-no EQ bf-oe-rel.b-ord-no
            AND oe-rell.po-no    EQ bf-oe-rel.po-no
            USE-INDEX ord-no NO-ERROR.          
     
        IF AVAILABLE oe-rell THEN
        DO:
            FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
            IF AVAILABLE oe-relh AND oe-relh.rel-date GE ipcBeginRelDate AND oe-relh.rel-date LE ipcEndRelDate THEN
                dtRelDateOld = oe-relh.rel-date.
            ELSE NEXT MAIN-LOOP.
        END.
        ELSE 
        DO:
            IF bf-oe-rel.rel-date GE ipcBeginRelDate AND bf-oe-rel.rel-date LE ipcEndRelDate THEN
                dtRelDateOld = bf-oe-rel.rel-date.
            ELSE NEXT MAIN-LOOP.
        END.
        
        
        CREATE ttOrderLineChange.
        ASSIGN 
            ttOrderLineChange.customerID  = oe-ordl.cust-no
            ttOrderLineChange.shipID      = oe-ordl.ship-id
            ttOrderLineChange.orderNo     = oe-ordl.ord-no
            ttOrderLineChange.orderLine   = oe-ordl.line
            ttOrderLineChange.itemCode    = oe-ordl.i-no
            ttOrderLineChange.orderQty    = oe-ordl.qty
            ttOrderLineChange.shippedQty  = oe-ordl.ship-qty
            ttOrderLineChange.releaseQty  = bf-oe-rel.qty
            ttOrderLineChange.qtyUOM      = "EA"
            ttOrderLineChange.price       = oe-ordl.price            
            ttOrderLineChange.priceUOM    = oe-ordl.pr-uom
            ttOrderLineChange.totalPrice  = oe-ordl.t-price            
            ttOrderLineChange.orderStatus = oe-ord.stat
            ttOrderLineChange.orderDate   = oe-ord.ord-date   
            ttOrderLineChange.relDateOld  = dtRelDateOld
            ttOrderLineChange.relDateNew  = IF iplUpdRelDate AND ipdtRelDate NE ? THEN ipdtRelDate ELSE dtRelDateOld
            .
        RUN oe/getStatusDesc.p (INPUT oe-ord.stat, OUTPUT ttOrderLineChange.orderStatus). 
        IF ipExecute AND iplUpdRelDate AND ipdtRelDate NE ? THEN
        DO:
            bf-oe-rel.rel-date =  ipdtRelDate.
            ttOrderLineChange.note = IF iplUpdRelDate AND ipdtRelDate NE ? THEN "Update Release date" ELSE "" .
            FIND CURRENT oe-relh EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE oe-relh THEN oe-relh.rel-date = ipdtRelDate.
            FIND CURRENT oe-relh NO-LOCK NO-ERROR.            
        END.
       
        IF ipExecute AND ipcChangeStatus NE "" THEN
        DO:
            IF LAST-OF(oe-ordl.ord-no) THEN
            DO:
                FIND FIRST bf-oe-ord EXCLUSIVE-LOCK
                    WHERE ROWID(bf-oe-ord) EQ rowid(oe-ord) NO-ERROR.
                bf-oe-ord.stat = ipcChangeStatus.
                FIND FIRST bf-oe-ord NO-LOCK NO-ERROR.
            END.
            FIND FIRST bf-oe-ordl EXCLUSIVE-LOCK
                WHERE ROWID(bf-oe-ordl) EQ rowid(oe-ordl) NO-ERROR.
            bf-oe-ordl.stat = ipcChangeStatus.
            FIND FIRST bf-oe-ordl NO-LOCK NO-ERROR. 
        END.
       
        IF ipExecute  AND  iplRelItem AND NOT AVAILABLE oe-rell THEN
        DO:          
            RUN oe/rel-stat.p (ROWID(bf-oe-rel), OUTPUT cStatus).  
            IF INDEX("ABCPZ", cStatus) > 0 THEN.
            ELSE 
            DO:
                RUN oe/actrel.p (RECID(bf-oe-rel), INPUT-OUTPUT cPrompt).
                ttOrderLineChange.note = IF ttOrderLineChange.note NE "" THEN ttOrderLineChange.note +  ", Create Release  " ELSE ttOrderLineChange.note +  " Create Release  " .
            END.         
        END.
       
    END. /*bf-oe-rel*/
   
END.
RUN Output_TempTableToCSV IN hdOutput (TEMP-TABLE ttOrderLineChange:HANDLE, ipFilePath ,YES,YES, OUTPUT lError, OUTPUT cMessage).
