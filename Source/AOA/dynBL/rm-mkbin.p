/*------------------------------------------------------------------------
  File:         rm-mkbin.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 4.14.2022
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttRMBin
DEFINE TEMP-TABLE ttRMBin NO-UNDO
    FIELD itemNo      LIKE rm-bin.i-no
    FIELD itemName    LIKE item.i-name
    FIELD location    LIKE rm-bin.loc
    FIELD binLocation LIKE rm-bin.loc-bin
    FIELD qty         LIKE rm-bin.qty
    FIELD tag         LIKE rm-bin.tag
    FIELD actionType    AS CHARACTER     LABEL "Type" FORMAT "x(20)"
    FIELD origAvgCost LIKE ITEM.avg-cost LABEL "Orig Avg Cost"
    FIELD AvgCost     LIKE ITEM.avg-cost LABEL "New Avg Cost"
    .

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 201
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE dAvgCost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCost    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQty     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iRNo     AS INTEGER NO-UNDO.

    DEFINE BUFFER bRMBin FOR rm-bin.

    DISABLE TRIGGERS FOR LOAD OF item.
    DISABLE TRIGGERS FOR LOAD OF rm-bin.
    DISABLE TRIGGERS FOR LOAD OF rm-rcpth.
    DISABLE TRIGGERS FOR LOAD OF rm-rdtlh.

    FOR EACH item NO-LOCK 
        WHERE item.company EQ cCompany
          AND item.i-no    GE cStartRMItem
          AND item.i-no    LE cEndRMItem
          AND item.i-no    NE ""
        BY item.i-no
        :
        RUN rm/rm-mkbin.p (RECID(item)).

        IF lDeleteZeroQty THEN
        DO TRANSACTION:
            FOR EACH rm-bin EXCLUSIVE-LOCK
                WHERE rm-bin.company EQ item.company
                  AND rm-bin.i-no    EQ item.i-no
                  AND rm-bin.qty     EQ 0
                :
                CREATE ttRMBin.
                ASSIGN
                    ttRMBin.itemNo      = rm-bin.i-no
                    ttRMBin.itemName    = item.i-name
                    ttRMBin.location    = rm-bin.loc
                    ttRMBin.binLocation = rm-bin.loc-bin
                    ttRMBin.tag         = rm-bin.tag
                    ttRMBin.actionType  = "Delete Zero Bin"
                    .
                DELETE rm-bin.
            END. // each rm-bin
        END. // do trans

        IF lDeleteNegativeQty THEN
        DO TRANSACTION:
            FOR EACH rm-bin EXCLUSIVE-LOCK
                WHERE rm-bin.company EQ item.company
                  AND rm-bin.i-no    EQ item.i-no
                  AND rm-bin.qty     LT 0
                :        
                iRNo = 1.
                RUN sys/ref/asiseq.p (item.company, "rm_rcpt_seq", OUTPUT iRNo) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN RETURN.
    
                CREATE rm-rcpth.
                ASSIGN
                    rm-rcpth.r-no       = iRNo
                    rm-rcpth.trans-date = TODAY
                    rm-rcpth.company    = item.company
                    rm-rcpth.loc        = cLocation
                    rm-rcpth.rita-code  = "C"
                    rm-rcpth.i-no       = item.i-no
                    rm-rcpth.post-date  = TODAY
                    rm-rcpth.i-name     = item.i-name
                    rm-rcpth.pur-uom    = item.pur-uom
                    .
                CREATE rm-rdtlh.
                ASSIGN
                    rm-rdtlh.r-no      = rm-rcpth.r-no
                    rm-rdtlh.company   = rm-rcpth.company
                    rm-rdtlh.loc       = rm-bin.loc
                    rm-rdtlh.rita-code = "C"
                    rm-rdtlh.loc-bin   = rm-bin.loc-bin
                    rm-rdtlh.tag       = rm-bin.tag
                    rm-rdtlh.qty       = 0
                    .
                CREATE ttRMBin.
                ASSIGN
                    ttRMBin.itemNo      = rm-bin.i-no
                    ttRMBin.itemName    = item.i-name
                    ttRMBin.location    = rm-bin.loc
                    ttRMBin.binLocation = rm-bin.loc-bin
                    ttRMBin.qty         = rm-bin.qty
                    ttRMBin.tag         = rm-bin.tag
                    ttRMBin.actionType  = "Delete Negative Bin"
                    .
                DELETE rm-bin.
            END. // each rm-bin
        END. // do trans

        RUN rm/rm-reset.p (RECID(item)).
  
        // reset average cost
        ASSIGN
            dAvgCost = 0
            dCost    = 0
            dQty     = 0
            .
        FOR EACH bRMBin FIELDS(qty cost) NO-LOCK
            WHERE bRMBin.company EQ item.company
              AND bRMBin.i-no    EQ item.i-no
              AND bRMBin.cost    NE ?
            :
            ASSIGN
                dCost = dCost
                      + (bRMBin.cost
                      * (bRMBin.qty
                      * IF bRMBin.qty LT 0 THEN -1 ELSE 1))
                dQty  = dQty
                      + (bRMBin.qty
                      * IF bRMBin.qty LT 0 THEN -1 ELSE 1)
                .
        END.
        IF dQty NE 0 THEN
        dAvgCost = ROUND(dCost / dQty,4).
        IF dAvgCost EQ ? THEN
        dAvgCost = 0.
        IF dAvgCost NE item.avg-cost THEN
        DO TRANSACTION:
            FIND CURRENT item EXCLUSIVE-LOCK.
            CREATE ttRMBin.
            ASSIGN
                ttRMBin.itemNo      = item.i-no
                ttRMBin.itemName    = item.i-name
                ttRMBin.origAvgCost = item.avg-cost
                ttRMBin.AvgCost     = dAvgCost
                ttRMBin.actionType  = "Reset Average Cost"
                item.avg-cost       = dAvgCost
                .
            FIND CURRENT item NO-LOCK.  
        END. // if avg-cost changed
    END. /* each item */

END PROCEDURE.
