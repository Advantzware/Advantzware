
/*------------------------------------------------------------------------
    File        : PriceProcsLineBuilder.i
    Purpose     : Reduce code duplication for key temp-table build function within PricePrice.p

    Syntax      :

    Description : Include for oe-ordl, inv-line, ar-invl interchangeability

    Author(s)   : BV
    Created     : Wed May 02 23:47:32 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE BUFFER bf-{&LineTable} FOR {&LineTable}.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ASSIGN 
    opcType    = "{&LineTable}"
    .
FIND FIRST {&HeaderTable} OF {&LineTable} NO-LOCK NO-ERROR.
FIND FIRST cust NO-LOCK 
    WHERE cust.company EQ {&HeaderTable}.company
    AND cust.cust-no EQ {&HeaderTable}.cust-no
    NO-ERROR.
FIND FIRST itemfg NO-LOCK 
    WHERE itemfg.company EQ {&HeaderTable}.company
    AND itemfg.i-no EQ ipcFGItemID
    NO-ERROR.
IF AVAILABLE {&HeaderTable} 
    AND AVAILABLE cust 
    AND AVAILABLE itemfg 
    THEN 
DO:  
    ASSIGN 
        oplReprice = cust.auto-reprice.
    IF itemfg.i-code EQ "S" THEN DO:      
        ipcFGItemID = IF ipcFGItemID EQ "" THEN {&LineTable}.i-no ELSE ipcFGItemID.
        ipcCustID = IF ipcCustID EQ "" THEN  {&LineTable}.cust-no ELSE ipcCustID.
        ipcShipID = IF ipcShipID EQ "" THEN {&LineTable}.cust-no ELSE ipcShipID.
        ipdQuantity = IF ipdQuantity EQ 0 THEN {&LineTable}.{&LineQuantity} ELSE ipdQuantity.
        RUN pAddLineTableItem (BUFFER itemfg, 
            ROWID({&LineTable}), 
            YES,
            ipcFGItemID, 
            ipcCustID, 
            ipcShipID, 
            ipdQuantity,
            itemfg.sell-price,
            itemfg.sell-uom,
            {&LineTable}.disc,
            "{&LineTable}").  
    END.
    IF oplReprice THEN 
    DO:  
        FOR EACH bf-{&LineTable} OF {&HeaderTable} NO-LOCK
            WHERE ROWID(bf-{&LineTable}) NE ROWID({&LineTable}), 
            FIRST itemfg OF bf-{&LineTable} NO-LOCK
            WHERE itemfg.i-code EQ "S":
    
                RUN pAddLineTableItem (BUFFER itemfg,
                    ROWID(bf-{&LineTable}), 
                    NO,
                    bf-{&LineTable}.i-no, 
                    bf-{&LineTable}.cust-no, 
                    bf-{&LineTable}.cust-no, 
                    bf-{&LineTable}.{&LineQuantity},
                    itemfg.sell-price,
                    itemfg.sell-uom,
                    bf-{&LineTable}.disc,
                    "{&LineTable}").
                
        END. /*each bf-{&LineTable}*/
    END. /*oplReprice*/
END. /*avail oe-ord*/