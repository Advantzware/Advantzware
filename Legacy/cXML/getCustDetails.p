/*------------------------------------------------------------------------
    File        : cXML\genCustDetails.p
    Purpose     : Gets company code,customer number and shiptoID as inputs 
                  and returns valid company code,customer number,location code 
                  and shiptoID

    Syntax      :

    Description : Gets company code,customer number and shiptoID as inputs 
                  and returns valid company code,customer number,location code
                  and shiptoID
                  
    Author(s)   : Vishnu Vellanki
    Created     : Tue AUG 6 07:33:22 EDT 2019
    
    Notes       : 
----------------------------------------------------------------------*/

DEFINE INPUT        PARAMETER ipcIdentity     AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcShipToID    AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcCompany     AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcWarehouseID AS CHARACTER NO-UNDO.  
DEFINE OUTPUT       PARAMETER opcCustomerID   AS CHARACTER NO-UNDO. 
DEFINE              PARAMETER BUFFER bfshipto FOR shipto.

/* This function returns valid ShipID */
FUNCTION getShipID RETURNS CHARACTER (cCompany AS CHARACTER,cCustomerID AS CHARACTER,cShipToID AS CHARACTER):    
    FIND FIRST bfshipto NO-LOCK
         WHERE bfshipto.company EQ cCompany
           AND bfshipto.cust-no EQ cCustomerID
           AND bfshipto.ship-id EQ cShipToID
         NO-ERROR.  
    IF NOT AVAILABLE bfshipto THEN DO:
        FIND FIRST bfshipto NO-LOCK
             WHERE bfshipto.company EQ cCompany NO-ERROR.
        IF AVAILABLE bfshipto THEN
            cShipToID = shipto.ship-id.
    END.
    
    RETURN cShipToID.    
END FUNCTION.

/* This function returns valid locationID */
FUNCTION getWarehouseID RETURNS CHARACTER (cShipToID AS CHARACTER):
    DEFINE VARIABLE cWarehouseID AS CHARACTER NO-UNDO.
    
    FIND FIRST shipto NO-LOCK
         WHERE shipto.ship-id EQ cShipToID 
         NO-ERROR.  
    IF AVAILABLE shipto THEN
        cWarehouseID = shipto.loc.
    
    IF cWarehouseID EQ "" THEN DO:
        FIND FIRST loc NO-LOCK
             WHERE loc.active EQ TRUE NO-ERROR.
        IF AVAILABLE loc THEN
            cWarehouseID = loc.loc.     
    END.
    
    RETURN cWarehouseID.    
END FUNCTION.

/* This function validates company */
FUNCTION getCompany RETURNS CHARACTER (cCompany AS CHARACTER):    
    IF cCompany EQ "" THEN DO:
        FIND FIRST company NO-LOCK NO-ERROR.
        IF AVAILABLE company THEN
            cCompany = company.company.     
    END.
    
    RETURN cCompany.    
END FUNCTION.

iopcCompany = getCompany(iopcCompany).
  
/* Finding sys-cntrl */
FIND FIRST sys-ctrl-shipto NO-LOCK
     WHERE sys-ctrl-shipto.name      EQ 'cXMLOrder'
       AND sys-ctrl-shipto.cust-vend EQ YES
       AND sys-ctrl-shipto.char-fld  EQ ipcIdentity
       AND sys-ctrl-shipto.log-fld   EQ YES NO-ERROR.
IF AVAILABLE sys-ctrl-shipto THEN DO:
    ASSIGN
        opcCustomerID   = sys-ctrl-shipto.cust-vend-no
        iopcCompany     = sys-ctrl-shipto.company
        iopcShipToID    = getShipID(iopcCompany,opcCustomerID,iopcShipToID)
        iopcWarehouseID = getWarehouseID(iopcShipToID)
        .
       
  /* Option to find cust# by cust-no and ship-id */
    IF sys-ctrl-shipto.int-fld EQ 1 
        AND opcCustomerID GT "" 
        AND iopcShipToID GT "" THEN DO:
        FOR EACH edMast NO-LOCK 
        WHERE edMast.cust EQ opcCustomerID
        ,
        EACH edShipTo NO-LOCK 
            WHERE edShipTo.partner EQ edMast.partner
              AND edShipTo.siteID  EQ ipcIdentity
            ,
            FIRST bfshipto NO-LOCK
                WHERE bfshipto.company EQ iopcCompany 
                  AND bfshipto.cust-no EQ edShipto.cust
                  AND bfshipto.ship-id EQ iopcShipToID
                :
         opcCustomerID = edShipTo.cust.
        END.
    END.
    
    RETURN.
END.

/* Finding shipto */
FIND FIRST bfshipto NO-LOCK
     WHERE bfshipto.ship-id EQ iopcShipToID NO-ERROR.
IF AVAILABLE bfshipto THEN DO:
    ASSIGN
        opcCustomerID   = bfshipto.cust
        iopcCompany     = bfshipto.company
        iopcWarehouseID = bfshipto.loc
        .
        
    RETURN.
END.

/* Finding cust "X" */ 
FIND FIRST cust NO-LOCK
     WHERE cust.active EQ "X" NO-ERROR.
IF AVAILABLE cust THEN DO:
    ASSIGN 
        opcCustomerID   = cust.cust-no
        iopcCompany     = cust.company
        iopcShipToID    = getShipID(iopcCompany,opcCustomerID,iopcShipToID)
        iopcWarehouseID = getWarehouseID(iopcShipToID)
        .
        
    RETURN.
END.
