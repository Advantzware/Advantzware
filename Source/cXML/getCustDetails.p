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
DEFINE INPUT        PARAMETER ipcOrderID      AS CHARACTER NO-UNDO.
DEFINE OUTPUT       PARAMETER opcCustomerID   AS CHARACTER NO-UNDO. 
DEFINE OUTPUT       PARAMETER oplSuccess      AS LOGICAL   NO-UNDO. 
DEFINE OUTPUT       PARAMETER opcMessage      AS CHARACTER NO-UNDO. 
DEFINE              PARAMETER BUFFER obf-shipto FOR shipto.

oplSuccess = YES.

/* This function returns valid ShipID */
FUNCTION getShipID RETURNS CHARACTER (cCompany AS CHARACTER,cCustomerID AS CHARACTER,cShipToID AS CHARACTER):    
    FIND FIRST obf-shipto NO-LOCK
         WHERE obf-shipto.company EQ cCompany
           AND obf-shipto.cust-no EQ cCustomerID
           AND obf-shipto.ship-id EQ cShipToID
         NO-ERROR.  
    IF NOT AVAILABLE obf-shipto THEN DO:
        FIND FIRST obf-shipto NO-LOCK
             WHERE obf-shipto.company EQ cCompany NO-ERROR.
        IF AVAILABLE obf-shipto THEN
            cShipToID = obf-shipto.ship-id.
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
  
/* This code has been commented out because as per the new  
   requirements in ticket #60939 this is no longer used to  
   retrieve company, location and customer no */
/*        
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
            FIRST obf-shipto NO-LOCK
                WHERE obf-shipto.company EQ iopcCompany 
                  AND obf-shipto.cust-no EQ edShipto.cust
                  AND obf-shipto.ship-id EQ iopcShipToID
                :
         opcCustomerID = edShipTo.cust.
        END.
    END.
    
    RETURN.
END.
*/

/* Finding shipto based on SiteID */
FIND FIRST obf-shipto NO-LOCK
     WHERE obf-shipto.siteID EQ iopcShipToID NO-ERROR.
IF AVAILABLE obf-shipto THEN DO:
    ASSIGN
        opcCustomerID   = obf-shipto.cust
        iopcCompany     = obf-shipto.company
        iopcWarehouseID = obf-shipto.loc
        iopcShipToID    = obf-shipto.ship-id
        .
        
    RETURN.
END.


/* Finding shipto based on ShipToID */
FIND FIRST obf-shipto NO-LOCK
     WHERE obf-shipto.ship-id EQ iopcShipToID NO-ERROR.
IF NOT AVAILABLE obf-shipto THEN DO:
    ASSIGN
        opcMessage = "AddressID is not valid" 
        oplSuccess = NO
        .
        
    RETURN.
END.

ASSIGN
    opcCustomerID   = obf-shipto.cust
    iopcCompany     = obf-shipto.company
    iopcWarehouseID = obf-shipto.loc
    .
        
/*/* This code has been commented out because as per the new                 */
/*   requirements in ticket #60939 this is no longer used to                 */
/*   retrieve company, location and customer no */                           */
/*/* Finding cust "X" */                                                     */
/*FIND FIRST cust NO-LOCK                                                    */
/*     WHERE cust.active EQ "X" NO-ERROR.                                    */
/*IF AVAILABLE cust THEN DO:                                                 */
/*    ASSIGN                                                                 */
/*        opcCustomerID   = cust.cust-no                                     */
/*        iopcCompany     = cust.company                                     */
/*        iopcShipToID    = getShipID(iopcCompany,opcCustomerID,iopcShipToID)*/
/*        iopcWarehouseID = getWarehouseID(iopcShipToID)                     */
/*        .                                                                  */
/*                                                                           */
/*    RETURN.                                                                */
/*END.                                                                       */
