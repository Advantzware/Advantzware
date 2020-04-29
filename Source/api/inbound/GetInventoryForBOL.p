/*------------------------------------------------------------------------
    File        : api/inbound/GetInventoryForBOL.p
    Purpose     : Fetches Inventory response data for the given input data

    Syntax      :

    Description : Fetches Inventory response data for the given input data

    Author(s)   : Vishnu Vellanki
    Created     : Fri Nov 29 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
{inventory/ttinventory.i "NEW SHARED"}.
{api/inbound/ttBOLLine.i}
{api/inbound/ttItem.i}

DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiBOLID   AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttBOLLine. 
DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

DEFINE VARIABLE cInventoryStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE lBOLPosted       AS LOGICAL   NO-UNDO.

DEFINE VARIABLE cItemFG                      AS CHARACTER NO-UNDO INITIAL "FG" /* Finished Good */.
DEFINE VARIABLE cShippedInventoryStatus      AS CHARACTER NO-UNDO INITIAL "Shipped". 
DEFINE VARIABLE cOnHandInventoryStatus       AS CHARACTER NO-UNDO INITIAL "OnHand".
DEFINE VARIABLE cInsufficientInventoryStatus AS CHARACTER NO-UNDO INITIAL "InsufficientInventory".
DEFINE VARIABLE cNoInventoryStatus           AS CHARACTER NO-UNDO INITIAL "NoInventory".
DEFINE VARIABLE cInvalidTagStatus            AS CHARACTER NO-UNDO INITIAL "TagIsEmpty".

oplSuccess = YES.

/* Validate company */
IF NOT CAN-FIND(FIRST company NO-LOCK
            WHERE company.company EQ ipcCompany) THEN DO:
    ASSIGN 
        opcMessage = opcMessage + "Invalid Company"
        oplSuccess = NO
        .
        
    RETURN.
END.
.
/* Validate BOLID */
FIND FIRST oe-bolh NO-LOCK
     WHERE oe-bolh.company EQ ipcCompany
       AND oe-bolh.bol-no  EQ ipiBOLID 
      NO-ERROR.
IF NOT AVAILABLE oe-bolh THEN DO:
    ASSIGN 
        opcMessage = opcMessage + "Invalid BOL"
        oplSuccess = NO
        .
        
    RETURN.
END.

lBOLPosted = oe-bolh.posted. /* BOL posted or not */

/* Gets inventory for each BOL line */
BOLLINE-BLK:
FOR EACH  oe-boll NO-LOCK
    WHERE oe-boll.company EQ ipcCompany
      AND oe-boll.bol-no  EQ ipiBOLID:
    
    CREATE ttBOLLine.
    ASSIGN
        ttBOlLine.PrimaryID        = oe-boll.i-no
        ttBOlLine.InventoryStockID = oe-boll.tag
        ttBOlLine.ItemType         = cItemFG /* finished goods */
        .  
    
    IF oe-boll.tag EQ "" THEN DO:
        ttBOLLine.InventoryStatus = cInvalidTagStatus. /* Sets status to invalid tag if tag is null */
        NEXT BOLLINE-BLK.  /* If tag is null, then fetch next bolline */
    END.  
     
    RUN api\inbound\GetInventoryDetails.p (
        INPUT  oe-boll.company, /* Company */
        INPUT  "",              /* Location */
        INPUT  "",              /* Bin */
        INPUT  oe-boll.tag,     /* Tag */
        INPUT  oe-boll.i-no,    /* Primary ID */
        INPUT  oe-boll.job-no,  /* Job Number */
        INPUT  oe-boll.job-no2, /* Job Number2 */
        INPUT  oe-boll.cust-no, /* Customer Number */
        INPUT  0,               /* Po # */
        INPUT  YES,             /* Include Zero qty bins */
        INPUT  YES,             /* Include empty tag bins */
        INPUT  "",              /* status id */
        INPUT  ?,               /* On Hold status. Send ? for both yes and no */              
        INPUT  cItemFG ,        /* Item Type */
        OUTPUT oplSuccess,
        OUTPUT opcMessage,
        OUTPUT TABLE ttItem
        ).  
    
    /* Checks whether inventory is found or not */           
    IF NOT TEMP-TABLE ttItem:HAS-RECORDS THEN DO:
    
        ttBOlLine.InventoryStatus  = cNoInventoryStatus. /* Sets status to no-inventory if inventory is not found */
        NEXT BOLLINE-BLK.  /* If inventory is not found, then fetch next bolline */
        
    END. 
    
    FOR EACH ttItem:
         
        BUFFER-COPY ttItem EXCEPT ttItem.PrimaryID ttItem.InventoryStockID ttItem.ItemType TO ttBOLLine.
        
        /* Fetch Inventory Status */
        IF NOT lBOLPosted THEN DO:
            IF ttItem.Quantity >= oe-boll.qty THEN
                cInventoryStatus = cOnHandInventoryStatus. /* On-hand */
            ELSE IF ttItem.Quantity < oe-boll.qty AND ttItem.Quantity NE 0 THEN
                 cInventoryStatus = cInsufficientInventoryStatus.  /* Insufficient */
        END.
        ELSE
            cInventoryStatus = cShippedInventoryStatus. /* Shipped */
            
        ttBOLLine.InventoryStatus = cInventoryStatus.
    END.  

END.








    
    


