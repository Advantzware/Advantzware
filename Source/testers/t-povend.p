
/*------------------------------------------------------------------------
    File        : t-povend.p
    Purpose     : 

    Syntax      :

    Description : Vendor item cost testing procedure

    Author(s)   : 
    Created     : Mon Nov 11 07:38:54 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE gcCompany AS CHARACTER NO-UNDO INITIAL '001'.
DEF VAR ghVendorCost AS HANDLE NO-UNDO.

DEF VAR v-po# AS INT LABEL " Enter PO# " FORMAT "999999" NO-UNDO.

Main:
DO:
    
    RUN system\VendorCostProcs.p PERSISTENT SET ghVendorCost.
    SESSION:ADD-SUPER-PROCEDURE (ghVendorCost).
    
    UPDATE v-po# WITH CENTERED TITLE "  Vendor Cost Information for PO  ".
        
        //RUN valid-po#.
    RUN GetVendorItemCost.
        

    
END.



/* **********************  Internal Procedures  *********************** */

PROCEDURE GetVendorItemCost:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF    VAR      iPO#        AS INT       NO-UNDO.
    
    DEFINE VARIABLE dCostTotal  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOM AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostSetup  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCostUOM    AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.

    iPO# = v-po# /*105207*/ .
    
    FIND FIRST po-ord NO-LOCK WHERE po-ord.company = gcCompany
        AND po-ord.po-no = iPO#
        NO-ERROR.
    IF NOT AVAIL po-ord THEN 
    DO:
        MESSAGE "No PO available: PO# " iPO#
            VIEW-AS ALERT-BOX. 
        RETURN.
    END.
    
    FOR EACH po-ordl WHERE po-ordl.company = gcCompany
        AND po-ordl.po-no = po-ord.po-no NO-LOCK.
        
        IF po-ordl.item-type THEN 
        DO:  /* RM */
        
            FIND FIRST vendItemCost NO-LOCK
                WHERE vendItemCost.company EQ gcCompany
                AND vendItemCost.itemID    EQ po-ordl.i-no
                AND vendItemCost.itemType EQ "RM"
                NO-ERROR.
                        
            FIND item NO-LOCK WHERE item.company = gcCompany
                AND item.i-no = vendItemCost.itemID NO-ERROR.
                  
            RUN GetVendorCost(vendItemCost.company, 
                vendItemCost.ItemID, 
                vendItemCost.itemType, 
                vendItemCost.vendorID, 
                vendItemCost.customerID, 
                "", 
                0, 
                0,
                po-ordl.ord-qty, 
                vendItemCost.vendorUOM,
                item.s-len, 
                ITEM.s-wid, 
                0, 
                "IN", 
                item.basis-w, 
                "LB/EA", 
                NO,
                OUTPUT dCostPerUOM, 
                OUTPUT dCostSetup, 
                OUTPUT cCostUOM,
                OUTPUT dCostTotal, 
                OUTPUT lError, 
                OUTPUT cMessage).  
              
        END.
        ELSE 
        DO:  /* FG */
            
     
            FIND FIRST itemfg NO-LOCK 
                WHERE itemfg.company EQ gcCompany
                AND itemfg.i-no EQ po-ordl.i-no
                NO-ERROR.
            FIND FIRST vendItemCost EXCLUSIVE-LOCK
                WHERE vendItemCost.company EQ itemfg.company
                AND vendItemCost.itemID EQ itemfg.i-no
                AND vendItemCost.itemType EQ "FG"
                NO-ERROR.
            ASSIGN 
                vendItemCost.effectiveDate  = 1/1/2019
                vendItemCost.expirationDate = 1/1/2020
                .

            RUN GetVendorCost(vendItemCost.company, 
                vendItemCost.ItemID, 
                vendItemCost.itemType, 
                vendItemCost.vendorID, 
                vendItemCost.customerID, 
                "", 
                0, 
                0,
                po-ordl.ord-qty, 
                vendItemCost.vendorUOM,
                itemfg.t-len, 
                itemfg.t-wid, 
                0, 
                "IN", 
                itemfg.weight-100 / 100, 
                "LB/EA", 
                NO,
                OUTPUT dCostPerUOM, 
                OUTPUT dCostSetup, 
                OUTPUT cCostUOM,
                OUTPUT dCostTotal, 
                OUTPUT lError, 
                OUTPUT cMessage).  
        END.
         
        MESSAGE  "PO#: "  po-ordl.po-no  "   Item#: " po-ordl.i-no " Item-type: RM? " po-ordl.item-type SKIP(1)
        "------------------------- Vendor Cost information ------------------------------" skip(1) 
            "Total Cost: " dCostTotal SKIP
            "Cost UOM: " cCostUOM  "Cost Per UOM: " dCostPerUOM SKIP 
            "Cost Setup: " dCostSetup SKIP
            "Error: " lError SKIP(1) 
            "Message: " cMessage SKIP(1)
            "------------------------- PO information ------------------------------" skip(1)
            "Item: " po-ordl.i-no  "  Desc: " po-ordl.dscr[1] skip
            "Qty: " po-ordl.ord-qty  "  UOM: " po-ordl.pr-qty-uom  skip
            "Cost:" po-ordl.cost     "PR UOM: " po-ordl.pr-uom  skip
            "Setup: " po-ordl.setup skip
            "Total Cost: " po-ordl.t-cost
            
            
            VIEW-AS ALERT-BOX.
      
        
    END.
    

END PROCEDURE.

