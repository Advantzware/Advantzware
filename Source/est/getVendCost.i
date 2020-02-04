
/*------------------------------------------------------------------------
    File        : getVendCost.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Nov 18 13:57:07 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE dCostTotal{3}  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dCostPerUOM{3} AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dCostSetup{3}  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cCostUOM{3}    AS CHARACTER NO-UNDO.

DEFINE VARIABLE lError{3}      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage{3}    AS CHARACTER NO-UNDO.
/*DEF VAR lv-setup-{3} LIKE e-item-vend.setup NO-UNDO.*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND FIRST vendItemCost NO-LOCK
    WHERE vendItemCost.company EQ ITEM.company
    AND vendItemCost.itemID    EQ item.i-no
    AND vendItemCost.itemType EQ "RM"
/*    AND vendItemCost.vendorID EQ ip-vend-no*/
    NO-ERROR.
      
IF AVAILABLE vendItemCost /*AND ip-vend-no NE ""*/ THEN 
DO:
RUN GetVendorCost(vendItemCost.company, 
    vendItemCost.ItemID, 
    vendItemCost.itemType, 
    vendItemCost.vendorID, 
    vendItemCost.customerID, 
    "", 
    0, 
    0,
    {1}, 
    vendItemCost.vendorUOM,
    item.s-len, 
    item.s-wid, 
    0, 
    "IN", 
    item.basis-w, 
    "LB/EA", 
    NO,
    OUTPUT dCostPerUOM{3}, 
    OUTPUT dCostSetup{3}, 
    OUTPUT cCostUOM{3},
    OUTPUT dCostTotal{3}, 
    OUTPUT lError{3}, 
    OUTPUT cMessage{3}).  
    
  /* output value dCostTotal already included setup charge from vendorCostProcs.p */
    
assign {2} = dCostPerUOM{3}  
       lv-setup-{3} = dCostSetup{3}
       . 
END.

/* **********************  Internal Procedures  *********************** */

/*PROCEDURE RunGetVendorCost:                                                         */
/*    /*------------------------------------------------------------------------------*/
/*     Purpose:                                                                       */
/*     Notes:                                                                         */
/*    ------------------------------------------------------------------------------*/*/
/*    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.                          */
/*    DEFINE OUTPUT PARAMETER opdCostTotal AS DECIMAL NO-UNDO.                        */
/*    DEFINE OUTPUT PARAMETER opdCostSetup AS DECIMAL NO-UNDO.                        */
/*                                                                                    */
/*    RUN GetVendorCost(vendItemCost.company,                                         */
/*        vendItemCost.ItemID,                                                        */
/*        vendItemCost.itemType,                                                      */
/*        vendItemCost.vendorID,                                                      */
/*        vendItemCost.customerID,                                                    */
/*        "",                                                                         */
/*        0,                                                                          */
/*        0,                                                                          */
/*        DEC(po-ordl.ord-qty:SCREEN-VALUE IN FRAME {&frame-name}),                   */
/*        vendItemCost.vendorUOM,                                                     */
/*        item.s-len,                                                                 */
/*        item.s-wid,                                                                 */
/*        0,                                                                          */
/*        "IN",                                                                       */
/*        item.basis-w,                                                               */
/*        "LB/EA",                                                                    */
/*        NO,                                                                         */
/*        OUTPUT dCostPerUOM,                                                         */
/*        OUTPUT dCostSetup,                                                          */
/*        OUTPUT cCostUOM,                                                            */
/*        OUTPUT dCostTotal,                                                          */
/*        OUTPUT lError,                                                              */
/*        OUTPUT cMessage).                                                           */
/*                                                                                    */
/*                                                                                    */
/*END PROCEDURE.                                                                      */
/*                                                                                    */
