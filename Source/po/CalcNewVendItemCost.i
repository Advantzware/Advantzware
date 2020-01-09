
/*------------------------------------------------------------------------
    File        : CalcNewVendItemCost.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Dec 17 07:53:21 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEF VAR v-index AS INT NO-UNDO.

FIND FIRST vendItemCost no-lock    
    WHERE vendItemCost.company EQ cocode
    AND vendItemCost.ItemID    EQ po-ordl.i-no
    AND vendItemCost.ItemType EQ (IF po-ordl.item-type then "RM" ELSE "FG")
    NO-ERROR.
IF AVAIL vendItemCost THEN 
DO:    
    CREATE tt-ei.
    ASSIGN 
/*        tt-ei.company = itemfg.company*/
/*        tt-ei.i-no    = itemfg.i-no   */
        tt-ei.std-uom = vendItemCost.VendorUOM
        .        
END.
    
v-index = 0.      
FOR EACH vendItemCost NO-LOCK  WHERE vendItemCost.company EQ cocode
/*        AND vendItemCost.estimateNo EQ bf-w-job-mat.est-no*/
/*        AND vendItemCost.formNo EQ bf-w-job-mat.frm       */
/*        AND vendItemCost.blankNo EQ bf-w-job-mat.blank-no */
        AND vendItemCost.ItemID    EQ po-ordl.i-no
        AND vendItemCost.ItemType EQ (if po-ordl.item-type then "RM" else "FG")
        AND vendItemCost.vendorID EQ po-ordl.vend-no 
        AND (po-ordl.cust-no NE "" AND vendItemCost.customerID EQ po-ordl.cust-no)   ,
                                                     
        EACH vendItemCostLevel NO-LOCK WHERE vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostId
        BY vendItemCostLevel.vendItemCostLevelID:
         
        /*IF NOT CAN-FIND(FIRST tt-eiv
            WHERE tt-eiv.company   EQ e-itemfg-vend.company
            AND tt-eiv.i-no      EQ bf-w-job-mat.i-no
            AND tt-eiv.vend-no   EQ e-itemfg-vend.vend-no) THEN 
        */
        v-index = v-index + 1.    
        FIND FIRST tt-eiv WHERE tt-eiv.rec_key = vendItemCostLevel.rec_key NO-ERROR.
        IF NOT AVAIL tt-eiv THEN 
        DO:       
            CREATE tt-eiv.
            ASSIGN 
                tt-eiv.rec_key   = vendItemCostLevel.rec_key.
/*            tt-eiv.rec-id    = RECID(vendItemCostLevel).                       */
/*            tt-eiv.est-no    = "".                                             */
/*            tt-eiv.i-no      = vendItemCost.itemID.                            */
/*            tt-eiv.form-no   = 0.                                              */
/*            tt-eiv.blank-no  = 0.                                              */
/*            tt-eiv.company   = vendItemCost.company.                           */
/*            tt-eiv.vend-no   = vendItemCost.vendorID.                          */
/*            tt-eiv.vend-i-no = vendItemCost.vendorItemID.                      */
/*            tt-eiv.item-type = IF vendItemCost.itemType = "RM" THEN YES ELSE no*/
            .
        END.  
        IF /*vendItemCostLevel.vendItemCostLevelID GT 0 AND vendItemCostLevel.vendItemCostLevelID LE 20 */
            v-index GT 0 AND v-index LE 20 THEN 
            ASSIGN /*v-index                  = vendItemCostLevel.vendItemCostLevelID*/
                tt-eiv.run-qty[v-index]  = vendItemCostLevel.quantityBase  /* e-item-vend.run-qty[v-index]*/
                tt-eiv.run-cost[v-index] = vendItemCostLevel.costPerUOM  /* e-item-vend.run-cost[v-index] */
                tt-eiv.setups[v-index]   = vendItemCostLevel.costSetup   /* e-itemfg-vend.setups[v-index] */
                .
                
/*        IF /*vendItemCostLevel.vendItemCostLevelID GT 0 AND vendItemCostLevel.vendItemCostLevelID LE 30*/         */
/*            v-index GT 0 AND v-index LE 30 THEN                                                                   */
/*            assign tt-eiv.roll-w[v-index]   = vendItemCost.validWidth[v-index] /* e-itemfg-vend.roll-w[v-index] */*/
/*                .                                                                                                 */
                            
END. /* each vendcostitem */


