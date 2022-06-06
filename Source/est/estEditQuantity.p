
/*------------------------------------------------------------------------
    File        : estEditQuantity.p
    Purpose     : To edit the estimate quantities before calculation

    Syntax      :

    Description : Estimate Edit Quantity.

    Author(s)   : 
    Created     : 05/25/2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***********************Parameter Definitions  ********************* */
DEFINE INPUT PARAMETER ipriEB AS RECID NO-UNDO.

/* ***************************  Definitions  ************************** */
{est/ttEstimateQuantity.i}
DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD company AS CHAR
    FIELD std-uom AS CHAR
    FIELD i-no AS CHAR
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20
    INDEX i-no company i-no.

DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD company AS CHAR
    FIELD i-no AS CHAR
    FIELD vend-no AS CHAR
    FIELD item-type AS LOG
    FIELD row-id AS ROWID
    FIELD rec_key AS CHAR
    FIELD SELECTED AS LOG EXTENT 10
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20
    FIELD setups AS DECIMAL DECIMALS 2 EXTENT 20
    FIELD roll-w AS DECIMAL DECIMALS 4 EXTENT 30
    INDEX vend-no company i-no vend-no
    INDEX i-no company item-type i-no vend-no
    INDEX row-id row-id.

DEFINE VARIABLE glCorrware AS LOGICAL NO-UNDO INITIAL NO.
DEFINE VARIABLE glopError AS LOGICAL NO-UNDO.

DEFINE BUFFER buf-eb FOR eb.
DEFINE BUFFER buf-ef FOR ef.
DEFINE BUFFER buf-est FOR est.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN pCheckEstType.

/* **********************  Internal Procedures  *********************** */
PROCEDURE pCheckEstType PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/  
   FIND FIRST buf-eb NO-LOCK 
    WHERE RECID(buf-eb) EQ ipriEB NO-ERROR.
   
   FIND FIRST sys-ctrl NO-LOCK 
        WHERE sys-ctrl.company EQ buf-eb.company
         AND  sys-ctrl.name    EQ "MSFCALC" NO-ERROR.
         
   glCorrware = (NOT AVAILABLE sys-ctrl) OR sys-ctrl.char-fld EQ "Corrware".

   IF AVAILABLE buf-eb AND 
      (buf-eb.est-type EQ 1 OR buf-eb.est-type EQ 2 OR 
       buf-eb.est-type EQ 5 OR buf-eb.est-type EQ 6) THEN  
       RUN pBuildTempTable(BUFFER buf-eb).
   ELSE 
       MESSAGE "Edit-Quantity is not applicable for combo estimates"
       VIEW-AS ALERT-BOX. 
       
END PROCEDURE.          
 
PROCEDURE pBuildTempTable PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/ 
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    
    DEFINE VARIABLE dQuantityPerSet AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttEstimateQuantity.
             
    FIND FIRST est-qty NO-LOCK 
         WHERE est-qty.company EQ ipbf-eb.company  
           AND est-qty.est-no EQ  ipbf-eb.est-no NO-ERROR.
   
    IF AVAILABLE est-qty THEN
    DO: 
        FIND FIRST est NO-LOCK
             WHERE est.company EQ est-qty.company 
               AND est.est-no EQ est-qty.est-no NO-ERROR.
                      
        CREATE ttEstimateQuantity.
        ASSIGN ttEstimateQuantity.EstQuantity[1] = INTEGER(est-qty.eqty)
               ttEstimateQuantity.EstRelease[1]  = est-qty.qty[21]
               ttEstimateQuantity.EstRunship[1]  = est-qty.whsed[1].
             
        DO iCount = 2 TO 20:
            ASSIGN ttEstimateQuantity.EstQuantity[iCount] = est-qty.qty[iCount]
                   ttEstimateQuantity.EstRelease[iCount]  = est-qty.qty[iCount + 20]
                   ttEstimateQuantity.EstRunship[iCount]  = est-qty.whsed[iCount].
        END.                      
           
        ASSIGN dQuantityPerSet  = IF NOT glCorrware THEN 1
                                  ELSE (IF ipbf-eb.quantityPerSet < 0 THEN -1 / ipbf-eb.quantityPerSet ELSE ipbf-eb.quantityPerSet).
                                  
        FIND FIRST ttEstimateQuantity NO-LOCK NO-ERROR.
        
        IF ttEstimateQuantity.EstQuantity[1] > 0 THEN 
            ttEstimateQuantity.EstMSF[1] = IF glCorrware THEN 
                                              ((ttEstimateQuantity.EstQuantity[1] * ipbf-eb.t-len * ipbf-eb.t-wid * .007)
                                               * dQuantityPerSet
                                               / 1000 )
                                            ELSE 
                                              ((ttEstimateQuantity.EstQuantity[1] * ipbf-eb.t-len * ipbf-eb.t-wid / 144)
                                               * dQuantityPerSet
                                               / 1000 ).                                     

        DO iCount = 2 TO 20:
            IF est-qty.qty[iCount] > 0 THEN
                ttEstimateQuantity.EstMSF[iCount] = IF glCorrware THEN
                                                    ( (est-qty.qty[iCount] * ipbf-eb.t-len * ipbf-eb.t-wid * .007)
                                                    * dQuantityPerSet
                                                    / 1000 )
                                                    ELSE  ( (est-qty.qty[iCount] * ipbf-eb.t-len * ipbf-eb.t-wid / 144)
                                                    * dQuantityPerSet
                                                    / 1000 ).
        END.
        FIND FIRST ef NO-LOCK 
            WHERE ef.company EQ ipbf-eb.company 
              AND ef.est-no EQ ipbf-eb.est-no
              AND ef.form-no EQ ipbf-eb.form-no NO-ERROR.
               
        IF ipbf-eb.pur-man THEN 
        RUN pCreateTempFromVendItemCost(ipbf-eb.company, "FG", ipbf-eb.stock-no, ipbf-eb.est-no, ipbf-eb.form-no, ipbf-eb.blank-no).
        ELSE 
        RUN pCreateTempFromVendItemCost(ipbf-eb.company, "RM", ef.board, ipbf-eb.est-no, ipbf-eb.form-no, ipbf-eb.blank-no).
        RUN pGetNextPriceBreaks(BUFFER ef).
        RUN pCallUIToUpdate(BUFFER est-qty, BUFFER est).
    END.
END PROCEDURE.

PROCEDURE pCallUIToUpdate PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Call UI for updating quantities for estimate
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est-qty FOR est-qty.
    DEFINE PARAMETER BUFFER ipbf-est     FOR est.
        
    RUN est/Updestqtyd.w (ipriEB, glCorrware, OUTPUT glopError, INPUT-OUTPUT TABLE ttEstimateQuantity BY-REFERENCE).
    
    IF NOT glopError THEN
       RUN pProcessUpdatedQuantity(BUFFER ipbf-est-qty, BUFFER ipbf-est). 
          
END PROCEDURE.

PROCEDURE pGetNextPriceBreaks PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
      DEFINE PARAMETER BUFFER ipbf-ef FOR ef.
      DEFINE VARIABLE iCountRunQty AS INTEGER NO-UNDO.
      
      FIND FIRST item NO-LOCK 
          WHERE ITEM.company EQ ipbf-ef.company
            AND item.i-no EQ ipbf-ef.board 
      NO-ERROR.
      
      FIND FIRST tt-ei NO-ERROR.
    
      IF AVAIL tt-ei THEN 
      DO:
          FIND FIRST tt-eiv 
               WHERE tt-eiv.company EQ tt-ei.company 
                 AND tt-eiv.i-no    EQ tt-ei.i-no 
                 AND tt-eiv.item-type EQ YES 
                 AND tt-eiv.vend-no EQ "" NO-ERROR.
                     
          FIND FIRST ttEstimateQuantity.          
          IF AVAILABLE tt-eiv AND AVAILABLE ttEstimateQuantity THEN            
          DO iCountRunQty = 1 TO 20:
              ttEstimateQuantity.EstNextQuantity[iCountRunQty] = tt-eiv.run-qty[iCountRunQty].
              IF tt-ei.std-uom NE "EA" AND AVAILABLE item 
              AND ttEstimateQuantity.EstNextQuantity[iCountRunQty] LT 100 THEN
              DO:
                  RUN sys/ref/convquom.p(tt-ei.std-uom, "EA", item.basis-w,
                      ipbf-ef.gsh-len, ipbf-ef.gsh-wid, ipbf-ef.gsh-dep,
                      ttEstimateQuantity.EstNextQuantity[iCountRunQty], OUTPUT ttEstimateQuantity.EstNextQuantity[iCountRunQty]).
              
                  IF  (ttEstimateQuantity.EstNextQuantity[iCountRunQty] - INTEGER(ttEstimateQuantity.EstNextQuantity[iCountRunQty])) > 0 
                      THEN ttEstimateQuantity.EstNextQuantity[iCountRunQty] = INTEGER(ttEstimateQuantity.EstNextQuantity[iCountRunQty]) + 1.
                  ELSE ttEstimateQuantity.EstNextQuantity[iCountRunQty] = INTEGER(ttEstimateQuantity.EstNextQuantity[iCountRunQty]). 
              END.
              ELSE 
                  ASSIGN ttEstimateQuantity.EstNextQuantity[iCountRunQty] = 0.       
                      
          END. 
      END.

END PROCEDURE.

PROCEDURE pProcessUpdatedQuantity PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est-qty FOR est-qty.
    DEFINE PARAMETER BUFFER ipbf-est     FOR est. 
    
    DEFINE VARIABLE iEQtyBeforeEdit LIKE est-qty.eqty NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
    FIND CURRENT ipbf-est NO-ERROR.
    ASSIGN ipbf-est.est-qty[1] = INTEGER (ttEstimateQuantity.EstQuantity [1])
           ipbf-est.est-qty[2] = INTEGER (ttEstimateQuantity.EstQuantity [2])
           ipbf-est.est-qty[3] = INTEGER (ttEstimateQuantity.EstQuantity [3])
           ipbf-est.est-qty[4] = INTEGER (ttEstimateQuantity.EstQuantity [4]).
            
    FIND CURRENT ipbf-est NO-LOCK NO-ERROR.
    
    ASSIGN iEQtyBeforeEdit = ipbf-est-qty.eqty.
    FIND CURRENT ipbf-est-qty NO-ERROR.
    ipbf-est-qty.eqty = INTEGER (ttEstimateQuantity.EstQuantity [1]).
    DO iCount = 1 TO 20:
        ASSIGN est-qty.qty[iCount] = ttEstimateQuantity.EstQuantity[iCount] 
               est-qty.qty[iCount + 20] = ttEstimateQuantity.EstRelease[iCount] 
               est-qty.whsed[iCount] = ttEstimateQuantity.EstRunship[iCount].
    END.
    
    FIND CURRENT ipbf-est-qty NO-LOCK NO-ERROR.
    
    FOR EACH buf-eb
       WHERE buf-eb.company EQ ipbf-est.company
         AND buf-eb.est-no  EQ ipbf-est.est-no
         AND buf-eb.eqty    EQ iEQtyBeforeEdit
         AND buf-eb.form-no NE 0:  
                       
        buf-eb.eqty = INTEGER (ttEstimateQuantity.EstQuantity [1]).
    END.
     
    FOR EACH buf-ef
       WHERE buf-ef.company EQ ipbf-est.company
         AND buf-ef.est-no  EQ ipbf-est.est-no
         AND buf-ef.eqty    EQ iEQtyBeforeEdit:  
                
        buf-ef.eqty = INTEGER (ttEstimateQuantity.EstQuantity [1]).
    END.

END PROCEDURE.

PROCEDURE pCreateTempFromVendItemCost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    FOR EACH vendItemCost NO-LOCK
        WHERE vendItemCost.company  EQ ipcCompany
        AND vendItemCost.itemType EQ ipcItemType
        AND vendItemCost.itemID EQ ipcItemID
        AND (ipcItemType EQ "FG" AND (vendItemCost.estimateNo   EQ ipcEstNo
        AND vendItemCost.formNo  EQ ipiFormNo
        AND vendItemCost.blankNo EQ ipiBlankNo)
        OR (ipcItemType EQ "RM" AND vendItemCost.estimateNo EQ ""))
        BREAK BY vendItemCost.vendorID:

        IF FIRST(vendItemCost.vendorID) THEN 
        DO:

            CREATE tt-ei.
            ASSIGN
                tt-ei.std-uom = IF vendItemCost.vendorUOM <> "" THEN vendItemCost.vendorUOM ELSE "EA"
                tt-ei.i-no    = ipcItemID
                tt-ei.company = vendItemCost.company.

        END.
        IF NOT CAN-FIND(FIRST tt-eiv
                WHERE tt-eiv.company   EQ vendItemCost.company
                AND tt-eiv.i-no      EQ ipcItemID
                AND tt-eiv.vend-no   EQ vendItemCost.vendorID) THEN 
        DO:
            CREATE tt-eiv.
            ASSIGN
                tt-eiv.row-id = ROWID(vendItemCost)
                tt-eiv.i-no   = ipcItemID
                tt-eiv.company = vendItemCost.company
                tt-eiv.vend-no = vendItemCost.vendorID
                tt-eiv.item-type = vendItemCost.itemType = "RM"
                tt-eiv.rec_key   = vendItemCost.rec_key
                tt-eiv.roll-w[27] = vendItemCost.dimWidthMinimum
                tt-eiv.roll-w[28] = vendItemCost.dimWidthMaximum
                tt-eiv.roll-w[29] = vendItemCost.dimLengthMinimum
                tt-eiv.roll-w[30] = vendItemCost.dimLengthMaximum
                .
            DO iIndex = 1 TO 26:
                ASSIGN 
                    tt-eiv.roll-w[iIndex]   = vendItemCost.validWidth[iIndex].
            END.
        END.
        iIndex = 0.
        FOR EACH vendItemCostLevel NO-LOCK WHERE vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostId
            BY vendItemCostLevel.quantityBase:
            
            iIndex = iIndex + 1.
            IF iIndex LE 20 THEN 
                ASSIGN 
                    tt-eiv.run-qty[iIndex]  = vendItemCostLevel.quantityBase  /* e-item-vend.run-qty[v-index]*/
                    tt-eiv.run-cost[iIndex] = vendItemCostLevel.costPerUOM  /* e-item-vend.run-cost[v-index] */
                    tt-eiv.setups[iIndex]   = vendItemCostLevel.costSetup   /* e-itemfg-vend.setups[v-index] */
                    tt-eiv.SELECTED[iIndex] = vendItemCostLevel.useForBestCost
                    .
        END.
    END.

END PROCEDURE.

