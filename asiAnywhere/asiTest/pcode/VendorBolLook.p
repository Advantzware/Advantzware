/*------------------------------------------------------------------------
    File        : VendorBolLook.p
    Purpose     : VendorBol
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttVendorBolLookup NO-UNDO 
    FIELD vBolNo  AS INT
    FIELD vINo  AS CHARACTER
    FIELD vCustNo  AS CHARACTER
    FIELD vJobNo  AS CHARACTER
    FIELD vJobNo2   AS INT
    FIELD vOrdNo  AS INT 
    FIELD vLine  AS INT 
    FIELD vPoNo   AS CHARACTER 
    FIELD vQty    AS DEC    
    FIELD vVendorCode   AS CHARACTER  
    FIELD vVendorDeptCode    AS CHARACTER  
    FIELD vVendorPlantCode    AS CHARACTER  
    FIELD vCustPartNo    AS CHARACTER  
    FIELD vPlantTotOhQty    AS DEC  
    FIELD vTransDate AS DATE FORMAT "99/99/99"
    FIELD vSellPrice AS DEC
    FIELD vSelectedFlag AS LOG INIT FALSE
    .
    
DEFINE DATASET dsVendorBolLookup FOR ttVendorBolLookup .

DEF BUFFER b-vend-whse-trans FOR vend-whse-trans.
DEF BUFFER b-vend-whse-item  FOR vend-whse-item.
DEF BUFFER b-oe-bolh         FOR oe-bolh.
DEF BUFFER b-oe-boll         FOR oe-boll.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF VAR v-r-no LIKE vend-whse-trans.r-no NO-UNDO.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVendorBolLookup.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField     = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".




IF prmAction <> "search" THEN DO:

    FOR EACH b-oe-bolh NO-LOCK WHERE b-oe-bolh.company = prmComp
                             AND b-oe-bolh.posted  = YES
                       USE-INDEX b-no:
        FOR EACH b-oe-boll NO-LOCK WHERE b-oe-boll.company = b-oe-bolh.company
                                AND b-oe-boll.bol-no  = b-oe-bolh.bol-no:
            FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = b-oe-boll.company
                                    AND b-vend-whse-item.fg-item-no  = b-oe-boll.i-no NO-LOCK NO-ERROR.
            IF AVAILABLE b-vend-whse-item THEN DO:
                CREATE ttVendorBolLookup.
                    ASSIGN            
                        ttVendorBolLookup.vBolNo = b-oe-boll.bol-no 
                        ttVendorBolLookup.vINo = b-oe-boll.i-no
                        ttVendorBolLookup.vCustNo = b-oe-boll.cust-no 
                        ttVendorBolLookup.vJobNo = b-oe-boll.job-no
                        ttVendorBolLookup.vJobNo2 = b-oe-boll.job-no2    
                        ttVendorBolLookup.vOrdNo = b-oe-boll.ord-no   
                        ttVendorBolLookup.vLine = b-oe-boll.line   
                        ttVendorBolLookup.vPoNo = b-oe-boll.po-no   
                        ttVendorBolLookup.vQty = b-oe-boll.qty    
                        ttVendorBolLookup.vVendorCode = CAPS(b-vend-whse-item.vendor-code)  
                        ttVendorBolLookup.vVendorDeptCode = CAPS(b-vend-whse-item.vendor-dept-code)
                        ttVendorBolLookup.vVendorPlantCode = CAPS(b-vend-whse-item.vendor-plant-code)
                        ttVendorBolLookup.vCustPartNo = CAPS(b-vend-whse-item.cust-part-no)   
                        ttVendorBolLookup.vTransDate = b-oe-boll.bol-date                        
                        ttVendorBolLookup.vPlantTotOhQty = b-oe-boll.qty                       
                        .
            END.

            FIND FIRST b-oe-ordl WHERE b-oe-ordl.company  EQ prmComp
                          AND b-oe-ordl.i-no     EQ ttVendorBolLookup.vINo
                          AND b-oe-ordl.job-no   EQ ttVendorBolLookup.vJobNo
                          AND b-oe-ordl.ord-no   EQ ttVendorBolLookup.vOrdNo
                          AND b-oe-ordl.line     EQ ttVendorBolLookup.vLine NO-LOCK NO-ERROR.
            IF AVAILABLE(b-oe-ordl) THEN
                ttVendorBolLookup.vSellPrice = b-oe-ordl.t-price.

        END.
    END.
END.


/******************Search***********************************/
IF prmAction = "search" then do:
     if prmField = "BolNum"  then do:
         if prmCondition = "EQUAL" then do:
                     

            FOR EACH b-oe-bolh NO-LOCK WHERE b-oe-bolh.company = prmComp
                             AND b-oe-bolh.posted  = YES
                       USE-INDEX b-no:
            FOR EACH b-oe-boll NO-LOCK WHERE b-oe-boll.company = b-oe-bolh.company
                                AND b-oe-boll.bol-no  = b-oe-bolh.bol-no AND b-oe-boll.bol-no = INTEGER(prmText):
                FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = b-oe-boll.company
                                    AND b-vend-whse-item.fg-item-no  = b-oe-boll.i-no NO-LOCK NO-ERROR.
                IF AVAILABLE b-vend-whse-item THEN DO:
                    CREATE ttVendorBolLookup.
                        ASSIGN            
                            ttVendorBolLookup.vBolNo = b-oe-boll.bol-no 
                            ttVendorBolLookup.vINo = b-oe-boll.i-no
                            ttVendorBolLookup.vCustNo = b-oe-boll.cust-no 
                            ttVendorBolLookup.vJobNo = b-oe-boll.job-no
                            ttVendorBolLookup.vJobNo2 = b-oe-boll.job-no2    
                            ttVendorBolLookup.vOrdNo = b-oe-boll.ord-no   
                            ttVendorBolLookup.vLine = b-oe-boll.line   
                            ttVendorBolLookup.vPoNo = b-oe-boll.po-no   
                            ttVendorBolLookup.vQty = b-oe-boll.qty    
                            ttVendorBolLookup.vVendorCode = CAPS(b-vend-whse-item.vendor-code)  
                            ttVendorBolLookup.vVendorDeptCode = CAPS(b-vend-whse-item.vendor-dept-code)
                            ttVendorBolLookup.vVendorPlantCode = CAPS(b-vend-whse-item.vendor-plant-code)
                            ttVendorBolLookup.vCustPartNo = CAPS(b-vend-whse-item.cust-part-no)   
                            ttVendorBolLookup.vTransDate = b-oe-boll.bol-date        
                            ttVendorBolLookup.vPlantTotOhQty = b-oe-boll.qty                 
                            .
                END.

                FIND FIRST b-oe-ordl WHERE b-oe-ordl.company  EQ prmComp
                          AND b-oe-ordl.i-no     EQ ttVendorBolLookup.vINo
                          AND b-oe-ordl.job-no   EQ ttVendorBolLookup.vJobNo
                          AND b-oe-ordl.ord-no   EQ ttVendorBolLookup.vOrdNo
                          AND b-oe-ordl.line     EQ ttVendorBolLookup.vLine NO-LOCK NO-ERROR.
                IF AVAILABLE(b-oe-ordl) THEN
                    ttVendorBolLookup.vSellPrice = b-oe-ordl.t-price.

                END.
            END.

         END.          
     END .  /* if prmField = BolNum  */


     if prmField = "fgitem"  then do:
          if prmCondition = "EQUAL" then do:            
                             

            FOR EACH b-oe-bolh NO-LOCK WHERE b-oe-bolh.company = prmComp
                             AND b-oe-bolh.posted  = YES
                       USE-INDEX b-no:
            FOR EACH b-oe-boll NO-LOCK WHERE b-oe-boll.company = b-oe-bolh.company
                                AND b-oe-boll.bol-no  = b-oe-bolh.bol-no AND b-oe-boll.i-no = prmText:
                FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = b-oe-boll.company
                                    AND b-vend-whse-item.fg-item-no  = b-oe-boll.i-no NO-LOCK NO-ERROR.
                IF AVAILABLE b-vend-whse-item THEN DO:
                    CREATE ttVendorBolLookup.
                        ASSIGN            
                            ttVendorBolLookup.vBolNo = b-oe-boll.bol-no 
                            ttVendorBolLookup.vINo = b-oe-boll.i-no
                            ttVendorBolLookup.vCustNo = b-oe-boll.cust-no 
                            ttVendorBolLookup.vJobNo = b-oe-boll.job-no
                            ttVendorBolLookup.vJobNo2 = b-oe-boll.job-no2    
                            ttVendorBolLookup.vOrdNo = b-oe-boll.ord-no   
                            ttVendorBolLookup.vLine = b-oe-boll.line   
                            ttVendorBolLookup.vPoNo = b-oe-boll.po-no   
                            ttVendorBolLookup.vQty = b-oe-boll.qty    
                            ttVendorBolLookup.vVendorCode = CAPS(b-vend-whse-item.vendor-code)  
                            ttVendorBolLookup.vVendorDeptCode = CAPS(b-vend-whse-item.vendor-dept-code)
                            ttVendorBolLookup.vVendorPlantCode = CAPS(b-vend-whse-item.vendor-plant-code)
                            ttVendorBolLookup.vCustPartNo = CAPS(b-vend-whse-item.cust-part-no)   
                            ttVendorBolLookup.vTransDate = b-oe-boll.bol-date 
                            ttVendorBolLookup.vPlantTotOhQty = b-oe-boll.qty                         
                            .
                END.

                FIND FIRST b-oe-ordl WHERE b-oe-ordl.company  EQ prmComp
                          AND b-oe-ordl.i-no     EQ ttVendorBolLookup.vINo
                          AND b-oe-ordl.job-no   EQ ttVendorBolLookup.vJobNo
                          AND b-oe-ordl.ord-no   EQ ttVendorBolLookup.vOrdNo
                          AND b-oe-ordl.line     EQ ttVendorBolLookup.vLine NO-LOCK NO-ERROR.
                IF AVAILABLE(b-oe-ordl) THEN
                    ttVendorBolLookup.vSellPrice = b-oe-ordl.t-price.

                END.
            END.

          END. /*FOR EACH prmcondition*/

          IF prmCondition = "BEGIN" then do:             

              FOR EACH b-oe-bolh NO-LOCK WHERE b-oe-bolh.company = prmComp
                             AND b-oe-bolh.posted  = YES
                       USE-INDEX b-no:
            FOR EACH b-oe-boll NO-LOCK WHERE b-oe-boll.company = b-oe-bolh.company
                                AND b-oe-boll.bol-no  = b-oe-bolh.bol-no AND b-oe-boll.i-no BEGINS prmText:
                FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = b-oe-boll.company
                                    AND b-vend-whse-item.fg-item-no  = b-oe-boll.i-no NO-LOCK NO-ERROR.
                IF AVAILABLE b-vend-whse-item THEN DO:
                    CREATE ttVendorBolLookup.
                        ASSIGN            
                            ttVendorBolLookup.vBolNo = b-oe-boll.bol-no 
                            ttVendorBolLookup.vINo = b-oe-boll.i-no
                            ttVendorBolLookup.vCustNo = b-oe-boll.cust-no 
                            ttVendorBolLookup.vJobNo = b-oe-boll.job-no
                            ttVendorBolLookup.vJobNo2 = b-oe-boll.job-no2    
                            ttVendorBolLookup.vOrdNo = b-oe-boll.ord-no   
                            ttVendorBolLookup.vLine = b-oe-boll.line   
                            ttVendorBolLookup.vPoNo = b-oe-boll.po-no   
                            ttVendorBolLookup.vQty = b-oe-boll.qty    
                            ttVendorBolLookup.vVendorCode = CAPS(b-vend-whse-item.vendor-code)  
                            ttVendorBolLookup.vVendorDeptCode = CAPS(b-vend-whse-item.vendor-dept-code)
                            ttVendorBolLookup.vVendorPlantCode = CAPS(b-vend-whse-item.vendor-plant-code)
                            ttVendorBolLookup.vCustPartNo = CAPS(b-vend-whse-item.cust-part-no)   
                            ttVendorBolLookup.vTransDate = b-oe-boll.bol-date              
                            ttVendorBolLookup.vPlantTotOhQty = b-oe-boll.qty  
                            .
                END.

                FIND FIRST b-oe-ordl WHERE b-oe-ordl.company  EQ prmComp
                          AND b-oe-ordl.i-no     EQ ttVendorBolLookup.vINo
                          AND b-oe-ordl.job-no   EQ ttVendorBolLookup.vJobNo
                          AND b-oe-ordl.ord-no   EQ ttVendorBolLookup.vOrdNo
                          AND b-oe-ordl.line     EQ ttVendorBolLookup.vLine NO-LOCK NO-ERROR.
                IF AVAILABLE(b-oe-ordl) THEN
                    ttVendorBolLookup.vSellPrice = b-oe-ordl.t-price.

                END.
            END.


          END.    /*if prmCondition = BEGIN*/    
     END.  /* if prmField = fgitem  */
END.  /* IF prmAction = search then do: */


