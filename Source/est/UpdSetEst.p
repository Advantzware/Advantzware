
/*------------------------------------------------------------------------
    File        : est/UpdSetEst.p
    Purpose     : Processes the ttInputEst temp-table to create new estimates or to add forms to estimates

    Syntax      :

    Description : Estimate Misc Procedure.

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipirRowid AS ROWID  NO-UNDO. /* eb rowid */ 
{est\ttInputEst.i}

DEFINE VARIABLE riEb AS ROWID NO-UNDO.


/*Refactor - From B-estitm.w*/
DEFINE SHARED BUFFER xest FOR est.
DEFINE SHARED BUFFER xef  FOR ef.
DEFINE SHARED BUFFER xeb  FOR eb.
DEFINE SHARED BUFFER xqty FOR est-qty.
DEFINE SHARED TEMP-TABLE tt-eb-set NO-UNDO LIKE eb. 
DEFINE BUFFER bf-existing-eb FOR eb.
DEFINE BUFFER bf-eb          FOR eb.
DEFINE BUFFER bf-del-eb      FOR eb.
DEFINE BUFFER bf-ef          FOR ef.
DEFINE BUFFER bf-est         FOR est.
DEFINE BUFFER bf-set-eb      FOR eb.
DEFINE VARIABLE rwRowid AS ROWID NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND FIRST bf-eb NO-LOCK
    WHERE ROWID(bf-eb) EQ ipirRowid NO-ERROR.
FIND FIRST bf-est 
    WHERE bf-est.company EQ bf-eb.company
    AND bf-est.est-no EQ bf-eb.est-no  
    NO-ERROR.     
      
FOR EACH eb  EXCLUSIVE-LOCK
    WHERE eb.company  EQ bf-eb.company
    AND eb.est-no EQ bf-eb.est-no
    AND eb.form-no NE 0 :
        
    FIND FIRST ttInputEst NO-LOCK
        WHERE ttInputEst.cCompany EQ eb.company
        AND ttInputEst.iFormNo EQ eb.form-no 
        AND ttInputEst.iBlankNo EQ eb.blank-no NO-ERROR.
    IF NOT AVAILABLE ttInputEst THEN
    DO:
            
        FIND FIRST bf-del-eb NO-LOCK
            WHERE bf-del-eb.company EQ eb.company 
            AND bf-del-eb.form-no EQ eb.form-no 
            AND bf-del-eb.blank-no NE eb.blank-no NO-ERROR.
        IF NOT AVAILABLE bf-del-eb THEN
        DO:
            FIND FIRST ef EXCLUSIVE-LOCK
                WHERE ef.company EQ eb.company
                AND ef.est-no EQ eb.est-no 
                AND ef.form-no EQ eb.form-no NO-ERROR.
            IF AVAILABLE ef THEN  
                DELETE ef.                
        END.
                
        DELETE eb.       
    END.           
END. 
 
FIND FIRST bf-set-eb EXCLUSIVE-LOCK
    WHERE bf-set-eb.company EQ bf-est.company
    AND bf-set-eb.est-no EQ bf-est.est-no
    AND bf-set-eb.form-no EQ 0 NO-ERROR.
IF AVAILABLE bf-set-eb THEN
DO:
    FIND FIRST tt-eb-set NO-LOCK NO-ERROR.
    IF AVAILABLE tt-eb-set THEN 
    DO:
        ASSIGN
            bf-set-eb.part-no          = tt-eb-set.part-no          
            bf-set-eb.stock-no         = tt-eb-set.stock-no      
            bf-set-eb.part-dscr1       = tt-eb-set.part-dscr1     
            bf-set-eb.part-dscr2       = tt-eb-set.part-dscr2   
            bf-set-eb.cust-no          = tt-eb-set.cust-no         
            bf-set-eb.ship-id          = tt-eb-set.ship-id                 
            bf-set-eb.len              = tt-eb-set.len           
            bf-set-eb.wid              = tt-eb-set.wid            
            bf-set-eb.dep              = tt-eb-set.dep                
            bf-set-eb.procat           = tt-eb-set.procat                 
            bf-set-eb.set-is-assembled = tt-eb-set.set-is-assembled 
            /*tt-eb-set.pur-man           = TRUE */  .
        FIND FIRST itemfg
            WHERE itemfg.company EQ bf-set-eb.company
            AND itemfg.i-no    EQ bf-set-eb.stock-no
            NO-ERROR.
        IF AVAILABLE itemfg THEN
            ASSIGN
                itemfg.alloc  = bf-set-eb.set-is-assembled
                itemfg.procat = bf-set-eb.procat.
    END.      
    RELEASE itemfg . 
    EMPTY TEMP-TABLE tt-eb-set .     
END. 
      
        
FOR EACH ttInputEst NO-LOCK BREAK BY ttInputEst.iFormNo 
    BY ttInputEst.iBlankNo:
    
    FIND FIRST eb 
        WHERE eb.company  EQ bf-est.company
        AND eb.est-no EQ bf-est.est-no
        AND eb.form-no EQ ttInputEst.iFormNo  
        AND eb.blank-no EQ ttInputEst.iBlankNo 
        NO-ERROR.     
    IF NOT AVAILABLE eb THEN
    DO:
        FIND FIRST bf-ef NO-LOCK 
            WHERE bf-ef.company EQ bf-est.company
            AND bf-ef.est-no EQ bf-est.est-no
            AND bf-ef.form-no EQ ttInputEst.iFormNo NO-ERROR.
        IF NOT AVAILABLE bf-ef THEN
        DO:
            FIND FIRST est NO-LOCK
                WHERE est.company EQ bf-est.company 
                AND est.est-no EQ bf-est.est-no NO-ERROR.
              
            RUN est/NewEstimateForm.p ('C', ROWID(est), OUTPUT rwRowid). 
            FIND FIRST eb 
                WHERE ROWID(eb) EQ rwRowid  
                NO-ERROR.            
        END.
        ELSE 
        DO: 
            RUN cec/newblank.p (ROWID(bf-ef), OUTPUT rwRowid).
            FIND FIRST eb 
                WHERE ROWID(eb) EQ rwRowid  
                NO-ERROR. 
        END.
       
    END.
        
        
    FIND FIRST ef 
        WHERE ef.company EQ eb.company
        AND ef.est-no EQ eb.est-no
        AND ef.form-no EQ eb.form-no  
        NO-ERROR.
    FIND FIRST est 
        WHERE est.company EQ ef.company
        AND est.est-no EQ eb.est-no  
        NO-ERROR.
    FIND est-qty 
        WHERE est-qty.company EQ ef.company
        AND est-qty.est-no EQ ef.est-no
        AND est-qty.eqty EQ ef.eqty 
        NO-ERROR.
        

    ASSIGN 
        est-qty.eqty   = ttInputEst.iQuantity
        est-qty.qty[1] = ttInputEst.iQuantity
        est.est-qty[1] = ttInputEst.iQuantity
        ef.eqty        = ttInputEst.iQuantity
        eb.eqty        = ttInputEst.iQuantity   
        .
          
    ASSIGN 
        eb.part-no         = ttInputEst.cPartID
        eb.part-dscr1      = ttInputEst.cPartName
        eb.part-dscr2      = ttInputEst.cPartDescription
        eb.style           = ttInputEst.cStyle
        eb.pur-man         = ttInputEst.lPurchased
        eb.len             = ttInputEst.dLength
        eb.wid             = ttInputEst.dWidth
        eb.dep             = ttInputEst.dDepth
        ef.board           = ttInputEst.cBoard 
        eb.stock-no        = ttInputEst.cStockNo
        eb.cas-cnt         = ttInputEst.iUnitCount
        eb.cas-pal         = ttInputEst.iPerPallet
        eb.tr-no           = ttInputEst.cPallet
        eb.cas-no          = ttInputEst.cBndlCode
        eb.weight          = ttInputEst.dWeightPerM
        eb.stackHeight     = ttInputEst.iStackHeight
        eb.quantityPartial = ttInputEst.iPartial
        eb.tr-cnt          = eb.cas-cnt * eb.cas-pal + eb.quantityPartial
        eb.quantityPerSet  = ttInputEst.dQtyPerSet
        .

    IF AVAILABLE est-qty THEN 
    DO:
        ASSIGN 
            est-qty.eqty   = ttInputEst.iQuantity
            est-qty.qty[1] = ttInputEst.iQuantity .
        est-qty.qty[21] = ttInputEst.copy-rel[1] .
        IF ttInputEst.copy-runship[1] EQ "Yes" OR ttInputEst.copy-runship[1] EQ "No" THEN 
            est-qty.whsed[1] = LOGICAL(ttInputEst.copy-runship[1]) NO-ERROR .
        ASSIGN
            est-qty.qty[2]    = ttInputEst.copy-qty[2] 
            est-qty.qty[22]   = ttInputEst.copy-rel[2]
            est-qty.whsed[2]  = LOGICAL(ttInputEst.copy-runship[2])          
            est-qty.qty[3]    = ttInputEst.copy-qty[3]
            est-qty.qty[23]   = ttInputEst.copy-rel[3]
            est-qty.whsed[3]  = LOGICAL(ttInputEst.copy-runship[3])         
            est-qty.qty[4]    = ttInputEst.copy-qty[4] 
            est-qty.qty[24]   = ttInputEst.copy-rel[4]
            est-qty.whsed[4]  = LOGICAL(ttInputEst.copy-runship[4])          
            est-qty.qty[5]    = ttInputEst.copy-qty[5]
            est-qty.qty[25]   = ttInputEst.copy-rel[5] 
            est-qty.whsed[5]  = LOGICAL(ttInputEst.copy-runship[5])          
            est-qty.qty[6]    = ttInputEst.copy-qty[6] 
            est-qty.qty[26]   = ttInputEst.copy-rel[6]
            est-qty.whsed[6]  = LOGICAL(ttInputEst.copy-runship[6])          
            est-qty.qty[7]    = ttInputEst.copy-qty[7] 
            est-qty.qty[27]   = ttInputEst.copy-rel[7]
            est-qty.whsed[7]  = LOGICAL(ttInputEst.copy-runship[7])          
            est-qty.qty[8]    = ttInputEst.copy-qty[8] 
            est-qty.qty[28]   = ttInputEst.copy-rel[8]
            est-qty.whsed[8]  = LOGICAL(ttInputEst.copy-runship[8])          
            est-qty.qty[9]    = ttInputEst.copy-qty[9] 
            est-qty.qty[29]   = ttInputEst.copy-rel[9]
            est-qty.whsed[9]  = LOGICAL(ttInputEst.copy-runship[9])          
            est-qty.qty[10]   = ttInputEst.copy-qty[10] 
            est-qty.qty[30]   = ttInputEst.copy-rel[10]
            est-qty.whsed[10] = LOGICAL(ttInputEst.copy-runship[10])          
            est-qty.qty[11]   = ttInputEst.copy-qty[11] 
            est-qty.qty[31]   = ttInputEst.copy-rel[11]
            est-qty.whsed[11] = LOGICAL(ttInputEst.copy-runship[11])          
            est-qty.qty[12]   = ttInputEst.copy-qty[12] 
            est-qty.qty[32]   = ttInputEst.copy-rel[12]
            est-qty.whsed[12] = LOGICAL(ttInputEst.copy-runship[12])          
            est-qty.qty[13]   = ttInputEst.copy-qty[13] 
            est-qty.qty[33]   = ttInputEst.copy-rel[13]
            est-qty.whsed[13] = LOGICAL(ttInputEst.copy-runship[13])          
            est-qty.qty[14]   = ttInputEst.copy-qty[14] 
            est-qty.qty[34]   = ttInputEst.copy-rel[14]
            est-qty.whsed[14] = LOGICAL(ttInputEst.copy-runship[14])          
            est-qty.qty[15]   = ttInputEst.copy-qty[15] 
            est-qty.qty[35]   = ttInputEst.copy-rel[15]
            est-qty.whsed[15] = LOGICAL(ttInputEst.copy-runship[15])          
            est-qty.qty[16]   = ttInputEst.copy-qty[16]
            est-qty.qty[36]   = ttInputEst.copy-rel[16] 
            est-qty.whsed[16] = LOGICAL(ttInputEst.copy-runship[16])          
            est-qty.qty[17]   = ttInputEst.copy-qty[17] 
            est-qty.qty[37]   = ttInputEst.copy-rel[17]
            est-qty.whsed[17] = LOGICAL(ttInputEst.copy-runship[17])          
            est-qty.qty[18]   = ttInputEst.copy-qty[18] 
            est-qty.qty[38]   = ttInputEst.copy-rel[18]
            est-qty.whsed[18] = LOGICAL(ttInputEst.copy-runship[18])          
            est-qty.qty[19]   = ttInputEst.copy-qty[19] 
            est-qty.qty[39]   = ttInputEst.copy-rel[19]
            est-qty.whsed[19] = LOGICAL(ttInputEst.copy-runship[19])          
            est-qty.qty[20]   = ttInputEst.copy-qty[20] 
            est-qty.qty[40]   = ttInputEst.copy-rel[20]
            est-qty.whsed[20] = LOGICAL(ttInputEst.copy-runship[20]).
    END.
    

    /*IF NOT CAN-FIND(FIRST itemfg                                          */
    /*            WHERE itemfg.company EQ eb.company                        */
    /*              AND itemfg.i-no    EQ eb.stock-no) THEN DO:             */
    /*    FIND FIRST xeb WHERE ROWID(xeb) EQ ROWID(eb) NO-LOCK NO-ERROR.    */
    /*    FIND FIRST xest WHERE ROWID(xest) EQ ROWID(est) NO-LOCK NO-ERROR. */
    /*    RUN fg/ce-addfg.p (xeb.stock-no).                                 */
    /* END.                                                                 */
    

    IF eb.sman NE "" AND eb.comm EQ 0 THEN 
    DO:
        FIND FIRST sman NO-LOCK 
            WHERE sman.company EQ eb.company
            AND sman.sman EQ eb.sman
            NO-ERROR.
        IF AVAILABLE sman THEN 
            eb.comm = sman.scomm.
    END.
   
    IF ttInputEst.cCategory NE '' THEN 
        eb.procat       = ttInputEst.cCategory.
    IF ttInputEst.iQuantityYield GT 0 THEN 
        eb.yld-qty      = ttInputEst.iQuantityYield.
    ELSE 
        eb.yld-qty      = eb.eqty.
        
    IF ttInputEst.cCustomer NE "" THEN 
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ  eb.company 
            AND cust.cust-no EQ ttInputEst.cCustomer
            NO-ERROR.
    IF NOT AVAILABLE cust AND AVAILABLE bf-existing-eb THEN 
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ  eb.company 
            AND cust.cust-no EQ bf-existing-eb.cust-no
            NO-ERROR.
    IF AVAILABLE cust THEN 
    DO: 
        eb.cust-no = cust.cust-no.
        IF ttInputEst.cShipTo NE '' THEN 
            FIND FIRST shipto NO-LOCK 
                WHERE shipto.company EQ cust.company
                AND shipto.cust-no EQ cust.cust-no
                AND shipto.ship-id EQ ttInputEst.cShipTo
                NO-ERROR .
        IF NOT AVAILABLE shipto THEN 
            FIND FIRST shipto NO-LOCK 
                WHERE shipto.company EQ cust.company
                AND shipto.cust-no EQ cust.cust-no
                AND shipto.ship-id EQ cust.cust-no
                NO-ERROR.
        IF NOT AVAILABLE shipto THEN 
            FIND FIRST shipto OF cust NO-LOCK NO-ERROR.
        IF AVAILABLE shipto THEN
            ASSIGN 
                eb.ship-id   = shipto.ship-id
                eb.carrier   = shipto.carrier
                eb.dest-code = shipto.dest-code
                .
            
        ELSE 
            ASSIGN 
                eb.ship-id   = cust.cust-no
                eb.carrier   = cust.carrier
                eb.dest-code = cust.del-zone.
    END.
    
    IF ttInputEst.cTab EQ '' THEN 
        eb.tab-in = YES.
    ELSE 
        eb.tab-in = ttInputEst.cTab EQ "In".
   
    IF ttInputEst.dLengthBlank GT 0 THEN 
        eb.t-len = ttInputEst.dLengthBlank.
    IF ttInputEst.dWidthBlank GT 0 THEN 
        eb.t-wid = ttInputEst.dWidthBlank.  
   
   
    IF ef.board EQ '' THEN 
    DO:
        FIND FIRST ITEM NO-LOCK 
            WHERE item.company EQ ef.company
            AND  item.flute EQ ttInputEst.cFlute
            AND item.reg-no EQ ttInputEst.cTest
            NO-ERROR.
        IF AVAILABLE ITEM THEN 
            ef.board = item.i-no.
    END.
    FIND FIRST item NO-LOCK 
        WHERE item.company EQ ef.company 
        AND item.i-no  EQ ef.board
        NO-ERROR.
    IF AVAILABLE item THEN
        ASSIGN 
            ef.board = item.i-no
            ef.cal   = item.cal
            eb.flute = item.flute
            eb.test  = item.reg-no
            .
    RUN est/CalcLayout.p ("C",
        ROWID(ef),
        ROWID(eb),
        YES,  /*New Layout vs. Recalculation*/
        NO, /*Prompt to Reset*/
        YES /*Recalc dimensions - Refactor - should be no if Style is foam*/).
    RUN pCalcPacking(ROWID(eb)).
    IF ttInputEst.iStackCode NE "" THEN
        eb.stack-code = ttInputEst.iStackCode .
        
END. /*each ttInputEst*/
RELEASE eb.
RELEASE ef.
RELEASE est-qty.
RELEASE est.
RELEASE bf-set-eb.


/* **********************  Internal Procedures  *********************** */


PROCEDURE pCalcPacking:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.

    DEFINE VARIABLE dUnitsPerPallet AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iCountOnPallet  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLayers         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iStacks         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cStackCode      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPackCodeStyle  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPackCode       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPalletCode     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError          AS LOGICAL   NO-UNDO.
    
    FIND eb WHERE ROWID(eb) EQ ipriEb.
    
    IF eb.cas-no EQ "" OR eb.tr-no EQ "" THEN /*Fill in case and pallet if not already set*/ 
    DO:
        FIND FIRST ce-ctrl NO-LOCK  
            WHERE ce-ctrl.company EQ eb.company 
            AND ce-ctrl.loc EQ eb.loc
            NO-ERROR.
        IF AVAILABLE ce-ctrl THEN 
            ASSIGN 
                cPackCode   = ce-ctrl.def-case
                cPalletCode = ce-ctrl.def-pal.      
        FIND FIRST cust NO-LOCK  
            WHERE cust.company EQ eb.company 
            AND cust.cust-no EQ eb.cust-no
            NO-ERROR.
        IF AVAILABLE cust THEN 
        DO: 
            ASSIGN
                cPackCode   = IF cust.case-bundle NE '' THEN cust.case-bundle ELSE eb.cas-no 
                cPalletCode = IF cust.pallet NE '' THEN cust.pallet ELSE eb.tr-no
                .
            RUN est/packCodeOverride.p (eb.company, eb.cust-no, eb.style, OUTPUT cPackCodeStyle).
            IF cPackCodeStyle NE '' THEN cPackCode = cPackCodeStyle.
            FIND FIRST shipto NO-LOCK 
                WHERE shipto.company EQ cust.company
                AND shipto.cust-no EQ cust.cust-no
                AND shipto.ship-id EQ eb.ship-id
                NO-ERROR.
            IF AVAILABLE shipto AND shipto.pallet NE '' THEN 
                cPalletCode = shipto.pallet.
        END.    
        IF eb.cas-no EQ "" THEN eb.cas-no = cPackCode.
        IF eb.tr-no EQ "" THEN eb.tr-no = cPalletCode. 
    END.         
    
    FIND item NO-LOCK 
        WHERE item.company EQ eb.company 
        AND item.i-no EQ eb.cas-no
        NO-ERROR.
    IF AVAILABLE item THEN 
    DO: 
        ASSIGN 
            eb.cas-len = (item.case-l)
            eb.cas-wid = (item.case-w)
            eb.cas-dep = (item.case-d)
            eb.cas-wt  = (item.avg-w)
            .
        IF eb.cas-pal EQ 0 THEN eb.cas-pal = (item.case-pall).
        IF eb.cas-cnt EQ 0 THEN eb.cas-cnt = (item.box-case).         
    END.
    FIND FIRST item NO-LOCK 
        WHERE item.company EQ eb.company 
        AND  item.i-no EQ eb.tr-no
        NO-ERROR.
    IF AVAILABLE item THEN 
        ASSIGN 
            eb.tr-len = (item.case-l)
            eb.tr-wid = (item.case-w)
            eb.tr-dep = (item.case-d)
            .
    
    RUN cec/kpallet.p (RECID(eb), 
        OUTPUT dUnitsPerPallet, 
        OUTPUT iCountOnPallet,
        OUTPUT iStacks, 
        OUTPUT cStackCode, 
        OUTPUT lError).

    IF NOT lError THEN 
    DO:
        iLayers = dUnitsPerPallet / iStacks.
        {sys/inc/roundup.i iLayers}

        ASSIGN
            eb.stacks     = iStacks
            eb.stack-code = cStackCode
            .
        IF eb.tr-cas  EQ 0 THEN eb.tr-cas  = iLayers .
        IF eb.cas-pal EQ 0 THEN eb.cas-pal = dUnitsPerPallet.
        IF eb.tr-cnt  EQ 0 THEN eb.tr-cnt = iCountOnPallet.
    END.
    IF eb.tr-cnt EQ 0 THEN 
        eb.tr-cnt = eb.cas-cnt * eb.cas-pal + eb.quantityPartial.

END PROCEDURE.



