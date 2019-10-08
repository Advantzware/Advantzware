
/*------------------------------------------------------------------------
    File        : BuildEstimate.p
    Purpose     : Processes the ttInputEst temp-table to create new estimates or to add forms to estimates

    Syntax      :

    Description : Estimate Builder Procedure.

    Author(s)   : BV
    Created     : Wed Jun 14 22:27:39 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcIndustry AS CHARACTER INIT 'C' NO-UNDO. /*C for corrugated, F for folding*/

{est\ttInputEst.i}

DEFINE VARIABLE riEb AS ROWID NO-UNDO.


/*Refactor - From B-estitm.w*/
DEF SHARED BUFFER xest FOR est.
DEF SHARED BUFFER xef  FOR ef.
DEF SHARED BUFFER xeb  FOR eb.
DEF SHARED BUFFER xqty FOR est-qty.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FOR EACH ttInputEst NO-LOCK:
    FIND FIRST est NO-LOCK 
        WHERE ROWID(est) EQ ttInputEst.riParentEst NO-ERROR.
    IF NOT AVAILABLE est THEN 
    DO:
        RUN est/NewEstimate.p ('C', 5, OUTPUT riEb).
    END.
    ELSE 
    DO:
        RUN est/NewEstimateForm.p ('C', ROWID(est), OUTPUT riEb).
    END.
    FIND eb 
        WHERE ROWID(eb) EQ riEb  
        NO-ERROR.
    FIND FIRST ef 
        WHERE ef.company EQ eb.company
        AND ef.est-no EQ eb.est-no
        AND ef.form-no EQ eb.form-no  
        NO-ERROR.
    FIND FIRST est 
        WHERE est.company EQ ef.company
        AND ef.est-no EQ eb.est-no  
        NO-ERROR.
    FIND est-qty 
        WHERE est-qty.company EQ ef.company
        AND est-qty.est-no EQ ef.est-no
        AND est-qty.eqty EQ ef.eqty 
        NO-ERROR.
        
     IF eb.eqty EQ 0 THEN 
        eb.eqty         = ttInputEst.iQuantity.
     ASSIGN 
        est-qty.eqty    = eb.eqty
        est.est-qty[1]  = eb.eqty
        ef.eqty         = eb.eqty
        .
        
    ASSIGN 
        eb.part-no      = ttInputEst.cPartID
        eb.part-dscr1   = ttInputEst.cPartName
        eb.part-dscr2   = ttInputEst.cPartDescription
        eb.style        = ttInputEst.cStyle
        eb.sman         = ttInputEst.cSalesManID
        eb.die-in       = ttInputEst.dDieInches
        eb.cad-no       = ttInputEst.cCadID
        eb.die-no       = ttInputEst.cDieID
        eb.num-up       = 1
        eb.num-wid      = 1
        eb.num-len      = 1
        eb.pur-man      = ttInputEst.lPurchased
        eb.spare-char-1 = ttInputEst.cDesigner
        eb.len          = ttInputEst.dLength
        eb.wid          = ttInputEst.dWidth
        eb.dep          = ttInputEst.dDepth
        eb.i-coldscr    = ttInputEst.cInkDescription
        eb.i-code       = ttInputEst.cInkCode
        eb.i-%          = ttInputEst.iInkCoverage
        ef.board        = ttInputEst.cBoard     
        ef.nc           = NOT ttInputEst.lPurchased   
        ef.blank-qty    = 1
        ef.trim-w       = ttInputEst.dWidthDie
        ef.trim-l       = ttInputEst.dLengthDie

        .
    IF ttInputEst.cCategory NE '' THEN 
        eb.procat       = ttInputEst.cCategory.
    IF ttInputEst.iQuantityYield GT 0 THEN 
        eb.yld-qty      = ttInputEst.iQuantityYield.
    ELSE 
        eb.yld-qty      = eb.eqty.
        
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ  eb.company 
        AND cust.cust-no EQ ttInputEst.cCustomer
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
            eb.ship-id = shipto.ship-id.
        ELSE 
            eb.ship-id = cust.cust-no.
    END.
    
    IF ttInputEst.cTab EQ '' THEN 
        eb.tab-in = YES.
    ELSE 
        eb.tab-in = ttInputEst.cTab EQ "In".
    
    RUN est/CalcBlankSize.p (ipcIndustry, ROWID(eb)).
   
    IF ttInputEst.dLengthBlank GT 0 THEN 
        eb.t-len = ttInputEst.dLengthBlank.
    IF ttInputEst.dWidthBlank GT 0 THEN 
        eb.t-wid = ttInputEst.dWidthBlank.    
    
    RUN pAssignInks (BUFFER eb).

   
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
    RUN est/CalcLayout.p (ipcIndustry,
                         ROWID(ef),
                         ROWID(eb),
                         YES,  /*New Layout vs. Recalculation*/
                         NO, /*Prompt to Reset*/
                         YES /*Recalc dimensions - Refactor - should be no if Style is foam*/).
        
    RUN est/BuildDefaultPreps.p (BUFFER est,
        BUFFER ef,
        INPUT eb.form-no,
        INPUT 0).


    RUN pCalcPacking(ROWID(eb)).
   
/*    REFACTOR ALL /* create set header record */                                                        */
/*    IF iArtiosCount > 1 THEN                                                              */
/*    DO:                                                                                   */
/*        FIND bf-eb WHERE bf-eb.company = est.company                                      */
/*            AND bf-eb.est-no = est.est-no                                                 */
/*            AND bf-eb.form-no = 0 NO-ERROR.                                               */
/*        IF NOT AVAILABLE bf-eb THEN CREATE bf-eb.                                         */
/*        ASSIGN                                                                            */
/*            bf-eb.company          = est.company                                          */
/*            bf-eb.est-no           = est.est-no                                           */
/*            bf-eb.form-no          = 0                                                    */
/*            bf-eb.part-no          = SUBSTRING(tt-artios.cadnum,1,6)                      */
/*            /*bf-eb.part-dscr1 =*/                                                        */
/*            bf-eb.procat           = tt-artios.procat                                     */
/*            bf-eb.est-type         = est.est-type                                         */
/*            bf-eb.eqty             = ef.eqty                                              */
/*            bf-eb.blank-no         = 0                                                    */
/*            bf-eb.set-is-assembled = IF v-alloc THEN NO /*assembled */                    */
/*                                           ELSE IF NOT v-alloc THEN YES  /* Unassembled */*/
/*                                           ELSE ?                                         */
/*            bf-eb.pur-man          = IF v-alloc THEN NO ELSE YES                          */
/*            .                                                                             */
/*                                                                                          */
/*    END.                                                                                  */

/*    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).*/
/*    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN                                             */
/*        RUN init-box-design IN WIDGET-HANDLE(char-hdl) (THIS-PROCEDURE).                      */
/*                                                                                              */
/*    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"box-calc-target",OUTPUT char-hdl). */
/*    IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(1,char-hdl))) THEN                                    */
/*        RUN build-box IN WIDGET-HANDLE(ENTRY(1,char-hdl)) ("B").                              */
/*                                                                                              */
/*    FIND FIRST box-design-hdr WHERE                                                           */
/*        box-design-hdr.design-no EQ 0 AND                                                     */
/*        box-design-hdr.company EQ cocode AND                                                  */
/*        box-design-hdr.est-no EQ est.est-no AND                                               */
/*        box-design-hdr.form-no EQ tt-artios.form-num AND                                      */
/*        box-design-hdr.blank-no EQ tt-artios.blank-num                                        */
/*        NO-ERROR.                                                                             */
/*                                                                                              */
/*    IF AVAILABLE box-design-hdr THEN                                                          */
/*    DO:                                                                                       */
/*        box-design-hdr.box-image = tt-artios.DesignImg.                                       */
/*        FIND CURRENT box-design-hdr NO-LOCK NO-ERROR.                                         */
/*    END.                                                                                      */
    
        
END. /*each ttInputEst*/



/* **********************  Internal Procedures  *********************** */

PROCEDURE pAssignInks:
    /*------------------------------------------------------------------------------
     Purpose: Assigns input ink arrays to eb 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    
    DEFINE VARIABLE iIndex AS INTEGER.
    DEFINE VARIABLE iCount AS INTEGER.
    
    DO iIndex = 1 TO EXTENT(ttInputEst.cInkCode):
        IF ttInputEst.cInkCode[iIndex] NE '' THEN 
        DO: 
            ASSIGN 
                iCount            = iCount + 1
                eb.i-code[iCount] = ttInputEst.cInkCode[iIndex]
                eb.i-%[iCount]    = ttInputEst.iInkCoverage[iIndex]
                .
        END.
    END.
    IF iCount > 0 THEN 
        eb.i-col = iCount.
    ELSE 
        eb.i-col = ttInputEst.iCountColors.
        
END PROCEDURE.

PROCEDURE pCalcPacking:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.

    DEFINE VARIABLE dUnitsPerPallet AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCountOnPallet  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iLayers         AS INTEGER NO-UNDO.
    DEFINE VARIABLE iStacks         AS INTEGER NO-UNDO.
    DEFINE VARIABLE cStackCode    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError        AS LOGICAL NO-UNDO.
    
    FIND eb WHERE ROWID(eb) EQ ipriEb.
    
    IF eb.tr-cnt = 0 THEN 
        eb.tr-cnt = eb.cas-cnt * eb.cas-pal.

    IF eb.stock-no = "" THEN 
    DO:
        FIND FIRST ce-ctrl NO-LOCK  
            WHERE ce-ctrl.company EQ eb.company 
            AND ce-ctrl.loc EQ eb.loc
            NO-ERROR.
        IF AVAILABLE ce-ctrl THEN 
            ASSIGN 
                eb.cas-no = ce-ctrl.def-case
                eb.tr-no  = ce-ctrl.def-pal.      
    END.
    FIND FIRST cust NO-LOCK  
        WHERE cust.company EQ eb.company 
        AND cust.cust-no EQ eb.cust-no
        NO-ERROR.
    IF AVAILABLE cust THEN DO: 
        ASSIGN
            eb.cas-no = IF cust.case-bundle NE '' THEN cust.case-bundle ELSE eb.cas-no 
            eb.tr-no  = IF cust.pallet NE '' THEN cust.pallet ELSE eb.tr-no
            .
        FIND FIRST shipto NO-LOCK 
            WHERE shipto.company EQ cust.company
            AND shipto.ship-id EQ eb.ship-id
            NO-ERROR.
        IF AVAILABLE shipto AND shipto.pallet NE '' THEN 
            eb.tr-no = shipto.pallet. 
    END.         
    FIND item NO-LOCK 
        WHERE item.company EQ eb.company 
        AND item.i-no EQ eb.cas-no
        NO-ERROR.
    IF AVAILABLE item THEN 
        ASSIGN 
            eb.cas-cnt = (item.box-case)
            eb.cas-len = (item.case-l)
            eb.cas-wid = (item.case-w)
            eb.cas-dep = (item.case-d)
            eb.cas-pal = (item.case-pall)
            eb.cas-wt  = (item.avg-w)         
            .
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
            eb.cas-pal    = dUnitsPerPallet
            eb.tr-cnt     = iCountOnPallet
            eb.tr-cas     = iLayers
            eb.stacks     = iStacks
            eb.stack-code = cStackCode.
    END.


END PROCEDURE.

