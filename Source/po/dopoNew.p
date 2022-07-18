/*------------------------------------------------------------------------
    File        : po/dopo.p 
    Purpose     :
                 Add PO from Order Entry Program - P/O Module                          m.    
                 Similar logic in dopo-best.p                                              
    Syntax      :

    Description :

    Author(s)   : Varun 
    Created     : 04/18/2022
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
USING system.SharedConfig.

DEFINE INPUT  PARAMETER reORDRowId      AS ROWID       NO-UNDO.

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE debasis-w           LIKE ITEM.basis-w NO-UNDO.
DEFINE VARIABLE deS-Len             LIKE ITEM.s-len NO-UNDO.
DEFINE VARIABLE deS-wid             LIKE ITEM.s-wid NO-UNDO.
DEFINE VARIABLE deS-dep             LIKE ITEM.s-dep NO-UNDO.
DEFINE VARIABLE chGl-Desc           AS CHARACTER NO-UNDO.
DEFINE VARIABLE deTot-msf           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deAdder             AS DECIMAL   NO-UNDO EXTENT 2.
DEFINE VARIABLE chPartDescr1        AS CHARACTER NO-UNDO FORMAT "x(30)".
DEFINE VARIABLE chPartDescr2        AS CHARACTER NO-UNDO FORMAT "x(30)".
DEFINE VARIABLE loCountUpdate       AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE cocode              AS CHARACTER NO-UNDO.
DEFINE VARIABLE loAccessClose       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chAccessList        AS CHARACTER NO-UNDO.
DEFINE VARIABLE deqty               AS DECIMAL   NO-UNDO.
DEFINE VARIABLE decost              AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deItemCost          AS DECIMAL   NO-UNDO FORMAT ">>,>>9.9999" INIT 0.
DEFINE VARIABLE chVendNo            LIKE vend.vend-no INITIAL "" NO-UNDO.
DEFINE VARIABLE chVendItem          LIKE ITEM.vend-item NO-UNDO.
DEFINE VARIABLE deSetup             LIKE e-item-vend.setup NO-UNDO.
DEFINE VARIABLE loAutoPoSec         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE loAutoFgSec         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE loAutoPrepSec       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE inOeAutoPONK1       AS INTEGER   NO-UNDO.
DEFINE VARIABLE looeAutopoNK1       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE choeAutoPoNK1       AS CHARACTER FORMAT "x(8)" NO-UNDO.
DEFINE VARIABLE chLoc               AS CHARACTER NO-UNDO.
DEFINE VARIABLE looeAutoPrep        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE inOrderNo           LIKE oe-ordl.ord-no NO-UNDO.
DEFINE VARIABLE chNewItemNo         LIKE ITEM.i-no INITIAL "" NO-UNDO.
DEFINE VARIABLE chpoCost1           AS CHARACTER NO-UNDO.
DEFINE VARIABLE loHoldop1           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE loPoQty             AS LOGICAL   NO-UNDO INITIAL TRUE.

DEFINE VARIABLE deQtyComp           LIKE job-mat.qty NO-UNDO.
DEFINE VARIABLE deQtyComp1          LIKE job-mat.qty NO-UNDO.
DEFINE VARIABLE loNewAvail          AS LOGICAL   NO-UNDO INITIAL NO.
DEFINE VARIABLE chJobNumber         AS CHARACTER NO-UNDO.
DEFINE VARIABLE deJobMatQty         LIKE job-mat.qty NO-UNDO.
DEFINE VARIABLE chJobMatQtyUOM      LIKE job-mat.qty-uom NO-UNDO.
DEFINE VARIABLE chDropCustNo        AS CHARACTER NO-UNDO.
DEFINE VARIABLE chShipChoice        AS CHARACTER NO-UNDO.
DEFINE VARIABLE deDimCharge         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE reJobRowid          AS ROWID     NO-UNDO.
DEFINE VARIABLE chCharge            AS CHARACTER NO-UNDO.
DEFINE VARIABLE inIndex             AS INTEGER   NO-UNDO.
DEFINE VARIABLE loPOBest            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE loFromPoEntry       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE roPoOrdl            AS ROWID     NO-UNDO.
DEFINE VARIABLE roPoOrd             AS ROWID     NO-UNDO.
DEFINE VARIABLE roOeOrdl            AS ROWID     NO-UNDO.
DEFINE VARIABLE roOeOrd             AS ROWID     NO-UNDO.
DEFINE VARIABLE roJob               AS ROWID     NO-UNDO.
DEFINE VARIABLE roItemFG            AS ROWID     NO-UNDO.
DEFINE VARIABLE chFilIdSource       AS CHARACTER NO-UNDO.
DEFINE VARIABLE loNextOuters        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE deOeAutoFg          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE haPOProcs           AS HANDLE    NO-UNDO.
DEFINE VARIABLE scInstance          AS CLASS     system.SharedConfig NO-UNDO.
DEFINE VARIABLE loNewFile           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE loAPGL              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chAPGL              AS CHARACTER NO-UNDO.
DEFINE VARIABLE loOEAutoFg          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chOEAutoFG          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chAPTax             AS CHARACTER NO-UNDO.        
DEFINE VARIABLE chPOStatus          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chPOUom             AS CHARACTER NO-UNDO.
DEFINE VARIABLE inPOUom             AS INTEGER   NO-UNDO.
DEFINE VARIABLE loMSFcalc           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lNewVendorItemCost  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lIncludeBlankVendor AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE cMessage            AS CHARACTER NO-UNDO.
DEFINE VARIABLE loUpdatePO          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chMessage           AS CHARACTER NO-UNDO.
DEFINE VARIABLE loError             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hVendorCostProcs    AS HANDLE    NO-UNDO.
DEFINE VARIABLE chCompanyId         AS CHARACTER NO-UNDO.
    
DEFINE BUFFER b-item    FOR ITEM.
DEFINE BUFFER b-oe-ordl FOR oe-ordl.
DEFINE BUFFER bf-ordl   FOR oe-ordl.
DEFINE BUFFER bf-ord    FOR oe-ord.
DEFINE BUFFER b-jc-calc FOR reftable.
DEFINE BUFFER b-job-mat FOR job-mat.
DEFINE BUFFER b-job-hdr FOR job-hdr.
DEFINE BUFFER xest      FOR est.
DEFINE BUFFER xeb       FOR eb.

{system/VendorCostProcs.i}
{PO/itemToPurchase.i}

DEFINE TEMP-TABLE tt-fg-set LIKE fg-set
    FIELD isaset       LIKE itemfg.isaset
    FIELD alloc        LIKE itemfg.alloc
    FIELD part-qty-dec AS DECIMAL.
    
DEFINE TEMP-TABLE tt-fg-set2 LIKE tt-fg-set.

DEFINE TEMP-TABLE tt-itemfg NO-UNDO 
    FIELD isaset       LIKE itemfg.isaset
    FIELD isacomponent AS LOG 
    FIELD pur-man      LIKE itemfg.pur-man
    FIELD form-no      LIKE eb.form-no
    FIELD blank-no     LIKE eb.blank-no
    FIELD qty          LIKE oe-ordl.qty                                 
    FIELD pur-uom      LIKE itemfg.pur-uom
    FIELD row-id       AS ROWID.

DEFINE BUFFER b-tt-itemfg FOR tt-itemfg.

DEFINE TEMP-TABLE ttEstError
    FIELD estHeaderID AS INT64
    FIELD iFormNo     AS INTEGER
    FIELD iBlankNo    AS INTEGER
    FIELD cErrorType  AS CHARACTER
    FIELD cError      AS CHARACTER.
       
IF INDEX(PROGRAM-NAME(2),"add-po-best") GT 0 THEN
    loPOBest = YES.

IF INDEX(PROGRAM-NAME(2),"w-purord") GT 0
    OR INDEX(PROGRAM-NAME(3),"w-purord") GT 0
    OR INDEX(PROGRAM-NAME(4),"w-purord") GT 0 THEN
    ASSIGN loFromPoEntry = TRUE.

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no

/* ************************  Function Prototypes ********************** */

FUNCTION fGetVendCostQty RETURNS DECIMAL 
    (ipdQty AS DECIMAL, ipcFromUom AS CHARACTER, ipcToUom AS CHARACTER  ) FORWARD.
	
FUNCTION fnRoundUp RETURNS INTEGER(INPUT deNumber AS DECIMAL) FORWARD.

/* *********************** Procedure Settings ************************ */

/* *************************  Create Window  ************************** */ 

/* ***************************  Main Block  *************************** */

RUN pFindOrderFromRowID(INPUT reORDRowId, 
    OUTPUT roOeOrdl, 
    OUTPUT roJob,
    OUTPUT chFilIdSource,
    OUTPUT chCompanyId,
    OUTPUT chLoc).

IF chFilIdSource EQ "JOB" THEN
    RUN pItemFGFromJob (INPUT roJob, INPUT roOeOrdl, INPUT chFilIdSource).

FIND oe-ordl WHERE ROWID(oe-ordl) EQ roOeOrdl NO-LOCK NO-ERROR.
FIND oe-ord WHERE ROWID(oe-ord) EQ roOeOrd NO-LOCK NO-ERROR.
FIND job WHERE ROWID(job) EQ roJob NO-LOCK NO-ERROR.
FIND FIRST company NO-LOCK WHERE company.company EQ chCompanyId NO-ERROR.
RUN pSetGlobalSettings(INPUT chCompanyId).
/* Build ttItemTopurchase table */

RUN pBuildItemToPurchase(INPUT chOEAutoFG,
    INPUT roOeOrd,
    INPUT roOeOrdl,
    INPUT roJob,
    INPUT loPOBest).

RUN po/ItemToPurchase.w(INPUT-OUTPUT TABLE ttJobMaterial,
                        OUTPUT loUpdatePO) NO-ERROR.  
                        
IF NOT CAN-FIND (FIRST ttJobMaterial WHERE ttJobMaterial.createpo EQ TRUE) THEN 
DO:
    RETURN.    
END.

IF ERROR-STATUS:ERROR THEN
DO:    
    IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN
    MESSAGE ERROR-STATUS:GET-MESSAGE(1)
        VIEW-AS ALERT-BOX.
    RETURN.
END.  
                    
RUN system/VendorCostProcs.p PERSISTENT SET hVendorCostProcs.

RUN Po/POProcs.p PERSISTENT SET haPOProcs.

/* For each ttJobMaterial, process it */
RUN pProcessJobMat.

IF VALID-HANDLE(hVendorCostProcs) THEN
DO:
    DELETE PROCEDURE hVendorCostProcs.
END.

IF VALID-HANDLE(haPOProcs) THEN 
DO:
    DELETE PROCEDURE haPOProcs.
END.
    

                                                    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */ 
PROCEDURE pAddHeaderTot PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl FOR po-ordl.  

    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.    
    ASSIGN
        bf-po-ordl.dscr[1] = chPartDescr1
        bf-po-ordl.dscr[2] = chPartDescr2.
    RELEASE bf-po-ordl.

END PROCEDURE.
  
PROCEDURE pAutoRm PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        inputs:
          po-ordl
          job-mat
          ttJobMaterial (updated)
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprJobMat    AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprItem      AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl FOR po-ordl.  
    DEFINE BUFFER b-item     FOR ITEM.
    DEFINE BUFFER xjob-mat   FOR job-mat.

    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.    
 
    IF AVAILABLE bf-po-ordl AND choeAutoPoNK1 EQ "AutoRM" AND loAutoPoSec THEN 
    DO:
        FIND FIRST b-item
            WHERE b-item.company EQ ipchCompany
            AND b-item.i-no    EQ chNewItemNo
            AND b-item.i-no    NE ""
            NO-LOCK NO-ERROR.
            
        IF AVAILABLE b-item THEN bf-po-ordl.i-no = chNewItemNo.
        FIND FIRST xjob-mat NO-LOCK WHERE RowID(xjob-mat) EQ RowID(job-mat) NO-ERROR.
        IF AVAILABLE xjob-mat THEN ttJobMaterial.rm-i-no = chNewItemNo.

    END. /* choeAutoPoNK1 eq "AutoRM" ... */ 
    RELEASE bf-po-ordl.
END PROCEDURE.
 
PROCEDURE pBrdLenCheck PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl FOR po-ordl.
      
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl NO-LOCK NO-ERROR.

    IF deS-Len               EQ 0                       AND
        bf-po-ordl.cons-uom    EQ "EA"                    AND
        (bf-po-ordl.item-type OR
        NOT DYNAMIC-FUNCTION("Conv_IsEAUOM", bf-po-ordl.company, bf-po-ordl.i-no, bf-po-ordl.pr-qty-uom) OR
        NOT DYNAMIC-FUNCTION("Conv_IsEAUOM", bf-po-ordl.company, bf-po-ordl.i-no, bf-po-ordl.pr-uom))   AND
        (bf-po-ordl.pr-qty-uom NE bf-po-ordl.cons-uom OR
        bf-po-ordl.pr-uom     NE bf-po-ordl.cons-uom)        THEN 
    DO:
        RUN pAddError(INPUT "Length must be entered", INPUT "3", INPUT 0, INPUT 0, INPUT 0). 
    END. /* if deS-Len eq 0 ... */
    RELEASE bf-po-ordl.
    
END PROCEDURE.
    
PROCEDURE pCalculateVendorCost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: po detail line - vendor cost       
     Notes: Copy from po/po-vendc.i
    ------------------------------------------------------------------------------*/    
       
    DEFINE VARIABLE iCount5 AS INTEGER NO-UNDO.    
    
    deCost = po-ordl.cost.
    /*
    IF po-ordl.pr-qty-uom NE po-ordl.pr-uom THEN
        RUN Conv_QuantityFromUOMToUOM(po-ordl.company,
            po-ordl.i-no,
            po-ordl.item-type,
            po-ordl.ord-qty,
            po-ordl.pr-qty-uom,
            po-ordl.pr-uom,                 
            debasis-w,
            deS-Len,
            deS-wid, 
            deS-dep,
            0, 
            OUTPUT deqty,
            OUTPUT loError,
            OUTPUT chMessage).
    */
    
    deSetup = 0.
        
    IF loHoldop1 THEN 
    DO:
        /*
        IF po-ordl.pr-qty-uom NE po-ordl.pr-uom THEN
            RUN Conv_QuantityFromUOMToUOM(po-ordl.company,
                po-ordl.i-no,
                po-ordl.Item-Type,
                deCost,
                po-ordl.pr-qty-uom,
                po-ordl.pr-uom,
                debasis-w,
                deS-Len,
                deS-wid,
                deS-dep,
                0, 
                OUTPUT deCost,
                OUTPUT loError,
                OUTPUT chMessage).
        */
        deCost = deCost + deAdder[1].

        IF po-ordl.cost GT deCost THEN po-ord.stat = "H".
    END.

    IF po-ordl.pr-qty-uom EQ po-ordl.cons-uom THEN
        po-ordl.cons-cost = po-ordl.cost.
    ELSE
        RUN Conv_QuantityFromUOMToUOM(po-ordl.Company,
            po-ordl.i-no,
            po-ordl.Item-Type,
            po-ordl.cost,
            po-ordl.pr-qty-uom,
            po-ordl.pr-uom, 
            debasis-w,
            deS-Len,
            deS-wid,
            deS-dep,
            0, 
            OUTPUT po-ordl.cons-cost,
            OUTPUT loError,
            OUTPUT chMessage).

    ASSIGN
        po-ordl.cost      = po-ordl.cost      + deAdder[1]
        po-ordl.cons-cost = po-ordl.cons-cost + deAdder[2].
END PROCEDURE.

PROCEDURE pGetBestVendorWithCost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprottJobMaterial AS ROWID      NO-UNDO.
    DEFINE OUTPUT PARAMETER opchVendorId      AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER opdeCostPerUOM    AS DECIMAL    NO-UNDO.
    DEFINE OUTPUT PARAMETER opchCostUOM       AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER opdeCostSetup     AS DECIMAL    NO-UNDO.
    DEFINE OUTPUT PARAMETER opdeCostTotal     AS DECIMAL    NO-UNDO.    
    
    DEFINE VARIABLE deBestCostDeviation AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE oplError            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE opcMessage          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE gcScopeRMOverride   AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - RM Override".
    DEFINE VARIABLE gcScopeFGEstimated  AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - FG Estimated".
    
    FIND FIRST ttJobMaterial WHERE ROWID(ttJobMaterial) = iprottJobMaterial NO-ERROR.
    
    RUN VendCost_GetBestCost(
        INPUT  ttJobMaterial.company,    //ipcCompany ,
        INPUT  ttJobMaterial.i-no,
        INPUT  IF this-is-a-rm THEN "RM" ELSE "FG",   //ipcItemType ,
        INPUT  IF this-is-a-rm NE TRUE THEN gcScopeFGEstimated ELSE gcScopeRMOverride, //ipcScope ,
        INPUT  lIncludeBlankVendor,                           //iplIncludeBlankVendor ,
        INPUT  ttJobMaterial.est-no,           //ipcEstimateNo,
        INPUT  ttJobMaterial.frm,              //ipifrm,
        INPUT  ttJobMaterial.blank-no,         //ipiblank-no,
        INPUT  ttJobMaterial.qty,              //ipdqty,
        INPUT  ttJobMaterial.qty-uom,          //ipcqty-uom ,
        INPUT  ttJobMaterial.Len,              //ipdDimLength,
        INPUT  ttJobMaterial.Wid,              //ipdDimWidth,
        INPUT  ttJobMaterial.Dep,              //ipdDimDepth,
        INPUT  ttJobMaterial.sc-UOM,           //ipcDimUOM ,
        INPUT  ttJobMaterial.basis-w,          //ipdBasisWeight ,
        INPUT  "",                             //ipcBasisWeightUOM ,
        OUTPUT opdeCostPerUOM,
        OUTPUT opchCostUOM,
        OUTPUT opdeCostSetup,
        OUTPUT opchVendorId,
        OUTPUT deBestCostDeviation,
        OUTPUT opdeCostTotal,
        OUTPUT oplError,
        OUTPUT opcMessage).

END PROCEDURE.
    
PROCEDURE pPoordlUp PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER v-RowID  AS   RowID     NO-UNDO.
    DEFINE INPUT PARAMETER v-factor AS   INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER v-reopen AS   LOGICAL   NO-UNDO.

    DEFINE VARIABLE v-hld-qty AS DECIMAL NO-UNDO.
    
    FIND po-ordl WHERE RowID(po-ordl) EQ v-RowID NO-ERROR.

    IF AVAILABLE po-ordl THEN 
    DO:
        // below code copy from - {po/poordlup.i}
                
        IF po-ordl.item-type THEN 
        DO:
            FIND FIRST item EXCLUSIVE
                     WHERE item.company eq po-ordl.company
                       AND item.i-no    eq po-ordl.i-no
                       USE-INDEX i-no NO-ERROR NO-WAIT.
                
            IF AVAILABLE item THEN 
            DO:
                IF item.i-code EQ "R" THEN
                    IF item.r-wid NE 0 THEN 
                        ASSIGN po-ordl.s-wid = item.r-wid.
                    ELSE 
                    DO:
                        IF item.s-wid NE 0 THEN 
                            ASSIGN po-ordl.s-wid = item.s-wid.
                        IF item.s-len NE 0 THEN 
                            ASSIGN po-ordl.s-len = item.s-len.
                    END.

                RUN po/rm-q-ono.p (BUFFER po-ordl, OUTPUT v-hld-qty).

                ASSIGN 
                    item.q-ono = item.q-ono + (v-hld-qty * v-factor).
          
                IF item.q-ono LT 0 AND v-reopen THEN 
                    ASSIGN item.q-ono = 0.
    
                ASSIGN 
                    item.q-avail = item.q-onh + item.q-ono - item.q-comm.

            END.        
        END.
        ELSE 
        DO:
            FIND FIRST itemfg EXCLUSIVE
                     WHERE itemfg.company EQ po-ordl.company
                       AND itemfg.i-no    EQ po-ordl.i-no
                    USE-INDEX i-no NO-ERROR.
                    
            IF AVAILABLE itemfg THEN 
            DO:
                RUN fg/calcqono.p (ROWID(itemfg), OUTPUT itemfg.q-ono).
                    
                IF NOT AVAILABLE po-ord THEN
                    FIND FIRST po-ord 
                        WHERE po-ord.company EQ {1}po-ordl.company
                        AND po-ord.po-no EQ {1}po-ordl.po-no
                        NO-LOCK NO-ERROR.
                            
                IF AVAIL(po-ord) THEN 
                DO:      
                    RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT po-ord.loc).
                    FIND FIRST itemfg-loc 
                        WHERE itemfg-loc.company EQ itemfg.company
                        AND itemfg-loc.i-no    EQ itemfg.i-no
                        AND itemfg-loc.loc     EQ po-ord.loc
                        EXCLUSIVE-LOCK NO-ERROR.
                    RUN fg/calcqool.p (ROWID(itemfg), 
                        INPUT po-ord.loc, 
                        OUTPUT itemfg-loc.q-ono).
                    FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
                END.                    
            END.                
        END.
    END.
END PROCEDURE.
    

PROCEDURE pSetGlobalSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE loFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE chReturn AS CHARACTER NO-UNDO.    
            
    /* POCOST */
    RUN sys/ref/nk1look.p (ipchCompany, "POCOST", "C" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    chpoCost1 = IF loFound THEN chReturn ELSE "".
    
    RUN sys/ref/nk1look.p (ipchCompany, "POCOST", "L" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    loHoldop1 = IF loFound AND chReturn = "Yes" THEN YES ELSE NO.
        
    /* OEAUTOPO */
    RUN sys/ref/nk1look.p (ipchCompany, "OEAUTOPO", "C" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    choeAutoPoNK1 = IF loFound THEN chReturn ELSE "".
    
    RUN sys/ref/nk1look.p (ipchCompany, "OEAUTOPO", "L" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    looeAutopoNK1 = IF loFound AND chReturn = "Yes" THEN YES ELSE NO.
    
    RUN sys/ref/nk1look.p (ipchCompany, "OEAUTOPO", "I" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    inOeAutoPONK1 = IF loFound THEN INTEGER(chReturn) ELSE 0.
    
    /* OEAUTOPREP */
    RUN sys/ref/nk1look.p (ipchCompany, "OEAUTOPREP", "L" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    looeAutoPrep = IF loFound AND chReturn = "Yes" THEN YES ELSE NO.
    
    /* POSTATUS */
    RUN sys/ref/nk1look.p (ipchCompany, "POSTATUS", "C" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    chPOStatus = IF loFound THEN chReturn ELSE "".
    
    /* POQTY */
    RUN sys/ref/nk1look.p (ipchCompany, "POQTY", "C" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    loPoQty = NOT(loFound AND chReturn EQ "Net Shts").
        
    /* "AP GL#" */
    RUN sys/ref/nk1look.p (ipchCompany, "AP GL#", "C" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    chAPGL = IF loFound THEN chReturn ELSE "".
    
    RUN sys/ref/nk1look.p (ipchCompany, "AP GL#", "L" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    loAPGL = IF loFound AND chReturn = "Yes" THEN YES ELSE NO.
    
    /* OEAUTOFG */
    RUN sys/ref/nk1look.p (ipchCompany, "OEAUTOFG", "C" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    chOEAutoFG = IF loFound THEN chReturn ELSE "".
    
    RUN sys/ref/nk1look.p (ipchCompany, "OEAUTOFG", "L" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    loOEAutoFg = IF loFound AND chReturn = "YES" THEN YES ELSE NO.
    
    /* OEAUTOFG */
    RUN sys/ref/nk1look.p (ipchCompany, "OEAUTOFG", "D", NO, YES, "", "", OUTPUT chReturn, OUTPUT loFound).
    deOeAutoFg = IF loFound THEN DECIMAL(chReturn) ELSE 0. 
        
    /* POUOM */
    RUN sys/ref/nk1look.p (ipchCompany, "POUOM", "C" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    chPOUom = IF loFound THEN chReturn ELSE "".
    
    RUN sys/ref/nk1look.p (ipchCompany, "POUOM", "I" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    inPOUom = IF loFound THEN  INTEGER(chReturn) ELSE 0.
    
    /* APTAX */
    RUN sys/ref/nk1look.p (ipchCompany, "APTAX", "C" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    chAPTax = IF loFound THEN chReturn ELSE "".
    
    /* MSFCALC */
    RUN sys/ref/nk1look.p (ipchCompany, "MSFCALC", "C" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    loMSFcalc = loFound AND chReturn EQ "Corrware".
    
    /* VendItemCost */
    RUN sys/ref/nk1look.p (ipchCompany, "VendItemCost", "L", NO, NO, "", "", OUTPUT chReturn, OUTPUT loFound).
    lNewVendorItemCost = IF loFound AND chReturn = "YES" THEN YES ELSE NO.

    IF loFromPoEntry THEN /* Security check only for order entry */
        ASSIGN 
            loAutoPoSec   = TRUE
            loAutoPrepSec = TRUE
            loAutoFgSec   = TRUE.
    ELSE 
    DO:
        /* Check if authorized to create PO's */
        RUN methods/prgsecur.p
            (INPUT "OEAutoPO",
            INPUT "ALL",
            INPUT NO,
            INPUT NO,
            INPUT NO,
            OUTPUT loAutoPoSec,
            OUTPUT loAccessClose,
            OUTPUT chAccessList).
            
        /* Check if authorized to create PO's */
        IF looeAutoPrep THEN
            RUN methods/prgsecur.p
                (INPUT "OEAutoPrep", /* program */
                INPUT "ALL",        /*Basis */
                INPUT NO,
                INPUT NO,
                INPUT NO,
                OUTPUT loAutoPrepSec,
                OUTPUT loAccessClose,
                OUTPUT chAccessList).
            
        /* Check if authorized to create PO's */
        IF loOEAutoFg THEN
            RUN methods/prgsecur.p
                (INPUT "OEAutoFG", /* Program master program name */
                INPUT "ALL",      /* Security based on */
                INPUT NO,
                INPUT NO,
                INPUT NO,
                OUTPUT loAutoFgSec,
                OUTPUT loAccessClose,
                OUTPUT chAccessList).        
    END.
END PROCEDURE.
    
PROCEDURE pFullSet PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
     Source: Copy of fg/fullset.p (ROWID(itemfg)).     
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.    

    DEFINE VARIABLE v-part-qty-dec LIKE tt-fg-set.part-qty-dec NO-UNDO.
    DEFINE VARIABLE v-i-no         LIKE itemfg.i-no NO-UNDO.

    FOR EACH tt-fg-set:
        DELETE tt-fg-set.
    END.

    FIND itemfg NO-LOCK 
        WHERE ROWID(itemfg) EQ ip-rowid NO-ERROR.

    IF AVAILABLE itemfg THEN 
    DO:
        FIND FIRST fg-set NO-LOCK 
            WHERE fg-set.company EQ itemfg.Company 
            AND fg-set.set-no    EQ itemfg.i-no 
            AND fg-set.part-no   EQ itemfg.i-no NO-ERROR.

        IF AVAILABLE fg-set THEN 
            ASSIGN 
                v-part-qty-dec = IF fg-set.qtyPerSet LT 0 THEN (1 / (fg-set.qtyPerSet * -1)) ELSE fg-set.qtyPerSet.
        
        CREATE tt-fg-set.
        ASSIGN
            tt-fg-set.part-no      = itemfg.i-no
            tt-fg-set.qtyPerSet    = IF AVAILABLE fg-set THEN fg-set.qtyPerSet ELSE 1
            tt-fg-set.part-qty-dec = IF AVAILABLE fg-set THEN v-part-qty-dec ELSE 1
            tt-fg-set.isaset       = NOT AVAILABLE fg-set
            tt-fg-set.alloc        = NOT AVAILABLE fg-set.
    END.

    DO WHILE CAN-FIND(FIRST tt-fg-set WHERE 
        tt-fg-set.set-no NE tt-fg-set.part-no AND 
        tt-fg-set.isaset AND tt-fg-set.alloc):
        FOR EACH tt-fg-set WHERE 
            tt-fg-set.isaset AND tt-fg-set.alloc:
            FOR EACH fg-set NO-LOCK WHERE 
                fg-set.company EQ fg-set.Company AND 
                fg-set.set-no  EQ tt-fg-set.part-no AND 
                fg-set.set-no  NE fg-set.part-no,
                FIRST itemfg NO-LOCK WHERE 
                itemfg.company EQ fg-set.Company AND 
                itemfg.i-no    EQ fg-set.part-no:
                    
                ASSIGN 
                    v-part-qty-dec = IF fg-set.qtyPerSet LT 0 THEN (1 / (fg-set.qtyPerSet * -1)) ELSE fg-set.qtyPerSet.

                CREATE tt-fg-set2.
                BUFFER-COPY fg-set TO tt-fg-set2
                    ASSIGN
                    tt-fg-set2.qtyPerSet     = tt-fg-set.qtyPerSet * fg-set.qtyPerSet
                    tt-fg-set2.part-qty-dec = tt-fg-set.part-qty-dec * v-part-qty-dec
                    tt-fg-set2.isaset       = itemfg.isaset
                    tt-fg-set2.alloc        = itemfg.alloc.
            END.
            DELETE tt-fg-set.
        END.

        FOR EACH tt-fg-set2:
            CREATE tt-fg-set.
            BUFFER-COPY tt-fg-set2 TO tt-fg-set.
            DELETE tt-fg-set2.
        END.
    END.

    FOR EACH tt-fg-set 
        BREAK BY tt-fg-set.part-no:
        IF FIRST-OF(tt-fg-set.part-no) THEN ASSIGN
                v-part-qty-dec = 0.
        ASSIGN
            v-part-qty-dec = v-part-qty-dec + tt-fg-set.part-qty-dec.
        IF LAST-OF(tt-fg-set.part-no) THEN ASSIGN
                tt-fg-set.part-qty-dec = v-part-qty-dec.
        ELSE 
            DELETE tt-fg-set.
    END.
END PROCEDURE.
    
PROCEDURE pAddError PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ipcError         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcErrorType     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiEstHeaderID   AS INT64     NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo       AS INTEGER   NO-UNDO.

    CREATE ttEstError.
    ASSIGN
        ttEstError.cError      = ipcError
        ttEstError.cErrorType  = ipcErrorType
        ttEstError.estHeaderID = ipiEstHeaderID
        ttEstError.iFormNo     = ipiFormNo
        ttEstError.iBlankNo    = ipiBlankNo.

END PROCEDURE.
    
PROCEDURE pCalcCost PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     Calculate of po-ordl.cost
      Parameters:  <none>
      Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.

    IF bf-po-ordl.item-type AND chpoCost1 EQ "Vendor/MSH" AND
        bf-po-ordl.pr-uom EQ "TON" AND debasis-w NE 0 AND deS-wid NE 0 THEN 
    DO:
        RUN Conv_ValueFromUOMtoUOM (ipchCompany,
            bf-po-ordl.i-no,
            bf-po-ordl.item-type,
            bf-po-ordl.cost,
            bf-po-ordl.pr-uom,
            "MSH",
            debasis-w, 
            deS-Len, 
            deS-wid, 
            deS-dep,
            0, 
            OUTPUT bf-po-ordl.cost,
            OUTPUT loError,
            OUTPUT chMessage).
        bf-po-ordl.pr-uom = "MSH".
        
    END. /* if bf-po-ordl.item-type ... */
    
    RELEASE bf-po-ordl.
    
END PROCEDURE.
 
PROCEDURE pCalcCostSetup PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprWJobMat    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl       FOR po-ordl.  
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.  
  
    FIND bf-ttJobMaterial WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-LOCK NO-ERROR.    
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.  
    
    
        ASSIGN
            bf-po-ordl.cost      = bf-ttJobMaterial.costPerUOM
            bf-po-ordl.setup     = bf-ttJobMaterial.costSetup     
            bf-po-ordl.vend-i-no = bf-ttJobMaterial.i-no
            bf-po-ordl.pr-uom    = bf-ttJobMaterial.costUOM
            bf-po-ordl.t-cost    = bf-ttJobMaterial.costTotal.
             
    RELEASE bf-po-ordl.
    RELEASE bf-ttJobMaterial.
END PROCEDURE.
 
PROCEDURE pCalcEstValues PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     If an RM, get values from estimate
      Parameters:  <none>
      Notes:       
        inputs:
          po-ordl (exclusive)
          loPoQty
          ttJobMaterial
          bf-ordl
          job-hdr
          job
          item
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprJob     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItem    AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl        FOR po-ordl.  
    DEFINE BUFFER bf-ttJobMaterial  FOR ttJobMaterial.
    DEFINE BUFFER bf-ordl           FOR oe-ordl.  
    DEFINE BUFFER bf-item           FOR ITEM.
    DEFINE BUFFER bf2-ordl          FOR oe-ordl.
    DEFINE BUFFER bf-job-hdr        FOR job-hdr.
    DEFINE BUFFER bf2-ttJobMaterial FOR ttJobMaterial.
    
    DEFINE VARIABLE deLineQty LIKE po-ordl.ord-qty NO-UNDO.
    DEFINE VARIABLE dePartQty AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE chMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE loError   AS LOGICAL   NO-UNDO.

    FIND bf-ttJobMaterial WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-LOCK NO-ERROR.
    FIND job WHERE ROWID(job) EQ iprJob NO-LOCK NO-ERROR.
    FIND bf-ordl WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-LOCK NO-ERROR.
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.
    FIND bf-item WHERE ROWID(bf-item) EQ iprItem NO-LOCK NO-ERROR.

    IF reJobRowid NE ? THEN
        FIND job-hdr 
            WHERE RowID(job-hdr) EQ reJobRowid
            NO-LOCK.
    FIND FIRST est
        WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
        NO-LOCK NO-ERROR.
    IF bf-po-ordl.item-type THEN 
    DO:
        /* S-8-POQTY JOBQTY or NETSHTS */

        IF loPoQty OR bf-ttJobMaterial.n-up EQ 0 OR INDEX("BP",bf-item.mat-type) LE 0 THEN  
        DO:       
            deLineQty = bf-ttJobMaterial.qty.  /* Job Qty */
        END.
        ELSE 
        DO:
            ASSIGN
                deLineQty = IF AVAILABLE bf-ordl THEN
                        bf-ordl.qty
                     ELSE
                     IF AVAILABLE job-hdr THEN
                        job-hdr.qty
                     ELSE
                        bf-ttJobMaterial.qty
                dePartQty = 0.                  

            IF AVAILABLE est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN 
            DO:     
                FOR EACH eb FIELDS(quantityPerSet)
                    WHERE eb.company EQ job.company
                    AND eb.est-no  EQ job.est-no
                    AND eb.form-no EQ bf-ttJobMaterial.frm
                    NO-LOCK:
                    dePartQty = dePartQty +
                        (deLineQty * IF eb.quantityPerSet LT 0 THEN (-1 / eb.quantityPerSet)
                        ELSE eb.quantityPerSet).
                END. /* Each eb */         
            END.
            ELSE  IF AVAILABLE est AND (est.est-type EQ 4 OR est.est-type EQ 8) THEN 
                DO:
                    deLineQty = 0.
        
                    IF AVAILABLE bf-ordl THEN 
                    DO:
                        FOR EACH bf-job-hdr WHERE bf-job-hdr.company EQ job-hdr.company
                            AND bf-job-hdr.job-no EQ job-hdr.job-no 
                            AND bf-job-hdr.job-no2 EQ job-hdr.job-no2
                            NO-LOCK,
                            FIRST bf2-ordl WHERE bf2-ordl.company EQ bf-ordl.company
                            AND bf2-ordl.ord-no EQ bf-ordl.ord-no
                            AND bf2-ordl.i-no EQ bf-job-hdr.i-no
                            NO-LOCK:
                            IF AVAILABLE bf2-ordl THEN
                                deLineQty = deLineQty + bf2-ordl.qty.

                        END. /* each bf-job-hdr */         
          
                    END. /* oe-ordl was available */
                    ELSE IF AVAILABLE job-hdr THEN 
                        DO:
                            FOR EACH bf-job-hdr WHERE bf-job-hdr.company EQ job-hdr.company
                                NO-LOCK:
                                deLineQty = deLineQty + bf-job-hdr.qty.
                            END. /* each bf-job-hdr */          
                        END. /* job-hdr was available */
                        ELSE IF AVAILABLE  bf-ttJobMaterial THEN 
                            DO:
                                FOR EACH bf2-ttJobMaterial WHERE bf2-ttJobMaterial.i-no EQ bf-ttJobMaterial.i-no:
                                    deLineQty = deLineQty + (bf2-ttJobMaterial.qty * bf2-ttJobMaterial.n-up).
                                END.          
                            END.
          
                    deLineQty = deLineQty / bf-ttJobMaterial.n-up.

                END.
                ELSE 
                    dePartQty = deLineQty.

            IF NOT(AVAILABLE est AND (est.est-type EQ 4 OR est.est-type EQ 8)) THEN
                deLineQty = dePartQty / bf-ttJobMaterial.n-up.

            IF bf-po-ordl.pr-qty-uom EQ "EA" THEN 
                deLineQty = fnRoundUp(deLineQty).
            
        END. /* NOT loPoQty OR bf-ttJobMaterial.n-up EQ 0 OR ... */

        IF bf-po-ordl.pr-qty-uom NE "EA" THEN
            RUN Conv_QuantityFromUOMToUOM(bf-ttJobMaterial.company,
                bf-ttJobMaterial.i-no,
                bf-ttJobMaterial.ItemType,
                deLineQty,                                                                                    
                bf-po-ordl.pr-qty-uom,
                bf-ttJobMaterial.qty-uom,                                          
                bf-ttJobMaterial.basis-w, 
                bf-ttJobMaterial.len,
                bf-ttJobMaterial.wid, 
                bf-ttJobMaterial.dep,
                0,                                           
                OUTPUT deLineQty,
                OUTPUT loError,
                OUTPUT chMessage).

        bf-po-ordl.ord-qty = deLineQty.
    END. /* If po-ordl.item-type */
    RELEASE bf-po-ordl.
    RELEASE job.
    RELEASE job-hdr.
    RELEASE bf-ttJobMaterial.
    RELEASE bf-ordl.

END PROCEDURE.
 
PROCEDURE pCalcExtCost PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.

    DEFINE VARIABLE deOrderQty LIKE po-ordl.ord-qty.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.  

    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.
    
    /**************************************************************/
    /**  Calculate Extended cost, order quantity is based on UOM **/
    /**************************************************************/
    IF LOOKUP(bf-po-ordl.pr-uom,"L,LOT") GT 0 THEN
        bf-po-ordl.t-cost = (bf-po-ordl.cost + bf-po-ordl.setup) *
            IF bf-po-ordl.ord-qty LT 0 THEN -1 ELSE 1.

    ELSE 
    DO:
        deOrderQty = bf-po-ordl.ord-qty.

        IF bf-po-ordl.pr-qty-uom NE bf-po-ordl.pr-uom            AND
            (bf-po-ordl.item-type                           OR
            NOT DYNAMIC-FUNCTION("Conv_IsEAUOM", bf-po-ordl.Company, bf-po-ordl.i-no, bf-po-ordl.pr-qty-uom) OR
            NOT DYNAMIC-FUNCTION("Conv_IsEAUOM", bf-po-ordl.Company, bf-po-ordl.i-no, bf-po-ordl.pr-uom))   THEN

            RUN Conv_QuantityFromUOMToUOM(bf-po-ordl.Company,
                bf-po-ordl.i-no,
                bf-po-ordl.item-type,
                deOrderQty,
                bf-po-ordl.pr-qty-uom, 
                bf-po-ordl.pr-uom,
                debasis-w, 
                deS-Len, 
                deS-wid, 
                deS-dep,
                0, 
                OUTPUT deOrderQty,
                OUTPUT loError,
                OUTPUT chMessage).
 
        bf-po-ordl.t-cost = (deOrderQty * bf-po-ordl.cost) + bf-po-ordl.setup.
    END. /* NOT LOOKUP(bf-po-ordl.pr-uom,"L,LOT") GT 0 */

    bf-po-ordl.cons-cost = bf-po-ordl.t-cost / bf-po-ordl.cons-qty.

    IF bf-po-ordl.disc NE 0 THEN bf-po-ordl.t-cost = bf-po-ordl.t-cost * (1 - (bf-po-ordl.disc / 100)).
    RELEASE bf-po-ordl.

END PROCEDURE.

PROCEDURE pCalcLenWid PRIVATE:
    /*------------------------------------------------------------------------------
          Purpose:     Calculate len & width values 
          Parameters:  <none>
          Notes:                    
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrd  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItem   AS ROWID       NO-UNDO.
    
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-po-ord  FOR po-ord.
    DEFINE BUFFER b-item     FOR ITEM.
    
    DEFINE VARIABLE chVendItemNo AS CHARACTER NO-UNDO.

    FIND bf-po-ord WHERE ROWID(bf-po-ord) EQ iprPoOrd NO-LOCK NO-ERROR.
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.
    FIND b-item WHERE ROWID(b-item) EQ iprItem NO-LOCK NO-ERROR.

    ASSIGN
        deS-Len = 0
        deS-wid = 0
        deS-dep = 0.

    IF AVAILABLE b-item THEN 
    DO:        
        RUN Vendor_GetVendorItemNumber IN hVendorCostProcs(INPUT bf-po-ordl.Company, 
            INPUT bf-po-ordl.i-no,
            INPUT bf-po-ord.vend-no, 
            INPUT lNewVendorItemCost,
            OUTPUT chVendItemNo).
                                       
        IF chVendItemNo NE "" THEN
            bf-po-ordl.vend-i-no = chVendItemNo.
        ELSE
            IF b-item.vend-no EQ bf-po-ord.vend-no THEN
                bf-po-ordl.vend-i-no = b-item.vend-item.
            ELSE
                IF b-item.vend2-no EQ bf-po-ord.vend-no THEN
                    bf-po-ordl.vend-i-no = b-item.vend2-item.

        IF INDEX("1234BPR",b-item.mat-type) GT 0 THEN 
        DO:
            ASSIGN
                debasis-w = b-item.basis-w
                deS-Len   = b-item.s-len
                deS-wid   = b-item.s-wid
                deS-dep   = b-item.s-dep.
     
            IF deS-wid EQ 0 THEN deS-wid = b-item.r-wid.
        END. /* if index(... */
    END. /* Avail b-item */
  
    /* Cust-no from order or job */
    bf-po-ordl.cust-no = IF AVAILABLE bf-ord THEN bf-ord.cust-no
                            ELSE IF AVAILABLE job-hdr THEN job-hdr.cust-no
                         ELSE "".
                         
    FIND CURRENT bf-po-ordl NO-LOCK NO-ERROR.
    RELEASE bf-po-ordl.
    RELEASE bf-po-ord.
    RELEASE b-item.

END PROCEDURE.
    
PROCEDURE pCalcMSF PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     Total MSF Calculation
      Parameters:  <none>
      Notes:       
        inputs:
          po-ordl (modified)
          globals: v-tot-msf, deS-Len, deS-wid
          
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.  

    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.

    IF bf-po-ordl.pr-qty-uom EQ "EA" THEN
        deTot-msf = IF loMSFcalc THEN ((((bf-po-ordl.s-len * bf-po-ordl.s-wid) * .007) *
            bf-po-ordl.ord-qty) / 1000)
            ELSE ((((bf-po-ordl.s-len * bf-po-ordl.s-wid) / 144) *
            bf-po-ordl.ord-qty) / 1000).
    ELSE
        deTot-msf = 0.

    /** Appears to be here so that it is assigned after the Total MSF Calculation */
    ASSIGN
        bf-po-ordl.s-len = deS-Len
        bf-po-ordl.s-wid = deS-wid.
        
    IF deS-dep GT 0 THEN 
    DO:        
        bf-po-ordl.s-dep = deS-dep.
    END.
    RELEASE bf-po-ordl.
END PROCEDURE.
 
PROCEDURE pCalcOrdQty PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      Inputs:
        ttJobMaterial
        po-ordl (updates)
        globals vars: debasis-w, deS-Len, deS-wid, deS-dep    
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl       FOR po-ordl.  
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
  
    FIND bf-ttJobMaterial WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-LOCK NO-ERROR.
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.  
    IF NOT AVAILABLE bf-ttJobMaterial OR NOT AVAILABLE bf-po-ordl THEN 
    DO:
        FIND CURRENT bf-po-ordl NO-LOCK NO-ERROR.
        RETURN ERROR.
    END.

    ASSIGN
        deS-Len = bf-ttJobMaterial.len
        deS-wid = bf-ttJobMaterial.wid
        deS-dep = bf-ttJobMaterial.dep.

    IF bf-po-ordl.s-num EQ 0 AND bf-po-ordl.b-num EQ 0 THEN
        ASSIGN bf-po-ordl.s-num = bf-ttJobMaterial.frm
            bf-po-ordl.b-num = bf-ttJobMaterial.blank-no.

    IF bf-po-ordl.pr-qty-uom EQ "BF" THEN 
    DO:
        RUN Conv_QuantityFromUOMToUOM(bf-ttJobMaterial.company,
            bf-ttJobMaterial.i-no,
            bf-ttJobMaterial.ItemType,                                      
            bf-po-ordl.ord-qty,
            bf-po-ordl.pr-qty-uom, 
            "EA",
            debasis-w, 
            deS-Len, 
            deS-wid, 
            deS-dep,
            0,                                       
            OUTPUT bf-po-ordl.ord-qty,
            OUTPUT loError,
            OUTPUT chMessage).
        
        ASSIGN 
            bf-po-ordl.ord-qty    = fnRoundUp(bf-po-ordl.ord-qty)
            bf-po-ordl.pr-qty-uom = "EA".
            
    END. /* bf-po-ordl.pr-qty-uom EQ "BF" */
    RELEASE bf-po-ordl.
    RELEASE bf-ttJobMaterial.

END PROCEDURE.
 
PROCEDURE pCheckZeroQty PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprPoOrdl AS ROWID       NO-UNDO.

    DEFINE VARIABLE deHoldCost   AS DECIMAL FORMAT "->>>>>>>9.99<<".  //Varun - We can remove
    DEFINE VARIABLE dehdeLineQty AS DECIMAL FORMAT "->>>>>>>>9.99<<". //Varun - We can remove
    
    DEFINE BUFFER bf-po-ordl       FOR po-ordl.  
    DEFINE BUFFER bf-po-ord        FOR po-ord.  
    DEFINE BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.  

    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE bf-po-ordl THEN
        FIND FIRST bf-po-ord 
            WHERE bf-po-ord.company EQ bf-po-ordl.company
            AND bf-po-ord.po-no EQ bf-po-ordl.po-no
            NO-LOCK NO-ERROR.
  
    /***********************************************/
    /* Handle a zero line quantity or cost         */
    /***********************************************/
    /** Purchase UOM and Consuption UOM are equal **/
    IF NOT bf-po-ord.received AND (dehdeLineQty EQ 0 OR deHoldCost EQ 0) THEN 
    DO:
        IF bf-po-ordl.pr-qty-uom EQ bf-po-ordl.cons-uom           OR
            (NOT bf-po-ordl.item-type                       AND
            DYNAMIC-FUNCTION("Conv_IsEAUOM", bf-po-ordl.Company, bf-po-ordl.i-no, bf-po-ordl.pr-qty-uom) AND
            DYNAMIC-FUNCTION("Conv_IsEAUOM", bf-po-ordl.Company, bf-po-ordl.i-no, bf-po-ordl.cons-uom))     THEN
            bf-po-ordl.cons-qty = bf-po-ordl.ord-qty.
        ELSE
            RUN Conv_QuantityFromUOMToUOM(bf-po-ordl.Company,
                bf-po-ordl.i-no,
                bf-po-ordl.item-type,
                bf-po-ordl.ord-qty,
                bf-po-ordl.pr-qty-uom, 
                bf-po-ordl.cons-uom,
                debasis-w, 
                deS-Len, 
                deS-wid, 
                deS-dep,
                0,                                           
                OUTPUT bf-po-ordl.cons-qty,
                OUTPUT loError,
                OUTPUT chMessage).
      
        IF bf-po-ordl.pr-uom EQ bf-po-ordl.cons-uom           OR
            (NOT bf-po-ordl.item-type                     AND
            DYNAMIC-FUNCTION("Conv_IsEAUOM", bf-po-ordl.Company, bf-po-ordl.i-no, bf-po-ordl.pr-uom) AND
            DYNAMIC-FUNCTION("Conv_IsEAUOM", bf-po-ordl.Company, bf-po-ordl.i-no, bf-po-ordl.cons-uom))   THEN
            bf-po-ordl.cons-cost = bf-po-ordl.cost.
        ELSE
            RUN Conv_ValueFromUOMtoUOM(bf-po-ordl.Company,
                bf-po-ordl.i-no,
                bf-po-ordl.item-type,
                bf-po-ordl.cost,
                bf-po-ordl.pr-uom, 
                bf-po-ordl.cons-uom,
                debasis-w, 
                deS-Len, 
                deS-wid, 
                deS-dep,
                0, 
                OUTPUT bf-po-ordl.cons-cost,
                OUTPUT loError,
                OUTPUT chMessage).

        /*FG*/
        /**************************************/
        /* Calculate oe-ordl.cost             */
        /*************************************/
        IF NOT bf-po-ordl.item-type THEN
        DO:
            FIND oe-ordl WHERE ROWID(oe-ordl) EQ reORDRowId EXCLUSIVE-LOCK NO-ERROR.
      
            IF AVAILABLE oe-ordl THEN
            DO:
                IF bf-po-ordl.cons-uom EQ "M" THEN
                    oe-ordl.cost = bf-po-ordl.cons-cost.
                ELSE
                    RUN Conv_ValueFromUOMtoUOM(bf-po-ordl.Company,
                        bf-po-ordl.i-no,
                        bf-po-ordl.item-type,
                        bf-po-ordl.cons-cost,
                        bf-po-ordl.cons-uom, 
                        "M", 
                        0, 
                        0, 
                        0, 
                        0,
                        0, 
                        OUTPUT oe-ordl.cost,
                        OUTPUT loError,
                        OUTPUT chMessage).
            END. /* avail oe-ordl */
      
            FIND FIRST bf-e-itemfg-vend WHERE
                bf-e-itemfg-vend.company EQ bf-po-ordl.company AND
                bf-e-itemfg-vend.i-no EQ bf-po-ordl.i-no AND
                bf-e-itemfg-vend.vend-no EQ bf-po-ord.vend-no AND
                bf-e-itemfg-vend.est-no EQ ""
                NO-LOCK NO-ERROR.

            IF AVAILABLE bf-e-itemfg-vend THEN
            DO:
                oe-ordl.cost = oe-ordl.cost * (1 + (bf-e-itemfg-vend.markup / 100.0 )).
            END. /* avail reftable */

            RELEASE oe-ordl.
        END. /* not bf-po-ordl.item-type */
    END. /* not po-ord.received  ... */
    RELEASE bf-po-ordl.
END PROCEDURE.
 
PROCEDURE pCreatePoOrd PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        Requires company record
        Creates PO-ord
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipChCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrd       AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER iploDropShip   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oprPoOrd       AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER oploNextOuters AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE iCount3     AS INTEGER NO-UNDO.
    DEFINE VARIABLE fil_id      AS RowID   NO-UNDO.
    DEFINE VARIABLE inExpLimit  AS INTEGER   NO-UNDO INITIAL 10.
        
    DEFINE BUFFER bf-ord FOR oe-ord.
    
    FIND bf-ord WHERE ROWID(bf-ord) EQ iprOeOrd NO-LOCK NO-ERROR.
    
    /* Below logic copied from po/po-ord.a */
    
    DO iCount3 = 1 TO inExpLimit:
        FIND FIRST po-ctrl
            WHERE po-ctrl.company EQ ipchCompany
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            
        IF NOT AVAILABLE po-ctrl THEN 
        DO:
            IF iCount3 EQ 1 THEN MESSAGE
                    " Please wait, the system is searching for the PO CONTROL RECORD.".
            READKEY PAUSE 1.
        END.
        
        IF NOT AVAILABLE po-ctrl AND iCount3 EQ inExpLimit THEN 
        DO:
            BELL.
            MESSAGE " The system was unable to obtain the PO CONTROL RECORD.".
            PAUSE 2 NO-MESSAGE.
            loNewFile = NO.            
        END.
    END. /* x = 1 to inExpLimit */

    RUN sys/ref/asiseq.p (ipchCompany,'po_seq',OUTPUT iCount3) NO-ERROR.
    
    CREATE po-ord.
    ASSIGN
        po-ord.company        = ipchCompany
        po-ord.po-no          = iCount3
        po-ord.po-date        = TODAY
        po-ord.loc            = chLOC
        po-ord.buyer          = USERID("NOSWEAT")  /*global-uid*/
        po-ord.under-pct      = 10
        po-ord.over-pct       = 10        
        po-ord.due-date       = po-ord.po-date + IF WEEKDAY(po-ord.po-date) EQ 6 THEN 3 ELSE 1
        po-ord.last-ship-date = po-ord.due-date
        fil_id                = RowID (po-ord).
 
    IF TRIM(chPOStatus) = "Hold" THEN po-ord.stat = "H".
 
    RELEASE po-ctrl.
    FIND FIRST po-ctrl WHERE po-ctrl.company EQ ipchCompany NO-LOCK NO-ERROR.    
    
    /* End of po/po-ord.a */

    ASSIGN
        po-ord.po-date        = ttJobMaterial.PODate
        po-ord.due-date       = ttJobMaterial.PODueDate
        po-ord.last-ship-date = po-ord.due-date
        po-ord.vend-no        = ttJobMaterial.vendorID.
  
    IF AVAILABLE bf-ord THEN
        ASSIGN
            chDropCustNo = bf-ord.cust-no
            chShipChoice = "C".

    roPoOrd = ROWID(po-ord).
  
    /* Prompt for drop ship shipto and assign po-ord ship fields */
    IF iploDropShip AND ttJobMaterial.ShipChoice = "C" THEN 
    DO: 
        FIND shipto WHERE RECID(shipto) EQ ttJobMaterial.ShipToRecID NO-LOCK NO-ERROR.
        IF AVAILABLE shipto THEN 
        DO:
            ASSIGN
                po-ord.type         = "D"
                po-ord.cust-no      = ttJobMaterial.DropCustNo
                po-ord.ship-id      = shipto.ship-id
                po-ord.ship-name    = shipto.ship-name
                po-ord.ship-addr[1] = shipto.ship-addr[1]
                po-ord.ship-addr[2] = shipto.ship-addr[2]
                po-ord.ship-city    = shipto.ship-city
                po-ord.ship-state   = shipto.ship-state
                po-ord.ship-zip     = shipto.ship-zip.

            IF po-ord.frt-pay NE "P" THEN po-ord.carrier = shipto.carrier.            
        END. /* if avail shipto */
    END.
    
    IF iploDropShip AND ttJobMaterial.ShipChoice = "V" AND ttJobMaterial.ShipToVendId NE "" THEN
    DO:     
        FIND FIRST vend NO-LOCK
            WHERE vend.company EQ ttJobMaterial.company
            AND vend.vend-no   EQ ENTRY(1,ttJobMaterial.ShipToVendId)
            NO-ERROR.
        IF AVAILABLE vend THEN 
        DO:
            ASSIGN
                po-ord.type         = "D"
                po-ord.ship-id      = vend.vend-no
                po-ord.cust-no      = ""
                po-ord.ship-name    = vend.name
                po-ord.ship-addr[1] = vend.add1
                po-ord.ship-addr[2] = vend.add2
                po-ord.ship-city    = vend.city
                po-ord.ship-state   = vend.state
                po-ord.ship-zip     = vend.zip
                po-ord.carrier      = vend.carrier.                
        END. /* avail vend */
    END. 
    ELSE 
        IF AVAILABLE company THEN
            ASSIGN
                po-ord.ship-id      = company.company
                po-ord.ship-name    = company.NAME
                po-ord.ship-addr[1] = company.addr[1]
                po-ord.ship-addr[2] = company.addr[2]
                po-ord.ship-city    = company.city
                po-ord.ship-state   = company.state
                po-ord.ship-zip     = company.zip.

    oprPoOrd = ROWID(po-ord).
    FIND CURRENT po-ord NO-LOCK NO-ERROR.
    RELEASE po-ord.
    
END PROCEDURE.
 
PROCEDURE pCreatePoOrdl PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrd    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl   AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItem     AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprItemfg   AS ROWID       NO-UNDO.

    DEFINE VARIABLE iCount4 AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-ordl          FOR oe-ordl.
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    DEFINE BUFFER bf-itemfg        FOR Itemfg.

    FIND ITEM WHERE ROWID(ITEM) EQ iprItem NO-LOCK NO-ERROR.
    FIND bf-ttJobMaterial WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-LOCK NO-ERROR.
    FIND po-ord WHERE ROWID(po-ord) EQ iprPoOrd NO-LOCK NO-ERROR.
    FIND bf-ordl WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-LOCK NO-ERROR.

    IF AVAILABLE item THEN
        FIND b-item WHERE RowID(b-item) EQ RowID(item) NO-LOCK.

    loNewAvail = NO.
    /* If bf-itemfg found, then this is an FG item and don't join on frm */
    
    FIND FIRST bf-itemfg
        WHERE bf-itemfg.company EQ bf-ordl.company
        AND bf-itemfg.i-no      EQ bf-ttJobMaterial.rm-i-no
        NO-LOCK NO-ERROR.
              
    IF NOT AVAILABLE po-ordl AND choeAutoPoNK1 EQ "AutoRM" AND loAutoPoSec AND bf-ttJobMaterial.this-is-a-rm THEN 
    DO: 
        FIND FIRST po-ordl EXCLUSIVE-LOCK 
            WHERE po-ordl.company EQ ipchCompany
            AND po-ordl.job-no  EQ bf-ttJobMaterial.job-no
            AND po-ordl.job-no2 EQ bf-ttJobMaterial.job-no2
            AND po-ordl.s-num   EQ bf-ttJobMaterial.frm
            AND po-ordl.i-no    EQ
            IF LENGTH(bf-ttJobMaterial.i-no) LE 10 THEN bf-ttJobMaterial.i-no
            ELSE substr(bf-ttJobMaterial.i-no,LENGTH(bf-ttJobMaterial.i-no) - 9,10)
            NO-ERROR.
        
        IF AVAILABLE po-ordl THEN 
        DO:
            FIND FIRST b-item
                WHERE b-item.company EQ ipchCompany
                AND b-item.i-no    EQ
                IF LENGTH(bf-ttJobMaterial.i-no) LE 10 THEN bf-ttJobMaterial.i-no
                ELSE substr(bf-ttJobMaterial.i-no,LENGTH(bf-ttJobMaterial.i-no) - 9,10)
                NO-LOCK NO-ERROR.
            
            IF AVAILABLE b-item THEN loNewAvail = YES.
            ELSE
            DO:
                IF bf-ttJobMaterial.prep EQ NO THEN
                DO:
                    IF loPOBest EQ NO THEN
                        FIND FIRST b-item
                            WHERE b-item.company  EQ ipchCompany 
                            AND b-item.i-no     EQ bf-ttJobMaterial.rm-i-no 
                            AND index("1234BPR",b-item.mat-type) GT 0 
                            NO-LOCK NO-ERROR.
                    ELSE
                        FIND FIRST b-item
                            WHERE b-item.company  EQ ipchCompany 
                            AND b-item.i-no     EQ bf-ttJobMaterial.rm-i-no 
                            AND b-item.mat-type EQ "B" 
                            NO-LOCK NO-ERROR.
                END. /* if bf-ttJobMaterial.prep eq no */
                ELSE
                    FIND FIRST b-item
                        WHERE b-item.company  EQ ipchCompany 
                        AND b-item.i-no     EQ bf-ttJobMaterial.rm-i-no
                        NO-LOCK NO-ERROR.
            END. /* not avail b-item */
        END. /* if avail po-ordl */
    END. /* Not avail po-ordl and this is RM */
  
    IF NOT AVAILABLE po-ordl THEN 
    DO:
        /* Below code copied from po-ordl.a */
                
        IF AVAILABLE po-ord THEN 
        DO:
            iCount4 = 1.
            FIND LAST po-ordl WHERE
                po-ordl.company EQ po-ord.company AND
                po-ordl.po-no EQ po-ord.po-no
                NO-LOCK NO-ERROR.
        
            IF AVAILABLE po-ordl THEN iCount4 = po-ordl.line + 1.
            
            FIND vend WHERE vend.company = po-ord.company
                AND vend.vend-no = po-ord.vend-no NO-LOCK NO-ERROR.

            CREATE po-ordl.
            ASSIGN
                po-ordl.company   = po-ord.company
                po-ordl.po-no     = po-ord.po-no
                po-ordl.stat      = "O"
                po-ordl.ord-qty   = 1
                po-ordl.cons-qty  = 1
                loNewFile         = TRUE
                po-ordl.line      = iCount4
                po-ordl.due-date  = po-ord.due-date
                po-ordl.over-pct  = po-ord.over-pct
                po-ordl.under-pct = po-ord.under-pct
                po-ordl.vend-no   = po-ord.vend-no.

            IF AVAILABLE vend THEN 
            DO:
                ASSIGN
                    po-ordl.disc = vend.disc-%
                    po-ordl.tax  = vend.tax-gr NE "".
     
                IF loAPGL AND index(chAPGL,"Vend") GT 0 THEN
                    po-ordl.actnum = vend.actnum.
            END.
            
            IF po-ord.printed OR po-ord.stat NE "N" THEN
                ASSIGN po-ordl.stat = "A".
        END.

        ELSE 
        DO:
            BELL. 
            MESSAGE " PO record is not available. " . 
            PAUSE.
        END.
        
        /* End of po-ordl.a */
        ASSIGN
            po-ordl.tax       = po-ord.tax-gr NE "" AND
                         (chAPTax EQ "Vendor" OR 
                          (chAPTax EQ "Item" AND
                           (AVAILABLE b-item AND b-item.tax-rcpt) OR
                           (AVAILABLE itemfg AND itemfg.taxable)))
            po-ordl.item-type = bf-ttJobMaterial.this-is-a-rm.
    END. /* Not avail po-ordl then add it */

    IF AVAILABLE bf-itemfg THEN
        oprItemfg = ROWID(bf-itemfg).

    IF AVAILABLE po-ordl THEN
        roPoOrdl = ROWID(po-ordl).
    FIND CURRENT po-ordl NO-LOCK NO-ERROR.
    RELEASE po-ordl.
    RELEASE bf-ttJobMaterial.

END PROCEDURE. 
 
PROCEDURE pFindExistingPo PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        Inputs:
          b-orderpo
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoNo       AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER iploDropShip  AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipchVendorId  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdaPODate    AS DATE      NO-UNDO.
    DEFINE INPUT  PARAMETER ipdaPODueDate AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER oplAvailPO    AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oprPoOrd      AS ROWID NO-UNDO.

    DEFINE VARIABLE loMultiPOAvailable AS LOGICAL NO-UNDO.
    
    
     
    
    FIND po-ord NO-LOCK
        WHERE po-ord.company  EQ ipchCompany
        AND po-ord.po-date   EQ ipdaPODate
        AND po-ord.due-date EQ ipdaPODueDate
        AND po-ord.vend-no  EQ ipchVendorId
        AND po-ord.opened   EQ YES
        AND (po-ord.type    EQ "D" OR NOT iploDropShip)
        NO-ERROR.

    loMultiPOAvailable = AMBIGUOUS po-ord.
        
    IF loMultiPOAvailable THEN
    DO:
        RUN windows/l-povndt.w (ipchCompany, ipchVendorId, ipdaPODueDate, BUFFER po-ord).
        ASSIGN 
            oplAvailPo = YES.
    END.
    ELSE IF AVAILABLE po-ord THEN
            ASSIGN oplAvailPo = YES.
        ELSE 
            oplAvailPo = NO.        
        
    IF AVAILABLE po-ord THEN
        oprPoOrd = ROWID(po-ord).
     
END PROCEDURE.
 
PROCEDURE pFindOrderFromRowID PRIVATE:
    /*------------------------------------------------------------------------------
 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprFilId        AS RowID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprOeOrdl       AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprJob          AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFilIdSource  AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opchCompanyId   AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opchLOC         AS CHARACTER   NO-UNDO.

    FIND bf-ordl WHERE RowID(bf-ordl) EQ iprFilId NO-LOCK NO-ERROR.

    /* Build tt-itemfg based on job */
    IF AVAILABLE bf-ordl THEN 
    DO:
        /* bf-itemfg holds buffer for item of bf-ordl since itemfg is used for set components below */
        ASSIGN 
            opcFilIdSource = "oe-ordl"
            inOrderNo      = bf-ordl.ord-no
            opchCompanyId  = bf-ordl.Company.

        FIND FIRST bf-ord NO-LOCK 
            WHERE bf-ord.company EQ bf-ordl.company
            AND bf-ord.ord-no  EQ bf-ordl.ord-no
            AND bf-ord.opened  EQ YES
            AND bf-ord.stat    NE "H"
            NO-ERROR.

        IF AVAILABLE bf-ord THEN 
        DO:
            ASSIGN 
                roOeOrd = ROWID(bf-ord)            
                opchLOC = bf-ord.loc.
            
            IF TRIM(bf-ordl.job-no) NE "" THEN
                FIND FIRST job NO-LOCK 
                    WHERE job.company EQ bf-ordl.company
                     AND job.job-no  EQ bf-ordl.job-no
                    AND job.job-no2 EQ bf-ordl.job-no2
                    NO-ERROR.
            
            IF NOT AVAILABLE job THEN
            DO:
                FIND FIRST itemfg NO-LOCK 
                    WHERE itemfg.company EQ bf-ordl.company
                    AND itemfg.i-no    EQ bf-ordl.i-no
                    NO-ERROR.
                    
                IF AVAILABLE itemfg THEN 
                DO:
                    IF itemfg.isaset THEN 
                    DO:
                        RUN pFullSet(ROWID(itemfg)).
       
                        RELEASE itemfg.
       
                        FOR EACH tt-fg-set,
                            FIRST itemfg NO-LOCK
                            WHERE itemfg.company EQ bf-ordl.company
                            AND itemfg.i-no    EQ tt-fg-set.part-no:
                            
                            CREATE tt-itemfg.
                            BUFFER-COPY itemfg EXCEPT rec_key TO tt-itemfg
                                ASSIGN
                                tt-itemfg.isacomponent = TRUE
                                tt-itemfg.form-no  = 0
                                tt-itemfg.blank-no = 0
                                tt-itemfg.qty      = bf-ordl.qty * tt-fg-set.part-qty-dec
                                tt-itemfg.pur-uom  = "EA"
                                tt-itemfg.row-id   = ROWID(itemfg).
              
                            IF CAN-FIND(FIRST eb NO-LOCK WHERE
                                eb.company EQ bf-ordl.company AND
                                eb.est-no EQ bf-ordl.est-no AND
                                eb.stock-no EQ tt-fg-set.part-no AND
                                eb.blank-no EQ tt-itemfg.blank-no AND
                                eb.form-no EQ tt-itemfg.form-no AND
                                eb.pur-man EQ TRUE) THEN 
                                ASSIGN tt-itemfg.pur-man = TRUE.
                        END. /* Each tt-fg-set */
                    END. /* If itemfg.isaset */       
                    ELSE 
                    DO:
                        CREATE tt-itemfg.
                        BUFFER-COPY itemfg EXCEPT rec_key TO tt-itemfg
                            ASSIGN
                            tt-itemfg.form-no  = bf-ordl.form-no
                            tt-itemfg.blank-no = bf-ordl.blank-no
                            tt-itemfg.qty      = bf-ordl.qty
                            tt-itemfg.pur-uom  = "EA"
                            tt-itemfg.row-id   = ROWID(itemfg).
                            
                        IF CAN-FIND (FIRST eb NO-LOCK WHERE
                            eb.company EQ bf-ordl.company AND
                            eb.est-no EQ bf-ordl.est-no AND
                            eb.stock-no EQ bf-ordl.i-no AND
                            eb.blank-no EQ bf-ordl.blank-no AND
                            eb.form-no EQ bf-ordl.form-no AND 
                            eb.pur-man EQ TRUE) THEN 
                            ASSIGN tt-itemfg.pur-man = TRUE.
                    END. /* If Not Itemfg.isaset */
                END. /* If avail itemfg */
            END. /* If not avail job ... */
        END. /* If avail bf-ord */
    END. /* If avail bf-ordl */
    ELSE 
    DO:
        /* Oe-ordl not available */
        FIND job WHERE RowID(job) EQ iprFilId NO-LOCK NO-ERROR.
        
        IF AVAILABLE job THEN 
        DO: 
            ASSIGN 
                opcFilIdSource = "JOB"
                opchCompanyId  = job.company
                opchLOC        = job.loc.
                
            FOR EACH job-hdr NO-LOCK
                WHERE job-hdr.company EQ job.company
                AND job-hdr.job     EQ job.job
                AND job-hdr.job-no  EQ job.job-no
                AND job-hdr.job-no2 EQ job.job-no2
                AND job-hdr.ord-no  NE 0:
                FIND FIRST bf-ord NO-LOCK
                    WHERE bf-ord.company EQ job-hdr.company
                    AND bf-ord.ord-no  EQ job-hdr.ord-no
                    AND bf-ord.opened  EQ YES
                    AND bf-ord.stat    NE "H"
                    NO-ERROR.
                IF NOT AVAILABLE bf-ord THEN 
                DO:
                    RELEASE job.
                    LEAVE.
                END. /* not avail bf-ord */
        
                roOeOrd = ROWID(bf-ord).
    
                FIND FIRST bf-ordl NO-LOCK 
                    WHERE bf-ordl.company EQ job-hdr.company
                    AND bf-ordl.ord-no  EQ job-hdr.ord-no
                    AND bf-ordl.i-no    EQ job-hdr.i-no
                    NO-ERROR.
                IF AVAILABLE bf-ordl THEN LEAVE.
            END. /* each job-hdr */
        END. 
    END. /* bf-ordl not avail */

    IF AVAILABLE bf-ordl THEN 
    DO:
        oprOeOrdl = ROWID(bf-ordl).
        FIND FIRST oe-ord NO-LOCK
            WHERE oe-ord.company EQ bf-ordl.company
            AND oe-ord.ord-no EQ bf-ordl.ord-no
            NO-ERROR.
        IF AVAILABLE oe-ord THEN
            roOeOrd = ROWID(oe-ord).
  
    END. /* avail bf-ordl */

    IF AVAILABLE job THEN
        oprJob = ROWID(job).

END PROCEDURE.
 
PROCEDURE pGetItemfgGL PRIVATE :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    
    DEFINE INPUT  PARAMETER ip-comp      LIKE job-hdr.company.
    DEFINE INPUT  PARAMETER ip-i-no      LIKE itemfg.i-no.
    DEFINE OUTPUT PARAMETER out-actnum   LIKE po-ordl.actnum.

    DEFINE VARIABLE chCharge AS CHARACTER NO-UNDO.
     
    /* populate GL# from reftable if it exists using itemfg AH 02-23-10 */

    FIND itemfg NO-LOCK WHERE itemfg.company = ip-comp AND itemfg.i-no = ip-i-no NO-ERROR.
    IF AVAILABLE itemfg THEN 
    DO:
        ASSIGN 
            chCharge = "".
        FIND FIRST surcharge NO-LOCK WHERE surcharge.company = ip-comp
            AND surcharge.charge <> "" NO-ERROR.
        IF AVAILABLE surcharge THEN
            ASSIGN chCharge = surcharge.charge.
        FIND FIRST fgcat NO-LOCK WHERE fgcat.company  EQ itemfg.company
            AND fgcat.procat      EQ itemfg.procat
            NO-ERROR.

        IF AVAILABLE fgcat 
            AND fgcat.miscCharge  EQ chCharge 
            AND fgcat.brdExpAcct <> "" THEN 
            ASSIGN out-actnum = fgcat.brdExpAcct.
    END. /* avail itemfg */
    RELEASE itemfg.

END PROCEDURE.
 
PROCEDURE pInitJobVals PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFilId    AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprJob      AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl   AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJob      AS CHARACTER   NO-UNDO.

    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    DEFINE BUFFER bf-oe-ordl       FOR oe-ordl.

    FIND bf-ttJobMaterial WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-LOCK NO-ERROR.
    FIND job WHERE ROWID(job) EQ iprJob NO-LOCK NO-ERROR.
    FIND bf-oe-ordl WHERE ROWID(bf-oe-ordl) EQ iprOeOrdl NO-LOCK NO-ERROR.
    
    ASSIGN 
        reJobRowid = ?.

    IF AVAILABLE job THEN
        FOR EACH job-hdr NO-LOCK
            WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            BREAK BY job-hdr.frm      DESCENDING
            BY job-hdr.blank-no DESCENDING:
            IF (job-hdr.frm EQ bf-ttJobMaterial.frm AND
                (job-hdr.blank-no EQ bf-ttJobMaterial.blank-no OR
                bf-ttJobMaterial.blank-no EQ 0)) OR
                LAST(job-hdr.blank-no) THEN 
            DO:
          
                ASSIGN 
                    inOrderNo  = job-hdr.ord-no
                    reJobRowid = RowID(job-hdr).
                LEAVE.
            END.
        END. /* each job-hdr */

    /*once out of loop above, not pointing to correct job-hdr*/
    IF reJobRowid NE ? THEN
        FIND job-hdr NO-LOCK
            WHERE RowID(job-hdr) EQ reJobRowid.
  
    ASSIGN loNewFile = YES.
    
    IF AVAILABLE job THEN 
        ASSIGN
            chJobNumber = job.job-no
            chJobNumber = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', chJobNumber, job.job-no2)) .

    opcJob    = chJobNumber.

    RELEASE po-ord.
    RELEASE po-ordl.  
    RELEASE bf-ttJobMaterial.
    
END PROCEDURE.
 
PROCEDURE pInitRptRecs PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat  AS ROWID       NO-UNDO.
    
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.

    DEFINE VARIABLE v-uom-comp LIKE po-ordl.pr-qty-uom NO-UNDO.

    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-ERROR.

    ASSIGN
        deS-Len   = bf-ttJobMaterial.len
        deS-wid   = bf-ttJobMaterial.wid
        deS-dep   = bf-ttJobMaterial.dep
        debasis-w = bf-ttJobMaterial.basis-w.
    
        IF bf-ttJobMaterial.qty-uom EQ "BF" THEN 
        DO:
            RUN Conv_QuantityFromUOMToUOM(bf-ttJobMaterial.company,
                bf-ttJobMaterial.i-no,
                bf-ttJobMaterial.itemtype,
                bf-ttJobMaterial.qty,
                bf-ttJobMaterial.qty-uom, 
                "EA",
                debasis-w, 
                deS-Len, 
                deS-wid, 
                deS-dep,
                0, 
                OUTPUT deJobMatQty,
                OUTPUT loError,
                OUTPUT chMessage).

            ASSIGN deJobMatQty    = fnRoundUp(deJobMatQty)
                   chJobMatQtyUOM = "EA".
        END. /* bf-ttJobMaterial.qty-uom eq "Bf */

        ELSE
            ASSIGN
                chJobMatQtyUOM = bf-ttJobMaterial.qty-uom
                deJobMatQty    = bf-ttJobMaterial.qty.

        v-uom-comp = IF bf-ttJobMaterial.this-is-a-rm THEN
            IF CAN-DO("1,2,3,4",item.mat-type) THEN "BF" ELSE "MSF" ELSE "EA".

        IF chJobMatQtyUOM EQ v-uom-comp                 OR
            (NOT bf-ttJobMaterial.this-is-a-rm             AND
            DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-ttJobMaterial.i-no, chJobMatQtyUOM) AND
            DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-ttJobMaterial.i-no, v-uom-comp))    THEN
            deQtyComp = deJobMatQty.
        ELSE
            RUN Conv_QuantityFromUOMToUOM(ipchCompany,
                bf-ttJobMaterial.i-no,
                bf-ttJobMaterial.ItemType,
                deJobMatQty,
                chJobMatQtyUOM, 
                v-uom-comp,
                debasis-w, 
                deS-Len, 
                deS-wid, 
                deS-dep,
                0, 
                OUTPUT deQtyComp,
                OUTPUT loError,
                OUTPUT chMessage).

        v-uom-comp = "TON".

        IF chJobMatQtyUOM EQ v-uom-comp                 OR
            (NOT bf-ttJobMaterial.this-is-a-rm             AND
            DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-ttJobMaterial.i-no, chJobMatQtyUOM) AND
            DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-ttJobMaterial.i-no, v-uom-comp))    THEN
            deQtyComp1 = deJobMatQty.
        ELSE
            RUN Conv_QuantityFromUOMToUOM(bf-ttJobMaterial.company,
                bf-ttJobMaterial.i-no,
                bf-ttJobMaterial.itemType,
                deJobMatQty,
                chJobMatQtyUOM, 
                v-uom-comp,
                debasis-w, 
                deS-Len, 
                deS-wid, 
                deS-dep,
                0, 
                OUTPUT deQtyComp1,
                OUTPUT loError,
                OUTPUT chMessage).               
     
    RELEASE bf-ttJobMaterial.

END PROCEDURE.
 
PROCEDURE pPoOrdlAddVals PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:      Assign more values to bf-po-ordl if a RM      
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.
    DEFINE INPUT PARAMETER iprItem       AS ROWID       NO-UNDO.
    DEFINE INPUT PARAMETER iprWJobMat    AS ROWID       NO-UNDO.
    DEFINE INPUT PARAMETER iprOeOrdl     AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oploNextOuters AS LOGICAL    NO-UNDO.

    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    DEFINE BUFFER bf-po-ordl       FOR po-ordl.
    DEFINE BUFFER b-item           FOR ITEM .
    DEFINE BUFFER bf-ordl          FOR oe-ordl.


    FIND bf-po-ordl       EXCLUSIVE-LOCK WHERE ROWID(bf-po-ordl)   EQ iprPoOrdl NO-ERROR.
    FIND b-item           NO-LOCK WHERE ROWID(b-item)       EQ iprItem NO-ERROR.
    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-ERROR.
    FIND bf-ordl          NO-LOCK WHERE ROWID(bf-ordl)      EQ iprOeOrdl NO-ERROR.  

    IF NOT AVAILABLE bf-po-ordl THEN 
    DO:
        RUN pAddError(INPUT "Error: PO line not available in poOrdlAddVals", INPUT "1", INPUT 0, INPUT 0, INPUT 0).
        ASSIGN oploNextOuters = TRUE.
        RETURN.
    END.
    IF NOT AVAILABLE b-item THEN 
    DO:
        RUN pAddError(INPUT "Error: item record not available in poOrdlAddVals", INPUT "1", INPUT 0, INPUT 0, INPUT 0).
        ASSIGN oploNextOuters = TRUE.
        RETURN.
    END.

    IF bf-po-ordl.item-type THEN 
    DO:
        IF AVAILABLE bf-ordl AND bf-po-ordl.item-type THEN 
        DO:
            /* Adding code to create new RM from Estimated RM */
            ASSIGN
                chNewItemNo = IF LENGTH(bf-ordl.i-no) LE 10 THEN bf-ordl.i-no
                                  ELSE SUBSTRING(bf-ordl.i-no,(LENGTH(bf-ordl.i-no) - 9),10).
                       
            IF choeAutoPoNK1 EQ "AutoRM" AND loAutoPoSec AND NOT loNewAvail THEN 
            DO: 
                RUN rm/itemcopy.p.   /* not done */
            END. /* format eq "AutoRM" ... */
        END. /* avail bf-ordl ... */
        
        IF inPOUom EQ 1 AND b-item.mat-type EQ "P" THEN bf-po-ordl.cons-uom = "TON".
    END. /* bf-po-ordl.item-type = yes */

    RELEASE bf-po-ordl.
    RELEASE b-item.
    RELEASE bf-ttJobMaterial.
    RELEASE bf-ordl.   

END PROCEDURE.
 
PROCEDURE pProcessAdders PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     processAdders
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrd      AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl       FOR po-ordl.  
    DEFINE BUFFER bf-po-ord        FOR po-ord.
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
     
    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-ERROR.
    FIND bf-po-ordl NO-LOCK WHERE ROWID(bf-po-ordl) EQ iprPoOrdl NO-ERROR.
    FIND bf-po-ord NO-LOCK WHERE ROWID(bf-po-ord) EQ iprPoOrd NO-ERROR.

    FIND job-mat NO-LOCK WHERE ROWID(job-mat) EQ bf-ttJobMaterial.w-RowID NO-ERROR.

    IF AVAILABLE job-mat AND AVAILABLE b-item AND index("1234BPR",b-item.mat-type) GT 0 AND b-item.i-code EQ "E" THEN                          
        RUN PO_ProcessAdder IN haPOProcs(ROWID(bf-po-ordl), 
                             ROWID(job-mat),
                             ipchCompany,
                             debasis-w,
                             deS-Len,
                             deS-Wid,
                             deS-dep,
                             deAdder).

    /* needed for po-vendc.i */
    FIND po-ordl EXCLUSIVE-LOCK WHERE ROWID(po-ordl) EQ ROWID(bf-po-ordl) .
    FIND po-ord EXCLUSIVE-LOCK WHERE ROWID(po-ord) EQ ROWID(bf-po-ord) .
    /* updates decost, v-qty deSetup, po-ordl.cost, po-ordl.cons-cost, deAdder[] */
    
    IF (chpoCost1 BEGINS "Vendor" OR po-ordl.job-no EQ "") AND po-ordl.item-type AND loNewFile THEN
        RUN pCalculateVendorCost.
                
    FIND CURRENT po-ordl NO-LOCK NO-ERROR.
    FIND CURRENT po-ord NO-LOCK NO-ERROR.
    
    RELEASE po-ordl.
    RELEASE po-ord.
    RELEASE bf-ttJobMaterial.

END PROCEDURE.
 
PROCEDURE pProcessExisting PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl       AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iplFirstOfFrm   AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat      AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrd        AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iploDropShip    AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER iploUpdatePO    AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER ipdaPODate      AS DATE  NO-UNDO.
    DEFINE INPUT  PARAMETER ipdaPODueDate   AS DATE  NO-UNDO.
    
    DEFINE BUFFER bf-ordl          FOR oe-ordl.
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.

    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-ERROR.
    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.
    FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ iprPoOrd NO-ERROR.

    IF iploUpdatePO AND AVAILABLE po-ord THEN 
    DO:
        IF AVAILABLE bf-ordl AND iplFirstOfFrm THEN
            FOR EACH b-oe-ordl
                WHERE b-oe-ordl.company EQ bf-ordl.company
                AND b-oe-ordl.ord-no  EQ bf-ordl.ord-no
                AND b-oe-ordl.job-no  EQ bf-ordl.job-no
                AND b-oe-ordl.job-no2 EQ bf-ordl.job-no2
                AND b-oe-ordl.i-no    EQ bf-ttJobMaterial.fg-i-no:
                ASSIGN
                    b-oe-ordl.po-no-po = po-ord.po-no
                    b-oe-ordl.vend-no  = chVendNo.
            END. /* each b-oe-ordl */
    END.  /* Chose not to update the PO */ 
    RELEASE bf-ttJobMaterial.         
END PROCEDURE.

PROCEDURE pBuildItemToPurchase PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        Inputs:
          job
          updates ttJobMaterial
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcOeAutoFg  AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER iprOeOrd     AS ROWID       NO-UNDO.
    DEFINE INPUT PARAMETER iprOeOrdl    AS ROWID       NO-UNDO.
    DEFINE INPUT PARAMETER iprJob       AS ROWID       NO-UNDO.
    DEFINE INPUT PARAMETER iplPoBest    AS LOGICAL     NO-UNDO.    
        
    DEFINE BUFFER bf-job  FOR job.
    DEFINE BUFFER bf-ord  FOR oe-ord.
    DEFINE BUFFER bf-ordl FOR oe-ordl.
        
    FIND bf-job  NO-LOCK WHERE ROWID(bf-job)  EQ iprJob    NO-ERROR.
    FIND bf-ord  NO-LOCK WHERE ROWID(bf-ord)  EQ iprOeOrd  NO-ERROR.
    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.
    FIND job NO-LOCK WHERE ROWID(job) EQ iprJob NO-ERROR.

    IF NOT AVAILABLE job THEN
        RETURN.
    /* Create ttJobMaterial based on tt-itemfg created above */
    
    ItemBlk: 
    FOR EACH tt-itemfg WHERE tt-itemfg.pur-man,
        FIRST itemfg NO-LOCK WHERE ROWID(itemfg) EQ tt-itemfg.row-id:
        
        CREATE ttJobMaterial.
        ASSIGN             
            ttJobMaterial.w-RowID      = ?
            ttJobMaterial.rm-i-no      = itemfg.i-no
            ttJobMaterial.i-no         = itemfg.i-no
            ttJobMaterial.fg-i-no      = itemfg.i-no
            ttJobMaterial.fg-part-no   = bf-ordl.part-no
            ttJobMaterial.est-no       = bf-ordl.est-no
            ttJobMaterial.frm          = tt-itemfg.form-no
            ttJobMaterial.blank-no     = tt-itemfg.blank-no
            ttJobMaterial.isaset       = itemfg.isaset
            ttJobMaterial.isacomponent = tt-itemfg.isacomponent
            ttJobMaterial.this-is-a-rm = NO
            ttJobMaterial.basis-w      = itemfg.t-wid * itemfg.t-len * 100
            ttJobMaterial.basis-w      = itemfg.weight-100 / (IF loMSFcalc THEN (ttJobMaterial.basis-w * .007)  ELSE (ttJobMaterial.basis-w / 144) / 1000)
            ttJobMaterial.qty-uom      = "EA"
            ttJobMaterial.sc-uom       = itemfg.pur-uom
            ttJobMaterial.qty          = tt-itemfg.qty     
            ttJobMaterial.ItemName     = itemfg.i-name           
            ttJobMaterial.ItemType     = IF ttJobMaterial.this-is-a-rm THEN "RM" ELSE "FG"
            ttJobMaterial.dropShipment = IF inOeAutoPONK1 EQ 1 AND deOeAutoFg EQ 1 THEN TRUE ELSE FALSE                
            ttJobMaterial.PODate       = TODAY 
            ttJobMaterial.PODueDate    = TODAY + 1
            ttJobMaterial.DropCustNo   = IF AVAILABLE bf-ord THEN bf-ord.cust-no ELSE "" 
            ttJobMaterial.ShipChoice   = IF AVAILABLE bf-ord THEN "C" ELSE "".
                
        IF ((chOEAutoFG  EQ "NonStock" OR ipcOeAutoFg EQ "Any") AND NOT itemfg.stocked) THEN 
        DO:
            ASSIGN 
                ttJobMaterial.IsValid       = TRUE 
                ttJobMaterial.InvalidReason = "".
        END.
        ELSE 
        DO:
            ASSIGN 
                ttJobMaterial.IsValid       = FALSE
                ttJobMaterial.InvalidReason = "NK1 setting is set to NonStock but Item: " + itemfg.i-no + " is Stock".
                
            NEXT ItemBlk.                
        END.
            
        IF ((ipcOeAutoFg EQ "LotCntrl" OR ipcOeAutoFg EQ "Any") AND NOT itemfg.ord-policy) THEN 
        DO:
            ASSIGN 
                ttJobMaterial.IsValid       = TRUE 
                ttJobMaterial.InvalidReason = "".
        END.
        ELSE 
        DO:
            ASSIGN 
                ttJobMaterial.IsValid       = FALSE
                ttJobMaterial.InvalidReason = "NK1 setting is set to LotCntrl but Item: " + itemfg.i-no + " Reorder Point".
            NEXT ItemBlk.                
        END.
            
        IF ((ipcOeAutoFg EQ "Avail<0"  OR ipcOeAutoFg EQ "Any") AND itemfg.q-avail LT 0) THEN 
        DO:
            ASSIGN 
                ttJobMaterial.IsValid       = TRUE 
                ttJobMaterial.InvalidReason = "".
        END.
        ELSE 
        DO:
            ASSIGN 
                ttJobMaterial.IsValid       = FALSE
                ttJobMaterial.InvalidReason = "NK1 setting is set to LotCntrl but Item: " + itemfg.i-no + " Reorder Point".
            NEXT ItemBlk.                
        END.
                
        IF loOEAutoFg AND loAutoFgSec AND AVAILABLE bf-ord AND AVAILABLE bf-ordl THEN 
        DO:                
            ASSIGN 
                ttJobMaterial.IsValid       = TRUE 
                ttJobMaterial.InvalidReason = "".
        END.
        ELSE 
        DO:
            ASSIGN 
                ttJobMaterial.IsValid = FALSE.
                    
            IF NOT AVAILABLE bf-ord THEN 
                ASSIGN ttJobMaterial.InvalidReason = "OE Order Master record not found.".
            ELSE IF NOT AVAILABLE bf-ordl THEN 
                    ASSIGN ttJobMaterial.InvalidReason = "OE Order Lines record not found.".
                ELSE IF loAutoFgSec = FALSE THEN 
                        ASSIGN ttJobMaterial.InvalidReason = "Auto FG item security not available.".
                    ELSE IF loOEAutoFg = FALSE THEN 
                            ASSIGN ttJobMaterial.InvalidReason = "OE Auto FG not available.".    
            NEXT ItemBlk.                    
        END.
                
        RUN pGetBestVendorWithCost(INPUT ROWID(ttJobMaterial),
            OUTPUT ttJobMaterial.VendorId,
            OUTPUT ttJobMaterial.CostPerUOM,
            OUTPUT ttJobMaterial.CostUOM,
            OUTPUT ttJobMaterial.CostSetup,
            OUTPUT ttJobMaterial.CostTotal).
    
        RUN po/GetFGDimsForPO.p(INPUT ROWID(itemfg), 
            OUTPUT ttJobMaterial.len, 
            OUTPUT ttJobMaterial.wid, 
            OUTPUT ttJobMaterial.dep).
            
        IF ttJobMaterial.VendorId = "" THEN
            ASSIGN ttJobMaterial.IsValid       = FALSE 
                   ttJobMaterial.InvalidReason = ttJobMaterial.InvalidReason + " | " + "Vendor Should not be blank.". 
    
    END. /* each tt-itemfg */

    /* Create ttJobMaterial from job-mat */
    JobMatBlk:
    FOR EACH job-mat NO-LOCK
        WHERE job-mat.company EQ bf-job.company
        AND job-mat.job     EQ bf-job.job
        AND job-mat.job-no  EQ bf-job.job-no
        AND job-mat.job-no2 EQ bf-job.job-no2
        AND NOT CAN-FIND(FIRST tt-itemfg
        WHERE tt-itemfg.form-no EQ job-mat.frm
        AND tt-itemfg.pur-man EQ YES
        AND tt-itemfg.isaset EQ NO),
        FIRST ITEM NO-LOCK
        WHERE item.company EQ job-mat.company
        AND item.i-no    EQ job-mat.rm-i-no:
            
        FIND FIRST b-jc-calc NO-LOCK
            WHERE b-jc-calc.reftable EQ "jc/jc-calc.p"
            AND b-jc-calc.company  EQ bf-job.company
            AND b-jc-calc.loc      EQ ""
            AND b-jc-calc.code     EQ STRING(bf-job.job,"999999999")
            AND b-jc-calc.val[12]  EQ job-mat.frm
            AND (b-jc-calc.val[13] EQ job-mat.blank-no OR job-mat.blank-no EQ 0)
            AND b-jc-calc.code2    NE bf-ordl.i-no NO-ERROR.
        
        CREATE ttJobMaterial.
        BUFFER-COPY job-mat TO ttJobMaterial
            ASSIGN
            ttJobMaterial.w-RowID       = RowID(job-mat)
            ttJobMaterial.this-is-a-rm  = YES
            ttJobMaterial.dep           = IF item.s-dep NE 0 THEN item.s-dep ELSE job-mat.dep
            ttJobMaterial.basis-w       = item.basis-w
            ttJobMaterial.fg-i-no       = IF AVAILABLE b-jc-calc THEN b-jc-calc.code2 ELSE bf-ordl.i-no
            ttJobMaterial.ItemName      = item.i-name
            ttJobMaterial.ItemType      = IF ttJobMaterial.this-is-a-rm THEN "RM" ELSE "FG"
            ttJobMaterial.dropShipment  = IF inOeAutoPONK1 EQ 1 AND deOeAutoFg EQ 1 THEN TRUE ELSE FALSE
            ttJobMaterial.PODate        = TODAY 
            ttJobMaterial.PODueDate     = TODAY + 1.

        IF iplPoBest = NO AND NOT CAN-DO("1,2,3,4,B,P,R",item.mat-type) THEN
        DO:
            ASSIGN 
                ttJobMaterial.IsValid       = FALSE 
                ttJobMaterial.InvalidReason = "Item Type for Item: " + ITEM.i-no + " is " + item.mat-type + " not present in 1,2,3,4,B,P,R.".
            NEXT JobMatBlk.            
        END.
        
        IF iplPoBest AND ITEM.mat-type NE "B" THEN
        DO:
            ASSIGN 
                ttJobMaterial.IsValid       = FALSE 
                ttJobMaterial.InvalidReason = "Item Type for Item: " + ITEM.i-no + " should be B".
            NEXT JobMatBlk.            
        END.
            
        IF AVAILABLE bf-job AND ((choeAutoPoNK1 NE "Manual" AND loAutoPoSec AND loPOBest EQ NO) OR loPOBest) THEN
        DO:
            ASSIGN 
                ttJobMaterial.IsValid       = TRUE 
                ttJobMaterial.InvalidReason = "".                
        END.
        ELSE 
        DO:
            ASSIGN 
                ttJobMaterial.IsValid = FALSE.
            
            IF NOT AVAILABLE bf-job THEN 
                ASSIGN ttJobMaterial.InvalidReason = "Job record not available.".
            ELSE 
                ASSIGN ttJobMaterial.InvalidReason = "Auto PO security not set.".
            NEXT JobMatBlk.
        END.
        
        RUN pGetBestVendorWithCost(INPUT ROWID(ttJobMaterial),
            OUTPUT ttJobMaterial.VendorId,
            OUTPUT ttJobMaterial.CostPerUOM,
            OUTPUT ttJobMaterial.CostUOM,
            OUTPUT ttJobMaterial.CostSetup,
            OUTPUT ttJobMaterial.CostTotal).
        
        IF ttJobMaterial.VendorId = "" THEN
            ASSIGN ttJobMaterial.IsValid       = FALSE 
                   ttJobMaterial.InvalidReason = ttJobMaterial.InvalidReason + " | " + "Vendor Should not be blank.".
        
    END. /* each job-mat */

    /* Create ttJobMaterial from item */
    JobMatBlk2:
    FOR EACH b-job-mat NO-LOCK WHERE
        b-job-mat.company EQ bf-job.company AND
        b-job-mat.job     EQ bf-job.job AND
        b-job-mat.job-no  EQ bf-job.job-no AND
        b-job-mat.job-no2 EQ bf-job.job-no2 AND
        NOT CAN-FIND(FIRST tt-itemfg WHERE
        tt-itemfg.form-no EQ b-job-mat.frm AND
        tt-itemfg.pur-man EQ YES),
        FIRST prep FIELDS(i-no number-up) NO-LOCK WHERE
        prep.company EQ b-job-mat.company AND
        prep.i-no EQ b-job-mat.i-no,
        
        FIRST ITEM  NO-LOCK WHERE
        item.company EQ b-job-mat.company AND
        item.i-no    EQ prep.i-no:        

        FIND FIRST b-jc-calc NO-LOCK WHERE
            b-jc-calc.reftable EQ "jc/jc-calc.p" AND
            b-jc-calc.company  EQ bf-job.company AND
            b-jc-calc.loc      EQ "" AND
            b-jc-calc.code     EQ STRING(bf-job.job,"999999999") AND
            b-jc-calc.val[12]  EQ b-job-mat.frm AND
            (b-jc-calc.val[13] EQ b-job-mat.blank-no OR b-job-mat.blank-no EQ 0) AND
            b-jc-calc.code2    NE bf-ordl.i-no
            NO-ERROR.        

        CREATE ttJobMaterial.
        BUFFER-COPY b-job-mat TO ttJobMaterial
        
        ASSIGN
            ttJobMaterial.w-RowID      = RowID(b-job-mat)
            ttJobMaterial.this-is-a-rm = YES
            ttJobMaterial.dep          = IF item.s-dep NE 0 THEN item.s-dep ELSE b-job-mat.dep
            ttJobMaterial.basis-w      = item.basis-w
            ttJobMaterial.fg-i-no      = IF AVAILABLE b-jc-calc THEN b-jc-calc.code2 ELSE bf-ordl.i-no
            ttJobMaterial.prep         = YES
            ttJobMaterial.rm-i-no      = b-job-mat.i-no
            ttJobMaterial.i-no         = b-job-mat.i-no
            ttJobMaterial.qty-uom      = "EA"
            ttJobMaterial.n-up         = b-job-mat.n-up
            ttJobMaterial.itemName      = ITEM.i-name
            ttJobMaterial.ItemType      = IF ttJobMaterial.this-is-a-rm THEN "RM" ELSE "FG"
            ttJobMaterial.DropShipment  = IF inOeAutoPONK1 EQ 1 AND deOeAutoFg EQ 1 THEN TRUE ELSE FALSE
            ttJobMaterial.PODate        = TODAY 
            ttJobMaterial.PODueDate     = TODAY + 1.
        
        IF NOT CAN-DO("7,8,M,X,Y",item.mat-type) THEN 
        DO:
            ASSIGN 
                ttJobMaterial.IsValid       = FALSE 
                ttJobMaterial.InvalidReason = "Item Type for Item: " + ITEM.i-no + " is " + item.mat-type + " not present in 7,8,M,X,Y.".
            NEXT JobMatBlk2.
        END.
            
        IF AVAILABLE bf-job AND ((choeAutoPoNK1 NE "Manual" AND loAutoPoSec AND loPOBest EQ NO) OR loPOBest) THEN 
        DO:
            ASSIGN 
                ttJobMaterial.IsValid       = TRUE 
                ttJobMaterial.InvalidReason = "".                
        END.
        ELSE 
        DO:
            ASSIGN 
                ttJobMaterial.IsValid       = FALSE.
                
            IF NOT AVAILABLE bf-job THEN 
                ASSIGN ttJobMaterial.InvalidReason = "Job record not available.".
            ELSE 
                ASSIGN ttJobMaterial.InvalidReason = "Auto PO security not set.".
                
            NEXT JobMatBlk2.                
        END.
            
        RUN pGetBestVendorWithCost(INPUT ROWID(ttJobMaterial),
            OUTPUT ttJobMaterial.VendorId,
            OUTPUT ttJobMaterial.CostPerUOM,
            OUTPUT ttJobMaterial.CostUOM,
            OUTPUT ttJobMaterial.CostSetup,
            OUTPUT ttJobMaterial.CostTotal).
            
        IF ttJobMaterial.VendorId = "" THEN
            ASSIGN ttJobMaterial.IsValid       = FALSE 
                   ttJobMaterial.InvalidReason = ttJobMaterial.InvalidReason + " | " + "Vendor Should not be blank.".
            
    END. /* for each b-job-mat */    

    /* Create ttJobMaterial  from job-prep */
    JobPrepBlk:
    FOR EACH job-prep NO-LOCK WHERE
        job-prep.company EQ bf-job.company AND
        job-prep.job     EQ bf-job.job AND
        job-prep.job-no  EQ bf-job.job-no AND
        job-prep.job-no2 EQ bf-job.job-no2 AND
        NOT CAN-FIND(FIRST tt-itemfg WHERE
        tt-itemfg.form-no EQ job-prep.frm AND
        tt-itemfg.pur-man EQ YES AND
        tt-itemfg.isaset EQ NO),
        FIRST prep FIELDS(i-no number-up)  NO-LOCK WHERE
        prep.company EQ job-prep.company AND
        prep.CODE EQ job-prep.CODE AND
        prep.i-no NE "",
        FIRST ITEM NO-LOCK WHERE
        item.company EQ job-prep.company AND
        item.i-no    EQ prep.i-no:
   
        FIND FIRST b-jc-calc WHERE
            b-jc-calc.reftable EQ "jc/jc-calc.p" AND
            b-jc-calc.company  EQ bf-job.company AND
            b-jc-calc.loc      EQ "" AND
            b-jc-calc.code     EQ STRING(bf-job.job,"999999999") AND
            b-jc-calc.val[12]  EQ job-prep.frm AND
            (b-jc-calc.val[13] EQ job-prep.blank-no OR job-prep.blank-no EQ 0) AND
            b-jc-calc.code2    NE bf-ordl.i-no
            NO-LOCK NO-ERROR.
              
        CREATE ttJobMaterial.
        BUFFER-COPY job-prep TO ttJobMaterial
            ASSIGN            
            ttJobMaterial.w-RowID       = RowID(job-prep)
            ttJobMaterial.this-is-a-rm  = YES
            ttJobMaterial.dep           = item.s-dep
            ttJobMaterial.basis-w       = item.basis-w
            ttJobMaterial.fg-i-no       = IF AVAILABLE b-jc-calc THEN b-jc-calc.code2 ELSE bf-ordl.i-no
            ttJobMaterial.prep          = YES
            ttJobMaterial.rm-i-no       = prep.i-no
            ttJobMaterial.i-no          = prep.i-no
            ttJobMaterial.qty-uom       = "EA"
            ttJobMaterial.n-up          = prep.number-up
            ttJobMaterial.ItemName      = ITEM.i-name
            ttJobMaterial.ItemType      = IF ttJobMaterial.this-is-a-rm THEN "RM" ELSE "FG"
            ttJobMaterial.dropShipment  = IF inOeAutoPONK1 EQ 1 AND deOeAutoFg EQ 1 THEN TRUE ELSE FALSE
            ttJobMaterial.PODate        = TODAY 
            ttJobMaterial.PODueDate     = TODAY + 1.
            
        IF AVAILABLE bf-job AND ((choeAutoPoNK1 NE "Manual" AND loAutoPoSec AND loPOBest EQ NO) OR loPOBest) THEN 
        DO:
            ASSIGN 
                ttJobMaterial.IsValid       = TRUE
                ttJobMaterial.InvalidReason = "".                
        END.
        ELSE 
        DO:
            ASSIGN 
                ttJobMaterial.IsValid = FALSE.
            IF NOT AVAILABLE bf-job THEN 
                ASSIGN ttJobMaterial.InvalidReason = "Job record not available.".
            ELSE 
                ASSIGN ttJobMaterial.InvalidReason = "Auto PO security not set.".
                
            NEXT JobPrepBlk.                  
        END.
        
        RUN pGetBestVendorWithCost(INPUT ROWID(ttJobMaterial),
            OUTPUT ttJobMaterial.VendorId,
            OUTPUT ttJobMaterial.CostPerUOM,
            OUTPUT ttJobMaterial.CostUOM,
            OUTPUT ttJobMaterial.CostSetup,
            OUTPUT ttJobMaterial.CostTotal).        
        
        IF ttJobMaterial.VendorId = "" THEN
            ASSIGN ttJobMaterial.IsValid       = FALSE 
                   ttJobMaterial.InvalidReason = ttJobMaterial.InvalidReason + " | " + "Vendor Should not be blank.".  
    END.    
END PROCEDURE.      
 
PROCEDURE pProcessJobMat PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lPoExists       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE roItem          AS ROWID   NO-UNDO.
    DEFINE VARIABLE roVend          AS ROWID   NO-UNDO.
    DEFINE VARIABLE loFirstJobFrm   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE loFirstOfJobFrm AS LOGICAL NO-UNDO.
   
    outers:
    FOR EACH ttJobMaterial    
        WHERE ttJobMaterial.CreatePO = TRUE
        BREAK BY ttJobMaterial.this-is-a-rm
              BY ttJobMaterial.frm
              BY ttJobMaterial.blank-no
              BY ttJobMaterial.i-no:

        ASSIGN 
            chVendNo      = ""
            roPoOrdl      = ?
            roItem        = ?
            roVend        = ?
            roPoOrd       = ?
            roItemFG      = ?
            chFilIdSource = ?.

        IF NOT ttJobMaterial.this-is-a-rm THEN 
        DO:
            FIND itemfg NO-LOCK WHERE itemfg.company EQ ttJobMaterial.Company
                AND itemfg.i-no EQ ttJobMaterial.i-no 
                NO-ERROR.
            IF AVAILABLE itemfg THEN
                roItemFG = ROWID(itemfg).
        END.
        ELSE 
        DO: 
            FIND item NO-LOCK WHERE item.company EQ ttJobMaterial.Company
                AND item.i-no EQ ttJobMaterial.i-no 
                NO-ERROR.
            IF AVAILABLE item THEN
                roItem = ROWID(item).
        END.
        
        FIND oe-ord NO-LOCK WHERE ROWID(oe-ord) EQ roOeOrd NO-ERROR.
        
        loNextOuters = NO.

        loFirstOfJobFrm = FIRST-OF(ttJobMaterial.frm).
        loFirstJobFrm   = FIRST(ttJobMaterial.frm).

        /* Sets gvrB-orderpo, initialize global variables and create a b-orderpo */
        RUN pInitJobVals (INPUT ttJobMaterial.Company,     // Varun - This can also be merged need to look more
            INPUT "",
            INPUT roJob,
            INPUT ROWID(ttJobMaterial),
            INPUT roOeOrdl,
            OUTPUT chJobNumber).

        /* Get deS-Len, deS-wid, deS-dep, deJobMatQty, deQtyComp, v-uom-comp */         
        RUN pInitRptRecs (INPUT ttJobMaterial.Company,
                          INPUT ROWID(ttJobMaterial)).

        ASSIGN 
            chVendNo = ttJobMaterial.vendorID.
        
        /* prompt for updating PO for given vendor and date */
        RUN pPromptUpdPoNum (INPUT ttJobMaterial.Company, 
            INPUT ttJobMaterial.po-no,
            INPUT ttJobMaterial.dropShipment,
            INPUT loUpdatePO, 
            OUTPUT roPoOrd,
            OUTPUT loNextOuters). /* set choice */

        IF loNextOuters THEN
            NEXT outers.

        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ roPoOrd NO-ERROR.
        FIND oe-ord NO-LOCK WHERE ROWID(oe-ord) EQ roOeOrd NO-ERROR.

        /* If they chose not to update existing then return */
        IF NOT loUpdatePO AND AVAIL(po-ord) THEN 
            NEXT outers.       
    
        /* update oe-ordl.po-no-po and vend-no */
        RUN pProcessExisting(INPUT ttJobMaterial.Company,
            INPUT roOeOrdl,
            INPUT loFirstOfJobFrm,
            INPUT ROWID(ttJobMaterial),
            INPUT roPoOrd,
            INPUT ttJobMaterial.DropShipment,
            INPUT loUpdatePO,
            INPUT ttJobMaterial.PODate,
            INPUT ttJobMaterial.PODueDate).
        
        /* Find existing PO for a due date and vendor. */
        IF loUpdatePO = TRUE THEN
            RUN pFindExistingPo (INPUT ttJobMaterial.Company,
                INPUT ttJobMaterial.po-no, 
                INPUT ttJobMaterial.DropShipment,
                INPUT ttJobMaterial.vendorID,
                INPUT ttJobMaterial.PODate,
                INPUT ttJobMaterial.PODueDate,
                OUTPUT lPoExists, 
                OUTPUT roPoOrd).
        
        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ roPoOrd NO-ERROR.
        FIND oe-ord NO-LOCK WHERE ROWID(oe-ord) EQ roOeOrd NO-ERROR.
          
        IF NOT lPoExists THEN 
        DO:                
            RUN pCreatePoOrd (INPUT ttJobMaterial.Company,
                INPUT roOeOrd, 
                INPUT ttJobMaterial.DropShipment, 
                OUTPUT roPoOrd,
                OUTPUT loNextOuters).
                
            IF loNextOuters THEN 
                NEXT outers.
                                
            FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ roPoOrd NO-ERROR.            
        END.

        /* Assign po values to oe-ordl and assign vend values to po-ord */
        RUN pSetPoValues(INPUT ttJobMaterial.Company,
            INPUT loFirstOfJobFrm, 
            INPUT roPoOrd,   
            INPUT roOeOrdl,
            INPUT ROWID(ttJobMaterial)).
                  
        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ roPoOrd NO-ERROR.

        IF NOT AVAILABLE po-ord THEN 
        DO:
            RUN pAddError(INPUT "Error: No Po found (after assign po values)", INPUT "4", INPUT 0, INPUT 0, INPUT 0).
            NEXT.
        END.

        FIND b-item NO-LOCK WHERE ROWID(b-item) EQ roItem NO-ERROR.
        FIND vend NO-LOCK WHERE ROWID(vend) EQ roVend NO-ERROR.       
        FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ roOeOrdl NO-ERROR.

        /* creates po-ordl */
        RUN pCreatePoOrdl (INPUT ttJobMaterial.Company,
            INPUT roPoOrd,
            INPUT roOeOrdl,
            INPUT ROWID(ttJobMaterial),
            INPUT roItem,
            OUTPUT roItemFG).

        FIND itemfg NO-LOCK WHERE ROWID(itemfg) EQ roItemFG NO-ERROR.

        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ roPoOrd NO-ERROR.
        IF NOT AVAILABLE po-ord THEN 
        DO:
            RUN pAddError(INPUT "Error: No Po found (after create po-ordl)", INPUT "4", INPUT 0, INPUT 0, INPUT 0).            
            NEXT outers.
        END.

        FIND po-ordl NO-LOCK WHERE ROWID(po-ordl) EQ roPoOrdl NO-ERROR.
        IF NOT AVAILABLE po-ordl THEN 
        DO:
            RUN pAddError(INPUT "PO Line not available, skipping it", INPUT "4", INPUT 0, INPUT 0, INPUT 0).            
            NEXT outers.
        END.

        /* set values from item and ttJobMaterial */
        IF po-ordl.item-type THEN
            RUN pSetPoOrdRm (INPUT roPoOrd,
                INPUT roPoOrdl,
                INPUT roItem,
                INPUT ROWID(ttJobMaterial),
                INPUT roOeOrdl,
                INPUT roJob).
        ELSE
            /* Set values from itemfg */
            RUN pSetPoOrdlFg (INPUT ttJobMaterial.Company,
                INPUT roPoOrd,
                INPUT roPoOrdl,
                INPUT roItemFG,
                INPUT ROWID(ttJobMaterial),
                INPUT roOeOrdl,
                INPUT roJob).
        
        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ roPoOrd NO-ERROR.
        IF NOT AVAILABLE po-ord THEN 
        DO:
            RUN pAddError(INPUT "Error: No Po found (after assign po-ordl)", INPUT "4", INPUT 0, INPUT 0, INPUT 0).            
            NEXT outers.
        END.

        FIND po-ordl NO-LOCK WHERE ROWID(po-ordl) EQ roPoOrdl NO-ERROR.

        IF NOT AVAILABLE po-ordl THEN 
        DO:
            RUN pAddError(INPUT 'Error: No Po Line available (main block)', INPUT "4", INPUT 0, INPUT 0, INPUT 0).
            
            NEXT outers.
        END.

        IF po-ordl.item-type THEN 
        DO: 
            /* get chNewItemNo, v-new-len, v-new-wid, set po-ordl.cons-uom and pr-uom */
            RUN pPoOrdlAddVals(INPUT roPoOrdl,
                INPUT roItem,
                INPUT ROWID(ttJobMaterial),
                INPUT roOeOrdl,
                OUTPUT loNextOuters).
                
            IF loNextOuters THEN 
                NEXT outers.                
        END. /* run poOrdlAddVals */ 
        
        /* Get len, wid, depth from item. Set po-ordl.cust-no */
        RUN pCalcLenWid (INPUT roPoOrd,
            INPUT roPoOrdl,
            INPUT roItem).

        /* get po-ordl.ord-qty from job-hdr, ttJobMaterial or oe-ordl */
        IF po-ordl.item-type THEN 
            RUN pCalcEstValues (INPUT roPoOrdl,
                INPUT roJob,
                INPUT ROWID(ttJobMaterial),
                INPUT roOeOrdl,
                INPUT roItem).
                
        /* Set po-ordl.s-num and b-num. UOM Conversion on po-ordl.ord-qty */
        RUN pCalcOrdQty    (INPUT roPoOrdl, INPUT ROWID(ttJobMaterial)).

        /* get po-ordl.cost from ttJobMaterial or deItemCost */
        RUN pCalcCostSetup (INPUT ROWID(ttJobMaterial),
            INPUT roPoOrdl).

        /*  Calculate v-tot-msf. Set po-ordl.s-len and s-wid */
        RUN pCalcMSF       (INPUT roPoOrdl).

        /* runs po/po-adder.p and po/po-vendc.i */
        RUN pProcessAdders (INPUT ttJobMaterial.Company, 
                            INPUT ROWID(ttJobMaterial), 
                            INPUT roPoOrdl, 
                            INPUT roPoOrd).

        /* UOM conversion for po-ordl.cost */
        RUN pCalcCost(INPUT ttJobMaterial.Company, 
                      INPUT roPoOrdl).

        RELEASE b-item.
  
        /* Warns user of zero length or width in Job */
        RUN pZeroLenWarning (INPUT ttJobMaterial.Company, INPUT roPoOrdl, OUTPUT roItem).

        FIND b-item WHERE ROWID(b-item) EQ roItem NO-LOCK NO-ERROR.
        FIND po-ordl WHERE ROWID(po-ordl) EQ roPoOrdl NO-LOCK NO-ERROR.

        IF (AVAILABLE b-item AND index("1234BPR",b-item.mat-type) GT 0) OR NOT po-ordl.item-type THEN 
        DO:    
            /* Validate than Length was entered */
            RUN pBrdLenCheck (INPUT roPoOrdl).

            /* UOM conversions to handle zere line qty or cost. Calculate oe-ordl.cost  */
            RUN pCheckZeroQty (INPUT roPoOrdl).

            /* Calculate po-ordl.t-cost and cons-cost */
            RUN pCalcExtCost (INPUT roPoOrdl).

            /* Find po-ordl and set v-old-i-no, v-tot-ord and po-ordl.dscr */
            RUN pAddHeaderTot (INPUT roPoOrdl).

            /* If chNewItemNo is a valid item, set ttJobMaterial.rm-i-no to it */
            RUN pAutoRm (INPUT ttJobMaterial.Company,
                         INPUT roPoOrdl,
                         INPUT ROWID(ttJobMaterial),
                         OUTPUT roItem) /* needs additional buffers */.

        END. /* if avail b-item ... */
                
        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ roPoOrd NO-ERROR.
        IF NOT AVAILABLE po-ord THEN 
        DO:
            RUN pAddError(INPUT "Internal Error - PO not found.", INPUT "3", INPUT 0, INPUT 0, INPUT 0).            
            NEXT outers.
        END.
        
        /* Check user po limit */
        RUN pCheckUserLimit(BUFFER po-ord).
        
        /* Calculate PO Header Totals */
        RUN PO_CalLineTotalandTax IN haPOProcs(ROWID(po-ord)).

        /* CHECK for exceeding vendor's max PO Cost and worn for it (set lohold) */
        RUN pValidMaxCost (roPoOrd).

        FIND po-ordl WHERE ROWID(po-ordl) EQ roPoOrdl NO-LOCK NO-ERROR.
        IF NOT AVAILABLE po-ordl THEN 
        DO:
            RUN pAddError(INPUT "Error - Po Line not found (22)", INPUT "3", INPUT 0, INPUT 0, INPUT 0).            
            NEXT outers.
        END.

        /* Update item inventory totals */
        RUN pPoordlUp (RowID(po-ordl), 1, loCountUpdate).

    END. /* each ttJobMaterial */
END PROCEDURE.
 
PROCEDURE pPromptUpdPoNum PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     If PO Already Exists, ask to Update it    
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoNo         AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER iploDropShip    AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER iploUpdatePO    AS LOGICAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER oprPoOrd        AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oploNextOuters  AS LOGICAL     NO-UNDO.

    DEFINE VARIABLE fil_id AS RowID NO-UNDO.
    
    oploNextOuters = FALSE.
    
    FIND LAST po-ord NO-LOCK
        WHERE po-ord.company EQ ipchCompany
        AND po-ord.po-no     EQ ipiPoNo
        AND po-ord.vend-no   EQ chVendNo
        AND (po-ord.type     EQ "D" OR NOT iploDropShip)
        NO-ERROR.

    IF AVAILABLE po-ord THEN
    DO:
        IF NOT po-ord.opened THEN 
            ASSIGN oploNextOuters = TRUE.
        ELSE 
            ASSIGN oprPoOrd = ROWID(po-ord).
    END. /* If PO was found but was not opened */
END PROCEDURE.
 
PROCEDURE pSetPoOrdlFg PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:    
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrd    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl   AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItemFg   AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl   AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprJob      AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    DEFINE BUFFER bf-po-ordl       FOR po-ordl.
    DEFINE BUFFER bf-po-ord        FOR po-ord.
    DEFINE BUFFER bf-itemfg        FOR itemfg.
    DEFINE BUFFER bf-ordl          FOR oe-ordl.
    DEFINE BUFFER bf-job           FOR job.

    FIND bf-po-ord   NO-LOCK WHERE ROWID(bf-po-ord)  EQ iprPoOrd NO-ERROR.
    FIND bf-itemfg  NO-LOCK WHERE ROWID(bf-itemfg)  EQ iprItemfg NO-ERROR.
    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-ERROR.
    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.
    FIND bf-job NO-LOCK WHERE ROWID(bf-job) EQ iprJob  NO-ERROR.

    FIND bf-po-ordl EXCLUSIVE-LOCK WHERE ROWID(bf-po-ordl) EQ iprPoOrdl NO-ERROR.
    
    IF NOT AVAILABLE bf-po-ordl THEN 
    DO:
        RUN pAddError(INPUT "Error: Po Line not available (setPoOrdlFg)", INPUT "1", INPUT 0, INPUT 0, INPUT 0).        
        RETURN ERROR.
    END.

    IF AVAILABLE bf-itemfg THEN 
    DO:        
        IF AVAILABLE bf-job THEN
            FIND FIRST xest NO-LOCK
                WHERE xest.company EQ bf-po-ordl.company
                AND xest.est-no  EQ bf-job.est-no
                NO-ERROR.
        IF AVAIL(xest) THEN
            FIND FIRST xeb NO-LOCK WHERE xeb.company = bf-po-ordl.company
                AND xeb.est-no = xest.est-no
                AND xeb.stock-no EQ bf-itemfg.i-no
                NO-ERROR.  
  
        ASSIGN
            bf-po-ordl.i-no       = bf-itemfg.i-no
            bf-po-ordl.i-name     = bf-ordl.i-name
            bf-po-ordl.pr-qty-uom = IF chPOUom EQ "Purchase" THEN bf-itemfg.pur-uom
                                                       ELSE bf-itemfg.prod-uom
            bf-po-ordl.cons-uom   = bf-itemfg.prod-uom
            bf-po-ordl.pr-uom     = bf-itemfg.pur-uom
            bf-po-ordl.cons-cost  = bf-itemfg.last-cost
            chPartDescr1          = bf-ordl.part-dscr1
            chPartDescr2          = bf-ordl.part-dscr2
            bf-po-ordl.ord-qty    = bf-ttJobMaterial.qty.
  
        IF AVAILABLE xeb THEN
            ASSIGN
                bf-po-ordl.s-num = xeb.form-no
                bf-po-ordl.b-num = xeb.blank-no.
            
        //bf-po-ordl.pr-uom = tt-ei.std-uom. - Varun
           
        bf-po-ordl.ord-no = IF AVAILABLE bf-ordl THEN bf-ordl.ord-no ELSE 0.
        FIND oe-ordl EXCLUSIVE-LOCK WHERE ROWID(oe-ordl) EQ reORDRowId NO-ERROR.
  
        IF AVAILABLE oe-ordl THEN
        DO:
            IF bf-po-ordl.cons-uom EQ "M" THEN
                oe-ordl.cost = bf-po-ordl.cons-cost.
            ELSE
                RUN Conv_ValueFromUOMtoUOM(ipchCompany,
                    bf-po-ordl.i-no,
                    bf-po-ordl.item-type,
                    bf-po-ordl.cons-cost,
                    bf-po-ordl.cons-uom,
                    "M",
                    0,
                    0,
                    0,
                    0,
                    0, 
                    OUTPUT oe-ordl.cost,
                    OUTPUT loError,
                    OUTPUT chMessage).
         
            RELEASE oe-ordl. 
        END. /* oe-ordl */
  
        IF bf-ordl.i-no NE bf-itemfg.i-no THEN
            ASSIGN
                bf-po-ordl.i-name = bf-itemfg.i-name
                chPartDescr1      = bf-itemfg.part-dscr1
                chPartDescr2      = bf-itemfg.part-dscr2.
  
        IF bf-po-ordl.pr-qty-uom NE "EA"                    AND
            (bf-po-ordl.item-type OR
            NOT DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-po-ordl.i-no, bf-po-ordl.pr-qty-uom)) THEN
            RUN Conv_QuantityFromUOMToUOM(ipchCompany,
                bf-ttJobMaterial.i-no,
                bf-ttJobMaterial.itemType,
                bf-po-ordl.ord-qty,
                "EA", 
                bf-po-ordl.pr-qty-uom,
                bf-ttJobMaterial.basis-w, 
                bf-ttJobMaterial.len, 
                bf-ttJobMaterial.wid, 
                bf-ttJobMaterial.dep,
                0, 
                OUTPUT bf-po-ordl.ord-qty,
                OUTPUT loError,
                OUTPUT chMessage).
  
        FOR EACH prodl FIELDS(prolin)
            WHERE prodl.company EQ ipchCompany
            AND prodl.procat  EQ bf-itemfg.procat
            NO-LOCK,
            FIRST prod FIELDS(fg-mat)
            WHERE prod.company EQ ipchCompany
            AND prod.prolin  EQ prodl.prolin
            NO-LOCK:
  
            bf-po-ordl.actnum = prod.fg-mat.
            LEAVE.
        END. /* each prodl */
  
        IF bf-itemfg.vend-no EQ bf-po-ord.vend-no THEN
            bf-po-ordl.vend-i-no = bf-itemfg.vend-item.
        ELSE IF bf-itemfg.vend2-no EQ bf-po-ord.vend-no THEN
                bf-po-ordl.vend-i-no = bf-itemfg.vend2-item.
  
        /* populate GL# from reftable if it exists using bf-itemfg AH 02-23-10*/
        ASSIGN 
            chCharge = "".
        FIND FIRST surcharge WHERE surcharge.company = ipchCompany
            AND surcharge.charge <> "" NO-LOCK NO-ERROR.
        IF AVAILABLE surcharge THEN
            ASSIGN chCharge = surcharge.charge.
        FIND FIRST fgcat NO-LOCK WHERE fgcat.company  EQ bf-itemfg.company
            AND fgcat.procat      EQ bf-itemfg.procat
            NO-ERROR.

        IF AVAILABLE fgcat 
            AND fgcat.miscCharge  EQ chCharge 
            AND fgcat.brdExpAcct <> "" THEN 
            ASSIGN bf-po-ordl.actnum = fgcat.brdExpAcct.


    END. /* avail bf-itemfg */

    FIND CURRENT bf-po-ordl NO-LOCK NO-ERROR.
    RELEASE bf-po-ordl.
    RELEASE bf-ttJobMaterial.

END PROCEDURE.
 
PROCEDURE pSetPoOrdRm PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrd    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl   AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItem     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl   AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprJob      AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    DEFINE BUFFER bf-po-ordl       FOR po-ordl.
    DEFINE BUFFER bf-po-ord        FOR po-ord.
    DEFINE BUFFER b-item           FOR ITEM .
    DEFINE BUFFER bf-ordl          FOR oe-ordl.
    DEFINE BUFFER bf-job           FOR job.
    
    DEFINE VARIABLE chActnum AS CHARACTER NO-UNDO.

    FIND bf-po-ordl EXCLUSIVE-LOCK WHERE ROWID(bf-po-ordl) EQ iprPoOrdl NO-ERROR.
    FIND bf-po-ord NO-LOCK WHERE ROWID(bf-po-ord) EQ iprPoOrd NO-ERROR.
    FIND b-item NO-LOCK WHERE ROWID(b-item) EQ iprItem NO-ERROR.
    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-ERROR.
    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.
    FIND bf-job NO-LOCK WHERE ROWID(bf-job) EQ iprJob NO-ERROR.

    ASSIGN 
        chActnum = "".
    FIND FIRST costtype NO-LOCK
        WHERE costtype.company   EQ bf-po-ordl.company
        AND costtype.loc       EQ bf-po-ord.loc
        AND costtype.cost-type EQ b-item.cost-type
        NO-ERROR.
    IF AVAILABLE costtype AND loAPGL THEN
        bf-po-ordl.actnum = IF chAPGL EQ "Asset"   THEN costtype.inv-asset
        ELSE
            IF chAPGL BEGINS "Exp"  AND
            (chAPGL EQ "Expense" OR costtype.cons-exp NE "")
            THEN costtype.cons-exp
            ELSE bf-po-ordl.actnum.


    /* populate GL# from job-hdr.i-no + itemfg tables, then reftable AH 02-24-10*/
    IF reJobRowid <> ? THEN 
    DO:
        FIND b-job-hdr NO-LOCK WHERE RowID(b-job-hdr) = reJobRowid NO-ERROR.

        /* Get gl actnum for a itemfg */
        IF AVAILABLE b-job-hdr THEN 
            RUN pGetItemfgGL (INPUT b-job-hdr.company, b-job-hdr.i-no, OUTPUT chActnum).

        IF chActnum <> "" THEN 
            ASSIGN bf-po-ordl.actnum = chActnum.
    END.
    RELEASE b-job-hdr.

    ASSIGN
        bf-po-ordl.s-num      = bf-ttJobMaterial.frm
        bf-po-ordl.b-num      = bf-ttJobMaterial.blank-no
        bf-po-ordl.job-no     = bf-job.job-no
        bf-po-ordl.job-no2    = bf-job.job-no2
        bf-po-ordl.i-no       = b-item.i-no
        bf-po-ordl.i-name     = b-item.i-name
        bf-po-ordl.pr-qty-uom = bf-ttJobMaterial.qty-uom
        bf-po-ordl.cons-uom   = b-item.cons-uom
        bf-po-ordl.pr-uom     = b-item.pur-uom
        bf-po-ordl.cons-cost  = b-item.last-cost
        chPartDescr1          = b-item.i-dscr
        chPartDescr2          = b-item.est-dscr.
   
    ASSIGN 
        bf-po-ordl.pr-qty-uom = IF chPOUom EQ "Purchase" THEN b-item.pur-uom
                                                                      ELSE b-item.cons-uom .
   
    bf-po-ordl.ord-no = IF AVAILABLE bf-ordl THEN bf-ordl.ord-no ELSE 0.
    FIND CURRENT bf-po-ordl NO-LOCK.
    RELEASE bf-po-ordl.
    RELEASE bf-ttJobMaterial.

END PROCEDURE.
 
PROCEDURE pSetPoValues PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     procedure Assign PO ord values from vend and sales order
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplFirstOfFrm AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrd      AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat    AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-ordl          FOR oe-ordl.
    DEFINE BUFFER bf-po-ord        FOR po-ord.
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.

    FIND bf-po-ord EXCLUSIVE-LOCK WHERE ROWID(bf-po-ord) EQ iprPoOrd NO-ERROR.
    FIND bf-ordl EXCLUSIVE-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.
    FIND bf-ttJobMaterial  NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-ERROR.

    IF AVAILABLE bf-ordl AND iplFirstOfFrm AND AVAILABLE bf-po-ord THEN 
        FOR EACH b-oe-ordl
            WHERE b-oe-ordl.company EQ bf-ordl.company
            AND b-oe-ordl.ord-no  EQ bf-ordl.ord-no
            AND b-oe-ordl.job-no  EQ bf-ordl.job-no
            AND b-oe-ordl.job-no2 EQ bf-ordl.job-no2
            AND b-oe-ordl.i-no    EQ bf-ttJobMaterial.fg-i-no
            EXCLUSIVE-LOCK:
            /* for testing put this back!!! */

            ASSIGN
                b-oe-ordl.po-no-po = bf-po-ord.po-no
                b-oe-ordl.vend-no  = chVendNo.
        END.

    FIND CURRENT bf-po-ord EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAILABLE bf-po-ord THEN 
    FIND FIRST vend
        WHERE vend.company EQ ipchCompany 
        AND vend.vend-no EQ bf-po-ord.vend-no 
        USE-INDEX vend NO-LOCK NO-ERROR.
    IF AVAILABLE vend AND AVAILABLE bf-po-ord THEN 
    DO:
        ASSIGN
            bf-po-ord.last-ship-date = bf-po-ord.due-date
            bf-po-ord.over-pct       = vend.over-pct
            bf-po-ord.under-pct      = vend.under-pct
            bf-po-ord.carrier        = vend.carrier
            bf-po-ord.contact        = vend.contact
            bf-po-ord.terms          = vend.terms
            bf-po-ord.fob-code       = vend.fob-code
            bf-po-ord.frt-pay        = vend.frt-pay
            bf-po-ord.tax-gr         = vend.tax-gr.
     
        IF bf-po-ord.fob-code EQ "" THEN bf-po-ord.fob = "DEST".
        IF bf-po-ord.frt-pay  EQ "" THEN bf-po-ord.frt-pay = "B".
    
        IF bf-po-ord.due-date LT bf-po-ord.po-date THEN
            ASSIGN
                bf-po-ord.due-date       = bf-po-ord.po-date
                bf-po-ord.last-ship-date = bf-po-ord.due-date.
    END. /* If avail vend */

    IF AVAILABLE bf-ordl AND AVAILABLE bf-po-ord AND bf-po-ord.type EQ "D"    AND
        bf-po-ord.cust-no NE "" AND bf-po-ord.frt-pay NE "P" THEN
    DO:
        FIND FIRST oe-rel OF bf-ordl NO-LOCK NO-ERROR.
        IF AVAILABLE oe-rel THEN bf-po-ord.carrier = oe-rel.carrier.
    END.

    IF AVAILABLE bf-ord AND AVAILABLE bf-po-ord THEN
        ASSIGN bf-po-ord.frt-pay  = bf-ord.frt-pay
            bf-po-ord.fob-code = bf-ord.fob-code.
            
    FIND CURRENT bf-po-ord NO-LOCK NO-ERROR.
    RELEASE bf-po-ord.
    RELEASE bf-ordl.
    RELEASE bf-ttJobMaterial.
/* end procedure Assign PO ord values from vend and sales order */
END PROCEDURE.
 
PROCEDURE pItemFGFromJob PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      Input:
        job buffer
        b-tt-itemfg?
        bf-ordl
      Output:
        creates tt-itemfg records
        create tt-fg-set
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprJob         AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl      AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFilIdSource AS CHARACTER   NO-UNDO.
    
    DEFINE BUFFER bf-ordl   FOR oe-ordl.
    /* Check that cFilIDSource is JOB since job record may be available here    */
    /* even though the fil_id is related to oe-ordl and could cause a duplicate */
    /* tt-itemfg                                                                */
    
    DEFINE BUFFER bf-itemfg FOR Itemfg.
    FIND job NO-LOCK WHERE ROWID(job) EQ iprJob NO-ERROR.
    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.
  
    IF AVAILABLE job AND ipcFilIdSource = "JOB" THEN 
    DO:
        FOR EACH job-hdr NO-LOCK
            WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2,
            FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ job-hdr.company
            AND itemfg.i-no    EQ job-hdr.i-no:
       
            FIND FIRST eb NO-LOCK WHERE 
                eb.company EQ job-hdr.company AND
                eb.est-no EQ job-hdr.est-no AND
                eb.stock-no EQ job-hdr.i-no AND
                eb.blank-no EQ job-hdr.blank-no AND
                eb.form-no EQ job-hdr.frm
                NO-ERROR.
  
            CREATE tt-itemfg.
            BUFFER-COPY itemfg EXCEPT rec_key pur-man TO tt-itemfg
                ASSIGN
                tt-itemfg.form-no  = job-hdr.frm
                tt-itemfg.blank-no = job-hdr.blank-no
                tt-itemfg.qty      = IF AVAILABLE bf-ordl THEN bf-ordl.qty ELSE job-hdr.qty
                tt-itemfg.row-id   = ROWID(itemfg). 
            tt-itemfg.pur-man = IF AVAILABLE eb THEN eb.pur-man ELSE itemfg.pur-man.
        END.
    
        /* WFK - needs more explanation, why would this be found? */
        /* was in original versions of program                    */
        FIND b-tt-itemfg EXCLUSIVE-LOCK NO-ERROR.
    
        IF AVAILABLE b-tt-itemfg THEN 
        DO:
            IF b-tt-itemfg.isaset THEN 
            DO:
                b-tt-itemfg.pur-man = NO.
          
                FOR EACH b-jc-calc NO-LOCK
                    WHERE b-jc-calc.reftable EQ "jc/jc-calc.p"
                    AND b-jc-calc.company  EQ job.company
                    AND b-jc-calc.loc      EQ ""
                    AND b-jc-calc.code     EQ STRING(job.job,"999999999"),
                    FIRST itemfg NO-LOCK
                    WHERE itemfg.company EQ b-jc-calc.company
                    AND itemfg.i-no    EQ b-jc-calc.code2
                    AND itemfg.pur-man:
                    
                    CREATE tt-itemfg.
                    BUFFER-COPY itemfg EXCEPT rec_key TO tt-itemfg
                        ASSIGN
                        tt-itemfg.form-no  = b-jc-calc.val[12]
                        tt-itemfg.blank-no = b-jc-calc.val[13]
                        tt-itemfg.row-id   = ROWID(itemfg). 
              
                    IF AVAILABLE bf-ordl THEN
                        FIND FIRST bf-itemfg NO-LOCK
                            WHERE bf-itemfg.company EQ bf-ordl.company
                            AND bf-itemfg.i-no    EQ bf-ordl.i-no
                            NO-ERROR.
                    IF AVAILABLE bf-itemfg THEN
                        RUN pFullSet (ROWID(bf-itemfg)).
          
                    FOR EACH tt-fg-set WHERE tt-fg-set.part-no EQ b-jc-calc.code2:
                        tt-itemfg.qty = tt-itemfg.qty + (b-tt-itemfg.qty * tt-fg-set.part-qty-dec).
                    END. /* each tt-fg-set */
                END. /* each b-jc-calc */
            END. /* if b-tt-itemfg.isaset */
       
            IF b-tt-itemfg.pur-man EQ NO THEN DELETE b-tt-itemfg.
        END. /* if avail bb-tt-itemfg */
    END. /* if avail job... */
END PROCEDURE.
 
PROCEDURE pValidMaxCost PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     Validate Maximum P.O. Cost 
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrd AS ROWID       NO-UNDO.
    
    DEFINE VARIABLE deTotalCost AS DECIMAL DECIMALS 4 NO-UNDO.
    DEFINE VARIABLE lohold      AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-po-ord FOR po-ord.
    DEFINE BUFFER b-po-ordl FOR po-ordl.

    FIND bf-po-ord WHERE ROWID(bf-po-ord) EQ iprPoOrd EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE bf-po-ord THEN 
    DO:
        RUN pAddError(INPUT "Internal error - No PO found.", INPUT "1", INPUT 0, INPUT 0, INPUT 0).        
        RETURN. 
    END.

    IF NOT lohold AND bf-po-ord.stat NE "H" AND
        AVAILABLE vend AND vend.rebate-% NE 0 THEN
    DO:
        deTotalCost = 0.
        FOR EACH b-po-ordl FIELDS(t-cost) NO-LOCK WHERE
            b-po-ordl.company EQ bf-po-ord.company AND
            b-po-ordl.po-no EQ bf-po-ord.po-no
            :

            deTotalCost = deTotalCost + b-po-ordl.t-cost.
        END.

        IF deTotalCost GT vend.rebate-% THEN
        DO:
            RUN pAddError(INPUT "Purchase Order Cost Has Exceeded Vendor's Max P.O. Cost." + CHR(10) + "Purchase Order Will Be Placed On Hold.", 
                INPUT "4", INPUT 0, INPUT 0, INPUT 0).
            ASSIGN 
                lohold         = YES
                bf-po-ord.stat = "H".
        END. /* deTotalCost gt vend.rebate-% */
    END. /* not lohold ...*/
END PROCEDURE.
 
PROCEDURE pZeroLenWarning PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprItem       AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl FOR po-ordl.  
    DEFINE BUFFER b-item     FOR ITEM.
    
    FIND bf-po-ordl NO-LOCK WHERE ROWID(bf-po-ordl) EQ iprPoOrdl NO-ERROR.

    IF bf-po-ordl.item-type THEN /*DO*/
        FIND FIRST b-item NO-LOCK 
            WHERE b-item.company EQ ipchCompany
            AND b-item.i-no    EQ bf-po-ordl.i-no
            USE-INDEX i-no NO-ERROR.
            
    IF AVAILABLE b-item THEN
        oprItem = ROWID(b-item).
    IF AVAILABLE b-item AND b-item.i-code EQ "E"
        AND index("1234BPRWF",b-item.mat-type) GT 0 THEN 
    DO:
        IF (deS-Len EQ 0 OR deS-wid EQ 0) THEN 
        DO:
            RUN pAddError(INPUT "Invalid Length or Width. Enter Valid Job or Non-Zero Length and Width", 
                INPUT "4", INPUT 0, INPUT 0, INPUT 0).            
        /* return ??*/          
        END. /* deS-Len eq 0 ... */
    END. /* avail b-item ... */
END PROCEDURE.
 
PROCEDURE pCheckFGItemCustHold:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER ipchCompany             AS CHARACTER       NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItem               AS CHARACTER       NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER oplFgItemCustHold AS LOGICAL         NO-UNDO.

    DEFINE VARIABLE cCurrentTitle    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCurrentMessage  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuppressMessage AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bff-itemfg FOR itemfg.  
    
    FIND FIRST bff-itemfg NO-LOCK
        WHERE bff-itemfg.company EQ ipchCompany
        AND bff-itemfg.i-no    EQ ipcFGItem NO-ERROR.
        
    IF AVAILABLE bff-itemfg AND bff-itemfg.cust-no NE ""  THEN 
    DO:
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ ipchCompany 
            AND cust.cust-no EQ bff-itemfg.cust-no NO-ERROR .
        IF AVAILABLE cust AND cust.cr-hold THEN 
            RUN displayMessageQuestionLOG ("12", OUTPUT oplFgItemCustHold). // varun 
    END.
    
END PROCEDURE.
 
PROCEDURE pCheckUserLimit PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-po-ord FOR po-ord.
    
    DEFINE VARIABLE lHoldPoStatus  AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dPurchaseLimit AS DECIMAL NO-UNDO.

    IF TRIM(chPOStatus) = "User Limit" THEN
    DO:          
        RUN PO_CheckPurchaseLimit IN haPOProcs(BUFFER ipbf-po-ord, OUTPUT lHoldPoStatus, OUTPUT dPurchaseLimit) .
        IF lHoldPoStatus THEN 
        DO:
            FIND CURRENT ipbf-po-ord EXCLUSIVE-LOCK NO-ERROR.
            
            ipbf-po-ord.stat    = "H". 
            scInstance = SharedConfig:instance.
            scInstance:SetValue("PurchaseLimit",TRIM(STRING(dPurchaseLimit))).
            
            //RUN displayMessage ( INPUT 57).  
            FIND CURRENT ipbf-po-ord NO-LOCK NO-ERROR.
        END.        
    END.               
END PROCEDURE.



/* ************************  Function Implementations ***************** */
FUNCTION fGetVendCostQty RETURNS DECIMAL(ipdQty AS DECIMAL, ipcFromUom AS CHARACTER, ipcToUom AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ReturnQty AS DECIMAL NO-UNDO.
       
    IF ipcToUom = ipcFromUom THEN ReturnQty = ipdQty.
    ELSE 
    DO:                                  
        RUN Conv_QuantityFromUOMToUOM(ttJobMaterial.company,
            ttJobMaterial.i-no,
            ttJobMaterial.ItemType,
            ipdQty,
            ipcFromUom, 
            ipcToUom, 
            ttJobMaterial.basis-w, 
            ttJobMaterial.len, 
            ttJobMaterial.wid, 
            ttJobMaterial.dep, 
            0, 
            OUTPUT ReturnQty,
            OUTPUT loError,
            OUTPUT chMessage) .    
    END. 
       
    RETURN ReturnQty.

END FUNCTION.

FUNCTION fnRoundUp RETURNS INTEGER(INPUT deNumber AS DECIMAL):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE inResult AS INTEGER NO-UNDO.

    IF (deNumber - integer(deNumber)) > 0 THEN 
        ASSIGN inResult = INTEGER(deNumber) + 1.
    ELSE 
        ASSIGN inResult = INTEGER(deNumber).   

    RETURN inResult.
		
END FUNCTION.

