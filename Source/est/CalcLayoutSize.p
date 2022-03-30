
/*------------------------------------------------------------------------
    File        : CalcLayoutSize.p
    Purpose     : replaces the calc-dim.p and calc-dim1.p procedure called from the various b-estitm.w

    Syntax      : Accepts ROWID for ef

    Description : Calculates the dimensions of various ef fields for the Layout Tab

    Author(s)   : sakshi.singh
    Created     : Fri Dec 10 03:25:24 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

{est/ttCalcLayoutSize.i}

DEFINE INPUT PARAMETER ipriEf           AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipriEb           AS ROWID NO-UNDO.
DEFINE INPUT  PARAMETER iplCalcSizeOnly AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE           FOR ttLayoutSize.


/* ********************  Preprocessor Definitions  ******************** */

/*Refactor Globals or poorly defined shared temp-tables*/
DEFINE TEMP-TABLE formule NO-UNDO
    FIELD formule AS DECIMAL EXTENT 12
    .



DEFINE VARIABLE isFoamStyle       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cIndustryType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE dTrimLength       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dTrimWidth        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dCalcTotalLength  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dCalcTotalWidth   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dWidtoUse         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dLengthtoUse      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iCalcNumOnWidth   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCalcNumOnLength  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCnt              AS INTEGER   NO-UNDO.
DEFINE VARIABLE iExt              AS INTEGER   NO-UNDO.
DEFINE VARIABLE dSwapDim          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dTempLength       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dTempWidth        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lRound            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lDecimal          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE dDecimalFactor    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dDecimalMax       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dConversionFactor AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iStyleFormulaOnLen LIKE style.use-l   NO-UNDO.
DEFINE VARIABLE iStyleFormulaOnWid LIKE style.use-w   NO-UNDO.



DEFINE VARIABLE ceroute-dec AS DECIMAL NO-UNDO. // ASk  BRAD


    

DEFINE BUFFER bf-style   FOR style.
DEFINE BUFFER bf-item    FOR item.
DEFINE BUFFER bf-ef      FOR ef.
DEFINE BUFFER bf-eb      FOR eb.
DEFINE BUFFER bf-mach    FOR mach.
DEFINE BUFFER bf-ce-ctrl FOR ce-ctrl.


/* ************************  Function Prototypes ********************** */

FUNCTION fRound RETURNS LOGICAL 
    ( ipcCompany AS CHARACTER ) FORWARD.



/* ***************************  Main Block  *************************** */

FIND bf-ef WHERE ROWID(bf-ef) EQ ipriEf NO-ERROR.
FIND bf-eb WHERE ROWID(bf-eb) EQ ipriEb NO-ERROR.

IF NOT AVAILABLE bf-ef OR NOT AVAILABLE bf-eb THEN
    RETURN.

FIND FIRST bf-item NO-LOCK
    WHERE bf-item.company = bf-ef.company 
    AND bf-item.i-no EQ bf-ef.board NO-ERROR.

FIND FIRST bf-style NO-LOCK 
    WHERE bf-style.company = bf-ef.company  
    AND bf-style.style = bf-eb.style NO-ERROR.
    
IF AVAILABLE bf-style THEN 
    ASSIGN
        cIndustryType = IF bf-style.industry = "1" THEN "Folding" ELSE "Corrugated"
        isFoamStyle   = YES WHEN bf-style.type = "F"
        iStyleFormulaOnLen = bf-style.use-l
        iStyleFormulaOnWid = bf-style.use-w.


IF iplCalcSizeOnly THEN
    RUN pCalcLayoutSizeOnly.
ELSE
    RUN pCalcLayout.   


PROCEDURE pCalcLayoutSizeOnly:
    
    /* Create Temp-table record and populate the fields later */    
    CREATE ttLayoutSize.    
    
    /* Initializing the fields using previous calculated DB values and then re-assigning as per logic below */
    ASSIGN
        ttLayoutSize.dGrossSheetLength  = bf-ef.gsh-len
        ttLayoutSize.dGrossSheetWidth   = bf-ef.gsh-wid
        ttLayoutSize.dGrossSheetDepth   = bf-ef.gsh-dep
        ttLayoutSize.dNetSheetLength    = bf-ef.nsh-len
        ttLayoutSize.dNetSheetWidth     = bf-ef.nsh-wid 
        ttLayoutSize.dNetSheetDepth     = bf-ef.nsh-dep
        ttLayoutSize.dDieSizeWidth      = bf-ef.trim-w
        ttLayoutSize.dDieSizeLength     = bf-ef.trim-l
        ttLayoutSize.dDieSizeDepth      = bf-ef.trim-d 
        ttLayoutSize.dLayoutSheetLength = bf-ef.lsh-len
        ttLayoutSize.dLayoutSheetWidth  = bf-ef.lsh-wid
        . 
    
    
    IF bf-ef.m-code NE "" THEN
    DO:
        FIND FIRST bf-mach 
            WHERE bf-mach.company = bf-ef.company  
            AND bf-mach.loc     = bf-ef.loc
            AND bf-mach.m-code = bf-ef.m-code  NO-LOCK NO-ERROR.
    
        IF AVAILABLE bf-mach THEN 
            ASSIGN 
                dTrimLength = bf-mach.min-triml * 2
                dTrimWidth  = bf-mach.min-trimw * 2.
                
                
    END.
    ELSE
    DO:
        FIND FIRST bf-ce-ctrl NO-LOCK
            WHERE bf-ce-ctrl.company = bf-ef.company  
            AND bf-ce-ctrl.loc     = bf-ef.loc NO-ERROR.
        
        ASSIGN 
            dTrimLength = bf-ce-ctrl.ls-triml * 2
            dTrimWidth  = bf-ce-ctrl.ls-trimw * 2.
    END.
    
    RUN pGetDecimalSettings(bf-eb.company,  
        cIndustryType,
        OUTPUT lDecimal,
        OUTPUT dDecimalFactor,
        OUTPUT dDecimalMax, 
        OUTPUT dConversionFactor,
        OUTPUT lRound).      
     
            
    RUN pGetFormulaTT (BUFFER bf-eb,
        INPUT cIndustryType, 
        INPUT lDecimal, 
        INPUT dDecimalFactor, 
        INPUT dDecimalMax, 
        INPUT dConversionFactor, 
        INPUT lRound,
        OUTPUT TABLE formule).
    
    /*formule is populated by CalcStyleFormulae*/
    FIND FIRST formule NO-LOCK NO-ERROR.
    
    /* Calculate Width and lentgh Size using Panel dimension for xGrain S or B */    
    
    IF cIndustryType = "Folding" THEN
    DO:
        IF bf-ef.xgrain = "B" OR bf-ef.xgrain = "S" THEN
        DO:
            DO iCnt = 1 TO bf-eb.num-len:
                iExt = iCnt.
                IF iCnt > 13 THEN iExt = 13.
               
                iCalcNumOnLength  = iCnt.
                dCalcTotalLength = dCalcTotalLength + formule[iStyleFormulaOnWid[iExt] + (iStyleFormulaOnWid[iExt] - 1)].
            END.
            DO iCnt = 1 TO bf-eb.num-wid:
                iExt = iCnt.
                IF iCnt > 13 THEN iExt = 13.
                iCalcNumOnWidth  = iCnt.
                dCalcTotalWidth = dCalcTotalWidth + formule[iStyleFormulaOnLen[iExt] * 2].
            END.
       
        END.
        ELSE 
        DO:
            DO iCnt = 1 TO bf-eb.num-wid:
                iExt = iCnt.
                IF iCnt > 13 THEN iExt = 13.
                iCalcNumOnWidth  = iCnt.
                dCalcTotalWidth = dCalcTotalWidth + formule[iStyleFormulaOnLen[iExt] + (iStyleFormulaOnLen[iExt] - 1)].
            END.
            DO iCnt = 1 TO bf-eb.num-len:
                iExt = iCnt.
                IF iCnt > 13 THEN iExt = 13.
                iCalcNumOnLength  = iCnt.
                dCalcTotalLength = dCalcTotalLength + formule[iStyleFormulaOnWid[iExt] * 2].
            END.
            
        END.
    END. /* Folding */
    /* Corrugated */
    ELSE
    DO:
        IF bf-ef.xgrain = "B" THEN
        DO:
            DO iCnt = 1 TO bf-eb.num-wid:
                iExt = iCnt.
                IF iCnt > 13 THEN iExt = 13.
               
                iCalcNumOnLength  = iCnt.
                dCalcTotalLength = dCalcTotalLength + formule[iStyleFormulaOnWid[iExt] + (iStyleFormulaOnWid[iExt] - 1)].
            END.
            DO iCnt = 1 TO bf-eb.num-len:
                iExt = iCnt.
                IF iCnt > 13 THEN iExt = 13.
                iCalcNumOnWidth  = iCnt.
                dCalcTotalWidth = dCalcTotalWidth + formule[iStyleFormulaOnLen[iExt] * 2].
            END.
       
        END.
        ELSE 
        DO:  
            DO iCnt = 1 TO bf-eb.num-len:
                iExt = iCnt.
                IF iCnt > 13 THEN iExt = 13.
                iCalcNumOnWidth  = iCnt.
                dCalcTotalWidth = dCalcTotalWidth + formule[iStyleFormulaOnLen[iExt] + (iStyleFormulaOnLen[iExt] - 1)].
            END.
            DO iCnt = 1 TO bf-eb.num-wid:
                iExt = iCnt.
                IF iCnt > 13 THEN iExt = 13.
                iCalcNumOnLength  = iCnt.
                dCalcTotalLength = dCalcTotalLength + formule[iStyleFormulaOnWid[iExt] * 2].
            END.
            
        END.
    END. /* ELSE Corrugated */
      
    ASSIGN
        ttLayoutSize.dDieSizeWidth      = dCalcTotalWidth
        ttLayoutSize.dDieSizeLength     = dCalcTotalLength
        ttLayoutSize.iBlankNumUp        = bf-eb.num-wid * bf-eb.num-len
        ttLayoutSize.dDieInchesRequired = formule.formule[12] * ttLayoutSize.iBlankNumUp
        .
    IF cIndustryType = "Folding" THEN
        RUN pCalcLeafDieInchs (INPUT ttLayoutSize.iBlankNumUp, INPUT-OUTPUT ttLayoutSize.dDieInchesRequired). 
        
    
    IF bf-item.i-code EQ "E" THEN 
    DO: 
        IF bf-ef.xgrain EQ "S" THEN
            ASSIGN
                ttLayoutSize.dGrossSheetLength = ttLayoutSize.dDieSizeWidth + dTrimWidth
                ttLayoutSize.dGrossSheetWidth  = ttLayoutSize.dDieSizeLength + dTrimLength
                .
        ELSE
            ASSIGN
                ttLayoutSize.dGrossSheetLength = ttLayoutSize.dDieSizeLength + dTrimLength
                ttLayoutSize.dGrossSheetWidth  = ttLayoutSize.dDieSizeWidth + dTrimWidth
                .
        IF bf-ef.roll EQ TRUE THEN 
            ttLayoutSize.dRollWidth = ttLayoutSize.dGrossSheetWidth.
        
        ASSIGN
            ttLayoutSize.dNetSheetLength = IF bf-ef.xgrain EQ "S" THEN ttLayoutSize.dGrossSheetWidth ELSE ttLayoutSize.dGrossSheetLength
            ttLayoutSize.dNetSheetWidth  = IF bf-ef.xgrain EQ "S" THEN ttLayoutSize.dGrossSheetLength ELSE ttLayoutSize.dGrossSheetWidth.
        
        
        /* In legacy, for Estimated material this field is not being calculated 
            and using previously stored value */
        ASSIGN
            ttLayoutSize.dLayoutSheetLength = bf-ef.lsh-len
            ttLayoutSize.dLayoutSheetWidth  = bf-ef.lsh-wid.
        
        
        IF cIndustryType = "Folding" THEN
        DO:
            IF AVAILABLE bf-mach THEN 
            DO:
                ASSIGN
                    ttLayoutSize.dGrossSheetLength = MAX(ttLayoutSize.dGrossSheetLength,bf-mach.min-wid) 
                    ttLayoutSize.dGrossSheetLength = MIN(ttLayoutSize.dGrossSheetLength,bf-mach.max-wid) 
                    ttLayoutSize.dGrossSheetWidth  = MAX(ttLayoutSize.dGrossSheetWidth,bf-mach.min-len) 
                    ttLayoutSize.dGrossSheetWidth  = MIN(ttLayoutSize.dGrossSheetWidth,bf-mach.max-len)
                    ttLayoutSize.dNetSheetLength   = IF bf-ef.xgrain EQ "S" THEN ttLayoutSize.dGrossSheetWidth ELSE ttLayoutSize.dGrossSheetLength
                    ttLayoutSize.dNetSheetWidth    = IF bf-ef.xgrain EQ "S" THEN ttLayoutSize.dGrossSheetLength ELSE ttLayoutSize.dGrossSheetWidth
                    .
               
                IF bf-ef.roll = TRUE THEN 
                   ttLayoutSize.dRollWidth = ttLayoutSize.dGrossSheetWidth.
               
            END. /* IF AVAILABLE bf-mach THEN  */
        END.
        
    END. /**/
    // Real Item
    ELSE
    DO:
        ASSIGN 
            ttLayoutSize.dNetSheetWidth  = ttLayoutSize.dDieSizeWidth + dTrimWidth
            ttLayoutSize.dNetSheetLength = ttLayoutSize.dDieSizeLength + dTrimLength.

        IF bf-ef.roll EQ TRUE THEN
        DO:
            IF bf-ef.xgrain EQ "S" THEN
                ttLayoutSize.dGrossSheetLength = ttLayoutSize.dNetSheetWidth. 
            ELSE 
                ttLayoutSize.dGrossSheetLength = ttLayoutSize.dNetSheetLength.
        END.
        
        IF bf-ef.xgrain EQ "S" THEN 
            ASSIGN
                ttLayoutSize.dLayoutSheetLength = ttLayoutSize.dGrossSheetWidth
                ttLayoutSize.dLayoutSheetWidth  = ttLayoutSize.dGrossSheetLength.
        ELSE
            ASSIGN
                ttLayoutSize.dLayoutSheetLength = ttLayoutSize.dGrossSheetLength
                ttLayoutSize.dLayoutSheetWidth  = ttLayoutSize.dGrossSheetWidth.
        
    END. // ELSE Real Item   
    
    /* Folding specific */
    IF cIndustryType = "Folding" THEN
    DO:
        IF AVAIL bf-mach AND bf-mach.p-type EQ "R" THEN
        DO iCnt = 1 TO 20:
            IF bf-mach.max-pan-ss[iCnt] / 1000 LT ttLayoutSize.dNetSheetLength THEN 
                LEAVE.
            ELSE 
                ttLayoutSize.dGrossSheetLength = bf-mach.max-pan-ss[iCnt] / 1000.
        END.
    END.
    
    /* Corrugated specific */
    ELSE
    DO:
        IF bf-item.i-code EQ "E" THEN
        DO: 
            IF AVAIL bf-mach AND bf-mach.dept[1] EQ "RC" THEN
                ASSIGN  
                    ttLayoutSize.dNetSheetWidth  = ttLayoutSize.dNetSheetWidth - dTrimWidth
                    ttLayoutSize.dNetSheetLength = ttLayoutSize.dNetSheetLength - dTrimLength.
        
            ASSIGN 
                ttLayoutSize.iNumOutWidth   = trunc(ttLayoutSize.dLayoutSheetWidth / IF bf-ef.xgrain EQ "S" THEN ttLayoutSize.dNetSheetLength ELSE ttLayoutSize.dNetSheetWidth,0)
                ttLayoutSize.iNumOutLength  = trunc(ttLayoutSize.dLayoutSheetLength / IF bf-ef.xgrain EQ "S" THEN ttLayoutSize.dNetSheetWidth ELSE ttLayoutSize.dNetSheetLength,0)
                ttLayoutSize.iNumOutDepth   = 1
                ttLayoutSize.dNetSheetDepth = bf-eb.t-dep
                ttLayoutSize.dDieSizeDepth  = bf-eb.t-dep.
        END.
        ELSE
            ASSIGN 
                ttLayoutSize.iNumOutWidth  = trunc(ttLayoutSize.dGrossSheetWidth / IF bf-ef.xgrain EQ "S" THEN ttLayoutSize.dNetSheetLength ELSE ttLayoutSize.dNetSheetWidth,0)
                ttLayoutSize.iNumOutLength = trunc(ttLayoutSize.dGrossSheetLength / IF bf-ef.xgrain EQ "S" THEN ttLayoutSize.dNetSheetWidth ELSE ttLayoutSize.dNetSheetLength,0).
        
        IF AVAIL bf-mach THEN 
        DO:
            IF ttLayoutSize.iNumOutWidth GT bf-mach.num-wid AND bf-mach.num-wid NE 0 THEN
                ttLayoutSize.iNumOutWidth   = bf-mach.num-wid.
            IF ttLayoutSize.iNumOutLength GT bf-mach.num-len AND bf-mach.num-len NE 0 THEN
                ttLayoutSize.iNumOutLength = bf-mach.num-len.
        END.
        IF ttLayoutSize.iNumOutWidth   LE 0 THEN ttLayoutSize.iNumOutWidth   = 1.
        IF ttLayoutSize.iNumOutLength LE 0 THEN ttLayoutSize.iNumOutLength = 1.
        
        ASSIGN
            ttLayoutSize.iNumOutDepth   = 1
            ttLayoutSize.dNetSheetDepth = bf-eb.t-dep
            ttLayoutSize.dDieSizeDepth  = bf-eb.t-dep
            ttLayoutSize.iNumberCuts    = (ttLayoutSize.iNumOutWidth - 1) + (ttLayoutSize.iNumOutLength - 1) + (ttLayoutSize.iNumOutDepth - 1).

        IF ttLayoutSize.iNumberCuts LT 0 THEN 
            ttLayoutSize.iNumberCuts = 0.  
            
        IF bf-item.i-code EQ "E" THEN 
        DO:
                
            ASSIGN
                ttLayoutSize.iBlankNumOnDepth = 1
                ttLayoutSize.dGrossSheetDepth = (ttLayoutSize.iNumOutDepth * ttLayoutSize.dNetSheetDepth)
                .
                               
            IF bf-ef.xgrain EQ "S" THEN
                ASSIGN 
                        ttLayoutSize.dGrossSheetWidth  = (ttLayoutSize.iNumOutWidth  * ttLayoutSize.dNetSheetLength)
                        ttLayoutSize.dGrossSheetLength = (ttLayoutSize.iNumOutLength * ttLayoutSize.dNetSheetWidth).
            ELSE
                ASSIGN 
                    ttLayoutSize.dGrossSheetWidth  = (ttLayoutSize.iNumOutWidth  * ttLayoutSize.dNetSheetWidth)
                    ttLayoutSize.dGrossSheetLength = (ttLayoutSize.iNumOutLength * ttLayoutSize.dNetSheetLength).
        
            
            IF AVAILABLE bf-mach AND bf-mach.dept[1] EQ "RC" THEN
                ASSIGN  
                    ttLayoutSize.dGrossSheetWidth  = ttLayoutSize.dGrossSheetWidth + dTrimWidth 
                    ttLayoutSize.dGrossSheetLength = ttLayoutSize.dGrossSheetLength + dTrimLength. 
    
        END.
        
        IF lDecimal = NO THEN
            ASSIGN
                ttLayoutSize.dGrossSheetLength = ROUND(ttLayoutSize.dGrossSheetLength * dDecimalFactor,0)
                ttLayoutSize.dGrossSheetLength = ttLayoutSize.dGrossSheetLength / dDecimalFactor
                ttLayoutSize.dGrossSheetWidth  = ROUND(ttLayoutSize.dGrossSheetWidth * dDecimalFactor,0)
                ttLayoutSize.dGrossSheetWidth  = ttLayoutSize.dGrossSheetWidth / dDecimalFactor
                ttLayoutSize.dGrossSheetDepth  = ROUND(ttLayoutSize.dGrossSheetDepth * dDecimalFactor,0)
                ttLayoutSize.dGrossSheetDepth  = ttLayoutSize.dGrossSheetDepth / dDecimalFactor
                ttLayoutSize.dNetSheetLength   = ROUND(ttLayoutSize.dNetSheetLength * dDecimalFactor,0)
                ttLayoutSize.dNetSheetLength   = ttLayoutSize.dNetSheetLength / dDecimalFactor
                ttLayoutSize.dNetSheetWidth    = ROUND(ttLayoutSize.dNetSheetWidth * dDecimalFactor,0)
                ttLayoutSize.dNetSheetWidth    = ttLayoutSize.dNetSheetWidth / dDecimalFactor
                ttLayoutSize.dNetSheetDepth    = ROUND(ttLayoutSize.dNetSheetDepth * dDecimalFactor,0)
                ttLayoutSize.dNetSheetDepth    = ttLayoutSize.dNetSheetDepth / dDecimalFactor
                ttLayoutSize.dDieSizeLength    = ROUND(ttLayoutSize.dDieSizeLength * dDecimalFactor,0)
                ttLayoutSize.dDieSizeLength    = ttLayoutSize.dDieSizeLength / dDecimalFactor
                ttLayoutSize.dDieSizeWidth     = ROUND(ttLayoutSize.dDieSizeWidth * dDecimalFactor,0)
                ttLayoutSize.dDieSizeWidth     = ttLayoutSize.dDieSizeWidth / dDecimalFactor
                ttLayoutSize.dDieSizeDepth     = ROUND(ttLayoutSize.dDieSizeDepth * dDecimalFactor,0)
                ttLayoutSize.dDieSizeDepth     = ttLayoutSize.dDieSizeDepth / dDecimalFactor.
             
        IF AVAIL bf-mach AND bf-mach.dept[1] EQ "RC" 
            AND (ttLayoutSize.dGrossSheetLength LT ttLayoutSize.dDieSizeLength + dTrimLength  
              OR ttLayoutSize.dGrossSheetWidth LT ttLayoutSize.dDieSizeWidth + dTrimWidth) THEN
      DO:
            RUN pResetFields.
            RUN pCalcLayout.
            lRecalcFullLayout = YES.
      END.
      
    END. /* Corrugated */
  
    IF ttLayoutSize.dNetSheetLength LT ttLayoutSize.dDieSizeLength + dTrimLength
     OR ttLayoutSize.dNetSheetWidth LT ttLayoutSize.dDieSizeWidth + dTrimWidth THEN
    DO:
        RUN pResetFields.
        RUN pCalcLayout.
        lRecalcFullLayout = YES.
    END.
    
END PROCEDURE.    

PROCEDURE pCalcLayout:
    
    /* Create Temp-table record and populate the fields later */    
    CREATE ttLayoutSize.
        
    IF bf-ef.m-code NE "" THEN
    DO:
        FIND FIRST bf-mach 
            WHERE bf-mach.company = bf-ef.company  
            AND bf-mach.loc     = bf-ef.loc
            AND bf-mach.m-code = bf-ef.m-code  NO-LOCK NO-ERROR.
    
        IF AVAILABLE bf-mach THEN 
        DO:
            ASSIGN 
                dTrimLength                     = bf-mach.min-triml * 2
                dTrimWidth                      = bf-mach.min-trimw * 2
                ttLayoutSize.dLayoutSheetLength = bf-mach.max-wid
                ttLayoutSize.dLayoutSheetWidth  = bf-mach.max-len
                ttLayoutSize.IsRollMaterial     = bf-mach.p-type = "R". 
                  
            IF AVAILABLE bf-item AND bf-item.i-code EQ "E" THEN
            DO:
                IF cIndustryType = "Folding" THEN
                    ASSIGN 
                    ttLayoutSize.dGrossSheetWidth  = (trunc(bf-mach.max-wid / bf-eb.t-wid,0) * bf-eb.t-wid + dTrimWidth )
                    ttLayoutSize.dGrossSheetLength = ( trunc(bf-mach.max-len / bf-eb.t-len,0) * bf-eb.t-len + dTrimLength)
                    ttLayoutSize.dGrossSheetDepth  = IF isFoamStyle THEN (trunc(bf-mach.max-dep / bf-eb.t-dep,0) * bf-eb.t-dep ) ELSE 0.
            
                
                ELSE
                    ASSIGN 
                    ttLayoutSize.dGrossSheetWidth  = ( trunc(bf-mach.max-len / bf-eb.t-len,0) * bf-eb.t-len + dTrimWidth)
                    ttLayoutSize.dGrossSheetLength = (trunc(bf-mach.max-wid / bf-eb.t-wid,0) * bf-eb.t-wid + dTrimLength )
                    ttLayoutSize.dGrossSheetDepth  = IF isFoamStyle THEN (trunc(bf-mach.max-dep / bf-eb.t-dep,0) * bf-eb.t-dep ) ELSE 0.
            
            END.
            ELSE IF AVAILABLE bf-item THEN
                ASSIGN 
                    ttLayoutSize.dGrossSheetWidth  = (bf-item.s-wid)
                    ttLayoutSize.dGrossSheetLength = (bf-item.s-len)
                    ttLayoutSize.dGrossSheetDepth  = IF isFoamStyle THEN (bf-item.s-dep) ELSE 0.
        END.
    END. /* IF bf-ef.m-code NE "" THEN */
    
    ELSE
    DO:
        FIND FIRST bf-ce-ctrl NO-LOCK
            WHERE bf-ce-ctrl.company = bf-ef.company  
            AND bf-ce-ctrl.loc     = bf-ef.loc NO-ERROR.
        
        ASSIGN 
            dTrimLength                     = bf-ce-ctrl.ls-triml * 2
            dTrimWidth                      = bf-ce-ctrl.ls-trimw * 2
            ttLayoutSize.dLayoutSheetWidth  = bf-ce-ctrl.ls-length
            ttLayoutSize.dLayoutSheetLength = bf-ce-ctrl.ls-width.
            
        IF AVAILABLE bf-item THEN
            ASSIGN
                ttLayoutSize.dGrossSheetLength = (bf-item.s-len)
                ttLayoutSize.dGrossSheetWidth  = (bf-item.s-wid)
                ttLayoutSize.dGrossSheetDepth  = IF isFoamStyle THEN (bf-item.s-dep) ELSE 0.
            
        ELSE
            ASSIGN
                ttLayoutSize.dGrossSheetLength = bf-eb.t-len
                ttLayoutSize.dGrossSheetWidth  = bf-eb.t-wid.
            
            
    END.        
            
    IF AVAILABLE bf-item THEN 
    DO:
        ASSIGN 
            ttLayoutSize.cBoardItemCode        = bf-item.i-code
            ttLayoutSize.cBoardItemBasisWeight = bf-item.basis-w.
        
        IF NOT bf-ef.lsh-lock THEN 
        DO:
            ttLayoutSize.dBoardItemCaliper   = bf-item.cal. 
               
            IF bf-item.i-code EQ "R" THEN 
            DO:
                IF bf-item.r-wid GT 0  THEN 
                DO: 
                    ASSIGN 
                        ttLayoutSize.dRollWidth         = bf-item.r-wid
                        ttLayoutSize.dGrossSheetWidth   = bf-item.r-wid
                        ttLayoutSize.IsRollMaterial     = YES
                        .
                    IF cIndustryType = "Folding" THEN    
                        ttLayoutSize.dGrossSheetLength  = IF bf-ef.xgrain EQ "S" THEN ttLayoutSize.dLayoutSheetWidth ELSE ttLayoutSize.dLayoutSheetLength.
                    ELSE
                        ASSIGN
                            ttLayoutSize.dGrossSheetLength  = ttLayoutSize.dLayoutSheetLength.
                        
                END.
                ELSE 
                DO:
                    ASSIGN 
                        ttLayoutSize.dGrossSheetWidth   = bf-item.s-wid
                        ttLayoutSize.dGrossSheetLength  = bf-item.s-len
                        ttLayoutSize.IsRollMaterial     = NO
                        ttLayoutSize.dRollWidth         = 0
                        .
                    IF cIndustryType NE "Folding" THEN    
                        ASSIGN
                            ttLayoutSize.dLayoutSheetLength = bf-item.s-len
                            ttLayoutSize.dLayoutSheetWidth  = bf-item.s-wid.
                END.
                
                IF cIndustryType = "Folding" THEN
                DO:
                    IF bf-ef.xgrain EQ "S" THEN
                        ASSIGN 
                            ttLayoutSize.dNetSheetLength    = ttLayoutSize.dGrossSheetWidth
                            ttLayoutSize.dNetSheetWidth     = ttLayoutSize.dGrossSheetLength
                            ttLayoutSize.dLayoutSheetLength = ttLayoutSize.dGrossSheetWidth
                            .
                    ELSE
                        ASSIGN 
                            ttLayoutSize.dNetSheetLength    = ttLayoutSize.dGrossSheetLength
                            ttLayoutSize.dNetSheetWidth     = ttLayoutSize.dGrossSheetWidth
                            ttLayoutSize.dLayoutSheetWidth   = ttLayoutSize.dGrossSheetLength
                            .
                END.
           
            END. /* i-code = "R" */
            /* ELSE item.i-code EQ "E" */
            ELSE 
            DO:
                ASSIGN 
                    ttLayoutSize.dGrossSheetWidth  = ttLayoutSize.dLayoutSheetWidth
                    ttLayoutSize.dGrossSheetLength = ttLayoutSize.dLayoutSheetLength
                    ttLayoutSize.dNetSheetLength   = ttLayoutSize.dGrossSheetLength
                    ttLayoutSize.dNetSheetWidth    = ttLayoutSize.dGrossSheetWidth
                    ttLayoutSize.dRollWidth        = ttLayoutSize.dGrossSheetWidth.
                    
                IF cIndustryType = "Folding" AND bf-ef.xgrain EQ "S" THEN
                    ASSIGN 
                        ttLayoutSize.dGrossSheetWidth  = ttLayoutSize.dLayoutSheetlength
                        ttLayoutSize.dGrossSheetLength = ttLayoutSize.dLayoutSheetWidth
                        ttLayoutSize.dNetSheetLength   = ttLayoutSize.dGrossSheetWidth
                        ttLayoutSize.dNetSheetWidth    = ttLayoutSize.dGrossSheetLength.
                
            END.   /* bf-item.i-code = "E" */
        END. /* IF NOT bf-ef.lsh-lock THEN */
    END. /* IF AVAIL bf-item THEN */
    
    ASSIGN
        ttLayoutSize.iNumOutWidth  = MAX(ttLayoutSize.iNumOutWidth,1)
        ttLayoutSize.iNumOutLength = MAX(ttLayoutSize.iNumOutLength,1)
        ttLayoutSize.iNumOutDepth  = MAX(ttLayoutSize.iNumOutDepth,1)
        ttLayoutSize.iNumberCuts   = (ttLayoutSize.iNumOutWidth - 1) + (ttLayoutSize.iNumOutLength - 1) + (ttLayoutSize.iNumOutDepth - 1).
      
        
    IF cIndustryType = "Folding" THEN
    DO:
        IF bf-ef.xgrain EQ "S" THEN
            ASSIGN
                dTempLength = ttLayoutSize.dGrossSheetLength
                dTempWidth  = ttLayoutSize.dGrossSheetWidth.
        ELSE
            ASSIGN
                dTempLength = ttLayoutSize.dGrossSheetWidth
                dTempWidth  = ttLayoutSize.dGrossSheetLength.
    END.
    ELSE 
    DO:
        ASSIGN 
            dTempLength = ttLayoutSize.dGrossSheetLength / ttLayoutSize.iNumOutWidth
            dTempWidth  = ttLayoutSize.dGrossSheetWidth / ttLayoutSize.iNumOutLength.
        
        IF avail(bf-item) AND bf-item.i-code EQ "E" AND bf-ef.xgrain = "N" AND AVAILABLE bf-mach THEN
            ASSIGN
                dTempLength = min(dTempLength, bf-mach.max-wid)
                dTempWidth  = min(dTempWidth, bf-mach.max-len).
    
    
        IF bf-ef.lam-dscr EQ "R" THEN
            ASSIGN  
                dSwapDim    = dTempLength
                dTempLength = dTempWidth
                dTempWidth  = dSwapDim.
    END. 
    
    RUN pGetDecimalSettings(bf-eb.company,  
            cIndustryType,
            OUTPUT lDecimal,
            OUTPUT dDecimalFactor,
            OUTPUT dDecimalMax, 
            OUTPUT dConversionFactor,
            OUTPUT lRound).      
     
            
    RUN pGetFormulaTT (BUFFER bf-eb,
                       INPUT cIndustryType, 
                       INPUT lDecimal, 
                       INPUT dDecimalFactor, 
                       INPUT dDecimalMax, 
                       INPUT dConversionFactor, 
                       INPUT lRound,
                       OUTPUT TABLE formule).
    
    /*formule is populated by CalcStyleFormulae*/
    FIND FIRST formule NO-LOCK NO-ERROR.
    
    
    IF ceroute-dec EQ 1 THEN 
    DO:
        ASSIGN
            iCalcNumOnWidth            = 1
            iCalcNumOnLength           = 1
            ttLayoutSize.iNumOutWidth  = 1
            ttLayoutSize.iNumOutLength = 1.
          
        IF bf-ef.xgrain EQ "B" THEN
            ASSIGN
                dCalcTotalLength = formule.formule[1]
                dCalcTotalWidth  = formule.formule[2].
        ELSE
            ASSIGN
                dCalcTotalWidth  = formule.formule[1]
                dCalcTotalLength = formule.formule[2].
    END.
    ELSE 
    DO:
        IF cIndustryType = "Folding" THEN
        DO:
            IF bf-ef.xgrain = "B" OR bf-ef.xgrain = "S" THEN
                ASSIGN 
                    dLengthtoUse = dTempWidth - dTrimLength 
                    dWidtoUse    = dTempLength - dTrimWidth.
            ELSE
                ASSIGN
                    dLengthtoUse = dTempLength - dTrimWidth
                    dWidtoUse    = dTempWidth - dTrimLength. 
        END.
        ELSE
        DO:
            IF bf-ef.xgrain = "B" THEN
                ASSIGN 
                    dLengthtoUse = dTempWidth - dTrimWidth
                    dWidtoUse    = dTempLength - dTrimLength.
            ELSE
                ASSIGN
                    dLengthtoUse = dTempLength - dTrimLength
                    dWidtoUse    = dTempWidth - dTrimWidth.
        END.
            
        /* Calculate Width Size using Panel dimension */    
        DO iCnt = 1 TO 50:
            iExt = iCnt.        
            IF iCnt > 13 THEN 
                iExt = 13.
            
            IF cIndustryType = "Folding" THEN
            DO:
                IF bf-ef.xgrain = "B" OR bf-ef.xgrain = "S" THEN
                DO: 
                    IF dCalcTotalWidth + formule.formule[iStyleFormulaOnWid[iExt] * 2] <= dWidtoUse THEN 
                        ASSIGN 
                            iCalcNumOnWidth = iCnt
                            dCalcTotalWidth = dCalcTotalWidth + formule.formule[iStyleFormulaOnWid[iExt] * 2].
                        
                    ELSE 
                        LEAVE.
                END.
                ELSE
                DO:
                   IF dCalcTotalWidth + formule[iStyleFormulaOnLen[iExt] + (iStyleFormulaOnLen[iExt] - 1)] <= dLengthtoUse THEN 
                        ASSIGN
                            iCalcNumOnWidth = iCnt
                            dCalcTotalWidth = dCalcTotalWidth + formule[iStyleFormulaOnLen[iExt] + (iStyleFormulaOnLen[iExt] - 1)].
            
                    ELSE 
                        LEAVE.
                END.
            END.
            /* Corrugated Industry */
            ELSE
            DO:
                IF bf-ef.xgrain = "B" THEN
                DO: 
                    IF iCnt = 1 OR dCalcTotalWidth + formule.formule[iStyleFormulaOnWid[iExt] * 2] <= dWidtoUse THEN 
                        ASSIGN 
                            iCalcNumOnWidth = iCnt
                            dCalcTotalWidth = dCalcTotalWidth + formule.formule[iStyleFormulaOnWid[iExt] * 2].
                        
                    ELSE 
                        LEAVE.
                END.
                ELSE
                DO:
                    IF dCalcTotalWidth + formule[iStyleFormulaOnWid[iExt] + (iStyleFormulaOnWid[iExt] - 1)] <= dWidtoUse
                        OR (NOT (avail(bf-item) AND bf-item.i-code = "R" AND bf-ef.xgrain EQ "N") AND iCnt = 1) THEN 
                        ASSIGN
                            iCalcNumOnWidth = iCnt
                            dCalcTotalWidth = dCalcTotalWidth + formule[iStyleFormulaOnWid[iExt] + (iStyleFormulaOnWid[iExt] - 1)].
            
                    ELSE 
                        LEAVE.
                END.
            END.
        END. // DO iCnt = 1 TO 50:
         
        /* Calculate Length Size using Panel dimension */    
        DO iCnt = 1 TO 50:
            iExt = iCnt.        
            IF iCnt > 13 THEN 
                iExt = 13.
            
            IF cIndustryType = "Folding" THEN
            DO:
                IF bf-ef.xgrain = "B" OR bf-ef.xgrain = "S" THEN
                DO:
                    IF dCalcTotalLength + formule.formule[iStyleFormulaOnLen[iExt] + (iStyleFormulaOnLen[iExt] - 1)] <= dLengthtoUse THEN
                        ASSIGN 
                            iCalcNumOnLength = iCnt
                            dCalcTotalLength = dCalcTotalLength + formule.formule[iStyleFormulaOnLen[iExt] + (iStyleFormulaOnLen[iExt] - 1)].
                
                    ELSE 
                        LEAVE.
                END.
                ELSE
                DO:
                    IF dCalcTotalLength + formule[iStyleFormulaOnWid[iExt] * 2] <= dWidtoUse THEN 
                        ASSIGN 
                            iCalcNumOnLength = iCnt
                            dCalcTotalLength = dCalcTotalLength + formule[iStyleFormulaOnWid[iExt] * 2].
                        
                    ELSE 
                        LEAVE.
                END. 
            END.
            
            /* Corrugated Industry */
            ELSE
            DO: 
                IF bf-ef.xgrain = "B" THEN
                DO:
                    IF iCnt = 1 OR dCalcTotalLength + formule.formule[iStyleFormulaOnLen[iExt] + (iStyleFormulaOnLen[iExt] - 1)] <= dLengthtoUse THEN
                        ASSIGN 
                            iCalcNumOnLength = iCnt
                            dCalcTotalLength = dCalcTotalLength + formule.formule[iStyleFormulaOnLen[iExt] + (iStyleFormulaOnLen[iExt] - 1)].
                
                    ELSE 
                        LEAVE.
                END.
                ELSe
                DO:
                    IF dCalcTotalLength + formule[iStyleFormulaOnLen[iExt] * 2] <= dLengthtoUse
                        OR (NOT (avail(bf-item) AND bf-item.i-code = "R" AND bf-ef.xgrain EQ "N") AND iCnt = 1) THEN 
                        ASSIGN 
                            iCalcNumOnLength = iCnt
                            dCalcTotalLength = dCalcTotalLength + formule[iStyleFormulaOnLen[iExt] * 2].
                        
                    ELSE 
                        LEAVE.
                END.
            END. 
        END. // DO iCnt = 1 TO 50:
        
    END. /* ELSE*/
    
    
    IF NOT bf-ef.lsh-lock THEN /* autocalc */
    DO: 
        IF cIndustryType = "Folding" THEN
        DO:
            ASSIGN 
                    ttLayoutSize.iBlankNumOnWidth  = iCalcNumOnWidth 
                    ttLayoutSize.iBlankNumOnLength = iCalcNumOnLength.
            
            IF AVAILABLE bf-mach THEN
                ASSIGN
                    dCalcTotalWidth  = MAX(dCalcTotalWidth, bf-mach.min-len)
                    dCalcTotalLength = MAX(dCalcTotalLength, bf-mach.min-wid).
        END.
        ELSE
        DO:
            IF bf-ef.xgrain = "B" THEN 
            DO:
                ASSIGN 
                    ttLayoutSize.iBlankNumOnWidth  = iCalcNumOnLength
                    ttLayoutSize.iBlankNumOnLength = iCalcNumOnWidth.
                
                IF bf-eb.t-len * ttLayoutSize.iBlankNumOnLength GT dCalcTotalWidth THEN  
                    dCalcTotalWidth = bf-eb.t-len * ttLayoutSize.iBlankNumOnLength.
                IF bf-eb.t-wid * ttLayoutSize.iBlankNumOnWidth GT dCalcTotalLength THEN  
                    dCalcTotalLength = bf-eb.t-wid * ttLayoutSize.iBlankNumOnWidth.  
            END.
            ELSE 
            DO:
                ASSIGN 
                    ttLayoutSize.iBlankNumOnWidth  = iCalcNumOnLength
                    ttLayoutSize.iBlankNumOnLength = iCalcNumOnWidth 
                    .   
            
                  
                IF bf-eb.t-len * ttLayoutSize.iBlankNumOnWidth GT dCalcTotalLength THEN  
                    dCalcTotalLength = bf-eb.t-len * ttLayoutSize.iBlankNumOnWidth.
                IF bf-eb.t-wid * ttLayoutSize.iBlankNumOnLength GT dCalcTotalWidth THEN  
                    dCalcTotalWidth = bf-eb.t-wid * ttLayoutSize.iBlankNumOnLength.
            END. 
        END. 
    END.
    
    ASSIGN 
        ttLayoutSize.iBlankNumUp        = ttLayoutSize.iBlankNumOnWidth * ttLayoutSize.iBlankNumOnLength
        ttLayoutSize.dDieInchesRequired = formule.formule[12] * ttLayoutSize.iBlankNumUp  .
        
    IF cIndustryType = "Folding" THEN
        RUN pCalcLeafDieInchs (INPUT ttLayoutSize.iBlankNumUp, INPUT-OUTPUT ttLayoutSize.dDieInchesRequired).    
    
    ASSIGN   
        ttLayoutSize.dNetSheetWidth  = dCalcTotalWidth + dTrimWidth
        ttLayoutSize.dNetSheetLength = dCalcTotalLength + dTrimLength 
        ttLayoutSize.dDieSizeWidth   = dCalcTotalWidth
        ttLayoutSize.dDieSizeLength  = dCalcTotalLength.
        
    
    IF ttLayoutSize.dLayoutSheetWidth LT ttLayoutSize.dNetSheetWidth THEN 
        ttLayoutSize.dLayoutSheetWidth = ttLayoutSize.dNetSheetWidth.
        
    IF ttLayoutSize.dLayoutSheetLength LT ttLayoutSize.dNetSheetLength THEN 
        ttLayoutSize.dLayoutSheetLength = ttLayoutSize.dNetSheetLength.
    
    IF bf-item.i-code EQ "E" THEN 
    DO:
        IF INDEX("B",bf-ef.xgrain) EQ 0 THEN 
        DO:
            IF ttLayoutSize.dGrossSheetWidth LT ttLayoutSize.dNetSheetWidth THEN 
                ttLayoutSize.dGrossSheetWidth = ttLayoutSize.dNetSheetWidth.
            IF ttLayoutSize.dGrossSheetLength LT ttLayoutSize.dNetSheetLength THEN 
                ttLayoutSize.dGrossSheetLength = ttLayoutSize.dNetSheetLength.
        END.
        
        
        IF AVAILABLE bf-mach AND bf-mach.dept[1] EQ "RC" THEN
            ASSIGN  
                ttLayoutSize.dNetSheetWidth  = ttLayoutSize.dNetSheetWidth - dTrimWidth
                ttLayoutSize.dNetSheetLength = ttLayoutSize.dNetSheetLength - dTrimLength.
    
    
        IF ceroute-dec NE 1 THEN        
            ASSIGN 
                ttLayoutSize.iNumOutWidth  = TRUNCATE(ttLayoutSize.dLayoutSheetWidth / ttLayoutSize.dNetSheetWidth,0)
                ttLayoutSize.iNumOutLength = TRUNCATE(ttLayoutSize.dLayoutSheetLength / ttLayoutSize.dNetSheetLength,0).
    
        ASSIGN 
            ttLayoutSize.iNumOutDepth   = 1
            ttLayoutSize.dNetSheetDepth = bf-eb.t-dep
            ttLayoutSize.dDieSizeDepth  = bf-eb.t-dep.
            
            
        IF cIndustryType = "Folding" THEN
        DO: 
            IF bf-ef.xgrain EQ "S" THEN 
                ASSIGN 
                    ttLayoutSize.dGrossSheetWidth  = IF ttLayoutSize.IsRollMaterial = NO THEN ttLayoutSize.dDieSizeLength + dTrimLength ELSE ttLayoutSize.dGrossSheetWidth
                    ttLayoutSize.dGrossSheetLength = ttLayoutSize.dDieSizeWidth + dTrimWidth
                    ttLayoutSize.dNetSheetWidth    = ttLayoutSize.dGrossSheetLength 
                    ttLayoutSize.dNetSheetLength   = ttLayoutSize.dGrossSheetWidth
                    .
    
            ELSE 
                ASSIGN 
                    ttLayoutSize.dGrossSheetWidth  = IF ttLayoutSize.IsRollMaterial = NO THEN  ttLayoutSize.dDieSizeWidth + dTrimWidth ELSE ttLayoutSize.dGrossSheetWidth
                    ttLayoutSize.dGrossSheetLength = ttLayoutSize.dDieSizeLength + dTrimLength
                    ttLayoutSize.dNetSheetWidth    = ttLayoutSize.dGrossSheetWidth
                    ttLayoutSize.dNetSheetLength   = ttLayoutSize.dGrossSheetLength
                    .
        END. /* IF cIndustryType = "Folding" THEN */
    END. /* IF bf-item.i-code EQ "E" THEN  */
    
    /* Real Material */
    ELSE
    DO:
        IF cIndustryType = "Folding" THEN
        DO: 
            ASSIGN 
                ttLayoutSize.dNetSheetWidth  = ttLayoutSize.dDieSizeWidth + dTrimWidth
                ttLayoutSize.dNetSheetLength = ttLayoutSize.dDieSizeLength + dTrimLength
                .
                
            IF bf-ef.xgrain EQ "S" THEN 
                ASSIGN
                    ttLayoutSize.dGrossSheetLength = IF ttLayoutSize.IsRollMaterial = YES THEN ttLayoutSize.dNetSheetWidth ELSE ttLayoutSize.dGrossSheetLength
                    ttLayoutSize.dLayoutSheetLength = ttLayoutSize.dGrossSheetWidth
                    ttLayoutSize.dLayoutSheetWidth  = ttLayoutSize.dGrossSheetLength.
            ELSE
                ASSIGN
                    ttLayoutSize.dGrossSheetLength = IF ttLayoutSize.IsRollMaterial = YES THEN ttLayoutSize.dNetSheetLength ELSE ttLayoutSize.dGrossSheetLength
                    dLayoutSheetLength             = ttLayoutSize.dGrossSheetLength
                    ttLayoutSize.dLayoutSheetWidth = ttLayoutSize.dGrossSheetWidth.
        END. /* IF cIndustryType = "Folding" THEN */
        
    END.    
    
    /* Check machine limits */
    IF AVAILABLE bf-mach THEN 
    DO:
        IF ttLayoutSize.iNumOutWidth  GT bf-mach.num-wid AND bf-mach.num-wid NE 0 THEN
            ttLayoutSize.iNumOutWidth  = bf-mach.num-wid.
        IF ttLayoutSize.iNumOutLength GT bf-mach.num-len AND bf-mach.num-len NE 0 THEN
            ttLayoutSize.iNumOutLength = bf-mach.num-len.
    END.
    
    IF bf-item.i-code EQ "E" AND cIndustryType NE "Folding" THEN 
    DO:
        ASSIGN 
            ttLayoutSize.iBlankNumOnDepth  = (IF isFoamStyle THEN 1 ELSE 0)
            ttLayoutSize.dGrossSheetWidth  = (ttLayoutSize.iNumOutWidth  * ttLayoutSize.dNetSheetWidth)
            ttLayoutSize.dGrossSheetLength = (ttLayoutSize.iNumOutLength * ttLayoutSize.dNetSheetLength)
            ttLayoutSize.dGrossSheetDepth  = (ttLayoutSize.iNumOutDepth * ttLayoutSize.dNetSheetDepth)
            .
            
                
        IF AVAILABLE bf-mach AND bf-mach.dept[1] EQ "RC" THEN
            ASSIGN  
                ttLayoutSize.dGrossSheetWidth  = ttLayoutSize.dGrossSheetWidth + dTrimWidth 
                ttLayoutSize.dGrossSheetLength = ttLayoutSize.dGrossSheetLength + dTrimLength. 
        
    END.
                                    
    
    IF isFoamStyle THEN
        ASSIGN
            ttLayoutSize.dNetSheetDepth   = bf-eb.t-dep
            ttLayoutSize.dDieSizeDepth    = bf-eb.t-dep
            ttLayoutSize.dGrossSheetDepth = IF bf-item.i-code EQ "E" THEN bf-eb.t-dep ELSE bf-item.s-dep
            ttLayoutSize.iNumOutDepth     = IF ttLayoutSize.dNetSheetDepth NE 0 THEN TRUNCATE(ttLayoutSize.dGrossSheetDepth / ttLayoutSize.dNetSheetDepth,0) ELSE 1
            ttLayoutSize.iBlankNumOnDepth = 1. 
    
    IF cIndustryType = "Folding" AND lDecimal = NO THEN
    DO:
        IF AVAILABLE bf-item AND bf-item.i-code EQ "E" THEN
        ASSIGN
            ttLayoutSize.dGrossSheetLength = ROUND(ttLayoutSize.dGrossSheetLength * dDecimalFactor,0)
            ttLayoutSize.dGrossSheetLength = ttLayoutSize.dGrossSheetLength / dDecimalFactor
            ttLayoutSize.dGrossSheetWidth  = ROUND(ttLayoutSize.dGrossSheetWidth * dDecimalFactor,0)
            ttLayoutSize.dGrossSheetWidth  = ttLayoutSize.dGrossSheetWidth / dDecimalFactor.
        ELSE
            ASSIGN
                ttLayoutSize.dNetSheetLength = ROUND(ttLayoutSize.dNetSheetLength * dDecimalFactor,0)
                ttLayoutSize.dNetSheetLength = ttLayoutSize.dNetSheetLength / dDecimalFactor
                ttLayoutSize.dNetSheetWidth  = ROUND(ttLayoutSize.dNetSheetWidth * dDecimalFactor,0)
                ttLayoutSize.dNetSheetWidth  = ttLayoutSize.dNetSheetWidth / dDecimalFactor.
    END.      
    
    /* Corrugated Version */  
    ELSE IF lDecimal = NO THEN
    DO:
        ASSIGN
            ttLayoutSize.dGrossSheetLength = ROUND(ttLayoutSize.dGrossSheetLength * dDecimalFactor,0)
            ttLayoutSize.dGrossSheetLength = ttLayoutSize.dGrossSheetLength / dDecimalFactor
            ttLayoutSize.dGrossSheetWidth  = ROUND(ttLayoutSize.dGrossSheetWidth * dDecimalFactor,0)
            ttLayoutSize.dGrossSheetWidth  = ttLayoutSize.dGrossSheetWidth / dDecimalFactor
            ttLayoutSize.dGrossSheetDepth  = ROUND(ttLayoutSize.dGrossSheetDepth * dDecimalFactor,0)
            ttLayoutSize.dGrossSheetDepth  = ttLayoutSize.dGrossSheetDepth / dDecimalFactor
            ttLayoutSize.dNetSheetLength   = ROUND(ttLayoutSize.dNetSheetLength * dDecimalFactor,0)
            ttLayoutSize.dNetSheetLength   = ttLayoutSize.dNetSheetLength / dDecimalFactor
            ttLayoutSize.dNetSheetWidth    = ROUND(ttLayoutSize.dNetSheetWidth * dDecimalFactor,0)
            ttLayoutSize.dNetSheetWidth    = ttLayoutSize.dNetSheetWidth / dDecimalFactor
            ttLayoutSize.dNetSheetDepth    = ROUND(ttLayoutSize.dNetSheetDepth * dDecimalFactor,0)
            ttLayoutSize.dNetSheetDepth    = ttLayoutSize.dNetSheetDepth / dDecimalFactor
            ttLayoutSize.dDieSizeLength    = ROUND(ttLayoutSize.dDieSizeLength * dDecimalFactor,0)
            ttLayoutSize.dDieSizeLength    = ttLayoutSize.dDieSizeLength / dDecimalFactor
            ttLayoutSize.dDieSizeWidth     = ROUND(ttLayoutSize.dDieSizeWidth * dDecimalFactor,0)
            ttLayoutSize.dDieSizeWidth     = ttLayoutSize.dDieSizeWidth / dDecimalFactor
            ttLayoutSize.dDieSizeDepth     = ROUND(ttLayoutSize.dDieSizeDepth * dDecimalFactor,0)
            ttLayoutSize.dDieSizeDepth     = ttLayoutSize.dDieSizeDepth / dDecimalFactor.

        IF AVAIL(bf-item) AND bf-item.i-code EQ "E" AND bf-ef.xgrain = "S" THEN
            ASSIGN
                dSwapDim                       = ttLayoutSize.dGrossSheetLength
                ttLayoutSize.dGrossSheetLength = ttLayoutSize.dGrossSheetWidth
                ttLayoutSize.dGrossSheetWidth  = dSwapDim.
    END.

END PROCEDURE.


/* **********************  Internal Procedures  *********************** */

PROCEDURE pCalcLeafDieInchs PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT         PARAMETER ipiNumUp    AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT  PARAMETER opdDieInch  AS DECIMAL NO-UNDO.

    DEFINE VARIABLE dCalcVal AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCnt     AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bfleaf-Item FOR Item.
    
    DO iCnt = 1 TO 4:
        IF bf-ef.leaf[iCnt] NE "" AND bf-ef.leaf-bnum[iCnt] NE 0 
            AND ((bf-ef.leaf-w[iCnt] NE 0) AND (bf-ef.leaf-l[iCnt] NE 0)) THEN
        DO:
            FIND FIRST bfleaf-Item NO-LOCK
                WHERE bfleaf-Item.company = bf-ef.company 
                AND bfleaf-Item.i-no EQ bf-ef.leaf[iCnt] NO-ERROR.
            
            IF bfleaf-Item.mat-type NE "W" THEN 
                NEXT.
            dCalcVal = dCalcVal + ((bf-ef.leaf-w[iCnt] + bf-ef.leaf-l[iCnt]) * 2 * ipiNumUp).
        END.
    END.
    
    opdDieInch = opdDieInch + dCalcVal.
END PROCEDURE.

PROCEDURE pGetDecimalSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Return conversion settings based upon Industry
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcIndustry AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplDecimal AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDecimalFactor AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDecimalMax AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdConversionFactor AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRound AS LOGICAL NO-UNDO.

    IF ipcIndustry  = "Folding" THEN
    DO:
        RUN pGetDecimalSettingsFold(ipcCompany, 
            OUTPUT oplDecimal,
            OUTPUT opdDecimalFactor,
            OUTPUT opdDecimalMax, 
            OUTPUT opdConversionFactor).
            
    END.
    ELSE
    DO: 
        RUN pGetDecimalSettingsCorr(ipcCompany,  /*Get NK1 CECSCRN settings*/
            OUTPUT oplDecimal,
            OUTPUT opdDecimalFactor,
            OUTPUT opdDecimalMax, 
            OUTPUT opdConversionFactor). 
        
        /*Get Rounding Settings*/
        oplRound = fRound( ipcCompany ).  /*Get NK1 Round logical*/
       
    END.        
    

END PROCEDURE.

PROCEDURE pGetDecimalSettingsCorr:
    /*------------------------------------------------------------------------------
     Purpose: Returns values from CECSCRN N-K-1
     Notes:  For how decimals appear in dimensions.  .08, .16, .50 (for 16ths, 32nds, and Dec respectively)
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplDecimal AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDecimalFactor AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDecimalMax AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdConversionFactor AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO. 
    
    RUN sys\ref\nk1look.p (ipcCompany,
        'CECSCRN',
        'C',
        NO,
        NO,
        '',
        '', 
        OUTPUT cReturn,
        OUTPUT lFound).
    
    IF lFound THEN 
    DO:
        IF cReturn EQ 'Decimal' THEN
            ASSIGN 
                oplDecimal          = YES
                opdConversionFactor = 1
                opdDecimalFactor    = 1
                opdDecimalMax       = 1
                .
        ELSE IF cReturn EQ "32nd's" THEN   
                ASSIGN 
                    oplDecimal          = NO 
                    opdConversionFactor = 3.125
                    opdDecimalFactor    = 32
                    opdDecimalMax       = 0.32
                    .
            ELSE    
                ASSIGN 
                    oplDecimal          = NO 
                    opdConversionFactor = 6.25
                    opdDecimalFactor    = 16
                    opdDecimalMax       = 0.16
                    .
    END.

END PROCEDURE.

PROCEDURE pGetDecimalSettingsFold:
    /*------------------------------------------------------------------------------
     Purpose: Returns values from CECSCRN N-K-1
     Notes:  For how decimals appear in dimensions.  .08, .16, .50 (for 16ths, 32nds, and Dec respectively)
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplDecimal AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDecimalFactor AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDecimalMax AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdConversionFactor AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO. 
    
    RUN sys\ref\nk1look.p (ipcCompany,
        'CELAYOUT',
        'C',
        NO,
        NO,
        '',
        '', 
        OUTPUT cReturn,
        OUTPUT lFound).
    
    oplDecimal = YES.
    
    IF cReturn = "None" THEN
    DO:
        ASSIGN 
            oplDecimal          = YES
            opdConversionFactor = 1
            opdDecimalFactor    = 1
            opdDecimalMax       = 1
            .
        RETURN.
    END.
    IF lFound THEN 
    DO:
        IF cReturn EQ '1' OR cReturn EQ '1UP' THEN
            ASSIGN 
                oplDecimal          = YES
                opdConversionFactor = 1
                opdDecimalFactor    = 1
                opdDecimalMax       = 1
                .
        ELSE IF cReturn = "1/8" OR cReturn EQ '1/8UP' THEN
            ASSIGN 
                oplDecimal          = YES
                opdConversionFactor = 1
                opdDecimalFactor    = 8
                opdDecimalMax       = 8
                . 
        ELSE IF cReturn = "1/4" OR cReturn EQ '1/4UP' THEN
            ASSIGN 
                oplDecimal          = YES
                opdConversionFactor = 1
                opdDecimalFactor    = 4
                opdDecimalMax       = 4
                .
        ELSE IF cReturn = "1/2" OR cReturn EQ '1/2UP' THEN
            ASSIGN 
                oplDecimal          = YES
                opdConversionFactor = 1
                opdDecimalFactor    = 2
                opdDecimalMax       = 2
                .  
    END.

END PROCEDURE.

PROCEDURE pGetFormulaTT PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    DEFINE INPUT  PARAMETER ipcIndustry AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplDecimal AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDecimalFactor AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDecimalMax AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdConversionFactor AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER iplRound AS LOGICAL NO-UNDO.    
    DEFINE OUTPUT PARAMETER TABLE FOR formule.

    IF bf-eb.sty-lock OR ceroute-dec EQ 1 AND ipcIndustry NE "Folding" THEN 
    DO:
        CREATE formule.
        ASSIGN
            formule.formule[1]  = ipbf-eb.t-wid
            formule.formule[3]  = ipbf-eb.t-wid
            formule.formule[5]  = ipbf-eb.t-wid
            formule.formule[7]  = ipbf-eb.t-wid
            formule.formule[9]  = ipbf-eb.t-wid
            formule.formule[2]  = ipbf-eb.t-len
            formule.formule[4]  = ipbf-eb.t-len
            formule.formule[6]  = ipbf-eb.t-len
            formule.formule[8]  = ipbf-eb.t-len
            formule.formule[10] = ipbf-eb.t-len
            formule.formule[12] = ipbf-eb.die-in.
    END.
              
    ELSE 
    DO:
        /*Calculate Style formulas and scoring*/        
        RUN est/CalcStyleFormulae.p (ROWID(ipbf-eb),
            iplRound,
            iplDecimal,
            ipdDecimalFactor,
            ipdConversionFactor,
            OUTPUT TABLE formule).
    END.
    

END PROCEDURE.

PROCEDURE pResetFields PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE ttLayoutSize.
    ASSIGN
        dTrimLength       = 0
        dTrimWidth        = 0
        dCalcTotalLength  = 0
        dCalcTotalWidth   = 0
        dWidtoUse         = 0
        dLengthtoUse      = 0
        iCalcNumOnWidth   = 0
        iCalcNumOnLength  = 0
        iCnt              = 0
        iExt              = 0
        dSwapDim          = 0
        dTempLength       = 0
        dTempWidth        = 0
        lRound            = NO
        lDecimal          = NO
        dDecimalFactor    = 0
        dDecimalMax       = 0
        dConversionFactor = 0
        .


END PROCEDURE.
        
/* ************************  Function Implementations ***************** */
      
FUNCTION fRound RETURNS LOGICAL 
    ( ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Returns a logical value based on the value of the ROUND N-K-1 
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE lResult AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO. 
    
    RUN sys\ref\nk1look.p (ipcCompany,
        'ROUND',
        'L',
        NO,
        NO,
        '',
        '', 
        OUTPUT cReturn,
        OUTPUT lFound).
    lResult = lFound AND cReturn EQ 'YES'.
    
    RETURN lResult.
        
END FUNCTION.

