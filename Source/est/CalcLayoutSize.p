
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

DEFINE INPUT PARAMETER ipriEf       AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipriEb       AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE       FOR ttLayoutSize.


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
        cIndustryType = bf-style.industry
        isFoamStyle   = YES WHEN bf-style.type = "F"
        iStyleFormulaOnLen = bf-style.use-l
        iStyleFormulaOnWid = bf-style.use-w.

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
              
        IF bf-item.i-code EQ "E" THEN
            ASSIGN 
                ttLayoutSize.dGrossSheetWidth  = ( trunc(bf-mach.max-len / bf-eb.t-len,0) * bf-eb.t-len + dTrimWidth)
                ttLayoutSize.dGrossSheetLength = (trunc(bf-mach.max-wid / bf-eb.t-wid,0) * bf-eb.t-wid + dTrimLength )
                ttLayoutSize.dGrossSheetDepth  = IF isFoamStyle THEN (trunc(bf-mach.max-dep / bf-eb.t-dep,0) * bf-eb.t-dep ) ELSE 0.
        
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
        
END.        
        
IF AVAILABLE bf-item THEN
    ASSIGN
        ttLayoutSize.dGrossSheetLength = (bf-item.s-len)
        ttLayoutSize.dGrossSheetWidth  = (bf-item.s-wid)
        ttLayoutSize.dGrossSheetDepth  = IF isFoamStyle THEN (bf-item.s-dep) ELSE 0.

IF ttLayoutSize.dGrossSheetLength = 0 OR ttLayoutSize.dGrossSheetWidth = 0 THEN 
    ASSIGN
        ttLayoutSize.dGrossSheetLength = bf-eb.t-len
        ttLayoutSize.dGrossSheetWidth  = bf-eb.t-wid.



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
                ASSIGN 
                    ttLayoutSize.dRollWidth         = bf-item.r-wid
                    ttLayoutSize.dGrossSheetWidth   = bf-item.r-wid
                    ttLayoutSize.dLayoutSheetLength = bf-item.r-wid
                    ttLayoutSize.IsRollMaterial     = YES
                    .
            ELSE 
            DO:
                ASSIGN 
                    ttLayoutSize.dGrossSheetWidth   = bf-item.s-wid
                    ttLayoutSize.dGrossSheetLength  = bf-item.s-len
                    ttLayoutSize.dLayoutSheetLength = bf-item.s-len
                    ttLayoutSize.dLayoutSheetWidth  = bf-item.s-wid
                    ttLayoutSize.IsRollMaterial     = NO
                    ttLayoutSize.dRollWidth         = 0
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
            
        END.   /* bf-item.i-code = "E" */
    END. /* IF NOT bf-ef.lsh-lock THEN */
END. /* IF AVAIL bf-item THEN */

ASSIGN
    ttLayoutSize.iNumOutWidth  = MAX(ttLayoutSize.iNumOutWidth,1)
    ttLayoutSize.iNumOutLength = MAX(ttLayoutSize.iNumOutLength,1)
    ttLayoutSize.iNumOutDepth  = MAX(ttLayoutSize.iNumOutDepth,1)
    ttLayoutSize.iNumberCuts   = (ttLayoutSize.iNumOutWidth - 1) + (ttLayoutSize.iNumOutLength - 1) + (ttLayoutSize.iNumOutDepth - 1).
  
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
        
IF bf-eb.sty-lock OR ceroute-dec EQ 1 THEN 
DO:
    CREATE formule.
    ASSIGN
        formule.formule[1]  = bf-eb.t-wid
        formule.formule[3]  = bf-eb.t-wid
        formule.formule[5]  = bf-eb.t-wid
        formule.formule[7]  = bf-eb.t-wid
        formule.formule[9]  = bf-eb.t-wid
        formule.formule[2]  = bf-eb.t-len
        formule.formule[4]  = bf-eb.t-len
        formule.formule[6]  = bf-eb.t-len
        formule.formule[8]  = bf-eb.t-len
        formule.formule[10] = bf-eb.t-len
        formule.formule[12] = bf-eb.die-in.
END.
          
ELSE 
DO:
    /*Get Rounding Settings*/
    lRound = fRound( bf-eb.company ).  /*Get NK1 Round logical*/
    RUN pGetDecimalSettings(bf-eb.company,  /*Get NK1 CECSCRN settings*/
        OUTPUT lDecimal,
        OUTPUT dDecimalFactor,
        OUTPUT dDecimalMax, 
        OUTPUT dConversionFactor).

    /*Calculate Style formulas and scoring*/        
    RUN est/CalcStyleFormulae.p (ROWID(bf-eb),
        lRound,
        lDecimal,
        dDecimalFactor,
        dConversionFactor,
        OUTPUT TABLE formule).
END.

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
    IF bf-ef.xgrain = "B" THEN
        ASSIGN 
            dLengthtoUse = dTempWidth - dTrimWidth
            dWidtoUse    = dTempLength - dTrimLength.
    ELSE
        ASSIGN
            dLengthtoUse = dTempLength - dTrimLength
            dWidtoUse    = dTempWidth - dTrimWidth.
        
    /* Calculate Width Size using Panel dimension */    
    DO iCnt = 1 TO 50:
        iExt = iCnt.        
        IF iCnt > 13 THEN 
            iExt = 13.
        
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
    END. // DO iCnt = 1 TO 50:
     
    /* Calculate Length Size using Panel dimension */    
    DO iCnt = 1 TO 50:
        iExt = iCnt.        
        IF iCnt > 13 THEN 
            iExt = 13.
            
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
         
    END. // DO iCnt = 1 TO 50:
    
END. /* ELSE*/


IF NOT bf-ef.lsh-lock THEN /* autocalc */
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
            ttLayoutSize.iBlankNumOnWidth  = iCalcNumOnWidth
            ttLayoutSize.iBlankNumOnLength = iCalcNumOnLength.   
        
              
        IF bf-eb.t-len * ttLayoutSize.iBlankNumOnLength GT dCalcTotalLength THEN  
            dCalcTotalLength = bf-eb.t-len * ttLayoutSize.iBlankNumOnLength.
        IF bf-eb.t-wid * ttLayoutSize.iBlankNumOnWidth GT dCalcTotalWidth THEN  
            dCalcTotalWidth = bf-eb.t-wid * ttLayoutSize.iBlankNumOnWidth.
    END.  
END.

ASSIGN 
    ttLayoutSize.iBlankNumUp        = ttLayoutSize.iBlankNumOnWidth * ttLayoutSize.iBlankNumOnLength
    ttLayoutSize.dDieInchesRequired = formule.formule[12] * ttLayoutSize.iBlankNumUp  .

ASSIGN   
    ttLayoutSize.dNetSheetWidth  = dCalcTotalWidth + dTrimWidth
    ttLayoutSize.dNetSheetLength = dCalcTotalLength + dTrimLength 
    ttLayoutSize.dDieSizeWidth   = dCalcTotalWidth
    ttLayoutSize.dDieSizeLength  = dCalcTotalLength.
    

IF ttLayoutSize.dLayoutSheetWidth LT ttLayoutSize.dNetSheetWidth THEN 
    ttLayoutSize.dLayoutSheetWidth = ttLayoutSize.dNetSheetWidth.
    
IF ttLayoutSize.dLayoutSheetLength LT ttLayoutSize.dNetSheetLength THEN 
    ttLayoutSize.dLayoutSheetLength = ttLayoutSize.dNetSheetLength.


IF INDEX("B",bf-ef.xgrain) EQ 0 AND bf-item.i-code NE "R"  THEN 
DO:

    IF ttLayoutSize.dGrossSheetWidth LT ttLayoutSize.dNetSheetWidth THEN 
        ttLayoutSize.dGrossSheetWidth = ttLayoutSize.dNetSheetWidth.
    IF ttLayoutSize.dGrossSheetLength LT ttLayoutSize.dNetSheetLength THEN 
        ttLayoutSize.dGrossSheetLength = ttLayoutSize.dNetSheetLength.
END.

IF bf-item.i-code EQ "E" THEN 
DO:
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
END.

/* Check machine limits */
IF AVAILABLE bf-mach THEN 
DO:
    IF ttLayoutSize.iNumOutWidth  GT bf-mach.num-wid AND bf-mach.num-wid NE 0 THEN
        ttLayoutSize.iNumOutWidth  = bf-mach.num-wid.
    IF ttLayoutSize.iNumOutLength GT bf-mach.num-len AND bf-mach.num-len NE 0 THEN
        ttLayoutSize.iNumOutLength = bf-mach.num-len.
END.

IF bf-item.i-code EQ "E" THEN 
DO:
    ASSIGN 
        ttLayoutSize.iBlankNumOnDepth  = 1
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

IF AVAIL(bf-item) AND bf-item.i-code EQ "E" AND bf-ef.xgrain = "S" THEN
    ASSIGN
        dSwapDim                       = ttLayoutSize.dGrossSheetLength
        ttLayoutSize.dGrossSheetLength = ttLayoutSize.dGrossSheetWidth
        ttLayoutSize.dGrossSheetWidth  = dSwapDim.



/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetDecimalSettings:
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

