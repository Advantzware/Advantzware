
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


DEFINE INPUT PARAMETER ipriEf       AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipriEb       AS ROWID NO-UNDO.


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



DEFINE VARIABLE ceroute-dec AS DECIMAL NO-UNDO. // ASk  BRAD


DEFINE TEMP-TABLE ttLayoutDims NO-UNDO
    FIELD dLayoutSheetLength    AS DECIMAL  // xef.lsh-len- Side to Side Layout Length
    FIELD dLayoutSheetWidth     AS DECIMAL  // xef.lsh-wid- Front to Back Layout Width
    FIELD dNetSheetLength       AS DECIMAL  // xef.nsh-len
    FIELD dNetSheetWidth        AS DECIMAL  // xef.nsh-wid
    FIELD dNetSheetDepth        AS DECIMAL  // xef.nsh-dep
    FIELD dGrossSheetLength     AS DECIMAL  // xef.gsh-len 
    FIELD dGrossSheetWidth      AS DECIMAL  // xef.gsh-wid
    FIELD dGrossSheetDepth      AS DECIMAL  // xef.gsh-dep
    FIELD dDieSizeLength        AS DECIMAL  // xef.trim-l
    FIELD dDieSizeWidth         AS DECIMAL  // xef.trim-w
    FIELD dDieSizeDepth         AS DECIMAL  // xef.trim-d 
    FIELD dRollWidth            AS DECIMAL  // xef.roll-wid
    FIELD dDieInchesRequired    AS DECIMAL  // xef.die-in - Total die inches required for the layout
    FIELD cBoardItemCode        AS CHARACTER  // xef.i-code
    FIELD cBoardItemBasisWeight AS DECIMAL  // xef.weight
    FIELD dBoardItemCaliper     AS DECIMAL  // xef.cal
    FIELD IsRollMaterial        AS LOGICAL  // xef.roll - Is this a Roll material? 
    FIELD iNumOutWidth          AS DECIMAL  // xef.n-out - Number Out
    FIELD iNumOutLength         AS DECIMAL  // xef.n-out-l
    FIELD iNumOutDepth          AS DECIMAL  //  xef.n-out-d
    FIELD iNumberCuts           AS DECIMAL  // xef.n-cuts
    FIELD iBlankNumUp           AS DECIMAL  // xeb.num-up 
    FIELD iBlankNumOnWidth      AS DECIMAL  // xeb.num-wid
    FIELD iBlankNumOnLength     AS DECIMAL  // xeb.num-len
    FIELD iBlankNumOnDepth      AS DECIMAL  // xeb.num-len
    .
    

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

FIND FIRST bf-item NO-LOCK
    WHERE bf-item.company = bf-ef.company 
    AND bf-item.i-no EQ bf-ef.board NO-ERROR.

FIND FIRST bf-style NO-LOCK 
    WHERE bf-style.company = bf-ef.company  
    AND bf-style.style = bf-eb.style NO-ERROR.
    
IF AVAILABLE bf-style AND bf-style.type = "F" THEN isFoamStyle = YES.
IF AVAILABLE bf-style THEN 
    ASSIGN
        cIndustryType = bf-style.industry
        isFoamStyle   = YES WHEN bf-style.type = "F".

/* Create Temp-table record and populate the fields later */    
CREATE ttLayoutDims.    
    
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
            ttLayoutDims.dLayoutSheetLength = bf-mach.max-wid
            ttLayoutDims.dLayoutSheetWidth  = bf-mach.max-len
            ttLayoutDims.IsRollMaterial     = bf-mach.p-type = "R". 
              
        IF bf-item.i-code EQ "E" THEN
            ASSIGN 
                ttLayoutDims.dGrossSheetWidth  = ( trunc(bf-mach.max-len / bf-eb.t-len,0) * bf-eb.t-len + dTrimWidth)
                ttLayoutDims.dGrossSheetLength = (trunc(bf-mach.max-wid / bf-eb.t-wid,0) * bf-eb.t-wid + dTrimLength )
                ttLayoutDims.dGrossSheetDepth  = IF isFoamStyle THEN (trunc(bf-mach.max-dep / bf-eb.t-dep,0) * bf-eb.t-dep ) ELSE 0.
        
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
        ttLayoutDims.dLayoutSheetWidth  = bf-ce-ctrl.ls-length
        ttLayoutDims.dLayoutSheetLength = bf-ce-ctrl.ls-width.
        
END.        
        
IF AVAILABLE bf-item THEN
    ASSIGN
        ttLayoutDims.dGrossSheetLength = (bf-item.s-len)
        ttLayoutDims.dGrossSheetWidth  = (bf-item.s-wid)
        ttLayoutDims.dGrossSheetDepth  = IF isFoamStyle THEN (bf-item.s-dep) ELSE 0.

IF ttLayoutDims.dGrossSheetLength = 0 OR ttLayoutDims.dGrossSheetWidth = 0 THEN 
    ASSIGN
        ttLayoutDims.dGrossSheetLength = bf-eb.t-len
        ttLayoutDims.dGrossSheetWidth  = bf-eb.t-wid.



IF AVAILABLE bf-item THEN 
DO:
    ASSIGN 
        ttLayoutDims.cBoardItemCode        = bf-item.i-code
        ttLayoutDims.cBoardItemBasisWeight = bf-item.basis-w.
    
    IF NOT bf-ef.lsh-lock THEN 
    DO:
        ttLayoutDims.dBoardItemCaliper   = bf-item.cal. 
           
        IF bf-item.i-code EQ "R" THEN 
        DO:
            IF bf-item.r-wid GT 0  THEN 
                ASSIGN 
                    ttLayoutDims.dRollWidth         = bf-item.r-wid
                    ttLayoutDims.dGrossSheetWidth   = bf-item.r-wid
                    ttLayoutDims.dLayoutSheetLength = bf-item.r-wid
                    ttLayoutDims.IsRollMaterial     = YES
                    .
            ELSE 
            DO:
                ASSIGN 
                    ttLayoutDims.dGrossSheetWidth   = bf-item.s-wid
                    ttLayoutDims.dGrossSheetLength  = bf-item.s-len
                    ttLayoutDims.dLayoutSheetLength = bf-item.s-len
                    ttLayoutDims.dLayoutSheetWidth  = bf-item.s-wid
                    ttLayoutDims.IsRollMaterial     = NO
                    ttLayoutDims.dRollWidth         = 0
                    .
            END.
       
        END. /* i-code = "R" */
        /* ELSE item.i-code EQ "E" */
        ELSE 
        DO:
            ASSIGN 
                ttLayoutDims.dGrossSheetWidth  = ttLayoutDims.dLayoutSheetWidth
                ttLayoutDims.dGrossSheetLength = ttLayoutDims.dLayoutSheetLength
                ttLayoutDims.dNetSheetLength   = ttLayoutDims.dGrossSheetLength
                ttLayoutDims.dNetSheetWidth    = ttLayoutDims.dGrossSheetWidth
                ttLayoutDims.dRollWidth        = ttLayoutDims.dGrossSheetWidth.
            
        END.   /* bf-item.i-code = "E" */
    END. /* IF NOT bf-ef.lsh-lock THEN */
END. /* IF AVAIL bf-item THEN */

ASSIGN
    ttLayoutDims.iNumOutWidth  = MAX(ttLayoutDims.iNumOutWidth,1)
    ttLayoutDims.iNumOutLength = MAX(ttLayoutDims.iNumOutLength,1)
    ttLayoutDims.iNumOutDepth  = MAX(ttLayoutDims.iNumOutDepth,1)
    ttLayoutDims.iNumberCuts   = (ttLayoutDims.iNumOutWidth - 1) + (ttLayoutDims.iNumOutLength - 1) + (ttLayoutDims.iNumOutDepth - 1).
  
ASSIGN 
    dTempLength = ttLayoutDims.dGrossSheetLength / ttLayoutDims.iNumOutWidth
    dTempWidth  = ttLayoutDims.dGrossSheetWidth / ttLayoutDims.iNumOutLength.
  
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
        ttLayoutDims.iNumOutWidth  = 1
        ttLayoutDims.iNumOutLength = 1.
      
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
        
        
    DO iCnt = 1 TO 50:
        iExt = iCnt.
        
        IF iCnt > 13 THEN 
            iExt = 13.
        
        /* Calculate Length Size using Panel dimension */    
        IF iCnt = 1 OR dCalcTotalLength + formule.formule[use-l[iExt] + (use-l[iExt] - 1)] <= dLengthtoUse THEN
            ASSIGN 
                iCalcNumOnLength = iCnt
                dCalcTotalLength = dCalcTotalLength + formule.formule[use-l[iExt] + (use-l[iExt] - 1)].
                
        /* Calculate Width Size using Panel dimension */
        IF iCnt = 1 OR dCalcTotalWidth + formule.formule[use-w[iExt] * 2] <= dWidtoUse THEN 
            ASSIGN 
                iCalcNumOnWidth = iCnt
                dCalcTotalWidth = dCalcTotalWidth + formule.formule[use-w[iExt] * 2].
        
    END.
    
END.

IF NOT bf-ef.lsh-lock THEN /* autocalc */
DO:
    IF bf-ef.xgrain = "B" THEN 
    DO:
        ASSIGN 
            ttLayoutDims.iBlankNumOnWidth  = iCalcNumOnWidth
            ttLayoutDims.iBlankNumOnLength = iCalcNumOnLength.
            
        IF bf-eb.t-len * ttLayoutDims.iBlankNumOnLength GT dCalcTotalWidth THEN  
            dCalcTotalWidth = bf-eb.t-len * ttLayoutDims.iBlankNumOnLength.
        IF bf-eb.t-wid * ttLayoutDims.iBlankNumOnWidth GT dCalcTotalLength THEN  
            dCalcTotalLength = bf-eb.t-wid * ttLayoutDims.iBlankNumOnWidth.  
    END.
    ELSE 
    DO:
        ASSIGN 
            ttLayoutDims.iBlankNumOnWidth  = iCalcNumOnWidth
            ttLayoutDims.iBlankNumOnLength = iCalcNumOnLength.   
               
        IF bf-eb.t-len * ttLayoutDims.iBlankNumOnWidth GT dCalcTotalLength THEN  
            dCalcTotalLength = bf-eb.t-len * ttLayoutDims.iBlankNumOnWidth.
        IF bf-eb.t-wid * ttLayoutDims.iBlankNumOnLength GT dCalcTotalWidth THEN  
            dCalcTotalWidth = bf-eb.t-wid * ttLayoutDims.iBlankNumOnLength.
    END.  
END.

ASSIGN 
    ttLayoutDims.iBlankNumUp        = ttLayoutDims.iBlankNumOnWidth * ttLayoutDims.iBlankNumOnLength
    ttLayoutDims.dDieInchesRequired = formule.formule[12] * ttLayoutDims.iBlankNumUp  .

ASSIGN   
    ttLayoutDims.dNetSheetWidth  = dCalcTotalWidth + dTrimWidth
    ttLayoutDims.dNetSheetLength = dCalcTotalLength + dTrimLength 
    ttLayoutDims.dDieSizeWidth   = dCalcTotalWidth
    ttLayoutDims.dDieSizeLength  = dCalcTotalLength.

IF ttLayoutDims.dLayoutSheetWidth LT ttLayoutDims.dNetSheetWidth THEN 
    ttLayoutDims.dLayoutSheetWidth = ttLayoutDims.dNetSheetWidth.
    
IF ttLayoutDims.dLayoutSheetLength LT ttLayoutDims.dNetSheetLength THEN 
    ttLayoutDims.dLayoutSheetLength = ttLayoutDims.dNetSheetLength.


IF INDEX("B",bf-ef.xgrain) EQ 0 AND bf-item.i-code NE "R"  THEN 
DO:

    IF ttLayoutDims.dGrossSheetWidth LT ttLayoutDims.dNetSheetWidth THEN 
        ttLayoutDims.dGrossSheetWidth = ttLayoutDims.dNetSheetWidth.
    IF ttLayoutDims.dGrossSheetLength LT ttLayoutDims.dNetSheetLength THEN 
        ttLayoutDims.dGrossSheetLength = ttLayoutDims.dNetSheetLength.
END.

IF bf-item.i-code EQ "E" THEN 
DO:
    IF AVAILABLE bf-mach AND bf-mach.dept[1] EQ "RC" THEN
        ASSIGN  
            ttLayoutDims.dNetSheetWidth  = ttLayoutDims.dNetSheetWidth - dTrimWidth
            ttLayoutDims.dNetSheetLength = ttLayoutDims.dNetSheetLength - dTrimLength.


    IF ceroute-dec NE 1 THEN        
        ASSIGN 
            ttLayoutDims.iNumOutWidth  = TRUNCATE(ttLayoutDims.dLayoutSheetWidth / ttLayoutDims.dNetSheetWidth,0)
            ttLayoutDims.iNumOutLength = TRUNCATE(ttLayoutDims.dLayoutSheetLength / ttLayoutDims.dNetSheetLength,0).

    ASSIGN 
        ttLayoutDims.iNumOutDepth   = 1
        ttLayoutDims.dNetSheetDepth = bf-eb.t-dep
        ttLayoutDims.dDieSizeDepth  = bf-eb.t-dep.
END.

/* Check machine limits */
IF AVAILABLE bf-mach THEN 
DO:
    IF ttLayoutDims.iNumOutWidth  GT bf-mach.num-wid AND bf-mach.num-wid NE 0 THEN
        ttLayoutDims.iNumOutWidth  = bf-mach.num-wid.
    IF ttLayoutDims.iNumOutLength GT bf-mach.num-len AND bf-mach.num-len NE 0 THEN
        ttLayoutDims.iNumOutLength = bf-mach.num-len.
END.

IF bf-item.i-code EQ "E" THEN 
DO:
    ASSIGN 
        ttLayoutDims.iBlankNumOnDepth  = 1
        ttLayoutDims.dGrossSheetWidth  = (ttLayoutDims.iNumOutWidth  * ttLayoutDims.dNetSheetWidth)
        ttLayoutDims.dGrossSheetLength = (ttLayoutDims.iNumOutLength * ttLayoutDims.dNetSheetLength)
        ttLayoutDims.dGrossSheetDepth  = (ttLayoutDims.iNumOutDepth * ttLayoutDims.dNetSheetDepth)
        .
        
    IF AVAILABLE bf-mach AND bf-mach.dept[1] EQ "RC" THEN
        ASSIGN  
            ttLayoutDims.dGrossSheetWidth  = ttLayoutDims.dGrossSheetWidth + dTrimWidth 
            ttLayoutDims.dGrossSheetLength = ttLayoutDims.dGrossSheetLength + dTrimLength. 
    
END.
                                

IF isFoamStyle THEN
    ASSIGN
        ttLayoutDims.dNetSheetDepth   = bf-eb.t-dep
        ttLayoutDims.dDieSizeDepth    = bf-eb.t-dep
        ttLayoutDims.dGrossSheetDepth = IF bf-item.i-code EQ "E" THEN bf-eb.t-dep ELSE bf-item.s-dep
        ttLayoutDims.iNumOutDepth     = IF ttLayoutDims.dNetSheetDepth NE 0 THEN TRUNCATE(ttLayoutDims.dGrossSheetDepth / ttLayoutDims.dNetSheetDepth,0) ELSE 1
        ttLayoutDims.iBlankNumOnDepth = 1. 

IF lDecimal = NO THEN
    ASSIGN
        ttLayoutDims.dGrossSheetLength = ROUND(ttLayoutDims.dGrossSheetLength * dDecimalFactor,0)
        ttLayoutDims.dGrossSheetLength = ttLayoutDims.dGrossSheetLength / dDecimalFactor
        ttLayoutDims.dGrossSheetWidth  = ROUND(ttLayoutDims.dGrossSheetWidth * dDecimalFactor,0)
        ttLayoutDims.dGrossSheetWidth  = ttLayoutDims.dGrossSheetWidth / dDecimalFactor
        ttLayoutDims.dGrossSheetDepth  = ROUND(ttLayoutDims.dGrossSheetDepth * dDecimalFactor,0)
        ttLayoutDims.dGrossSheetDepth  = ttLayoutDims.dGrossSheetDepth / dDecimalFactor
        ttLayoutDims.dNetSheetLength   = ROUND(ttLayoutDims.dNetSheetLength * dDecimalFactor,0)
        ttLayoutDims.dNetSheetLength   = ttLayoutDims.dNetSheetLength / dDecimalFactor
        ttLayoutDims.dNetSheetWidth    = ROUND(ttLayoutDims.dNetSheetWidth * dDecimalFactor,0)
        ttLayoutDims.dNetSheetWidth    = ttLayoutDims.dNetSheetWidth / dDecimalFactor
        ttLayoutDims.dNetSheetDepth    = ROUND(ttLayoutDims.dNetSheetDepth * dDecimalFactor,0)
        ttLayoutDims.dNetSheetDepth    = ttLayoutDims.dNetSheetDepth / dDecimalFactor
        ttLayoutDims.dDieSizeLength    = ROUND(ttLayoutDims.dDieSizeLength * dDecimalFactor,0)
        ttLayoutDims.dDieSizeLength    = ttLayoutDims.dDieSizeLength / dDecimalFactor
        ttLayoutDims.dDieSizeWidth     = ROUND(ttLayoutDims.dDieSizeWidth * dDecimalFactor,0)
        ttLayoutDims.dDieSizeWidth     = ttLayoutDims.dDieSizeWidth / dDecimalFactor
        ttLayoutDims.dDieSizeDepth     = ROUND(ttLayoutDims.dDieSizeDepth * dDecimalFactor,0)
        ttLayoutDims.dDieSizeDepth     = ttLayoutDims.dDieSizeDepth / dDecimalFactor.

IF AVAIL(bf-item) AND bf-item.i-code EQ "E" AND bf-ef.xgrain = "S" THEN
    ASSIGN
        dSwapDim                       = ttLayoutDims.dGrossSheetLength
        ttLayoutDims.dGrossSheetLength = ttLayoutDims.dGrossSheetWidth
        ttLayoutDims.dGrossSheetWidth  = dSwapDim.



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

