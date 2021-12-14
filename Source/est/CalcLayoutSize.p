
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
DEFINE VARIABLE isFoamStyle   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cIndustryType AS CHARACTER NO-UNDO.
DEFINE VARIABLE dTrimLength   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dTrimWidth    AS DECIMAL   NO-UNDO.


DEFINE BUFFER bf-style   FOR style.
DEFINE BUFFER bf-item    FOR item.
DEFINE BUFFER bf-ef      FOR ef.
DEFINE BUFFER bf-eb      FOR eb.
DEFINE BUFFER bf-mach    FOR mach.
DEFINE BUFFER bf-ce-ctrl FOR ce-ctrl.


/* ***************************  Main Block  *************************** */

FIND bf-ef WHERE ROWID(bf-ef) EQ ipriEf NO-ERROR.
FIND bf-eb WHERE ROWID(bf-eb) EQ ipriEb NO-ERROR.


FIND FIRST bf-style NO-LOCK 
    WHERE bf-style.company = cocode 
    AND bf-style.bf-style = bf-eb.bf-style NO-ERROR.
    
IF AVAIL bf-style AND bf-style.type = "F" THEN isFoamStyle = YES.
IF AVAIL bf-style THEN 
    ASSIGN
        cIndustryType = bf-style.industry
        isFoamStyle   = YES WHEN bf-style.type = "F".

FIND FIRST bf-item NO-LOCK
    WHERE bf-item.company = bf-ef.company 
    AND bf-item.i-no EQ bf-ef.board NO-ERROR.

    
IF bf-ef.m-code NE "" THEN
DO:
    FIND FIRST bf-mach 
        WHERE bf-mach.company = bf-ef.company  
        AND bf-mach.loc     = bf-ef.loc
        AND bf-mach.m-code = bf-ef.m-code  NO-LOCK NO-ERROR.

    IF AVAIL bf-mach THEN 
    DO:
        ASSIGN 
            dTrimLength        = bf-mach.min-triml * 2
            dTrimWidth         = bf-mach.min-trimw * 2
            dLayoutSheetLength = bf-mach.max-wid
            dLayoutSheetWidth  = bf-mach.max-len
            IsRollMaterial     = bf-mach.p-type = "R". 
              
        IF bf-item.i-code EQ "E" THEN
            ASSIGN 
                dGrossSheetWidth  = ( trunc(bf-mach.max-len / bf-eb.t-len,0) * bf-eb.t-len + dTrimWidth)
                dGrossSheetLength = (trunc(bf-mach.max-wid / bf-eb.t-wid,0) * bf-eb.t-wid + dTrimLength )
                dGrossSheetDepth  = IF isFoamStyle THEN (trunc(bf-mach.max-dep / bf-eb.t-dep,0) * bf-eb.t-dep ) ELSE 0.
        
    END.
END. /* IF bf-ef.m-code NE "" THEN */

ELSE
DO:
    FIND FIRST bf-ce-ctrl NO-LOCK
        WHERE bf-ce-ctrl.company = cocode 
          AND bf-ce-ctrl.loc     = locode NO-ERROR.
    
    ASSIGN 
        dTrimLength        = bf-ce-ctrl.ls-triml * 2
        dTrimWidth         = bf-ce-ctrl.ls-trimw * 2
        IsRollMaterial     = bf-ce-ctrl.avg-cscost <> 0
        dLayoutSheetWidth  = bf-ce-ctrl.ls-length
        dLayoutSheetLength = (bf-ce-ctrl.ls-width).
        
END.        
        
IF AVAIL bf-item THEN
    ASSIGN
        dGrossSheetLength = (bf-item.s-len)
        dGrossSheetWidth  = (bf-item.s-wid)
        dGrossSheetDepth  = IF isFoamStyle THEN (bf-item.s-dep) ELSE 0.

IF dGrossSheetLength = 0 OR dGrossSheetWidth = 0 THEN 
    ASSIGN
        dGrossSheetLength = bf-eb.t-len
        dGrossSheetWidth  = bf-eb.t-wid.



IF AVAIL bf-item THEN 
DO:
    ASSIGN 
        cBoardItemCode        = bf-item.i-code
        cBoardItemBasisWeight = bf-item.basis-w.
    
    IF NOT bf-ef.lsh-lock THEN 
    DO:
        dBoardItemCaliperDimension   = bf-item.cal. 
           
        IF bf-item.i-code EQ "R" THEN 
        DO:
            IF IsRollMaterial THEN 
                ASSIGN 
                    dRollWidth         = bf-item.r-wid
                    dGrossSheetWidth   = bf-item.r-wid
                    dLayoutSheetLength = bf-item.r-wid
                    .
            ELSE 
            DO:
                ASSIGN 
                    dGrossSheetWidth   = bf-item.s-wid
                    dGrossSheetLength  = bf-item.s-len
                    dLayoutSheetLength = bf-item.s-len
                    dLayoutSheetWidth  = bf-item.s-wid
                    IsRollMaterial     = NO
                    dRollWidth         = 0
                    .
            END.
       
        END. /* i-code = "R" */
        ELSE
            IF bf-item.i-code EQ "E" THEN 
            DO:
                dGrossSheetWidth = dLayoutSheetWidth.
                
                ASSIGN 
                    dGrossSheetWidth  = dLayoutSheetWidth
                    dGrossSheetLength = dLayoutSheetLength
                    dNetSheetLength   = dGrossSheetLength
                    dNetSheetWidth    = dGrossSheetWidth
                    dRollWidth        = dGrossSheetWidth.
                
            END.   /* bf-item.i-code = "E" */
    END. /* IF NOT bf-ef.lsh-lock THEN */
END. /* IF AVAIL bf-item THEN */

ASSIGN
    iNumOutWidth  = MAX(iNumOutWidth,1)
    iNumOutLength = MAX(iNumOutLength,1)
    iNumOutDepth  = MAX(iNumOutDepth,1)
    iNumberCuts   = (iNumOutWidth - 1) + (iNumOutLength - 1) + (iNumOutDepth - 1).
  
ASSIGN 
    llen = dGrossSheetLength / iNumOutWidth
    lwid = dGrossSheetWidth / iNumOutLength.
  
IF avail(bf-item) AND bf-item.i-code EQ "E" AND bf-ef.xgrain = "N" AND AVAIL bf-mach THEN
    ASSIGN
        llen = min(llen, bf-mach.max-wid)
        lwid = min(lwid, bf-mach.max-len).



IF bf-ef.lam-dscr EQ "R" THEN
    ASSIGN  zzz  = llen
        llen = lwid
        lwid = zzz.
        
IF bf-eb.sty-lock OR ceroute-dec EQ 1 THEN 
DO:
    CREATE formule.
    ASSIGN
        formule[1]  = bf-eb.t-wid
        formule[3]  = bf-eb.t-wid
        formule[5]  = bf-eb.t-wid
        formule[7]  = bf-eb.t-wid
        formule[9]  = bf-eb.t-wid
        formule[2]  = bf-eb.t-len
        formule[4]  = bf-eb.t-len
        formule[6]  = bf-eb.t-len
        formule[8]  = bf-eb.t-len
        formule[10] = bf-eb.t-len
        formule[12] = bf-eb.die-in.
END.
          
ELSE 
DO:
    RUN est/u2kinc1c.p (RECID(bf-eb)).
    RUN est/u2kinc2c.p (RECID(bf-eb)).
    FIND FIRST formule.
END.

num = 0. /* zero array */

IF ceroute-dec EQ 1 THEN 
DO:
    ASSIGN
        op[1]         = "1"
        op[2]         = "1"

        iNumOutWidth  = 1
        iNumOutLength = 1.
      
    IF bf-ef.xgrain EQ "B" THEN
        ASSIGN
            num[1] = formule[1]
            num[2] = formule[2].
    ELSE
        ASSIGN
            num[2] = formule[1]
            num[1] = formule[2].
END.

ELSE 
DO:
    IF INDEX("B",bf-ef.xgrain) EQ 0 THEN 
    DO:
        /* aaa = 2 -> Blk W on Press.Len *** aaa = 1 -> Blk W on Press.Wid */
        ASSIGN  
            aaa = 2
            bbb = 1.

        DO i = 1 TO 50:
            j = i.
            IF i > 13 THEN j = 13.
            
            IF num[aaa] + formule[use-w[j] + (use-w[j] - 1)] <= (lwid - dTrimWidth)
                OR (NOT (avail(bf-item) AND bf-item.i-code = "R" AND bf-ef.xgrain EQ "N") AND i = 1)  /* at least 1 up!!! */
                THEN 
            DO:
                ASSIGN
                    op[aaa]  = STRING(i)
                    num[aaa] = num[aaa] + formule[use-w[j] + (use-w[j] - 1)].
            END.
            ELSE 
            DO:
                LEAVE.
            END.
        END.
        DO i = 1 TO 50:
            j = i. 
            IF i > 13 THEN j = 13.
            IF num[bbb] + formule[use-l[j] * 2] <= (llen - dTrimLength)
                OR (NOT (avail(bf-item) AND bf-item.i-code = "R" AND bf-ef.xgrain EQ "N") AND i = 1)
                THEN ASSIGN op[bbb]  = STRING(i)
                    num[bbb] = num[bbb] + formule[use-l[j] * 2].
            ELSE 
            DO:
                LEAVE.
            END.  
        END.
        IF num[aaa] = 0 THEN
            ASSIGN v-error-msg = "Invalid dimensions"
                op[aaa]     = "0"
                num[aaa]    = num[aaa] + formule[use-w[1] + (use-w[1] - 1)].
        IF num[bbb] = 0 THEN
            ASSIGN v-error-msg = "Invalid dimensions"
                op[bbb]     = "0"
                num[bbb]    = num[bbb] + formule[use-l[1] * 2].
                 
    END.

    ELSE 
        IF (bf-ef.lam-dscr EQ "R" OR
            (bf-ef.lam-dscr NE "R" AND index("SB",bf-ef.xgrain) GT 0)) THEN 
        DO:
            DEF VAR v-dim-to-use AS DEC NO-UNDO.
            ASSIGN 
                aaa = 1
                bbb = 2. /* aaa = # on layout width, bbb = # layout length */
            IF bf-ef.xgrain = "B" THEN
                v-dim-to-use = lwid - dTrimWidth.
            ELSE
                v-dim-to-use = llen - dTrimLength.
            DO i = 1 TO 50:
                j = i.
                IF i > 13 THEN j = 13.
                IF num[aaa] + formule[use-l[j] + (use-l[j] - 1)] <= v-dim-to-use
                    OR i = 1
                    THEN ASSIGN op[aaa]  = STRING(i)
                        num[aaa] = num[aaa] + formule[use-l[j] + (use-l[j] - 1)].
                ELSE LEAVE.
            END.

            IF bf-ef.xgrain = "B" THEN
                v-dim-to-use = llen - dTrimLength.
            ELSE
                v-dim-to-use = lwid - dTrimWidth.

            DO i = 1 TO 50:
                j = i.
                IF i > 13 THEN j = 13.
                IF num[bbb] + formule[use-w[j] * 2] <= v-dim-to-use
                    OR i = 1
                    THEN ASSIGN op[bbb]  = STRING(i)
                        num[bbb] = num[bbb] + formule[use-w[j] * 2].
                ELSE LEAVE.
            END.
        END.
END.

IF NOT bf-ef.lsh-lock THEN /* autocalc */
DO:
    IF bf-ef.xgrain = "B" THEN 
    DO:
        ASSIGN 
            iBlankNumberOnWidth  = int(op[1])
            iBlankNumberOnLength = int(op[2]).
        IF bf-eb.t-len * iBlankNumberOnLength GT num[2] THEN  num[2] = bf-eb.t-len * iBlankNumberOnLength.
        IF bf-eb.t-wid * iBlankNumberOnWidth GT num[1] THEN  num[1] = bf-eb.t-wid * iBlankNumberOnWidth.  
    END.
    ELSE 
    DO:
        ASSIGN 
            iBlankNumberOnWidth  = int(op[1])
            iBlankNumberOnLength = int(op[2]).      
        IF bf-eb.t-len * iBlankNumberOnWidth GT num[1] THEN  num[1] = bf-eb.t-len * iBlankNumberOnWidth.
        IF bf-eb.t-wid * iBlankNumberOnLength GT num[2] THEN  num[2] = bf-eb.t-wid * iBlankNumberOnLength.
    END.  
END.

ASSIGN 
    iBlankNumberUp              = iBlankNumberOnWidth * iBlankNumberOnLength
    dDieInchesRequiredForLayout = formule[12] * iBlankNumberUp  .

ASSIGN   
    dNetSheetWidth  = num[2] + dTrimWidth
    dNetSheetLength = num[1] + dTrimLength 
    dDieSizeWidth   = num[2]
    dDieSizeLength  = num[1].

IF dLayoutSheetWidth LT dNetSheetWidth THEN dLayoutSheetWidth = dNetSheetWidth.
IF dLayoutSheetLength LT dNetSheetLength THEN dLayoutSheetLength = dNetSheetLength.


IF INDEX("B",bf-ef.xgrain) EQ 0 AND bf-item.i-code NE "R"  THEN 
DO:

    IF dGrossSheetWidth LT dNetSheetWidth THEN dGrossSheetWidth = dNetSheetWidth.
    IF dGrossSheetLength LT dNetSheetLength THEN dGrossSheetLength = dNetSheetLength.
END.

IF bf-item.i-code EQ "E" THEN 
DO:
    IF AVAIL bf-mach AND bf-mach.dept[1] EQ "RC" THEN
        ASSIGN  dNetSheetWidth  = dNetSheetWidth - dTrimWidth
            dNetSheetLength = dNetSheetLength - dTrimLength.


    IF ceroute-dec NE 1 THEN        
        ASSIGN iNumOutWidth  = trunc(dLayoutSheetWidth / dNetSheetWidth,0)
            iNumOutLength = trunc(dLayoutSheetLength / dNetSheetLength,0).

    ASSIGN 
        iNumOutDepth   = 1
        dNetSheetDepth = bf-eb.t-dep
        dDieSizeDepth  = bf-eb.t-dep.
END.

IF AVAIL bf-mach THEN 
DO:
    IF iNumOutWidth  GT bf-mach.num-wid AND bf-mach.num-wid NE 0 THEN
        iNumOutWidth  = bf-mach.num-wid.
    IF iNumOutLength GT bf-mach.num-len AND bf-mach.num-len NE 0 THEN
        iNumOutLength = bf-mach.num-len.
END.

IF bf-item.i-code EQ "E" THEN 
DO:
    ASSIGN 
        iBlankNumberOnDepth = 1
        dGrossSheetWidth    = IF NOT AVAIL bf-item OR bf-item.i-code EQ "E" THEN
                         ( (iNumOutWidth  * dNetSheetWidth) +
                              IF AVAIL bf-mach AND bf-mach.dept[1] EQ "RC" THEN
                                dTrimWidth ELSE 0 )
                         ELSE dGrossSheetWidth
        dGrossSheetLength   = IF NOT AVAIL bf-item OR bf-item.i-code EQ "E" THEN
                                ( (iNumOutLength * dNetSheetLength) +
                              IF AVAIL bf-mach AND bf-mach.dept[1] EQ "RC" THEN
                                dTrimLength ELSE 0 )
                             ELSE dGrossSheetLength
        dGrossSheetDepth    = IF NOT AVAIL bf-item OR bf-item.i-code EQ "E" THEN
                              (iNumOutDepth * dNetSheetDepth)
                            ELSE dGrossSheetDepth.
                     
    IF iNumOutDepth EQ ? THEN iNumOutDepth = 0.
    IF dGrossSheetDepth EQ ? THEN dGrossSheetDepth = 0.
END.

IF isFoamStyle THEN
    ASSIGN
        dNetSheetDepth      = xeb.t-dep
        dDieSizeDepth       = xeb.t-dep
        dGrossSheetDepth    = IF bf-item.i-code EQ "E" THEN xeb.t-dep ELSE bf-item.s-dep
        iNumOutDepth        = IF dNetSheetDepth NE 0 THEN TRUNCATE(dGrossSheetDepth / dNetSheetDepth,0)
                      ELSE 1
        iBlankNumberOnDepth = 1. 

IF v-cecscrn-char NE "Decimal" THEN
    ASSIGN
        dGrossSheetLength = ROUND(dGrossSheetLength * li-16-32,0)
        dGrossSheetLength = dGrossSheetLength / li-16-32
        dGrossSheetWidth  = ROUND(dGrossSheetWidth * li-16-32,0)
        dGrossSheetWidth  = dGrossSheetWidth / li-16-32
        dGrossSheetDepth  = ROUND(dGrossSheetDepth * li-16-32,0)
        dGrossSheetDepth  = dGrossSheetDepth / li-16-32
        dNetSheetLength   = ROUND(dNetSheetLength * li-16-32,0)
        dNetSheetLength   = dNetSheetLength / li-16-32
        dNetSheetWidth    = ROUND(dNetSheetWidth * li-16-32,0)
        dNetSheetWidth    = dNetSheetWidth / li-16-32
        dNetSheetDepth    = ROUND(dNetSheetDepth * li-16-32,0)
        dNetSheetDepth    = dNetSheetDepth / li-16-32
        dDieSizeLength    = ROUND(dDieSizeLength * li-16-32,0)
        dDieSizeLength    = dDieSizeLength / li-16-32
        dDieSizeWidth     = ROUND(dDieSizeWidth * li-16-32,0)
        dDieSizeWidth     = dDieSizeWidth / li-16-32
        dDieSizeDepth     = ROUND(dDieSizeDepth * li-16-32,0)
        dDieSizeDepth     = dDieSizeDepth / li-16-32.

IF AVAIL(bf-item) AND bf-item.i-code EQ "E" AND bf-ef.xgrain = "S" THEN
    ASSIGN
        zzz               = dGrossSheetLength
        dGrossSheetLength = dGrossSheetWidth
        dGrossSheetWidth  = zzz.

        
        
        