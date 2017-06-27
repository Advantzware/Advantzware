
/*------------------------------------------------------------------------
    File        : CalcBlankSize.p
    Purpose     : Centralize the business logic to calculate the blank width and length from various estimate procedures and across estimate types

    Syntax      :

    Description : Calculates the blank width and length for a given estimate blank (eb)

    Author(s)   : BV
    Created     : Thu Jun 15 23:30:19 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcIndustry AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.

/*Refactor Globals or poorly defined shared temp-tables*/
DEFINE NEW SHARED TEMP-TABLE formule 
    FIELD formule AS DECIMAL EXTENT 12
    .
/*Refactor Global Buffers*/
DEFINE BUFFER bf-eb FOR eb.

/*Refactor - for cec/descalc*/
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
{sys/inc/var.i shared}
{cec/descalc.i NEW}
{sys/inc/f16to32.i}


DEFINE VARIABLE lRound            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lDecimal          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE dDecimalFactor    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dDecimalMax       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dConversionFactor AS DECIMAL   NO-UNDO.


DEFINE VARIABLE iCount            AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCount2           AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-score-char      AS CHARACTER EXTENT 100.
DEFINE VARIABLE ld-total          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-index           AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-str             AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION fRound RETURNS LOGICAL 
    ( ipcCompany AS CHARACTER ) FORWARD.
    
/* ***************************  Main Block  *************************** */


FIND eb NO-LOCK 
    WHERE ROWID(eb) EQ ipriEb
    .

IF AVAILABLE eb THEN 
DO:
    FIND FIRST style WHERE
        style.company = eb.company AND
        style.style = eb.style
        NO-LOCK NO-ERROR.
    FIND est OF eb NO-LOCK.
    FIND ef OF eb NO-LOCK. 
END.
IF NOT AVAILABLE style
    OR NOT AVAILABLE est
    OR NOT AVAILABLE ef
    OR NOT AVAILABLE eb THEN 
    RETURN.

/*Get Rounding Settings*/
lRound = fRound( eb.company ).  /*Get NK1 Round logical*/
RUN pGetDecimalSettings(eb.company,  /*Get NK1 CECSCRN settings*/
    OUTPUT lDecimal,
    OUTPUT dDecimalFactor,
    OUTPUT dDecimalMax, 
    OUTPUT dConversionFactor).

/*Calculate Style formulas and scoring*/        
RUN est/CalcStyleFormulae.p (ROWID(eb),
    lRound,
    lDecimal,
    dDecimalFactor,
    dConversionFactor).
                               
/*formule is populated by CalcStyleFormulae*/
FIND FIRST formule NO-LOCK NO-ERROR.

FIND bf-eb OF eb EXCLUSIVE-LOCK.    
ASSIGN 
    bf-eb.t-wid        = formule.formule[1]
    bf-eb.t-len        = formule.formule[2]
    bf-eb.t-sqin       = formule.formule[7] * formule.formule[8]
    bf-eb.k-wid-array2 = 0
    bf-eb.k-len-array2 = 0.
      
/*REFACTOR - Calculate Panels and Box Design*/
/*IF style.type EQ "F" THEN  /*Is Foam*/                                                                      */
    ASSIGN
        bf-eb.k-wid-array2[1] = bf-eb.t-wid
        bf-eb.k-len-array2[1] = bf-eb.t-len
        .
/*ELSE                                                                                                        */
/*DO:                                                                                                         */
/*    RUN cec/descalc.p (RECID(est),RECID(bf-eb)).                                                            */
/*                                                                                                            */
/*    DO iCount = 1 TO EXTENT(bf-eb.k-wid-scr-type2):                                                         */
/*        ASSIGN                                                                                              */
/*            bf-eb.k-wid-scr-type2[iCount] = lv-k-wid-scr-type[iCount]                                       */
/*            bf-eb.k-len-scr-type2[iCount] = lv-k-len-scr-type[iCount].                                      */
/*    END.                                                                                                    */
/*                                                                                                            */
/*    IF v-lscore-c BEGINS "No" THEN                                                                          */
/*        ASSIGN  eb.k-wid-array2[1] = eb.t-wid                                                               */
/*            eb.k-len-array2[1] = eb.t-len.                                                                  */
/*    ELSE                                                                                                    */
/*    DO:                                                                                                     */
/*        iCount = 0.                                                                                         */
/*        FOR EACH w-box-design-line:                                                                         */
/*            ASSIGN                                                                                          */
/*                iCount                   = iCount + 1                                                       */
/*                eb.k-wid-array2[iCount] = w-box-design-line.wscore-d.                                       */
/*                                                                                                            */
/*            {sys/inc/k16bb.i eb.k-wid-array2[iCount]}                                                       */
/*        END.                                                                                                */
/*                                                                                                            */
/*        ASSIGN                                                                                              */
/*            v-score-char = ""                                                                               */
/*            iCount2      = 1.                                                                               */
/*        MESSAGE v-lscore-c VIEW-AS ALERT-BOX.                                                               */
/*        DO iCount = 1 TO 80:                                                                                */
/*            IF SUBSTRING(v-lscore-c,i,1) NE "" THEN                                                         */
/*            DO:                                                                                             */
/*                v-score-char[iCount2] = v-score-char[iCount2] + substr(v-lscore-c,i,1).                     */
/*                IF substr(v-lscore-c,iCount + 1,1) EQ "" THEN                                               */
/*                    ASSIGN  v-score-char[iCount2] = TRIM(v-score-char[iCount2])                             */
/*                        iCount2               = iCount2 + 1.                                                */
/*            END.                                                                                            */
/*            IF iCount2 GT 12 THEN LEAVE.                                                                    */
/*        END.                                                                                                */
/*                                                                                                            */
/*/*REFACTOR*/                                                                                                */
/*/*        IF ll-add-set-part EQ NO AND ll-add-set-part-2 EQ NO THEN*/                                       */
/*/*        DO iCount = 1 TO EXTENT(xeb.k-len-array2):                                                      */*/
/*/*                                                                                                        */*/
/*/*            IF v-cecscrn-dec AND v-score-char[iCount] NE "" THEN                                        */*/
/*/*                ASSIGN                                                                                  */*/
/*/*                    v-index                            = INDEX(v-score-char[iCount],".")                */*/
/*/*                    v-str                              = SUBSTRING(v-score-char[iCount],v-index + 1)    */*/
/*/*                    v-str                              = LEFT-TRIM(STRING(INT(v-str) / 64.0,">.999999"))*/*/
/*/*                    SUBSTRING(v-score-char[iCount],v-index) = v-str.                                    */*/
/*/*                                                                                                        */*/
/*/*            eb.k-len-array2[iCount] = DECIMAL(v-score-char[iCount]).                                    */*/
/*/*            {sys/inc/k16bb.i eb.k-len-array2[iCount]}.                                                  */*/
/*/*        END.                                                                                            */*/
/*/*        ELSE                                                                           */                 */
/*/*        DO:                                                                            */                 */
/*/*            ld-total = 0.                                                              */                 */
/*/*            IF AVAILABLE style THEN                                                    */                 */
/*/*            DO iCount = 1 TO style.dim-df + 1:                                         */                 */
/*/*                                                                                       */                 */
/*/*                IF iCount EQ 1 THEN                                                    */                 */
/*/*                DO:                                                                    */                 */
/*/*                    IF ll-add-set-part EQ YES AND ll-add-set-part-2 EQ NO THEN         */                 */
/*/*                        eb.k-len-array2[iCount] = tt-eb-set-part.end-cell-length-1.    */                 */
/*/*                    ELSE                                                               */                 */
/*/*                        eb.k-len-array2[iCount] = v-end-cell-w1.                       */                 */
/*/*                END.                                                                   */                 */
/*/*                ELSE IF iCount EQ style.dim-df + 1 THEN                                */                 */
/*/*                    DO:                                                                */                 */
/*/*                        IF ll-add-set-part EQ YES AND ll-add-set-part-2 EQ NO THEN     */                 */
/*/*                            eb.k-len-array2[iCount] = tt-eb-set-part.end-cell-length-2.*/                 */
/*/*                        ELSE                                                           */                 */
/*/*                            eb.k-len-array2[iCount] = v-end-cell-w2.                   */                 */
/*/*                    END.                                                               */                 */
/*/*                    ELSE                                                               */                 */
/*/*                    DO:                                                                */                 */
/*/*                        IF ll-add-set-part EQ YES AND ll-add-set-part-2 EQ NO THEN     */                 */
/*/*                            eb.k-len-array2[iCount] = tt-eb-set-part.in-cell-length.   */                 */
/*/*                        ELSE                                                           */                 */
/*/*                            eb.k-len-array2[iCount] = v-in-cell-w.                     */                 */
/*/*                    END.                                                               */                 */
/*/*                                                                                       */                 */
/*/*                  {sys/inc/k16bb.i eb.k-len-array2[iCount]}.                           */                 */
/*/*                ld-total = ld-total + eb.k-len-array2[iCount].                         */                 */
/*/*            END.                                                                       */                 */
/*/*                                */                                                                        */
/*/*            eb.t-len = ld-total.*/                                                                        */
/*/*                                */                                                                        */
/*/*        END.                    */                                                                        */
/*    END.  /* else v-lscore */*/
/*END. /* panels or not foam */*/

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
