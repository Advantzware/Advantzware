
/*------------------------------------------------------------------------
    File        : MachLimitsSlot.p
    Purpose     : Specific Logic for Slot Size Test

    Syntax      : Run est\MachLimitsSlot.p (ROWID(style), ROWID(mach), ROWID(xeb), OUTPUT tt-mach-exc.reason).

    Description : Returns a reason character for all failures. 
                  Blank return value indicates valid sizes
                  **Deprecates cec\machslot.i**
                  

    Author(s)   : BV
    Created     : Tue May 09 14:17:27 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriStyle AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipriMach AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER opcReason AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-eb    FOR eb.
DEFINE BUFFER bf-style FOR style.
DEFINE BUFFER bf-mach  FOR mach.

DEFINE VARIABLE dDimensionToTest AS DECIMAL NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */


/* ************************  Function Prototypes ********************** */


FUNCTION fIsPrinterOrDieCutter RETURNS LOGICAL 
    ( cDept LIKE mach.dept ) FORWARD.


/* ***************************  Main Block  *************************** */

FIND bf-eb NO-LOCK WHERE ROWID(bf-eb) EQ ipriEb NO-ERROR.
FIND bf-mach NO-LOCK WHERE ROWID(bf-mach) EQ ipriMach NO-ERROR.
FIND bf-style NO-LOCK WHERE ROWID(bf-style) EQ ipriStyle NO-ERROR.

IF AVAILABLE bf-eb AND AVAILABLE bf-mach AND AVAILABLE bf-style THEN 
DO:
    IF bf-style.type EQ 'B' AND fIsPrinterOrDieCutter(bf-mach.dept) /*If box style and machine is PR or DC*/
        THEN 
    DO:
        dDimensionToTest = bf-eb.k-wid-array2[1]. /*first panel on blank width*/
        IF dDimensionToTest LT bf-mach.min-dep THEN 
            opcReason = opcReason + STRING(dDimensionToTest) + ' Under Min Slot Size of ' + STRING(bf-mach.min-dep) + ', '.
        IF dDimensionToTest GT bf-mach.max-dep AND bf-mach.max-dep GT 0 THEN 
            opcReason = opcReason + STRING(dDimensionToTest) + ' Over Max Slot Size of ' + STRING(bf-mach.max-dep) + ', '.
        
    END. /*Machine is Printer or Die Cutter and the Style is a Box*/
    
    IF opcReason NE '' THEN opcReason = TRIM(opcReason,', ').     
END. /*Key records are all available*/ 
               


/* **********************  Internal Procedures  *********************** */

/* ************************  Function Implementations ***************** */


FUNCTION fIsPrinterOrDieCutter RETURNS LOGICAL 
    ( cDept LIKE mach.dept ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE i       AS INTEGER NO-UNDO.
		
    DO i = 1 TO 4:
        lResult = lResult OR cDept[i] EQ 'PR' OR cDept[i] EQ 'DC'.  
    END.

    RETURN lResult.


		
END FUNCTION.


