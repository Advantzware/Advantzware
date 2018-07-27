
/*------------------------------------------------------------------------
    File        : MachLimitsDims.p
    Purpose     : Tests Machine Dimensions

    Syntax      : Run est\MachLimitsDims.p (ROWID(style), ROWID(mach), ROWID(xeb), {1}, {2}, {3}, v-run, OUTPUT tt-mach-exc.reason).

    Description : Returns a reason character for all failures. 
                  Blank return value indicates valid sizes
                  **Deprecates cec\mach-dim.p**
                  

    Author(s)   : BV
    Created     : Tue May 09 14:17:27 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriStyle AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipriMach AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipdL AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdW AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdD AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdQty AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcReason AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-eb     FOR eb.
DEFINE BUFFER bf-mach   FOR mach.
DEFINE BUFFER bf-style FOR style.
DEFINE BUFFER bf-eb-set FOR eb.

DEFINE VARIABLE dDimensionToTest AS DECIMAL NO-UNDO.
{sys/inc/var.i shared}

DEFINE VARIABLE K_FRAC AS DECIMAL INIT 6.25 NO-UNDO.
{sys/inc/f16to32.i}

/* ********************  Preprocessor Definitions  ******************** */


/* ************************  Function Prototypes ********************** */

FUNCTION fIsAssemblyMach RETURNS LOGICAL 
	( ipcType AS CHARACTER ) FORWARD.

FUNCTION fIsAssemblyStyle RETURNS LOGICAL 
	( ipcType AS CHARACTER ) FORWARD.


/* ***************************  Main Block  *************************** */

FIND bf-eb NO-LOCK WHERE ROWID(bf-eb) EQ ipriEb NO-ERROR.
FIND bf-mach NO-LOCK WHERE ROWID(bf-mach) EQ ipriMach NO-ERROR.
FIND bf-style NO-LOCK WHERE ROWID(bf-style) EQ ipriStyle NO-ERROR.

IF AVAILABLE bf-eb AND AVAILABLE bf-mach AND AVAILABLE bf-style THEN 
DO:
    IF fIsAssemblyMach(bf-mach.p-type) AND fIsAssemblyStyle(bf-style.type) THEN DO:
        
        /*Override size inputs if Assembly*/
        FIND FIRST bf-eb-set NO-LOCK
            WHERE bf-eb-set.company EQ bf-eb.company
            AND bf-eb-set.est-no  EQ bf-eb.est-no
            AND bf-eb-set.form-no EQ 0
            NO-ERROR.
        IF AVAILABLE bf-eb-set THEN
            ASSIGN
                ipdL = bf-eb-set.len
                ipdW = bf-eb-set.wid
                ipdD = bf-eb-set.dep.
    END.

    IF bf-mach.min-len GT ipdL THEN 
        opcReason = opcReason +  STRING({sys/inc/k16.i ipdL}) + ' Under Min Front-To-Back of ' + STRING(bf-mach.min-len) + ', '.
    IF bf-mach.max-len LT ipdL AND bf-mach.max-len GT 0 THEN 
        opcReason = opcReason + STRING({sys/inc/k16.i ipdL}) + ' Over Max Front-To-Back of ' + STRING(bf-mach.max-len) + ', '.
    IF bf-mach.min-wid GT ipdW THEN
        opcReason = opcReason + STRING({sys/inc/k16.i ipdW}) + ' Under Min Side-To-Side of ' + STRING(bf-mach.min-wid) + ', '. 
    IF bf-mach.max-wid LT ipdW AND bf-mach.max-wid GT 0 THEN 
        opcReason = opcReason + STRING({sys/inc/k16.i ipdW}) + ' Over Max Side-To-Side of ' + STRING(bf-mach.max-wid) + ', '.
    IF bf-mach.min-cal GT ipdD THEN 
        opcReason = opcReason + STRING(ipdD) + ' Under Min Caliper/Depth of ' + STRING(bf-mach.min-cal) + ', '.
    IF bf-mach.max-cal LT ipdD AND bf-mach.max-cal GT 0 THEN 
        opcReason = opcReason + STRING(ipdD) + ' Over Max Caliper/Depth of ' + STRING(bf-mach.max-cal) + ', '.
    IF bf-mach.min-run GT ipdQty THEN 
        opcReason = opcReason + STRING(ipdQty) + ' Under Min Run Quantity of ' + STRING(bf-mach.min-run) + ', '.
    IF bf-mach.max-run LT ipdQty AND bf-mach.max-run GT 0 THEN 
        opcReason = opcReason + STRING(ipdQty) + ' Over Max Run Quantity of ' + STRING(bf-mach.max-run) + ', '.
    
    IF opcReason NE '' THEN opcReason = TRIM(opcReason,', ').     
END. /*Key records are all available*/ 
               


/* **********************  Internal Procedures  *********************** */

/* ************************  Function Implementations ***************** */

FUNCTION fIsAssemblyMach RETURNS LOGICAL 
	( ipcType AS CHARACTER  ):
/*------------------------------------------------------------------------------
 Purpose: Returns True if the machine type passed is assembly
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
        
        lResult = INDEX('AP',ipcType) GT 0.
    
		RETURN lResult.


		
END FUNCTION.

FUNCTION fIsAssemblyStyle RETURNS LOGICAL 
	( ipcType AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
		
        lResult = INDEX('PR',ipcType) GT 0.
    
		RETURN lResult.


		
END FUNCTION.


