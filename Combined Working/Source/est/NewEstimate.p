
/*------------------------------------------------------------------------
    File        : NewEstimate.p
    Purpose     : Creates a new estimate with a new estimate number and establishes default form, blank and other child records.

    This deprecates cec\new-est.p and ce\new-est.p

    Syntax      :

    Description : Initializes a New Estimate

    Author(s)   : BV
    Created     : Wed Jun 14 23:08:18 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcIndustry AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiEstType AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opriEb AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DEFINE BUFFER recalc-mr FOR reftable. /*for sys/ref/est-add.i REFACTOR*/

DEFINE VARIABLE iNextEstNum AS INTEGER NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fDefaultMetric RETURNS LOGICAL 
	( ipcCompany AS CHARACTER ) FORWARD.


/* ***************************  Main Block  *************************** */

RUN pGetNextEstNum (cocode,
                    locode,
                    OUTPUT iNextEstNum).

CREATE est.
ASSIGN 
    est.est-type = ipiEstType
    est.company  = cocode
    est.loc      = locode
    est.est-no   = STRING(iNextEstNum,">>>>>>>>")
    est.form-qty = 1
    est.est-date = TODAY
    est.mod-date = ?
    est.metric   = fDefaultMetric(cocode)
    .

CREATE est-qty.
ASSIGN 
    est-qty.company  = cocode
    est-qty.est-no   = est.est-no
    est-qty.eqty     = 0
    est-qty.qty-date = est.est-date
    .

IF ipcIndustry EQ 'C' THEN DO: 
    {sys/ref/est-add.i est C} /*Set est.recalc (CERUNC), est.override(CEGSA) and the recalc-mr reftable*/
END.
ELSE DO: /*REFACTOR - Compare with "C" procedures above*/
    {sys/ref/est-add.i est F}
END.

RUN est/NewEstimateForm.p (ipcIndustry, 
                           ROWID(est), 
                           OUTPUT opriEb). /*Continue to create child records*/

/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetNextEstNum:
/*------------------------------------------------------------------------------
 Purpose: Returns the next estimate number available
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER opiNextEstNum AS INTEGER NO-UNDO.

REPEAT:
    FIND FIRST ce-ctrl EXCLUSIVE-LOCK   
        WHERE ce-ctrl.company EQ ipcCompany
        AND ce-ctrl.loc EQ ipcLocation
        NO-ERROR NO-WAIT.

    IF AVAILABLE ce-ctrl THEN
    DO:
        ASSIGN
            opiNextEstNum = ce-ctrl.e-num + 1
            ce-ctrl.e-num = opiNextEstNum.
        FIND CURRENT ce-ctrl NO-LOCK.
        LEAVE.
    END.
END.

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fDefaultMetric RETURNS LOGICAL 
	( ipcCompany AS CHARACTER  ):
/*------------------------------------------------------------------------------
 Purpose: Returns the value of NK1 METRIC for purposes of defaulting new estimates
 Notes: est.metric field is updated
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO. 
    DEFINE VARIABLE lMetric  AS LOGICAL   NO-UNDO. 
    
    RUN sys\ref\nk1look.p (ipcCompany,
        'METRIC',
        'L',
        NO,
        NO,
        '',
        '', 
        OUTPUT cReturn,
        OUTPUT lFound).
    
    lMetric = lFound AND (cReturn EQ "YES").
    
    RETURN lMetric.
        		

END FUNCTION.


