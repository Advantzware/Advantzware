
/*------------------------------------------------------------------------
    File        : NewEstimateForm.p
    Purpose     : Initializes the base form for an estimate before additiona input assignments.  Deprecates ce\new-form.p and cec\new-form.p

    Syntax      :

    Description : Creates a New Form for the Estimate passed as parameter

    Author(s)   : BV
    Created     : Thu Jun 15 21:42:21 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcIndustry AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  ipriEst AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER opriEb AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DEFINE VARIABLE iForm AS INTEGER NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND FIRST ce-ctrl NO-LOCK 
    {sys/look/ce-ctrlW.i}
    NO-ERROR.

FIND est NO-LOCK 
    WHERE ROWID(est) EQ ipriEst
    NO-ERROR.
IF NOT AVAILABLE est THEN LEAVE.

FIND FIRST est-qty NO-LOCK 
    WHERE est-qty.company EQ est.company
    AND est-qty.est-no  EQ est.est-no
    NO-ERROR. 

FIND LAST ef NO-LOCK 
    WHERE ef.company EQ est-qty.company
    AND ef.est-no  EQ est-qty.est-no
    AND ef.eqty    EQ est-qty.eqty
    USE-INDEX est-qty
    NO-ERROR.

iForm = (IF AVAILABLE ef THEN ef.form-no ELSE 0) + 1.

CREATE ef.
ASSIGN
    ef.est-type  = est.est-type
    ef.company   = est.company
    ef.loc       = est.loc
    ef.e-num     = est.e-num
    ef.est-no    = est.est-no
    ef.eqty      = est-qty.eqty
    ef.form-no   = iForm
    ef.cust-seq  = 1
    ef.blank-qty = 1
    ef.lsh-wid   = ce-ctrl.ls-length
    ef.lsh-len   = ce-ctrl.ls-width
    ef.cost-uom  = "MSF".

IF ipcIndustry EQ 'C' THEN 
    RUN cec/newblank.p (ROWID(ef), OUTPUT opriEb).
ELSE 
    RUN ce/newblank.p (ROWID(ef), OUTPUT opriEb).
