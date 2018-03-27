
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


DEFINE BUFFER bf-est FOR est.
DEFINE BUFFER bf-ef FOR ef.
DEFINE BUFFER bf-est-qty FOR est-qty.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND FIRST ce-ctrl NO-LOCK 
    {sys/look/ce-ctrlW.i}
    NO-ERROR.


FIND bf-est NO-LOCK 
    WHERE ROWID(bf-est) EQ ipriEst
    NO-ERROR.
IF NOT AVAILABLE bf-est THEN LEAVE.

FIND FIRST bf-est-qty NO-LOCK 
    WHERE bf-est-qty.company EQ bf-est.company
    AND bf-est-qty.est-no  EQ bf-est.est-no
    NO-ERROR. 

FIND LAST bf-ef NO-LOCK 
    WHERE bf-ef.company EQ bf-est-qty.company
    AND bf-ef.est-no  EQ bf-est-qty.est-no
    AND bf-ef.eqty    EQ bf-est-qty.eqty
    USE-INDEX est-qty
    NO-ERROR.

iForm = (IF AVAILABLE bf-ef THEN bf-ef.form-no ELSE 0) + 1.

CREATE ef.
ASSIGN
    ef.est-type  = bf-est.est-type
    ef.company   = bf-est.company
    ef.loc       = bf-est.loc
    ef.e-num     = bf-est.e-num
    ef.est-no    = bf-est.est-no
    ef.eqty      = bf-est-qty.eqty
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

