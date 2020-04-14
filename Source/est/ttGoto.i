
/*------------------------------------------------------------------------
    File        : ttGoto.i
    Purpose     : All yeild quantity calculations are performed on the temp-table first and get assigned to eb and ef table after changes are completed

    Syntax      :

    Description : TEMP-TABLE definition for ttGoto

    Author(s)   : Mithun Porandla
    Created     : Wed Mar 25 06:36:15 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttGoto NO-UNDO 
    FIELD company          AS CHARACTER 
    FIELD estNo            AS CHARACTER
    FIELD eQty             AS DECIMAL
    FIELD formNo           AS INTEGER   FORMAT ">9":U            LABEL "Form"
    FIELD blankNo          AS INTEGER   FORMAT ">9":U            LABEL "Blank"
    FIELD formNoOrig       AS INTEGER   FORMAT ">9":U            LABEL "Original Form"
    FIELD blankNoOrig      AS INTEGER   FORMAT ">9":U            LABEL "Original Blank"
    FIELD partNo           AS CHARACTER FORMAT "X(20)":U         LABEL "Cust Part#"
    FIELD partDesc         AS CHARACTER FORMAT "X(50)":U         LABEL "Item Description"
    FIELD numWid           AS INTEGER   FORMAT "->>>,>>>,>>>":U  LABEL "#On Width"
    FIELD numLen           AS INTEGER   FORMAT "->>>,>>>,>>>":U  LABEL "#On Length"
    FIELD numUp            AS INTEGER   FORMAT "->>>,>>>,>>>":U  LABEL "#Up"
    FIELD reqQty           AS INTEGER   FORMAT "->>>,>>>,>>>":U  LABEL "Required Qty"
    FIELD yldQty           AS INTEGER   FORMAT "->>>,>>>,>>>":U  LABEL "Yield Qty"
    FIELD yieldRequest     AS LOGICAL   FORMAT "Yield/Request":U LABEL "Yield/Request" 
    FIELD reqQtyAdj        AS INTEGER   FORMAT "->>>,>>>,>>>":U  LABEL "Required Qty Adjustment" 
    FIELD sheetsRequired   AS DECIMAL   FORMAT "->>>,>>>,>>>":U  LABEL "Sheets Required"
    FIELD maxSheetsPerForm AS DECIMAL   FORMAT "->>>,>>>,>>>":U  LABEL "Max Sheets Per Form"
    FIELD calcYldQty       AS DECIMAL   FORMAT "->>>,>>>,>>>":U  LABEL "Calculated Yield Qty"
    FIELD effectiveYldQty  AS DECIMAL   FORMAT "->>>,>>>,>>>":U  LABEL "Effective Yield Qty"
    FIELD surplusQty       AS DECIMAL   FORMAT "->>>,>>>,>>>":U  LABEL "Surplus Qty"
    FIELD estType          AS INTEGER                            LABEL "Estimate Type"
    FIELD board            AS CHARACTER
    FIELD boardDesc        AS CHARACTER    
    FIELD ebRowid          AS ROWID
    INDEX estQty company estNo eQty formNo blankNo
    .


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
