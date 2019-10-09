
/*------------------------------------------------------------------------
    File        : GetMarkup.p
    Purpose     : 

    Syntax      :

    Description : Will return the markup/margin % and value 
    (net/gross/board) to apply this to ("N","B","G")
     Accepts a reference value to lookup the markup.
     Configures the return value based on CEMarkupMatrixInterpolate 
     NK1 logical value (yes = interpolate, no = step/discrete)

    Author(s)   : 
    Created     : Thu Nov 10 12:37:10 EST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCustID AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcProductCategory AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcStyleID AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdLookupValue AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdLookupValueReduction AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopdMarkup AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcMarkupOn AS CHARACTER NO-UNDO.
 
DEFINE BUFFER bf-cust-markup FOR cust-markup.
DEFINE VARIABLE dMarkupReduction AS DECIMAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fInterpolateOn RETURNS LOGICAL  
    ( ipcComp AS CHARACTER ) FORWARD.


/* ***************************  Main Block  *************************** */

RUN pGetMatrixMatch(ipcCompany,
    ipcCustID,
    ipcProductCategory,
    ipcStyleID,
    BUFFER bf-cust-markup).
  
IF AVAILABLE bf-cust-markup THEN DO: 
    RUN pGetMarkup(BUFFER bf-cust-markup, 
        ipdLookupValue,
        OUTPUT iopdMarkup,
        OUTPUT iopcMarkupOn).
    
    IF ipdLookupValueReduction GT 0 THEN 
        RUN pGetMarkupReduction(BUFFER bf-cust-markup,
            ipdLookupValueReduction,
            OUTPUT dMarkupReduction).
            
    iopdMarkup = iopdMarkup - dMarkupReduction.
    IF iopdMarkup LT 0 THEN iopdMarkup = 0.
END.
               

/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetMarkup:
    /*------------------------------------------------------------------------------
     Purpose:  Returns the effective markup based on input lookup
     Notes:  Refernece NK1 CEMarkupMatrixInterpolate logical value 
             for interpolate vs. step
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-selected-cust-markup FOR cust-markup.
    DEFINE INPUT PARAMETER ipdLookupValue AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdReturnMarkup AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcReturnMarkupOn AS CHARACTER NO-UNDO. 

    DEFINE VARIABLE iIndexL     AS INTEGER NO-UNDO. 
    DEFINE VARIABLE iIndexU     AS INTEGER NO-UNDO. 
    DEFINE VARIABLE dValueLower AS DECIMAL NO-UNDO. 
    DEFINE VARIABLE dValueUpper AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dValue      AS DECIMAL NO-UNDO.
 
    RUN custom\Lookup1D.p (ipdLookupValue, 
        ipbf-selected-cust-markup.run-qty, 
        ipbf-selected-cust-markup.markup,  
        OUTPUT dValueLower, 
        OUTPUT dValueUpper, 
        OUTPUT dValue,
        OUTPUT iIndexL,
        OUTPUT iIndexU).

    IF iIndexU GT 0 THEN 
        opcReturnMarkupOn = ipbf-selected-cust-markup.markup-on[iIndexU].     
    ELSE 
        opcReturnMarkupOn = ipbf-selected-cust-markup.markup-on[1].      
                                      
    IF fInterpolateOn(ipbf-selected-cust-markup.company) AND iIndexL NE 0 THEN 
        opdReturnMarkup = dValue.
    ELSE
       opdReturnMarkup = dValueUpper.

END PROCEDURE.

PROCEDURE pGetMarkupReduction:
 /*------------------------------------------------------------------------------
     Purpose:  Returns the effective markup reduction based on input lookup
     Notes:  Refernece NK1 CEMarkupMatrixInterpolate logical value 
             for interpolate vs. step
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-selected-cust-markup FOR cust-markup.
    DEFINE INPUT PARAMETER ipdLookupValueRed AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdReturnMarkupRed AS DECIMAL NO-UNDO.
     
    DEFINE VARIABLE iIndexL     AS INTEGER NO-UNDO. 
    DEFINE VARIABLE iIndexU     AS INTEGER NO-UNDO. 
    DEFINE VARIABLE dValueLower AS DECIMAL NO-UNDO. 
    DEFINE VARIABLE dValueUpper AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dValue      AS DECIMAL NO-UNDO.
 
    RUN custom\Lookup1D.p (ipdLookupValueRed,
        ipbf-selected-cust-markup.lookup_reduction,
        ipbf-selected-cust-markup.markup_reduction,
        OUTPUT dValueLower,
        OUTPUT dValueUpper,
        OUTPUT dValue,
        OUTPUT iIndexL,
        OUTPUT iIndexU).
           
    opdReturnMarkupRed = dValueUpper.
                
END PROCEDURE.

PROCEDURE pGetMatrixMatch:
    /*------------------------------------------------------------------------------
     Purpose: Finds the appropriate cust-markup matrix based on inputs
     Notes:  Hierarchy is
     1) match customer, prod cat, style
     2) match customer, prod cat, blank style
     3) match customer, style, blank prod cat
     4) match customer, blank style, blank prod cat
     5) match blank customer, prod cat, style
     6) match blank customer, prod cat, blank style
     7) match blank customer, style, blank prod cat
     8) match blank customer, blank style, blank prod cat
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcComp AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCust AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcProdCat AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcStyle AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-cust-markup FOR cust-markup.

    FOR EACH ipbf-cust-markup NO-LOCK
        WHERE ipbf-cust-markup.company EQ ipcComp
        AND (ipbf-cust-markup.cust-no EQ ipcCust OR
        ipbf-cust-markup.cust-no EQ "")
        AND (ipbf-cust-markup.style  EQ ipcStyle OR
        ipbf-cust-markup.style  EQ "")
        AND (ipbf-cust-markup.procat EQ ipcProdCat OR
        ipbf-cust-markup.procat EQ "")
        BY ipbf-cust-markup.cust-no DESCENDING
        BY ipbf-cust-markup.procat DESCENDING
        BY ipbf-cust-markup.style  DESCENDING:
           
        LEAVE.
    END.
  
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fInterpolateOn RETURNS LOGICAL 
    ( ipcComp AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the logical value of the CEMarkupMatrixInterpolate NK1
     Notes:
    ------------------------------------------------------------------------------*/	
    
    DEFINE VARIABLE lReturn AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO. 
    
    RUN sys/ref/nk1look.p (ipcComp,
        "CEMarkupMatrixInterpolate",
        "L",
        NO,
        NO,
        "",
        "",
        OUTPUT cReturn,
        OUTPUT lFound).

    IF lFound THEN 
        lReturn = (cReturn EQ "YES").

    RETURN lReturn.
		
END FUNCTION.


