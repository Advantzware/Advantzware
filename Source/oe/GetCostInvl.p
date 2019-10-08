
/*------------------------------------------------------------------------
    File        : GetCostInvl.p
    Purpose     : 

    Syntax      :

    Description : Gets the cost for an invoice line given a rowid for 
        either inv-line or ar-invl.  Returns 4 cost/M details, a total cost/M, 
        a total extended Cost, and Source indicator for debugging purposes

    Author(s)   : BV
    Created     : Thu Dec 06 13:51:43 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER ipriInvl                        AS   ROWID   NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostPerUOMDL                 AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostPerUOMFO                 AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostPerUOMVO                 AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostPerUOMDM                 AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostPerUOMTotal              AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcCostUOM                      AS   CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opdCostTotalExtended            AS   DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcCostSource                   AS   CHARACTER NO-UNDO.

DEFINE VARIABLE hdCostProcs AS HANDLE.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN system\CostProcs.p PERSISTENT SET hdCostProcs.

RUN GetCostForInvoiceLine IN hdCostProcs (ipriInvl, 
    OUTPUT opdCostPerUOMDL,
    OUTPUT opdCostPerUOMFO, 
    OUTPUT opdCostPerUOMVO,
    OUTPUT opdCostPerUOMDM,
    OUTPUT opdCostPerUOMTotal, 
    OUTPUT opcCostUOM, 
    OUTPUT opdCostTotalExtended,
    OUTPUT opcCostSource).
                        

