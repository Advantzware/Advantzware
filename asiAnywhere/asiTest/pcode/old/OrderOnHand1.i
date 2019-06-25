
/*------------------------------------------------------------------------
    File        : OrderOnHand.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order on Hand Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttOrderFg NO-UNDO 
BEFORE-TABLE beforeOrderFg
        FIELD ord-no AS character
        FIELD job-no AS character
        FIELD job-no2 AS CHARACTER
        FIELD i-no AS CHARACTER
        FIELD loc AS CHARACTER
        FIELD loc-bin AS CHARACTER 
        FIELD tag as character
        FIELD cust-no    AS CHARACTER
        FIELD cases AS CHARACTER
        FIELD case-count AS CHARACTER
        FIELD cases-unit AS CHARACTER
        FIELD partial-count  AS CHARACTER
        FIELD qty  AS Decimal
        FIELD std-tot-cost  AS Decimal
        FIELD std-mat-cost  AS Decimal
        FIELD std-lab-cost AS Decimal
        FIELD std-var-cost AS Decimal
        FIELD std-fix-cost AS Decimal
        FIELD last-cost AS Decimal
        FIELD sell-uom AS CHARACTER
        FIELD j-no AS CHARACTER
    .
        
                                    
    
DEFINE DATASET dsOrderFg FOR ttOrderFg .

DEFINE QUERY q-OrderFgQuery FOR ttOrderFg.

DEFINE DATA-SOURCE src-OrderFg  FOR QUERY q-OrderFgQuery.

BUFFER ttOrderFg :ATTACH-DATA-SOURCE(DATA-SOURCE src-OrderFg  :HANDLE).
