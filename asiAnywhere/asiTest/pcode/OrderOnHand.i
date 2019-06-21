
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
        FIELD ord-no AS Integer
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
        FIELD std-tot-cost  AS DECIMAL FORMAT "->>>,>>9.99<<" 
        FIELD std-mat-cost  AS DECIMAL FORMAT "->>>,>>9.99<<" 
        FIELD std-lab-cost AS DECIMAL FORMAT "->>>,>>9.99<<" 
        FIELD std-var-cost AS DECIMAL FORMAT "->>>,>>9.99<<" 
        FIELD std-fix-cost AS DECIMAL FORMAT "->>>,>>9.99<<" 
        FIELD last-cost AS Decimal
        FIELD sell-uom AS CHARACTER
        FIELD j-no AS CHARACTER
        FIELD q-onh AS DECIMAL FORMAT "->>,>>>,>>9.999"
        FIELD q-ono AS DECIMAL FORMAT "->>,>>>,>>9.999"
        FIELD q-alloc AS DECIMAL FORMAT "->>,>>>,>>9.999"
        FIELD q-back AS DECIMAL FORMAT "->>,>>>,>>9.999"
        FIELD q-avail AS DECIMAL FORMAT "->>,>>>,>>9.999"
        FIELD reorder AS DECIMAL FORMAT ">>>,>>>,>>9.999"
        FIELD reorderMin AS DECIMAL FORMAT ">>>,>>>,>>9.999"
        FIELD reorderMax AS DECIMAL FORMAT ">>>,>>>,>>9.99"
        FIELD rel-qty as int format "->>>,>>9"
        FIELD bol-qty as int format "->>>,>>9"
        FIELD avl-qty as int format "->>>,>>9"
   .
        
                                    
    
DEFINE DATASET dsOrderFg FOR ttOrderFg .

DEFINE QUERY q-OrderFgQuery FOR ttOrderFg.

DEFINE DATA-SOURCE src-OrderFg  FOR QUERY q-OrderFgQuery.

BUFFER ttOrderFg :ATTACH-DATA-SOURCE(DATA-SOURCE src-OrderFg  :HANDLE).
