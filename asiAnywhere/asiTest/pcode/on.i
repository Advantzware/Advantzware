
/*------------------------------------------------------------------------
    File        On.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order on Hand Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttOrderFg1 NO-UNDO
BEFORE-TABLE beforeOrderFg1 
        FIELD q-onh AS Decimal
        FIELD q-ono AS Decimal
        FIELD q-alloc AS Decimal
        FIELD q-back AS Decimal
        FIELD q-avail AS Decimal
        FIELD reorder AS Decimal
        FIELD reorderMin AS Decimal
        FIELD reorderMax AS Decimal
   .
        
                                    
    
DEFINE DATASET dsOrderFg1 FOR ttOrderFg1 .

DEFINE QUERY q-OrderFg1Query FOR ttOrderFg1.

DEFINE DATA-SOURCE src-OrderFg1  FOR QUERY q-OrderFg1Query.

BUFFER ttOrderFg1 :ATTACH-DATA-SOURCE(DATA-SOURCE src-OrderFg1  :HANDLE).
