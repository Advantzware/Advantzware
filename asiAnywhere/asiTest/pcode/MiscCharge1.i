
/*------------------------------------------------------------------------
    File        MiscCharge1.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order MiscCharge1ory Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttMiscCharge1 NO-UNDO 
BEFORE-TABLE beforeMiscCharge1
        FIELD Weight AS Decimal
        FIELD Tax1 AS Decimal
        FIELD Freight AS Decimal
        FIELD BillFre AS Logical
        FIELD Total AS Decimal
        FIELD Cost AS Decimal
   .
        
                                    
    
DEFINE DATASET dsMiscCharge1 FOR ttMiscCharge1 .

DEFINE QUERY q-MiscCharge1Query FOR ttMiscCharge1.

DEFINE DATA-SOURCE src-MiscCharge1  FOR QUERY q-MiscCharge1Query.

BUFFER ttMiscCharge1 :ATTACH-DATA-SOURCE(DATA-SOURCE src-MiscCharge1  :HANDLE).
