
/*------------------------------------------------------------------------
    File        MiscCharge.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order MiscChargeory Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttMiscCharge NO-UNDO 
BEFORE-TABLE beforeMiscCharge
        FIELD Charge AS CHAR FORMAT "x(20)"
        FIELD Sell AS DECIMAL FORMAT "->>>,>>9.99"
        FIELD Account  AS CHAR FORMAT "x(25)"
        FIELD Dscr AS CHAR FORMAT "x(30)"
        FIELD Po AS CHAR FORMAT "x(15)"
        FIELD Job AS CHAR FORMAT "x(6)"
        FIELD Job2 AS INTEGER FORMAT "99"
        FIELD VenPo AS Integer
        FIELD Tax AS CHAR
        FIELD Bill AS char
        FIELD Weight AS Decimal
        FIELD Tax1 AS Decimal
        FIELD Freight AS Decimal
        FIELD BillFre AS Logical
        FIELD Total AS Decimal
        FIELD Cost AS Decimal
        FIELD Comm AS DEC
    FIELD vItem LIKE oe-ordl.i-no.
   .
        
                                    
    
DEFINE DATASET dsMiscCharge FOR ttMiscCharge .

DEFINE QUERY q-MiscChargeQuery FOR ttMiscCharge.

DEFINE DATA-SOURCE src-MiscCharge  FOR QUERY q-MiscChargeQuery.

BUFFER ttMiscCharge :ATTACH-DATA-SOURCE(DATA-SOURCE src-MiscCharge  :HANDLE).
