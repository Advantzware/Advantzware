
/*------------------------------------------------------------------------
    File        : OrderLookup.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for OrdLookup

    Author(s)   : Jyoti Bajaj
    Created     : Oct 02 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttOrd2 NO-UNDO like oe-ord
BEFORE-TABLE beforeOrd2
field Custe as character.
                                           
    
DEFINE DATASET dsOrd2 FOR ttOrd2 .
DEFINE VARIABLE q-Ord2Query AS HANDLE.
DEFINE VARIABLE src-Ord2 AS HANDLE.

DEFINE QUERY q-Ord2Query FOR ttOrd2 .

DEFINE DATA-SOURCE src-Ord2  FOR QUERY q-Ord2Query.

BUFFER ttOrd2 :ATTACH-DATA-SOURCE(DATA-SOURCE src-Ord2  :HANDLE).



FUNCTION GetCurrentCust RETURNS CHARACTER (prmCust AS CHARACTER ):
    IF prmCust > "" THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.cust-no = prmCust NO-ERROR.
        RETURN IF AVAIL cust THEN cust.cust-no ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.
END FUNCTION.

