
/*------------------------------------------------------------------------
    File        : CustLookup.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for CustomerLookup

    Author(s)   : Jyoti Bajaj
    Created     : Oct 02 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttOrdLook NO-UNDO 
        FIELD Order AS INTEGER
        FIELD Estimate AS CHARACTER
        FIELD CustName AS CHARACTER
    FIELD cust-no AS CHAR
          .
                                           
    
DEFINE DATASET dsOrdLook FOR ttOrdLook .
DEFINE VARIABLE q-OrdLookQuery AS HANDLE.
DEFINE VARIABLE src-OrdLook AS HANDLE.

DEFINE QUERY q-OrdLookQuery FOR ttOrdLook .

DEFINE DATA-SOURCE src-OrdLook  FOR QUERY q-OrdLookQuery.

BUFFER ttOrdLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-OrdLook  :HANDLE).



FUNCTION GetCurrentCust RETURNS CHARACTER (prmCust AS CHARACTER ):
    IF prmCust > "" THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.cust-no = prmCust NO-ERROR.
        RETURN IF AVAIL cust THEN cust.cust-no ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.
END FUNCTION.

