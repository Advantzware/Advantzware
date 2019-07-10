
/*------------------------------------------------------------------------
    File        : PoLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for CustomerLookup

    Author(s)   : Jyoti Bajaj
    Created     : Oct 02 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttPoLook NO-UNDO 
        FIELD CustPo AS CHARACTER
        FIELD Number AS CHARACTER
        
          .
                                           
    
DEFINE DATASET dsPoLook FOR ttPoLook .
DEFINE VARIABLE q-PoLookQuery AS HANDLE.
DEFINE VARIABLE src-PoLook AS HANDLE.

DEFINE QUERY q-PoLookQuery FOR ttPoLook .

DEFINE DATA-SOURCE src-PoLook  FOR QUERY q-PoLookQuery.

BUFFER ttPoLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-PoLook  :HANDLE).



FUNCTION GetCurrentCust RETURNS CHARACTER (prmCust AS CHARACTER ):
    IF prmCust > "" THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.cust-no = prmCust NO-ERROR.
        RETURN IF AVAIL cust THEN cust.cust-no ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.
END FUNCTION.

