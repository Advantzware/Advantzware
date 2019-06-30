
/*------------------------------------------------------------------------
    File        : EbLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for CustomerLookup

    Author(s)   : Jyoti Bajaj
    Created     : Oct 02 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttEbLook NO-UNDO 
        FIELD Estimate AS CHARACTER
        FIELD Customer AS CHARACTER
        FIELD Type     AS INTEGER

          .
                                           
    
DEFINE DATASET dsEbLook FOR ttEbLook .
DEFINE VARIABLE q-EbLookQuery AS HANDLE.
DEFINE VARIABLE src-EbLook AS HANDLE.

DEFINE QUERY q-EbLookQuery FOR ttEbLook .

DEFINE DATA-SOURCE src-EbLook  FOR QUERY q-EbLookQuery.

BUFFER ttEbLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-EbLook  :HANDLE).



FUNCTION GetCurrentCust RETURNS CHARACTER (prmCust AS CHARACTER ):
    IF prmCust > "" THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.cust-no = prmCust NO-ERROR.
        RETURN IF AVAIL cust THEN cust.cust-no ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.
END FUNCTION.

