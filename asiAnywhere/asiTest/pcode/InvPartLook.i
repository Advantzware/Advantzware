
/*------------------------------------------------------------------------
    File        : InvPartLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for CustomerLookup

    Author(s)   : Jyoti Bajaj
    Created     : Oct 02 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttInvPartLook NO-UNDO 
    FIELD InvPart AS CHARACTER
    FIELD InvName AS CHARACTER
    FIELD Invcust-no AS CHAR
    .
                                           
    
DEFINE DATASET dsInvPartLook FOR ttInvPartLook .
DEFINE VARIABLE q-OrdLookQuery AS HANDLE.
DEFINE VARIABLE src-OrdLook AS HANDLE.

DEFINE QUERY q-InvPartLookQuery FOR ttInvPartLook .

DEFINE DATA-SOURCE src-InvPartLook  FOR QUERY q-InvPartLookQuery.

BUFFER ttInvPartLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-InvPartLook  :HANDLE).



FUNCTION GetCurrentCust RETURNS CHARACTER (prmCust AS CHARACTER ):
    IF prmCust > "" THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.cust-no = prmCust NO-ERROR.
        RETURN IF AVAIL cust THEN cust.cust-no ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.
END FUNCTION.

