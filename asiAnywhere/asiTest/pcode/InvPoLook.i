
/*------------------------------------------------------------------------
    File        : InvPoLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for CustomerLookup

    Author(s)   : Jyoti Bajaj
    Created     : Oct 02 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttInvPoLook NO-UNDO 
        FIELD InvCustPo AS CHARACTER
        FIELD InvNumber AS CHARACTER
        
          .
                                           
    
DEFINE DATASET dsInvPoLook FOR ttInvPoLook .
DEFINE VARIABLE q-InvPoLookQuery AS HANDLE.
DEFINE VARIABLE src-InvPoLook AS HANDLE.

DEFINE QUERY q-InvPoLookQuery FOR ttInvPoLook .

DEFINE DATA-SOURCE src-InvPoLook  FOR QUERY q-InvPoLookQuery.

BUFFER ttInvPoLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-InvPoLook  :HANDLE).



FUNCTION GetCurrentCust RETURNS CHARACTER (prmCust AS CHARACTER ):
    IF prmCust > "" THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.cust-no = prmCust NO-ERROR.
        RETURN IF AVAIL cust THEN cust.cust-no ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.
END FUNCTION.

