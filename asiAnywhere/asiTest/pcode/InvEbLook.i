
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
DEFINE TEMP-TABLE ttInvEbLook NO-UNDO 
        FIELD InvEstimate AS CHARACTER
        FIELD InvCustomer AS CHARACTER
        FIELD InvType     AS INTEGER

          .
                                           
    
DEFINE DATASET dsInvEbLook FOR ttInvEbLook .
DEFINE VARIABLE q-InvEbLookQuery AS HANDLE.
DEFINE VARIABLE src-InvEbLook AS HANDLE.

DEFINE QUERY q-InvEbLookQuery FOR ttInvEbLook .

DEFINE DATA-SOURCE src-InvEbLook  FOR QUERY q-InvEbLookQuery.

BUFFER ttInvEbLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-InvEbLook  :HANDLE).



FUNCTION GetCurrentCust RETURNS CHARACTER (prmCust AS CHARACTER ):
    IF prmCust > "" THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.cust-no = prmCust NO-ERROR.
        RETURN IF AVAIL cust THEN cust.cust-no ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.
END FUNCTION.

