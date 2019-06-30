

/*------------------------------------------------------------------------
    File        : CarrierLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for CustomerLookup

    Author(s)   : Jyoti Bajaj
    Created     : Oct 02 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCarrierLook NO-UNDO 
    FIELD carrier AS CHARACTER
    FIELD dscr AS CHARACTER
   
    .
                                           
    
DEFINE DATASET dsCarrierLook FOR ttCarrierLook .
DEFINE VARIABLE q-OrdLookQuery AS HANDLE.
DEFINE VARIABLE src-OrdLook AS HANDLE.

DEFINE QUERY q-CarrierLookQuery FOR ttCarrierLook .

DEFINE DATA-SOURCE src-CarrierLook  FOR QUERY q-CarrierLookQuery.

BUFFER ttCarrierLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-CarrierLook  :HANDLE).



FUNCTION GetCurrentCust RETURNS CHARACTER (prmCust AS CHARACTER ):
    IF prmCust > "" THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.cust-no = prmCust NO-ERROR.
        RETURN IF AVAIL cust THEN cust.cust-no ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.
END FUNCTION.


