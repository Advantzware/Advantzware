


/*------------------------------------------------------------------------
    File        : ShipLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for CustomerLookup

    Author(s)   : Jyoti Bajaj
    Created     : Oct 02 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttShipLook NO-UNDO 
    FIELD cust-no AS CHARACTER
    FIELD ship-id AS CHARACTER
    FIELD SName AS CHARACTER
    FIELD Addr AS CHARACTER
    FIELD City AS CHARACTER
    FIELD State AS CHARACTER
    FIELD Zip AS CHARACTER
    FIELD carrier AS CHARACTER
    FIELD Loc AS CHARACTER .
                                           
    
DEFINE DATASET dsShipLook FOR ttShipLook .
DEFINE VARIABLE q-OrdLookQuery AS HANDLE.
DEFINE VARIABLE src-OrdLook AS HANDLE.

DEFINE QUERY q-ShipLookQuery FOR ttShipLook .

DEFINE DATA-SOURCE src-ShipLook  FOR QUERY q-ShipLookQuery.

BUFFER ttShipLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-ShipLook  :HANDLE).



FUNCTION GetCurrentCust RETURNS CHARACTER (prmCust AS CHARACTER ):
    IF prmCust > "" THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.cust-no = prmCust NO-ERROR.
        RETURN IF AVAIL cust THEN cust.cust-no ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.
END FUNCTION.


