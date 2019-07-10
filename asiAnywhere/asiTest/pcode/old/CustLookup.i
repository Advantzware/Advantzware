
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
DEFINE TEMP-TABLE ttCustomer NO-UNDO 
BEFORE-TABLE beforeCustomer
        FIELD Customer AS character
        FIELD Name AS CHARACTER
        FIELD city AS CHARACTER
        FIELD state AS CHARACTER              
        FIELD zip AS CHARACTER 
        FIELD type as character
        FIELD sman    AS CHARACTER
        FIELD terr AS CHARACTER
        
        .
                                           
    
DEFINE DATASET dsCustomer FOR ttCustomer .
DEFINE VARIABLE q-CustomerQuery AS HANDLE.
DEFINE VARIABLE src-Customer AS HANDLE.

DEFINE QUERY q-CustomerQuery FOR ttCustomer .

DEFINE DATA-SOURCE src-Customer  FOR QUERY q-CustomerQuery.

BUFFER ttCustomer :ATTACH-DATA-SOURCE(DATA-SOURCE src-Customer  :HANDLE).



FUNCTION GetCurrentCust RETURNS CHARACTER (prmCust AS CHARACTER ):
    IF prmCust > "" THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.cust-no = prmCust NO-ERROR.
        RETURN IF AVAIL cust THEN cust.cust-no ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.
END FUNCTION.

