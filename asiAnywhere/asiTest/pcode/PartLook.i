
/*------------------------------------------------------------------------
    File        : PartLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for CustomerLookup

    Author(s)   : Sewa
    Created     : Oct 02 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttPartLook NO-UNDO 
     FIELD ino AS CHARACTER
    FIELD iname AS CHARACTER
    FIELD qtyhand AS DECIMAL
    FIELD cust-no AS CHAR
    FIELD Part AS CHARACTER
    FIELD part2 AS CHAR
    FIELD est AS CHARACTER
    FIELD mat AS LOGICAL
    FIELD partdscr1 AS CHARACTER
    FIELD partdscr2 AS CHARACTER
     FIELD price AS DECIMAL
    FIELD uom AS CHARACTER
    FIELD casecount AS INTEGER  
    FIELD casepall  AS INTEGER
    FIELD tax AS CHARACTER
    
    .
                                           
    
DEFINE DATASET dsPartLook FOR ttPartLook .
DEFINE VARIABLE q-OrdLookQuery AS HANDLE.
DEFINE VARIABLE src-OrdLook AS HANDLE.

DEFINE QUERY q-PartLookQuery FOR ttPartLook .

DEFINE DATA-SOURCE src-PartLook  FOR QUERY q-PartLookQuery.

BUFFER ttPartLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-PartLook  :HANDLE).



FUNCTION GetCurrentCust RETURNS CHARACTER (prmCust AS CHARACTER ):
    IF prmCust > "" THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.cust-no = prmCust NO-ERROR.
        RETURN IF AVAIL cust THEN cust.cust-no ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.
END FUNCTION.

