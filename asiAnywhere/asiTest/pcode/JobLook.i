
/*------------------------------------------------------------------------
    File        : JobLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for CustomerLookup

    Author(s)   : Jyoti Bajaj
    Created     : Oct 02 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttJobLook NO-UNDO 
        FIELD JOB AS CHARACTER
        FIELD JOB2 AS INTEGER
        FIELD ESTIMATE AS CHARACTER
        FIELD TICKET AS LOGICAL
        FIELD cust-no AS CHAR
        FIELD ord-no    AS CHAR
        FIELD item-no   AS CHAR
        FIELD itemname  AS CHAR
        FIELD stdcost   AS DECIMAL
        FIELD costuom   AS CHAR
        FIELD loc-no    AS CHAR
        FIELD loc-bin   AS CHAR
        FIELD qty-case  AS DECIMAL
        .
        
        /*        FIELD Run      AS INTEGER
        FIELD Estimate AS CHARACTER
        FIELD Ticket   AS Logical
                 .
                       */                    
    
DEFINE DATASET dsJobLook FOR ttJobLook .
DEFINE VARIABLE q-JobLookQuery AS HANDLE.
DEFINE VARIABLE src-JobLook AS HANDLE.

DEFINE QUERY q-JobLookQuery FOR ttJobLook .

DEFINE DATA-SOURCE src-JobLook  FOR QUERY q-JobLookQuery.

BUFFER ttJobLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-JobLook  :HANDLE).



FUNCTION GetCurrentCust RETURNS CHARACTER (prmCust AS CHARACTER ):
    IF prmCust > "" THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.cust-no = prmCust NO-ERROR.
        RETURN IF AVAIL cust THEN cust.cust-no ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.
END FUNCTION.

