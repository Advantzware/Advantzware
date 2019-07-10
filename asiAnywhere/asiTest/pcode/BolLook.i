


/*------------------------------------------------------------------------
    File        : BolLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Invoice

    Author(s)   : Jyoti Bajaj
    Created     : feb 07 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttBolLook NO-UNDO 
    FIELD cust-no AS CHAR
    FIELD BolNum     AS INTEGER      
    FIELD BolItem AS CHARACTER
    .

DEFINE DATASET dsBolLook FOR ttBolLook .
DEFINE VARIABLE q-BolLookQuery AS HANDLE.
DEFINE VARIABLE src-BolLook AS HANDLE.

DEFINE QUERY q-BolLookQuery FOR ttBolLook .

DEFINE DATA-SOURCE src-BolLook  FOR QUERY q-BolLookQuery.


BUFFER ttBolLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-BolLook  :HANDLE).



