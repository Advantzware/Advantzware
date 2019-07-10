

/*------------------------------------------------------------------------
    File        : BolNumLOok.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Invoice

    Author(s)   : Jyoti Bajaj
    Created     : feb 07 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttBolNumLook NO-UNDO 
    FIELD ArBol     AS INTEGER      
    FIELD ArBolItem AS CHARACTER
    FIELD vCust AS CHARACTER
    FIELD vDate AS DATE
    FIELD vBolFmt AS CHARACTER
    .

DEFINE DATASET dsBolNumLook FOR ttBolNumLook .
DEFINE VARIABLE q-BolNumLookQuery AS HANDLE.
DEFINE VARIABLE src-BolNumLook AS HANDLE.

DEFINE QUERY q-BolNumLookQuery FOR ttBolNumLook .

DEFINE DATA-SOURCE src-BolNumLook  FOR QUERY q-BolNumLookQuery.


BUFFER ttBolNumLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-BolNumLook  :HANDLE).


