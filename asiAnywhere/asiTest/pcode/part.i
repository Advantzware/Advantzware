
/*------------------------------------------------------------------------
    File        : part.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order

    Author(s)   : Jyoti
    Created     : SEP 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttpart NO-UNDO LIKE cust-part.

DEFINE DATASET dspart FOR ttpart.

DEFINE QUERY q-partQuery FOR cust-part.

DEFINE DATA-SOURCE src-part FOR QUERY q-partQuery.

BUFFER ttpart:ATTACH-DATA-SOURCE(DATA-SOURCE src-part:HANDLE).

