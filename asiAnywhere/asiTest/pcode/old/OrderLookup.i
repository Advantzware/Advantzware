
/*------------------------------------------------------------------------
    File        :OrderLookup.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order

    Author(s)   : Jyoti
    Created     : SEP 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttOrder2 NO-UNDO LIKE oe-ord 
BEFORE-TABLE beforeOrder2
field Ord-Num as character.
                         .

DEFINE DATASET dsOrder2 FOR ttOrder2.

DEFINE QUERY q-Order2Query FOR oe-ord.

DEFINE DATA-SOURCE src-Order2 FOR QUERY q-Order2Query.

BUFFER ttOrder2:ATTACH-DATA-SOURCE(DATA-SOURCE src-Order2:HANDLE).

