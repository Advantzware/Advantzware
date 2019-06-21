
/*------------------------------------------------------------------------
    File        :Order.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order

    Author(s)   : Jyoti
    Created     : SEP 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttOrder1 NO-UNDO LIKE oe-ordl.

DEFINE DATASET dsOrder1 FOR ttOrder1.

DEFINE QUERY q-Order1Query FOR oe-ordl.

DEFINE DATA-SOURCE src-Order1 FOR QUERY q-Order1Query.

BUFFER ttOrder1:ATTACH-DATA-SOURCE(DATA-SOURCE src-Order1:HANDLE).

