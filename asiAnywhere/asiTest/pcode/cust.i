

/*------------------------------------------------------------------------
    File        : cust.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Customer

    Author(s)   : Jyoti
    Created     : SEP 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttcust1 NO-UNDO LIKE cust.

DEFINE DATASET dscust1 FOR ttcust1.

DEFINE QUERY q-cust1Query FOR cust.

DEFINE DATA-SOURCE src-cust1 FOR QUERY q-cust1Query.

BUFFER ttcust1:ATTACH-DATA-SOURCE(DATA-SOURCE src-cust1:HANDLE).

