
/*------------------------------------------------------------------------
    File        : vieworder.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for View Order

    Author(s)   : Sewa Singh
    Created     : Sep 14 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttVorder NO-UNDO LIKE oe-ord
BEFORE-TABLE beforeViewOrder
FIELD frt-pay-dscr AS CHARACTER FORMAT "x(30)"
    FIELD Carrdscr AS CHARACTER FORMAT "x(30)"
FIELD fob-dscr AS CHARACTER FORMAT "x(30)"
.
 
DEFINE DATASET dsvieworder FOR ttVorder.

DEFINE QUERY q-vieworderQuery FOR oe-ord.

DEFINE DATA-SOURCE src-vieworder FOR QUERY q-vieworderQuery.

BUFFER ttVorder:ATTACH-DATA-SOURCE(DATA-SOURCE src-vieworder:HANDLE).

