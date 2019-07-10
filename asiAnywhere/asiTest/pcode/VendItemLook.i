


/*------------------------------------------------------------------------
    File        : VendItemLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for VendItemLookup

    Author(s)   : Jyoti Bajaj
    Created     : Jan 23 2008
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttVendItemLook NO-UNDO
    FIELD vend-i-no AS CHARACTER 
    FIELD i-no AS CHARACTER
    FIELD cust AS CHAR
          .

DEFINE DATASET dsVendItemLook FOR ttVendItemLook .
DEFINE VARIABLE q-VendItemLookQuery AS HANDLE.
DEFINE VARIABLE src-VendItemLook AS HANDLE.

DEFINE QUERY q-VendItemLookQuery FOR ttVendItemLook .

DEFINE DATA-SOURCE src-VendItemLook  FOR QUERY q-VendItemLookQuery.


BUFFER ttVendItemLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-VendItemLook  :HANDLE).


