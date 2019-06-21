



/*------------------------------------------------------------------------
    File        : VendorLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for VendorLookup

    Author(s)   : Jyoti Bajaj
    Created     : Jan 23 2008
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttVendorLook NO-UNDO
    FIELD vend-no AS CHARACTER 
    FIELD Vname AS CHARACTER
    
          .

DEFINE DATASET dsVendorLook FOR ttVendorLook .
DEFINE VARIABLE q-VendorLookQuery AS HANDLE.
DEFINE VARIABLE src-VendorLook AS HANDLE.

DEFINE QUERY q-VendorLookQuery FOR ttVendorLook .

DEFINE DATA-SOURCE src-VendorLook  FOR QUERY q-VendorLookQuery.


BUFFER ttVendorLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-VendorLook  :HANDLE).


