

/*------------------------------------------------------------------------
    File        : InvNumLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Invoice

    Author(s)   : Jyoti Bajaj
    Created     : feb 07 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttInvoiceLook NO-UNDO 
    FIELD ArInv     AS INTEGER      
    FIELD ArInvName AS CHARACTER 
    FIELD ArInvItem AS CHARACTER
    FIELD ArInvcust-no AS CHAR
          .

DEFINE DATASET dsInvoiceLook FOR ttInvoiceLook .
DEFINE VARIABLE q-InvoiceLookQuery AS HANDLE.
DEFINE VARIABLE src-InvoiceLook AS HANDLE.

DEFINE QUERY q-InvoiceLookQuery FOR ttInvoiceLook .

DEFINE DATA-SOURCE src-InvoiceLook  FOR QUERY q-InvoiceLookQuery.


BUFFER ttInvoiceLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-InvoiceLook  :HANDLE).


