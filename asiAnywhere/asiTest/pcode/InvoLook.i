


/*------------------------------------------------------------------------
    File        : InvoLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Invoice

    Author(s)   : Jyoti Bajaj
    Created     : feb 07 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttInvoLook NO-UNDO 
    FIELD Invo     AS INTEGER      
    FIELD InvoName AS CHARACTER 
    FIELD InvoItem AS CHARACTER
    FIELD Invocust-no AS CHAR
          .

DEFINE DATASET dsInvoLook FOR ttInvoLook .
DEFINE VARIABLE q-InvoLookQuery AS HANDLE.
DEFINE VARIABLE src-InvoLook AS HANDLE.

DEFINE QUERY q-InvoLookQuery FOR ttInvoLook .

DEFINE DATA-SOURCE src-InvoLook  FOR QUERY q-InvoLookQuery.


BUFFER ttInvoLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-InvoLook  :HANDLE).



