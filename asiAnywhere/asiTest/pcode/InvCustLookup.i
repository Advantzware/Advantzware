
/*------------------------------------------------------------------------
    File        : CustLookup.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for CustomerLookup

    Author(s)   : Jyoti Bajaj
    Created     : Oct 02 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttInvCustomer NO-UNDO 
        FIELD InvCustomer AS character
        FIELD InvName AS CHARACTER
        FIELD Invcity AS CHARACTER
        FIELD Invstate AS CHARACTER              
        FIELD Invzip AS CHARACTER 
        FIELD Invtype as character
        FIELD Invsman    AS CHARACTER
        FIELD Invterr AS CHARACTER
        
        .
                                           
    
DEFINE DATASET dsInvCustomer FOR ttInvCustomer .
DEFINE VARIABLE q-InvCustomerQuery AS HANDLE.
DEFINE VARIABLE src-InvCustomer AS HANDLE.

DEFINE QUERY q-InvCustomerQuery FOR ttInvCustomer .

DEFINE DATA-SOURCE src-InvCustomer  FOR QUERY q-InvCustomerQuery.

BUFFER ttInvCustomer :ATTACH-DATA-SOURCE(DATA-SOURCE src-InvCustomer  :HANDLE).



