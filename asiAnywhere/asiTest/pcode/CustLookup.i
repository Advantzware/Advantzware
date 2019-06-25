


/*------------------------------------------------------------------------
    File        : CustLookup.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for CustomerLookup

    Author(s)   : 
    Created     : 
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCustomer NO-UNDO 
        FIELD Customer AS character
        FIELD Name AS CHARACTER
        FIELD Address1 AS CHARACTER
        FIELD Address2 AS CHARACTER
        FIELD city AS CHARACTER
        FIELD state AS CHARACTER              
        FIELD zip AS CHARACTER 
        FIELD type as character
        FIELD sman    AS CHARACTER
        FIELD sname    AS CHARACTER
        FIELD country    AS CHARACTER
        FIELD county    AS CHARACTER
        FIELD terr AS CHARACTER
        FIELD frtpay AS CHAR FORMAT "x(1)"
        FIELD sales AS DECIMAL FORMAT  "->>>,>>>,>>9.99"
        FIELD comm AS DECIMAL FORMAT  "->>,>>>,>>9.99"
        FIELD fob-code      AS CHAR FORMAT  "x(5)"
        FIELD carrier       AS CHAR
        FIELD contact       AS CHAR
        FIELD over          AS DECIMAL
        FIELD under         AS DECIMAL
        FIELD terms         AS CHAR
        FIELD Tdscr         AS CHAR
        FIELD prevOrder     AS INT
        FIELD Taxcode       AS CHAR
        FIELD ExpDate       LIKE cust.date-field
        FIELD DueDate       AS DATE
        FIELD LastShip      AS DATE
        FIELD dueCode       AS CHAR
        FIELD soldname      AS CHAR
        FIELD soldadd1       AS CHAR
        FIELD soldadd2      AS CHAR
        FIELD soldcity       AS CHAR
        FIELD soldstate      AS CHAR
        FIELD soldzip       AS CHAR

        .
                                           
    
DEFINE DATASET dsCustomer FOR ttCustomer .
DEFINE VARIABLE q-CustomerQuery AS HANDLE.
DEFINE VARIABLE src-Customer AS HANDLE.

DEFINE QUERY q-CustomerQuery FOR ttCustomer .

DEFINE DATA-SOURCE src-Customer  FOR QUERY q-CustomerQuery.

BUFFER ttCustomer :ATTACH-DATA-SOURCE(DATA-SOURCE src-Customer  :HANDLE).



