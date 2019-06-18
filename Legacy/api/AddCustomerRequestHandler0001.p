/*------------------------------------------------------------------------
    File        : api/AddCustomerRequestHandler0001.p
    Purpose     : Returns the request data for customer addition

    Syntax      :

    Description : Returns the request data for customer addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData    AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage          AS CHARACTER NO-UNDO.
DEFINE OUTPUT       PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.

DEFINE	VARIABLE company   AS CHARACTER NO-UNDO INITIAL "ASI".
DEFINE	VARIABLE customer  AS CHARACTER NO-UNDO INITIAL "C02309".
DEFINE	VARIABLE name      AS CHARACTER NO-UNDO INITIAL "Testing.com".
DEFINE	VARIABLE groupCode AS CHARACTER NO-UNDO INITIAL "1".
DEFINE	VARIABLE address   AS CHARACTER NO-UNDO INITIAL "Address".

                   
ASSIGN  
    ioplcRequestData = REPLACE(ioplcRequestData, "cust.addr",  address)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD1",  groupCode)
    ioplcRequestData = REPLACE(ioplcRequestData, "cust.name",  name)
    ioplcRequestData = REPLACE(ioplcRequestData, "cust.cust-no",  customer)
    ioplcRequestData = REPLACE(ioplcRequestData, "cust.company",  company)
    .

ASSIGN
    opcMessage = ""
    oplSuccess = TRUE
    .                       
