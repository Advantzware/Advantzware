/*------------------------------------------------------------------------
    File        : api/AddVendorRequestHandler0001.p
    Purpose     : Returns the request data for vendor addition

    Syntax      :

    Description : Returns the request data for vendor addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage       AS CHARACTER NO-UNDO.
DEFINE OUTPUT       PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.

DEFINE	VARIABLE  company       AS  CHARACTER	NO-UNDO  INITIAL   "ASI".
DEFINE	VARIABLE  customer      AS  CHARACTER	NO-UNDO  INITIAL   "S019".
DEFINE	VARIABLE  supplierName  AS  CHARACTER	NO-UNDO  INITIAL   "BubbleFillerandCo".
DEFINE	VARIABLE  groupCode     AS  CHARACTER	NO-UNDO  INITIAL   "1".

                   
ASSIGN  
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD3",  groupCode)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD2",  supplierName)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD1",  customer)
    ioplcRequestData = REPLACE(ioplcRequestData, "vend.company",  company)
    .
    
ASSIGN
    opcMessage = ""
    oplSuccess = TRUE
    .                       
