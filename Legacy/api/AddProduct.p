/*------------------------------------------------------------------------
    File        : api/AddProduct.p
    Purpose     : Returns the request data for product addition

    Syntax      :

    Description : Returns the request data for product addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER oplcRequestData AS LONGCHAR  NO-UNDO.

ASSIGN
    oplcRequestData = REPLACE(oplcRequestData, "&21",  "1")
    oplcRequestData = REPLACE(oplcRequestData, "&20",  "2")
    oplcRequestData = REPLACE(oplcRequestData, "&19",  "1")
    oplcRequestData = REPLACE(oplcRequestData, "&18",  "Each")
    oplcRequestData = REPLACE(oplcRequestData, "&17",  "1")
    oplcRequestData = REPLACE(oplcRequestData, "&16",  "Each")
    oplcRequestData = REPLACE(oplcRequestData, "&15",  "Each")
    oplcRequestData = REPLACE(oplcRequestData, "&14",  "100")
    oplcRequestData = REPLACE(oplcRequestData, "&13",  "1")
    oplcRequestData = REPLACE(oplcRequestData, "&12",  "180")
    oplcRequestData = REPLACE(oplcRequestData, "&11",  "180")
    oplcRequestData = REPLACE(oplcRequestData, "&10",  "")
    oplcRequestData = REPLACE(oplcRequestData, "&9",  "")
    oplcRequestData = REPLACE(oplcRequestData, "&8",  "")
    oplcRequestData = REPLACE(oplcRequestData, "&7",  "A")
    oplcRequestData = REPLACE(oplcRequestData, "&6",  "CUST01")
    oplcRequestData = REPLACE(oplcRequestData, "&5",  "765987")
    oplcRequestData = REPLACE(oplcRequestData, "&4",  "1")
    oplcRequestData = REPLACE(oplcRequestData, "&3",  "Box T1")
    oplcRequestData = REPLACE(oplcRequestData, "&2",  "1006")
    oplcRequestData = REPLACE(oplcRequestData, "&1",  "ASI")
    .
