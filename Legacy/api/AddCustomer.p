/*------------------------------------------------------------------------
    File        : api/AddCustomer.p
    Purpose     : Returns the request data for customer addition

    Syntax      :

    Description : Returns the request data for customer addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER oplcRequestData AS LONGCHAR  NO-UNDO.

oplcRequestData = SUBSTITUTE (
                      oplcRequestData, 
                      "ASI",   /* company */
                      "C02291",
                      "Testing.com",
                      "1",
                      "Address"
                      ).