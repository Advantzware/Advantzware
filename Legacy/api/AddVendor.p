/*------------------------------------------------------------------------
    File        : api/AddVendor.p
    Purpose     : Returns the request data for vendor addition

    Syntax      :

    Description : Returns the request data for vendor addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER oplcRequestData AS LONGCHAR  NO-UNDO.

oplcRequestData = SUBSTITUTE (
                      oplcRequestData, 
                      "ASI",   /* company */
                      "S008",
                      "BubbleFillerandCo",
                      "1"
                      ).