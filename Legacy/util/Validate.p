
/*------------------------------------------------------------------------
    File        : Validate.p
    Purpose     : Centralize validations for input fields

    Syntax      :

    Description : Houses various validation calls for single value validation.  Returns 	logical value and character message for validation.	

    Author(s)   : BV
    Created     : Tue Mar 13 18:21:25 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pIsValidCustomerID:
    /*------------------------------------------------------------------------------
     Purpose:  Validates customer number
     Notes: Input 1 = customer ID
    ------------------------------------------------------------------------------*/
    {util\Validate.i 
        &ValidateTable = "cust" 
        &ValidateField = "cust-no" 
        &ValidateMessage = "Customer ID"}

END PROCEDURE.

PROCEDURE pIsValidShiptoID:
    /*------------------------------------------------------------------------------
     Purpose:  Validates shipto ID
     Notes: Input 1 = customer id, input 2 = ship to id
    ------------------------------------------------------------------------------*/
    {util\ValidateWith2Criteria.i 
        &ValidateTable = "shipto" 
        &ValidateField1 = "cust-no"
        &ValidateField2 = "ship-id"  
        &ValidateMessage = "ShipTo ID"}
    

END PROCEDURE.

PROCEDURE pIsValidFGITemID:
    /*------------------------------------------------------------------------------
     Purpose:  Validates FG Item ID 
     Notes: Input 1 = FG Item Number
    ------------------------------------------------------------------------------*/
    {util\Validate.i 
        &ValidateTable = "itemfg" 
        &ValidateField = "i-no" 
        &ValidateMessage = "FG Item ID"}

END PROCEDURE.
PROCEDURE pIsValidStyle:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Style 
     Notes: Input 1 = Style
    ------------------------------------------------------------------------------*/
    {util\Validate.i 
        &ValidateTable = "style" 
        &ValidateField = "style" 
        &ValidateMessage = "Style"}
        

END PROCEDURE.
PROCEDURE pIsValidUOM:
    /*------------------------------------------------------------------------------
     Purpose:  Validates UOM 
     Notes: Input 1 = UOM
    ------------------------------------------------------------------------------*/
    {util\ValidateWithNoCompany.i 
        &ValidateTable = "uom" 
        &ValidateField = "uom" 
        &ValidateMessage = "Style"}
        uom.

END PROCEDURE.
PROCEDURE pIsValidCurrency:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Currency 
     Notes: Input 1 = Currency Code
    ------------------------------------------------------------------------------*/
    {util\Validate.i 
        &ValidateTable = "currency" 
        &ValidateField = "c-code" 
        &ValidateMessage = "Currency"}
        

END PROCEDURE.
PROCEDURE pIsValidFGCategory:
    /*------------------------------------------------------------------------------
     Purpose:  Validates FG Category 
     Notes: Input 1 = FG Category
    ------------------------------------------------------------------------------*/
    {util\Validate.i 
        &ValidateTable = "fgcat" 
        &ValidateField = "procat" 
        &ValidateMessage = "FG Product Category"}
        

END PROCEDURE.
PROCEDURE pIsValidWarehouse:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Warehouse/Location 
     Notes: Input 1 = Warehouse/Location
    ------------------------------------------------------------------------------*/
    {util\Validate.i 
        &ValidateTable = "loc" 
        &ValidateField = "loc" 
        &ValidateMessage = "Warehouse/Location"}
     

END PROCEDURE.

PROCEDURE pIsValidFGBin:
    /*------------------------------------------------------------------------------
     Purpose:  Validates FG Bin
     Notes: Input 1 = Bin location
    ------------------------------------------------------------------------------*/
        {util\ValidateWith2Criteria.i 
        &ValidateTable = "fg-bin" 
        &ValidateField1 = "loc-bin"
        &ValidateField2 = "i-no"  
        &ValidateMessage = "ShipTo ID"}
END PROCEDURE.
PROCEDURE pIsValidSalesRep:
    /*------------------------------------------------------------------------------
     Purpose:  Validates SalesRep 
     Notes: Input 1 = Sales Rep Code
    ------------------------------------------------------------------------------*/
    {util\Validate.i 
        &ValidateTable = "sman" 
        &ValidateField = "sman" 
        &ValidateMessage = "Sales Rep"}
    

END PROCEDURE.

