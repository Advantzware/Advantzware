
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

PROCEDURE pIsValidCustomerType:
    /*------------------------------------------------------------------------------
     Purpose:  Validates customer type
     Notes: Input 1 = customer type
    ------------------------------------------------------------------------------*/
    {util\Validate.i 
        &ValidateTable = "custype" 
        &ValidateField = "custype" 
        &ValidateMessage = "Customer Type"}

END PROCEDURE. 

PROCEDURE pIsValidCarrier:
    /*------------------------------------------------------------------------------
     Purpose:  Validates carrier
     Notes: Input 1 = carrier
    ------------------------------------------------------------------------------*/
    {util\Validate.i 
        &ValidateTable = "carrier" 
        &ValidateField = "carrier" 
        &ValidateMessage = "Carrier"}

END PROCEDURE. 

PROCEDURE pIsValidDeliveryZone:
    /*------------------------------------------------------------------------------
     Purpose:  Validates shipto ID
     Notes: Input 1 = customer id, input 2 = ship to id
    ------------------------------------------------------------------------------*/
    {util\ValidateWith2Criteria.i 
        &ValidateTable = "carr-mtx" 
        &ValidateField1 = "carrier"
        &ValidateField2 = "del-zone"  
        &ValidateMessage = "Delivery Zone"}
    

END PROCEDURE.

PROCEDURE pIsValidItemForType:
    /*------------------------------------------------------------------------------
     Purpose:  Validates shipto ID
     Notes: Input 1 = customer id, input 2 = ship to id
    ------------------------------------------------------------------------------*/
    {util\ValidateWith2Criteria.i 
        &ValidateTable = "item" 
        &ValidateField1 = "i-no"
        &ValidateField2 = "mat-type"  
        &ValidateMessage = "Item for Specific Type"}
    

END PROCEDURE.

PROCEDURE pIsValidGLAccount:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Terms 
     Notes: Input 1 = Terms Code
    ------------------------------------------------------------------------------*/
    {util\Validate.i 
        &ValidateTable = "account" 
        &ValidateField = "actnum" 
        &ValidateMessage = "GL Account"}
    
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
        &ValidateMessage = "UOM"}

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

PROCEDURE pIsValidState:
    /*------------------------------------------------------------------------------
     Purpose:  Validates State 
     Notes: Input 1 = state
    ------------------------------------------------------------------------------*/
    {util\ValidateWithNoCompany.i 
        &ValidateTable = "state" 
        &ValidateField = "state" 
        &ValidateMessage = "State"}
   

END PROCEDURE.

PROCEDURE pIsValidTaxGroup:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Tax Group 
     Notes: Input 1 = Tax Group
    ------------------------------------------------------------------------------*/
    {util\Validate.i 
        &ValidateTable = "stax" 
        &ValidateField = "tax-group" 
        &ValidateMessage = "Tax Group"}
    
END PROCEDURE.

PROCEDURE pIsValidTerms:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Terms 
     Notes: Input 1 = Terms Code
    ------------------------------------------------------------------------------*/
    {util\Validate.i 
        &ValidateTable = "terms" 
        &ValidateField = "t-code" 
        &ValidateMessage = "Terms"}
    
END PROCEDURE.

PROCEDURE pIsValidVendor:
    /*------------------------------------------------------------------------------
     Purpose:  Validates vendor
     Notes: Input 1 = vendor
    ------------------------------------------------------------------------------*/
    {util\Validate.i 
        &ValidateTable = "vend" 
        &ValidateField = "vend-no" 
        &ValidateMessage = "Vendor"}

END PROCEDURE. 

PROCEDURE pIsValidVendorType:
    /*------------------------------------------------------------------------------
     Purpose:  Validates vendor type
     Notes: Input 1 = vendor type
    ------------------------------------------------------------------------------*/
    {util\Validate.i 
        &ValidateTable = "ventype" 
        &ValidateField = "type" 
        &ValidateMessage = "Vendor Type"}

END PROCEDURE. 

PROCEDURE pIsValidFromList:
    /*------------------------------------------------------------------------------
     Purpose:  Validates a value based to confirm it is in a comma separated list
     Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcField AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcInputValue AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcValidList AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    oplIsValid = LOOKUP(ipcInputValue,ipcValidList) GT 0.
    IF NOT oplIsValid THEN 
        opcMessage = CHR(34) + ipcInputValue + CHR(34) + " for " + CHR(34) + ipcField + CHR(34) + " is not in one of " + CHR(34) + ipcValidList + CHR(34) .

END PROCEDURE.