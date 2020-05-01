
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
    {util/dev/Validate.i 
        &ValidateTable = "cust" 
        &ValidateField = "cust-no" 
        &ValidateMessage = "Customer ID"}

END PROCEDURE.

PROCEDURE pIsValidCustomerType:
    /*------------------------------------------------------------------------------
     Purpose:  Validates customer type
     Notes: Input 1 = customer type
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i 
        &ValidateTable = "custype" 
        &ValidateField = "custype" 
        &ValidateMessage = "Customer Type"}

END PROCEDURE. 

PROCEDURE pIsValidCarrier:
    /*------------------------------------------------------------------------------
     Purpose:  Validates carrier
     Notes: Input 1 = carrier
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i 
        &ValidateTable = "carrier" 
        &ValidateField = "carrier" 
        &ValidateMessage = "Carrier"}

END PROCEDURE. 

PROCEDURE pIsValidTerr:
    /*------------------------------------------------------------------------------
     Purpose:  Validates carrier
     Notes: Input 1 = carrier
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i 
        &ValidateTable = "terr" 
        &ValidateField = "terr" 
        &ValidateMessage = "terr"}

END PROCEDURE. 

PROCEDURE pIsValidDeliveryZone:
    /*------------------------------------------------------------------------------
     Purpose:  Validates shipto ID
     Notes: Input 1 = customer id, input 2 = ship to id
    ------------------------------------------------------------------------------*/
    {util/dev/ValidateWith2Criteria.i 
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
    {util/dev/ValidateWith2Criteria.i 
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
    {util/dev/validate.i 
        &ValidateTable = "account" 
        &ValidateField = "actnum" 
        &ValidateMessage = "GL Account"}
    
END PROCEDURE.

PROCEDURE pIsValidShiptoID:
    /*------------------------------------------------------------------------------
     Purpose:  Validates shipto ID
     Notes: Input 1 = customer id, input 2 = ship to id
    ------------------------------------------------------------------------------*/
    {util/dev/ValidateWith2Criteria.i 
        &ValidateTable = "shipto" 
        &ValidateField1 = "cust-no"
        &ValidateField2 = "ship-id"  
        &ValidateMessage = "ShipTo ID"}
    

END PROCEDURE.

PROCEDURE pIsValidCustPartID:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Cust Part ID 
     Notes: Input 1 = Cust Part Number
    ------------------------------------------------------------------------------*/
    {util/dev/ValidateCustPart.i 
        &ValidateTable = "itemfg" 
        &ValidateField1 = "part-no" 
        &ValidateField2 = "cust-no"
        &ValidateMessage = "Cust Part ID"}

END PROCEDURE.

PROCEDURE pIsValidCustPartIDNonFG:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Cust Part ID 
     Notes: Input 1 = Cust Part Number
    ------------------------------------------------------------------------------*/
    {util/dev/ValidateWith2Criteria.i  
        &ValidateTable = "cust-part" 
        &ValidateField1 = "part-no" 
        &ValidateField2 = "cust-no"
        &ValidateMessage = "Cust Part ID"}

END PROCEDURE.

PROCEDURE pIsValidCustPartFromEst:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Cust Part ID 
     Notes: Input 1 = Estimate Number
    ------------------------------------------------------------------------------*/
    {util/dev/ValidateWith2Criteria.i  
        &ValidateTable = "eb" 
        &ValidateField1 = "est-no" 
        &ValidateField2 = "part-no"
        &ValidateMessage = "Cust Part ID on estimate"}

END PROCEDURE.

PROCEDURE pIsValidEstID:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Estimate No
     Notes: Input 1 = Estimate Number
    ------------------------------------------------------------------------------*/
    {util/dev/VALIDATEEst.i 
        &ValidateTable = "eb" 
        &ValidateField = "est-no" 
        &ValidateMessage = "Estimate Number"}

END PROCEDURE.

PROCEDURE pIsValidFGITemID:
    /*------------------------------------------------------------------------------
     Purpose:  Validates FG Item ID 
     Notes: Input 1 = FG Item Number
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i 
        &ValidateTable = "itemfg" 
        &ValidateField = "i-no" 
        &ValidateMessage = "FG Item ID"}

END PROCEDURE.
PROCEDURE pIsValidStyle:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Style 
     Notes: Input 1 = Style
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i 
        &ValidateTable = "style" 
        &ValidateField = "style" 
        &ValidateMessage = "Style"}
        

END PROCEDURE.
PROCEDURE pIsValidUOM:
    /*------------------------------------------------------------------------------
     Purpose:  Validates UOM 
     Notes: Input 1 = UOM
    ------------------------------------------------------------------------------*/
    {util/dev/ValidateWithNoCompany.i 
        &ValidateTable = "uom" 
        &ValidateField = "uom" 
        &ValidateMessage = "UOM"}

END PROCEDURE.
PROCEDURE pIsValidCurrency:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Currency 
     Notes: Input 1 = Currency Code
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i 
        &ValidateTable = "currency" 
        &ValidateField = "c-code" 
        &ValidateMessage = "Currency"}
        

END PROCEDURE.
PROCEDURE pIsValidFGCategory:
    /*------------------------------------------------------------------------------
     Purpose:  Validates FG Category 
     Notes: Input 1 = FG Category
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i 
        &ValidateTable = "fgcat" 
        &ValidateField = "procat" 
        &ValidateMessage = "FG Product Category"}
        

END PROCEDURE.
PROCEDURE pIsValidWarehouse:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Warehouse/Location 
     Notes: Input 1 = Warehouse/Location
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i 
        &ValidateTable = "loc" 
        &ValidateField = "loc" 
        &ValidateMessage = "Warehouse/Location"}
     

END PROCEDURE.

PROCEDURE pIsValidFGBin:
    /*------------------------------------------------------------------------------
     Purpose:  Validates FG Bin
     Notes: Input 1 = Bin location
    ------------------------------------------------------------------------------*/
        {util/dev/ValidateWith2Criteria.i 
        &ValidateTable = "fg-bin" 
        &ValidateField1 = "loc-bin"
        &ValidateField2 = "i-no"  
        &ValidateMessage = "FG Bin Loc"}
END PROCEDURE.

PROCEDURE pIsValidFGBinForLoc:
    /*------------------------------------------------------------------------------
     Purpose:  Validates FG Bin
     Notes: Input 1 = Bin location
    ------------------------------------------------------------------------------*/
        {util/dev/ValidateWith2Criteria.i 
        &ValidateTable = "fg-bin" 
        &ValidateField1 = "loc-bin"
        &ValidateField2 = "loc"  
        &ValidateMessage = "FG Bin Location for warehouse"}
END PROCEDURE.

PROCEDURE pIsValidRMBinForLoc:
    /*------------------------------------------------------------------------------
     Purpose:  Validates RM Bin
     Notes: Input 1 = Bin location
    ------------------------------------------------------------------------------*/
        {util/dev/ValidateWith2Criteria.i 
        &ValidateTable = "rm-bin" 
        &ValidateField1 = "loc-bin"
        &ValidateField2 = "loc"  
        &ValidateMessage = "RM Bin Location for warehouse"}
END PROCEDURE.

PROCEDURE pIsValidWipBinForLoc:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Wip Bin
     Notes: Input 1 = Bin location
    ------------------------------------------------------------------------------*/
        {util/dev/ValidateWith2Criteria.i 
        &ValidateTable = "Wip-bin" 
        &ValidateField1 = "loc-bin"
        &ValidateField2 = "loc"  
        &ValidateMessage = "Wip Bin Location for warehouse"}
END PROCEDURE.

PROCEDURE pIsValidSalesRep:
    /*------------------------------------------------------------------------------
     Purpose:  Validates SalesRep 
     Notes: Input 1 = Sales Rep Code
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i 
        &ValidateTable = "sman" 
        &ValidateField = "sman" 
        &ValidateMessage = "Sales Rep"}
    

END PROCEDURE.

PROCEDURE pIsValidState:
    /*------------------------------------------------------------------------------
     Purpose:  Validates State 
     Notes: Input 1 = state
    ------------------------------------------------------------------------------*/
    {util/dev/ValidateWithNoCompany.i 
        &ValidateTable = "state" 
        &ValidateField = "state" 
        &ValidateMessage = "State"}
   

END PROCEDURE.

PROCEDURE pIsValidTaxGroup:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Tax Group 
     Notes: Input 1 = Tax Group
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i 
        &ValidateTable = "stax" 
        &ValidateField = "tax-group" 
        &ValidateMessage = "Tax Group"}
    
END PROCEDURE.

PROCEDURE pIsValidTerms:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Terms 
     Notes: Input 1 = Terms Code
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i 
        &ValidateTable = "terms" 
        &ValidateField = "t-code" 
        &ValidateMessage = "Terms"}
    
END PROCEDURE.

PROCEDURE pIsValidVendor:
    /*------------------------------------------------------------------------------
     Purpose:  Validates vendor
     Notes: Input 1 = vendor
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i 
        &ValidateTable = "vend" 
        &ValidateField = "vend-no" 
        &ValidateMessage = "Vendor"}

END PROCEDURE. 

PROCEDURE pIsValidVendorType:
    /*------------------------------------------------------------------------------
     Purpose:  Validates vendor type
     Notes: Input 1 = vendor type
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i 
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

PROCEDURE pIsValidUserId:
    /*------------------------------------------------------------------------------
     Purpose:  Validates User ID
     Notes: Input 1 = User id
    ------------------------------------------------------------------------------*/
    {util/dev/ValidateWithNoCompany.i 
        &ValidateTable = "users" 
        &ValidateField = "user_id" 
        &ValidateMessage = "User ID"}
    

END PROCEDURE. 

PROCEDURE pIsValidPrepCode:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Prep Code
     Notes: Input 1 = Prep Code
    ------------------------------------------------------------------------------*/
    {util/dev/ValidateWithNoCompany.i 
        &ValidateTable = "prep" 
        &ValidateField = "code" 
        &ValidateMessage = "Prep Code"}
    

END PROCEDURE.

PROCEDURE pIsValidRMITemID:
    /*------------------------------------------------------------------------------
     Purpose:  Validates RM Item ID 
     Notes: Input 1 = RM Item Number
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i 
        &ValidateTable = "item" 
        &ValidateField = "i-no" 
        &ValidateMessage = "RM Item ID"}

END PROCEDURE.

PROCEDURE pIsValidMatType:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Mat Type
     Notes: Input 1 = Mat Type
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i
        &ValidateTable = "matprep" 
        &ValidateField = "mat" 
        &ValidateMessage = "Mat Type"}
    

END PROCEDURE.

PROCEDURE pIsValidCostType:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Cost Type
     Notes: Input 1 = Cost Type
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i
        &ValidateTable = "costtype" 
        &ValidateField = "cost-type" 
        &ValidateMessage = "Cost Type"}
    

END PROCEDURE.

PROCEDURE pIsValidJob:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Cost Type
     Notes: Input 1 = Cost Type
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i
        &ValidateTable = "job" 
        &ValidateField = "job-no" 
        &ValidateMessage = "Job Number"}
    

END PROCEDURE.

PROCEDURE pIsValidPoNo:
    /*------------------------------------------------------------------------------
     Purpose:  Validates Cost Type
     Notes: Input 1 = Cost Type
    ------------------------------------------------------------------------------*/
    {util/dev/validateIntField.i
        &ValidateTable = "po-ord" 
        &ValidateField = "po-no" 
        &ValidateMessage = "PO #"}
    

END PROCEDURE.
PROCEDURE pIsValidFlute:
    /*------------------------------------------------------------------------------
     Purpose:  Validates FG Item ID 
     Notes: Input 1 = FG Item Number
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i 
        &ValidateTable = "flute" 
        &ValidateField = "code" 
        &ValidateMessage = "Flute"}

END PROCEDURE.
PROCEDURE pIsValidDept:
    /*------------------------------------------------------------------------------
     Purpose:  Validates FG Item ID 
     Notes: Input 1 = FG Item Number
    ------------------------------------------------------------------------------*/
    {util/dev/ValidateWithNoCompany.i 
        &ValidateTable = "dept" 
        &ValidateField = "code" 
        &ValidateMessage = "Department"}

END PROCEDURE.

PROCEDURE pIsValidMaterialType:
    /*------------------------------------------------------------------------------
    Purpose:  Validates Mat Type
    Notes: Input 1 = Mat Type
    ------------------------------------------------------------------------------*/
    {util/dev/ValidateWithNoCompany.i
        &ValidateTable = "mat" 
        &ValidateField = "mat"        
        &ValidateMessage = "Mat Type"}
        
END PROCEDURE.

PROCEDURE pIsValidProcat:
    /*------------------------------------------------------------------------------
    Purpose:  Validates Mat Type
    Notes: Input 1 = Mat Type
    ------------------------------------------------------------------------------*/
    {util/dev/validate.i
        &ValidateTable = "procat" 
        &ValidateField = "procat"        
        &ValidateMessage = "Product Category"}
        
END PROCEDURE.

PROCEDURE pIsValidNAICS:
    /*------------------------------------------------------------------------------
    Purpose:  Validates Mat Type
    Notes: Input 1 = Mat Type
    ------------------------------------------------------------------------------*/
    {util/dev/ValidateWithNoCompany.i
        &ValidateTable = "NAICS" 
        &ValidateField = "naicsID"        
        &ValidateMessage = "NAICS Id"}
        
END PROCEDURE.
