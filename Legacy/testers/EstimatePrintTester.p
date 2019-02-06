
/*------------------------------------------------------------------------
    File        : EstimatePrintTester.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : BV
    Created     : Thu Jan 24 16:45:11 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{custom/xprint.i}
{est/ttEstPrint.i "NEW SHARED"}
DEFINE VARIABLE gcOutputFile AS CHARACTER INIT "C:\temp\estPrintOut.txt".

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

RUN pBuildTestData.
FIND FIRST ttEstHeader NO-LOCK.
RUN est\EstimatePrint.p (ROWID(ttEstHeader), gcOutputFile, "By Form with Item Summary","Classic").
RUN printFile(gcOutputFile).
/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildTestData PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Builds the temptables to test with
 Notes:
------------------------------------------------------------------------------*/
CREATE ttEstHeader.
ASSIGN 
    ttEstHeader.cEstNo = "123456"
    ttEstHeader.cCalculator = "est"
    ttEstHeader.cEstType = "Single"
    ttEstHeader.cPrinter = "sales"
    ttEstHeader.dQtyMaster = 1000
    ttEstHeader.dtCalcDateTime = NOW - 2000000000
    ttEstHeader.dtPrintDateTime = NOW
    ttEstHeader.rec_KeyHeader = "1"
    .
CREATE ttEstItem.
ASSIGN 
    ttEstItem.rec_keyItem = "2"
    ttEstItem.rec_keyHeader = ttEstHeader.rec_KeyHeader
    ttEstItem.dQtyPerSet = 1
    ttEstItem.dQtyRequired = 15000
    ttEstItem.dQtyYielded = 15000
    ttEstItem.cItemName = "Long Item Name"
    ttEstItem.cItemDescription1 = "Long Description 1"
    ttEstItem.cItemDescription2 = "Long Description 2"
    ttEstItem.cStyle = "Reverse Tuck"
    ttEstItem.lIsSet = NO
    ttEstItem.cCustomerName = "Customer Name"
    ttEstItem.cCustomerAddress1 = "Customer Address 1"
    ttEstItem.cCustomerAddress2 = "Customer Address 2"
    ttEstItem.cCustomerAddress3 = "Customer City, ST Zipcode"
    ttEstItem.cShipToName = "Shipto Name"
    ttEstItem.cShipToAddress1 = "Shipto Address 1"
    ttEstItem.cShipToAddress2 = "Shipto Address 2"
    ttEstItem.cShipToAddress3 = "Shipto City, ST Zipcode"
    ttEstItem.cSalesgroupName = "Salesgroup Name"
    ttEstItem.cCustomerID = "CUSTID"
    ttEstItem.cShipToID = "SHIPID"
    ttEstItem.cColor = "Color Description"
    .
CREATE ttEstForm.
ASSIGN 
    ttEstForm.rec_KeyForm = "3"
    ttEstForm.rec_keyHeader = ttEstHeader.rec_KeyHeader
    ttEstForm.iFormNo = 1
    ttEstForm.iNumOutLength = 1
    ttEstForm.iNumOutWidth = 2
    ttEstForm.iNumOut = MAX(ttEstForm.iNumOutLength, 1) * MAX(ttEstForm.iNumOutWidth, 1) * MAX(ttEstForm.iNumOutDepth, 1)
    ttEstForm.dGrossWidth = 48 
    ttEstForm.dGrossLength = 74 
    ttEstForm.dGrossArea = ttEstForm.dGrossWidth * ttEstForm.dGrossLength / 144
    ttEstForm.dNetWidth = ttEstForm.dGrossWidth / iNumOutWidth - 0.25
    ttEstForm.dNetLength = ttEstForm.dGrossLength / iNumOutLength - 0.25
    ttEstForm.dNetArea = ttEstForm.dNetWidth * ttEstForm.dGrossLength / 144
    ttEstForm.dDieWidth = ttEstForm.dNetWidth - .625
    ttEstForm.dDieLength = ttEstForm.dNetLength - .625
    ttEstForm.dDieArea = ttEstForm.dDieWidth * ttEstForm.dDieLength / 144
    ttEstForm.cUOMDimension = "In"
    ttEstForm.cUOMArea = "SF"
    ttEstForm.dGrossQtyRequiredWasteMR = 20
    ttEstForm.dGrossQtyRequiredWasteRun = 6
    ttEstForm.dBasisWeightInLbsPerSqin = 123 
    ttEstForm.dWeightDie = ttEstForm.dBasisWeightInLbsPerSqin * ttEstForm.dDieArea * 144 / 1000
    ttEstForm.cUOMWeightDie = "LB/MSHT"
    ttEstForm.dWeightNet = ttEstForm.dBasisWeightInLbsPerSqin * ttEstForm.dNetArea * 144 / 1000
    ttEstForm.cUOMWeightNet = "LB/MSHT"
    ttEstForm.dWeightGross = ttEstForm.dBasisWeightInLbsPerSqin * ttEstForm.dGrossArea * 144 / 1000
    ttEstForm.cUOMWeightGross = "LB/MSHT"
    .
CREATE ttEstBlank.
ASSIGN 
    ttEstBlank.rec_keyBlank = "4"
    ttEstBlank.rec_keyForm = ttEstForm.rec_KeyForm
    ttEstBlank.rec_keyItem = ttEstItem.rec_keyItem
    ttEstBlank.iBlankNo = 1
    ttEstBlank.iNumOutLength = 2
    ttEstBlank.iNumOutWidth = 1
    ttEstBlank.iNumOut = MAX(ttEstBlank.iNumOutWidth, 1) * MAX(ttEstBlank.iNumOutLength, 1) * MAX(ttEstBlank.iNumOutDepth, 1)
    ttEstBlank.dBlankWidth = ttEstForm.dDieWidth / ttEstBlank.iNumOutWidth
    ttEstBlank.dBlankLength = ttEstForm.dDieLength / ttEstBlank.iNumOutLength
    ttEstBlank.dBlankArea = ttEstBlank.dBlankWidth * ttEstBlank.dBlankLength / 144
    ttEstBlank.cUOMArea = "SF"
    ttEstBlank.cUOMDimension = "In"
    ttEstBlank.dLength = 11
    ttEstBlank.dWidth = 11
    ttEstBlank.dDepth = 11
    
    ttEstForm.dGrossQtyRequiredNoWaste = ttEstItem.dQtyRequired / (ttEstBlank.iNumOut * ttEstForm.iNumOut)
    ttEstForm.dGrossQtyRequiredTotal = ttEstForm.dGrossQtyRequiredNoWaste + ttEstForm.dGrossQtyRequiredWasteMR + ttEstForm.dGrossQtyRequiredWasteRun
    ttEstForm.dGrossQtyRequiredTotalArea = ttEstForm.dGrossQtyRequiredTotal * ttEstForm.dGrossArea / 1000
    ttEstForm.cUOMGrossQtyRequiredTotalArea = "MSF"
    
    ttEstItem.cSize = TRIM(STRING(ttEstBlank.dLength,">>>9.99")) + " x " + TRIM(STRING(ttEstBlank.dWidth,">>>9.99"))
    .
    IF ttEstBlank.dDepth NE 0 THEN 
        ttEstItem.cSize = ttEstItem.cSize + " x " + TRIM(STRING(ttEstBlank.dDepth,">>>9.99")).
    
    ASSIGN 
        ttEstBlank.dWeight = ttEstForm.dBasisWeightInLbsPerSqin * ttEstBlank.dBlankArea * 144 / 1000 
        ttEstBlank.cUOMWeight = "LB/M"
        .
    
END PROCEDURE.
