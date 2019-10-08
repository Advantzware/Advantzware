
/*------------------------------------------------------------------------
    File        : ttFGLabels.i
    Purpose     : Used in FGLabelProcs.p

    Syntax      :

    Description : Temp table definition for ttFGLabels

    Author(s)   : BV
    Created     : Wed Mar 27 15:12:16 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE {1} TEMP-TABLE ttFGLabel
    FIELD cJobNumber AS CHARACTER COLUMN-LABEL "JobNumber"
    FIELD iJobID2 AS INTEGER COLUMN-LABEL "JobRunNumber"
    FIELD cJobID AS CHARACTER COLUMN-LABEL "JobID"
    FIELD cJobIDTrimmed AS CHARACTER COLUMN-LABEL "JobIDTrimmed"
    FIELD cJobIDFull AS CHARACTER COLUMN-LABEL "JobIDFull"
    FIELD cJobIDFullTrimmed AS CHARACTER COLUMN-LABEL "JobIDFullTrimmed"
    FIELD cFGItemID AS CHARACTER COLUMN-LABEL "FGItemID"
    FIELD cFGItemName AS CHARACTER COLUMN-LABEL "FGItemName"
    FIELD cFGCustPart AS CHARACTER COLUMN-LABEL "CustPart" 
    FIELD iForm AS INTEGER COLUMN-LABEL "Form"
    FIELD iBlank AS INTEGER COLUMN-LABEL "Blank"
    FIELD cCustomerID AS CHARACTER COLUMN-LABEL "CustomerID"
    FIELD cCustomerName AS CHARACTER COLUMN-LABEL "CustomerName"
    FIELD cFGItemDesc1 AS CHARACTER COLUMN-LABEL "FGItemDesc1"
    FIELD cFGItemDesc2 AS CHARACTER COLUMN-LABEL "FGItemDesc1"
    FIELD cFGItemDesc3 AS CHARACTER COLUMN-LABEL "FGItemDesc1"        
    FIELD cEstimate AS CHARACTER COLUMN-LABEL "Estimate"
    FIELD cStyle AS CHARACTER COLUMN-LABEL "Style"
    FIELD cDie AS CHARACTER COLUMN-LABEL "Die"
    FIELD cPlate AS CHARACTER COLUMN-LABEL "Plate"
    FIELD cCAD AS CHARACTER COLUMN-LABEL "CAD"
    FIELD cQC AS CHARACTER COLUMN-LABEL "QC"
    FIELD cUPC AS CHARACTER COLUMN-LABEL "UPC"
    FIELD cCompany AS CHARACTER COLUMN-LABEL "Company"
    .


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
