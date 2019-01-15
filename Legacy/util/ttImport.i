
/*------------------------------------------------------------------------
    File        : ttImport.i
    Purpose     : Used to generically process an import file and store the data into data array

    Syntax      :

    Description : Definition of the ttImportData Temp-table

    Author(s)   : BV
    Created     : Wed Nov 22 11:54:32 EST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE {1} {2} TEMP-TABLE ttImportData
    FIELD cData       AS CHARACTER EXTENT 200
    FIELD lHeader     AS LOGICAL
    FIELD lValid      AS LOGICAL 
    FIELD cImportNote AS CHARACTER
    FIELD iCount      AS INTEGER  
    FIELD cAction     AS CHARACTER
    .
    
DEFINE {1} {2} TEMP-TABLE ttImportMap
    FIELD cType         AS CHARACTER 
    FIELD iIndex        AS INTEGER     /*primary index for ImportMap - fixed*/
    FIELD cLabel        AS CHARACTER   /*primary label for ImportMap - built off temp-table for type*/
    FIELD cColumnLabel  AS CHARACTER   /*Display for Column in Preview*/
    FIELD iImportIndex  AS INTEGER     /*index map from import file*/
    FIELD cImportLabel  AS CHARACTER   /*index map column header from import file*/
    FIELD iColumnWidth  AS INTEGER 
    FIELD cColumnFormat AS CHARACTER 
    FIELD cDataType     AS CHARACTER  
    FIELD cHelp         AS CHARACTER
    .
    
DEFINE {1} {2} VARIABLE gcTypeList AS CHARACTER.   
DEFINE {1} {2} VARIABLE gcTypeLabels AS CHARACTER.
DEFINE {1} {2} VARIABLE gcTypePrograms AS CHARACTER.
DEFINE {1} {2} VARIABLE gcUpdatesAllowedTypes AS CHARACTER.
DEFINE {1} {2} VARIABLE gcTypeProgramsFolder AS CHARACTER INIT "util\".
 
ASSIGN          /*     1           2             3             4              5           6          7              8        9           10           11               12                13                14                     15                16             17        18               19     */
    gcTypeList = "ttImportAP,ttImportCash,ttImportShipTo,ttImportEstimate,ttImportFG,ttImportVend,ttImportCust,ttImportAR,ttImportGL,ttImportBin,ttImportRelease,ttImportVendCost,ttImportPriceMatrix,ttImportEstimateARD,ttImportEstimateARDP,ttImportUsers,ttImportConfig,ttImportQuote,ttImportUtil"
                /*          1                    2                        3                                  4                 5           6                 7               8                  9                              10                       11                                    12                         13                  14                         15          16                  17                    18                     19      */
    gcTypeLabels = "Accounts Payable (VU1),Cash Receipts (AC1),Customer ShipTos (AF1 - ShipTo Tab),Estimates (EC/EF),Finished Goods (IF1),Vendor (VF1),Customer (AF1),Customer Invoice (AU1),GL Account (GF2),Whse / bin Location (IF4/MF6),Order Release (OU1 - Release Tab),Finished Goods (IF1 - Vend Cost Tab),Price Matrix (OF3),Estimate - CAD File,Estimate - CAD Project,Users (NU),Configuration Settings (NK1),Import Quote(EQ),Import Utilities(NM)"
                /*           1       2            3            4             5                 6          7           8           9          10            11                12                13                     14             15          16               17         18            19       */   
    gcTypePrograms = "ImportAP.p,ImportCash.p,ImportShipTo.p,ImportEstimate.p,ImportFG.p,ImportVend.p,ImportCust.p,ImportAR.p,ImportGL.p,ImportBin.p,importRelease.p,ImportVendorCost.p,ImportPriceMatrix.p,ImportEstimate.p,ImportEstimate.p,ImportUser.p,ImportConfig.p,ImportQuote.p,ImportUtil.p "
    
    gcUpdatesAllowedTypes = "ttImportShipTo,ttImportFG,ttImportVend,ttImportCust,ttImportGL,ttImportBin,ttImportVendCost,ttImportPriceMatrix,ttImportConfig,ttImportQuote,ttImportUtil"
    .
                          
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
