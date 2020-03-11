
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
DEFINE {1} {2} VARIABLE giRecordLimit AS INTEGER INIT 5000.
 
ASSIGN          /*     1           2             3             4              5           6          7              8        9           10           11               12                13                14                     15                16             17        18               19         20                      21                 22           23                 24              25          26            27                28                   29           30         31     */
    gcTypeList = "ttImportAP,ttImportCash,ttImportShipTo,ttImportEstimate,ttImportFG,ttImportVend,ttImportCust,ttImportAR,ttImportGL,ttImportBin,ttImportRelease,ttImportVendCost,ttImportPriceMatrix,ttImportEstimateARD,ttImportEstimateARDP,ttImportUsers,ttImportConfig,ttImportQuote,ttImportUtil,ttImportReOrdLevel,ttImportCarrier,ttImportCarrierMtx,ttImportBankReconl,ttImportPrep,ttImportLoadtag,ttImportAP1,ttImportVendCostMtx,ttImportWarehouse,ttImportRmRctd,ttImportItem,ttImportMach"
                /*               1                                         2                                  3                                      4                                         5                            6                        7                         8                            9                              10                                      11                                               12                            13                       14                                            15                       16                              17                         18                   19                                20                                       21                                       22                                23                            24                                             25                            26                                                27                                28                             29                                   30        */
    gcTypeLabels = "Import - Accounts Payable Invoice (VU1),Import - Cash Receipts (AC1),Import - Customer ShipTos Address (A-F-1 - Ship To),Import - Estimates (EC/EF),Import - Finished Goods Item Inventory (IF1),Import - Vendor (VF1),Import - Customer (AF1),Import - Customer Invoice (AU1),Import - GL Account (GF2),Import - Whse / bin Location (IF4),Import - Order Release (OU1 - Release),Import - Finished Goods (IF1 - Vend Cost),Import - Price Matrix (OF3),Import - Estimate CAD File (EC/EF),Import - Estimate CAD Project (EC/EF),Import - Users (NU),Import - Configuration Settings (NK1),Import - Quote (EQ),Import - Utilities (NM),Import - Finish Goods Inventory (IF1 - Inventory),Import - Common Carriers (AF8),Import - Common Carriers (AF8 - Matrix),Import - Bank Reconciliation (VT1),Import - Prep and Die (EB8),Import - Loadtags (BL - File Maintenance - Finished Goods),Import - Accounts Payable Invoices (Docuware),Import - Vendor Cost Matrix,Import -  Warehouse Locations,Import - Warehouse Transaction Receipt,Import - Raw Materials," +
                /*   31   */
                "Import - Machine(EB1)"
                /*           1       2            3            4             5                 6          7           8           9          10            11                12                13                     14             15          16               17         18            19           20                     21              22            23                   24                25       26            27                   28            29             30            31*/   
    gcTypePrograms = "ImportAP.p,ImportCash.p,ImportShipTo.p,ImportEstimate.p,ImportFG.p,ImportVend.p,ImportCust.p,ImportAR.p,ImportGL.p,ImportBin.p,importRelease.p,ImportVendorCost.p,ImportPriceMatrix.p,ImportEstimate.p,ImportEstimate.p,ImportUser.p,ImportConfig.p,ImportQuote.p,ImportUtil.p,ImportReOrdLevel.p,ImportCarrier.p,ImportCarrierMtx.p,ImportBankReconl.p,ImportPrep.p,ImportLoadtag.p,ImportAP1.p,ImportVendCostMtx.p,ImportWarehouse.p,ImportRmRctd.p,ImportItem.p,ImportMachine.p"
    
    gcUpdatesAllowedTypes = "ttImportShipTo,ttImportFG,ttImportVend,ttImportCust,ttImportGL,ttImportBin,ttImportVendCost,ttImportPriceMatrix,ttImportConfig,ttImportQuote,ttImportUtil,ttImportReOrdLevel,ttImportCarrier,ttImportCarrierMtx,ttImportBankReconl,ttImportPrep,ttImportLoadtag,ttImportVendCostMtx,ttImportWarehouse,ttImportItem,ttImportMach"

    .
                          
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
