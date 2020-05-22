
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
    
DEFINE {1} {2} VARIABLE gcTypeList             AS CHARACTER.   
DEFINE {1} {2} VARIABLE gcTypeLabels           AS CHARACTER.
DEFINE {1} {2} VARIABLE gcTypePrograms         AS CHARACTER.
DEFINE {1} {2} VARIABLE gcUpdatesAllowedTypes  AS CHARACTER.
DEFINE {1} {2} VARIABLE gcTypeProgramsFolder   AS CHARACTER INIT "util\".
DEFINE {1} {2} VARIABLE giRecordLimit          AS INTEGER   INIT 5000.
DEFINE {1} {2} VARIABLE gcImporterMessageList  AS CHARACTER NO-UNDO.
 
ASSIGN          /*     1           2             3             4              5           6          7              8        9           10           11               12                13                14                     15                16             17        18               19         20                      21                 22           23                 24              25          26            27                28                   29           30         31            32               33                    34 */
    gcTypeList = "ttImportAP,ttImportCash,ttImportShipTo,ttImportEstimate,ttImportFG,ttImportVend,ttImportCust,ttImportAR,ttImportGL,ttImportBin,ttImportRelease,ttImportVendCost,ttImportPriceMatrix,ttImportEstimateARD,ttImportEstimateARDP,ttImportUsers,ttImportConfig,ttImportQuote,ttImportUtil,ttImportReOrdLevel,ttImportCarrier,ttImportCarrierMtx,ttImportBankReconl,ttImportPrep,ttImportLoadtag,ttImportAP1,ttImportVendCostMtx,ttImportWarehouse,ttImportRmRctd,ttImportItem,ttImportMach,ttImportMessage,ttImportapiClientXref,ttImportPo"
                /*               1                         2                      3                                      4               5                     6             7                      8                            9                              10                                      11                       12                            13                       14                              15                            16              17                      18          19                    20                          21                  22                              23                      24                     25                            26                                    27                       28                        29                    30        */
    gcTypeLabels = "Accounts Payable Invoices (VU1),Cash Receipts (AC1),Customer ShipTos Address (AF1 - Shipto Tab),Estimates (EC/EF),FG Item Masters (IF1),Vendors (VF1),Customers (AF1),Accounts Receivable Invoices (AU1),General Ledger Accounts (GF2),Warehouse Bin Locations (IF4),Order Releases (OU1 - Release Tab),FG Vendor Costs (IF1 - Vend Cost),Price Matrix (OF3),Estimates from CAD File (EC/EF),Estimates from CAD Project (EC/EF),Users (NU),Configuration Settings (NK1),Quotes (EQ),Utilities (NM),FG Reorder Levels (IF1 - Bins Tab),Carriers (AF8),Carrier Matrix (AF8 - Matrix),Bank Reconciliation (VT1),Preps and Dies (EB8),Loadtags (BL - File Maintenance),Accounts Payable Invoices (Docuware),Vendor Cost Matrix (PF3),Warehouse Locations (IF4),RM Inventory Receipts,RM Item Masters (MF1/MF2)," +
                /*   31               32         33              34  */
                "Machines (EB1),Message (NZ@),API/EDI (AF1),Purchase Order(PU1)"
                /*           1       2            3            4             5                 6          7           8           9          10            11                12                13                     14             15          16               17         18            19           20                     21              22            23                   24                25       26            27                   28            29             30            31                 32         33                     34  */   
    gcTypePrograms = "ImportAP.p,ImportCash.p,ImportShipTo.p,ImportEstimate.p,ImportFG.p,ImportVend.p,ImportCust.p,ImportAR.p,ImportGL.p,ImportBin.p,importRelease.p,ImportVendorCost.p,ImportPriceMatrix.p,ImportEstimate.p,ImportEstimate.p,ImportUser.p,ImportConfig.p,ImportQuote.p,ImportUtil.p,ImportReOrdLevel.p,ImportCarrier.p,ImportCarrierMtx.p,ImportBankReconl.p,ImportPrep.p,ImportLoadtag.p,ImportAP1.p,ImportVendCostMtx.p,ImportWarehouse.p,ImportRmRctd.p,ImportItem.p,ImportMachine.p,ImportMessage.p,ImportapiClientXref.p,ImportPo.p"
    
    gcUpdatesAllowedTypes = "ttImportShipTo,ttImportFG,ttImportVend,ttImportCust,ttImportGL,ttImportBin,ttImportVendCost,ttImportPriceMatrix,ttImportConfig,ttImportQuote,ttImportUtil,ttImportReOrdLevel,ttImportCarrier,ttImportCarrierMtx,ttImportBankReconl,ttImportPrep,ttImportLoadtag,ttImportVendCostMtx,ttImportWarehouse,ttImportItem,ttImportMach,ttImportMessage,ttImportAP1,ttImportapiClientXref,ttImportPo"

    gcImporterMessageList = "ImportAP1.p"
    .
                          
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
