
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
DEFINE {1} {2} VARIABLE gcTypeProgramsFolder AS CHARACTER INIT "util\".
 
ASSIGN          /*     1           2             3             4              5          */
    gcTypeList = "ttImportAP,ttImportCash,ttImportShipTo,ttImportEstimate,ttImportFG"
                /*          1                    2                        3                                  4                 5       */
    gcTypeLabels = "Accounts Payable (VU1),Cash Receipts (AC1),Customer ShipTos (AF1 - ShipTo Tab),Estimates (EC/EF),Finished Goods (IF1)"
                /*           1       2            3            4             5           */   
    gcTypePrograms = "ImportAP.p,ImportCash.p,ImportShipTo.p,ImportEstimate.p,ImportFG.p"
    .
                     
     

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
