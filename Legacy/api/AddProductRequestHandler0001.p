/*------------------------------------------------------------------------
    File        : api/AddProductRequestHandler0001.p
    Purpose     : Returns the request data for product addition

    Syntax      :

    Description : Returns the request data for product addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage       AS CHARACTER NO-UNDO.
DEFINE OUTPUT       PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.

DEFINE VARIABLE company                   AS CHARACTER NO-UNDO INITIAL "ASI".
DEFINE VARIABLE product                   AS CHARACTER NO-UNDO INITIAL "1019".
DEFINE VARIABLE description               AS CHARACTER NO-UNDO INITIAL "Box T1".
DEFINE VARIABLE itmsGrpCod                AS CHARACTER NO-UNDO INITIAL "1".
DEFINE VARIABLE codeBars                  AS CHARACTER NO-UNDO INITIAL "765987".
DEFINE VARIABLE cat01                     AS CHARACTER NO-UNDO INITIAL "CUST012".
DEFINE VARIABLE cat02                     AS CHARACTER NO-UNDO INITIAL "A".
DEFINE VARIABLE cat03                     AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE cat04                     AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE cat05                     AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE purchaseRestrictionDays   AS CHARACTER NO-UNDO INITIAL "180".
DEFINE VARIABLE saleRestrictionDays       AS CHARACTER NO-UNDO INITIAL "180".
DEFINE VARIABLE boxesXbed                 AS CHARACTER NO-UNDO INITIAL "1"  .
DEFINE VARIABLE boxesXpallet              AS CHARACTER NO-UNDO INITIAL "100".
DEFINE VARIABLE mainUnit                  AS CHARACTER NO-UNDO INITIAL "Each".
DEFINE VARIABLE packUnit                  AS CHARACTER NO-UNDO INITIAL "Each".
DEFINE VARIABLE unitsPerPack              AS CHARACTER NO-UNDO INITIAL "1".
DEFINE VARIABLE refUnit                   AS CHARACTER NO-UNDO INITIAL "Each".
DEFINE VARIABLE unitsPerSale              AS CHARACTER NO-UNDO INITIAL "1".
DEFINE VARIABLE weight                    AS CHARACTER NO-UNDO INITIAL "2".
DEFINE VARIABLE cardCode                  AS CHARACTER NO-UNDO INITIAL "1".

ASSIGN
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD16",  cardCode)
    ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.lockWeight",  weight)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD15",  unitsPerSale)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD14",  refUnit)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD13",  unitsPerPack)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD12",  packUnit)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD11",  mainUnit)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD10",  boxesXpallet)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD9",  boxesXbed)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD8",  saleRestrictionDays)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD7",  purchaseRestrictionDays)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD6",  cat05)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD5",  cat04)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD4",  cat03)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD3",  cat02)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD2",  cat01)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD1",  codeBars)
    ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.cc-code",  itmsGrpCod)
    ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.i-dscr",  description)
    ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.prod-code",  product)
    ioplcRequestData = REPLACE(ioplcRequestData, "itemfg.company",  company)
    .

ASSIGN
    opcMessage = ""
    oplSuccess = TRUE
    .   
