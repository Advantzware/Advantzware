/*------------------------------------------------------------------------
    File        : api/premier/AddProductRequestHandler.p
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
DEFINE VARIABLE product                   AS CHARACTER NO-UNDO INITIAL "1017".
DEFINE VARIABLE description               AS CHARACTER NO-UNDO INITIAL "Box T1".
DEFINE VARIABLE itmsGrpCod                AS CHARACTER NO-UNDO INITIAL "1".
DEFINE VARIABLE codeBars                  AS CHARACTER NO-UNDO INITIAL "765987".
DEFINE VARIABLE cat01                     AS CHARACTER NO-UNDO INITIAL "CUST01".
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
    ioplcRequestData = REPLACE(ioplcRequestData, "&21",  cardCode)
    ioplcRequestData = REPLACE(ioplcRequestData, "&20",  weight)
    ioplcRequestData = REPLACE(ioplcRequestData, "&19",  unitsPerSale)
    ioplcRequestData = REPLACE(ioplcRequestData, "&18",  refUnit)
    ioplcRequestData = REPLACE(ioplcRequestData, "&17",  unitsPerPack)
    ioplcRequestData = REPLACE(ioplcRequestData, "&16",  packUnit)
    ioplcRequestData = REPLACE(ioplcRequestData, "&15",  mainUnit)
    ioplcRequestData = REPLACE(ioplcRequestData, "&14",  boxesXpallet)
    ioplcRequestData = REPLACE(ioplcRequestData, "&13",  boxesXbed)
    ioplcRequestData = REPLACE(ioplcRequestData, "&12",  saleRestrictionDays)
    ioplcRequestData = REPLACE(ioplcRequestData, "&11",  purchaseRestrictionDays)
    ioplcRequestData = REPLACE(ioplcRequestData, "&10",  cat05)
    ioplcRequestData = REPLACE(ioplcRequestData, "&9",  cat04)
    ioplcRequestData = REPLACE(ioplcRequestData, "&8",  cat03)
    ioplcRequestData = REPLACE(ioplcRequestData, "&7",  cat02)
    ioplcRequestData = REPLACE(ioplcRequestData, "&6",  cat01)
    ioplcRequestData = REPLACE(ioplcRequestData, "&5",  codeBars)
    ioplcRequestData = REPLACE(ioplcRequestData, "&4",  itmsGrpCod)
    ioplcRequestData = REPLACE(ioplcRequestData, "&3",  description)
    ioplcRequestData = REPLACE(ioplcRequestData, "&2",  product)
    ioplcRequestData = REPLACE(ioplcRequestData, "&1",  company)
    .

ASSIGN
    opcMessage = ""
    oplSuccess = TRUE
    .   
