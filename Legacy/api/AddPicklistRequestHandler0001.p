/*------------------------------------------------------------------------
    File        : api/AddPicklistRequestHandler0001.p
    Purpose     : Returns the request data for picklist addition

    Syntax      :

    Description : Returns the request data for picklist addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
  
DEFINE INPUT        PARAMETER ipcParentID      AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage       AS CHARACTER NO-UNDO.
DEFINE OUTPUT       PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.

DEFINE VARIABLE lcDetailData  AS   LONGCHAR   NO-UNDO.
DEFINE VARIABLE lcDetailData1 AS   LONGCHAR   NO-UNDO.
DEFINE VARIABLE lcDetailData2 AS   LONGCHAR   NO-UNDO.
DEFINE VARIABLE lineNum       AS   CHARACTER  NO-UNDO  INITIAL  1.
DEFINE VARIABLE itemCode      AS   CHARACTER  NO-UNDO  INITIAL  "1001".
DEFINE VARIABLE quantity      AS   CHARACTER  NO-UNDO  INITIAL  10.
DEFINE VARIABLE whsCode       AS   CHARACTER  NO-UNDO  INITIAL  "005".
DEFINE VARIABLE unitsPerPack  AS   CHARACTER  NO-UNDO  INITIAL  "1".
DEFINE VARIABLE salesUnit     AS   CHARACTER  NO-UNDO  INITIAL  "Each".
DEFINE VARIABLE lotNumber     AS   CHARACTER  NO-UNDO  INITIAL  "190002".
DEFINE VARIABLE company       AS   CHARACTER  NO-UNDO  INITIAL  "ASI".
DEFINE VARIABLE docType       AS   CHARACTER  NO-UNDO  INITIAL  "SO".
DEFINE VARIABLE docEntry      AS   CHARACTER  NO-UNDO  INITIAL  "150245".
DEFINE VARIABLE docNum        AS   CHARACTER  NO-UNDO  INITIAL  "150245".
DEFINE VARIABLE docStatus     AS   CHARACTER  NO-UNDO  INITIAL  "1".
DEFINE VARIABLE docDate       AS   CHARACTER  NO-UNDO  INITIAL  "2019-05-14T00:00:00".
DEFINE VARIABLE reqDate       AS   CHARACTER  NO-UNDO  INITIAL  "2019-05-14T00:00:00".
DEFINE VARIABLE cardCode      AS   CHARACTER  NO-UNDO  INITIAL  "201".
DEFINE VARIABLE cardName      AS   CHARACTER  NO-UNDO  INITIAL  "Shoes DC".
DEFINE VARIABLE numatCard     AS   CHARACTER  NO-UNDO  INITIAL  "987".
DEFINE VARIABLE docDueDate    AS   CHARACTER  NO-UNDO  INITIAL  "2019-05-14T00:0:00".
DEFINE VARIABLE routeID       AS   CHARACTER  NO-UNDO  INITIAL  "MON01".
DEFINE VARIABLE address       AS   CHARACTER  NO-UNDO  INITIAL  "Louisville".
DEFINE VARIABLE slpCode       AS   CHARACTER  NO-UNDO  INITIAL  "898".
DEFINE VARIABLE priority      AS   CHARACTER  NO-UNDO  INITIAL  "2".

FIND FIRST APIOutboundDetail NO-LOCK
     WHERE APIOutboundDetail.detailID = "detail"
       AND APIOutboundDetail.parentID = ipcParentID
     NO-ERROR.

IF AVAILABLE APIOutboundDetail THEN
     ASSIGN
        lcDetailData1 = APIOutboundDetail.data   
        lcDetailData2 = APIOutboundDetail.data 
        lcDetailData1 = REPLACE(lcDetailData1, "TBD7",  lotNumber)
        lcDetailData1 = REPLACE(lcDetailData1, "TBD6",  salesUnit)
        lcDetailData1 = REPLACE(lcDetailData1, "TBD5",  unitsPerPack)
        lcDetailData1 = REPLACE(lcDetailData1, "TBD4",  whsCode)
        lcDetailData1 = REPLACE(lcDetailData1, "TBD3",  quantity)
        lcDetailData1 = REPLACE(lcDetailData1, "TBD2",  itemCode)
        lcDetailData1 = REPLACE(lcDetailData1, "TBD1",  lineNum)
        lcDetailData2 = REPLACE(lcDetailData2, "TBD7",  lotNumber)
        lcDetailData2 = REPLACE(lcDetailData2, "TBD6",  salesUnit)
        lcDetailData2 = REPLACE(lcDetailData2, "TBD5",  unitsPerPack)
        lcDetailData2 = REPLACE(lcDetailData2, "TBD4",  whsCode)
        lcDetailData2 = REPLACE(lcDetailData2, "TBD3",  quantity)
        lcDetailData2 = REPLACE(lcDetailData2, "TBD2",  itemCode)
        lcDetailData2 = REPLACE(lcDetailData2, "TBD1",  lineNum)
        .

lcDetailData = lcDetailData1 + "," + lcDetailData2.   

ASSIGN
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD16", lcDetailData)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD15", priority)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD14", slpCode)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD13", address)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD12", routeID)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD11", docDueDate)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD10", numatCard)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD9",  cardName)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD8",  cardCode)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD7",  reqDate)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD6",  docDate)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD5",  docStatus)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD4",  docNum)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD3",  docEntry)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD2",  docType)
    ioplcRequestData = REPLACE(ioplcRequestData, "TBD1",  company)
    .

ASSIGN
    opcMessage = ""
    oplSuccess = TRUE
    .     
