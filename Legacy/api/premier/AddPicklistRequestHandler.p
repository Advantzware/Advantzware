/*------------------------------------------------------------------------
    File        : api/premier/AddPicklistRequestHandler.p
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
DEFINE VARIABLE lineNum       AS   INTEGER    NO-UNDO  INITIAL  1.
DEFINE VARIABLE itemCode      AS   CHARACTER  NO-UNDO  INITIAL  "1001".
DEFINE VARIABLE quantity      AS   INTEGER    NO-UNDO  INITIAL  10.
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
        lcDetailData1 = SUBSTITUTE (
                            lcDetailData1,
                            lineNum,
                            itemCode,
                            quantity,
                            whsCode,
                            unitsPerPack,
                            salesUnit,
                            lotNumber
                            )
        lcDetailData2 = SUBSTITUTE (
                            lcDetailData2,
                            lineNum,
                            itemCode,
                            quantity,
                            whsCode,
                            unitsPerPack,
                            salesUnit,
                            lotNumber
                             ).

lcDetailData = lcDetailData1 + "," + lcDetailData2.   

ASSIGN
    ioplcRequestData = REPLACE(ioplcRequestData, "&16", lcDetailData)
    ioplcRequestData = REPLACE(ioplcRequestData, "&15", priority)
    ioplcRequestData = REPLACE(ioplcRequestData, "&14", slpCode)
    ioplcRequestData = REPLACE(ioplcRequestData, "&13", address)
    ioplcRequestData = REPLACE(ioplcRequestData, "&12", routeID)
    ioplcRequestData = REPLACE(ioplcRequestData, "&11", docDueDate)
    ioplcRequestData = REPLACE(ioplcRequestData, "&10", numatCard)
    ioplcRequestData = REPLACE(ioplcRequestData, "&9",  cardName)
    ioplcRequestData = REPLACE(ioplcRequestData, "&8",  cardCode)
    ioplcRequestData = REPLACE(ioplcRequestData, "&7",  reqDate)
    ioplcRequestData = REPLACE(ioplcRequestData, "&6",  docDate)
    ioplcRequestData = REPLACE(ioplcRequestData, "&5",  docStatus)
    ioplcRequestData = REPLACE(ioplcRequestData, "&4",  docNum)
    ioplcRequestData = REPLACE(ioplcRequestData, "&3",  docEntry)
    ioplcRequestData = REPLACE(ioplcRequestData, "&2",  docType)
    ioplcRequestData = REPLACE(ioplcRequestData, "&1",  company)
    .

ASSIGN
    opcMessage = ""
    oplSuccess = TRUE
    .     
