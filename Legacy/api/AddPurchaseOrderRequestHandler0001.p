/*------------------------------------------------------------------------
    File        : api/AddPurchaseOrderRequestHandler0001.p
    Purpose     : Returns the request data for purchase order addition

    Syntax      :

    Description : Returns the request data for purchase order addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER ipcParentID      AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage       AS CHARACTER NO-UNDO.
DEFINE OUTPUT       PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.

DEFINE  VARIABLE  lcDetailData   AS   LONGCHAR   NO-UNDO.
DEFINE  VARIABLE  lcDetailData1  AS   LONGCHAR   NO-UNDO.
DEFINE  VARIABLE  lcDetailData2  AS   LONGCHAR   NO-UNDO.
DEFINE  VARIABLE  lineNum        AS   CHARACTER  NO-UNDO  INITIAL  1.
DEFINE  VARIABLE  itemCode       AS   CHARACTER  NO-UNDO  INITIAL  "1001".
DEFINE  VARIABLE  description    AS   CHARACTER  NO-UNDO  INITIAL  "ACETAMINOPHEN 325MG 100 TABS".
DEFINE  VARIABLE  uom            AS   CHARACTER  NO-UNDO  INITIAL  "Each".
DEFINE  VARIABLE  quantity       AS   CHARACTER  NO-UNDO  INITIAL  10.
DEFINE  VARIABLE  whsCode        AS   CHARACTER  NO-UNDO  INITIAL  "005".
DEFINE  VARIABLE  qTyperPack     AS   CHARACTER  NO-UNDO  INITIAL  24.
DEFINE  VARIABLE  purchaseUnit   AS   CHARACTER  NO-UNDO  INITIAL  "CASE24".
DEFINE  VARIABLE  lastRequestIP  AS   CHARACTER  NO-UNDO  INITIAL  "localhost".
DEFINE  VARIABLE  company        AS   CHARACTER  NO-UNDO  INITIAL  "ASI".
DEFINE  VARIABLE  docType        AS   CHARACTER  NO-UNDO  INITIAL  "PO".
DEFINE  VARIABLE  docEntry       AS   CHARACTER  NO-UNDO  INITIAL  "25046".
DEFINE  VARIABLE  docNum         AS   CHARACTER  NO-UNDO  INITIAL  "25046".
DEFINE  VARIABLE  docStatus      AS   CHARACTER  NO-UNDO  INITIAL  "1".
DEFINE  VARIABLE  docDate        AS   CHARACTER  NO-UNDO  INITIAL  "2019-05-14T00:00:00".
DEFINE  VARIABLE  reqDate        AS   CHARACTER  NO-UNDO  INITIAL  "2019-05-14T00:00:00".
DEFINE  VARIABLE  cardCode       AS   CHARACTER  NO-UNDO  INITIAL  "001".
DEFINE  VARIABLE  cardName       AS   CHARACTER  NO-UNDO  INITIAL  "Siggins".
DEFINE  VARIABLE  numatCard      AS   CHARACTER  NO-UNDO  INITIAL  "123".
DEFINE  VARIABLE  docDueDate     AS   CHARACTER  NO-UNDO  INITIAL  "2019-05-14T00:00:00".


FIND FIRST APIOutboundDetail NO-LOCK
     WHERE APIOutboundDetail.detailID = "detail"
       AND APIOutboundDetail.parentID = ipcParentID
     NO-ERROR.

IF AVAILABLE APIOutboundDetail THEN
    ASSIGN
        lcDetailData1 = APIOutboundDetail.data   
        lcDetailData2 = APIOutboundDetail.data   
        lcDetailData1 = REPLACE(lcDetailData1, "TBD9", lastRequestIP)
        lcDetailData1 = REPLACE(lcDetailData1, "TBD8", purchaseUnit)
        lcDetailData1 = REPLACE(lcDetailData1, "TBD7", qTyperPack)
        lcDetailData1 = REPLACE(lcDetailData1, "TBD6", whsCode)
        lcDetailData1 = REPLACE(lcDetailData1, "TBD5", quantity)
        lcDetailData1 = REPLACE(lcDetailData1, "TBD4",  uom)
        lcDetailData1 = REPLACE(lcDetailData1, "TBD3",  description)
        lcDetailData1 = REPLACE(lcDetailData1, "TBD2",  itemCode)
        lcDetailData1 = REPLACE(lcDetailData1, "TBD1",  lineNum)
        lcDetailData2 = REPLACE(lcDetailData2, "TBD9", lastRequestIP)
        lcDetailData2 = REPLACE(lcDetailData2, "TBD8", purchaseUnit)
        lcDetailData2 = REPLACE(lcDetailData2, "TBD7", qTyperPack)
        lcDetailData2 = REPLACE(lcDetailData2, "TBD6", whsCode)
        lcDetailData2 = REPLACE(lcDetailData2, "TBD5", quantity)
        lcDetailData2 = REPLACE(lcDetailData2, "TBD4",  uom)
        lcDetailData2 = REPLACE(lcDetailData2, "TBD3",  description)
        lcDetailData2 = REPLACE(lcDetailData2, "TBD2",  itemCode)
        lcDetailData2 = REPLACE(lcDetailData2, "TBD1", lineNum)
        .     
        
         lcDetailData = lcDetailData1 + "," + lcDetailData2.   
        
    ASSIGN
         ioplcRequestData = REPLACE(ioplcRequestData, "TBD12", lcDetailData)
         ioplcRequestData = REPLACE(ioplcRequestData, "TBD11", lastRequestIP)
         ioplcRequestData = REPLACE(ioplcRequestData, "TBD10", docDueDate)
         ioplcRequestData = REPLACE(ioplcRequestData, "TBD9",  numatCard)
         ioplcRequestData = REPLACE(ioplcRequestData, "TBD8",  cardName)
         ioplcRequestData = REPLACE(ioplcRequestData, "TBD7",  cardCode)
         ioplcRequestData = REPLACE(ioplcRequestData, "TBD6",  reqDate)
         ioplcRequestData = REPLACE(ioplcRequestData, "TBD5",  docDate)
         ioplcRequestData = REPLACE(ioplcRequestData, "TBD4",  docStatus)
         ioplcRequestData = REPLACE(ioplcRequestData, "TBD3",  docNum)
         ioplcRequestData = REPLACE(ioplcRequestData, "TBD2",  docEntry)
         ioplcRequestData = REPLACE(ioplcRequestData, "TBD1",  docType)
         ioplcRequestData = REPLACE(ioplcRequestData, "po-ord.company",  company)
         opcMessage = ""
         oplSuccess = TRUE
         .

