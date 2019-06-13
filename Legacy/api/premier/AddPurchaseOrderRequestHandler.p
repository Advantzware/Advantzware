/*------------------------------------------------------------------------
    File        : api/premier/AddPurchaseOrderRequestHandler.p
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
DEFINE  VARIABLE  lineNum        AS   INTEGER    NO-UNDO  INITIAL  1.
DEFINE  VARIABLE  itemCode       AS   CHARACTER  NO-UNDO  INITIAL  "1001".
DEFINE  VARIABLE  description    AS   CHARACTER  NO-UNDO  INITIAL  "ACETAMINOPHEN 325MG 100 TABS".
DEFINE  VARIABLE  uom            AS   CHARACTER  NO-UNDO  INITIAL  "Each".
DEFINE  VARIABLE  quantity       AS   INTEGER    NO-UNDO  INITIAL  10.
DEFINE  VARIABLE  whsCode        AS   CHARACTER  NO-UNDO  INITIAL  "005".
DEFINE  VARIABLE  qTyperPack     AS   INTEGER    NO-UNDO  INITIAL  24.
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
        lcDetailData1 = SUBSTITUTE (
                            lcDetailData1,
                            lineNum,
                            itemCode,
                            description,
                            uom,
                            quantity,
                            whsCode,
                            qTyperPack,
                            purchaseUnit,
                            lastRequestIP                      
                            )
        lcDetailData2 = SUBSTITUTE (
                            lcDetailData2,
                            lineNum,
                            itemCode,
                            description,
                            uom,
                            quantity,
                            whsCode,
                            qTyperPack,
                            purchaseUnit,
                            lastRequestIP                         
                            ).
        
        lcDetailData = lcDetailData1 + "," + lcDetailData2.   
        
        ASSIGN
            ioplcRequestData = REPLACE(ioplcRequestData, "&13", lcDetailData)
            ioplcRequestData = REPLACE(ioplcRequestData, "&12", lastRequestIP)
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
            opcMessage = ""
            oplSuccess = TRUE
            .

