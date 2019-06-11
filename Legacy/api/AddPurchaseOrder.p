/*------------------------------------------------------------------------
    File        : api/AddPurchaseOrder.p
    Purpose     : Returns the request data for purchase order addition

    Syntax      :

    Description : Returns the request data for purchase order addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER ipcParentID     AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER oplcRequestData AS LONGCHAR  NO-UNDO.

DEFINE VARIABLE lcDetailData  AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcDetailData1 AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcDetailData2 AS LONGCHAR NO-UNDO.

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
                            1.0,
                            "1001",
                            "ACETAMINOPHEN 325MG 100 TABS",
                            "Each",
                            10.0,
                            "005",
                            24.0,
                            "CASE24",
                            "localhost"                       
                            )
        lcDetailData2 = SUBSTITUTE (
                            lcDetailData2,
                            1.0,
                            "1002",
                            "ACETAMINOPHEN 325MG 100 TABS",
                            "Each",
                            10.0,
                            "005",
                            24.0,
                            "CASE24",
                            "localhost"                       
                            ).
        
        lcDetailData = lcDetailData1 + "," + lcDetailData2.   
        
        ASSIGN
            oplcRequestData = REPLACE(oplcRequestData, "&13", lcDetailData)
            oplcRequestData = REPLACE(oplcRequestData, "&12", "localhost")
            oplcRequestData = REPLACE(oplcRequestData, "&11", "2019-05-14T00:00:00")
            oplcRequestData = REPLACE(oplcRequestData, "&10", "123")
            oplcRequestData = REPLACE(oplcRequestData, "&9",  "Siggins")
            oplcRequestData = REPLACE(oplcRequestData, "&8",  "001")
            oplcRequestData = REPLACE(oplcRequestData, "&7",  "2019-05-14T00:00:00")
            oplcRequestData = REPLACE(oplcRequestData, "&6",  "2019-05-14T00:00:00")
            oplcRequestData = REPLACE(oplcRequestData, "&5",  "1")
            oplcRequestData = REPLACE(oplcRequestData, "&4",  "25046")
            oplcRequestData = REPLACE(oplcRequestData, "&3",  "25046")
            oplcRequestData = REPLACE(oplcRequestData, "&2",  "PO")
            oplcRequestData = REPLACE(oplcRequestData, "&1",  "ASI")
            .
