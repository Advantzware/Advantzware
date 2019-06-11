/*------------------------------------------------------------------------
    File        : api/AddPicklist.p
    Purpose     : Returns the request data for picklist addition

    Syntax      :

    Description : Returns the request data for picklist addition

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
                            0.0,
                            "005",
                            "1",
                            "Each",
                            "190002"
                            )
        lcDetailData2 = SUBSTITUTE (
                            lcDetailData2,
                            1.0,
                            "1002",
                            0.0,
                            "005",
                            "1",
                            "Each",
                            "190002"
                             ).
        
lcDetailData = lcDetailData1 + "," + lcDetailData2.   

ASSIGN
    oplcRequestData = REPLACE(oplcRequestData, "&16", lcDetailData)
    oplcRequestData = REPLACE(oplcRequestData, "&15", "2")
    oplcRequestData = REPLACE(oplcRequestData, "&14", "898")
    oplcRequestData = REPLACE(oplcRequestData, "&13", "Louisville")
    oplcRequestData = REPLACE(oplcRequestData, "&12", "MON01")
    oplcRequestData = REPLACE(oplcRequestData, "&11", "2019-05-14T00:00:00")
    oplcRequestData = REPLACE(oplcRequestData, "&10", "987")
    oplcRequestData = REPLACE(oplcRequestData, "&9",  "ShoesDC")
    oplcRequestData = REPLACE(oplcRequestData, "&8",  "201")
    oplcRequestData = REPLACE(oplcRequestData, "&7",  "2019-05-14T00:00:00")
    oplcRequestData = REPLACE(oplcRequestData, "&6",  "2019-05-14T00:00:00")
    oplcRequestData = REPLACE(oplcRequestData, "&5",  "1")
    oplcRequestData = REPLACE(oplcRequestData, "&4",  "150245")
    oplcRequestData = REPLACE(oplcRequestData, "&3",  "150245")
    oplcRequestData = REPLACE(oplcRequestData, "&2",  "SO")
    oplcRequestData = REPLACE(oplcRequestData, "&1",  "ASI")
    .
