/* ttScheduledReleases.i */

/* Scheduled Releases.rpa */
DEFINE TEMP-TABLE ttScheduledReleases NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD jobNo           AS CHARACTER LABEL "Job No"           FORMAT "X(9)"
    FIELD customerName    AS CHARACTER LABEL "Customer Name"    FORMAT "X(30)"
    FIELD shipTo          AS CHARACTER LABEL "ShipTo"           FORMAT "X(8)"
    FIELD poNo            AS CHARACTER LABEL "PO No"            FORMAT "X(15)"
    FIELD orderNo         AS INTEGER   LABEL "Order No"         FORMAT ">>>>>9"
    FIELD relNo           AS INTEGER   LABEL "Rel No"           FORMAT ">>>>>9"
    FIELD itemNo          AS CHARACTER LABEL "Item No"          FORMAT "X(15)"
    FIELD itemDescription AS CHARACTER LABEL "Item Description" FORMAT "X(30)"
    FIELD relQty          AS INTEGER   LABEL "Rel Qty"          FORMAT "->,>>>,>>9"
    FIELD relDate         AS DATE      LABEL "Rel Date"         FORMAT "99/99/9999"
    FIELD relStat         AS CHARACTER LABEL "Rel Stat"         FORMAT "X(1)"
    FIELD dueAlert        AS CHARACTER LABEL "Due Alert"        FORMAT "X(2)"
    FIELD carrier         AS CHARACTER LABEL "Carrier"          FORMAT "X(5)"
    FIELD jobQtyOH        AS INTEGER   LABEL "Job Qty OH"       FORMAT "->,>>>,>>9"
    FIELD totQtyOH        AS INTEGER   LABEL "Tot Qty OH"       FORMAT "->,>>>,>>9"
    FIELD salesValue      AS INTEGER   LABEL "Sales Value"      FORMAT "->,>>>,>>9"
    FIELD orderQty        AS INTEGER   LABEL "Order Qty"        FORMAT "->,>>>,>>9"
    FIELD msf             AS DECIMAL   LABEL "MSF"              FORMAT "->,>>>,>>9.999"
    FIELD shippedQty      AS INTEGER   LABEL "Shipped Qty"      FORMAT "->,>>>,>>9"
    FIELD custNo          AS CHARACTER LABEL "Cust No"          FORMAT "X(8)"
    FIELD customerPartNo  AS CHARACTER LABEL "Customer Part No" FORMAT "X(15)"
    FIELD delZone         AS CHARACTER LABEL "Del Zone"         FORMAT "X(5)"
    FIELD terr            AS CHARACTER LABEL "Terr"             FORMAT "X(3)"
    FIELD creditRating    AS CHARACTER LABEL "Credit Rating"    FORMAT "X(3)"
    FIELD routing         AS CHARACTER LABEL "Routing"          FORMAT "X(30)"
    FIELD skidQty         AS INTEGER   LABEL "Skid Qty"         FORMAT "->,>>>,>>9"
    FIELD ohRelQty        AS INTEGER   LABEL "OH Rel Qty"       FORMAT "->,>>>,>>9"
    FIELD sampleDate      AS DATE      LABEL "Sample Date"      FORMAT "99/99/9999"
    FIELD dockDate        AS DATE      LABEL "Dock Date"        FORMAT "99/99/9999"
    FIELD earlyDate       AS DATE      LABEL "Early Date"       FORMAT "99/99/9999"
    FIELD lateDate        AS DATE      LABEL "Late Date"        FORMAT "99/99/9999"
    FIELD transitDays     AS INTEGER   LABEL "Transit Days"     FORMAT ">>>>>>>>>>9"
    FIELD state           AS CHARACTER LABEL "State"            FORMAT "X(2)"
    FIELD totalAlloc      AS INTEGER   LABEL "Total Alloc"      FORMAT "->,>>>,>>9"
    FIELD totalAvail      AS INTEGER   LABEL "Total Avail"      FORMAT "->,>>>,>>9"
    FIELD shipFrom        AS CHARACTER LABEL "Ship From"        FORMAT "X(3)"
    FIELD dockNote        AS CHARACTER LABEL "Dock Note"        FORMAT "X(10)"
    FIELD salRep          AS CHARACTER LABEL "Sal Rep"          FORMAT "X(3)"
    FIELD lastUserID      AS CHARACTER LABEL "Last User ID"     FORMAT "X(8)"
    FIELD shipToAdd1      AS CHARACTER LABEL "ShipTo Add1"      FORMAT "X(30)"
    FIELD shipToAdd2      AS CHARACTER LABEL "ShipTo Add2"      FORMAT "X(30)"
    FIELD shipToCity      AS CHARACTER LABEL "ShipTo City"      FORMAT "X(15)"
    FIELD shipToState     AS CHARACTER LABEL "ShipTo State"     FORMAT "X(2)"
    FIELD shipToZip       AS CHARACTER LABEL "ShipTo Zip"       FORMAT "X(10)"
    FIELD shipToName      AS CHARACTER LABEL "ShipTo Name"      FORMAT "X(30)"
    FIELD dueDate         AS DATE      LABEL "Due Date"         FORMAT "99/99/9999"
    FIELD style           AS CHARACTER LABEL "Style"            FORMAT "X(6)"
    FIELD runComplete     AS LOGICAL   LABEL "Run Complete"     FORMAT "Yes/No"
    FIELD fgCategory      AS CHARACTER LABEL "FG Category"      FORMAT "X(5)"
    FIELD overRunPct      AS DECIMAL   LABEL "OverRun Pct"      FORMAT ">>9.99"
    FIELD jobHoldCode     AS CHARACTER LABEL "Job Hold Code"    FORMAT "X(2)"
    FIELD jobHoldDesc     AS CHARACTER LABEL "Job Hold Desc"    FORMAT "X(45)"
    FIELD xxItemFGRecKey  AS CHARACTER LABEL "Item FG Rec Key"  FORMAT "x(20)"
    FIELD cDockHour       AS CHARACTER LABEL "Dock hour"      FORMAT "X(20)"
    FIELD dLastDate      AS DATE      LABEL "Last Receipt Date"        FORMAT "99/99/9999"    
    FIELD cRelType       AS CHARACTER LABEL "Release Type"      FORMAT "X(12)"
    FIELD xxSort01        AS CHARACTER LABEL "Sort01 By"        FORMAT "x(50)"
    FIELD xxSort02        AS CHARACTER LABEL "Sort02 By"        FORMAT "x(50)"
    FIELD xxSort03        AS CHARACTER LABEL "Sort03 By"        FORMAT "x(50)"
    FIELD xxSort04        AS CHARACTER LABEL "Sort04 By"        FORMAT "x(50)"
    FIELD csrUserID       AS CHARACTER LABEL "CSR"              FORMAT "x(10)"
        INDEX xxSort IS PRIMARY xxSort01 xxSort02 xxSort03 xxSort04
        .
