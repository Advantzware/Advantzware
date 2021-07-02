/* subjectID20Defs.i - auto generated 06.11.2021 @ 11:26:29 am */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE dtStartOrderDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndOrderDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartDueDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndDueDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllUserID AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartUserID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartUserName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndUserID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndUserName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllPO AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartPONumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndPONumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllJobNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartJobNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStartJobNo2 AS INTEGER NO-UNDO.
DEFINE VARIABLE cEndJobNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE iEndJobNo2 AS INTEGER NO-UNDO.
DEFINE VARIABLE lAllCAD AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCAD AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCAD AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllItemNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartItemNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartItemDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndItemNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndItemDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllSalesRep AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartSalesRep AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartSalesRepName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSalesRep AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSalesRepName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrderStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWIPQty AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDropOrderUnderRun AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIncludeZeroQtyActReleaseQty AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIncludeZeroOrderBalanceItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIncludeJobsQOH AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIncludeZeroQtyWIPItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIncludeInactiveItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE lUseReleaseDueDate AS LOGICAL NO-UNDO.
DEFINE VARIABLE lShowDetail AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        dtStartOrderDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startOrderDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartOrderDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartOrderDate)
        dtEndOrderDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endOrderDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndOrderDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndOrderDate)
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        dtStartDueDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startDueDate"))
        cDatePickList-3 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-3")
        dtStartDueDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-3,dtStartDueDate)
        dtEndDueDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endDueDate"))
        cDatePickList-4 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-4")
        dtEndDueDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-4,dtEndDueDate)
        lAllUserID = DYNAMIC-FUNCTION("fGetDynParamValue","allUserID") EQ "YES"
        cStartUserID = DYNAMIC-FUNCTION("fGetDynParamValue","startUserID")
        cStartUserName = DYNAMIC-FUNCTION("fGetDynParamValue","startUserName")
        cEndUserID = DYNAMIC-FUNCTION("fGetDynParamValue","endUserID")
        cEndUserName = DYNAMIC-FUNCTION("fGetDynParamValue","endUserName")
        lAllPO = DYNAMIC-FUNCTION("fGetDynParamValue","allPO") EQ "YES"
        cStartPONumber = DYNAMIC-FUNCTION("fGetDynParamValue","StartPONumber")
        cEndPONumber = DYNAMIC-FUNCTION("fGetDynParamValue","endPONumber")
        lAllJobNo = DYNAMIC-FUNCTION("fGetDynParamValue","allJobNo") EQ "YES"
        cStartJobNo = DYNAMIC-FUNCTION("fGetDynParamValue","startJobNo")
        iStartJobNo2 = DYNAMIC-FUNCTION("fGetDynParamValue","startJobNo2")
        cEndJobNo = DYNAMIC-FUNCTION("fGetDynParamValue","endJobNo")
        iEndJobNo2 = DYNAMIC-FUNCTION("fGetDynParamValue","endJobNo2")
        lAllCAD = DYNAMIC-FUNCTION("fGetDynParamValue","allCAD") EQ "YES"
        cStartCAD = DYNAMIC-FUNCTION("fGetDynParamValue","startCAD")
        cEndCAD = DYNAMIC-FUNCTION("fGetDynParamValue","endCAD")
        lAllItemNo = DYNAMIC-FUNCTION("fGetDynParamValue","AllItemNo") EQ "YES"
        cStartItemNo = DYNAMIC-FUNCTION("fGetDynParamValue","startItemNo")
        cStartItemDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startItemDescription")
        cEndItemNo = DYNAMIC-FUNCTION("fGetDynParamValue","endItemNo")
        cEndItemDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endItemDescription")
        lAllSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","allSalesRep") EQ "YES"
        cStartSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","startSalesRep")
        cStartSalesRepName = DYNAMIC-FUNCTION("fGetDynParamValue","startSalesRepName")
        cEndSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","endSalesRep")
        cEndSalesRepName = DYNAMIC-FUNCTION("fGetDynParamValue","endSalesRepName")
        cJobStatus = DYNAMIC-FUNCTION("fGetDynParamValue","JobStatus")
        cOrderStatus = DYNAMIC-FUNCTION("fGetDynParamValue","OrderStatus")
        cWIPQty = DYNAMIC-FUNCTION("fGetDynParamValue","WIPQty")
        lDropOrderUnderRun = DYNAMIC-FUNCTION("fGetDynParamValue","DropOrderUnderRun") EQ "YES"
        lIncludeZeroQtyActReleaseQty = DYNAMIC-FUNCTION("fGetDynParamValue","IncludeZeroQtyActReleaseQty") EQ "YES"
        lIncludeZeroOrderBalanceItems = DYNAMIC-FUNCTION("fGetDynParamValue","IncludeZeroOrderBalanceItems") EQ "YES"
        lIncludeJobsQOH = DYNAMIC-FUNCTION("fGetDynParamValue","IncludeJobsQOH") EQ "YES"
        lIncludeZeroQtyWIPItems = DYNAMIC-FUNCTION("fGetDynParamValue","IncludeZeroQtyWIPItems") EQ "YES"
        lIncludeInactiveItems = DYNAMIC-FUNCTION("fGetDynParamValue","IncludeInactiveItems") EQ "YES"
        lUseReleaseDueDate = DYNAMIC-FUNCTION("fGetDynParamValue","UseReleaseDueDate") EQ "YES"
        lShowDetail = DYNAMIC-FUNCTION("fGetDynParamValue","ShowDetail") EQ "YES"
        .
END PROCEDURE.
