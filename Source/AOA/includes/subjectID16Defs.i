/* subjectID16Defs.i - auto generated 07.06.2021 @  7:13:09 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtPostDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPeriod AS INTEGER NO-UNDO.
DEFINE VARIABLE lAllVendNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartVendNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartVendorName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndVendNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndVendName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllUserID AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartUserID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartUserName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndUserID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndUserName AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartInvoiceDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndInvoiceDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPostOutOfPeriod AS LOGICAL NO-UNDO.
DEFINE VARIABLE lPostIntoClosedPeriod AS LOGICAL NO-UNDO.
DEFINE VARIABLE lPost AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        lSecure = DYNAMIC-FUNCTION("fGetDynParamValue","secure") EQ "YES"
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cLocation = DYNAMIC-FUNCTION("fGetDynParamValue","location")
        dtPostDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","postDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtPostDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtPostDate)
        iPeriod = DYNAMIC-FUNCTION("fGetDynParamValue","period")
        lAllVendNo = DYNAMIC-FUNCTION("fGetDynParamValue","allVendNo") EQ "YES"
        cStartVendNo = DYNAMIC-FUNCTION("fGetDynParamValue","startVendNo")
        cStartVendorName = DYNAMIC-FUNCTION("fGetDynParamValue","startVendorName")
        cEndVendNo = DYNAMIC-FUNCTION("fGetDynParamValue","endVendNo")
        cEndVendName = DYNAMIC-FUNCTION("fGetDynParamValue","endVendName")
        lAllUserID = DYNAMIC-FUNCTION("fGetDynParamValue","allUserID") EQ "YES"
        cStartUserID = DYNAMIC-FUNCTION("fGetDynParamValue","startUserID")
        cStartUserName = DYNAMIC-FUNCTION("fGetDynParamValue","startUserName")
        cEndUserID = DYNAMIC-FUNCTION("fGetDynParamValue","endUserID")
        cEndUserName = DYNAMIC-FUNCTION("fGetDynParamValue","endUserName")
        dtStartInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startInvoiceDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtStartInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtStartInvoiceDate)
        dtEndInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endInvoiceDate"))
        cDatePickList-3 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-3")
        dtEndInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-3,dtEndInvoiceDate)
        lPostOutOfPeriod = DYNAMIC-FUNCTION("fGetDynParamValue","postOutOfPeriod") EQ "YES"
        lPostIntoClosedPeriod = DYNAMIC-FUNCTION("fGetDynParamValue","postIntoClosedPeriod") EQ "YES"
        lPost = DYNAMIC-FUNCTION("fGetDynParamValue","post") EQ "YES"
        .
END PROCEDURE.
