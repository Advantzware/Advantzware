/* subjectID165Defs.i - auto generated 03.09.2021 @  8:53:39 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE lAllCompany AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtAsOfDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWhichDate AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPeriodDays1 AS INTEGER NO-UNDO.
DEFINE VARIABLE iPeriodDays2 AS INTEGER NO-UNDO.
DEFINE VARIABLE iPeriodDays3 AS INTEGER NO-UNDO.
DEFINE VARIABLE iPeriodDays4 AS INTEGER NO-UNDO.
DEFINE VARIABLE lElectronic AS LOGICAL NO-UNDO.
DEFINE VARIABLE lNonElectronic AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAllVendNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartVendNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartVendorName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndVendNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndVendName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllVendorTypes AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartVendorType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartVendorTypeDescr AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndVendorType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndVendorTypeDescr AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCurrency AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCurrency AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCurrencyDscr AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCurrency AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCurrencyDscr AS CHARACTER NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        lAllCompany = DYNAMIC-FUNCTION("fGetDynParamValue","allCompany") EQ "YES"
        cStartCompany = DYNAMIC-FUNCTION("fGetDynParamValue","startCompany")
        cStartDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startDescription")
        cEndCompany = DYNAMIC-FUNCTION("fGetDynParamValue","endCompany")
        cEndDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endDescription")
        dtAsOfDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","asOfDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtAsOfDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtAsOfDate)
        cWhichDate = DYNAMIC-FUNCTION("fGetDynParamValue","WhichDate")
        iPeriodDays1 = DYNAMIC-FUNCTION("fGetDynParamValue","PeriodDays1")
        iPeriodDays2 = DYNAMIC-FUNCTION("fGetDynParamValue","PeriodDays2")
        iPeriodDays3 = DYNAMIC-FUNCTION("fGetDynParamValue","PeriodDays3")
        iPeriodDays4 = DYNAMIC-FUNCTION("fGetDynParamValue","PeriodDays4")
        lElectronic = DYNAMIC-FUNCTION("fGetDynParamValue","Electronic") EQ "YES"
        lNonElectronic = DYNAMIC-FUNCTION("fGetDynParamValue","NonElectronic") EQ "YES"
        lAllVendNo = DYNAMIC-FUNCTION("fGetDynParamValue","allVendNo") EQ "YES"
        cStartVendNo = DYNAMIC-FUNCTION("fGetDynParamValue","startVendNo")
        cStartVendorName = DYNAMIC-FUNCTION("fGetDynParamValue","startVendorName")
        cEndVendNo = DYNAMIC-FUNCTION("fGetDynParamValue","endVendNo")
        cEndVendName = DYNAMIC-FUNCTION("fGetDynParamValue","endVendName")
        lAllVendorTypes = DYNAMIC-FUNCTION("fGetDynParamValue","AllVendorTypes") EQ "YES"
        cStartVendorType = DYNAMIC-FUNCTION("fGetDynParamValue","startVendorType")
        cStartVendorTypeDescr = DYNAMIC-FUNCTION("fGetDynParamValue","startVendorTypeDescr")
        cEndVendorType = DYNAMIC-FUNCTION("fGetDynParamValue","endVendorType")
        cEndVendorTypeDescr = DYNAMIC-FUNCTION("fGetDynParamValue","endVendorTypeDescr")
        lAllCurrency = DYNAMIC-FUNCTION("fGetDynParamValue","AllCurrency") EQ "YES"
        cStartCurrency = DYNAMIC-FUNCTION("fGetDynParamValue","startCurrency")
        cStartCurrencyDscr = DYNAMIC-FUNCTION("fGetDynParamValue","startCurrencyDscr")
        cEndCurrency = DYNAMIC-FUNCTION("fGetDynParamValue","endCurrency")
        cEndCurrencyDscr = DYNAMIC-FUNCTION("fGetDynParamValue","endCurrencyDscr")
        .
END PROCEDURE.
