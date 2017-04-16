/* pPostBOLCreateInvoice.i - auto generated 11.03.2016 @ 12:48:02 am from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtPostDate AS DATE NO-UNDO.
    DEFINE VARIABLE cPostDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartBOLDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartBOLDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndBOLDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndBOLDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllBOL AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iStartBOL AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEndBOL AS INTEGER NO-UNDO.
    DEFINE VARIABLE lAllLoc AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartLoc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndLoc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllLocBin AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartLocBin AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndLocBin AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPost AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-bolpst.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        cLocation = DYNAMIC-FUNCTION("fGetParamValue","svLocation")
        dtPostDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svPostDate"))
        cPostDateOption = DYNAMIC-FUNCTION("fGetParamValue","svPostDateOption")
        dtPostDate = DYNAMIC-FUNCTION("fDateOptionDate",cPostDateOption,dtPostDate)
        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        dtStartBOLDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartBOLDate"))
        cStartBOLDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartBOLDateOption")
        dtStartBOLDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartBOLDateOption,dtStartBOLDate)
        dtEndBOLDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndBOLDate"))
        cEndBOLDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndBOLDateOption")
        dtEndBOLDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndBOLDateOption,dtEndBOLDate)
        lAllBOL = DYNAMIC-FUNCTION("fGetParamValue","svAllBOL") EQ "yes"
        iStartBOL = DYNAMIC-FUNCTION("fGetParamValue","svStartBOL")
        iEndBOL = DYNAMIC-FUNCTION("fGetParamValue","svEndBOL")
        lAllLoc = DYNAMIC-FUNCTION("fGetParamValue","svAllLoc") EQ "yes"
        cStartLoc = DYNAMIC-FUNCTION("fGetParamValue","svStartLoc")
        cEndLoc = DYNAMIC-FUNCTION("fGetParamValue","svEndLoc")
        lAllLocBin = DYNAMIC-FUNCTION("fGetParamValue","svAllLocBin") EQ "yes"
        cStartLocBin = DYNAMIC-FUNCTION("fGetParamValue","svStartLocBin")
        cEndLocBin = DYNAMIC-FUNCTION("fGetParamValue","svEndLocBin")
        lPost = DYNAMIC-FUNCTION("fGetParamValue","svPost") EQ "yes"
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttPostBOLCreateInvoice:HANDLE, cAvailableColumns, cSelectedColumns).

    IF lAllCustNo THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(254)
        .

    IF lAllBOL THEN
    ASSIGN
        iStartBOL = 0
        iEndBOL   = 99999999
        .

    IF lAllLoc THEN
    ASSIGN
        cStartLoc = CHR(32)
        cEndLoc   = CHR(254)
        .

    IF lAllLocBin THEN
    ASSIGN
        cStartLocBin = CHR(32)
        cEndLocBin   = CHR(254)
        .

    IF lCustList THEN
    RUN pBuildCustList (ipcCompany, "OS5", OUTPUT cStartCustNo, OUTPUT cEndCustNo, OUTPUT lCustList).
