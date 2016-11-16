/* pFinishedGoodsExport.i - auto generated 11.03.2016 @  8:29:00 pm from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE lAllItemNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllItemName AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartItemName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndItemName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllCustPart AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCustPart AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustPart AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllEstimate AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartEstimate AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndEstimate AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllStyle AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartStyle AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndStyle AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllProdCategory AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartProdCategory AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndProdCategory AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lInactive AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSpecNote AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "rd-fgexp.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        lAllItemNo = DYNAMIC-FUNCTION("fGetParamValue","svAllItemNo") EQ "yes"
        cStartItemNo = DYNAMIC-FUNCTION("fGetParamValue","svStartItemNo")
        cEndItemNo = DYNAMIC-FUNCTION("fGetParamValue","svEndItemNo")
        lAllItemName = DYNAMIC-FUNCTION("fGetParamValue","svAllItemName") EQ "yes"
        cStartItemName = DYNAMIC-FUNCTION("fGetParamValue","svStartItemName")
        cEndItemName = DYNAMIC-FUNCTION("fGetParamValue","svEndItemName")
        lAllCustPart = DYNAMIC-FUNCTION("fGetParamValue","svAllCustPart") EQ "yes"
        cStartCustPart = DYNAMIC-FUNCTION("fGetParamValue","svStartCustPart")
        cEndCustPart = DYNAMIC-FUNCTION("fGetParamValue","svEndCustPart")
        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        lAllEstimate = DYNAMIC-FUNCTION("fGetParamValue","svAllEstimate") EQ "yes"
        cStartEstimate = DYNAMIC-FUNCTION("fGetParamValue","svStartEstimate")
        cEndEstimate = DYNAMIC-FUNCTION("fGetParamValue","svEndEstimate")
        lAllStyle = DYNAMIC-FUNCTION("fGetParamValue","svAllStyle") EQ "yes"
        cStartStyle = DYNAMIC-FUNCTION("fGetParamValue","svStartStyle")
        cEndStyle = DYNAMIC-FUNCTION("fGetParamValue","svEndStyle")
        lAllProdCategory = DYNAMIC-FUNCTION("fGetParamValue","svAllProdCategory") EQ "yes"
        cStartProdCategory = DYNAMIC-FUNCTION("fGetParamValue","svStartProdCategory")
        cEndProdCategory = DYNAMIC-FUNCTION("fGetParamValue","svEndProdCategory")
        lActive = DYNAMIC-FUNCTION("fGetParamValue","svActive") EQ "yes"
        lInactive = DYNAMIC-FUNCTION("fGetParamValue","svInactive") EQ "yes"
        lSpecNote = DYNAMIC-FUNCTION("fGetParamValue","svSpecNote") EQ "yes"
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttFinishedGoodsExport:HANDLE, cAvailableColumns, cSelectedColumns).

    IF lAllItemNo THEN
    ASSIGN
        cStartItemNo = CHR(32)
        cEndItemNo   = CHR(254)
        .

    IF lAllItemName THEN
    ASSIGN
        cStartItemName = CHR(32)
        cEndItemName   = CHR(254)
        .

    IF lAllCustPart THEN
    ASSIGN
        cStartCustPart = CHR(32)
        cEndCustPart   = CHR(254)
        .

    IF lAllCustNo THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(254)
        .

    IF lAllEstimate THEN
    ASSIGN
        cStartEstimate = CHR(32)
        cEndEstimate   = CHR(254)
        .

    IF lAllStyle THEN
    ASSIGN
        cStartStyle = CHR(32)
        cEndStyle   = CHR(254)
        .

    IF lAllProdCategory THEN
    ASSIGN
        cStartProdCategory = CHR(32)
        cEndProdCategory   = CHR(254)
        .

    IF lCustList THEN
    RUN pBuildCustList (ipcCompany, "IF1", OUTPUT cStartCustNo, OUTPUT cEndCustNo, OUTPUT lCustList).
