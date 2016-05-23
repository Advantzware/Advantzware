/* aoaAppSrv/pOrdersBooked.i - auto generated 5.5.2016 from aoa/aoaParam.w */

    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBatch   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID  AS CHARACTER NO-UNDO.

    /* parameter values loaded into these variables */
    DEFINE VARIABLE lCustList             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lAllCustomers         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cStartCustNo          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartOrderDate      AS DATE      NO-UNDO.
    DEFINE VARIABLE cStartOrderDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndOrderDate        AS DATE      NO-UNDO.
    DEFINE VARIABLE cEndOrderDateOption   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllSalesReps         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cStartSalesRep        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndSalesRep          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllProdCategory      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cStartProdCategory    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndProdCategory      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lMiscChg              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lPageRep              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSetCom               AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lRepTot               AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lRelOrd               AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lUnder                AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iUnderValue           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lOver                 AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iOverValue            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cAvailableColumns     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns      AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-booked.", ipcUserID, ipiBatch).
    
    /* load parameter values from above record into variables */
    ASSIGN
        lCustList             = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllCustomers         = DYNAMIC-FUNCTION("fGetParamValue","svAllCustomers") EQ "yes"
        cStartCustNo          = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo            = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        dtStartOrderDate      = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartOrderDate"))
        cStartOrderDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartOrderDateOption")
        dtStartOrderDate      = DYNAMIC-FUNCTION("fDateOptionDate",cStartOrderDateOption,dtStartOrderDate)
        dtEndOrderDate        = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndOrderDate"))
        cEndOrderDateOption   = DYNAMIC-FUNCTION("fGetParamValue","svEndOrderDateOption")
        dtEndOrderDate        = DYNAMIC-FUNCTION("fDateOptionDate",cEndOrderDateOption,dtEndOrderDate)
        lAllSalesReps         = DYNAMIC-FUNCTION("fGetParamValue","svAllSalesReps") EQ "yes"
        cStartSalesRep        = DYNAMIC-FUNCTION("fGetParamValue","svStartSalesRep")
        cEndSalesRep          = DYNAMIC-FUNCTION("fGetParamValue","svEndSalesRep")
        lMiscChg              = DYNAMIC-FUNCTION("fGetParamValue","svMiscChg") EQ "yes"
        lPageRep              = DYNAMIC-FUNCTION("fGetParamValue","svPageRep") EQ "yes"
        lSetCom               = DYNAMIC-FUNCTION("fGetParamValue","svSetCom") EQ "yes"
        lRepTot               = DYNAMIC-FUNCTION("fGetParamValue","svRepTot") EQ "yes"
        lRelOrd               = DYNAMIC-FUNCTION("fGetParamValue","svRelOrd") EQ "yes"
        lUnder                = DYNAMIC-FUNCTION("fGetParamValue","svUnder") EQ "yes"
        iUnderValue           = DYNAMIC-FUNCTION("fGetParamValue","svUnderValue")
        lOver                 = DYNAMIC-FUNCTION("fGetParamValue","svOver") EQ "yes"
        iOverValue            = DYNAMIC-FUNCTION("fGetParamValue","svOverValue")
        cAvailableColumns     = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns      = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttOrdersBooked:HANDLE,
                     cAvailableColumns,
                     cSelectedColumns
                     ).

    IF lAllCustomers THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(255)
        .
    IF lAllSalesReps THEN
    ASSIGN
        cStartSalesRep = CHR(32)
        cEndSalesRep   = CHR(255)
        .
    IF lAllProdCategory THEN
    ASSIGN
        cStartProdCategory = CHR(32)
        cEndProdCategory   = CHR(255)
        .

    RUN pBuildCustList (ipcCompany, lCustList, cStartCustNo, cEndCustNo, "OR5").

