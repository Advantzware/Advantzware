/*------------------------------------------------------------------------
  File:         r-invprtoe.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 7.15.2021
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttFormHeader
{AOA/tempTable/ttFormTempTables.i}

{sys/ref/CustList.i NEW}

RUN spSetSessionParam ("DetailTables", "1").
RUN spSetSessionParam ("DetailHandle1", TEMP-TABLE ttFormDetail:HANDLE).
RUN spSetSessionParam ("SummaryTables", "1").
RUN spSetSessionParam ("SummaryHandle1", TEMP-TABLE ttFormSummary:HANDLE).

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 176
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE iRecordID AS INTEGER NO-UNDO.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE hInvoiceProcs AS HANDLE NO-UNDO.

    RUN ar/InvoiceProcs.p PERSISTENT SET hInvoiceProcs.

    FOR EACH inv-head NO-LOCK
        WHERE inv-head.company EQ cCompany
          AND inv-head.inv-no  GE iStartInvNo
          AND inv-head.inv-no  LE iEndInvNo
          AND inv-head.cust-no GE cStartCustNo
          AND inv-head.cust-no LE cEndCustNo
          AND inv-head.r-no    GE iStartInvoiceID
          AND inv-head.r-no    LE iEndInvoiceID
        :
        IF lCustList AND
           NOT CAN-FIND(FIRST ttCustList
                        WHERE ttCustList.cust-no EQ inv-head.cust-no
                          AND ttCustList.log-fld EQ YES) THEN
        NEXT.
        // additional logic found in oerep/r-invprtProc.i procedure build-list1
        RUN BuildData IN hInvoiceProcs (
            ROWID(inv-head),
            OUTPUT TABLE ttInv BY-REFERENCE,
            OUTPUT TABLE ttinvLine BY-REFERENCE,
            OUTPUT TABLE ttTaxDetail BY-REFERENCE
            ).
        IF AVAILABLE ttInv THEN DO:
            CREATE ttFormHeader.
            BUFFER-COPY ttInv TO ttFormHeader.
            ASSIGN
                iRecordID                   = iRecordID + 1
                ttFormHeader.recordID       = iRecordID
                ttFormHeader.emailKeyValues = "r-invprt.|"
                                            + ttFormHeader.customerID + "|"
                                            + ttFormHeader.shiptoID
                .
            FOR EACH ttInvLine:
                CREATE ttFormDetail.
                BUFFER-COPY ttInvLine TO ttFormDetail.
                ttFormDetail.recordID = iRecordID.
            END. /* each ttInvLine */
            FOR EACH ttTaxDetail:
                CREATE ttFormSummary.
                BUFFER-COPY ttTaxDetail TO ttFormSummary.
                ttFormSummary.recordID = iRecordID.
            END. /* each ttInvoiceTax */
        END. /* if avail */
    END. /* each inv-head */

END PROCEDURE.

{AOA/dynBL/pGetCustInfo.i}
