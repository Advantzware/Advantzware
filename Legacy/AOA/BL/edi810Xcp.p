/*------------------------------------------------------------------------
  File: edi810Xcp.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* EDI 810 Exception.rpa */
{AOA/tempTable/ttEDI810Exception.i}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttEDI810Exception.
{AOA/includes/pEDI810Exception.i}

/* local variables */
DEFINE VARIABLE cStatus     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cstatusDesc AS CHARACTER NO-UNDO.
DEFINE VARIABLE iRecCnt     AS INTEGER NO-UNDO.

/* subject business logic */
FUNCTION fCheckstatus RETURNS CHARACTER (ipiInvoiceNumber AS INTEGER, ipcPartner AS CHARACTER ):
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    
    /* Start with assuming No edi */
    cReturn = "NED".
    FOR EACH eddoc NO-LOCK
        WHERE eddoc.setID EQ "810"
          AND eddoc.partner EQ ipcPartner
          AND eddoc.docID EQ STRING(ipiInvoiceNumber)
        BY eddoc.addDate DESCENDING
        :
        cReturn = IF eddoc.c-FAdate EQ ? THEN "NAK"
                  ELSE eddoc.status-flag.
        LEAVE.
    END. /* each eddoc */
    RETURN cReturn.
END FUNCTION.

FUNCTION fGetStatDesc RETURNS CHARACTER (ipcStat AS CHARACTER):
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    
    CASE ipcStat:
        WHEN "NST" THEN 
        cReturn = "Sending Message Failed".
        WHEN "NAK" THEN 
        cReturn = "No Acknowledgment Received".
        WHEN "NED" THEN 
        cReturn = "No EDI Message Generated".
        WHEN "NEW" THEN 
        cReturn = "No EDI Message Generated".
        WHEN "SNT" THEN 
        cReturn = "Invoice Sent Successfully".
        OTHERWISE
        cReturn = "Unknown Error".
    END CASE.
    RETURN cReturn.
END FUNCTION.

FOR EACH company NO-LOCK,
    EACH edmast NO-LOCK 
    WHERE edmast.cust GE cStartCustNo
      AND edmast.cust LE cEndCustNo
      AND CAN-FIND(FIRST edcode
                   WHERE edcode.setID EQ "810"
                     AND edcode.partner EQ edmast.partnerGrp),
    FIRST cust NO-LOCK
    WHERE cust.company EQ company.company
      AND cust.cust-no EQ edmast.cust
    :              

    FOR EACH ar-inv NO-LOCK
        WHERE ar-inv.company EQ company.company
          AND ar-inv.inv-date GE dtStartInvoiceDate
          AND ar-inv.inv-date LE dtEndInvoiceDate
          AND ar-inv.cust-no EQ edmast.cust  
        USE-INDEX inv-date     
        :              
        cStatus = fCheckStatus(ar-inv.inv-no, edmast.partner).
        IF cStatus NE "SNT" OR NOT lExceptionOnly THEN DO:
            cStatusDesc = fGetStatDesc(cStatus).   
            RUN writeRecord (
                ar-inv.cust-no,
                cust.name,
                ar-inv.inv-no,
                ar-inv.inv-date,
                cStatusDesc
                ).
        END. /* if not snt */
    END. /* each ar-inv */
END. /* each company */

PROCEDURE writeRecord:
    DEFINE INPUT  PARAMETER ipcCust AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcName AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiInvoice AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdInvDate AS DATE NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStatDesc AS CHARACTER NO-UNDO.

    CREATE ttEDI810Exception.
    ASSIGN 
        iRecCnt = iRecCnt + 1
        ttEDI810Exception.custCode  = ipcCust 
        ttEDI810Exception.custName  = ipcName
        ttEDI810Exception.invoiceNo = ipiInvoice
        ttEDI810Exception.invoiceDate = ipdInvDate
        ttEDI810Exception.statusDescription = ipcStatDesc
        ttEDI810Exception.xxID = iRecCnt
        .
END PROCEDURE.
