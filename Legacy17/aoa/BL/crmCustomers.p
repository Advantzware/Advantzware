/*------------------------------------------------------------------------
  File: crmCustomers.p
  Description: CRM Customers Business Logic
*/

/* ***************************  Definitions  ***************************/

/* CRM Customers.rpa */
{CRM/ttCRMCustomers.i}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttCRMCustomers.
{aoa/includes/pCRMCustomers.i}

DEFINE VARIABLE iRows AS INTEGER NO-UNDO.

RUN pZohoCRM (ipcCompany, OUTPUT iRows).
IF RETURN-VALUE NE "" THEN DO:
    MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
    RETURN RETURN-VALUE.
END.

IF lAutoAdd OR lAutoUpdate THEN DO:
    FOR EACH ttCRMCustomers:
        ASSIGN
            ttCRMCustomers.xxApplyAction = NO
            ttCRMCustomers.xxApplyAction = (lAutoUpdate AND ttCRMCustomers.action EQ "Update") OR
                                           (lAutoAdd    AND ttCRMCustomers.action EQ "Add")
            .
    END. /* each ttCRMCustomers */
    RUN pApplyCRM.
    RUN pSave.
END. /* auto add or update */

{CRM/crmCustomers.i}
