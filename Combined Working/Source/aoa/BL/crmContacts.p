/*------------------------------------------------------------------------
  File: crmContacts.p
  Description: CRM Contacts Business Logic
*/

/* ***************************  Definitions  ***************************/

/* CRM Contacts.rpa */
{CRM/ttCRMContacts.i}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttCRMContacts.
{aoa/includes/pCRMContacts.i}

DEFINE VARIABLE iRows AS INTEGER NO-UNDO.

RUN pZohoCRM (ipcCompany, "", OUTPUT iRows).
IF RETURN-VALUE NE "" THEN RETURN.

IF lAutoAdd OR lAutoUpdate THEN DO:
    FOR EACH ttCRMContacts:
        ASSIGN
            ttCRMContacts.xxApplyAction = NO
            ttCRMContacts.xxApplyAction = (lAutoUpdate AND ttCRMContacts.action EQ "Update") OR
                                           (lAutoAdd    AND ttCRMContacts.action EQ "Add")
            .
    END. /* each ttCRMContacts */
    RUN pApplyCRM.
    RUN pSave.
END. /* auto add or update */

{CRM/crmContacts.i}
