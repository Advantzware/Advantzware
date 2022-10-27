/*------------------------------------------------------------------------
  File:         fgBalBin.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 9.20.2022
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttFGBalBin
DEFINE TEMP-TABLE ttFGBalBin NO-UNDO
    FIELD reportComplete AS CHARACTER.

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 214
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

{sys/inc/var.i NEW SHARED}

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    FOR EACH itemfg NO-LOCK
        WHERE itemfg.company EQ cCompany
          AND itemfg.cust-no GE cStartCustNo
          AND itemfg.cust-no LE cEndCustNo
          AND itemfg.i-no    GE cStartFGItem
          AND itemfg.i-no    LE cEndFGItem
          AND (itemfg.stat EQ "A" OR lIncludeInavtiveItems)
        USE-INDEX customer
        :
        IF itemfg.i-no EQ "" THEN NEXT.
        IF itemfg.stat NE "A" AND lIncludeInavtiveItems EQ NO THEN NEXT.

        IF lRebuildAllFromHistory THEN
        DO TRANSACTION:
            RUN fg/fg-mkbin.p (RECID(itemfg)).
        END.

        IF lDeleteZeroBins THEN
        FOR EACH fg-bin EXCLUSIVE-LOCK
            WHERE fg-bin.company EQ itemfg.company
              AND fg-bin.i-no    EQ itemfg.i-no
              AND fg-bin.qty     EQ 0
            :
            DELETE fg-bin.
        END.

        IF lDeleteNegativeBins THEN
        FOR EACH fg-bin EXCLUSIVE-LOCK
            WHERE fg-bin.company EQ itemfg.company
              AND fg-bin.i-no    EQ itemfg.i-no
              AND fg-bin.qty     LT 0
            :
            RUN fg/cre-pchr.p (ROWID(fg-bin), "C", 0, 0,"").
            DELETE fg-bin.
        END.

        IF lRebuildAllFromHistory THEN DO TRANSACTION:
            RUN fg/fg-reset.p (RECID(itemfg)).  
        END.

    END. /* each itemfg */
    CREATE ttFGBalBin.
    ttFGBalBin.reportComplete = "FG Bin Balance and Reset Completed.".

END PROCEDURE.
