/*------------------------------------------------------------------------
  File:         AOA/dynBL/SalesTaxAuditRep.p
  Description:  Business Logic
  Author:       Sewa Singh
  Date Created: 5.22.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttSalesTaxAuditRep
DEFINE TEMP-TABLE ttSalesTaxAuditRep NO-UNDO
    FIELD cust-no        AS CHARACTER FORMAT "x(8)"       LABEL "Customer Code"
    FIELD cust-name      AS CHARACTER FORMAT "x(30)"      LABEL "Customer Name"
    FIELD cust-city      AS CHARACTER FORMAT "x(15)"      LABEL "Customer City"
    FIELD cust-state     AS CHARACTER FORMAT "x(2)"       LABEL "Customer State"
    FIELD cust-zip       AS CHARACTER FORMAT "x(15)"      LABEL "Customer Zip"
    FIELD cust-taxcode   AS CHARACTER FORMAT "x(10)"      LABEL "Cust Tax Code"
    FIELD cust-taxable   AS CHARACTER FORMAT "x(1)"       LABEL "Cust Taxable"
    FIELD tax-Resale     AS CHARACTER FORMAT "x(10)"      LABEL "Tax Resale #"
    FIELD resale-date    AS DATE      FORMAT "99/99/9999" LABEL "Resale Expiration"
    FIELD ship-id        AS CHARACTER FORMAT "x(10)"      LABEL "Ship to Id"         
    FIELD ship-name      AS CHARACTER FORMAT "x(30)"      LABEL "Ship to name"
    FIELD ship-city      AS CHARACTER FORMAT "x(15)"      LABEL "Ship to City"
    FIELD ship-state     AS CHARACTER FORMAT "x(2)"       LABEL "Ship to State"
    FIELD ship-zip       AS CHARACTER FORMAT "x(15)"      LABEL "Ship to Zip"
    FIELD ship-taxcode   AS CHARACTER FORMAT "x(10)"      LABEL "Ship to Tax Code"
    FIELD ship-taxable   AS LOGICAL   FORMAT "Yes/No"     LABEL "Ship to Taxable"
    FIELD ship-inactive  AS LOGICAL   FORMAT "Yes/No"     LABEL "Ship to Inactive"
    FIELD fgitem         AS CHARACTER FORMAT "x(15)"      LABEL "FG Item"
    FIELD fgdesc         AS CHARACTER FORMAT "x(30)"      LABEL "FG Item Description"
    FIELD fgitem-cust    AS CHARACTER FORMAT "x(15)"      LABEL "FG Item Customer"
    FIELD fgitem-taxable AS LOGICAL   FORMAT "Yes/No"     LABEL "FG Item Taxable"    
    FIELD prepitem       AS CHARACTER FORMAT "x(15)"      LABEL "Prep Item"
    FIELD prep-desc      AS CHARACTER FORMAT "x(30)"      LABEL "Prep Item Description"
    FIELD prep-cust      AS CHARACTER FORMAT "x(15)"      LABEL "Prep Item Customer"
    FIELD prep-taxable   AS LOGICAL   FORMAT "Yes/No"     LABEL "Prep Item Taxable"
    FIELD problem        AS CHARACTER FORMAT "x(70)"      LABEL "Problem"
    .

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 125
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic: 
    DEFINE VARIABLE lCustTaxAble AS LOGICAL NO-UNDO.
    
    FOR EACH cust NO-LOCK
        WHERE cust.company EQ cCompany
          AND cust.cust-no GE cStartCustNo
          AND cust.cust-no LE cEndCustNo
        :
        lCustTaxAble = cust.sort EQ "Y".
        IF cFGItemStatus NE "None" THEN
        FOR EACH itemfg NO-LOCK
            WHERE itemfg.company EQ cCompany
              AND itemfg.cust-no EQ cust.cust-no
              AND itemfg.i-no    GE cStartFGItem
              AND itemfg.i-no    LE cEndFGItem
            :
            IF cFGItemStatus EQ "Active" AND itemfg.stat NE "A" THEN
            NEXT. 
            IF itemfg.taxable NE lCustTaxAble THEN
            RUN pLogProblem ("FGItem", "FG Item Taxable <> Customer Taxable").
        END.
        IF cPrepStatus NE "None" THEN
        FOR EACH prep NO-LOCK
            WHERE prep.company EQ cCompany
              AND prep.cust-no EQ cust.cust-no
              AND prep.code    GE cStartPrepNo
              AND prep.code    LE cEndPrepNo
            :
            IF cPrepStatus EQ "Active" AND prep.inactive EQ YES THEN
            NEXT.
            IF prep.taxable NE lCustTaxAble THEN
            RUN pLogProblem ("Prep", "Prep Item Taxable <> Customer Taxable").
        END.    
        IF cShipToStatus NE "None" THEN DO:
            FOR EACH shipto NO-LOCK
                WHERE shipto.company EQ cCompany
                  AND shipto.cust-no EQ cust.cust-no
                :
                IF DYNAMIC-FUNCTION("IsActive",shipto.rec_key) EQ NO AND
                   cShipToStatus EQ "Active" THEN
                NEXT.
                FIND FIRST stax-group NO-LOCK
                     WHERE stax-group.tax-group EQ shipto.tax-code
                     NO-ERROR.
                IF NOT AVAILABLE stax-group AND Shipto.tax-mandatory THEN
                RUN pLogProblem ("ShipTo", "Ship to Tax code is not valid").
                IF Shipto.tax-mandatory AND shipto.tax-code EQ "" THEN
                RUN pLogProblem ("ShipTo", "Taxable ship to has no tax code").
                IF NOT lCustTaxAble AND Cust.tax-id EQ "" AND AVAILABLE ttSalesTaxAuditRep THEN
                RUN pLogProblem ("ShipTo", "Non Taxable customer without Resale certificate"). 
                IF NOT lCustTaxAble AND cust.date-field[2] LT TODAY + 30 AND AVAILABLE ttSalesTaxAuditRep THEN
                RUN pLogProblem ("ShipTo", "Resale expiration date issue").    
            END.
        END.
        IF cCustomerStatus NE "None" THEN DO:
            IF cCustomerStatus EQ "Active" AND cust.active NE "A" THEN
            NEXT.
            FIND FIRST stax-group NO-LOCK
                 WHERE stax-group.tax-group EQ cust.tax-gr
                 NO-ERROR.
            IF NOT AVAILABLE stax-group AND AVAILABLE ttSalesTaxAuditRep AND cust.tax-gr NE "" THEN
            RUN pLogProblem ("Cust", "Tax Group Defined is not valid").
            IF cust.tax-gr EQ "" AND lCustTaxAble AND AVAILABLE ttSalesTaxAuditRep THEN
            RUN pLogProblem ("Cust", "Taxable customer without tax code").
            IF NOT lCustTaxAble AND Cust.tax-id EQ "" AND AVAILABLE ttSalesTaxAuditRep THEN
            RUN pLogProblem ("Cust", "Non Taxable customer without Resale certificate"). 
            IF NOT lCustTaxAble AND cust.date-field[2] LT TODAY + 30 AND AVAILABLE ttSalesTaxAuditRep THEN
            RUN pLogProblem ("Cust", "Resale expiration date issue").
        END. /* if ccustomerstatus */       
    END. /* each cust */
END PROCEDURE.

PROCEDURE pCreateSalesTaxAudit:
    DEFINE INPUT PARAMETER ipcProblem AS CHARACTER NO-UNDO.

    CREATE ttSalesTaxAuditRep.
    ASSIGN
        ttSalesTaxAuditRep.cust-no      = cust.cust-no
        ttSalesTaxAuditRep.cust-name    = cust.name
        ttSalesTaxAuditRep.cust-city    = cust.city
        ttSalesTaxAuditRep.cust-state   = cust.state
        ttSalesTaxAuditRep.cust-zip     = cust.zip
        ttSalesTaxAuditRep.cust-taxcode = cust.tax-gr
        ttSalesTaxAuditRep.cust-taxable = cust.sort
        ttSalesTaxAuditRep.tax-Resale   = cust.tax-id
        ttSalesTaxAuditRep.resale-date  = cust.date-field[2]
        ttSalesTaxAuditRep.problem      = ipcProblem.
        .
END PROCEDURE.

PROCEDURE pLogProblem:
    DEFINE INPUT PARAMETER ipcType    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcProblem AS CHARACTER NO-UNDO.

    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    RUN pCreateSalesTaxAudit (ipcProblem).
    CASE ipcType:
        WHEN "FGItem" THEN
        ASSIGN
            ttSalesTaxAuditRep.fgitem         = itemfg.i-no
            ttSalesTaxAuditRep.fgdesc         = itemfg.i-name
            ttSalesTaxAuditRep.fgitem-cust    = itemfg.cust-no
            ttSalesTaxAuditRep.fgitem-taxable = itemfg.taxable
            .
        WHEN "Prep" THEN
        ASSIGN
            ttSalesTaxAuditRep.prepitem     = prep.i-no
            ttSalesTaxAuditRep.prep-desc    = prep.dscr
            ttSalesTaxAuditRep.prep-cust    = prep.cust-no
            ttSalesTaxAuditRep.prep-taxable = prep.taxable
            . 
        WHEN "ShipTo" THEN
        ASSIGN
            ttSalesTaxAuditRep.ship-id      = shipto.ship-id
            ttSalesTaxAuditRep.ship-name    = shipto.ship-name
            ttSalesTaxAuditRep.ship-city    = shipto.ship-city
            ttSalesTaxAuditRep.ship-state   = shipto.ship-state
            ttSalesTaxAuditRep.ship-zip     = shipto.ship-zip
            ttSalesTaxAuditRep.ship-taxcode = shipto.tax-code
            ttSalesTaxAuditRep.ship-taxable = shipto.tax-mandatory
            .
    END CASE.
END PROCEDURE.
