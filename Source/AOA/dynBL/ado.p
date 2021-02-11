/*------------------------------------------------------------------------
  File:         ado.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 11.15.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttCompare
DEFINE TEMP-TABLE ttCompare NO-UNDO
    FIELD cType        AS CHARACTER LABEL "Type"
    FIELD cTable       AS CHARACTER LABEL "Table"        FORMAT "x(20)"
    FIELD cField       AS CHARACTER LABEL "Field"        FORMAT "x(30)"
    FIELD cKeyValue    AS CHARACTER LABEL "Key Value"    FORMAT "x(40)"
    FIELD cBeforeValue AS CHARACTER LABEL "Before Value" FORMAT "x(40)"
    FIELD cAfterValue  AS CHARACTER LABEL "After Value"  FORMAT "x(40)"
        INDEX ttCompare IS PRIMARY
            cTable
            cField
            .
DEFINE TEMP-TABLE ttCust   NO-UNDO LIKE cust.
DEFINE TEMP-TABLE ttItemFG NO-UNDO LIKE itemfg.
DEFINE TEMP-TABLE ttTerms  NO-UNDO LIKE terms.

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 5154
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompare    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hConnection AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hFields     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hRecordSet  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hTable      AS HANDLE     NO-UNDO EXTENT 2.
DEFINE VARIABLE lNoChange   AS LOGICAL    NO-UNDO.

/* **********************  Internal Functions  ************************ */

FUNCTION fCheckUnknown RETURNS CHARACTER (ipcValue AS CHARACTER):
    /* don't allow any null values */
    RETURN IF ipcValue EQ ? THEN "" ELSE ipcValue.
END FUNCTION.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    /* create ActiveX Data Object connection */
    CREATE "ADODB.Connection.6.0" hConnection.
    hConnection:ConnectionString = "{AOA/dynBL/IndepedentII.i}".
    /* open ADO connection */
    hConnection:Open (,,,) NO-ERROR.
    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN DO:
        CREATE ttCompare.
        ASSIGN
            ttCompare.cType     = "ERROR"
            ttCompare.cTable    = "ADO Open Failed"
            ttCompare.cField    = "Contact System Administrator"
            ttCompare.cKeyValue = "for Assistance"
            .
        RETURN.
    END. /* if error */
    /* create record set connection */
    CREATE "ADODB.Recordset.6.0" hRecordSet.
    hRecordSet:LockType = 1. /* read only */
    IF lTerms     THEN RUN pADO ("Terms").
    IF lCustomers THEN RUN pADO ("Cust").
    IF lFGItems   THEN RUN pADO ("ItemFG").    
    hConnection:Close ().
    RELEASE OBJECT hRecordSet.
    RELEASE OBJECT hConnection.

END PROCEDURE.

PROCEDURE pADO:
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cSelect AS CHARACTER  NO-UNDO.

    EMPTY TEMP-TABLE ttCompare.
    /* set sql select statement */
    RUN pSetSelect (ipcType, OUTPUT cSelect).
    /* open record set */
    hRecordSet:Open (cSelect,hConnection,1,1,).
    /* grab all record set fields */
    hFields = hRecordSet:Fields.
    /* read all record set rows */
    DO WHILE TRUE:
        IF hRecordSet:EOF THEN LEAVE.
        CASE ipcType:
            WHEN "Cust" THEN
            RUN pCreatettCust.
            WHEN "ItemFG" THEN
            RUN pCreatettItemFG.
            WHEN "Terms" THEN
            RUN pCreatettTerms.
        END CASE.
        hRecordSet:MoveNext.
    END. /* do while */
    CASE ipcType:
        WHEN "Cust" THEN
        RUN pCust.
        WHEN "ItemFG" THEN
        RUN pItemFG.
        WHEN "Terms" THEN
        RUN pTerms.
    END CASE.
    /* close record set */
    hRecordSet:Close ().

END PROCEDURE.

PROCEDURE pCompare:
    DEFINE INPUT  PARAMETER ipcTable    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcKeyField AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplNoChange AS LOGICAL   NO-UNDO INITIAL YES.

    DEFINE VARIABLE cValue AS CHARACTER NO-UNDO EXTENT 2.
    DEFINE VARIABLE hField AS HANDLE    NO-UNDO EXTENT 2.
    DEFINE VARIABLE idx    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE kdx    AS INTEGER   NO-UNDO.

    IF cCompare EQ "" THEN RETURN.
    oplNoChange = NO.
    DO idx = 1 TO NUM-ENTRIES(cCompare):
        /* skip fields by table excluded from compare */
        CASE ENTRY(idx,cCompare):
            WHEN "date-field" THEN
                IF ipcTable EQ "cust" THEN NEXT.
            WHEN "modifiedBy" OR
            WHEN "modifiedDate" THEN
                IF ipcTable EQ "itemfg" THEN NEXT.
            WHEN "rec_key" THEN
                NEXT.
        END CASE.
        ASSIGN
            hField[1] = hTable[1]:BUFFER-FIELD(ENTRY(idx,cCompare))
            hField[2] = hTable[2]:BUFFER-FIELD(ENTRY(idx,cCompare))
            jdx       = IF hField[1]:EXTENT GT 0 THEN 1 ELSE 0
            .
        DO kdx = jdx TO hField[1]:EXTENT:
            ASSIGN
                cValue[1] = hField[1]:BUFFER-VALUE(kdx)
                cValue[2] = hField[2]:BUFFER-VALUE(kdx)
                .
            RUN pCreatettCompare (
                "Update",
                hTable[1]:NAME,
                hField[1]:NAME,
                ipcKeyField,
                cValue[1],
                cValue[2]
                ).
        END. /* do kdx */
    END. /* do idx */

END PROCEDURE.

PROCEDURE pCreatettCompare:
    DEFINE INPUT PARAMETER ipcType     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTable    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcField    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcKeyField AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcBefore   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAfter    AS CHARACTER NO-UNDO.

    CREATE ttCompare.
    ASSIGN
        ttCompare.cType        = ipcType
        ttCompare.cTable       = ipcTable
        ttCompare.cField       = ipcField
        ttCompare.cKeyValue    = ipcKeyField
        ttCompare.cBeforeValue = ipcBefore
        ttCompare.cAfterValue  = ipcAfter
        .

END PROCEDURE.

PROCEDURE pCreatettCust:
    CREATE ttCust.
    ASSIGN
        ttCust.addr[1]       = fCheckUnknown(hFields:Item("csaddr1"):Value)
        ttCust.addr[2]       = fCheckUnknown(hFields:Item("csaddr2"):Value)
        ttCust.carrier       = "OurTr"
        ttCust.cc-expiration = 12/31/2049
        ttCust.city          = fCheckUnknown(hFields:Item("cscity"):Value)
        ttCust.company       = cCompany
        ttCust.cust-no       = hFields:Item("cscode"):Value
        ttCust.country       = "USA"
        ttCust.del-zone      = "All"
        ttCust.fob-code      = "DEST"
        ttCust.frt-pay       = "P"
        ttCust.inv-meth      = NO
        ttCust.loc           = cLocation
        ttCust.name          = hFields:Item("csname"):Value
        ttCust.sman          = hFields:Item("sacode"):Value
        ttCust.sort          = "Y"
        ttCust.state         = fCheckUnknown(hFields:Item("csst"):Value)
        ttCust.tax-gr        = "NT"
        ttCust.terms         = hFields:Item("termcode"):Value
        ttCust.type          = ENTRY(LOOKUP(hFields:Item("cstype"):Value,"A,I,P"),
                                            "Active,Inactive,Prospect")
        ttCust.zip           = fCheckUnknown(hFields:Item("cszip"):Value)
        .

END PROCEDURE.

PROCEDURE pCreatettItemFG:
    CREATE ttItemFG.
    ASSIGN
        ttItemFG.case-count  = 1
        ttItemFG.case-pall   = 1
        ttItemFG.company     = cCompany
        ttItemFG.curr-code   = "USD"
        ttItemFG.cust-name   = hFields:Item("csname"):Value
        ttItemFG.cust-no     = hFields:Item("cscode"):Value
        ttItemFG.def-loc     = cLocation
        ttItemFG.def-loc-bin = "FLOOR"
        ttItemFG.i-code      = "C"
        ttItemFG.i-name      = hFields:Item("cust_ident"):Value
        ttItemFG.i-no        = hFields:Item("item_no"):Value
        ttItemFG.part-no     = ttItemFG.i-name
        ttItemFG.procat      = "BOXES"
        ttItemFG.prod-code   = "NEW"
        ttItemFG.prod-uom    = "M"
        ttItemFG.pur-uom     = "M"
        ttItemFG.sell-uom    = "M"
        ttItemFG.stackHeight = 1
        ttItemFG.stat        = "A"
        ttItemFG.type-code   = "O"
        ttItemFG.weight-100  = 1
        .
END PROCEDURE.

PROCEDURE pCreatettTerms:
    CREATE ttTerms.
    ASSIGN
        ttTerms.company   = cCompany
        ttTerms.disc-days = hFields:Item("termdisdays"):Value
        ttTerms.disc-rate = hFields:Item("termdisrate"):Value
        ttTerms.dscr      = hFields:Item("termname"):Value
        ttTerms.net-days  = hFields:Item("termnet"):Value
        ttTerms.t-code    = hFields:Item("termcode"):Value
        ttTerms.type      = "S"
        .

END PROCEDURE.

{AOA/dynBL/ado.i
    &table="cust"
    &keyField="cust-no"
    &dscrField="name"
    }

{AOA/dynBL/ado.i
    &table="itemfg"
    &keyField="i-no"
    &dscrField="i-name"
    }

{AOA/dynBL/ado.i
    &table="terms"
    &keyField="t-code"
    &dscrField="dscr"
    }

PROCEDURE pSetSelect:
    DEFINE INPUT  PARAMETER ipcType   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSelect AS CHARACTER NO-UNDO.

    /* define sql section statement */
    CASE ipcType:
        WHEN "Cust" THEN
        opcSelect = "Select "
                  + "Customer.cscode, "
                  + "Customer.csname, "
                  + "Customer.csaddr1, "
                  + "Customer.csaddr2, "
                  + "Customer.cscity, "
                  + "Customer.csst, "
                  + "Customer.cszip, "
                  + "Customer.cstype, "
                  + "Customer.sacode, "
                  + "Customer.termcode "  
                  + "from Customer"
                  .
        WHEN "ItemFG" THEN
        opcSelect = "Select "
                  + "Spec_File.item_no, "
                  + "Spec_File.cscode, "
                  + "Spec_File.cust_ident, "
                  + "Customer.csname "
                  + "from Spec_File "
                  + "Inner Join Customer Customer on Spec_File.cscode=Customer.cscode"
                  .
        WHEN "Terms" THEN
        opcSelect = "Select "
                  + "Terms.termcode, "
                  + "Terms.termname, "
                  + "Terms.termdisdays, "
                  + "Terms.termdisrate, "
                  + "Terms.termnet "
                  + "from Terms"
                  .
    END CASE.

END PROCEDURE.
