
/*------------------------------------------------------------------------
    File        : LancoYorkCompanyMerge.p
    Purpose     : 
    Syntax      :
    Description : Based on BV's DataUtilityProcs.p but HIGHLY modified for LY 
    Author(s)   : MYT
    Created     : Sat June 1, 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF STREAM logStream.

DEF VAR iCtr AS INT NO-UNDO.
DEF VAR cConvTypeList AS CHAR NO-UNDO INITIAL 
    "Loc,BankAcct,Account,Carrier,check-No,Est-no,inv-no,Item,ItemFg,Job,M-code,ra-no,Release#,Loadtag,Company".
    

DEF TEMP-TABLE ttFullTableList
    FIELD cTable          AS CHAR 
    FIELD cTableDesc      AS CHAR
    FIELD cAllIndexFields AS CHAR
    FIELD cUIndexFields   AS CHAR
    FIELD iRecordCount    AS INT 
    FIELD lConvert        AS LOG 
    .

DEF TEMP-TABLE ttTablesWithMergeFields
    FIELD cFieldType AS CHAR FORMAT "x(12)"
    FIELD cTableName AS CHAR FORMAT "x(24)"
    FIELD cFieldName AS CHAR FORMAT "x(24)"
    . 
    
DEF TEMP-TABLE ttNewCoA
    FIELD fromCompany AS CHAR 
    FIELD fromAcct    AS CHAR 
    FIELD toCompany   AS CHAR 
    FIELD toAcct      AS CHAR 
    FIELD AcctDesc    AS CHAR 
    .
    

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetNextRecKey RETURNS CHARACTER PRIVATE
    (  ) FORWARD.


/* ***************************  Main Block  *************************** */
DEF BUFFER bar-ledger FOR ar-ledger.    
DEF BUFFER boe-reth FOR oe-reth.  
DEF BUFFER boe-retl FOR oe-retl.  

RUN pStatus("Initialize").

RUN pBuildFullTableList.
RUN pBuildFieldLists.
RUN pOutputPreMerge.

RUN pConsolidateAmounts.
RUN pLoadCoAFromCSV ("c:\tmp\LYAccountConversion.csv").
RUN pBuildNewCoA.

DISABLE TRIGGERS FOR LOAD OF oe-reth.
DISABLE TRIGGERS FOR LOAD OF oe-retl.
DISABLE TRIGGERS FOR LOAD OF ar-ledger.
DISABLE TRIGGERS FOR LOAD OF ar-cash.
DISABLE TRIGGERS FOR LOAD OF ar-cashl.

RUN pStatus("  Reassigning oe-retx.r-no.").
FOR EACH oe-reth WHERE 
    oe-reth.company EQ "002"
    USE-INDEX rec_key:
    IF CAN-FIND(FIRST boe-reth WHERE 
        boe-reth.company EQ "001" AND 
        boe-reth.r-no EQ oe-reth.r-no) THEN DO:
        FOR EACH ar-cashl WHERE 
            ar-cashl.company EQ oe-reth.company AND 
            ar-cashl.cust-no EQ oe-reth.cust-no AND 
            ar-cashl.returnRno EQ oe-reth.r-no
            USE-INDEX rec_key:
            ASSIGN 
                ar-cashl.returnRno = ar-cashl.returnRno + 100.
        END.  
        FOR EACH oe-retl WHERE 
            oe-retl.company EQ oe-reth.company AND 
            oe-retl.r-no EQ oe-reth.r-no:
            ASSIGN 
                oe-retl.r-no = oe-retl.r-no + 100.
        END.
        ASSIGN 
            oe-reth.r-no = oe-reth.r-no + 100.
    END.
END.

RUN pStatus("  Reassigning ar-ledger check-nos.").
FOR EACH ar-ledger WHERE 
    ar-ledger.company = "002"
    USE-INDEX rec_key:
    IF CAN-FIND (FIRST bar-ledger WHERE 
        bar-ledger.company = "001" AND 
        bar-ledger.cust-no = ar-ledger.cust-no AND 
        bar-ledger.ref-date = ar-ledger.ref-date AND 
        bar-ledger.ref-num = ar-ledger.ref-num) THEN DO:
        FOR EACH ar-cash WHERE 
            ar-cash.company = "002" AND 
            ar-cash.cust-no = ar-ledger.cust-no AND 
            ar-cash.check-no = INTEGER(SUBSTRING(ar-ledger.ref-num, 6))
            USE-INDEX rec_key:
            ASSIGN 
                 ar-cash.check-no = ar-cash.check-no + 1000000000.
        END.
        ASSIGN 
            SUBSTRING(ar-ledger.ref-num,6,1) = "1".
    END.
END.

RUN pStatus("  Reassigning gl-jrn initial dates.").
FOR EACH gl-jrn WHERE 
    gl-jrn.company = "002" AND 
    gl-jrn.tr-date = 7/31/18
    USE-INDEX rec_key:
    ASSIGN 
        gl-jrn.tr-date = 7/30/18.
END.

RUN pStatus("  Reassigning ap-ledger Ref nos.").
FOR EACH ap-inv NO-LOCK WHERE
    ap-inv.company EQ "002":
    FIND FIRST ap-ledger EXCLUSIVE WHERE 
        ap-ledger.company  EQ ap-inv.company AND
        ap-ledger.vend-no  EQ ap-inv.vend-no AND 
        ap-ledger.ref-date EQ ap-inv.inv-date AND 
        ap-ledger.refnum   EQ ("INV# " + ap-inv.inv-no)
        NO-ERROR.
    IF AVAIL ap-ledger THEN ASSIGN 
        ap-ledger.refnum = ("INV# x" + ap-inv.inv-no).
    RELEASE ap-ledger.
END. 

RUN pStatus("  Reassigning ap-payl inv-nos.").
FOR EACH ap-pay NO-LOCK WHERE
    ap-pay.company EQ "002":
    FOR EACH ap-payl OF ap-pay:
        ASSIGN 
            ap-payl.inv-no = "x" + ap-payl.inv-no.
    END.
END.


DO iCtr = 1 TO NUM-ENTRIES(cConvTypeList):
    RUN pConvertRecsByType (ENTRY(iCtr,cConvTypeList)).
END.

RUN pDeleteSimpleMerges.
RUN pCleanup.

RUN pStatus("Conversion Complete").

MESSAGE 
    "Process Complete" 
    VIEW-AS ALERT-BOX.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildFieldLists:
/*------------------------------------------------------------------------------
 Purpose:   Review the _field table to determine which field-names will require conversion
            put these in a temp-table by file-name and 'type' (Account, CustNo, etc.)
 Notes:     This TT will output to a separate CSV that can be compared against requirement docs
------------------------------------------------------------------------------*/
    RUN pStatus("pBuildFieldLists...").
    
    /* Account */
    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "actnum" OR
        _field._field-name MATCHES "acct*" OR 
        _field._field-name MATCHES "*acct*" OR 
        _field._label MATCHES "*Account*" OR
        _field._label MATCHES "*Acct*" OR
        _field._label MATCHES "Account*" OR
        _field._label MATCHES "Acct*" OR
        _field._label MATCHES "*Account" OR
        _field._label MATCHES "*Acct*" OR
        _field._help MATCHES "*account*" OR
        _field._help MATCHES "*acct*" OR
        _field._help MATCHES "account*" OR
        _field._help MATCHES "acct*" OR  
        _field._help MATCHES "*account*" OR
        _field._help MATCHES "*acct*" OR 
        _field._field-name EQ "check-act"
        :
        IF _field._label MATCHES "*desc*" 
            OR _field._label MATCHES "*period*" 
            OR _field._label MATCHES "*bank*" 
            OR _field._label MATCHES "desc*" 
            OR _field._label MATCHES "period*" 
            OR _field._label MATCHES "bank*" 
            OR _field._label MATCHES "*desc"
            OR _field._label MATCHES "*period" 
            OR _field._label MATCHES "*bank" 
            OR _field._label MATCHES "*Desc*" 
            OR _field._label MATCHES "*Period*" 
            OR _field._label MATCHES "*Bank*" 
            OR _field._label MATCHES "Desc*" 
            OR _field._label MATCHES "Period*" 
            OR _field._label MATCHES "Bank*" 
            OR _field._label MATCHES "*Desc"
            OR _field._label MATCHES "*Period" 
            OR _field._label MATCHES "*Bank"
            OR _field._help MATCHES "*desc*" 
            OR _field._help MATCHES "*credit card*" 
            OR _field._help MATCHES "*period*" 
            OR _field._help MATCHES "desc*" 
            OR _field._help MATCHES "credit card*" 
            OR _field._help MATCHES "period*"
            OR _field._help MATCHES "*desc" 
            OR _field._help MATCHES "*credit card" 
            OR _field._help MATCHES "*period"
            THEN NEXT.
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF _file._file-name EQ "bank" 
        AND _field._field-name EQ "check-act" THEN NEXT.
        IF AVAIL _file THEN DO:
            RUN pBuildSingleFieldList ("Account",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END.
    END.

    /* BankAcct */
    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "check-act" 
        OR _field._field-name EQ "bk-act":
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._file-name EQ "ap-pay" 
        AND _field._field-name EQ "check-act" THEN NEXT.
        IF AVAIL _file THEN 
        DO:
            RUN pBuildSingleFieldList ("BankAcct",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.

    /* Carrier */
    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "carrier":
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF AVAIL _file THEN 
        DO:
            RUN pBuildSingleFieldList ("Carrier",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.
    
    /* Check-no - ap tables only */
    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "check-no":
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF NOT _file._file-name BEGINS "ap" THEN NEXT.
        IF AVAIL _file THEN 
        DO:
            RUN pBuildSingleFieldList ("Check-No",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.
        
    /* CustNo - Customers will just be merged so this is unnecessary */
    FOR EACH _field NO-LOCK WHERE 
        CAN-DO ("cust,cust_default,cust-no,customer,customerID,cust-vend-no,we-cust",_field._field-name):
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF AVAIL _file THEN 
        DO:
            RUN pBuildSingleFieldList ("CustNo",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.

    /* Est-no */
    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "est-no" OR 
        _field._field-name MATCHES "*est-no*" OR 
        _field._field-name MATCHES "*est-no" OR 
        _field._field-name MATCHES "est-no*":
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF AVAIL _file THEN 
        DO:
            RUN pBuildSingleFieldList ("Est-no",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.
    
    /* Item, ItemFG */
    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "i-no":
        IF _field._help MATCHES "*finished*"
            OR _field._help MATCHES "*F/G*" 
            OR _field._help MATCHES "Finished*"
            OR _field._help MATCHES "*Finished*"
            OR _field._help MATCHES "*Finished"
            OR _field._help MATCHES "finished*"
            OR _field._help MATCHES "F/G*"
            OR _field._help MATCHES "*finished"
            OR _field._help MATCHES "*F/G"THEN DO:
            FIND _file OF _field NO-LOCK NO-ERROR.
            IF _file._hidden THEN NEXT.
            IF AVAIL _file THEN 
            DO:
                RUN pBuildSingleFieldList ("ItemFG",_file._file-name,_field._field-name).
                RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
            END. 
        END.
        ELSE IF _field._help MATCHES "*Raw*"
                OR _field._label MATCHES "*RM*" 
                OR _field._help MATCHES "Raw*"
                OR _field._label MATCHES "RM*"
                OR _field._help MATCHES "*Raw"
                OR _field._label MATCHES "*RM" THEN DO:
            FIND _file OF _field NO-LOCK NO-ERROR.
            IF _file._hidden THEN NEXT.
            IF AVAIL _file THEN 
            DO:
                RUN pBuildSingleFieldList ("Item",_file._file-name,_field._field-name).
                RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
            END. 
        END.
        ELSE DO:
            FIND _file OF _field NO-LOCK NO-ERROR.
            IF _file._hidden THEN NEXT.
            IF AVAIL _file 
                AND CAN-DO (",",_file._file-name) THEN 
            DO:
                RUN pBuildSingleFieldList ("ItemUnk",_file._file-name,_field._field-name).
                RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
            END. 
            ELSE IF AVAIL _file 
                    AND CAN-DO (",",_file._file-name) THEN 
            DO:
                RUN pBuildSingleFieldList ("Item",_file._file-name,_field._field-name).
                RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
                    END. 
            END.
    END.

    /* inv-no */
    FOR EACH _field WHERE _field-name = "inv-no":
        FIND _file OF _field NO-LOCK.
        IF _file._hidden THEN NEXT.
        IF NOT _file._file-name BEGINS "ap" THEN NEXT.
        IF AVAIL _file THEN 
        DO:
            RUN pBuildSingleFieldList ("inv-no",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END.
    END.
        
    /* Job */
    FOR EACH _field WHERE _field._field-name = "job":
        IF _field._data-type = "integer" THEN DO:
            FIND _file OF _field NO-LOCK.
            IF _file._hidden THEN NEXT.
            IF AVAIL _file THEN DO:
                RUN pBuildSingleFieldList ("Job",_file._file-name,_field._field-name).
                RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
            END.
        END.
    END.

    /* Loadtag */
    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "tag" OR 
        _field._field-name = "tag-no":
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _field._field-name EQ "tag" AND SUBSTRING(_file._file-name,1,1) LT "v" THEN NEXT. 
        IF AVAIL _file THEN 
        DO:
            RUN pBuildSingleFieldList ("Loadtag",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.

    /* Loc */
    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "loc" OR
        _field._field-name MATCHES "*loc*" OR 
        _field._label MATCHES "*loc*" OR 
        _field._field-name MATCHES "loc*" OR 
        _field._label MATCHES "loc*" OR 
        _field._field-name MATCHES "*loc" OR 
        _field._label MATCHES "*loc":
        IF _field._field-name MATCHES "*bin*"
            OR _field._field-name MATCHES "*lock*"
            OR _field._field-name MATCHES "*a-no*"
            OR _field._field-name MATCHES "*all*"
            OR _field._field-name MATCHES "*dock*"
            OR _field._field-name MATCHES "*location*"
            OR _field._field-name MATCHES "bin*"
            OR _field._field-name MATCHES "lock*"
            OR _field._field-name MATCHES "a-no*"
            OR _field._field-name MATCHES "all*"
            OR _field._field-name MATCHES "dock*"
            OR _field._field-name MATCHES "location*"
            OR _field._field-name MATCHES "*bin"
            OR _field._field-name MATCHES "*lock"
            OR _field._field-name MATCHES "*a-no"
            OR _field._field-name MATCHES "*all"
            OR _field._field-name MATCHES "*dock"
            OR _field._field-name MATCHES "*location" 
            THEN NEXT.
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF AVAIL _file THEN DO:
            RUN pBuildSingleFieldList ("Loc",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.
    /* Handle oe-rel.spare-char-1 (Ship From Whse) and location (loc address table) */
    RUN pBuildSingleFieldList ("Loc","oe-rel","spare-char-1").
    RUN pBuildSingleFieldList ("Loc","location","locationCode").

    /* M-code */    
    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "m-code":
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF AVAIL _file THEN 
        DO:
            RUN pBuildSingleFieldList ("M-code",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.

    /* ra-no */    
    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "ra-no":
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF AVAIL _file THEN 
        DO:
            RUN pBuildSingleFieldList ("ra-no",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.

    /* Release# */    
    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "release#":
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF AVAIL _file THEN 
        DO:
            RUN pBuildSingleFieldList ("Release#",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.

    /* Company - DO LAST */
    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "company" OR
        _field._field-name = "cocode":
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF AVAIL _file THEN 
        DO:
            RUN pBuildSingleFieldList ("Company",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.
    
END PROCEDURE.


PROCEDURE pBuildFullTableList:
/*------------------------------------------------------------------------------
 Purpose:   Builds a TT of tables in the DB with index fields and record counts listed
 Notes:     This TT will output to a separate CSV for analysis if needed
------------------------------------------------------------------------------*/
    DEF VAR hQuery AS HANDLE NO-UNDO.
    
    RUN pStatus("pBuildFullTableList...").
    
    EMPTY TEMP-TABLE ttFullTableList.
    FOR EACH _file NO-LOCK
        WHERE NOT _file._hidden:
        CREATE ttFullTableList.
        ASSIGN 
            ttFullTableList.cTable     = _file._file-name
            ttFullTableList.cTableDesc = REPLACE(REPLACE(_file._desc,","," "),CHR(10)," ")
            .
        FOR EACH _index NO-LOCK OF _file:
            FOR EACH _index-field OF _index NO-LOCK,
            EACH _field OF _index-field:
                IF NOT CAN-DO(ttFullTableList.cAllIndexFields,_field._field-name) THEN ASSIGN 
                        ttFullTableList.cAllIndexFields = ttFullTableList.cAllIndexFields + _field._field-name + ",".
            END.
            IF _index._unique THEN DO:
                FOR EACH _index-field OF _index NO-LOCK,
                EACH _field OF _index-field:
                    IF NOT CAN-DO(ttFullTableList.cUIndexFields,_field._field-name) THEN ASSIGN 
                        ttFullTableList.cUIndexFields = ttFullTableList.cUIndexFields + _field._field-name + ",".
                END.
            END.
        END.
        ASSIGN 
            ttFullTableList.cAllIndexFields = TRIM(ttFullTableList.cAllIndexFields,",") 
            ttFullTableList.cUIndexFields = TRIM(ttFullTableList.cUIndexFields,","). 
    
        RUN pCreateQuery(_file._file-name, OUTPUT hQuery).
        IF VALID-HANDLE(hQuery) THEN 
            ttFullTableList.iRecordCount = hQuery:NUM-RESULTS.
    
    END. /*each _file*/
    
END PROCEDURE.


PROCEDURE pBuildNewCoA:
/*------------------------------------------------------------------------------
 Purpose:   Create new accounts based on LY provided spreadsheet
 Notes:     Spreadsheet (and TT) also contain old/new account mappings
------------------------------------------------------------------------------*/
    DEFINE BUFFER bNewCoA FOR account.
    DEFINE BUFFER bOldCoA FOR account. 
    
    RUN pStatus("pBuildNewCoA...").

    DISABLE TRIGGERS FOR LOAD OF account.
    
    FOR EACH ttNewCoA:
        FIND FIRST bNewCoA WHERE 
            bNewCoA.company EQ ttNewCoA.toCompany AND 
            bNewCoA.actnum EQ ttNewCoA.toAcct
            NO-LOCK NO-ERROR.
        IF AVAIL bNewCoA THEN 
            NEXT.        
        FIND FIRST bOldCoA WHERE 
            bOldCoA.company EQ ttNewCoA.fromCompany AND 
            bOldCoA.actnum EQ ttNewCoA.fromAcct
            EXCLUSIVE NO-ERROR.
        IF AVAIL bOldCoA THEN DO:
            CREATE bNewCoA.
            BUFFER-COPY bOldCoA EXCEPT company actnum dscr TO bNewCoA.
            ASSIGN 
                bNewCoA.company = ttNewCoA.toCompany
                bNewCoA.actnum  = ttNewCoA.toAcct
                bNewCoA.dscr    = ttNewCoA.AcctDesc.
            DELETE bOldCoA.
        END.
    END.
            
END PROCEDURE.


PROCEDURE pBuildSingleFieldList:
/*------------------------------------------------------------------------------
 Purpose:   Updates ttTablesWithMergeFields to populate list of fields in a single table
            that require conversion BY TYPE
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcType AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcTable AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcField AS CHAR NO-UNDO.
    
    RUN pStatus("   Reviewing " + ipcType + " conflict fields in " + ipcTable + "," + ipcField).

    FIND FIRST ttTablesWithMergeFields WHERE 
        ttTablesWithMergeFields.cFieldType = ipcType AND 
        ttTablesWithMergeFields.cTableName EQ ipcTable
        NO-ERROR.
    IF NOT AVAIL ttTablesWithMergeFields THEN DO:
        CREATE ttTablesWithMergeFields.
        ASSIGN 
            ttTablesWithMergeFields.cFieldType = ipcType
            ttTablesWithMergeFields.cTableName = ipcTable
            ttTablesWithMergeFields.cFieldName = ipcField.
    END.
    ELSE ASSIGN 
        ttTablesWithMergeFields.cFieldName = ttTablesWithMergeFields.cFieldName + "," + ipcField.

END PROCEDURE.


PROCEDURE pCleanup:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF loc.
    DISABLE TRIGGERS FOR LOAD OF location.
    DISABLE TRIGGERS FOR LOAD OF ce-ctrl.
    DISABLE TRIGGERS FOR LOAD OF oe-ctrl.
    DISABLE TRIGGERS FOR LOAD OF ar-ctrl.
    DISABLE TRIGGERS FOR LOAD OF po-ctrl.
    
    FOR EACH loc:
        FIND location WHERE 
            location.company EQ loc.company AND 
            location.locationCode EQ loc.loc.
        ASSIGN 
            loc.addrreckey = location.rec_key.
        IF loc.loc EQ "COMM" THEN ASSIGN 
            loc.dscr = "Lanco York Commack"
            location.streetaddr[1] = "81 Modular Ave."
            location.subCode3 = "Commack"
            location.subCode1 = "NY"
            location.subCode4 = "11725"
            location.subCode2 = "Suffolk"
            location.countryCode = "USA".
        ELSE ASSIGN 
            loc.dscr = "Lanco York Paterson"
            location.streetaddr[1] = "864 E. 25th St."
            location.subCode3 = "Paterson"
            location.subCode1 = "NJ"
            location.subCode4 = "07513"
            location.subCode2 = "Passaic"
            location.countryCode = "USA".
    END.
    
    FOR EACH ce-ctrl:
        IF ce-ctrl.loc NE "PATT"
        AND ce-ctrl.loc NE "COMM" THEN 
            DELETE ce-ctrl.
    END. 
    
    FIND oe-ctrl WHERE 
        oe-ctrl.company = "001"
        EXCLUSIVE.
    FIND LAST oe-ord NO-LOCK USE-INDEX ord-no.
    FIND LAST bolh NO-LOCK USE-INDEX bol-no.
    ASSIGN 
        oe-ctrl.n-ord = oe-ord.ord-no + 1
        oe-ctrl.n-bol = bolh.bol-no + 1.

    FOR EACH ar-ctrl WHERE ar-ctrl.company NE "001":
        DELETE ar-ctrl.
    END.
    FIND FIRST ar-ctrl WHERE 
        ar-ctrl.company = "001"
        EXCLUSIVE.
    FIND LAST inv-head NO-LOCK
        USE-INDEX inv-no.
    ASSIGN 
        ar-ctrl.last-inv = inv-head.inv-no
        iCtr = 0.
    
    FIND po-ctrl WHERE 
        po-ctrl.company = "001"
        EXCLUSIVE.
    FIND LAST po-ord NO-LOCK USE-INDEX po-no.
    ASSIGN 
        po-ctrl.next-po-no = po-ord.po-no + 1.

END PROCEDURE.

PROCEDURE pConsolidateAmounts:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF BUFFER bAccount FOR account.
    DEF BUFFER bCust FOR cust.
    DEF BUFFER bItem FOR ITEM.
    DEF BUFFER bItemfg FOR itemfg.
    DEF BUFFER bItemfg-loc FOR itemfg-loc.
    DEF BUFFER bVend FOR vend.
    
    DISABLE TRIGGERS FOR LOAD OF account.
    DISABLE TRIGGERS FOR LOAD OF cust.
    DISABLE TRIGGERS FOR LOAD OF item.
    DISABLE TRIGGERS FOR LOAD OF itemfg.
    DISABLE TRIGGERS FOR LOAD OF itemfg-loc.
    DISABLE TRIGGERS FOR LOAD OF vend.
    DISABLE TRIGGERS FOR LOAD OF location.
        
    RUN pStatus("Fixing blank company in location table").
    FOR EACH location EXCLUSIVE WHERE 
        location.company EQ "":
        FIND loc NO-LOCK WHERE 
            loc.loc = "MAIN" AND 
            loc.addrreckey = location.rec_key.
        ASSIGN 
            location.company = loc.company.
    END.

    RUN pStatus("Consolidating amount fields in tables...").
    RUN pStatus("   Consolidating amount fields in account table").
    FOR EACH account WHERE 
        account.company = "001":
        FIND bAccount EXCLUSIVE WHERE 
            baccount.company = "002" AND 
            baccount.actnum = account.actnum
            NO-ERROR.
        IF AVAIL baccount THEN DO:
            ASSIGN 
                account.cyr-open = account.cyr-open + baccount.cyr-open
                account.lyr-open = account.lyr-open + baccount.lyr-open.
            DO ictr = 1 TO 13:
                ASSIGN 
                    account.cyr[ictr] = account.cyr[ictr] + baccount.cyr[ictr]
                    account.lyr[ictr] = account.lyr[ictr] + baccount.lyr[ictr]
                    account.bud[ictr] = account.bud[ictr] + baccount.bud[ictr]
                    account.ly-bud[ictr] = account.ly-bud[ictr] + baccount.ly-bud[ictr]
                    account.ny-bud[ictr] = account.ny-bud[ictr] + baccount.ny-bud[ictr]
                .
            END.
            DELETE baccount.
        END.
    END.
            
    RUN pStatus("   Consolidating amount fields in cust table").
    FOR EACH cust WHERE 
        cust.company = "001":
        FIND bCust EXCLUSIVE WHERE 
            bCust.company = "002" AND 
            bCust.cust-no = Cust.cust-no
            NO-ERROR.
        IF AVAIL bCust THEN DO:
            ASSIGN 
                Cust.acc-bal = Cust.acc-bal + bCust.acc-bal
                Cust.ord-bal = Cust.ord-bal + bCust.ord-bal
                Cust.on-account = Cust.on-account + bCust.on-account
                Cust.ytd-msf = Cust.ytd-msf + bCust.ytd-msf
                Cust.ytd-sales = Cust.ytd-sales + bCust.ytd-sales
                Cust.lyr-sales = Cust.lyr-sales + bCust.lyr-sales
                Cust.lyytd-msf = Cust.lyytd-msf + bCust.lyytd-msf
                .
            DO ictr = 1 TO 13:
                ASSIGN 
                    Cust.sales[ictr] = Cust.sales[ictr] + bCust.sales[ictr]
                    Cust.n-sales[ictr] = Cust.n-sales[ictr] + bCust.n-sales[ictr]
                    Cust.ptd-msf[ictr] = Cust.ptd-msf[ictr] + bCust.ptd-msf[ictr]
                    Cust.lyptd-msf[ictr] = Cust.lyptd-msf[ictr] + bCust.lyptd-msf[ictr]
                    Cust.lyr-bud[ictr] = Cust.lyr-bud[ictr] + bCust.lyr-bud[ictr]
                    Cust.cyr-bud[ictr] = Cust.cyr-bud[ictr] + bCust.cyr-bud[ictr]
                    Cust.nyr-bud[ictr] = Cust.nyr-bud[ictr] + bCust.nyr-bud[ictr]
                    .
            END.
            DELETE bCust.
        END.
    END.
    
    RUN pStatus("   Consolidating amount fields in item table").
    FOR EACH item WHERE 
        item.company = "001" AND 
        item.mat-type NE "8":
        FIND bitem EXCLUSIVE WHERE 
            bitem.company = "002" AND 
            bitem.i-no = item.i-no
            NO-ERROR.
        IF AVAIL bitem THEN DO:
            ASSIGN 
                item.beg-bal = item.beg-bal + bitem.beg-bal
                item.q-onh = item.q-onh + bitem.q-onh
                item.q-ono = item.q-ono + bitem.q-ono
                item.q-comm = item.q-comm + bitem.q-comm
                item.q-back = item.q-back + bitem.q-back
                item.q-avail = item.q-avail + bitem.q-avail
                item.last-count = item.last-count + bitem.last-count
                item.q-ptd = item.q-ptd + bitem.q-ptd
                item.q-ytd = item.q-ytd + bitem.q-ytd
                item.q-lyr = item.q-lyr + bitem.q-lyr
                item.u-ptd = item.u-ptd + bitem.u-ptd
                item.u-ytd = item.u-ytd + bitem.u-ytd
                item.u-lyr = item.u-lyr + bitem.u-lyr
                item.pur-cnt = item.pur-cnt + bitem.pur-cnt
                .
            DELETE bitem.
        END.
    END.

    RUN pStatus("   Consolidating amount fields in itemfg table").
    FOR EACH itemfg WHERE 
        itemfg.company = "001":
        FIND bitemfg EXCLUSIVE WHERE 
            bitemfg.company = "002" AND 
            bitemfg.i-no = itemfg.i-no
            NO-ERROR.
        IF AVAIL bitemfg THEN 
        DO:
            ASSIGN 
                itemfg.beg-bal = itemfg.beg-bal + bitemfg.beg-bal
                itemfg.q-onh = itemfg.q-onh + bitemfg.q-onh
                itemfg.q-ono = itemfg.q-ono + bitemfg.q-ono
                itemfg.q-comm = itemfg.q-comm + bitemfg.q-comm
                itemfg.q-back = itemfg.q-back + bitemfg.q-back
                itemfg.q-avail = itemfg.q-avail + bitemfg.q-avail
                itemfg.last-count = itemfg.last-count + bitemfg.last-count
                itemfg.q-ptd = itemfg.q-ptd + bitemfg.q-ptd
                itemfg.q-ytd = itemfg.q-ytd + bitemfg.q-ytd
                itemfg.q-lyr = itemfg.q-lyr + bitemfg.q-lyr
                itemfg.u-ptd = itemfg.u-ptd + bitemfg.u-ptd
                itemfg.u-ytd = itemfg.u-ytd + bitemfg.u-ytd
                itemfg.u-lyr = itemfg.u-lyr + bitemfg.u-lyr
                itemfg.pur-cnt = itemfg.pur-cnt + bitemfg.pur-cnt
                .
            /*    
            FOR EACH itemfg-loc WHERE 
                itemfg-loc.company EQ itemfg.company AND 
                itemfg-loc.i-no EQ itemfg.i-no AND 
                itemfg-loc.loc EQ "main":
                FIND bitemfg-loc EXCLUSIVE WHERE 
                    bitemfg-loc.company = "002" AND 
                    bitemfg-loc.i-no = itemfg.i-no AND 
                    bitemfg-loc.loc EQ "MAIN"
                    NO-ERROR.
                IF AVAIL bitemfg-loc THEN DO:
                    ASSIGN 
                        itemfg-loc.q-adj = itemfg-loc.q-adj + bitemfg-loc.q-adj
                        itemfg-loc.q-adj-ptd = itemfg-loc.q-adj-ptd + bitemfg-loc.q-adj-ptd
                        itemfg-loc.q-adj-ytd = itemfg-loc.q-adj-ytd + bitemfg-loc.q-adj-ytd
                        
                        itemfg-loc.q-alloc = itemfg-loc.q-alloc + bitemfg-loc.q-alloc
                        itemfg-loc.q-alloc-ptd = itemfg-loc.q-alloc-ptd + bitemfg-loc.q-alloc-ptd
                        itemfg-loc.q-alloc-ytd = itemfg-loc.q-alloc-ytd + bitemfg-loc.q-alloc-ytd
                        
                        itemfg-loc.q-avail = itemfg-loc.q-avail + bitemfg-loc.q-avail
                        itemfg-loc.q-back = itemfg-loc.q-back + bitemfg-loc.q-back
                        
                        itemfg-loc.q-cogs = itemfg-loc.q-cogs + bitemfg-loc.q-cogs
                        itemfg-loc.q-cogs-ptd = itemfg-loc.q-cogs-ptd + bitemfg-loc.q-cogs-ptd
                        itemfg-loc.q-cogs-ytd = itemfg-loc.q-cogs-ytd + bitemfg-loc.q-cogs-ytd

                        itemfg-loc.q-comm = itemfg-loc.q-comm + bitemfg-loc.q-comm
                        
                        itemfg-loc.q-inv = itemfg-loc.q-inv + bitemfg-loc.q-inv
                        itemfg-loc.q-inv-ptd = itemfg-loc.q-inv-ptd + bitemfg-loc.q-inv-ptd
                        itemfg-loc.q-inv-ytd = itemfg-loc.q-inv-ytd + bitemfg-loc.q-inv-ytd

                        itemfg-loc.q-lyr = itemfg-loc.q-lyr + bitemfg-loc.q-lyr
                        itemfg-loc.q-onh = itemfg-loc.q-onh + bitemfg-loc.q-onh
                        itemfg-loc.q-ono = itemfg-loc.q-ono + bitemfg-loc.q-ono
                        
                        itemfg-loc.q-ord = itemfg-loc.q-ord + bitemfg-loc.q-ord
                        itemfg-loc.q-ord-ptd = itemfg-loc.q-ord-ptd + bitemfg-loc.q-ord-ptd
                        itemfg-loc.q-ord-ytd = itemfg-loc.q-ord-ytd + bitemfg-loc.q-ord-ytd

                        itemfg-loc.q-prod = itemfg-loc.q-prod + bitemfg-loc.q-prod
                        itemfg-loc.q-prod-ptd = itemfg-loc.q-prod-ptd + bitemfg-loc.q-prod-ptd
                        itemfg-loc.q-prod-ytd = itemfg-loc.q-prod-ytd + bitemfg-loc.q-prod-ytd

                        itemfg-loc.q-ptd = itemfg-loc.q-ptd + bitemfg-loc.q-ptd
                        
                        itemfg-loc.q-rec = itemfg-loc.q-rec + bitemfg-loc.q-rec
                        itemfg-loc.q-rec-ptd = itemfg-loc.q-rec-ptd + bitemfg-loc.q-rec-ptd
                        itemfg-loc.q-rec-ytd = itemfg-loc.q-rec-ytd + bitemfg-loc.q-rec-ytd
                        
                        itemfg-loc.q-rel = itemfg-loc.q-rel + bitemfg-loc.q-rel
                        itemfg-loc.q-rel-ptd = itemfg-loc.q-rel-ptd + bitemfg-loc.q-rel-ptd  
                        itemfg-loc.q-rel-ytd = itemfg-loc.q-rel-ytd + bitemfg-loc.q-rel-ytd  
        
                        itemfg-loc.q-ship = itemfg-loc.q-ship + bitemfg-loc.q-ship
                        itemfg-loc.q-ship-ptd = itemfg-loc.q-ship-ptd + bitemfg-loc.q-ship-ptd  
                        itemfg-loc.q-ship-ytd = itemfg-loc.q-ship-ytd + bitemfg-loc.q-ship-ytd  

                        itemfg-loc.q-tran = itemfg-loc.q-tran + bitemfg-loc.q-tran
                        itemfg-loc.q-tran-ptd = itemfg-loc.q-tran-ptd + bitemfg-loc.q-tran-ptd  
                        itemfg-loc.q-tran-ytd = itemfg-loc.q-tran-ytd + bitemfg-loc.q-tran-ytd  

                        itemfg-loc.q-ytd = itemfg-loc.q-ytd + bitemfg-loc.q-ytd
                        .
                    DELETE bitemfg-loc.
                END.
            END.
            */
            DELETE bitemfg.
        END.
    END.


    RUN pStatus("   Consolidating amount fields in vend table").
    FOR EACH vend WHERE 
        vend.company = "001":
        FIND bvend EXCLUSIVE WHERE 
            bvend.company = "002" AND 
            bvend.vend-no = vend.vend-no
            NO-ERROR.
        IF AVAIL bvend THEN DO:
            ASSIGN 
                vend.acc-bal = vend.acc-bal + bvend.acc-bal
                vend.last-year = vend.last-year + bvend.last-year
                vend.lyytd-msf = vend.lyytd-msf + vend.lyytd-msf
                vend.num-inv = vend.num-inv + bvend.num-inv
                vend.ord-bal = vend.ord-bal + bvend.ord-bal
                vend.pytd = vend.pytd + bvend.pytd
                vend.ytd-msf = vend.ytd-msf + vend.ytd-msf
                .
            DO ictr = 1 TO 13:
                ASSIGN 
                    vend.discount[ictr] = vend.discount[ictr] + bvend.discount[ictr]
                    vend.lyptd-msf[ictr] = vend.lyptd-msf[ictr] + bvend.lyptd-msf[ictr]
                    vend.n-purch[ictr] = vend.n-purch[ictr] + bvend.n-purch[ictr]
                    vend.ptd-msf[ictr] = vend.ptd-msf[ictr] + bvend.ptd-msf[ictr]
                    vend.purch[ictr] = vend.purch[ictr] + bvend.purch[ictr]
                    .
            END.
            DELETE bVend.
        END.
    END.

END PROCEDURE.

PROCEDURE pConvertBankAccountTable:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.
    
    /* ap-pay uses the GL account, NOT the bank acct */
    IF ipcTableName EQ "ap-pay" THEN RETURN.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).
    
    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "002"  
                    AND hField:BUFFER-VALUE = "123" THEN ASSIGN  
                        hField:BUFFER-VALUE = "00940529003" NO-ERROR.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    IF hCoField:BUFFER-VALUE EQ "002" 
                    AND hField:BUFFER-VALUE(jCtr) EQ "123" THEN ASSIGN  
                        hField:BUFFER-VALUE(jCtr) = "00940529003" NO-ERROR.
                END.
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pConvertJobTable:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hJobField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 
                AND hCoField:BUFFER-VALUE = "002" THEN
                DO:
                    ASSIGN 
                        hField:BUFFER-VALUE = hField:BUFFER-VALUE + 2000.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN 
                    DO:
                        ASSIGN 
                            hField:BUFFER-VALUE(jCtr) = hField:BUFFER-VALUE(jCtr) + 2000.
                    END.
                END.
            END.
        END.
    END.


END PROCEDURE.

PROCEDURE pConvertLoadtagTable:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hTagField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.
    DEF VAR cRepl AS CHAR NO-UNDO.
    DEF VAR cNew AS CHAR NO-UNDO.
    DEF VAR iRepl AS INT NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    checkTagNo:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hTagField = hBuffer:BUFFER-FIELD (iCtr).
        IF hTagField:NAME = "tag-no" THEN 
            LEAVE checkTagNo.
    END.

    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN
                DO:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN 
                    DO:
                        IF CAN-FIND(FIRST loadtag WHERE 
                            loadtag.company EQ "001" AND 
                            loadtag.tag-no EQ hField:BUFFER-VALUE) THEN DO:
                            ASSIGN 
                                cRepl = hField:BUFFER-VALUE
                                iRepl = INTEGER(SUBSTRING(cRepl,16,1)) + 1
                                cNew = STRING(iRepl,"9")
                                cRepl = SUBSTRING(cRepl,1,15) + cNew + SUBSTRING(cRepl,17)
                                hField:BUFFER-VALUE = cRepl.
                        END. 
                    END.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN 
                    DO:
                        IF CAN-FIND(FIRST loadtag WHERE 
                            loadtag.company EQ "001" AND 
                            loadtag.tag-no EQ hField:STRING-VALUE(jctr)) THEN 
                        DO:
                            ASSIGN 
                                cRepl = hField:STRING-VALUE(jctr)
                                iRepl = INTEGER(SUBSTRING(cRepl,16,1)) + 1
                                cNew = STRING(iRepl,"9")
                                cRepl = SUBSTRING(cRepl,1,15) + cNew + SUBSTRING(cRepl,17)
                                hField:BUFFER-VALUE(jctr) = cRepl.
                        END. 
                    END.
                END.
            END.
        END.
    END.



END PROCEDURE.

PROCEDURE pConvertRecsByType:
/*------------------------------------------------------------------------------
 Purpose:   Runner proc for record conversion by element TYPE (Account, CustNo, etc.)
 Notes:     Will run the pConvertXXXTable proc once per table in the TYPE group
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcType AS CHAR NO-UNDO.
    
    RUN pStatus("pConvertRecsByType - " + ipcType).
    FOR EACH ttTablesWithMergeFields WHERE 
        ttTablesWithMergeFields.cFieldType = ipcType:
        CASE ipcType:
            WHEN "Account"  THEN RUN pConvertAccountTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "BankAcct" THEN RUN pConvertBankAccountTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "Carrier"  THEN RUN pConvertCarrierTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "Check-no" THEN RUN pConvertCheckNoTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "Company"  THEN RUN pConvertCompanyTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "Est-no"   THEN RUN pConvertEstNoTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "inv-no"   THEN RUN pConvertInvNoTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "Item"     THEN RUN pConvertItemTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "Job"      THEN RUN pConvertJobTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "Loadtag"  THEN RUN pConvertLoadtagTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "Loc"      THEN RUN pConvertLocTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "M-code"   THEN RUN pConvertMcodeTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "ra-no"    THEN RUN pConvertRaNoTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "Release#" THEN RUN pConvertRelease#Table (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            /* These are just merged - don't convert 
            WHEN "ItemFg"   THEN RUN pConvertItemFGTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "CustNo"   THEN RUN pConvertCustTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            */
        END.            
    END.        

END PROCEDURE.


PROCEDURE pConvertAccountTable:
/*------------------------------------------------------------------------------
 Purpose:   Convert all account-type fields in each record of a single table
 Notes:     Note this method of conversion effectively eliminates the collision problem, 
            since company code DOES NOT change until all other index info has been converted.
            Since new records are not created, the rec_key for related tables does note change
            Also note, this logic is dependent on customer-supplied GL account mapping
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.
    DEF VAR cCompVal AS CHAR NO-UNDO.
    DEF VAR cFields AS CHAR NO-UNDO.
    DEF VAR cThisTable AS CHAR NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN DO:
                    FIND FIRST ttNewCoA WHERE 
                        ttNewCoA.fromCompany EQ hCoField:BUFFER-VALUE AND 
                        ttNewCoA.fromAcct EQ hField:BUFFER-VALUE
                        NO-LOCK NO-ERROR.
                    IF AVAIL ttNewCoA THEN ASSIGN 
                            hField:BUFFER-VALUE = ttNewCoA.toAcct NO-ERROR.
                END.
                ELSE DO jCtr = 1 TO hField:EXTENT:
                    FIND FIRST ttNewCoA WHERE 
                        ttNewCoA.fromCompany EQ hCoField:BUFFER-VALUE AND 
                        ttNewCoA.fromAcct EQ hField:BUFFER-VALUE(jCtr)
                        NO-LOCK NO-ERROR.
                    IF AVAIL ttNewCoA THEN ASSIGN 
                            hField:BUFFER-VALUE(jCtr) = ttNewCoA.toAcct NO-ERROR.
                END.
            END.
        END.
    END.

END PROCEDURE.


PROCEDURE pConvertCarrierTable:
/*------------------------------------------------------------------------------
 Purpose:   Convert all carrier-type fields in each record of a single table
 Notes:     Note this method of conversion effectively eliminates the collision problem, 
            since company code DOES NOT change until all other index info has been converted.
            Since new records are not created, the rec_key for related tables does note change
            Also note, this logic is dependent on customer-supplied conversion mapping
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).
    
    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "001" THEN ASSIGN  
                        hField:BUFFER-VALUE = "P" + SUBSTRING(hField:BUFFER-VALUE,1,4) NO-ERROR.
                    ELSE IF hCoField:BUFFER-VALUE EQ "002"  THEN ASSIGN  
                        hField:BUFFER-VALUE = "C" + SUBSTRING(hField:BUFFER-VALUE,1,4) NO-ERROR.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    IF hCoField:BUFFER-VALUE EQ "001" THEN ASSIGN  
                        hField:BUFFER-VALUE(jCtr) = "P" + SUBSTRING(hField:BUFFER-VALUE(jCtr),1,4) NO-ERROR.
                    ELSE IF hCoField:BUFFER-VALUE EQ "002" THEN ASSIGN  
                        hField:BUFFER-VALUE(jCtr) = "C" + SUBSTRING(hField:BUFFER-VALUE(jCtr),1,4) NO-ERROR.
                END.
            END.
        END.
    END.

END PROCEDURE.


PROCEDURE pConvertCheckNoTable:
    /*------------------------------------------------------------------------------
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.
    DEF VAR iIntVal AS INT NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).
    
    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN DO:
                        IF hField:DATA-TYPE = "INTEGER" 
                        AND hField:BUFFER-VALUE GE 800000
                        AND hField:BUFFER-VALUE LT 900000 THEN ASSIGN  
                            hField:BUFFER-VALUE = hField:BUFFER-VALUE + 10000 NO-ERROR.
                        ELSE 
                    END.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN 
                    DO:
                        IF hField:DATA-TYPE = "INTEGER" 
                            AND hField:BUFFER-VALUE(jctr) GE 800000
                            AND hField:BUFFER-VALUE(jctr) LT 900000 THEN ASSIGN  
                                hField:BUFFER-VALUE(jctr) = hField:BUFFER-VALUE(jctr) + 10000 NO-ERROR.
                        ELSE 
                    END.
                END.
            END.
        END.
    END.

END PROCEDURE.


PROCEDURE pConvertCompanyTable:
/*------------------------------------------------------------------------------
 Purpose:   Convert all company-type fields in each record of a single table
 Notes:     Note this method of conversion effectively eliminates the collision problem, 
            since company code DOES NOT change until all other index info has been converted.
            Since new records are not created, the rec_key for related tables does note change
            Also note, this logic is dependent on customer-supplied conversion mapping
            THIS MUST BE THE LAST TYPE GROUP RUN IN THE CONVERSION
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hField = hBuffer:BUFFER-FIELD (iCtr).
        IF hField:NAME EQ "Company" 
        OR hField:NAME EQ "cocode" THEN ASSIGN 
            hCoField = hField.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        /* Remove/merge certain tables */
        IF NOT hQuery:QUERY-OFF-END 
        AND hCoField:BUFFER-VALUE = "002" AND 
        CAN-DO("flute,scoreType,stax,style,terms",hBuffer:TABLE) THEN DO:
             hBuffer:BUFFER-DELETE().
             NEXT.
        END. 
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hBuffer:TABLE EQ "cust" 
                AND hField:NAME EQ "cust-no"
                AND hCoField:BUFFER-VALUE EQ "002"
                AND CAN-FIND(FIRST cust WHERE 
                    cust.company EQ "001" AND 
                    cust.cust-no EQ hField:BUFFER-VALUE) THEN DO:
                    hBuffer:BUFFER-DELETE().
                    NEXT.
                END. 
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN 
                    DO:
                        ASSIGN 
                            hField:BUFFER-VALUE = "001" NO-ERROR.
                        IF ERROR-STATUS:ERROR THEN RUN pStatus("      Error merging " + ipcTableName + "record. Duplicate company.").
                    END.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN 
                    DO:
                        ASSIGN 
                            hField:BUFFER-VALUE(jCtr) = "001" NO-ERROR.
                    END.
                END.
            END.
        END.
    END.

END PROCEDURE.


PROCEDURE pConvertCustTable:
/*------------------------------------------------------------------------------
 Purpose:   Convert all cust-no-type fields in each record of a single table
 Notes:     Note this method of conversion effectively eliminates the collision problem, 
            since company code DOES NOT change until all other index info has been converted.
            Since new records are not created, the rec_key for related tables does note change
            Also note, this logic is dependent on customer-supplied conversion mapping
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN
                DO:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN DO:
                        IF CAN-FIND(FIRST cust WHERE 
                                    cust.company EQ "001" AND 
                                    cust.cust-no EQ hField:BUFFER-VALUE) THEN ASSIGN 
                            hField:BUFFER-VALUE = "z" + hField:BUFFER-VALUE NO-ERROR.
                    END.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN DO:
                        IF CAN-FIND(FIRST cust WHERE 
                            cust.company EQ "001" AND 
                            cust.cust-no EQ hField:BUFFER-VALUE(jCtr)) THEN ASSIGN 
                                hField:BUFFER-VALUE(jCtr) = "z" + hField:BUFFER-VALUE(jCtr) NO-ERROR.
                    END.
                END.
            END.
        END.
    END.

END PROCEDURE.


PROCEDURE pConvertEstNoTable:
/*------------------------------------------------------------------------------
 Purpose:   Convert all est-no-type fields in each record of a single table
 Notes:     Note this method of conversion effectively eliminates the collision problem, 
            since company code DOES NOT change until all other index info has been converted.
            Since new records are not created, the rec_key for related tables does note change
            Also note, this logic is dependent on customer-supplied conversion mapping
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.
    DEF VAR iEstNo AS INT NO-UNDO.
    DEF VAR cEstNo AS CHAR NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN DO:
                        IF TRIM(hField:BUFFER-VALUE) EQ "" THEN ASSIGN 
                            cEstNo = hField:BUFFER-VALUE.
                        ELSE DO:
                            ASSIGN
                                cEstNo = "   1" + SUBSTRING(hField:BUFFER-VALUE,5,4).
                            IF SUBSTRING(cEstNo,5,1) = " " THEN ASSIGN
                                SUBSTRING(cEstNo,5,1) = "0".
                        END.
                        ASSIGN  
                            hField:BUFFER-VALUE = cEstNo NO-ERROR.
                    END.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN DO:
                        IF TRIM(hField:BUFFER-VALUE(jCtr)) EQ "" THEN ASSIGN 
                                cEstNo = hField:BUFFER-VALUE(jCtr).
                        ELSE 
                        DO:
                            ASSIGN
                                cEstNo = "   1" + SUBSTRING(hField:BUFFER-VALUE(jCtr),5,4).
                            IF SUBSTRING(cEstNo,5,1) = " " THEN ASSIGN
                                    SUBSTRING(cEstNo,5,1) = "0".
                        END.
                        ASSIGN  
                            hField:BUFFER-VALUE(jCtr) = cEstNo NO-ERROR.
                    END.
                END.
            END.
        END.
    END.

END PROCEDURE.


PROCEDURE pConvertInvNoTable:
    /*------------------------------------------------------------------------------
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.
    DEF VAR iIntVal AS INT NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).
    
    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN 
                    DO:
                        ASSIGN  
                            hField:BUFFER-VALUE = "x" + hField:BUFFER-VALUE NO-ERROR.
                    END.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN 
                    DO:
                        ASSIGN  
                            hField:BUFFER-VALUE(jctr) = "x" + hField:BUFFER-VALUE(jctr) NO-ERROR.
                    END.
                END.
            END.
        END.
    END.

END PROCEDURE.


PROCEDURE pConvertItemTable:
/*------------------------------------------------------------------------------
 Purpose:   Convert all RM-itemno-type fields in each record of a single table
 Notes:     Note this method of conversion effectively eliminates the collision problem, 
            since company code DOES NOT change until all other index info has been converted.
            Since new records are not created, the rec_key for related tables does note change
            Also note, this logic is dependent on customer-supplied conversion mapping
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.
    DEF VAR hMatCodeField AS HANDLE NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
    
    checkMatCode:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hMatCodeField = hBuffer:BUFFER-FIELD (iCtr).
        IF hMatCodeField:NAME = "mat-type" THEN 
            LEAVE checkMatCode.
    END.
    
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        /* Remove duplicate items if mat-type NE 8 */
        IF NOT hQuery:QUERY-OFF-END 
        AND hBuffer:TABLE EQ "item" 
        AND hCoField:BUFFER-VALUE EQ "002"
        AND hMatCodeField:BUFFER-VALUE NE "8" THEN DO: 
            hBuffer:BUFFER-DELETE().
            NEXT.
        END.
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN 
                    DO:
                        IF CAN-FIND(FIRST item WHERE 
                            item.company EQ "001" AND 
                            ITEM.i-no EQ hField:BUFFER-VALUE) THEN ASSIGN 
                                hField:BUFFER-VALUE = SUBSTRING(hField:BUFFER-VALUE,1,2) + "1" + SUBSTRING(hField:BUFFER-VALUE,3) NO-ERROR.
                    END.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN 
                    DO:
                        IF CAN-FIND(FIRST item WHERE 
                            item.company EQ "001" AND 
                            ITEM.i-no EQ hField:BUFFER-VALUE(jCtr)) THEN ASSIGN 
                                hField:BUFFER-VALUE(jCtr) = SUBSTRING(hField:BUFFER-VALUE(jCtr),1,2) + "1" + SUBSTRING(hField:BUFFER-VALUE(jCtr),3) NO-ERROR.
                    END.
                END.
            END.
        END.
    END.

END PROCEDURE.


PROCEDURE pConvertItemFGTable:
/*------------------------------------------------------------------------------
 Purpose:   Convert all FG-itemno-type fields in each record of a single table
 Notes:     Note this method of conversion effectively eliminates the collision problem, 
            since company code DOES NOT change until all other index info has been converted.
            Since new records are not created, the rec_key for related tables does note change
            Also note, this logic is dependent on customer-supplied conversion mapping
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.
    
    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "002" 
                    AND CAN-FIND (FIRST itemfg WHERE 
                        itemfg.company EQ "001" AND 
                        itemfg.i-no EQ hField:BUFFER-VALUE) THEN ASSIGN  
                        hField:BUFFER-VALUE = SUBSTRING("z" + hField:BUFFER-VALUE,1,15) NO-ERROR.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    IF hCoField:BUFFER-VALUE EQ "002" 
                    AND CAN-FIND (FIRST itemfg WHERE 
                        itemfg.company EQ "001" AND 
                        itemfg.i-no EQ hField:BUFFER-VALUE(jctr)) THEN ASSIGN    
                        hField:BUFFER-VALUE(jCtr) = SUBSTRING("z" + hField:BUFFER-VALUE(jCtr),1,15) NO-ERROR.
                END.
            END.
        END.
    END.

END PROCEDURE.


PROCEDURE pConvertLocTable:
/*------------------------------------------------------------------------------
 Purpose:   Convert all loc-type fields in each record of a single table
 Notes:     Note this method of conversion effectively eliminates the collision problem, 
            since company code DOES NOT change until all other index info has been converted.
            Since new records are not created, the rec_key for related tables does note change
            Also note, this logic is dependent on customer-supplied conversion mapping
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "001" 
                    AND hField:BUFFER-VALUE EQ "Main" THEN ASSIGN  
                        hField:BUFFER-VALUE = "PATT" NO-ERROR.
                    ELSE IF hCoField:BUFFER-VALUE EQ "002" 
                    AND hField:BUFFER-VALUE EQ "Main" THEN ASSIGN  
                        hField:BUFFER-VALUE = "COMM" NO-ERROR.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    IF hCoField:BUFFER-VALUE EQ "001" 
                    AND hField:BUFFER-VALUE(jCtr) EQ "Main" THEN ASSIGN  
                        hField:BUFFER-VALUE(jCtr) = "PATT" NO-ERROR.
                    ELSE IF hCoField:BUFFER-VALUE EQ "002" 
                    AND hField:BUFFER-VALUE(jCtr) EQ "Main" THEN ASSIGN  
                        hField:BUFFER-VALUE(jCtr) = "COMM" NO-ERROR.
                END.
            END.
        END.
    END.
    

END PROCEDURE.


PROCEDURE pConvertMcodeTable:
/*------------------------------------------------------------------------------
 Purpose:   Convert all m-code-type fields in each record of a single table
 Notes:     Note this method of conversion effectively eliminates the collision problem, 
            since company code DOES NOT change until all other index info has been converted.
            Since new records are not created, the rec_key for related tables does note change
            Also note, this logic is dependent on customer-supplied conversion mapping
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "001" THEN DO:
                        CASE STRING(hField:BUFFER-VALUE):
                            WHEN "HS" THEN ASSIGN hField:BUFFER-VALUE = "HS-P".
                            WHEN "HOTMLT" THEN ASSIGN hField:BUFFER-VALUE = "HM-P" NO-ERROR.
                            WHEN "FLATPK" THEN ASSIGN hField:BUFFER-VALUE = "FP-P" NO-ERROR.
                            WHEN "FOLD" THEN ASSIGN hField:BUFFER-VALUE = "FOLD-P" NO-ERROR.
                            WHEN "ASSMB" THEN ASSIGN hField:BUFFER-VALUE = "ASM-P" NO-ERROR.
                            WHEN "HL" THEN ASSIGN hField:BUFFER-VALUE = "HL-P" NO-ERROR.
                            WHEN "SHIP" THEN ASSIGN hField:BUFFER-VALUE = "SHIP-P" NO-ERROR.
                            WHEN "FORK" THEN ASSIGN hField:BUFFER-VALUE = "FORK-P" NO-ERROR.
                        END CASE.
                    END.
                    ELSE IF hCoField:BUFFER-VALUE EQ "002" THEN DO:
                        CASE STRING(hField:BUFFER-VALUE):
                            WHEN "HS" THEN ASSIGN hField:BUFFER-VALUE = "HS-C" NO-ERROR.
                            WHEN "HOTMLT" THEN ASSIGN hField:BUFFER-VALUE = "HM-C" NO-ERROR.
                            WHEN "FLATPK" THEN ASSIGN hField:BUFFER-VALUE = "FP-C" NO-ERROR.
                            WHEN "FOLD" THEN ASSIGN hField:BUFFER-VALUE = "FOLD-C" NO-ERROR.
                            WHEN "ASSMB" THEN ASSIGN hField:BUFFER-VALUE = "ASM-C" NO-ERROR.
                            WHEN "HL" THEN ASSIGN hField:BUFFER-VALUE = "HL-C" NO-ERROR.
                            WHEN "SHIP" THEN ASSIGN hField:BUFFER-VALUE = "SHIP-C" NO-ERROR.
                            WHEN "FORK" THEN ASSIGN hField:BUFFER-VALUE = "FORK-C" NO-ERROR.
                        END CASE.
                    END.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    IF hCoField:BUFFER-VALUE EQ "001" THEN 
                    DO:
                        CASE STRING(hField:BUFFER-VALUE(jCtr)):
                            WHEN "HS" THEN ASSIGN hField:BUFFER-VALUE(jCtr) = "HS-P".
                            WHEN "HOTMLT" THEN ASSIGN hField:BUFFER-VALUE(jCtr) = "HM-P" NO-ERROR.
                            WHEN "FLATPK" THEN ASSIGN hField:BUFFER-VALUE(jCtr) = "FP-P" NO-ERROR.
                            WHEN "FOLD" THEN ASSIGN hField:BUFFER-VALUE(jCtr) = "FOLD-P" NO-ERROR.
                            WHEN "ASSMB" THEN ASSIGN hField:BUFFER-VALUE(jCtr) = "ASM-P" NO-ERROR.
                            WHEN "HL" THEN ASSIGN hField:BUFFER-VALUE(jCtr) = "HL-P" NO-ERROR.
                            WHEN "SHIP" THEN ASSIGN hField:BUFFER-VALUE(jCtr) = "SHIP-P" NO-ERROR.
                            WHEN "FORK" THEN ASSIGN hField:BUFFER-VALUE(jCtr) = "FORK-P" NO-ERROR.
                        END CASE.
                    END.
                    ELSE IF hCoField:BUFFER-VALUE EQ "002" THEN 
                        DO:
                            CASE STRING(hField:BUFFER-VALUE(jCtr)):
                                WHEN "HS" THEN ASSIGN hField:BUFFER-VALUE(jCtr) = "HS-C" NO-ERROR.
                                WHEN "HOTMLT" THEN ASSIGN hField:BUFFER-VALUE(jCtr) = "HM-C" NO-ERROR.
                                WHEN "FLATPK" THEN ASSIGN hField:BUFFER-VALUE(jCtr) = "FP-C" NO-ERROR.
                                WHEN "FOLD" THEN ASSIGN hField:BUFFER-VALUE(jCtr) = "FOLD-C" NO-ERROR.
                                WHEN "ASSMB" THEN ASSIGN hField:BUFFER-VALUE(jCtr) = "ASM-C" NO-ERROR.
                                WHEN "HL" THEN ASSIGN hField:BUFFER-VALUE(jCtr) = "HL-C" NO-ERROR.
                                WHEN "SHIP" THEN ASSIGN hField:BUFFER-VALUE(jCtr) = "SHIP-C" NO-ERROR.
                                WHEN "FORK" THEN ASSIGN hField:BUFFER-VALUE(jCtr) = "FORK-C" NO-ERROR.
                            END CASE.
                        END.
                END.
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pConvertRaNoTable:
    /*------------------------------------------------------------------------------
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).
    
    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "002"  THEN ASSIGN  
                        hField:BUFFER-VALUE = hField:BUFFER-VALUE + 50 NO-ERROR.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN ASSIGN  
                        hField:BUFFER-VALUE(jCtr) = hField:BUFFER-VALUE(jCtr) + 50 NO-ERROR.
                END.
            END.
        END.
    END.

END PROCEDURE.


PROCEDURE pConvertRelease#Table:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN
                DO:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN 
                    DO:
                        ASSIGN 
                            hField:BUFFER-VALUE = hField:BUFFER-VALUE + 20000 NO-ERROR.
                    END.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN 
                    DO:
                        ASSIGN 
                            hField:BUFFER-VALUE(jCtr) = hField:BUFFER-VALUE(jCtr)  + 20000 NO-ERROR.
                    END.
                END.
            END.
        END.
    END.

END PROCEDURE.


PROCEDURE pCreateQuery:
/*------------------------------------------------------------------------------
 Purpose:   Returns handle to opened query
 Notes:     Used to provide record counts to ttFullTableList
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTable AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ophQuery AS HANDLE NO-UNDO.

    RUN pStatus("   Analyzing table " + ipcTable).

    DEFINE VARIABLE cQuery  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hBuffer AS HANDLE    NO-UNDO.

    IF VALID-HANDLE(hBuffer) THEN DELETE WIDGET hBuffer.                                                          
    IF VALID-HANDLE(ophQuery) THEN DELETE WIDGET ophQuery.                                                
    CREATE BUFFER hBuffer FOR TABLE ipcTable.
    CREATE QUERY ophQuery.
    ophQuery:ADD-BUFFER(hBuffer).
    ophQuery:QUERY-PREPARE("FOR EACH " + ipcTable + " BY ROWID(" + ipcTable + ")").
    ophQuery:QUERY-OPEN().

END PROCEDURE.


PROCEDURE pDeleteSimpleMerges:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    RUN pStatus("Removing merged records from company '002'...").

&scoped-def ctable ap-ctrl
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable bank
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable box-design-line
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable box-design-hdr
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable buyer
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable ce-ctrl
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable currency
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable custype
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable e-item-vend
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable e-itemfg
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable fg-ctrl
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable fgcat
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable flute
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable gl-rpt
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

    /*
    &scoped-def ctable item
        RUN pStatus("   Removing " + "{&cTable}" + " records").
        DISABLE TRIGGERS FOR LOAD OF {&cTable}.
        FOR EACH {&cTable} WHERE 
            {&cTable}.company EQ "002":
            DELETE {&cTable}.
        END.
    */
&scoped-def ctable item-bom
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.
    /*
    &scoped-def ctable itemfg
        RUN pStatus("   Removing " + "{&cTable}" + " records").
        DISABLE TRIGGERS FOR LOAD OF {&cTable}.
        FOR EACH {&cTable} WHERE 
            {&cTable}.company EQ "002":
            DELETE {&cTable}.
        END.
    */
&scoped-def ctable item-spec
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable mach-calendar
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable matprep
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable oe-ctrl
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable payment-type
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable period
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable po-ctrl
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable prod
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable prodl
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable procat
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable rm-ctrl
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable routing-mtx
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable shipto
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable soldto
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable sman
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable stax-group
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable std-code
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable sys-ctrl
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable terr
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable usercomp
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.

&scoped-def ctable ventype
    RUN pStatus("   Removing " + "{&cTable}" + " records").
    DISABLE TRIGGERS FOR LOAD OF {&cTable}.
    FOR EACH {&cTable} WHERE 
        {&cTable}.company EQ "002":
        DELETE {&cTable}.
    END.
    /*
    &scoped-def ctable company
        RUN pStatus("   Removing " + "{&cTable}" + " records").
        DISABLE TRIGGERS FOR LOAD OF {&cTable}.
        FOR EACH {&cTable} WHERE 
            {&cTable}.company EQ "002":
            DELETE {&cTable}.
        END.
    */
END PROCEDURE.

PROCEDURE pLoadCoAFromCSV:
/*------------------------------------------------------------------------------
 Purpose:   Creates COA mapping temp-table from user-supplied spreadsheet
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcCoAConvFile AS CHAR. 
    DEF VAR cLine AS CHAR NO-UNDO.
    
    RUN pStatus("pLoadCoAFromCSV...").

    INPUT FROM VALUE(ipcCoAConvFile).
    REPEAT:
        IMPORT UNFORMATTED cLine.
        CREATE ttNewCoA.
        ASSIGN 
            ttNewCoA.fromCompany = ENTRY(1,cLine,",")
            ttNewCoA.fromAcct    = ENTRY(2,cLine,",")
            ttNewCoA.toCompany   = ENTRY(3,cLine,",")
            ttNewCoA.toAcct      = ENTRY(4,cLine,",")
            ttNewCoA.AcctDesc    = ENTRY(5,cLine,",").
    END.
    


END PROCEDURE.


PROCEDURE pOutputPreMerge:
/*------------------------------------------------------------------------------
 Purpose:   Outputs ttFullTableList and ttTablesWithMergeFields to .csv files
 Notes:
------------------------------------------------------------------------------*/
    RUN pStatus("pOutputPreMerge...").

    OUTPUT TO c:\tmp\TablesToConvert.csv.
    FOR EACH ttFullTableList WHERE 
        ttFullTableList.iRecordCount GT 0:
        PUT UNFORMATTED 
            cTable + "," + 
            cTableDesc + "," +
            REPLACE(cAllIndexFields,",","|") + "," +
            REPLACE(cUIndexFields,",","|") + "," +
            STRING(iRecordCount)  + "," +
            STRING(lConvert) + CHR(10).
    END.
    OUTPUT CLOSE.

    OUTPUT TO c:\tmp\TablesWithMergeFields.csv.
    FOR EACH ttTablesWithMergeFields:
        PUT UNFORMATTED 
            cFieldType + "," +
            cTableName + "," +
            cFieldName + CHR(10).
    END.
    OUTPUT CLOSE.

END PROCEDURE.


PROCEDURE pStatus :
/*------------------------------------------------------------------------------
  Purpose:      Writes log of activities   
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcStatus AS CHAR NO-UNDO.
    DEF VAR cLogFile AS CHAR NO-UNDO.
    DEF VAR cMsgStr AS CHAR NO-UNDO.
        ASSIGN
            cLogFile = "c:\tmp\convertLog.txt"
            cMsgStr = "  " + ipcStatus.
        
    IF ipcStatus = "Initialize" THEN 
    DO:
        OUTPUT STREAM logStream TO VALUE(cLogFile).

        PUT STREAM logStream UNFORMATTED "---------------------------------------------------------------" + CHR(10).
        PUT STREAM logStream UNFORMATTED "Action Date: " + STRING(TODAY,"99/99/99") + CHR(10).
        PUT STREAM logStream UNFORMATTED "Action Time: " + STRING(TIME,"HH:MM:SS") + CHR(10).
        PUT STREAM logStream UNFORMATTED "---------------------------------------------------------------" + CHR(10).
        PUT STREAM logStream UNFORMATTED CHR(10).
        PUT STREAM logStream
            STRING(TODAY,"99/99/99") AT 1
            STRING(TIME,"HH:MM:SS") AT 12
            "Initializing log" FORMAT "x(160)" AT 25
            SKIP.
        OUTPUT STREAM logStream CLOSE.
        RETURN.
    END.
    ELSE 
    DO:
        IF INDEX(ipcStatus,"duplicate") EQ 0 THEN 
        DO:
            ASSIGN
                cMsgStr = ipcStatus.
            OUTPUT STREAM logStream TO VALUE(cLogFile) APPEND.
            PUT STREAM logStream
                STRING(TODAY,"99/99/99") AT 1
                STRING(TIME,"HH:MM:SS") AT 12
                cMsgStr FORMAT "x(160)" AT 25
                SKIP.
            OUTPUT STREAM logStream CLOSE.
        END.
    END.
        
    PROCESS EVENTS.

END PROCEDURE.



PROCEDURE pUpdateTableConvert:
/*------------------------------------------------------------------------------
 Purpose:   Updates ttFullTableList with flag that this table needs conversion
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    
    IF NOT CAN-FIND(FIRST ttFullTableList WHERE 
        ttFullTableList.cTable EQ ipcTableName AND 
        ttFullTableList.lConvert EQ TRUE) THEN 
    DO:
        FIND ttFullTableList WHERE 
            ttFullTableList.cTable EQ ipcTableName
            NO-ERROR.
        IF AVAIL ttFullTableList THEN ASSIGN 
                ttFullTableList.lConvert = TRUE.
    END. 


END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fGetNextRecKey RETURNS CHARACTER PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: REplace with call to persistent, but can't within util run directly
    ------------------------------------------------------------------------------*/    
    RETURN STRING(YEAR(TODAY),"9999")
        + STRING(MONTH(TODAY),"99")
        + STRING(DAY(TODAY),"99")
        + STRING(TIME,"99999")
        + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")
        .       
END FUNCTION.



