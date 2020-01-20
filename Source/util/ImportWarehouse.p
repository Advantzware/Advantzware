
/*------------------------------------------------------------------------
    File        : ImportWareHouse.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Location only 	

    Author(s)   : Sewa Singh
    Created     : Thur Oct 10 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportWarehouse
    FIELD Company               AS CHARACTER 
    FIELD Location              AS CHARACTER 
    FIELD locCode               AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Warehouse" HELP "Required - Size:5"
    FIELD dscr                  AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "Name" HELP "Optional - Size:30" 
    FIELD defaultBin            AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Default Bin" HELP "Optional - Size:8" 
    FIELD streetAddr1           AS CHARACTER FORMAT "x(60)" COLUMN-LABEL "Address1" HELP "Optional - Size:60" 
    FIELD streetAddr2           AS CHARACTER FORMAT "x(60)" COLUMN-LABEL "Address2" HELP "Optional - Size:60" 
    FIELD streetAddr3           AS CHARACTER FORMAT "x(60)" COLUMN-LABEL "Address3" HELP "Optional - Size:60" 
    FIELD subCode3              AS CHARACTER FORMAT "x(24)" COLUMN-LABEL "City" HELP "Optional - Size:24"
    FIELD subCode1              AS CHARACTER FORMAT "x(24)" COLUMN-LABEL "St/Prov" HELP "Optional - - Size:24"
    FIELD Phone                 AS CHARACTER FORMAT "x(40)" COLUMN-LABEL "Phone" HELP "Optional - Size:40"
    FIELD fax                   AS CHARACTER FORMAT "x(40)" COLUMN-LABEL "Fax" HELP "Optional - Size:40"
    FIELD email                 AS CHARACTER FORMAT "x(60)" COLUMN-LABEL "Email" HELP "Optional - Size:60"
    FIELD subCode4              AS CHARACTER FORMAT "x(24)" COLUMN-LABEL "Zip/Post" HELP "Optional - Size:24"
    FIELD countryCode           AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Country" HELP "Optional - Size:3"
    FIELD handlingCost          AS INTEGER FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Handling" HELP "Optional - Decimal"
    FIELD storageCost1          AS INTEGER FORMAT "->>,>>9.99" COLUMN-LABEL "Storage 1" HELP "Optional - Decimal"
    FIELD storageCost2          AS INTEGER FORMAT "->>,>>9.99" COLUMN-LABEL "Storage 2" HELP "Optional - Decimal"
    FIELD storageCost3          AS INTEGER FORMAT "->>,>>9.99" COLUMN-LABEL "Storage 3" HELP "Optional - Decimal"
    FIELD storageCost4          AS INTEGER FORMAT "->>,>>9.99" COLUMN-LABEL "Storage 4" HELP "Optional - Decimal"
    FIELD locationSquareFeet    AS INTEGER FORMAT ">>>,>>>,>>9" COLUMN-LABEL "Location Gross" HELP "Optional - Integer"
    FIELD palletCapacity        AS INTEGER FORMAT ">>>,>>>,>>9" COLUMN-LABEL "Pallets" HELP "Optional - Integer"
    FIELD division              AS CHARACTER FORMAT "x(12)" COLUMN-LABEL "Division" HELP "Optional - Size:12"
    FIELD GlCode                AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "GL Code" HELP "Optional - Size:20"
    FIELD subCode2              AS CHARACTER FORMAT "x(24)" COLUMN-LABEL "County" HELP "Optional - Size:24"
    FIELD geoLat                AS DECIMAL FORMAT "->>9.99999<<" COLUMN-LABEL "Lat" HELP "Optional - Decimal"
    FIELD geoLong               AS DECIMAL FORMAT "->>9.99999<<" COLUMN-LABEL "long" HELP "Optional - decimal"
    FIELD externalID1           AS CHARACTER FORMAT "x(24)" COLUMN-LABEL "Ext. Code" HELP "Optional - Size:24"
    FIELD notes                 AS CHARACTER FORMAT "x(500)" COLUMN-LABEL "Notes" HELP "Optional - Size:500"
    FIELD cActive               AS CHARACTER FORMAT "x" COLUMN-LABEL "Active" HELP "Optional - Yes or No (blank=No)"
    FIELD isAPIEnabled          AS CHARACTER FORMAT "x" COLUMN-LABEL "API Enabled" HELP "Optional - Yes or No (blank=No)"
    FIELD lActive               AS CHARACTER FORMAT "x" COLUMN-LABEL "Consignment" HELP "Optional - Yes or No (blank=No)"
    FIELD lovOwner              AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Owner" HELP "Optional - Size:8"
    .
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data*/
 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
 /*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportWarehouse"}


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportWarehouse FOR ttImportWarehouse.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE riNote AS ROWID NO-UNDO.

    FIND FIRST loc EXCLUSIVE-LOCK 
        WHERE loc.company EQ ipbf-ttImportWarehouse.Company
        AND loc.loc EQ ipbf-ttImportWarehouse.locCode
        NO-ERROR.
    IF AVAIL loc THEN
        FIND FIRST location EXCLUSIVE-LOCK 
           WHERE location.rec_key EQ loc.addrRecKey
           AND location.locationCode EQ ipbf-ttImportWarehouse.locCode
        NO-ERROR.

    IF NOT AVAILABLE loc THEN 
    DO:
        ASSIGN 
            iopiAdded = iopiAdded + 1.

        CREATE loc .
        CREATE location.
        ASSIGN
            loc.company           = ipbf-ttImportWarehouse.Company 
            loc.loc               = ipbf-ttImportWarehouse.locCODE
            location.company      = ipbf-ttImportWarehouse.Company
            location.locationCode = loc.loc
            loc.rec_key           = DYNAMIC-FUNCTION("sfGetNextRecKey") 
            location.rec_key      = DYNAMIC-FUNCTION("sfGetNextRecKey") 
            loc.addrRecKey        = location.rec_key.
    END.
                                                                                                                                     
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */  
    RUN pAssignValueC (ipbf-ttImportWarehouse.dscr, iplIgnoreBlanks, INPUT-OUTPUT loc.dscr).                                                   
    RUN pAssignValueD (ipbf-ttImportWarehouse.handlingCost, iplIgnoreBlanks, INPUT-OUTPUT loc.handlingCost).                               
    RUN pAssignValueD (ipbf-ttImportWarehouse.storageCost1, iplIgnoreBlanks, INPUT-OUTPUT loc.storageCost[1]).                                                   
    RUN pAssignValueD (ipbf-ttImportWarehouse.storageCost2, iplIgnoreBlanks, INPUT-OUTPUT loc.storageCost[2]).                                                   
    RUN pAssignValueD (ipbf-ttImportWarehouse.storageCost3, iplIgnoreBlanks, INPUT-OUTPUT loc.storageCost[3]).                                       
    RUN pAssignValueD (ipbf-ttImportWarehouse.storageCost4, iplIgnoreBlanks, INPUT-OUTPUT loc.storageCost[4]).                                         
    RUN pAssignValueI (ipbf-ttImportWarehouse.locationSquareFeet, iplIgnoreBlanks, INPUT-OUTPUT loc.locationSquareFeet).                      
    RUN pAssignValueI (ipbf-ttImportWarehouse.palletCapacity, iplIgnoreBlanks, INPUT-OUTPUT loc.palletCapacity).                                 
    RUN pAssignValueC (ipbf-ttImportWarehouse.division, iplIgnoreBlanks, INPUT-OUTPUT loc.division).                                     
    RUN pAssignValuec (ipbf-ttImportWarehouse.GlCode, iplIgnoreBlanks, INPUT-OUTPUT loc.GlCode).                                                 
    RUN pAssignValueC (ipbf-ttImportWarehouse.cActive, iplIgnoreBlanks, INPUT-OUTPUT loc.Active).                   
    RUN pAssignValueC (ipbf-ttImportWarehouse.isAPIEnabled, iplIgnoreBlanks, INPUT-OUTPUT loc.isAPIEnabled).                                         
    RUN pAssignValueC (ipbf-ttImportWarehouse.lovOwner, iplIgnoreBlanks, INPUT-OUTPUT loc.Owner).

    RUN pAssignValueC (ipbf-ttImportWarehouse.defaultBin, iplIgnoreBlanks, INPUT-OUTPUT location.defaultBin).                                                   
    RUN pAssignValueC (ipbf-ttImportWarehouse.streetAddr1, iplIgnoreBlanks, INPUT-OUTPUT location.streetAddr[1]).                               
    RUN pAssignValueC (ipbf-ttImportWarehouse.streetAddr2, iplIgnoreBlanks, INPUT-OUTPUT location.streetAddr[2]).                                                   
    RUN pAssignValueC (ipbf-ttImportWarehouse.streetAddr3, iplIgnoreBlanks, INPUT-OUTPUT location.streetAddr[3]).                                                   
    RUN pAssignValueC (ipbf-ttImportWarehouse.subCode3, iplIgnoreBlanks, INPUT-OUTPUT location.subCode3).                                       
    RUN pAssignValueC (ipbf-ttImportWarehouse.subCode1, iplIgnoreBlanks, INPUT-OUTPUT location.subCode1).                                         
    RUN pAssignValueC (ipbf-ttImportWarehouse.Phone, iplIgnoreBlanks, INPUT-OUTPUT location.phone).                      
    RUN pAssignValueC (ipbf-ttImportWarehouse.fax, iplIgnoreBlanks, INPUT-OUTPUT location.fax).                                 
    RUN pAssignValueC (ipbf-ttImportWarehouse.email, iplIgnoreBlanks, INPUT-OUTPUT location.email).                                     
    RUN pAssignValuec (ipbf-ttImportWarehouse.subCode4, iplIgnoreBlanks, INPUT-OUTPUT location.subCode4).                                                 
    RUN pAssignValueC (ipbf-ttImportWarehouse.countryCode, iplIgnoreBlanks, INPUT-OUTPUT location.countryCode).                   
    RUN pAssignValueC (ipbf-ttImportWarehouse.subCode2, iplIgnoreBlanks, INPUT-OUTPUT location.subCode2).                                         
    RUN pAssignValueD (ipbf-ttImportWarehouse.geoLat, iplIgnoreBlanks, INPUT-OUTPUT location.geoLat).                                 
    RUN pAssignValueD (ipbf-ttImportWarehouse.geoLong, iplIgnoreBlanks, INPUT-OUTPUT location.geoLong).                                       
    RUN pAssignValueC (ipbf-ttImportWarehouse.externalID1, iplIgnoreBlanks, INPUT-OUTPUT location.externalID[1]).                                 
    RUN pAssignValueC (ipbf-ttImportWarehouse.notes, iplIgnoreBlanks, INPUT-OUTPUT location.notes).                                   
    RUN pAssignValueC (ipbf-ttImportWarehouse.lActive, iplIgnoreBlanks, INPUT-OUTPUT location.lActive).                             
                                      

END PROCEDURE.                                                                                                                 
                                                                                                                               
PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportWarehouse FOR ttImportWarehouse.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE NO-UNDO.
    DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportWarehouse FOR ttImportWarehouse.

    RUN util/Validate.p PERSISTENT SET hdValidator.

    oplValid = YES.
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportWarehouse.locCode EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Location Code is Blank".
    END.
    
    /*Check for Duplicate Import Record and Ignore It*/ 
    IF oplValid THEN 
    DO:
        /*FIND FIRST bf-ttImportWarehouse NO-LOCK                                     */
        /*    WHERE bf-ttImportWarehouse.Company EQ ipbf-ttImportWarehouse.Company     */
        /*    AND bf-ttImportWarehouse.CODE EQ ipbf-ttImportWarehouse.CODE             */
        /*    AND ROWID(bf-ttImportWarehouse) NE ROWID(ipbf-ttImportWarehouse)         */
        /*    NO-ERROR.                                                              */
        /*IF AVAILABLE bf-ttImportWarehouse THEN                                      */
        /*    ASSIGN                                                                 */
        /*        oplValid = NO                                                      */
        /*        opcNote  = "Duplicate Record in Import File".                      */
    END.
    /*Determine if Add or Update*/
    IF oplValid THEN 
    DO:
        FIND FIRST loc NO-LOCK 
            WHERE loc.company EQ ipbf-ttImportWarehouse.Company
            AND loc.loc EQ ipbf-ttImportWarehouse.locCode
            NO-ERROR .
        IF AVAIL loc THEN
        DO: 
            IF NOT iplUpdateDuplicates THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Duplicate record exists"
                    .
            ELSE
                ASSIGN 
                    oplValid = YES
                    opcNote = "Update existing record"
                    .        
        END.
        ELSE 
            ASSIGN 
                oplValid = YES
                opcNote = "Add record"
                .
        
    END.
    
    /*Field Level Validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:
        
        IF oplValid AND ipbf-ttImportWarehouse.defaultBin NE "" THEN 
            RUN pIsValidFGBinForLoc IN hdValidator (ipbf-ttImportWarehouse.defaultBin, ipbf-ttImportWarehouse.locCode, NO, ipbf-ttImportWarehouse.Company, OUTPUT oplValid, OUTPUT cValidNote).
        IF NOT oplValid AND ipbf-ttImportWarehouse.defaultBin NE "" THEN 
            RUN pIsValidRMBinForLoc IN hdValidator (ipbf-ttImportWarehouse.defaultBin, ipbf-ttImportWarehouse.locCode, NO, ipbf-ttImportWarehouse.Company, OUTPUT oplValid, OUTPUT cValidNote).
        IF NOT oplValid AND ipbf-ttImportWarehouse.defaultBin NE "" THEN 
            RUN pIsValidWipBinForLoc IN hdValidator (ipbf-ttImportWarehouse.defaultBin, ipbf-ttImportWarehouse.locCode, NO, ipbf-ttImportWarehouse.Company, OUTPUT oplValid, OUTPUT cValidNote).
    END.

    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.

    IF ipbf-ttImportWarehouse.isAPIEnabled EQ "Yes" 
    OR ipbf-ttImportWarehouse.isAPIEnabled EQ "Y" 
    OR ipbf-ttImportWarehouse.isAPIEnabled EQ "True" THEN
         ipbf-ttImportWarehouse.isAPIEnabled = "Y".
    ELSE ipbf-ttImportWarehouse.isAPIEnabled = "N".

    IF ipbf-ttImportWarehouse.lActive EQ "Yes" 
    OR ipbf-ttImportWarehouse.lActive EQ "Y"
    OR ipbf-ttImportWarehouse.lActive EQ "True" THEN
         ipbf-ttImportWarehouse.lActive = "Y".
    ELSE ipbf-ttImportWarehouse.lActive = "N".

    IF ipbf-ttImportWarehouse.cActive EQ "Yes" 
    OR ipbf-ttImportWarehouse.cActive EQ "Y"
    OR ipbf-ttImportWarehouse.cActive EQ "True" THEN
         ipbf-ttImportWarehouse.cActive = "Y".
    ELSE ipbf-ttImportWarehouse.cActive = "N".


END PROCEDURE.

