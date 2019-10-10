
/*------------------------------------------------------------------------
    File        : ImportLocation.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Location only 	

    Author(s)   : Sewa Singh
    Created     : Thur Oct 10 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportLocation
    FIELD Company               AS CHARACTER 
    FIELD Location              AS CHARACTER 
    FIELD locCode               AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Code" HELP "Required - Size:5" 
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
    FIELD subCode2              AS CHARACTER FORMAT "x(24)" COLUMN-LABEL "County" HELP "Optional - Size:24"
    FIELD geoLat                AS DECIMAL FORMAT "->>9.99999<<" COLUMN-LABEL "Lat" HELP "Optional - Decimal"
    FIELD geoLong               AS DECIMAL FORMAT "->>9.99999<<" COLUMN-LABEL "long" HELP "Optional - decimal"
    FIELD externalID1           AS CHARACTER FORMAT "x(24)" COLUMN-LABEL "Ext. Code" HELP "Optional - Size:24"
    FIELD notes                 AS CHARACTER FORMAT "x(500)" COLUMN-LABEL "Notes" HELP "Optional - Size:500"
    FIELD lActive               AS CHARACTER FORMAT "x" COLUMN-LABEL "Consignment" HELP "Optional - Y or N (blank=N)"
    
    .
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data*/
 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
 /*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportLocation"}


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportLocation FOR ttImportLocation.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE riNote AS ROWID NO-UNDO.

    FIND FIRST location EXCLUSIVE-LOCK 
        WHERE location.company EQ ipbf-ttImportLocation.Company
        AND location.locationCode EQ ipbf-ttImportLocation.locCode
        NO-ERROR.

    IF NOT AVAILABLE location THEN 
    DO:
        ASSIGN 
            iopiAdded = iopiAdded + 1.
        CREATE location.
        ASSIGN 
            location.company      = ipbf-ttImportLocation.Company
            location.locationCode = ipbf-ttImportLocation.locCODE .
    END.
                                                                                                                                     
    /*Main assignments - Blanks ignored if it is valid to blank- or zero-out a field */                                        
    RUN pAssignValueC (ipbf-ttImportLocation.defaultBin, iplIgnoreBlanks, INPUT-OUTPUT location.defaultBin).                                                   
    RUN pAssignValueC (ipbf-ttImportLocation.streetAddr1, iplIgnoreBlanks, INPUT-OUTPUT location.streetAddr[1]).                               
    RUN pAssignValueC (ipbf-ttImportLocation.streetAddr2, iplIgnoreBlanks, INPUT-OUTPUT location.streetAddr[2]).                                                   
    RUN pAssignValueC (ipbf-ttImportLocation.streetAddr3, iplIgnoreBlanks, INPUT-OUTPUT location.streetAddr[3]).                                                   
    RUN pAssignValueC (ipbf-ttImportLocation.subCode3, iplIgnoreBlanks, INPUT-OUTPUT location.subCode3).                                       
    RUN pAssignValueC (ipbf-ttImportLocation.subCode1, iplIgnoreBlanks, INPUT-OUTPUT location.subCode1).                                         
    RUN pAssignValueC (ipbf-ttImportLocation.Phone, iplIgnoreBlanks, INPUT-OUTPUT location.phone).                      
    RUN pAssignValueC (ipbf-ttImportLocation.fax, iplIgnoreBlanks, INPUT-OUTPUT location.fax).                                 
    RUN pAssignValueC (ipbf-ttImportLocation.email, iplIgnoreBlanks, INPUT-OUTPUT location.email).                                     
    RUN pAssignValuec (ipbf-ttImportLocation.subCode4, iplIgnoreBlanks, INPUT-OUTPUT location.subCode4).                                                 
    RUN pAssignValueC (ipbf-ttImportLocation.countryCode, iplIgnoreBlanks, INPUT-OUTPUT location.countryCode).                   
    RUN pAssignValueC (ipbf-ttImportLocation.subCode2, iplIgnoreBlanks, INPUT-OUTPUT location.subCode2).                                         
    RUN pAssignValueD (ipbf-ttImportLocation.geoLat, iplIgnoreBlanks, INPUT-OUTPUT location.geoLat).                                 
    RUN pAssignValueD (ipbf-ttImportLocation.geoLong, iplIgnoreBlanks, INPUT-OUTPUT location.geoLong).                                       
    RUN pAssignValueC (ipbf-ttImportLocation.externalID1, iplIgnoreBlanks, INPUT-OUTPUT location.externalID[1]).                                 
    RUN pAssignValueC (ipbf-ttImportLocation.notes, iplIgnoreBlanks, INPUT-OUTPUT location.notes).                                   
    RUN pAssignValueC (ipbf-ttImportLocation.lActive, iplIgnoreBlanks, INPUT-OUTPUT location.lActive).                             
                                      

END PROCEDURE.                                                                                                                 
                                                                                                                               
PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportLocation FOR ttImportLocation.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE NO-UNDO.
    DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportLocation FOR ttImportLocation.

    RUN util/Validate.p PERSISTENT SET hdValidator.

    oplValid = YES.
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportLocation.locCode EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Location Code is Blank".
    END.
    
    /*Check for Duplicate Import Record and Ignore It*/ 
    IF oplValid THEN 
    DO:
        /*FIND FIRST bf-ttImportLocation NO-LOCK                                     */
        /*    WHERE bf-ttImportLocation.Company EQ ipbf-ttImportLocation.Company     */
        /*    AND bf-ttImportLocation.CODE EQ ipbf-ttImportLocation.CODE             */
        /*    AND ROWID(bf-ttImportLocation) NE ROWID(ipbf-ttImportLocation)         */
        /*    NO-ERROR.                                                              */
        /*IF AVAILABLE bf-ttImportLocation THEN                                      */
        /*    ASSIGN                                                                 */
        /*        oplValid = NO                                                      */
        /*        opcNote  = "Duplicate Record in Import File".                      */
    END.
    /*Determine if Add or Update*/
    IF oplValid THEN 
    DO:
        FIND FIRST location NO-LOCK 
            WHERE location.company EQ ipbf-ttImportLocation.Company
            AND location.locationCode EQ ipbf-ttImportLocation.locCode
            NO-ERROR .
        IF AVAIL location THEN
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
        
        IF oplValid AND ipbf-ttImportLocation.defaultBin NE "" THEN 
            RUN pIsValidFGBinForLoc IN hdValidator (ipbf-ttImportLocation.defaultBin, ipbf-ttImportLocation.locCode, NO, ipbf-ttImportLocation.Company, OUTPUT oplValid, OUTPUT cValidNote).
        IF NOT oplValid AND ipbf-ttImportLocation.defaultBin NE "" THEN 
            RUN pIsValidRMBinForLoc IN hdValidator (ipbf-ttImportLocation.defaultBin, ipbf-ttImportLocation.locCode, NO, ipbf-ttImportLocation.Company, OUTPUT oplValid, OUTPUT cValidNote).
        IF NOT oplValid AND ipbf-ttImportLocation.defaultBin NE "" THEN 
            RUN pIsValidWipBinForLoc IN hdValidator (ipbf-ttImportLocation.defaultBin, ipbf-ttImportLocation.locCode, NO, ipbf-ttImportLocation.Company, OUTPUT oplValid, OUTPUT cValidNote).
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.

END PROCEDURE.

