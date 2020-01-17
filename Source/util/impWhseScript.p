DISABLE TRIGGERS FOR LOAD OF loc.
DISABLE TRIGGERS FOR LOAD OF location.

DEF VAR cLine AS CHAR NO-UNDO.
DEF VAR cCompany AS CHAR INITIAL "001" LABEL "Company" NO-UNDO.
DEF VAR cFileName AS CHAR FORMAT "x(40)" LABEL "File" NO-UNDO.

UPDATE 
    cFileName SKIP 
    cCompany WITH FRAME a VIEW-AS DIALOG-BOX THREE-D. 

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


IF SEARCH(cFileName) EQ ? THEN 
DO:
    MESSAGE 
        "Unable to locate the specified file in the filesystem."
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

INPUT FROM VALUE (cFileName).
IMPORT UNFORMATTED cLine. /* Discard the column header line */
REPEAT:
    IMPORT UNFORMATTED cLine.
    CREATE ttImportWarehouse.
    /* Use the REPLACE since we previously replaced commas in cells with bars */
    ASSIGN 
        ttImportWarehouse.Company               = cCompany
        ttImportWarehouse.Location              = REPLACE(ENTRY(01,cLine,","),"|",",")     
        ttImportWarehouse.locCode               = REPLACE(ENTRY(01,cLine,","),"|",",")
        ttImportWarehouse.dscr                  = REPLACE(ENTRY(02,cLine,","),"|",",")     
        ttImportWarehouse.defaultBin            = REPLACE(ENTRY(03,cLine,","),"|",",")     
        ttImportWarehouse.streetAddr1           = REPLACE(ENTRY(04,cLine,","),"|",",")     
        ttImportWarehouse.streetAddr2           = REPLACE(ENTRY(05,cLine,","),"|",",")     
        ttImportWarehouse.streetAddr3           = REPLACE(ENTRY(06,cLine,","),"|",",")     
        ttImportWarehouse.subCode3              = REPLACE(ENTRY(07,cLine,","),"|",",")     
        ttImportWarehouse.subCode1              = REPLACE(ENTRY(08,cLine,","),"|",",")     
        ttImportWarehouse.Phone                 = REPLACE(ENTRY(09,cLine,","),"|",",")     
        ttImportWarehouse.fax                   = REPLACE(ENTRY(10,cLine,","),"|",",")     
        ttImportWarehouse.email                 = REPLACE(ENTRY(11,cLine,","),"|",",")     
        ttImportWarehouse.subCode4              = REPLACE(ENTRY(12,cLine,","),"|",",")     
        ttImportWarehouse.countryCode           = REPLACE(ENTRY(13,cLine,","),"|",",")     
        ttImportWarehouse.handlingCost          = INT(ENTRY(14,cLine,","))     
        ttImportWarehouse.storageCost1          = INT(ENTRY(15,cLine,","))     
        ttImportWarehouse.storageCost2          = INT(ENTRY(16,cLine,","))
        ttImportWarehouse.storageCost3          = INT(ENTRY(17,cLine,","))
        ttImportWarehouse.storageCost4          = INT(ENTRY(18,cLine,","))
        ttImportWarehouse.locationSquareFeet    = INT(ENTRY(19,cLine,","))
        ttImportWarehouse.palletCapacity        = INT(ENTRY(20,cLine,","))
        ttImportWarehouse.division              = REPLACE(ENTRY(21,cLine,","),"|",",")
        ttImportWarehouse.GlCode                = REPLACE(ENTRY(22,cLine,","),"|",",")
        ttImportWarehouse.subCode2              = REPLACE(ENTRY(23,cLine,","),"|",",")     
        ttImportWarehouse.geoLat                = DECI(ENTRY(24,cLine,","))
        ttImportWarehouse.geoLong               = DECI(ENTRY(25,cLine,","))
        ttImportWarehouse.externalID1           = REPLACE(ENTRY(26,cLine,","),"|",",")
        ttImportWarehouse.notes                 = REPLACE(ENTRY(27,cLine,","),"|",",")
        ttImportWarehouse.cActive               = REPLACE(ENTRY(28,cLine,","),"|",",")     
        ttImportWarehouse.isAPIEnabled          = REPLACE(ENTRY(29,cLine,","),"|",",")     
        ttImportWarehouse.lActive               = REPLACE(ENTRY(30,cLine,","),"|",",")     
        ttImportWarehouse.lovOwner              = REPLACE(ENTRY(31,cLine,","),"|",",")    
        . 
    
    IF ttImportWarehouse.isAPIEnabled EQ "Yes" 
        OR ttImportWarehouse.isAPIEnabled EQ "Y" 
        OR ttImportWarehouse.isAPIEnabled EQ "True" THEN
        ttImportWarehouse.isAPIEnabled = "Y".
    ELSE ttImportWarehouse.isAPIEnabled = "N".

    IF ttImportWarehouse.lActive EQ "Yes" 
        OR ttImportWarehouse.lActive EQ "Y"
        OR ttImportWarehouse.lActive EQ "True" THEN
        ttImportWarehouse.lActive = "Y".
    ELSE ttImportWarehouse.lActive = "N".

    IF ttImportWarehouse.cActive EQ "Yes" 
        OR ttImportWarehouse.cActive EQ "Y"
        OR ttImportWarehouse.cActive EQ "True" THEN
        ttImportWarehouse.cActive = "Y".
    ELSE ttImportWarehouse.cActive = "N".
END.

FOR EACH ttImportWarehouse:
    FIND FIRST loc EXCLUSIVE-LOCK 
        WHERE loc.company EQ ttImportWarehouse.Company
        AND loc.loc EQ ttImportWarehouse.locCode
        NO-ERROR.
    
    IF NOT AVAILABLE loc THEN 
    DO:
        CREATE loc .
        ASSIGN
            loc.company           = ttImportWarehouse.Company 
            loc.loc               = ttImportWarehouse.locCode
            loc.rec_key = STRING(YEAR(TODAY),"9999")
                + STRING(MONTH(TODAY),"99")
                + STRING(DAY(TODAY),"99")
                + STRING(TIME,"99999")
                + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999"). 
    END.
    IF AVAIL loc THEN
        FIND FIRST location EXCLUSIVE-LOCK 
            WHERE location.rec_key EQ loc.addrRecKey
            AND location.locationCode EQ ttImportWarehouse.locCode
            NO-ERROR.

    IF NOT AVAIL location THEN 
    DO:    
        CREATE location.
        ASSIGN 
            location.company      = ttImportWarehouse.Company
            location.locationCode = loc.loc
            location.rec_key = STRING(YEAR(TODAY),"9999")
                            + STRING(MONTH(TODAY),"99")
                            + STRING(DAY(TODAY),"99")
                            + STRING(TIME,"99999")
                            + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999"). 
    END.
    
    ASSIGN 
        loc.dscr                = ttImportWarehouse.dscr                                     
        loc.handlingCost        = ttImportWarehouse.handlingCost                       
        loc.storageCost[1]      = ttImportWarehouse.storageCost1                                             
        loc.storageCost[2]      = ttImportWarehouse.storageCost1                                             
        loc.storageCost[3]      = ttImportWarehouse.storageCost1
        loc.storageCost[4]      = ttImportWarehouse.storageCost1                                   
        loc.locationSquareFeet  = ttImportWarehouse.locationSquareFeet                    
        loc.palletCapacity      = ttImportWarehouse.palletCapacity                           
        loc.division            = ttImportWarehouse.division                         
        loc.GlCode              = ttImportWarehouse.GLCode                                   
        loc.Active              = LOGICAL(ttImportWarehouse.cActive)     
        loc.isAPIEnabled        = LOGICAL(ttImportWarehouse.isAPIEnabled)                                 
        loc.Owner               = ttImportWarehouse.lovOwner
        loc.addrRecKey          = location.rec_key
        .
    
    ASSIGN 
        location.defaultBin     = ttImportWarehouse.defaultBin
        location.streetAddr[1]  = ttImportWarehouse.streetAddr1
        location.streetAddr[2]  = ttImportWarehouse.streetAddr2
        location.streetAddr[3]  = ttImportWarehouse.streetAddr3
        location.subCode3       = ttImportWarehouse.subCode3
        location.subCode1       = ttImportWarehouse.subCode1
        location.phone          = ttImportWarehouse.phone
        location.fax            = ttImportWarehouse.fax
        location.email          = ttImportWarehouse.email
        location.subCode4       = ttImportWarehouse.subCode4
        location.countryCode    = ttImportWarehouse.countryCode
        location.subCode2       = ttImportWarehouse.subCode2
        location.geoLat         = ttImportWarehouse.geoLat
        location.geoLong        = ttImportWarehouse.geoLong
        location.externalID[1]  = ttImportWarehouse.externalID1
        location.notes          = ttImportWarehouse.notes
        location.lActive        = LOGICAL(ttImportWarehouse.lActive)
        .
        
END.    