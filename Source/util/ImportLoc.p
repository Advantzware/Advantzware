/*------------------------------------------------------------------------
    File        : ImportLoc.p
    Purpose     : 
    Syntax      :
    Description : Import Program (Persistent) for Configuring and Processing the Import for Loc and Location
    Author(s)   : MYT
    Created     : Sun Jan 5 10:37:00 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportLoc
    FIELD company                 AS CHAR    FORMAT "x(3)"          COLUMN-LABEL "Company"         
    FIELD location                AS CHAR    FORMAT "x(5)"          COLUMN-LABEL "Loc Code"         HELP "Required. SIZE: 5" 
    FIELD loc_dscr                AS CHAR    FORMAT "x(30)"         COLUMN-LABEL "Description"      HELP "Required. SIZE: 30"    
    FIELD loc_active              AS LOGI    FORMAT "yes/no"        COLUMN-LABEL "Active"           HELP "Required. Yes or No - Default Yes"
    FIELD loc_isAPIEnabled        AS LOGI    FORMAT "yes/no"        COLUMN-LABEL "API Enabled"      HELP "Required. Yes or No - Default No"   
    FIELD locn_countryCode        AS CHAR    FORMAT "x(3)"          COLUMN-LABEL "Country Code"     HELP "Optional. SIZE: 3"     
    FIELD locn_subCode1           AS CHAR    FORMAT "x(24)"         COLUMN-LABEL "State/Prov"       HELP "Optional. SIZE: 24"   
    FIELD locn_subCode2           AS CHAR    FORMAT "x(24)"         COLUMN-LABEL "County/Parish"    HELP "Optional. SIZE: 24"      
    FIELD locn_subCode3           AS CHAR    FORMAT "x(24)"         COLUMN-LABEL "City"             HELP "Optional. SIZE: 24"         
    FIELD locn_subCode4           AS CHAR    FORMAT "x(24)"         COLUMN-LABEL "Zip/Post"         HELP "Optional. SIZE: 24"
    FIELD locn_streetAddr1        AS CHAR    FORMAT "x(60)"         COLUMN-LABEL "Address"          HELP "Optional. SIZE: 60x6"
    FIELD locn_streetAddr2        AS CHAR    FORMAT "x(60)"         COLUMN-LABEL "Address"          HELP "Optional. SIZE: 60x6"
    FIELD locn_streetAddr3        AS CHAR    FORMAT "x(60)"         COLUMN-LABEL "Address"          HELP "Optional. SIZE: 60x6"
    FIELD locn_streetAddr4        AS CHAR    FORMAT "x(60)"         COLUMN-LABEL "Address"          HELP "Optional. SIZE: 60x6"
    FIELD locn_streetAddr5        AS CHAR    FORMAT "x(60)"         COLUMN-LABEL "Address"          HELP "Optional. SIZE: 60x6"
    FIELD locn_streetAddr6        AS CHAR    FORMAT "x(60)"         COLUMN-LABEL "Address"          HELP "Optional. SIZE: 60x6"
    FIELD locn_building           AS CHAR    FORMAT "x(24)"         COLUMN-LABEL "Building"         HELP "Optional. SIZE: 24"
    FIELD locn_room               AS CHAR    FORMAT "x(12)"         COLUMN-LABEL "Room"             HELP "Optional. SIZE: 12"
    FIELD locn_workCenter         AS CHAR    FORMAT "x(12)"         COLUMN-LABEL "WorkCenter"       HELP "Optional. SIZE: 12"  
    FIELD locn_defaultBin         AS CHAR    FORMAT "x(8)"          COLUMN-LABEL "Default Bin"      HELP "Optional. SIZE: 8"   
    FIELD locn_geoLat             AS DECI    FORMAT "->>9.99999<<"  COLUMN-LABEL "Latitude"         HELP "Optional. Decimal"
    FIELD locn_geoLong            AS DECI    FORMAT "->>9.99999<<"  COLUMN-LABEL "Longitude"        HELP "Optional. Decimal" 
    FIELD locn_geoAlt             AS DECI    FORMAT "->>>>9.99<<<"  COLUMN-LABEL "Altitude"         HELP "Optional. Decimal"
    FIELD locn_notes              AS CHAR    FORMAT "x(60)"         COLUMN-LABEL "Notes"            HELP "Optional. Size: 60"         
    FIELD locn_externalID1        AS CHAR    FORMAT "x(24)"         COLUMN-LABEL "Ext. ID"          HELP "Optional. Size:24x5"
    FIELD locn_externalID2        AS CHAR    FORMAT "x(24)"         COLUMN-LABEL "Ext. ID"          HELP "Optional. Size:24x5"
    FIELD locn_externalID3        AS CHAR    FORMAT "x(24)"         COLUMN-LABEL "Ext. ID"          HELP "Optional. Size:24x5"
    FIELD locn_externalID4        AS CHAR    FORMAT "x(24)"         COLUMN-LABEL "Ext. ID"          HELP "Optional. Size:24x5"
    FIELD locn_externalID5        AS CHAR    FORMAT "x(24)"         COLUMN-LABEL "Ext. ID"          HELP "Optional. Size:24x5"
    FIELD locn_phone              AS CHAR    FORMAT "x(40)"         COLUMN-LABEL "Phone"            HELP "Optional. Size: 40"
    FIELD locn_fax                AS CHAR    FORMAT "x(40)"         COLUMN-LABEL "Fax"              HELP "Optional. Size: 40"         
    FIELD locn_email              AS CHAR    FORMAT "x(60)"         COLUMN-LABEL "Email"            HELP "Optional. Size: 60"
    FIELD loc_whs-chg             AS DECI    FORMAT ">>9.9999"      COLUMN-LABEL "Whse Charge"      HELP "Optional. Decimal"   
    FIELD loc_wc-uom              AS CHAR    FORMAT "x(3)"          COLUMN-LABEL "Chg UoM"          HELP "Optional. Size: 3"
    FIELD loc_handlingCost        AS DECI    FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Hand. Cost"       HELP "Optional. Decimal"  
    FIELD loc_storageCost1        AS DECI    FORMAT "->>,>>9.99"    COLUMN-LABEL "Storg Cost"       HELP "Optional. Decimal"
    FIELD loc_storageCost2        AS DECI    FORMAT "->>,>>9.99"    COLUMN-LABEL "Storg Cost"       HELP "Optional. Decimal"
    FIELD loc_storageCost3        AS DECI    FORMAT "->>,>>9.99"    COLUMN-LABEL "Storg Cost"       HELP "Optional. Decimal"
    FIELD loc_storageCost4        AS DECI    FORMAT "->>,>>9.99"    COLUMN-LABEL "Storg Cost"       HELP "Optional. Decimal"
    FIELD loc_division            AS CHAR    FORMAT "x(12)"         COLUMN-LABEL "Division"         HELP "Optional. Size: 12"
    FIELD loc_locationSquareFeet  AS INTE    FORMAT ">>>,>>>,>>9"   COLUMN-LABEL "Loc. SqFt"        HELP "Optional. Integer" 
    FIELD loc_palletCapacity      AS INTE    FORMAT ">>>,>>>,>>9"   COLUMN-LABEL "Pallet Cap."      HELP "Optional. Integer"   
    FIELD loc_owner               AS CHAR    FORMAT "x(12)"         COLUMN-LABEL "Owner"            HELP "Optional. Size: 12"
    FIELD loc_glCode              AS CHAR    FORMAT "x(20)"         COLUMN-LABEL "GL Code"          HELP "Optional. Size: 20"         
    .

DEFINE VARIABLE giIndexOffset AS INTEGER   NO-UNDO INIT 1. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportLoc"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportLoc FOR ttImportLoc.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportLoc FOR ttImportLoc.
    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportLoc.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportLoc.Location EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Loc Code".
    END.
    
    IF ipbf-ttImportLoc.loc_active EQ ? THEN
        ipbf-ttImportLoc.loc_active = YES.
    IF ipbf-ttImportLoc.loc_isAPIEnabled EQ ? THEN
        ipbf-ttImportLoc.loc_isAPIEnabled = NO.
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportLoc FOR ttImportLoc.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
     
    IF AVAILABLE ipbf-ttImportLoc THEN DO: 
        FIND FIRST location EXCLUSIVE WHERE 
            location.company EQ ipbf-ttImportLoc.company and
            location.locationCode EQ ipbf-ttImportLoc.location
            NO-ERROR.
        IF NOT AVAIL location THEN DO:
            iopiAdded = iopiAdded + 1.
            CREATE location.
            ASSIGN 
                location.company        = ipbf-ttImportLoc.company
                location.locationCode   = ipbf-ttImportLoc.location.
        END.
        ASSIGN 
            location.countryCode    = ipbf-ttImportLoc.locn_CountryCode
            location.subCode1       = ipbf-ttImportLoc.locn_subCode1
            location.subCode2       = ipbf-ttImportLoc.locn_subCode2
            location.subCode3       = ipbf-ttImportLoc.locn_subCode3
            location.subCode4       = ipbf-ttImportLoc.locn_subCode4
            location.streetAddr[1]  = ipbf-ttImportLoc.locn_streetAddr1
            location.streetAddr[2]  = ipbf-ttImportLoc.locn_streetAddr2
            location.streetAddr[3]  = ipbf-ttImportLoc.locn_streetAddr3
            location.streetAddr[4]  = ipbf-ttImportLoc.locn_streetAddr4
            location.streetAddr[5]  = ipbf-ttImportLoc.locn_streetAddr5
            location.streetAddr[6]  = ipbf-ttImportLoc.locn_streetAddr6
            location.description    = ipbf-ttImportLoc.loc_dscr
            location.building       = ipbf-ttImportLoc.locn_building
            location.room           = ipbf-ttImportLoc.locn_room
            location.workCenter     = ipbf-ttImportLoc.locn_workCenter
            location.rec_key        = DYNAMIC-FUNCTION("sfGetNextRecKey")
            location.defaultBin     = ipbf-ttImportLoc.locn_defaultBin
            location.geoLat         = ipbf-ttImportLoc.locn_geoLat
            location.geoLong        = ipbf-ttImportLoc.locn_geoLong
            location.geoAlt         = ipbf-ttImportLoc.locn_geoAlt
            location.lActive        = ipbf-ttImportLoc.loc_active
            location.notes          = ipbf-ttImportLoc.locn_notes
            location.externalID[1]  = ipbf-ttImportLoc.locn_externalID1
            location.externalID[2]  = ipbf-ttImportLoc.locn_externalID2
            location.externalID[3]  = ipbf-ttImportLoc.locn_externalID3
            location.externalID[4]  = ipbf-ttImportLoc.locn_externalID4
            location.externalID[5]  = ipbf-ttImportLoc.locn_externalID5
            location.phone          = ipbf-ttImportLoc.locn_phone
            location.fax            = ipbf-ttImportLoc.locn_fax
            location.email          = ipbf-ttImportLoc.locn_email.
        
        FIND FIRST loc EXCLUSIVE WHERE 
            loc.company EQ ipbf-ttImportLoc.company and
            loc.loc EQ ipbf-ttImportLoc.location
            NO-ERROR.
        IF NOT AVAIL loc THEN DO:
            iopiAdded = iopiAdded + 1.
            CREATE loc.
            ASSIGN 
                loc.company              = ipbf-ttImportLoc.company
                loc.loc                  = ipbf-ttImportLoc.location.
        END.
        ASSIGN 
            loc.dscr                 = ipbf-ttImportLoc.loc_dscr
            loc.whs-chg              = ipbf-ttImportLoc.loc_whs-chg
            loc.wc-uom               = ipbf-ttImportLoc.loc_wc-uom
            loc.rec_key              = DYNAMIC-FUNCTION("sfGetNextRecKey")
            loc.addrRecKey           = location.rec_key
            loc.active               = ipbf-ttImportLoc.loc_active
            loc.handlingCost         = ipbf-ttImportLoc.loc_handlingCost
            loc.storageCost[1]       = ipbf-ttImportLoc.loc_storageCost1
            loc.storageCost[2]       = ipbf-ttImportLoc.loc_storageCost2
            loc.storageCost[3]       = ipbf-ttImportLoc.loc_storageCost3
            loc.storageCost[4]       = ipbf-ttImportLoc.loc_storageCost4
            loc.division             = ipbf-ttImportLoc.loc_division
            loc.locationSquareFeet   = ipbf-ttImportLoc.loc_locationSquareFeet
            loc.palletCapacity       = ipbf-ttImportLoc.loc_palletCapacity
            loc.owner                = ipbf-ttImportLoc.loc_owner
            loc.isAPIEnabled         = ipbf-ttImportLoc.loc_isAPIEnabled
            loc.glCode               = ipbf-ttImportLoc.loc_glCode.
            
        RELEASE loc .
        RELEASE location .
    END.
    
END PROCEDURE.
