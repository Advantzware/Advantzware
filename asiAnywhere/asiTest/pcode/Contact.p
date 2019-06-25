/*------------------------------------------------------------------------
    File        : Contact.p
    Purpose     : Contact

    Syntax      :

    Description : Update Advantzware DB

    Author(s)   : Kuldeep
    Created     : 03/17/2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER prmAction      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vCompany       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER custNo         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER shipId         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vSman          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER FirstName      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER LastName       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER MiddleInitial  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vSirname       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER ContactTitle   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vMaillist      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vType          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER ContactLoc     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER CustName       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vAddr1         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vAddr2        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vCity         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vState        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vZip          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vCountry      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vCounty       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vTerritory    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER AccessCode    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vPhone        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER CellPhone     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vFax          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vExtension    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vEmail        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vWebsite      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey     AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER vReckey     AS CHARACTER  NO-UNDO.


DEFINE BUFFER buff-contact FOR contact.
IF prmAction = "Add" THEN DO:
    FIND buff-contact WHERE  buff-contact.company = vCompany NO-LOCK NO-ERROR.
    IF NOT AVAILABLE buff-contact  THEN DO:
        CREATE buff-contact .
        ASSIGN
            buff-contact.company          = vCompany
            buff-contact.cust-no          = custNo
            buff-contact.ship-id          = shipId
            buff-contact.sman             = vSman
            buff-contact.first-name       = FirstName
            buff-contact.last-name        = LastName
            buff-contact.middle-initial   = MiddleInitial
            buff-contact.sirname          = vSirname 
            buff-contact.contact-title    = ContactTitle
            buff-contact.maillist         = IF vMaillist = "Yes" then true else false
            buff-contact.TYPE             = vType
            buff-contact.contact-loc      = ContactLoc
            buff-contact.cust-name        = CustName
            buff-contact.addr1            = vAddr1
            buff-contact.addr2            = vAddr2
            buff-contact.city             = vCity
            buff-contact.state            = vState
            buff-contact.zip              = vZip
            buff-contact.country          = vCountry
            buff-contact.county           = vCounty
            buff-contact.territory        = vTerritory
            buff-contact.access-code      = AccessCode
            buff-contact.phone            = vPhone
            buff-contact.cell-phone       = CellPhone
            buff-contact.fax              = vFax
            buff-contact.extension        = vExtension
            buff-contact.email            = vEmail
            buff-contact.website          = vWebsite
            buff-contact.rec_key          = STRING(TODAY,"99999999") + STRING(NEXT-VALUE(rec_key_seq,asi),"99999999").
            vReckey                      = buff-contact.rec_key. 
            
            
        RELEASE buff-contact.
        CREATE rec_key.
            ASSIGN
                rec_key.rec_key    = vReckey
                rec_key.table_name = "contact".
    END.
END. /*
IF prmAction = "Add" THEN DO:*/
IF prmAction = "Update" THEN DO:
    FIND FIRST buff-contact WHERE buff-contact.rec_key = prmReckey AND buff-contact.company = vCompany EXCLUSIVE-LOCK NO-ERROR.
    IF  AVAILABLE buff-contact  THEN DO:
        ASSIGN
           
            buff-contact.cust-no          = custNo
            buff-contact.ship-id          = shipId
            buff-contact.sman             = vSman
            buff-contact.first-name       = FirstName
            buff-contact.last-name        = LastName
            buff-contact.middle-initial   = MiddleInitial
            buff-contact.sirname          = vSirname 
            buff-contact.contact-title    = ContactTitle
            buff-contact.maillist         = IF vMaillist = "Yes" then true else false
            buff-contact.TYPE             = vType
            buff-contact.contact-loc      = ContactLoc
            buff-contact.cust-name        = CustName
            buff-contact.addr1            = vAddr1
            buff-contact.addr2            = vAddr2
            buff-contact.city             = vCity
            buff-contact.state            = vState
            buff-contact.zip              = vZip
            buff-contact.country          = vCountry
            buff-contact.county           = vCounty
            buff-contact.territory        = vTerritory
            buff-contact.access-code      = AccessCode
            buff-contact.phone            = vPhone
            buff-contact.cell-phone       = CellPhone
            buff-contact.fax              = vFax
            buff-contact.extension        = vExtension
            buff-contact.email            = vEmail
            buff-contact.website          = vWebsite
            
                        .
        RELEASE buff-contact.
    END.
END. /*IF prmAction = "Update" THEN DO:*/
IF prmAction = "Delete" THEN DO:
    FIND buff-contact WHERE buff-contact.rec_key = prmReckey AND buff-contact.company = vCompany  NO-ERROR.
    IF AVAILABLE buff-contact  THEN DO:
        DELETE buff-contact.
    END.
END. /*IF prmAction = "Delete" THEN DO:*/
