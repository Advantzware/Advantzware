/* ttCRMContacts.i */

DEFINE TEMP-TABLE ttCRMContacts NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD tickerSymbol   AS CHARACTER LABEL "CRM Ticker"     FORMAT "x(8)"
    FIELD crmFirstName   AS CHARACTER LABEL "CRM First Name" FORMAT "x(20)"
    FIELD crmLastName    AS CHARACTER LABEL "CRM Last Name"  FORMAT "x(20)"
    FIELD crmPhone       AS CHARACTER LABEL "CRM Phone"      FORMAT "x(20)"
    FIELD crmEmail       AS CHARACTER LABEL "CRM Email"      FORMAT "x(30)"
    FIELD action         AS CHARACTER LABEL "Action"         FORMAT "x(8)"  INITIAL "Add"
    FIELD phoneAttention AS CHARACTER LABEL "Attention"      FORMAT "x(35)"
    FIELD phoneCityCode  AS CHARACTER LABEL "City"           FORMAT "x(5)"
    FIELD phonePhone     AS CHARACTER LABEL "Phone"          FORMAT "x(12)"
    FIELD phoneExt       AS CHARACTER LABEL "Ext"            FORMAT "x(8)"
    FIELD phoneEmail     AS CHARACTER LABEL "Email"          FORMAT "x(30)"
    FIELD saveAction     AS CHARACTER LABEL "Save Action"    FORMAT "x(8)"
    FIELD origAttention  AS CHARACTER LABEL "Orig Attention" FORMAT "x(35)"
    FIELD origCityCode   AS CHARACTER LABEL "Orig City"      FORMAT "x(5)"
    FIELD origPhone      AS CHARACTER LABEL "Orig Phone"     FORMAT "x(12)"
    FIELD origExt        AS CHARACTER LABEL "Orig Ext"       FORMAT "x(8)"
    FIELD origEmail      AS CHARACTER LABEL "Orig Email"     FORMAT "x(30)"
    FIELD xxApplyAction  AS LOGICAL   LABEL "=>"
    FIELD xxPhoneRowID   AS ROWID
    FIELD xxTableRecKey  AS CHARACTER
    FIELD xxRow          AS INTEGER
        INDEX ttCRMContacts IS PRIMARY tickerSymbol xxRow
        INDEX crmEmail crmEmail
        .

DEFINE TEMP-TABLE ttAccounts NO-UNDO
    FIELD accountName  AS CHARACTER
    FIELD tickerSymbol AS CHARACTER
    .
