/* ttCRMCustomers.i */

DEFINE TEMP-TABLE ttCRMCustomers NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD tickerSymbol  AS CHARACTER LABEL "CRM Ticker"    FORMAT "x(8)"
    FIELD crmName       AS CHARACTER LABEL "CRM Name"      FORMAT "x(30)"
    FIELD crmPhone      AS CHARACTER LABEL "CRM Phone"     FORMAT "x(20)"
    FIELD crmStreet     AS CHARACTER LABEL "CRM Street"    FORMAT "x(30)"
    FIELD crmStreet2    AS CHARACTER LABEL "CRM Street 2"  FORMAT "x(30)"
    FIELD crmCity       AS CHARACTER LABEL "CRM City"      FORMAT "x(15)"
    FIELD crmState      AS CHARACTER LABEL "CRM State"     FORMAT "x(2)"
    FIELD crmCode       AS CHARACTER LABEL "CRM Code"      FORMAT "x(10)"
    FIELD action        AS CHARACTER LABEL "Action"        FORMAT "x(8)"  INITIAL "Add"
    FIELD custName      AS CHARACTER LABEL "Name"          FORMAT "x(30)"
    FIELD custAreaCode  AS CHARACTER LABEL "Area"          FORMAT "(999)"
    FIELD custPhone     AS CHARACTER LABEL "Phone"         FORMAT "999-9999"
    FIELD custStreet    AS CHARACTER LABEL "Street"        FORMAT "x(30)"
    FIELD custStreet2   AS CHARACTER LABEL "Street 2"      FORMAT "x(30)"
    FIELD custCity      AS CHARACTER LABEL "City"          FORMAT "x(15)"
    FIELD custState     AS CHARACTER LABEL "State"         FORMAT "x(2)"
    FIELD custCode      AS CHARACTER LABEL "Code"          FORMAT "x(10)"
    FIELD saveAction    AS CHARACTER LABEL "Save Action"   FORMAT "x(8)"
    FIELD origName      AS CHARACTER LABEL "Orig Name"     FORMAT "x(30)"
    FIELD origAreaCode  AS CHARACTER LABEL "Orig Area"     FORMAT "(999)"
    FIELD origPhone     AS CHARACTER LABEL "Orig Phone"    FORMAT "999-9999"
    FIELD origStreet    AS CHARACTER LABEL "Orig Street"   FORMAT "x(30)"
    FIELD origStreet2   AS CHARACTER LABEL "Orig Street 2" FORMAT "x(30)"
    FIELD origCity      AS CHARACTER LABEL "Orig City"     FORMAT "x(15)"
    FIELD origState     AS CHARACTER LABEL "Orig State"    FORMAT "x(2)"
    FIELD origCode      AS CHARACTER LABEL "Orig Code"     FORMAT "x(10)"
    FIELD xxApplyAction AS LOGICAL   LABEL "=>"
    FIELD xxCustRowID   AS ROWID
        INDEX ttCRMCustomers IS PRIMARY tickerSymbol
        .
