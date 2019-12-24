
/*------------------------------------------------------------------------
    File        : po/ttVendCostReport.i
    Purpose     : 

    Syntax      :

    Description : Temp-table tt-report definition

    Author(s)   : Mithun Porandla
    Created     : Tue Dec 24 01:30:00 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE tt-report NO-UNDO
    FIELD key-03      LIKE report.key-03
    FIELD key-04      LIKE report.key-04
    FIELD key-02      LIKE report.key-02 LABEL "MSF"
    FIELD vend-name   AS   CHARACTER 
    FIELD report-cost AS   DECIMAL
    FIELD cost-uom    AS   CHARACTER
    FIELD disc-days   LIKE vend.disc-days
    FIELD ext-price   AS   DECIMAL
    FIELD rec-id      AS   RECID
    FIELD vend-item   AS   CHAR
    FIELD wid-min     AS   DECIMAL 
    FIELD wid-max     AS   DECIMAL
    FIELD len-min     AS   DECIMAL
    FIELD len-max     AS   DECIMAL 
    FIELD po-no       AS   CHARACTER
    .

