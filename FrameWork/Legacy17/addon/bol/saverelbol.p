/* addon/bol/saverelbol.p **************************/

DEFINE INPUT PARAMETER ip-company AS CHAR NO-UNDO.

DEF SHARED TEMP-TABLE tt-relbol NO-UNDO 
    FIELD release# LIKE oe-relh.release#
    FIELD tag# AS cha
    FIELD i-no AS cha FORM "x(15)"
    FIELD i-name AS cha FORM "x(30)"
    FIELD ord-no LIKE oe-ord.ord-no
    FIELD job-no LIKE oe-rell.job-no
    FIELD job-no2 LIKE oe-rell.job-no2
    FIELD loc LIKE oe-rell.loc
    FIELD loc-bin LIKE oe-rell.loc-bin
    FIELD cust-no LIKE oe-rell.cust-no
    FIELD cases LIKE oe-rell.cases
    FIELD qty-case LIKE oe-rell.qty-case
    FIELD cases-unit LIKE fg-rctd.cases-unit
    FIELD partial LIKE oe-rell.partial
    FIELD qty LIKE oe-rell.qty
    FIELD t-qty LIKE oe-rell.qty
    FIELD line LIKE oe-rell.line
    FIELD oerell-row AS ROWID
    FIELD seq AS INT
    FIELD warned AS LOG
    FIELD po-no LIKE oe-boll.po-no
    FIELD trailer# LIKE oe-relh.trailer
    INDEX release# release# ord-no i-no po-no.

FOR EACH tt-relbol:

    CREATE ssrelbol.
    BUFFER-COPY tt-relbol TO ssrelbol
       ASSIGN ssrelbol.company = ip-company.

    RELEASE ssrelbol.
END.
                                   

