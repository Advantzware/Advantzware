/* addon/bol/saverelbol.p **************************/

DEFINE INPUT PARAMETER ip-company AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER iprRelbol AS ROWID NO-UNDO.

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

FOR EACH tt-relbol WHERE (IF iprRelbol NE ? THEN ROWID(tt-relbol) EQ iprRelbol ELSE TRUE):
    FIND FIRST ssrelbol EXCLUSIVE-LOCK 
          WHERE ssrelbol.company EQ ip-company
              AND ssrelbol.release#   EQ tt-relbol.release#
              AND ssrelbol.ord-no     EQ tt-relbol.ord-no
              AND ssrelbol.i-no       EQ tt-relbol.i-no
              AND ssrelbol.po-no      EQ tt-relbol.po-no
              AND ssrelbol.job-no     EQ tt-relbol.job-no
              AND ssrelbol.job-no2    EQ tt-relbol.job-no2
              AND ssrelbol.tag#       EQ tt-relbol.tag#
           NO-ERROR.    
           
    IF NOT AVAILABLE ssrelbol THEN DO: 
    CREATE ssrelbol.
      ASSIGN 
               ssrelbol.release#   = tt-relbol.release#
               ssrelbol.ord-no     = tt-relbol.ord-no
               ssrelbol.i-no       = tt-relbol.i-no
               ssrelbol.po-no      = tt-relbol.po-no
               ssrelbol.job-no     = tt-relbol.job-no
               ssrelbol.job-no2    = tt-relbol.job-no2
               ssrelbol.tag#       = tt-relbol.tag#
               ssrelbol.company    = ip-company
               .
    END.
    
    /* Only write when values are different to allow this procedure to run quickly if little has changed */
    IF tt-relbol.release#      NE ssrelbol.release#               OR
        tt-relbol.tag#         NE ssrelbol.tag#                   OR
        tt-relbol.i-no         NE ssrelbol.i-no                   OR
        tt-relbol.i-name       NE ssrelbol.i-name                 OR
        tt-relbol.ord-no       NE ssrelbol.ord-no                 OR
        tt-relbol.job-no       NE ssrelbol.job-no                 OR
        tt-relbol.job-no2      NE ssrelbol.job-no2                OR
        tt-relbol.loc          NE ssrelbol.loc                    OR
        tt-relbol.loc-bin      NE ssrelbol.loc-bin                OR
        tt-relbol.cust-no      NE ssrelbol.cust-no                OR
        tt-relbol.cases        NE ssrelbol.cases                  OR
        tt-relbol.cases-unit   NE ssrelbol.cases-unit             OR
        tt-relbol.partial      NE ssrelbol.partial                OR
        tt-relbol.qty          NE ssrelbol.qty                    OR
        tt-relbol.qty-case     NE ssrelbol.qty-case               OR
        tt-relbol.t-qty        NE ssrelbol.t-qty                  OR
        tt-relbol.line         NE ssrelbol.line                   OR
        tt-relbol.seq          NE ssrelbol.seq                    OR
        tt-relbol.warned       NE ssrelbol.warned                 OR
        tt-relbol.po-no        NE ssrelbol.po-no                  OR
        tt-relbol.trailer#     NE ssrelbol.trailer#                 
    THEN 
        BUFFER-COPY tt-relbol TO ssrelbol.


    RELEASE ssrelbol.
END.
                                   

