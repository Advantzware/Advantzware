
/*------------------------------------------------------------------------
    File        : order_enq.p
    Purpose     : Order Enquiry Maintenance

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : Sewa Singh
    Created     : Sat September 09 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{order_enq.i}
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPonum     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPartno    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPostatus  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum  AS CHARACTER  NO-UNDO.

DEFINE INPUT PARAMETER prmFgitem      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEst      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmJob      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmJob2      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrder.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.

DEF VAR li AS DECIMAL NO-UNDO.
DEF VAR op-bal AS INT NO-UNDO.
DEFINE VARIABLE relstat        AS CHARACTER.
DEFINE VARIABLE nextrelid      AS ROWID.
DEFINE VARIABLE cocode         AS CHARACTER.
DEFINE VARIABLE relstatb       AS CHARACTER.
DEFINE VARIABLE quotedate      AS DATE.
DEFINE VARIABLE Prodqty        AS Int.
ASSIGN quotedate = CalcQuoteDate().
DEFINE VARIABLE custx          AS CHARACTER.
GetCurrentCust(prmCust).

ASSIGN prmAction = "search".

IF prmComp = ?   THEN ASSIGN prmComp = "".
IF prmCust = ?   THEN ASSIGN prmCust = "".
IF prmUser = ?   THEN ASSIGN prmUser = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF prmPonum = ?  THEN ASSIGN prmPonum = "".
IF prmPartno = ? THEN ASSIGN prmPartno = "".
IF prmPostatus = ? THEN ASSIGN prmPostatus = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmFgitem = ?   THEN ASSIGN prmFgitem = "".
IF prmEst = ?   THEN ASSIGN prmEst = "".
IF prmJob = ?   THEN ASSIGN prmJob = "".
IF prmJob2 = ?   THEN ASSIGN prmJob2 = "".

DEFINE STREAM s1.
/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
    WHEN "search" THEN DO:        
        RUN build-qry IN THIS-PROCEDURE (OUTPUT v-qry-string).        
        q-OrderQuery:QUERY-PREPARE(v-qry-string).        
        DATASET dsOrder:FILL().
       

                /*Populate Extra Fields*/
        FOR EACH ttOrder:
            ASSIGN
                nextrelid = ?
                relstat   = "".
            FOR EACH oe-rel WHERE oe-rel.company = ttOrder.company 
                AND oe-rel.ord-no = ttOrder.ord-no
                AND oe-rel.i-no = ttOrder.i-no BY oe-rel.rel-date :
                RUN oi/rel-stat.p (ROWID(oe-rel),OUTPUT relstat).
                IF relstat NE "C" AND relstat NE "Z"  THEN DO:
                   IF nextrelid = ? THEN
                   nextrelid = ROWID(oe-rel).
                END.
            END. /*FOR EACH oe-rel*/
            IF nextrelid NE ? THEN FIND oe-rel WHERE ROWID(oe-rel) = nextrelid NO-LOCK NO-ERROR.
            IF (relstat = "P") OR (relstat = "C") THEN ASSIGN ttOrder.blue = TRUE.
            IF NOT AVAILABLE oe-ord THEN
            FIND FIRST oe-ord WHERE oe-ord.company = ttOrder.company 
                AND oe-ord.cust-no = ttOrder.cust-no 
                AND oe-ord.ord-no = ttOrder.ord-no NO-LOCK NO-ERROR.
            FOR EACH fg-bin WHERE fg-bin.company = ttOrder.company 
                AND fg-bin.i-no = ttOrder.i-no  
                AND fg-bin.job-no = string(ttOrder.job-no)  NO-LOCK:
                ASSIGN ttOrder.onhandqty = ttOrder.onhandqty + fg-bin.qty.
                       
            END.
            IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN
                 FOR EACH fg-rcpth  WHERE                 
                 fg-rcpth.job-no    = oe-ordl.job-no
                 AND fg-rcpth.job-no2   = oe-ordl.job-no2
                 AND fg-rcpth.i-no      = oe-ordl.i-no NO-LOCK:
                     
                        FIND FIRST fg-rdtlh 
                        WHERE fg-rdtlh.r-no     = fg-rcpth.r-no
                        AND fg-rdtlh.rita-code  = fg-rcpth.rita-code NO-LOCK NO-ERROR.
                        li = li + fg-rdtlh.qty.
                        IF AVAILABLE fg-rdtlh THEN 
                        ASSIGN ttOrder.Prod = li.

                 END.
 
               op-bal = li.
            GetCurrentUser(prmUser).
            IF canAccess(prmUser, "E") THEN DO:
                FOR EACH quoteitm WHERE quoteitm.part-no = ttOrder.part-no NO-LOCK:
                    FIND FIRST quotehd WHERE quotehd.company = cust.company
                        AND quotehd.loc = quoteitm.loc
                        AND quotehd.q-no = quoteitm.q-no
                        AND quotehd.quo-date > quotedate  NO-LOCK NO-ERROR.
                    IF ttOrder.showadd = FALSE AND AVAILABLE quotehd THEN ASSIGN ttOrder.showadd = TRUE. 
                    IF AVAILABLE quotehd THEN RELEASE quotehd.
                    IF ttOrder.showadd THEN LEAVE.
                END.
                IF ttOrder.showadd = FALSE THEN DO:
                    FIND FIRST itemfg WHERE itemfg.company = cust.company 
                        AND itemfg.cust-no = cust.cust-no 
                        AND itemfg.part-no = ttOrder.part-no NO-LOCK NO-ERROR.
                    IF AVAILABLE itemfg AND itemfg.cust-no = cust.cust-no AND itemfg.i-code = "s" THEN ASSIGN ttOrder.showadd = TRUE.
                    IF AVAILABLE itemfg AND itemfg.company = cust.company AND itemfg.cust-no = custx THEN ASSIGN ttOrder.showadd = TRUE.
                END.
            END. /*IF canAccess("E")*/
            FIND FIRST itemfg WHERE itemfg.company = cust.company 
                AND itemfg.cust-no = cust.cust-no 
                AND itemfg.part-no = ttOrder.part-no NO-LOCK NO-ERROR.
            IF showadd  AND AVAILABLE itemfg THEN DO:
                ASSIGN ttOrder.showadd = TRUE.
            END.
             FIND FIRST oe-ord WHERE oe-ord.company = ttOrder.company 
                AND oe-ord.cust-no = ttOrder.cust-no 
                AND oe-ord.ord-no = ttOrder.ord-no NO-LOCK NO-ERROR.
            IF AVAILABLE oe-ord THEN DO:
            ASSIGN ttOrder.Name = oe-ord.cust-name.
            END.  /*FIND FIRST oe-ord*/
            IF AVAILABLE oe-rel THEN DO:
                FIND FIRST oe-relh where oe-relh.company  eq oe-rel.company
                    and oe-relh.ord-no   eq oe-rel.ord-no
                    and oe-relh.rel-no   eq oe-rel.rel-no
                    and oe-relh.b-ord-no eq oe-rel.b-ord-no
                    and oe-relh.cust-no  eq oe-rel.cust-no
                    and oe-relh.posted   eq no
                    and oe-relh.deleted  eq no
                    use-index order NO-LOCK NO-ERROR.
                IF NOT AVAILABLE oe-relh THEN DO:
                    IF (TODAY + (5 * 365)) < oe-rel.rel-date THEN DO:
                        ASSIGN ttOrder.releasedd = "Not Scheduled".
                    END.
                    ELSE DO:
                        ASSIGN ttOrder.releasedd = STRING(oe-rel.rel-date).
                    END.
                END.
                ELSE DO:
                    IF (TODAY + (5 * 365)) < oe-relh.rel-date THEN DO:
                        ASSIGN ttOrder.releasedd = "Not Scheduled".
                    END.
                    ELSE DO:
                        ASSIGN ttOrder.releasedd = STRING(oe-relh.rel-date).
                    END.
                END.
            END. /*IF AVAILABLE oe-rel*/
            RELEASE oe-rel.
        END. /*FOR EACH ttOrder:*/
    END. /*WHEN "search" THEN DO: */
END CASE.
/* ***************************  Main Block  *************************** */


/* ***************************  Procedures  *************************** */
PROCEDURE build-qry:
    DEFINE OUTPUT PARAMETER prm-query AS CHARACTER NO-UNDO.
    ASSIGN prm-query = " PRESELECT EACH oe-ordl NO-LOCK WHERE oe-ordl.company = '" + prmComp + "' and oe-ordl.cust-no = '" + prmCust + "' ".
    
    
    IF prmPonum <> "" THEN ASSIGN prm-query = prm-query + " and oe-ordl.po-no begins '" + prmPonum + "'".
    IF prmPartno <> "" THEN ASSIGN prm-query = prm-query + " and oe-ordl.part-no begins '" + prmPartno + "'".
    /*IF prmPartdesc <> "" THEN ASSIGN prm-query = prm-query + " and (oe-ordl.i-name begins '" + prmPartdesc +  "' or oe-ordl.i-dscr begins '" + prmPartdesc +  "' or oe-ordl.part-dscr1 begins '" + prmPartdesc + "')".
    IF prmFrmdate <> "" THEN ASSIGN prm-query = prm-query + " and oe-ordl.req-date >= " + prmFrmdate.
    IF prmTodate <> "" THEN ASSIGN prm-query = prm-query + " and oe-ordl.req-date <= " + prmTodate.*/
    
    IF prmFgitem <> "" THEN ASSIGN prm-query = prm-query + " and oe-ordl.i-no begins '" + prmFgitem + "'".
    IF prmEst <> "" THEN ASSIGN prm-query = prm-query + " and oe-ordl.est-no begins '" + prmEst + "'".
    IF prmJob <> "" THEN ASSIGN prm-query = prm-query + " and oe-ordl.job-no begins '" + prmJob + "'".
    IF prmJob2 <> "" THEN ASSIGN prm-query = prm-query + " and oe-ordl.job-no2 begins '" + prmJob2 + "'".

    CASE prmPostatus:
     WHEN "open" THEN do:
        ASSIGN prm-query = prm-query + " ,first oe-ord of oe-ordl no-lock where oe-ord.opened eq yes".        
     END.
     WHEN "closed" THEN do:
        ASSIGN prm-query = prm-query + " ,first oe-ord of oe-ordl no-lock where oe-ord.opened eq no".
     END.
     WHEN "pending" THEN DO: 
        ASSIGN prm-query = prm-query + " ,first oe-ord of oe-ordl no-lock where oe-ord.opened eq yes and oe-ord.stat eq 'w'".
     END.
     OTHERWISE DO:
        ASSIGN prm-query = prm-query + " ,first oe-ord of oe-ordl no-lock where oe-ord.stat ge ''".
     END.
    END CASE.
    IF prmOrderNum <> "" THEN
      ASSIGN prm-query = prm-query + " and oe-ord.ord-no = " + prmOrderNum.
END PROCEDURE.



