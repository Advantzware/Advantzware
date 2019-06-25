
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
DEFINE VARIABLE cocode LIKE oe-ordl.company.

{order_enq.i}

DEFINE VARIABLE prmCust      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prmUser      AS CHARACTER  NO-UNDO INITIAL "Admin".
DEFINE VARIABLE prmAction    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prmPonum     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prmPartno    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prmPostatus  AS CHARACTER  NO-UNDO INITIAL "".
DEFINE VARIABLE prmOrderNum  AS CHARACTER  NO-UNDO.

DEFINE VARIABLE prmFgitem      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prmEst      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prmJob      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prmJob2      AS CHAR  NO-UNDO.
DEFINE VARIABLE prmQuote     AS CHAR NO-UNDO.

DEFINE VARIABLE i AS INTEGER.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vEst   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vJob   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.
def var ls-rel-stat as cha label "" form "x" no-undo.
def var lv-rel-recid as recid no-undo.
                 
DEF VAR prmComp AS CHAR NO-UNDO.


IF prmCust = ?   THEN ASSIGN prmCust = "".
IF prmUser = ?   THEN ASSIGN prmUser = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF prmPonum = ?  THEN ASSIGN prmPonum = "".
IF prmPartno = ? THEN ASSIGN prmPartno = "".
IF prmPostatus = ? THEN ASSIGN prmPostatus = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmOrderNum = "0" THEN ASSIGN prmOrderNum = "".

IF prmFgitem = ?   THEN ASSIGN prmFgitem = "".
IF prmEst = ?   THEN ASSIGN prmEst = "".
IF prmJob = ?   THEN ASSIGN prmJob = "".
IF prmJob2 = ?   THEN ASSIGN prmJob2 = "".
IF prmQuote = ?  THEN ASSIGN prmQuote = "" .



FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
 DEF VAR v-usercust AS LOG NO-UNDO.


ASSIGN prmAction = "search"
       cocode = prmComp.
   
    FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
    IF AVAILABLE users THEN DO:
        IF users.internal-user = YES THEN DO:
            ASSIGN
                v-usercust = YES .
        END.
         IF users.internal-user = NO THEN DO:
             ASSIGN
                 v-usercust = NO .
         END.
    END.
/*****************function**************************/
FUNCTION get-act-rel-qty RETURNS INTEGER () :

  DEF VAR li AS INT NO-UNDO.
 
  IF AVAIL oe-ordl THEN
     FOR EACH oe-rel WHERE 
         oe-rel.company EQ cocode AND
         oe-rel.ord-no  EQ oe-ordl.ord-no AND
         oe-rel.i-no    EQ oe-ordl.i-no AND
         oe-rel.line    EQ oe-ordl.line
         NO-LOCK:

         RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

         IF INDEX("A,B,P",lv-stat) > 0 THEN
            li = li + oe-rel.qty.
     END.

  RETURN li.

END FUNCTION.

/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
    WHEN "search" THEN DO:     
        DISPLAY STRING(TIME, "HH:MM:SS").
        ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
               vJob = FILL(" ",6 - LENGTH(TRIM(prmJob))) + TRIM(prmJob).

        CREATE QUERY q-OrderQuery.

        RUN build-qry IN THIS-PROCEDURE (OUTPUT v-qry-string).  
        
        q-OrderQuery:ADD-BUFFER(BUFFER oe-ordl:HANDLE).
        q-OrderQuery:ADD-BUFFER(BUFFER oe-ord:Handle).

        CREATE DATA-SOURCE src-Order.
        src-Order:QUERY = q-OrderQuery.

        BUFFER ttOrder:ATTACH-DATA-SOURCE(src-Order).

        q-OrderQuery:QUERY-PREPARE(v-qry-string).        
        DATASET dsOrder:FILL().

        /*Populate Extra Fields*/
    DISPLAY STRING(TIME, "HH:MM:SS").
    FOR EACH ttOrder BY ttOrder.ord-no DESC   :
            ASSIGN
                li-prod   = 0
                li-bal    = 0
                li-wip    = 0
                li-pct    = 0
                li-qoh    = 0
                li-act-rel-qty  = 0
                .
            
           
           FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.ord-no = ttOrder.ord-no
                AND oe-ordl.LINE   = ttOrder.LINE
                USE-INDEX ord-no NO-LOCK NO-ERROR.
           
           IF NOT AVAILABLE oe-ordl THEN NEXT.
          
                ASSIGN ttOrder.onhandqty = get-bal(OUTPUT li-qoh)
                    ttOrder.wipqty     = get-wip().
                       .
                      
                ASSIGN
                    ttOrder.Prod       = get-prod(OUTPUT li-bal)
                    ttOrder.vEst     = oe-ordl.est-no
                    ttOrder.VJob     = oe-ordl.job-no
                    ttOrder.VJob2    = oe-ordl.job-no2
                    ttOrder.vshipqty   = oe-ordl.ship-qty
                    ttOrder.vinvqty   = oe-ordl.inv-qty.
                
                ASSIGN
                    li-pct = get-pct(INPUT li-bal)
                    ttOrder.oupct      = li-pct                                   /*get-pct(li-bal)*/
                    .
            FIND FIRST oe-rel  
                WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.i-no    EQ oe-ordl.i-no
                AND oe-rel.line    EQ oe-ordl.LINE 
                USE-INDEX ord-item NO-LOCK NO-ERROR.
            ASSIGN
                ttOrder.actrelqty  = get-act-rel-qty().

            FOR EACH sman WHERE sman.company = prmComp NO-LOCK:
                IF sman.sman = oe-ordl.s-man[1] THEN
                    ASSIGN ttOrder.sname1         = sman.sname .
                IF sman.sman = oe-ordl.s-man[2] THEN 
                    ASSIGN ttOrder.sname2        = sman.sname.
                IF sman.sman = oe-ordl.s-man[3] THEN 
                    ASSIGN ttOrder.sname3        = sman.sname.
            END.   /*for each sman */
            ASSIGN i = i + 1.
        END. /*FOR EACH ttOrder:*/ 
        DISPLAY i STRING(TIME, "HH:MM:SS").
        
        IF NOT CAN-FIND(FIRST users WHERE users.user_id = prmUser) THEN
           EMPTY TEMP-TABLE ttOrder.        
        
    END. /*WHEN "search" THEN DO: */
END CASE.
/* ***************************  Main Block  *************************** */



/* ***************************  Procedures  *************************** */
PROCEDURE build-qry:
    
    DEFINE OUTPUT PARAMETER prm-query AS CHARACTER NO-UNDO.
    
    
    ASSIGN prm-query = " PRESELECT ".

    IF NOT  v-usercust  THEN DO:
                 prm-query = prm-query
                           + " EACH usercust WHERE usercust.user_id = "
                           + QUOTER(prmUser)
                           + " AND usercust.company = "
                           + QUOTER(prmComp).

          q-OrderQuery:ADD-BUFFER(BUFFER usercust:HANDLE).
                           
          prm-query = prm-query + " NO-LOCK, ".
    END.

    
    ASSIGN prm-query = prm-query + " EACH oe-ordl NO-LOCK WHERE oe-ordl.cust-no <> '""' and   oe-ordl.company = '" + prmComp + "'  ".
    
    IF NOT  v-usercust  THEN
    prm-query = prm-query + " AND oe-ordl.cust-no = usercust.cust-no ". 
    
    IF prmOrderNum <> "" THEN  ASSIGN prm-query = prm-query + " and oe-ordl.ord-no = " + prmOrderNum.
    IF prmCust <> "" THEN ASSIGN prm-query = prm-query + " and oe-ordl.cust-no = '" + prmCust + "' ".
    IF prmPonum <> "" THEN ASSIGN prm-query = prm-query + " and oe-ordl.po-no begins '" + prmPonum + "'".
    IF prmPartno <> "" THEN ASSIGN prm-query = prm-query + " and oe-ordl.part-no begins '" + prmPartno + "'".
    IF prmFgitem <> "" THEN ASSIGN prm-query = prm-query + " and oe-ordl.i-no begins '" + prmFgitem + "'".
    IF prmEst <> "" THEN ASSIGN prm-query = prm-query + " and oe-ordl.est-no begins '" + vEst + "'".
    IF prmJob <> "" THEN ASSIGN prm-query = prm-query + " and oe-ordl.job-no begins '" + vJob + "'".  
    IF prmJob2 <> "" THEN   ASSIGN prm-query = prm-query + " and oe-ordl.job-no2 = '" +  prmJob2 + "'".
    IF prmQuote <> "" THEN   ASSIGN prm-query = prm-query + " and oe-ordl.q-no = '" +  prmQuote + "'".

    CASE prmPostatus:
       WHEN "open" THEN
          ASSIGN prm-query = prm-query + " ,first oe-ord of oe-ordl no-lock where oe-ord.opened eq yes AND oe-ordl.stat NE 'C'  ".        
       
       WHEN "closed" THEN
          ASSIGN prm-query = prm-query + " ,first oe-ord of oe-ordl no-lock where oe-ord.opened eq no OR oe-ordl.stat EQ 'C'".
       
       WHEN "pending" THEN
          ASSIGN prm-query = prm-query + " ,first oe-ord of oe-ordl no-lock where oe-ord.opened eq yes and oe-ord.stat eq 'w'".

        OTHERWISE
          ASSIGN prm-query = prm-query + " ,first oe-ord of oe-ordl no-lock where oe-ord.stat ge ''". 
          
    END CASE.

    /* prm-query = prm-query + " BY oe-ordl.ord-no DESC   " .*/
     

END PROCEDURE.




