
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


DEFINE NEW SHARED VARIABLE cocode LIKE oe-ordl.company.
DEFINE NEW SHARED VARIABLE locode AS CHAR.

DEF VAR v-print-fmt AS CHAR NO-UNDO.
DEF VAR ld-price LIKE oe-ordl.price NO-UNDO.
DEF VAR ld-t-price LIKE oe-ordl.t-price NO-UNDO.
{order_enq.i}

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
DEFINE INPUT PARAMETER prmJob2      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote     AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrder.

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


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
 DEF VAR v-usercust AS LOG NO-UNDO.
 
ASSIGN prmAction = "search"
       cocode = prmComp.

{sys/inc/oeinq.i}  
  /**********Function**************************************/

FUNCTION get-extended-price RETURNS DECIMAL
  ( /* parameter-definitions */ ) :

  DEF BUFFER b-oe-ordl FOR oe-ordl.

  DEF VAR ld AS DEC NO-UNDO.
  def var v-tmp-price as dec format ">,>>>,>>9.9999" no-undo.
  def var lv-t-price as dec no-undo.

  FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK.

  ld = b-oe-ordl.t-price.

  IF oeinq-char NE "Order Price" THEN
  FOR EACH ar-invl FIELDS(inv-no amt i-no unit-pr disc) WHERE
      ar-invl.company EQ cocode AND
      ar-invl.ord-no EQ b-oe-ordl.ord-no AND
      ar-invl.i-no EQ b-oe-ordl.i-no
      NO-LOCK
      BY ar-invl.inv-no DESC:

      find first itemfg
      where (itemfg.company  = cocode )
        and itemfg.i-no eq ar-invl.i-no
        no-lock no-error.
      
      assign
         v-tmp-price = if b-oe-ordl.pr-uom begins "L" AND b-oe-ordl.pr-uom NE "LB" then
                       if b-oe-ordl.qty lt 0 then -1 else 1
                       else
                       if b-oe-ordl.pr-uom eq "CS" then
                          b-oe-ordl.qty / (if b-oe-ordl.cas-cnt ne 0 then b-oe-ordl.cas-cnt else
                                          if avail itemfg and itemfg.case-count ne 0
                                                         then itemfg.case-count else
                                                              1)
                       else
                       if b-oe-ordl.pr-uom eq "C" then
                          b-oe-ordl.qty / 100
                       else
                       if b-oe-ordl.pr-uom eq "M" then
                         b-oe-ordl.qty / 1000
                       else
                         b-oe-ordl.qty
                            
         lv-t-price = v-tmp-price * ar-invl.unit-pr
         ld =  IF v-print-fmt EQ "Dayton" THEN 
                (lv-t-price - ROUND(lv-t-price * ar-invl.disc / 100,2))
              ELSE
                ROUND(lv-t-price * (1 - (ar-invl.disc / 100)),2).
      
      LEAVE.
  END.

  RETURN ld.

END FUNCTION.


FUNCTION get-act-rel-qty RETURNS INTEGER () :

  DEF VAR li AS INT NO-UNDO.
 
  IF AVAIL oe-ordl THEN
     FOR EACH oe-rel WHERE 
         oe-rel.company EQ cocode AND
         oe-rel.ord-no  EQ oe-ordl.ord-no AND
         oe-rel.i-no    EQ oe-ordl.i-no AND
         oe-rel.line    EQ oe-ordl.LINE USE-INDEX ord-item
         NO-LOCK:

         RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

         IF INDEX("A,B,P",lv-stat) > 0 THEN
            li = li + oe-rel.qty.
     END.

  RETURN li.

END FUNCTION.
/**********************************************************/

    find first sys-ctrl
        where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "INVPRINT"
        no-lock no-error.
    if avail sys-ctrl then
        ASSIGN
        v-print-fmt  = sys-ctrl.char-fld.
   
    FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
   /* IF AVAILABLE users THEN DO:
        IF users.internal-user = YES THEN DO:
            ASSIGN
                v-usercust = YES .
        END.
         IF users.internal-user = NO THEN DO:
             ASSIGN
                 v-usercust = NO .
         END.
    END.*/

/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
    WHEN "search" THEN DO:     
        ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
               vJob = FILL(" ",6 - LENGTH(TRIM(prmJob))) + TRIM(prmJob).

        CREATE QUERY q-OrderQuery.

        RUN build-qry IN THIS-PROCEDURE (OUTPUT v-qry-string).  
        
        q-OrderQuery:ADD-BUFFER(BUFFER oe-ordl:HANDLE).
        q-OrderQuery:ADD-BUFFER(BUFFER oe-ord:Handle).
        
        q-OrderQuery:QUERY-PREPARE(v-qry-string).        
     
        q-OrderQuery:QUERY-OPEN.

   REPEAT:
      q-OrderQuery:GET-NEXT().
      IF q-OrderQuery:QUERY-OFF-END THEN LEAVE.

     CREATE ttOrder.

        ASSIGN
            ttOrder.ord-no    = oe-ordl.ord-no
            ttOrder.LINE      = oe-ordl.LINE
            ttOrder.rec_key   = oe-ordl.rec_key
            ttOrder.cust-no   = oe-ordl.cust-no
            ttOrder.req-date  = oe-ordl.req-date
            ttOrder.pr-uom    = oe-ordl.pr-uom
            ttOrder.i-no      = oe-ordl.i-no     
            ttOrder.i-name    = oe-ordl.i-name   
            ttOrder.part-no   = oe-ordl.part-no  
            ttOrder.po-no     = oe-ordl.po-no    
            ttOrder.q-no      = oe-ordl.q-no     
            ttOrder.qty       = oe-ordl.qty      
            ttOrder.req-code  = oe-ordl.req-code 
            ttOrder.prom-code = oe-ordl.prom-code
            ttOrder.prom-date = oe-ordl.prom-date
            ttOrder.over-pct  = oe-ordl.over-pct 
            ttOrder.under-pct = oe-ordl.under-pct
            ttOrder.s-man[1]     = oe-ordl.s-man[1]    
            ttOrder.s-man[2]     = oe-ordl.s-man[2]    
            ttOrder.s-man[3]     = oe-ordl.s-man[3]    
            ttOrder.s-pct[1]     = oe-ordl.s-pct[1]   
            ttOrder.s-pct[2]     = oe-ordl.s-pct[2] 
            ttOrder.s-pct[3]     = oe-ordl.s-pct[3] 
            ttOrder.s-comm[1]    = oe-ordl.s-comm[1]   
            ttOrder.s-comm[2]    = oe-ordl.s-comm[2]   
            ttOrder.s-comm[3]    = oe-ordl.s-comm[3]   
            ttOrder.type-code    = oe-ordl.type-code
            ttOrder.tax          = oe-ordl.tax 
            
            ttOrder.vEst       = oe-ordl.est-no
            ttOrder.VJob       = oe-ordl.job-no
            ttOrder.VJob2      = oe-ordl.job-no2
            ttOrder.vshipqty   = oe-ordl.ship-qty
            ttOrder.vinvqty    = oe-ordl.inv-qty

            ttOrder.stat            = oe-ord.stat
            ttOrder.cust-name       = oe-ord.cust-name
            ttOrder.due-date        = oe-ord.due-date
            ttOrder.ord-date        = oe-ord.ord-date   .

         /*Populate Extra Fields*/
        ASSIGN
            li-prod   = 0
            li-bal    = 0
            li-wip    = 0
            li-pct    = 0
            li-qoh    = 0
            li-act-rel-qty  = 0
            .
         
           FIND FIRST quotehd WHERE quotehd.company = prmComp 
               AND quotehd.est-no = oe-ordl.est-no AND oe-ordl.est-no <> "" AND oe-ordl.est-no <> "0" NO-LOCK NO-ERROR.
           IF AVAIL quotehd THEN 
               ASSIGN
                    ttOrder.q-no      = quotehd.q-no.
          
                ASSIGN ttOrder.onhandqty = get-bal(OUTPUT li-qoh)
                       ttOrder.wipqty     = get-wip()
                       ttOrder.price     = get-price-disc()
                       ttOrder.t-price   = get-extended-price()
                        
                       ttOrder.Prod       = get-prod(OUTPUT li-bal)
                       li-pct = get-pct(INPUT li-bal)
                       ttOrder.oupct      = li-pct                                   /*get-pct(li-bal)*/
                           
                       ttOrder.actrelqty  = get-act-rel-qty().

         /*  FOR EACH sman WHERE sman.company = prmComp NO-LOCK :
                IF sman.sman = oe-ordl.s-man[1] THEN
                    ASSIGN ttOrder.sname1         = sman.sname .
                IF sman.sman = oe-ordl.s-man[2] THEN 
                    ASSIGN ttOrder.sname2        = sman.sname.
                IF sman.sman = oe-ordl.s-man[3] THEN 
                    ASSIGN ttOrder.sname3        = sman.sname.
            END.   /*for each sman */  */


   END.

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

/****************************************/



