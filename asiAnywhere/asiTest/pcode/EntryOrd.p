

/*------------------------------------------------------------------------
    File        : EntryOrd.p
    Purpose     : Order Entry Maintenance

    Syntax      :

    Description : Return a Dataset of all Orders and this program is call  order_estimate.aspx.

    Author(s)   : Sewa Singh
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE cocode LIKE oe-ordl.company.

{EntryOrd.i}

DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPonum     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPartno    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPostatus  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum  AS INTEGER  NO-UNDO.

DEFINE INPUT PARAMETER prmFgitem      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEst      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmJob      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmJob2      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote     AS INT NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEntryIOrd.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vEst   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE inputJob   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.
def var ls-rel-stat as cha label "" form "x" no-undo.
def var lv-rel-recid as recid no-undo.
DEF VAR v-count AS INT NO-UNDO.                
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE qh AS WIDGET-HANDLE. 
DEF VAR v-q-string AS CHAR NO-UNDO.
DEF VAR v-usercust AS LOG NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.

IF prmCust = ?   THEN ASSIGN prmCust = "".
IF prmUser = ?   THEN ASSIGN prmUser = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF prmPonum = ?  THEN ASSIGN prmPonum = "".
IF prmPartno = ? THEN ASSIGN prmPartno = "".
IF prmPostatus = ? THEN ASSIGN prmPostatus = "open".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = 0.
IF prmFgitem = ?   THEN ASSIGN prmFgitem = "".
IF prmEst = ?   THEN ASSIGN prmEst = "".
IF prmJob = ?   THEN ASSIGN prmJob = "".
IF prmJob2 = ?   THEN ASSIGN prmJob2 = "".
IF prmQuote = ?  THEN ASSIGN prmQuote = 0.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
 ASSIGN
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
  
        /*ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
               vJob = FILL(" ",6 - LENGTH(TRIM(prmJob))) + TRIM(prmJob).*/
/* ********************  Preprocessor Definitions  ******************** */

CREATE QUERY qh.
qh:SET-BUFFERS(BUFFER oe-ordl:HANDLE,BUFFER oe-ord:HANDLE).

IF prmAction = "Select" THEN DO:
       ASSIGN v-q-string = " PRESELECT ".  
   
        /* IF NOT  v-usercust  THEN DO:*/
            v-q-string = v-q-string  + " EACH usercust WHERE usercust.user_id = "
            + QUOTER(prmUser)
            + " AND usercust.company = "
            + QUOTER(prmComp).
        
             qh:SET-BUFFERS(BUFFER usercust:HANDLE).
            v-q-string = v-q-string + " NO-LOCK, ".
       /* END.*/

        v-q-string = " EACH oe-ordl WHERE oe-ordl.cust-no <> '""'  and oe-ordl.company = " + QUOTER(prmComp)   .

    IF NOT  v-usercust  THEN
           v-q-string = v-q-string + " and oe-ordl.cust-no = usercust.cust-no  " .
    ASSIGN v-q-string = v-q-string + " ,first oe-ord of oe-ordl no-lock where oe-ord.opened eq yes AND oe-ordl.stat NE 'C' " .

    MESSAGE " v-q-string" v-q-string.
   qh:QUERY-PREPARE(v-q-string).
   qh:QUERY-OPEN.

   REPEAT:
      qh:GET-NEXT().
      IF qh:QUERY-OFF-END THEN LEAVE.

     CREATE ttEntryIOrd.
                        ASSIGN
                        ttEntryIOrd.vOrder         = oe-ordl.ord-no
                        ttEntryIOrd.vCust          = oe-ordl.cust-no
                       /* ttEntryIOrd.vcustName      = oe-ordl.cust-name */
                       /* ttEntryIOrd.vReqDate       = oe-ordl.req-date*/
                        ttEntryIOrd.vPrice         = oe-ordl.price
                        ttEntryIOrd.vUom           = oe-ordl.pr-uom
                        ttEntryIOrd.vExtPrice      = oe-ordl.t-price
                        ttEntryIOrd.vItem          = oe-ordl.i-no
                        ttEntryIOrd.vItemName      = oe-ordl.i-name 
                        ttEntryIOrd.vPart          = oe-ordl.part-no
                        ttEntryIOrd.vPoNum         = oe-ordl.po-no
                        ttEntryIOrd.vEst           = oe-ordl.est-no
                        ttEntryIOrd.vJob           = oe-ordl.job-no
                        ttEntryIOrd.vJob2          = oe-ordl.job-no2
                       /*ttEntryIOrd.vOrdDate       = oe-ordl.ord-date */
                        ttEntryIOrd.vStatus        = oe-ordl.stat   
                        ttEntryIOrd.vOrdqty        = oe-ordl.qty
                        ttEntryIOrd.company        = oe-ordl.company
                        ttEntryIOrd.LINE           = oe-ordl.LINE
                        ttEntryIOrd.shipqty        = oe-ordl.ship-qty
                        ttEntryIOrd.vsman1         = oe-ordl.s-man[1]
                        ttEntryIOrd.vsamn2         = oe-ordl.s-man[2]
                        ttEntryIOrd.vsman3         = oe-ordl.s-man[3]
                       
                        ttEntryIOrd.vOver          = oe-ordl.over-pct 
                        ttEntryIOrd.vUnder         = oe-ordl.under-pct
                        ttEntryIOrd.vqno            = oe-ordl.q-no
                            ttEntryIOrd.vqno2            = oe-ordl.q-no
                         ttEntryIOrd.vRecKey        = oe-ordl.rec_key
                        /*ttEntryIOrd.invqty         = oe-ordl.inv-qty*/  .

   END. /* end open query*/

       
 FOR EACH ttEntryIOrd :
             
    
            ASSIGN
                li-prod   = 0
                li-bal    = 0
                li-wip    = 0
                li-pct    = 0
                li-qoh    = 0
                li-act-rel-qty  = 0
                .
             FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.ord-no = ttEntryIOrd.vOrder
                AND oe-ordl.LINE   = ttEntryIOrd.LINE
                NO-LOCK NO-ERROR.
            
           IF NOT AVAILABLE oe-ordl THEN NEXT.
                ASSIGN ttEntryIOrd.onhandqty = get-bal(OUTPUT li-qoh)
                    ttEntryIOrd.wipqty     = get-wip().
                       .
                      
                ASSIGN
                    ttEntryIOrd.Prod       = get-prod(OUTPUT li-bal)
                    ttEntryIOrd.vEst     = oe-ordl.est-no
                    ttEntryIOrd.VJob     = oe-ordl.job-no
                    ttEntryIOrd.VJob2    = oe-ordl.job-no2
                    ttEntryIOrd.vshipqty   = oe-ordl.ship-qty
                    ttEntryIOrd.vinvqty   = oe-ordl.inv-qty.
               
                ASSIGN
                    li-pct = get-pct(INPUT li-bal)
                    ttEntryIOrd.oupct      = li-pct                                   /*get-pct(li-bal)*/
                    .
            
            FIND FIRST oe-rel  
                WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.i-no    EQ oe-ordl.i-no
                AND oe-rel.line    EQ oe-ordl.LINE NO-LOCK NO-ERROR.
            ASSIGN
                ttEntryIOrd.actrelqty  = get-act-rel-qty().
        END. /*FOR EACH ttOrder:*/  

    
END.    /*if prmAction:*/
/* ***************************  Main Block  *************************** */


IF prmAction = "Search" THEN DO:
    IF prmPostatus = "open" THEN DO:
       
    ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
           inputJob = FILL(" ",6 - LENGTH(TRIM(prmJob))) + TRIM(prmJob).
        /* v-count = 0.*/
         MAIN-LOOP:
   
    FOR EACH oe-ord WHERE oe-ord.company = prmComp  AND (oe-ord.ord-no = prmOrderNum OR prmOrderNum = 0 ) AND ( oe-ord.est-no = vEst OR vEst = "" OR oe-ord.est-no = prmEst) AND ( oe-ord.q-no = prmQuote OR prmQuote = 0)
                  AND ( oe-ord.cust-no BEGINS prmCust OR prmCust = "")  AND  (oe-ord.po-no BEGINS prmPonum OR prmPonum = "")   AND  oe-ord.opened = YES    NO-LOCK BY oe-ord.ord-no DESC BY oe-ord.ord-date DESC:
        IF oe-ord.cust-no = "" THEN NEXT MAIN-LOOP .
        IF LOOKUP(oe-ord.cust-no, custcount) = 0 THEN NEXT MAIN-LOOP .
              
            FIND FIRST oe-ordl WHERE  oe-ordl.company = prmComp AND  oe-ordl.ord-no = oe-ord.ord-no  AND oe-ordl.opened = YES AND oe-ordl.stat NE "C"   NO-LOCK NO-ERROR.
            FIND FIRST cust WHERE cust.cust-no = oe-ord.cust-no AND cust.company = prmComp NO-LOCK NO-ERROR.
            IF NOT AVAIL oe-ordl THEN DO:
               FIND FIRST ttEntryIOrd WHERE ttEntryIOrd.vOrder = oe-ord.ord-no NO-LOCK NO-ERROR.
                IF AVAIL ttEntryIOrd THEN NEXT.
               
                CREATE ttEntryIOrd.
                    ASSIGN 
                        ttEntryIOrd.vOrder         = oe-ord.ord-no
                        ttEntryIOrd.vEst           = oe-ord.est-no
                        ttEntryIOrd.VJob           = oe-ord.job-no 
                        ttEntryIOrd.VJob2          = oe-ord.job-no2
                        ttEntryIOrd.vCust          = oe-ord.cust-no
                        ttEntryIOrd.vcustName      = oe-ord.cust-name 
                        ttEntryIOrd.vReqDate       = oe-ord.due-date
                        ttEntryIOrd.vReqcode       = oe-ord.due-code
                        ttEntryIOrd.vPromCode      = oe-ord.due-code
                        ttEntryIOrd.vPromDate      = oe-ord.due-date
                        ttEntryIOrd.vOver          = oe-ord.over-pct 
                        ttEntryIOrd.vUnder         = oe-ord.under-pct
                        ttEntryIOrd.vsman1         = oe-ord.sman[1]
                        ttEntryIOrd.vsamn2         = oe-ord.sman[2]
                        ttEntryIOrd.vsman3         = oe-ord.sman[3]
                        ttEntryIOrd.vspct1         = oe-ord.s-pct[1]
                        ttEntryIOrd.vspct2         = oe-ord.s-pct[2]
                        ttEntryIOrd.vspct3         = oe-ord.s-pct[3]
                        ttEntryIOrd.vscomm         = oe-ord.s-comm[1]
                        ttEntryIOrd.vscomm2        = oe-ord.s-comm[2]
                        ttEntryIOrd.vscomm3        = oe-ord.s-comm[3]
                        ttEntryIOrd.sname1         = oe-ord.sname[1]
                        ttEntryIOrd.sname2         = oe-ord.sname[2] 
                        ttEntryIOrd.sname3         = oe-ord.sname[3]
                        ttEntryIOrd.vtype          = oe-ord.type
                        ttEntryIOrd.vPoNum         = oe-ord.po-no
                        ttEntryIOrd.disc           = cust.disc
                        ttEntryIOrd.tax            = cust.sort 
                        ttEntryIOrd.vqno            = oe-ord.q-no
                        ttEntryIOrd.vqno2            = oe-ord.q-no
                        ttEntryIOrd.vRecKey        = oe-ord.rec_key
                        .
                END.
                ELSE DO:  
                  FOR  EACH  oe-ordl WHERE oe-ordl.ord-no = oe-ord.ord-no AND   oe-ordl.opened = YES AND (oe-ordl.i-no BEGINS prmFgitem OR prmFgitem = "")
                                 AND (oe-ordl.part-no BEGINS prmPartno OR prmPartno = "")   
                                 AND (oe-ordl.po-no BEGINS prmPonum OR prmPonum = "")                 
                                 AND ( oe-ordl.est-no = vEst OR vEst = "")
                                 AND (oe-ordl.job-no = inputJob OR inputJob = "")
                                 AND ( oe-ordl.q-no = prmQuote OR prmQuote = 0)
                                 AND (oe-ordl.opened = YES AND oe-ordl.stat NE "C") NO-LOCK:
                     
                        CREATE ttEntryIOrd.
                        ASSIGN
                        ttEntryIOrd.vOrder         = oe-ordl.ord-no
                        ttEntryIOrd.vCust          = oe-ordl.cust-no
                        ttEntryIOrd.vcustName      = oe-ord.cust-name 
                        ttEntryIOrd.vReqDate       = oe-ordl.req-date
                        ttEntryIOrd.vPrice         = oe-ordl.price
                        ttEntryIOrd.vUom           = oe-ordl.pr-uom
                        ttEntryIOrd.vExtPrice      = oe-ordl.t-price
                        ttEntryIOrd.vItem          = oe-ordl.i-no
                        ttEntryIOrd.vItemName      = oe-ordl.i-name 
                        ttEntryIOrd.vPart          = oe-ordl.part-no
                        ttEntryIOrd.vPoNum         = oe-ordl.po-no
                        ttEntryIOrd.vEst           = oe-ordl.est-no
                        ttEntryIOrd.vJob           = oe-ordl.job-no
                        ttEntryIOrd.vJob2          = oe-ordl.job-no2
                        ttEntryIOrd.vOrdDate       = oe-ord.ord-date 
                        ttEntryIOrd.vStatus        = oe-ord.stat   
                        ttEntryIOrd.vOrdqty        = oe-ordl.qty
                        ttEntryIOrd.company        = oe-ordl.company
                        ttEntryIOrd.LINE           = oe-ordl.LINE
                        ttEntryIOrd.shipqty        = oe-ordl.ship-qty
                        ttEntryIOrd.vsman1         = oe-ord.sman[1]
                        ttEntryIOrd.vsamn2         = oe-ord.sman[2]
                        ttEntryIOrd.vsman3         = oe-ord.sman[3]
                        ttEntryIOrd.sname1         = oe-ord.sname[1]
                        ttEntryIOrd.sname2         = oe-ord.sname[2] 
                        ttEntryIOrd.sname3         = oe-ord.sname[3]
                        ttEntryIOrd.vOver          = oe-ord.over-pct 
                        ttEntryIOrd.vUnder         = oe-ord.under-pct
                        ttEntryIOrd.vqno            = oe-ordl.q-no
                        ttEntryIOrd.vqno2            = oe-ord.q-no
                         ttEntryIOrd.vRecKey        = oe-ordl.rec_key
                        /*ttEntryIOrd.invqty         = oe-ordl.inv-qty*/

                       .
                        v-count = v-count + 1.
                      IF v-count = 50 THEN LEAVE MAIN-LOOP.
                          
                    END.   /*FOR EACH oe-ordl*/
                END.  /**else do:*/   
            END.  /**for each oe-ord*/  
    
FOR EACH ttEntryIOrd :
            ASSIGN
                li-prod   = 0
                li-bal    = 0
                li-wip    = 0
                li-pct    = 0
                li-qoh    = 0
                li-act-rel-qty  = 0
                .
          
             FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.ord-no = ttEntryIOrd.vOrder
                AND oe-ordl.LINE   = ttEntryIOrd.LINE
                NO-LOCK NO-ERROR.
            
           IF NOT AVAILABLE oe-ordl THEN NEXT.
                ASSIGN ttEntryIOrd.onhandqty = get-bal(OUTPUT li-qoh)
                    ttEntryIOrd.wipqty     = get-wip().
                       .
                      
                ASSIGN
                    ttEntryIOrd.Prod       = get-prod(OUTPUT li-bal)
                    ttEntryIOrd.vEst     = oe-ordl.est-no
                    ttEntryIOrd.VJob     = oe-ordl.job-no
                    ttEntryIOrd.VJob2    = oe-ordl.job-no2
                    ttEntryIOrd.vshipqty   = oe-ordl.ship-qty
                    ttEntryIOrd.vinvqty   = oe-ordl.inv-qty.
                
                ASSIGN
                    li-pct = get-pct(INPUT li-bal)
                    ttEntryIOrd.oupct      = li-pct                                   /*get-pct(li-bal)*/
                    .
            
            FIND FIRST oe-rel  
                WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.i-no    EQ oe-ordl.i-no
                AND oe-rel.line    EQ oe-ordl.LINE NO-LOCK NO-ERROR.
            ASSIGN
                ttEntryIOrd.actrelqty  = get-act-rel-qty().
        END. /*FOR EACH ttOrder:*/  
   END.  /** for each user cust*/
 

/**********************************pending***********************************/
IF prmPostatus = "closed" THEN DO:
    ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
           inputJob = FILL(" ",6 - LENGTH(TRIM(prmJob))) + TRIM(prmJob).
    /* v-count = 0.*/
     MAIN-LOOP:
     
    FOR EACH oe-ord WHERE oe-ord.company = prmComp 
                      AND (oe-ord.ord-no = prmOrderNum OR prmOrderNum = 0 )
                     AND ( oe-ord.cust-no BEGINS prmCust OR prmCust = "")   NO-LOCK, 
       EACH oe-ordl OF oe-ord WHERE  (oe-ordl.i-no BEGINS prmFgitem OR prmFgitem = "")
                                 AND (oe-ordl.part-no BEGINS prmPartno OR prmPartno = "")   
                                 AND (oe-ordl.po-no BEGINS prmPonum OR prmPonum = "")                 
                                 AND ( oe-ordl.est-no = vEst OR vEst = "")
                                 AND (oe-ordl.job-no = inputJob OR inputJob = "")
                                 AND ( oe-ordl.q-no = prmQuote OR prmQuote = 0)
                                 AND (oe-ordl.opened = NO OR oe-ordl.stat EQ "C")    NO-LOCK BY oe-ord.ord-no DESC BY oe-ord.ord-date DESC:
                 IF oe-ord.cust-no = "" THEN NEXT MAIN-LOOP .
                 IF LOOKUP(oe-ord.cust-no, custcount) = 0 THEN NEXT MAIN-LOOP.
        
                CREATE ttEntryIOrd.
                ASSIGN
                ttEntryIOrd.vOrder         = oe-ordl.ord-no
                ttEntryIOrd.vCust          = oe-ordl.cust-no
                ttEntryIOrd.vcustName      = oe-ord.cust-name 
                ttEntryIOrd.vReqDate       = oe-ordl.req-date
                ttEntryIOrd.vPrice         = oe-ordl.price
                ttEntryIOrd.vUom           = oe-ordl.pr-uom
                ttEntryIOrd.vExtPrice      = oe-ordl.t-price
                ttEntryIOrd.vItem          = oe-ordl.i-no
                ttEntryIOrd.vItemName      = oe-ordl.i-name 
                ttEntryIOrd.vPart          = oe-ordl.part-no
                ttEntryIOrd.vPoNum         = oe-ordl.po-no
                ttEntryIOrd.vEst           = oe-ordl.est-no
                ttEntryIOrd.vJob           = oe-ordl.job-no
                ttEntryIOrd.vJob2          = oe-ordl.job-no2
                ttEntryIOrd.vOrdDate       = oe-ord.ord-date 
                ttEntryIOrd.vStatus        = oe-ord.stat
                ttEntryIOrd.vOrdqty        = oe-ordl.qty
                ttEntryIOrd.company        = oe-ordl.company
                ttEntryIOrd.LINE           = oe-ordl.LINE
                ttEntryIOrd.vsman1         = oe-ord.sman[1]
                ttEntryIOrd.vsamn2         = oe-ord.sman[2]
                ttEntryIOrd.vsman3         = oe-ord.sman[3]
                ttEntryIOrd.sname1         = oe-ord.sname[1]
                ttEntryIOrd.sname2         = oe-ord.sname[2] 
                ttEntryIOrd.sname3         = oe-ord.sname[3]
                ttEntryIOrd.vOver          = oe-ord.over-pct 
                ttEntryIOrd.vUnder         = oe-ord.under-pct
                ttEntryIOrd.vqno            = oe-ordl.q-no
                 ttEntryIOrd.vqno2            = oe-ord.q-no
                 ttEntryIOrd.vRecKey        = oe-ordl.rec_key
                     .
                 v-count = v-count + 1.
                      IF v-count = 50 THEN LEAVE MAIN-LOOP.
        
   
END.
FOR EACH ttEntryIOrd :
            ASSIGN
                li-prod   = 0
                li-bal    = 0
                li-wip    = 0
                li-pct    = 0
                li-qoh    = 0
                li-act-rel-qty  = 0
                .
          
           FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.ord-no = ttEntryIOrd.vOrder
                AND oe-ordl.LINE   = ttEntryIOrd.LINE
                NO-LOCK NO-ERROR.
            
           IF NOT AVAILABLE oe-ordl THEN NEXT.
                ASSIGN ttEntryIOrd.onhandqty = get-bal(OUTPUT li-qoh)
                    ttEntryIOrd.wipqty     = get-wip().
                       .
                      
                ASSIGN
                    ttEntryIOrd.Prod       = get-prod(OUTPUT li-bal)
                    ttEntryIOrd.vEst     = oe-ordl.est-no
                    ttEntryIOrd.VJob     = oe-ordl.job-no
                    ttEntryIOrd.VJob2    = oe-ordl.job-no2
                    ttEntryIOrd.vshipqty   = oe-ordl.ship-qty
                    ttEntryIOrd.vinvqty   = oe-ordl.inv-qty.
                
                ASSIGN
                    li-pct = get-pct(INPUT li-bal)
                    ttEntryIOrd.oupct      = li-pct                                   /*get-pct(li-bal)*/
                    .
            
            FIND FIRST oe-rel  
                WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.i-no    EQ oe-ordl.i-no
                AND oe-rel.line    EQ oe-ordl.LINE NO-LOCK NO-ERROR.
            ASSIGN
                ttEntryIOrd.actrelqty  = get-act-rel-qty().
        END. /*FOR EACH ttOrder:*/ 
     END.  /** fine usercust*****/
END.      
/********************************************/
IF prmPostatus = "pending" THEN DO:
    ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
           inputJob = FILL(" ",6 - LENGTH(TRIM(prmJob))) + TRIM(prmJob).
   /* v-count = 0.*/
    MAIN-LOOP:
    FOR EACH oe-ord WHERE oe-ord.company = prmComp 
                      AND (oe-ord.ord-no = prmOrderNum OR prmOrderNum = 0 )
                     AND ( oe-ord.cust-no BEGINS prmCust or prmCust = "")  AND oe-ord.stat = 'W'  NO-LOCK, 
        EACH oe-ordl OF oe-ord WHERE  (oe-ordl.i-no BEGINS prmFgitem OR prmFgitem = "")
                                 AND (oe-ordl.part-no BEGINS prmPartno OR prmPartno = "")   
                                 AND (oe-ordl.po-no BEGINS prmPonum OR prmPonum = "")                 
                                 AND ( oe-ordl.est-no = vEst OR vEst = "")
                                 AND (oe-ordl.job-no = inputJob OR inputJob = "") 
                                 AND ( oe-ordl.q-no = prmQuote OR prmQuote = 0)
                                 AND  (oe-ordl.opened = YES OR oe-ordl.stat EQ "W") NO-LOCK BY oe-ord.ord-no DESC BY oe-ord.ord-date DESC:
         
        IF oe-ord.cust-no = "" THEN NEXT MAIN-LOOP .
        IF LOOKUP(oe-ord.cust-no, custcount) = 0 THEN NEXT MAIN-LOOP.
        
          CREATE ttEntryIOrd.
                ASSIGN
                ttEntryIOrd.vOrder         = oe-ordl.ord-no
                ttEntryIOrd.vCust          = oe-ordl.cust-no
                ttEntryIOrd.vcustName      = oe-ord.cust-name 
                ttEntryIOrd.vReqDate       = oe-ordl.req-date
                ttEntryIOrd.vPrice         = oe-ordl.price
                ttEntryIOrd.vUom           = oe-ordl.pr-uom
                ttEntryIOrd.vExtPrice      = oe-ordl.t-price
                ttEntryIOrd.vItem          = oe-ordl.i-no
                ttEntryIOrd.vItemName      = oe-ordl.i-name 
                ttEntryIOrd.vPart          = oe-ordl.part-no
                ttEntryIOrd.vPoNum         = oe-ordl.po-no
                ttEntryIOrd.vEst           = oe-ordl.est-no
                ttEntryIOrd.vJob           = oe-ordl.job-no
                ttEntryIOrd.vJob2          = oe-ordl.job-no2
                ttEntryIOrd.vOrdDate       = oe-ord.ord-date 
                ttEntryIOrd.vStatus        = oe-ord.stat
                ttEntryIOrd.vOrdqty        = oe-ordl.qty
                ttEntryIOrd.company        = oe-ordl.company
                ttEntryIOrd.LINE           = oe-ordl.LINE
                ttEntryIOrd.vsman1         = oe-ord.sman[1]
                ttEntryIOrd.vsamn2         = oe-ord.sman[2]
                ttEntryIOrd.vsman3         = oe-ord.sman[3]
                ttEntryIOrd.sname1         = oe-ord.sname[1]
                ttEntryIOrd.sname2         = oe-ord.sname[2] 
                ttEntryIOrd.sname3         = oe-ord.sname[3]
                ttEntryIOrd.vOver          = oe-ord.over-pct 
                ttEntryIOrd.vUnder         = oe-ord.under-pct
                ttEntryIOrd.vqno            = oe-ordl.q-no
                ttEntryIOrd.vqno2            = oe-ord.q-no
                ttEntryIOrd.vRecKey        = oe-ordl.rec_key
                     .
       
             v-count = v-count + 1.
                      IF v-count = 50 THEN LEAVE MAIN-LOOP.
    
                  
END.
FOR EACH ttEntryIOrd :
            ASSIGN
                li-prod   = 0
                li-bal    = 0
                li-wip    = 0
                li-pct    = 0
                li-qoh    = 0
                li-act-rel-qty  = 0
                .
          
           FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.ord-no = ttEntryIOrd.vOrder
                AND oe-ordl.LINE   = ttEntryIOrd.LINE
                NO-LOCK NO-ERROR.
            
           IF NOT AVAILABLE oe-ordl THEN NEXT.
                ASSIGN ttEntryIOrd.onhandqty = get-bal(OUTPUT li-qoh)
                    ttEntryIOrd.wipqty     = get-wip().
                       .
                      
                ASSIGN
                    ttEntryIOrd.Prod       = get-prod(OUTPUT li-bal)
                    ttEntryIOrd.vEst     = oe-ordl.est-no
                    ttEntryIOrd.VJob     = oe-ordl.job-no
                    ttEntryIOrd.VJob2    = oe-ordl.job-no2
                    ttEntryIOrd.vshipqty   = oe-ordl.ship-qty
                    ttEntryIOrd.vinvqty   = oe-ordl.inv-qty.
                
                ASSIGN
                    li-pct = get-pct(INPUT li-bal)
                    ttEntryIOrd.oupct      = li-pct                                   /*get-pct(li-bal)*/
                    .
            
            FIND FIRST oe-rel  
                WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.i-no    EQ oe-ordl.i-no
                AND oe-rel.line    EQ oe-ordl.LINE NO-LOCK NO-ERROR.
            ASSIGN
                ttEntryIOrd.actrelqty  = get-act-rel-qty().
        END. /*FOR EACH ttOrder:*/ 
     
END.
/* ***************************  Main Block  *************************** */
IF prmPostatus = "any" THEN DO:
    ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
           inputJob = FILL(" ",6 - LENGTH(TRIM(prmJob))) + TRIM(prmJob).
      v-count = 0.
      MAIN-LOOP:
    FOR EACH oe-ord WHERE oe-ord.company = prmComp  AND (oe-ord.ord-no = prmOrderNum OR prmOrderNum = 0 ) AND ( oe-ord.est-no = vEst OR vEst = "" OR oe-ord.est-no = prmEst) AND ( oe-ord.q-no = prmQuote OR prmQuote = 0)
                     AND ( oe-ord.cust-no BEGINS prmCust OR prmCust = "")     NO-LOCK BY oe-ord.ord-no DESC BY oe-ord.ord-date DESC:
        IF oe-ord.cust-no = "" THEN NEXT MAIN-LOOP .
        IF LOOKUP(oe-ord.cust-no, custcount) = 0 THEN NEXT MAIN-LOOP.

             v-count = v-count + 1.
                      IF v-count = 50 THEN LEAVE MAIN-LOOP.
               
            FIND FIRST oe-ordl WHERE  oe-ordl.company = prmComp AND  oe-ordl.ord-no = oe-ord.ord-no /*AND oe-ordl.opened = YES AND oe-ordl.stat NE "C"*/  NO-LOCK NO-ERROR.
            FIND FIRST cust WHERE cust.cust-no = oe-ord.cust-no AND cust.company = prmComp NO-LOCK NO-ERROR.
           
            IF NOT AVAIL oe-ordl THEN DO:
               
                CREATE ttEntryIOrd.
                    ASSIGN 
                        ttEntryIOrd.vOrder         = oe-ord.ord-no
                        ttEntryIOrd.vEst           = oe-ord.est-no
                        ttEntryIOrd.VJob           = oe-ord.job-no 
                        ttEntryIOrd.VJob2          = oe-ord.job-no2
                        ttEntryIOrd.vCust          = oe-ord.cust-no
                        ttEntryIOrd.vcustName      = oe-ord.cust-name 
                        ttEntryIOrd.vReqDate       = oe-ord.due-date
                        ttEntryIOrd.vReqcode       = oe-ord.due-code
                        ttEntryIOrd.vPromCode      = oe-ord.due-code
                        ttEntryIOrd.vPromDate      = oe-ord.due-date
                        ttEntryIOrd.vOver          = oe-ord.over-pct 
                        ttEntryIOrd.vUnder         = oe-ord.under-pct
                        ttEntryIOrd.vsman1         = oe-ord.sman[1]
                        ttEntryIOrd.vsamn2         = oe-ord.sman[2]
                        ttEntryIOrd.vsman3         = oe-ord.sman[3]
                        ttEntryIOrd.vspct1         = oe-ord.s-pct[1]
                        ttEntryIOrd.vspct2         = oe-ord.s-pct[2]
                        ttEntryIOrd.vspct3         = oe-ord.s-pct[3]
                        ttEntryIOrd.vscomm         = oe-ord.s-comm[1]
                        ttEntryIOrd.vscomm2        = oe-ord.s-comm[2]
                        ttEntryIOrd.vscomm3        = oe-ord.s-comm[3]
                        ttEntryIOrd.sname1         = oe-ord.sname[1]
                        ttEntryIOrd.sname2         = oe-ord.sname[2] 
                        ttEntryIOrd.sname3         = oe-ord.sname[3]
                        ttEntryIOrd.vtype          = oe-ord.type
                        ttEntryIOrd.vPoNum         = oe-ord.po-no
                        ttEntryIOrd.disc           = cust.disc
                        ttEntryIOrd.tax            = cust.sort 
                        ttEntryIOrd.vqno            = oe-ord.q-no
                        ttEntryIOrd.vqno2            = oe-ord.q-no
                        ttEntryIOrd.vRecKey        = oe-ord.rec_key
                        .
                END.    /* not avail oe-ordl*/
                ELSE DO:  
                  FOR  EACH oe-ordl WHERE oe-ordl.ord-no = oe-ord.ord-no AND   (oe-ordl.i-no BEGINS prmFgitem OR prmFgitem = "")
                                 AND (oe-ordl.part-no BEGINS prmPartno OR prmPartno = "")   
                                 AND (oe-ordl.po-no BEGINS prmPonum OR prmPonum = "")                 
                                 AND ( oe-ordl.est-no = vEst OR vEst = "")
                                 AND (oe-ordl.job-no = inputJob OR inputJob = "")
                                 AND ( oe-ordl.q-no = prmQuote OR prmQuote = 0)  NO-LOCK:
                        CREATE ttEntryIOrd.
                        ASSIGN
                        ttEntryIOrd.vOrder         = oe-ordl.ord-no
                        ttEntryIOrd.vCust          = oe-ordl.cust-no
                        ttEntryIOrd.vcustName      = oe-ord.cust-name 
                        ttEntryIOrd.vReqDate       = oe-ordl.req-date
                        ttEntryIOrd.vPrice         = oe-ordl.price
                        ttEntryIOrd.vUom           = oe-ordl.pr-uom
                        ttEntryIOrd.vExtPrice      = oe-ordl.t-price
                        ttEntryIOrd.vItem          = oe-ordl.i-no
                        ttEntryIOrd.vItemName      = oe-ordl.i-name 
                        ttEntryIOrd.vPart          = oe-ordl.part-no
                        ttEntryIOrd.vPoNum         = oe-ordl.po-no
                        ttEntryIOrd.vEst           = oe-ordl.est-no
                        ttEntryIOrd.vJob           = oe-ordl.job-no
                        ttEntryIOrd.vJob2          = oe-ordl.job-no2
                       ttEntryIOrd.vOrdDate       = oe-ord.ord-date 
                        ttEntryIOrd.vStatus        = oe-ord.stat   
                        ttEntryIOrd.vOrdqty        = oe-ordl.qty
                        ttEntryIOrd.company        = oe-ordl.company
                        ttEntryIOrd.LINE           = oe-ordl.LINE
                        ttEntryIOrd.shipqty        = oe-ordl.ship-qty
                        ttEntryIOrd.vsman1         = oe-ord.sman[1]
                        ttEntryIOrd.vsamn2         = oe-ord.sman[2]
                        ttEntryIOrd.vsman3         = oe-ord.sman[3]
                        ttEntryIOrd.sname1         = oe-ord.sname[1]
                        ttEntryIOrd.sname2         = oe-ord.sname[2] 
                        ttEntryIOrd.sname3         = oe-ord.sname[3]
                        ttEntryIOrd.vOver          = oe-ord.over-pct 
                        ttEntryIOrd.vUnder         = oe-ord.under-pct
                        ttEntryIOrd.vqno            = oe-ordl.q-no
                        ttEntryIOrd.vqno2            = oe-ord.q-no
                         ttEntryIOrd.vRecKey        = oe-ordl.rec_key
                        /*ttEntryIOrd.invqty         = oe-ordl.inv-qty*/

                       .
                        
                       
                          
                    END.   /*FOR EACH oe-ordl*/
                END.  /**else do:*/  
           

FOR EACH ttEntryIOrd :
            ASSIGN
                li-prod   = 0
                li-bal    = 0
                li-wip    = 0
                li-pct    = 0
                li-qoh    = 0
                li-act-rel-qty  = 0
                .
          
           FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.ord-no = ttEntryIOrd.vOrder
                AND oe-ordl.LINE   = ttEntryIOrd.LINE
                NO-LOCK NO-ERROR.
            
           IF NOT AVAILABLE oe-ordl THEN NEXT.
                ASSIGN ttEntryIOrd.onhandqty = get-bal(OUTPUT li-qoh)
                    ttEntryIOrd.wipqty     = get-wip().
                       .
                      
                ASSIGN
                    ttEntryIOrd.Prod       = get-prod(OUTPUT li-bal)
                    ttEntryIOrd.vEst     = oe-ordl.est-no
                    ttEntryIOrd.VJob     = oe-ordl.job-no
                    ttEntryIOrd.VJob2    = oe-ordl.job-no2
                    ttEntryIOrd.vshipqty   = oe-ordl.ship-qty
                    ttEntryIOrd.vinvqty   = oe-ordl.inv-qty.
                
                ASSIGN
                    li-pct = get-pct(INPUT li-bal)
                    ttEntryIOrd.oupct      = li-pct                                   /*get-pct(li-bal)*/
                    .
            
            FIND FIRST oe-rel  
                WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.i-no    EQ oe-ordl.i-no
                AND oe-rel.line    EQ oe-ordl.LINE NO-LOCK NO-ERROR.
            ASSIGN
                ttEntryIOrd.actrelqty  = get-act-rel-qty().

          
        END. /*FOR EACH ttOrder:*/ 
    
END.      
END.


/* ***************************  Main Block  *************************** */

qh:QUERY-CLOSE().
DELETE OBJECT qh.








