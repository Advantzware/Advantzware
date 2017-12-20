
/*------------------------------------------------------------------------
    File        : oe850ord.p
    Purpose     : 

    Syntax      :

    Description : Create order(oe-ord) record from edi files

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE INPUT PARAMETER ipEdpotranRecid AS RECID.

/* ***************************  Definitions  ************************** */
/*DEFINE NEW SHARED VARIABLE g_company AS cha INIT "001" NO-UNDO.*/
/*DEFINE NEW SHARED VARIABLE g_loc AS cha INIT "main" NO-UNDO.   */
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}
{sys/inc/var.i new shared}

ASSIGN
  cocode = gcompany
  locode = gloc.

DEF VAR by_list AS CHAR NO-UNDO INITIAL "001,010,015,038,063,410".
DEF VAR on_list AS CHAR NO-UNDO INITIAL "002,007,037,064,077,018".
DEF VAR mh_list AS CHAR NO-UNDO INITIAL "".
def var conv_fact as decimal no-undo format "-9999999999.9999999999".
DEF VAR iNextRelNo AS INT NO-UNDO.


DISABLE TRIGGERs FOR load of oe-ord.
DISABLE TRIGGERs FOR load of oe-ordl.

DEFINE VARIABLE iNextOrder# AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION getNextOrderNum RETURNS INTEGER 
	(  ) FORWARD.


/* ***************************  Main Block  *************************** */ 

RUN createOrder.

/* **********************  Internal Procedures  *********************** */

PROCEDURE CreateOrder:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /* create oe-ord */
  
/*  FOR EACH EDPOTran NO-LOCK WHERE can-find(FIRST EDDoc OF EDPOTran WHERE EDDoc.Unique-Order-No = ? ): */
FOR EACH EDPOTran NO-LOCK WHERE recid(edpotran) = ipEdpotranRecid:  
    
    FIND cust WHERE cust.company = g_company and
                    cust.cust-no = EDPOTran.cust NO-LOCK NO-ERROR.
                       
/*    FIND FIRST oe-ctrl WHERE oe-ctrl.company = g_company .*/
    FIND FIRST EDMast WHERE EDMast.Partner = EDPOTran.Partner NO-LOCK NO-ERROR.
                    
    CREATE oe-ord.
    ASSIGN
          oe-ord.company       = gcompany
          oe-ord.loc           = gloc
/*          oe-ctrl.n-ord        = oe-ctrl.n-ord + 1*/
          oe-ord.ord-no        = getNextOrderNum()  /*oe-ctrl.n-ord*/ 
          oe-ord.type          = "O"
          oe-ord.ord-date      = edpotran.order-date
          oe-ord.po-no         = edpotran.cust-po
          oe-ord.bill-to       = edmast.cust
          oe-ord.cust-no   =   cust.cust-no
          oe-ord.cust-name = cust.name
          oe-ord.addr[1]   = cust.addr[1]
          oe-ord.addr[2]   = cust.addr[2]
          oe-ord.city      = cust.city
          oe-ord.state     = cust.state
          oe-ord.zip       = cust.zip
          oe-ord.contact   = cust.contact
          oe-ord.lead-days = cust.ship-days
          oe-ord.last-date =
          IF edpotran.cancel-date <> ?
          and edpotran.cancel-date < (today + cust.ship-days)
          then edpotran.cancel-date else today + cust.ship-day
          oe-ord.due-date  = IF EDPOTRAN.REQUEST-DATE = ? /* 9706 cah */
          THEN TODAY + 1 /* <9801 CAH */ ELSE edpotran.request-date
          oe-ord.terms     = cust.terms
          oe-ord.over-pct  = cust.over-pct
          oe-ord.under-pct = cust.under-pct
          oe-ord.fob-code  = cust.fob-code  /* dest or orig */
          oe-ord.frt-pay   = cust.frt-pay /* (P)repaid, (C)ollect or (B)ill */
          oe-ord.tax-gr    = cust.tax-gr
          oe-ord.carrier   = cust.carrier
          oe-ord.sman[1]       = cust.sman
          oe-ord.sman[2]       = ""
          oe-ord.sman[3]       = ""
          oe-ord.due-code      =  /* could be BY, ON or MH (make & hold) */
          IF CAN-DO(by_list, edpotran.ship-date-code) THEN "BY" ELSE
          IF CAN-DO(on_list, edpotran.ship-date-code) THEN "ON" ELSE
          IF CAN-DO(mh_list, edpotran.ship-date-code) THEN "MH" ELSE "ON"
                 
/*          oe-ord.sold-id = IF AVAIL dc THEN dc.ship-to*/
/*          ELSE edpotran.st-code.                      */
          .
        FIND terms WHERE terms.company = cust.company
                             AND terms.t-code = cust.terms NO-LOCK NO-ERROR.  
          oe-ord.terms-d = IF AVAILABLE terms THEN terms.dscr ELSE "". 
          
        FIND FIRST soldto WHERE soldto.company = g_company AND
          soldto.cust-no = oe-ord.cust-no
          AND TRIM(soldto.sold-id) BEGINS TRIM(oe-ord.sold-id)
          USE-INDEX sold-id NO-LOCK NO-ERROR.
        IF AVAILABLE soldto
          THEN
        ASSIGN oe-ord.sold-no = soldto.sold-no
          oe-ord.sold-id      = soldto.sold-id
          oe-ord.sold-name    = soldto.sold-name
          oe-ord.sold-addr[1] = soldto.sold-addr[1]
          oe-ord.sold-addr[2] = soldto.sold-addr[2]
          oe-ord.sold-city    = soldto.sold-city
          oe-ord.sold-state   = soldto.sold-state
          oe-ord.sold-zip     = soldto.sold-zip.
        FIND FIRST asi.shipto WHERE shipto.company = g_company AND
          shipto.cust-no = cust.cust-no and
          shipto.ship-id = EDPOTran.St-code NO-LOCK NO-ERROR.
        IF NOT AVAILABLE shipto then
          FIND FIRST asi.shipto WHERE shipto.company = g_company AND
          shipto.cust-no = cust.cust-no NO-LOCK NO-ERROR.
        IF AVAIL shipto THEN
        ASSIGN
          oe-ord.ship-i[1] = shipto.notes[1]
          oe-ord.ship-i[2] = shipto.notes[2]
          oe-ord.ship-i[3] = shipto.notes[3]
          oe-ord.ship-i[4] = shipto.notes[4].
        .
        FIND FIRST sman
          WHERE sman.company = oe-ord.company
          AND
          sman.sman = oe-ord.sman[1] NO-LOCK NO-ERROR.
        IF AVAIL sman THEN
        ASSIGN oe-ord.sman[1]   = CAPS(sman.sman)
          oe-ord.sname[1]  = sman.sname
          oe-ord.s-comm[1]  = sman.scomm
          oe-ord.s-pct[1] =      100
          .

      
      FOR EACH EDPOLine OF EDPOTran BREAK BY EDPOLine.seq:
          
          FIND itemfg WHERE itemfg.company = oe-ord.company
                        AND itemfg.i-no = EDPOLine.Item-no NO-LOCK NO-ERROR.
          IF NOT AVAILABLE itemfg then
             FIND FIRST itemfg WHERE itemfg.company = oe-ord.company
                        AND itemfg.part-no = EDPOLine.cust-item-no NO-LOCK NO-ERROR.           
          CREATE oe-ordl.
          ASSIGN
            oe-ordl.company       = oe-ord.company
            oe-ordl.ord-no        = oe-ord.ord-no
            oe-ordl.cust-no       = oe-ord.cust-no
            oe-ordl.line          = edpoline.line
            oe-ordl.part-no       = edpoline.cust-item-no
            oe-ordl.i-no          = edpoline.item-no
            oe-ordl.i-name        = IF AVAILABLE itemfg THEN itemfg.i-name ELSE ""
            oe-ordl.est-no        = IF AVAILABLE itemfg THEN itemfg.est-no ELSE ""
            oe-ordl.pr-uom        = EDPOLine.Uom-code  /*EA*/
            oe-ordl.qty           = EDPOLine.Qty-orig-ord 
            oe-ordl.i-dscr        = IF AVAILABLE itemfg THEN itemfg.i-dscr ELSE ""
            oe-ordl.price         = edpoline.unit-price
            oe-ordl.po-no         = oe-ord.po-no
            oe-ordl.cost          = IF AVAILABLE itemfg THEN itemfg.std-tot-cost ELSE 0
            oe-ordl.tax           = edpoline.tax
            oe-ordl.t-weight = IF AVAILABLE itemfg THEN round((oe-ordl.qty / 100) * itemfg.weight-100,0)
                               ELSE round(oe-ordl.qty / 100,0)
            oe-ordl.req-date      = oe-ord.due-date
            oe-ordl.req-code      = oe-ord.due-code
            oe-ordl.prom-date     = edpotran.cancel-date
            oe-ordl.over-pct      = oe-ord.over-pct
            oe-ordl.under-pct     = oe-ord.under-pct
            oe-ordl.prom-code     = IF edpotran.cancel-date <> ? THEN
            IF CAN-DO(by_list, edpotran.cancel-date-code) THEN "BY" ELSE
            IF CAN-DO(on_list, edpotran.cancel-date-code) THEN "ON" ELSE
            IF CAN-DO(mh_list, edpotran.cancel-date-code) THEN "MH" ELSE "BY"
            ELSE "BY"
            oe-ordl.t-price       = oe-ordl.qty * (oe-ordl.price / conv_fact)
            oe-ordl.part-dscr1    = IF AVAILABLE itemfg THEN itemfg.part-dscr1 ELSE ""
            oe-ordl.part-dscr2    = IF AVAILABLE itemfg THEN itemfg.part-dscr2 ELSE ""
            oe-ordl.cas-cnt       = IF AVAILABLE itemfg THEN itemfg.case-count ELSE 0 .
         
          IF AVAIL fg-bin THEN
          DO:
            ASSIGN
              oe-ordl.job-no        = fg-bin.job-no
              oe-ordl.job-no2       = fg-bin.job-no2 .
          END.
        if oe-ordl.pr-uom Begins "L" then
          assign oe-ordl.t-price =  oe-ordl.price -
                  round( (oe-ordl.price * oe-ordl.disc) / 100, 2).
        else if oe-ordl.pr-uom = "CS" and avail itemfg and
            itemfg.case-count ne 0 then
          assign oe-ordl.t-price = ((oe-ordl.qty / itemfg.case-count) *
                          oe-ordl.price) - round((((oe-ordl.qty /
                          itemfg.case-count) * oe-ordl.price) *
                          oe-ordl.disc) / 100, 2).
        else if oe-ordl.pr-uom = "C" then
          assign oe-ordl.t-price = (( oe-ordl.qty / 100) *
                     oe-ordl.price) - round(((( oe-ordl.qty / 100) *
                     oe-ordl.price) *  oe-ordl.disc) / 100, 2).
        else if  oe-ordl.pr-uom = "M" then
          assign oe-ordl.t-price = (( oe-ordl.qty / 1000) *
                 oe-ordl.price) - round(( (( oe-ordl.qty / 1000) *
                 oe-ordl.price) *  oe-ordl.disc) / 100, 2).
        else /** DEFAULT TO EACH **/
          assign oe-ordl.t-price = (( oe-ordl.qty ) *
                 oe-ordl.price) -
                round(( (( oe-ordl.qty ) *
                 oe-ordl.price) *  oe-ordl.disc) / 100, 2).
        /* 9903 CAH: Added Line item level notes support */
        find first edpoaddon of edpotran
        where edpoaddon.order-line = integer(edpoline.cust-po-line)
        and edpoaddon.note[1] > "" no-lock no-error.
/*        ws_logical = if avail edpoaddon then true else false.*/
        /* 9903 CAH: Moved this from header bill-i to line item ship-i */
        
        if first-of (edpoline.seq) then do:
           if not avail edpoaddon   then 
              find first edpoaddon of edpotran where edpoaddon.order-line = 0
                                                 and edpoaddon.note[1] > "" no-lock no-error.        
        end.    /* first of seq */
        if avail edpoaddon then 
        ASSIGN
          oe-ordl.ship-i[1] = edpoaddon.note[1]
          oe-ordl.ship-i[2] = edpoaddon.note[2]
          oe-ordl.ship-i[3] = edpoaddon.note[3]
          oe-ordl.ship-i[4] = edpoaddon.note[4].
        .
        
        assign
    oe-ord.t-weight  = oe-ord.t-weight  + oe-ordl.t-weight
    oe-ord.t-freight = oe-ord.t-freight + oe-ordl.t-freight
    oe-ordl.t-cost   = oe-ordl.cost * (oe-ordl.qty / 1000)
    oe-ord.t-cost    = oe-ord.t-cost  + oe-ordl.t-cost.
/*          DEF BUFFER xrel FOR oe-rel.                              */
/*          FIND FIRST xrel USE-INDEX seq-no EXCLUSIVE-LOCK NO-ERROR.*/
/*          ws_int = IF AVAIL xrel THEN                              */
/*          xrel.r-no + 1 ELSE   1.                                  */
          RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT iNextRelNo).
          CREATE oe-rel.
          ASSIGN
            oe-rel.r-no         = iNextRelNo
            oe-rel.company      = oe-ord.company
            oe-rel.ship-id      = shipto.ship-id
            oe-rel.ship-no      = shipto.ship-no
            oe-rel.sold-no      = oe-ord.sold-no
            oe-rel.tot-qty          = oe-ordl.qty
            oe-rel.po-no        = oe-ord.po-no
            oe-rel.i-no         = oe-ordl.i-no
            oe-rel.rel-date     = edpotran.request-date
            oe-rel.partial      = 0
            oe-rel.cust-no      = oe-ord.cust-no
            oe-rel.line         = oe-ordl.line
            oe-rel.o-no         = STRING(oe-ord.ord-no,"99999999")
            oe-rel.w-ord        = TRUE
            oe-rel.ship-date    = ?
            oe-rel.loc          =
            IF AVAIL fg-bin THEN fg-bin.loc ELSE oe-ord.loc
            oe-rel.loc-bin      =
            IF AVAIL fg-bin THEN fg-bin.loc-bin ELSE oe-rel.loc-bin
            oe-rel.tag          =
            IF AVAIL fg-bin THEN fg-bin.tag ELSE oe-rel.tag
            oe-rel.qty-case     =
            IF AVAIL fg-bin AND fg-bin.case-count > 0
            THEN fg-bin.case-count
            ELSE IF AVAIL itemfg AND itemfg.case-count > 0
            THEN itemfg.case-count
            ELSE 1
            oe-rel.cases        = TRUNC(oe-rel.qty / oe-rel.qty-case, 0)
            oe-rel.sman[1]      = oe-ord.sman[1]
            oe-rel.sman[2]      = oe-ord.sman[2]
            oe-rel.sman[3]      = oe-ord.sman[3]
            oe-rel.s-pct[1]     = oe-ord.s-pct[1]
            oe-rel.s-pct[2]     = oe-ord.s-pct[2]
            oe-rel.s-pct[3]     = oe-ord.s-pct[3]
            oe-rel.s-name[1]    = oe-ord.sname[1]
            oe-rel.s-name[2]    = oe-ord.sname[2]
            oe-rel.s-name[3]    = ''
            oe-rel.s-comm[1]    = oe-ord.s-comm[1]
            oe-rel.s-comm[2]    = oe-ord.s-comm[2]
            oe-rel.s-comm[3]    = oe-ord.s-comm[3]
            oe-rel.ord-no       = oe-ord.ord-no
            oe-rel.carrier      =
            IF shipto.carrier > "" THEN shipto.carrier ELSE oe-ord.carrier
            oe-rel.ship-addr[1] = shipto.ship-addr[1]
            oe-rel.ship-addr[2] = shipto.ship-addr[2]
            oe-rel.ship-city    = shipto.ship-city
            oe-rel.ship-state   = shipto.ship-state
            oe-rel.ship-zip     = shipto.ship-zip
            oe-rel.ship-i[1]    = oe-ord.ship-i[1]
            oe-rel.ship-i[2]    = oe-ord.ship-i[2]
            oe-rel.ship-i[3]    = oe-ord.ship-i[3]
            oe-rel.ship-i[4]    = oe-ord.ship-i[4]
            oe-rel.deleted      = oe-ord.deleted
            oe-ordl.rel-qty     = oe-ordl.rel-qty + oe-rel.qty.
          IF oe-rel.ship-i[1] = "" AND shipto.notes[1] > ""
            THEN
          DO i = 1 TO 4:
            oe-rel.ship-i[i] = shipto.notes[i].
          END.

/*          IF AVAIL itemfg then do:                                            */
/*            /* 9805 CAH: update the itemfg.q-alloc field, per Joe Hentz ... */*/
/*             Assign itemfg.q-alloc = itemfg.q-alloc + oe-ordl.qty.            */
/*          END.                                                                */

/*    FOR EACH edpoaddon OF edpotran:                                     */
/*      erclist = "".                                                     */
/*      IF pass = 2 THEN                                                  */
/*      DO:                                                               */
/*        {rc/incr.i ws_recs_selected}.                                   */
/*        IF edpoaddon.order-line > 0 THEN                                */
/*        FIND oe-ordl OF oe-ord                                          */
/*          WHERE                                                         */
/*          oe-ordl.line = edpoaddon.order-line NO-LOCK NO-ERROR.         */
/*        CREATE oe-ordm.                                                 */
/*        ASSIGN                                                          */
/*          oe-ordm.company = oe-ord.company                              */
/*          oe-ordm.ord-no  = oe-ord.ord-no                               */
/*          oe-ordm.est-no  = IF AVAIL oe-ordl THEN oe-ordl.est-no ELSE ""*/
/*          oe-ordm.line    = edpoaddon.line                              */
/*          oe-ordm.amt     = edpoaddon.amount                            */
/*          oe-ordm.dscr    = edpoaddon.description[1]                    */
/*          oe-ordm.posted  = FALSE                                       */
/*          .                                                             */
/*      END.    /* pass 2 */                                              */
/*    END.                                                                */
      
      END. /* each edPOLine */
      
      FIND EDDoc OF EDPOTran.
      ASSIGN EDDoc.Unique-Order-No = oe-ord.ord-no.
      
      ASSIGN oe-ord.spare-char-3 = EDPOTran.rec_key.
      
      MESSAGE "order creted: " oe-ord.ord-no
      VIEW-AS ALERT-BOX.
      
  END. /* each edPOTran */ 
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION getNextOrderNum RETURNS INTEGER 
	(  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
  DEFINE BUFFER bf-oe-ord FOR oe-ord.

  RUN sys/ref/asiseq.p (cocode,'order_seq',OUTPUT iNextOrder#) NO-ERROR.

  /* Supposed to be a new order number, so cannot be found on an existing order */
  DO WHILE CAN-FIND(FIRST bf-oe-ord
                    WHERE bf-oe-ord.company EQ cocode
                      AND bf-oe-ord.ord-no  EQ iNextOrder#):
     RUN sys/ref/asiseq.p (cocode,'order_seq',OUTPUT iNextOrder#) NO-ERROR.
  END.
  RETURN iNextOrder#.
		
END FUNCTION.

