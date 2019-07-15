

/*------------------------------------------------------------------------
    File        : poinq.p
    Purpose     : 
    Syntax      :

    Description : Return a Dataset of Estimate Corrugated box
    Author(s)   : 
    Created     : 14 Jan 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttb_po_inqlist NO-UNDO
        FIELD po-no            AS INT 
        FIELD vend_no          AS CHAR  
        FIELD due_date         AS CHAR FORMAT "99/99/9999"
        FIELD ship_id          AS CHAR FORMAT "x(8)"
        FIELD ship_name        AS CHAR FORMAT "x(30)"
        FIELD job_no           AS CHAR FORMAT "x(6)"  
        FIELD job_no2          AS INT
        FIELD s_num            AS INT
        FIELD i_no             AS CHAR FORMAT "x(15)"
        FIELD i_name           AS CHAR  FORMAT "x(30)" 
        FIELD s_wid            AS DECIMAL
        FIELD s_len            AS DECIMAL 
        FIELD vend_ino         AS CHAR
        FIELD ordqty           AS DECIMAL    
        FIELD poqtyrec         AS DECIMAL    
        FIELD qtyuom           AS CHAR FORMAT "x(4)"
        FIELD tqtyrec          AS DECIMAL
        FIELD consuom          AS CHAR FORMAT "x(4)"
        FIELD cost             AS DEC
        FIELD pruom            AS CHAR FORMAT "x(4)"
        FIELD buyer            AS CHAR FORMAT "x(10)"   
        FIELD stat             AS CHAR   FORMAT "x(9)"
        FIELD paid             AS CHAR  FORMAT "x(9)"
        FIELD poline           AS INT
        FIELD RecKey           AS CHAR 
        FIELD RecKey2           AS CHAR 
        .

DEFINE DATASET dsb_po_inqlist FOR ttb_po_inqlist .

DEFINE INPUT PARAMETER prmUser        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmPono        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVendor      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFgItem      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVenItem     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmDueDate     AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmJob         AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER prmJob2        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmOpen        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmClose       AS CHAR        NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsb_po_inqlist.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.

IF prmUser     = ? THEN ASSIGN prmUser    = "".
IF prmAction   = ? THEN ASSIGN prmAction  = "Select".
IF prmPono     = ? THEN ASSIGN prmPono    = "".
IF prmVendor   = ? THEN ASSIGN prmVendor  = "".  
IF prmFgItem   = ? THEN ASSIGN prmFgItem  = "".
IF prmVenItem  = ? THEN ASSIGN prmVenItem = "".
IF prmDueDate  = ? THEN ASSIGN prmDueDate = "".
IF prmJob      = ? THEN ASSIGN prmJob     = "".
IF prmJob2     = ? THEN ASSIGN prmJob2    = "".
IF prmOpen     = ? THEN ASSIGN prmOpen    = "".
IF prmClose    = ? THEN ASSIGN prmClose   = "".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp .
DEFINE VARIABLE tb_open AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE tb_closed AS LOGICAL INITIAL no NO-UNDO.
ASSIGN
    tb_open  = IF prmOpen = "True" THEN TRUE ELSE FALSE
    tb_closed  = IF prmClose = "True" THEN TRUE ELSE FALSE .

FUNCTION is-it-paid RETURNS LOGICAL
  (  /* parameter-definitions */  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR v-flg AS LOG.

FOR EACH  reftable NO-LOCK
    WHERE reftable.reftable EQ "AP-INVL" 
      AND reftable.company  EQ ""        
      AND reftable.loc      EQ ""        
      AND reftable.code     EQ STRING(po-ordl.po-no,"9999999999"),
    EACH  ap-invl NO-LOCK
    WHERE ap-invl.i-no              EQ int(reftable.code2) 
      AND ap-invl.po-no             EQ po-ordl.po-no 
      AND (ap-invl.line + 
           (ap-invl.po-no * -1000)) EQ po-ordl.line,
    EACH  ap-inv NO-LOCK
    WHERE ap-inv.i-no EQ ap-invl.i-no 
      AND ap-inv.due  EQ 0:

   v-flg = TRUE.

END.

  RETURN v-flg.   /* Function return value. */

END FUNCTION.

FUNCTION qty-in-ord-uom RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR ld AS DEC DECIMALS 10 EXTENT 2 NO-UNDO.

  DEF BUFFER b-po-ordl FOR po-ordl.

  

  FIND b-po-ordl WHERE ROWID(b-po-ordl) EQ ROWID(po-ordl) NO-LOCK NO-ERROR.

  ld[2] = b-po-ordl.t-rec-qty.

  IF b-po-ordl.item-type EQ YES                 AND
     b-po-ordl.pr-qty-uom NE b-po-ordl.cons-uom THEN DO:
    ld[2] = 0.

    FOR EACH rm-rcpth
        WHERE rm-rcpth.company   EQ b-po-ordl.company
          AND rm-rcpth.po-no     EQ STRING(b-po-ordl.po-no)
          AND rm-rcpth.i-no      EQ b-po-ordl.i-no
          AND rm-rcpth.rita-code EQ "R" NO-LOCK,
        EACH rm-rdtlh
        WHERE rm-rdtlh.r-no    EQ rm-rcpth.r-no
          AND rm-rdtlh.job-no  EQ b-po-ordl.job-no
          AND rm-rdtlh.job-no2 EQ b-po-ordl.job-no2
          AND rm-rdtlh.s-num   EQ b-po-ordl.s-num
        NO-LOCK:

      IF b-po-ordl.pr-qty-uom EQ "ROLL" AND rm-rdtlh.tag NE "" THEN ld[1] = 1.

      ELSE DO:
        ld[1] = rm-rdtlh.qty.

        IF rm-rcpth.pur-uom NE b-po-ordl.pr-qty-uom THEN DO:
          FIND FIRST item
              WHERE item.company EQ b-po-ordl.company
                AND item.i-no    EQ b-po-ordl.i-no
              NO-LOCK NO-ERROR.

          RUN custom/convquom.p(cocode, rm-rcpth.pur-uom, b-po-ordl.pr-qty-uom,
                                (IF AVAIL item THEN item.basis-w ELSE 0),
                                (IF b-po-ordl.pr-qty-uom EQ "ROLL" THEN 12
                                 ELSE b-po-ordl.s-len), b-po-ordl.s-wid,
                                (IF AVAIL item THEN item.s-dep ELSE 0),
                                ld[1], OUTPUT ld[1]).
        END.
      END.

      ld[2] = ld[2] + ld[1].
    END.
  END.

  IF b-po-ordl.pr-qty-uom EQ "EA" THEN DO:
    {sys/inc/roundup.i ld[2]}
  END.

  RETURN ld[2].   /* Function return value. */

END FUNCTION.
   

 IF prmAction = "Select" THEN DO:
 
    FOR EACH po-ordl WHERE  po-ordl.company = cocode 
        /*and po-ordl.po-no = 9999999*/ NO-LOCK, 
        FIRST po-ord WHERE po-ord.company eq po-ordl.company and 
        po-ord.po-no eq po-ordl.po-no NO-LOCK BY po-ordl.po-no DESC BY po-ordl.i-no BY po-ordl.LINE :
        
             

            create ttb_po_inqlist.
            assign
                ttb_po_inqlist.po-no         = po-ordl.po-no
                ttb_po_inqlist.vend_no       = string(po-ord.vend-no)
                ttb_po_inqlist.due_date      = STRING(po-ordl.due-date)
                ttb_po_inqlist.ship_id       = po-ord.ship-id
                ttb_po_inqlist.ship_name     = po-ord.ship-name
                ttb_po_inqlist.job_no        = po-ordl.job-no
                ttb_po_inqlist.job_no2       = po-ordl.job-no2
                ttb_po_inqlist.s_num         = po-ordl.s-num
                ttb_po_inqlist.i_no          = po-ordl.i-no
                ttb_po_inqlist.i_name        = po-ordl.i-name 
                ttb_po_inqlist.s_wid         = po-ordl.s-wid
                ttb_po_inqlist.s_len         = po-ordl.s-len 
                ttb_po_inqlist.vend_ino      = po-ordl.vend-i-no 
                ttb_po_inqlist.ordqty        = po-ordl.ord-qty  
                ttb_po_inqlist.poqtyrec      = dec(qty-in-ord-uom ())
                ttb_po_inqlist.qtyuom        = po-ordl.pr-qty-uom 
                ttb_po_inqlist.tqtyrec       = po-ordl.t-rec-qty
                ttb_po_inqlist.consuom       = po-ordl.cons-uom
                ttb_po_inqlist.cost          = po-ordl.cost
                ttb_po_inqlist.pruom         = po-ordl.pr-uom
                ttb_po_inqlist.buyer         = po-ord.buyer 
                ttb_po_inqlist.stat          = po-ord.stat
                ttb_po_inqlist.paid          = string(is-it-paid())
                ttb_po_inqlist.poline        = po-ordl.LINE 
                ttb_po_inqlist.RecKey        = po-ordl.rec_key
                ttb_po_inqlist.RecKey2       = po-ord.rec_key .

                      
       END. /*FOR EACH*/
 
  END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/
  
 IF prmAction = "Search" THEN DO:

 
    FOR EACH po-ordl WHERE 
       po-ordl.company eq cocode and 
           (po-ordl.po-no EQ INT(prmPono) OR prmPono = "") and 
           (po-ordl.vend-no EQ STRING(prmVendor) OR prmVendor = "") and 
           (po-ordl.i-no EQ STRING(prmFgItem) OR prmFgItem = "") and 
           (po-ordl.vend-i-no EQ STRING(prmVenItem) OR prmVenItem = "") AND
           (po-ordl.job-no EQ STRING(prmJob) OR prmJob = "") and 
           (po-ordl.due-date EQ DATE(prmDueDate) OR prmDueDate = "") and 
           (po-ordl.job-no2  EQ INT(prmJob2) OR prmJob2 = "") and 
            ((po-ordl.opened AND tb_open)   OR   
               (NOT po-ordl.opened AND tb_closed))  NO-LOCK, 
        FIRST po-ord WHERE                
        po-ord.company EQ po-ordl.company AND 
        po-ord.po-no EQ po-ordl.po-no 
        NO-LOCK BY po-ordl.po-no DESC BY po-ordl.i-no BY po-ordl.LINE: 

            create ttb_po_inqlist.
            assign
                ttb_po_inqlist.po-no         = po-ordl.po-no
                ttb_po_inqlist.vend_no       = string(po-ord.vend-no)
                ttb_po_inqlist.due_date      = STRING(po-ordl.due-date)
                ttb_po_inqlist.ship_id       = po-ord.ship-id
                ttb_po_inqlist.ship_name     = po-ord.ship-name
                ttb_po_inqlist.job_no        = po-ordl.job-no
                ttb_po_inqlist.job_no2       = po-ordl.job-no2
                ttb_po_inqlist.s_num         = po-ordl.s-num
                ttb_po_inqlist.i_no          = po-ordl.i-no
                ttb_po_inqlist.i_name        = po-ordl.i-name 
                ttb_po_inqlist.s_wid         = po-ordl.s-wid
                ttb_po_inqlist.s_len         = po-ordl.s-len 
                ttb_po_inqlist.vend_ino      = po-ordl.vend-i-no 
                ttb_po_inqlist.ordqty        = po-ordl.ord-qty  
                ttb_po_inqlist.poqtyrec      = dec(qty-in-ord-uom ())
                ttb_po_inqlist.qtyuom        = po-ordl.pr-qty-uom 
                ttb_po_inqlist.tqtyrec       = po-ordl.t-rec-qty
                ttb_po_inqlist.consuom       = po-ordl.cons-uom
                ttb_po_inqlist.cost          = po-ordl.cost
                ttb_po_inqlist.pruom         = po-ordl.pr-uom
                ttb_po_inqlist.buyer         = po-ord.buyer 
                ttb_po_inqlist.stat          = po-ord.stat
                ttb_po_inqlist.paid          = string(is-it-paid())
                ttb_po_inqlist.poline        = po-ordl.LINE 
                ttb_po_inqlist.RecKey        = po-ordl.rec_key 
                ttb_po_inqlist.RecKey2       = po-ord.rec_key .
                  
                END.   /* end of for loop*/
            
            
 END. /* end search */

