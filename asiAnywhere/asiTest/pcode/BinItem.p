
/*------------------------------------------------------------------------
    File        : Item.p
    Purpose     : OrderItem

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{BinItem.i}


DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmItemNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemJob.

DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE vJob AS CHARACTER NO-UNDO.
IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF prmItemNum = ? THEN ASSIGN prmItemNum = "".

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmAction = "Select" THEN
    MESSAGE "test" prmAction prmComp prmUser prmItemNum.
   /* FOR EACH oe-ordl where
        oe-ordl.company EQ prmComp AND
        oe-ordl.ord-no = int(prmOrderNum) AND
        oe-ordl.LINE = int(prmItemNum)
        NO-LOCK:
    ASSIGN vJob = FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) + TRIM(oe-ordl.job-no).*/
    FOR EACH itemfg where
        itemfg.company EQ prmComp AND
        itemfg.i-no = prmItemNum
        no-lock:
        
        for each fg-bin where fg-bin.company eq itemfg.company
            and fg-bin.i-no    eq itemfg.i-no AND fg-bin.qty NE 0
            no-lock:
            create ttItemJob.
            assign
                
               /* ttItemJob.job-no-disp       = job-hdr.job-no*/
                ttItemJob.job-no2           = fg-bin.job-no2
                ttItemJob.i-no              = itemfg.i-no
                ttItemJob.i-name            = itemfg.i-name
                ttItemJob.cust-no           = fg-bin.cust-no
                ttItemJob.cust              = itemfg.cust-no
                ttItemJob.loc               = fg-bin.loc
                ttItemJob.loc-bin           = fg-bin.loc-bin
                ttItemJob.tag               = fg-bin.tag
                ttItemJob.cases             = TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                ttItemJob.case-count        = fg-bin.case-count
                ttItemJob.cases-unit        = fg-bin.cases-unit
                ttItemJob.partial-count     = fg-bin.partial-count
                ttItemJob.qty               = fg-bin.qty
                ttItemJob.std-tot-cost      = fg-bin.std-tot-cost
                ttItemJob.std-mat-cost      = fg-bin.std-mat-cost
                ttItemJob.std-lab-cost      = fg-bin.std-lab-cost
                ttItemJob.std-var-cost      = fg-bin.std-var-cost
                ttItemJob.std-fix-cost      = fg-bin.std-fix-cost
                ttItemJob.last-cost         = fg-bin.last-cost
                ttItemJob.sell-uom          = fg-bin.pur-uom
                .
            ASSIGN vJob = FILL(" ",6 - LENGTH(TRIM(fg-bin.job-no))) + TRIM(fg-bin.job-no).
           ASSIGN ttItemJob.job-no-disp            = vJob.
                /*MESSAGE "jyoti" oe-ordl.job-no.*/
            IF ttItemJob.tag  <> "" THEN
                ttItemJob.tag  = SUBSTRING(ttItemJob.tag, 16,5).
            
            find first job-hdr where job-hdr.company eq fg-bin.company
                and job-hdr.i-no    eq fg-bin.i-no
                and job-hdr.job-no  eq fg-bin.job-no
                and job-hdr.job-no2 eq fg-bin.job-no2
                use-index i-no no-lock no-error.
            if avail job-hdr then assign ttItemJob.j-no = job-hdr.j-no.
            FOR EACH oe-relh NO-LOCK
          WHERE oe-relh.company EQ itemfg.company
            AND oe-relh.posted  EQ NO,
          EACH oe-rell NO-LOCK
          WHERE oe-rell.company EQ oe-relh.company
            AND oe-rell.r-no    EQ oe-relh.r-no
            AND oe-rell.i-no    EQ fg-bin.i-no
            AND oe-rell.loc     EQ fg-bin.loc
            AND oe-rell.loc-bin EQ fg-bin.loc-bin
            AND oe-rell.tag     EQ fg-bin.tag
            AND oe-rell.cust-no EQ fg-bin.cust-no:
       ttItemJob.rel-qty = ttItemJob.rel-qty + oe-rell.qty.
      END.

      FOR EACH oe-bolh NO-LOCK
          WHERE oe-bolh.company EQ itemfg.company
            AND oe-bolh.posted  EQ NO,
          EACH oe-boll NO-LOCK
          WHERE oe-boll.company EQ oe-bolh.company
            AND oe-boll.b-no    EQ oe-bolh.b-no
            AND oe-boll.i-no    EQ fg-bin.i-no
            AND oe-boll.loc     EQ fg-bin.loc
            AND oe-boll.loc-bin EQ fg-bin.loc-bin
            AND oe-boll.tag     EQ fg-bin.tag
            AND oe-boll.cust-no EQ fg-bin.cust-no:
        ttItemJob.bol-qty = ttItemJob.bol-qty + oe-boll.qty.
      END.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ itemfg.company NO-LOCK NO-ERROR.
      IF NOT oe-ctrl.u-inv THEN
      FOR EACH inv-line
          WHERE inv-line.company EQ itemfg.company
            AND inv-line.i-no    EQ fg-bin.i-no,
          EACH oe-boll
          WHERE oe-boll.company EQ inv-line.company
            AND oe-boll.b-no    EQ inv-line.b-no
            AND oe-boll.ord-no  EQ inv-line.ord-no
            AND oe-boll.i-no    EQ inv-line.i-no
            AND oe-boll.po-no   EQ inv-line.po-no
            AND oe-boll.loc     EQ fg-bin.loc
            AND oe-boll.loc-bin EQ fg-bin.loc-bin
            AND oe-boll.tag     EQ fg-bin.tag
            AND oe-boll.cust-no EQ fg-bin.cust-no:
       ttItemJob.bol-qty = ttItemJob.bol-qty + oe-boll.qty.
      END.

      ttItemJob.avl-qty = ttItemJob.qty - ttItemJob.rel-qty - ttItemJob.bol-qty.

END. /* each fg-bin */

END.




