
/*------------------------------------------------------------------------
    File         : Job Lookup
    Purpose     :  Job lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Oct 01 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttJobhdrLookup NO-UNDO 
        FIELD JOB       AS CHARACTER
        FIELD JOB2      AS INTEGER
        FIELD estno     AS CHARACTER
        FIELD i-no      AS CHAR
        FIELD ord-no    AS CHAR
        FIELD cust-no   AS CHAR
        FIELD blank-no  AS INT
        FIELD form-no   AS INT
        FIELD char-1    AS CHAR
        FIELD s-len     AS DECIMAL
        FIELD s-wid     AS DECIMAL
        FIELD s-dep     AS DECIMAL
        FIELD ord-qty AS CHAR
        FIELD  cost  AS DECIMAL
        FIELD  v-wid-frac  AS CHAR  
        FIELD  v-len-frac  AS CHAR
        FIELD  v-dep-frac   AS CHAR
        FIELD  setup     AS DECIMAL
        FIELD cons-cost AS DECIMAL
        FIELD  totcost  AS DECIMAL
        FIELD  adder    AS CHAR
        FIELD  nxtprcqty AS DEC
        FIELD  nxtprcst  AS DEC
        .
        
DEFINE DATASET dsJobhdrLookup FOR ttJobhdrLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmChar      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmJobno      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmJob2      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmSnum      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmBnum      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmPruom      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPrqtyuom      AS CHARACTER  NO-UNDO. 
DEFINE INPUT PARAMETER prmOrdqty      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmSwid      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmSlen      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmSdep     AS DECIMAL  NO-UNDO.
DEF    INPUT PARAMETER prmDis      AS DECIMAL NO-UNDO.
DEF    INPUT PARAMETER prmSetup      AS DECIMAL NO-UNDO.
DEF    INPUT PARAMETER prmConsuom      AS CHAR NO-UNDO.
DEF    INPUT PARAMETER prmCust      AS CHAR NO-UNDO.
DEF    INPUT PARAMETER prmItemType      AS CHAR NO-UNDO.
DEF INPUT PARAMETER prmpoNo  AS INT NO-UNDO.
DEF INPUT PARAMETER prmLine  AS INT NO-UNDO.

DEF OUTPUT PARAMETER cError AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsJobhdrLookup .

DEFINE VAR vJob AS CHAR.
DEFINE VAR vEst AS CHAR.


ASSIGN vJob = FILL(" ",6 - LENGTH(TRIM(prmText))) + TRIM(prmText)
       vEst =  FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) .

IF prmAction      = ? THEN ASSIGN prmAction      = "Select".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

DEFINE VARIABLE addersText AS CHARACTER NO-UNDO.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-setup FOR reftable.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-ord-qty AS dec NO-UNDO.

DEF TEMP-TABLE tt-job-mat NO-UNDO LIKE job-mat
    FIELD orig-lot-cost-upd AS LOG
    FIELD orig-lot-cost AS DEC DECIMALS 6
    FIELD row-id AS ROWID.
  
DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD std-uom AS CHAR.
DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20
    FIELD rec_key AS CHAR.


DEF TEMP-TABLE tt-eiv-2 NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20
    FIELD rec_key AS CHAR.


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.
def new shared var v-pocost1 as char.
def new shared var v-hold-op1 as log.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW  shared var v-po-qty as log initial true no-undo.
DEF NEW SHARED VAR v-basis-w AS DEC NO-UNDO. /* for po/po-adder2.p */
DEF NEW SHARED VAR v-len LIKE po-ordl.s-len NO-UNDO.
DEF NEW SHARED VAR v-wid LIKE po-ordl.s-wid NO-UNDO.
DEF NEW SHARED VAR v-dep LIKE po-ordl.s-len NO-UNDO.
def NEW shared var v-adder as dec extent 2 NO-UNDO.
def NEW shared var factor# as decimal no-undo.
def NEW shared var v-default-gl-log as log no-undo.
def NEW shared var v-default-gl-cha as cha no-undo.

def NEW shared var v-po-msf like sys-ctrl.int-fld no-undo.
DEF VAR ll-new-mat AS LOG NO-UNDO.
  DEF VAR ld-line-qty LIKE po-ordl.ord-qty NO-UNDO.
  DEF VAR ld-ord-qty LIKE oe-ordl.qty NO-UNDO.
  DEF VAR ld-job-qty AS DEC NO-UNDO.
  DEF VAR ld-line-cst LIKE po-ordl.cost NO-UNDO.
  DEF VAR ld-part-qty AS DEC NO-UNDO.
  DEF VAR ll-upd-job-qty AS LOG NO-UNDO.
  DEF VAR ll-update-cost AS LOG NO-UNDO.
  DEF VAR ll-qty-changed AS LOG NO-UNDO.
  DEF BUFFER b-job-hdr FOR job-hdr.
    DEF VAR fg-uom-list AS CHAR NO-UNDO.
    DEF VAR ld-roll-len AS DEC NO-UNDO.
    DEF VAR lv-save-job AS CHAR NO-UNDO.
DEF VAR lv-save-job2 AS CHAR NO-UNDO.
DEF VAR lv-save-s-num AS CHAR NO-UNDO.
DEF VAR lv-save-b-num AS CHAR NO-UNDO.
DEF VAR v-wid-frac AS CHAR NO-UNDO.
DEF VAR v-len-frac AS CHAR NO-UNDO.
DEF VAR v-dep-frac AS CHAR NO-UNDO.

ASSIGN
    cocode = prmComp
    g_company = cocode.

{windows/l-jobmt1.i NEW}

{fg/fullset.i NEW}

DO TRANSACTION:
  {sys/inc/pocostq.i}
  {sys/inc/poqty.i}
  {sys/inc/pouom.i}
  {sys/inc/aptax.i}
END.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

FIND FIRST uom NO-LOCK WHERE uom.uom EQ "ROLL" NO-ERROR.
IF AVAIL uom THEN ld-roll-len = uom.mult.


 FIND FIRST rm-ctrl WHERE rm-ctrl.company EQ cocode NO-LOCK.
  FIND FIRST fg-ctrl WHERE fg-ctrl.company EQ cocode NO-LOCK.

  RUN po/po-sysct.p.
  {sys/ref/pocost.i}
  assign
   v-pocost1  = v-pocost
   v-hold-op1 = v-hold-op.

DEF BUFFER b-w-po-ordl FOR w-po-ordl.

 
if prmAction = "select" then do:
    FOR EACH job-hdr WHERE job-hdr.company = prmComp and 
        job-hdr.opened = yes NO-LOCK,
         EACH job OF job-hdr NO-LOCK BY job-hdr.job-no DESC BY job-hdr.job-no2 DESC BY job-hdr.i-no:
        create ttJobhdrLookup.
        assign                                     
            ttJobhdrLookup.JOB        = job-hdr.job-no
            ttJobhdrLookup.JOB2        = job-hdr.job-no2 
            ttJobhdrLookup.estno      = job-hdr.est-no
            ttJobhdrLookup.i-no       = job-hdr.i-no
            ttJobhdrLookup.ord-no     = string(job-hdr.ord-no)
            ttJobhdrLookup.cust-no    = job-hdr.cust-no
            ttJobhdrLookup.form-no    = job-hdr.frm  .                       

        FIND FIRST job-mat
             WHERE job-mat.company EQ prmComp
               AND job-mat.job-no  EQ job-hdr.job-no
               AND job-mat.job-no2 EQ INT(job-hdr.job-no2)
               AND job-mat.rm-i-no EQ prmItem NO-LOCK NO-ERROR .
        IF AVAIL job-mat THEN
                ttJobhdrLookup.blank-no    = job-mat.blank-no.

    END.	 /* FOR EACH job-hdr */
    FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
    IF AVAILABLE users THEN DO:
        IF users.internal-user = NO THEN DO:
            FOR EACH ttJobhdrLookup:
                FIND FIRST usercust WHERE usercust.user_id = prmUser 
                    AND usercust.cust-no = ttJobhdrLookup.cust-no
                    AND usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE usercust THEN DELETE ttJobhdrLookup.
            END. /*FOR EACH ttJobhdrLookup*/
        END. /*IF users.internal-user = NO*/
        IF users.internal-user = YES THEN DO:
            FIND FIRST usercust WHERE usercust.user_id = prmUser 
                 AND usercust.company EQ prmComp
                NO-LOCK NO-ERROR.
        END. /*IF users.internal-user = YES*/
    END. /*IF AVAILABLE users*/
    ELSE DO:
        FOR EACH ttJobhdrLookup:
            DELETE ttJobhdrLookup.
        END.
    END. /*IF NOT AVAILABLE users*/

END.  /*ifif prmAction <> "search" */
ELSE
do:
    
    IF prmField = "item" then do:
        IF prmCondition = "EQUAL" then do:

            FOR EACH job-hdr where job-hdr.company = prmComp AND
                job-hdr.opened = yes NO-LOCK  BY job-hdr.job-no DESC BY job-hdr.job-no2 DESC BY job-hdr.i-no:
                IF job-hdr.i-no = prmText  THEN
                DO:
                   create ttJobhdrLookup.
                  assign                                     
           ttJobhdrLookup.JOB        = job-hdr.job-no
            ttJobhdrLookup.JOB2        = job-hdr.job-no2 
            ttJobhdrLookup.estno      = job-hdr.est-no
            ttJobhdrLookup.i-no       = job-hdr.i-no
            ttJobhdrLookup.ord-no     = string(job-hdr.ord-no)
            ttJobhdrLookup.cust-no    = job-hdr.cust-no
            ttJobhdrLookup.form-no    = job-hdr.frm  .                          

        FIND FIRST job-mat
             WHERE job-mat.company EQ prmComp
               AND job-mat.job-no  EQ job-hdr.job-no
               AND job-mat.job-no2 EQ INT(job-hdr.job-no2)
               AND job-mat.rm-i-no EQ prmItem NO-LOCK NO-ERROR .
        IF AVAIL job-mat THEN
               ttJobhdrLookup.blank-no    = job-mat.blank-no.
                END.
            END.   /*FOR EACH job-hdr*/
        END.   /* IF prmCondition = "EQUAL"*/
        if prmCondition = "BEGIN" then do:
            
            FOR EACH job-hdr WHERE job-hdr.company = prmComp
                AND job-hdr.opened = YES  
                AND  job-hdr.i-no begins prmText NO-LOCK BY job-hdr.job-no DESC BY job-hdr.job-no2 DESC BY job-hdr.i-no :

                
                   create ttJobhdrLookup.
                   assign                                     
           ttJobhdrLookup.JOB        = job-hdr.job-no
            ttJobhdrLookup.JOB2        = job-hdr.job-no2 
            ttJobhdrLookup.estno      = job-hdr.est-no
            ttJobhdrLookup.i-no       = job-hdr.i-no
            ttJobhdrLookup.ord-no     = string(job-hdr.ord-no)
            ttJobhdrLookup.cust-no    = job-hdr.cust-no
            ttJobhdrLookup.form-no    = job-hdr.frm  .                        

        FIND FIRST job-mat
             WHERE job-mat.company EQ prmComp
               AND job-mat.job-no  EQ job-hdr.job-no
               AND job-mat.job-no2 EQ INT(job-hdr.job-no2)
               AND job-mat.rm-i-no EQ prmItem NO-LOCK NO-ERROR .
        IF AVAIL job-mat THEN
              ttJobhdrLookup.blank-no    = job-mat.blank-no.
               

               
            END.  /*FOR EACH job-hdr*/
        END.   /*if prmCondition = "BEGIN" */     
    END.   /*IF prmField = "ANY" then do:*/
    if prmField = "job-no" then do:
        if prmCondition = "EQUAL" then do:        
            FOR EACH job-hdr WHERE job-hdr.company = prmComp 
                AND job-hdr.opened = yes
                AND job-hdr.job-no = vJob NO-LOCK  BY job-hdr.job-no DESC BY job-hdr.job-no2 DESC BY job-hdr.i-no:           
                create ttJobhdrLookup.
               assign                                     
            ttJobhdrLookup.JOB        = job-hdr.job-no
            ttJobhdrLookup.JOB2        = job-hdr.job-no2 
            ttJobhdrLookup.estno      = job-hdr.est-no
            ttJobhdrLookup.i-no       = job-hdr.i-no
            ttJobhdrLookup.ord-no     = string(job-hdr.ord-no)
            ttJobhdrLookup.cust-no    = job-hdr.cust-no
            ttJobhdrLookup.form-no    = job-hdr.frm  .                     

        FIND FIRST job-mat
             WHERE job-mat.company EQ prmComp
               AND job-mat.job-no  EQ job-hdr.job-no
               AND job-mat.job-no2 EQ INT(job-hdr.job-no2)
               AND job-mat.rm-i-no EQ prmItem NO-LOCK NO-ERROR .
        IF AVAIL job-mat THEN
                ttJobhdrLookup.blank-no    = job-mat.blank-no.
            END.  /*FOR EACH job-hdr*/
        END.   /*if prmCondition = "EQUAL"*/
        if prmCondition = "BEGIN" then do:            
            FOR EACH job-hdr WHERE job-hdr.company = prmComp 
                AND job-hdr.opened = yes
                AND job-hdr.job-no begins vJob NO-LOCK  BY job-hdr.job-no DESC BY job-hdr.job-no2 DESC BY job-hdr.i-no:
                create ttJobhdrLookup.
                assign                                     
            ttJobhdrLookup.JOB        = job-hdr.job-no
            ttJobhdrLookup.JOB2        = job-hdr.job-no2 
            ttJobhdrLookup.estno      = job-hdr.est-no
            ttJobhdrLookup.i-no       = job-hdr.i-no
            ttJobhdrLookup.ord-no     = string(job-hdr.ord-no)
            ttJobhdrLookup.cust-no    = job-hdr.cust-no
            ttJobhdrLookup.form-no    = job-hdr.frm  .    
        FIND FIRST job-mat
             WHERE job-mat.company EQ prmComp
               AND job-mat.job-no  EQ job-hdr.job-no
               AND job-mat.job-no2 EQ INT(job-hdr.job-no2)
               AND job-mat.rm-i-no EQ prmItem NO-LOCK NO-ERROR .
        IF AVAIL job-mat THEN
               ttJobhdrLookup.blank-no    = job-mat.blank-no.
            END.  /*FOR EACH job-hdr*/
        END.  /*if prmCondition = "BEGIN"*/       
    END.   /*  if prmField = "job-no" then do:*/
    

FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttJobhdrLookup:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.cust-no = ttJobhdrLookup.cust-no
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttJobhdrLookup.
                END. /*FOR EACH ttJobhdrLookup*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttJobhdrLookup:
                DELETE ttJobhdrLookup.
            END.
        END. /*IF NOT AVAILABLE users*/

END. /* IF prmAction = search then do: */   




if prmAction = "JobData" then do:
    
    FIND FIRST item
         WHERE item.company EQ g_company
           AND item.i-no    EQ prmItem
           AND  prmItemType EQ "RM"
         NO-LOCK NO-ERROR.
    
    IF NOT AVAIL ITEM THEN
    FIND FIRST itemfg
         WHERE itemfg.company EQ g_company
           AND itemfg.i-no    EQ prmItem
         NO-LOCK NO-ERROR.
    
    IF AVAIL itemfg THEN
        RETURN.

       FIND FIRST job
         WHERE job.company EQ g_company
           AND job.job-no  EQ  FILL(" ",6 - LENGTH(TRIM(prmJobno))) + TRIM(prmJobno)
           AND job.job-no2 EQ INT(prmJob2)
         NO-LOCK NO-ERROR.
    
      IF NOT AVAIL job THEN DO:
          cError = "Invalid Job, try help..." .
          RETURN.
      END.

     FIND FIRST item
         WHERE item.company EQ g_company
           AND item.i-no    EQ prmItem
         NO-LOCK NO-ERROR.
    
     IF AVAIL item AND prmJobno NE "" THEN DO:
        FIND tt-job-mat
            WHERE tt-job-mat.company EQ g_company
              AND tt-job-mat.job     EQ job.job
              AND tt-job-mat.job-no  EQ FILL(" ",6 - LENGTH(TRIM(prmJobno))) + TRIM(prmJobno)
              AND tt-job-mat.job-no2 EQ INT(prmJob2)
              AND tt-job-mat.rm-i-no EQ prmItem
              AND tt-job-mat.frm     EQ INT(prmSnum) 
            NO-LOCK NO-ERROR.
       
        IF NOT AVAIL tt-job-mat OR prmSnum EQ 0 THEN
        FIND FIRST job-mat
            WHERE job-mat.company   EQ g_company
              AND job-mat.job       EQ job.job
              AND job-mat.job-no    EQ FILL(" ",6 - LENGTH(TRIM(prmJobno))) + TRIM(prmJobno)
              AND job-mat.job-no2   EQ INT(prmJob2)
              AND (job-mat.rm-i-no  EQ prmItem        OR prmItem EQ "")
              AND (job-mat.frm EQ INT(prmSnum)  OR INT(prmSnum) EQ 0)
              AND (job-mat.blank-no EQ INT(prmBnum)  OR INT(prmBnum) EQ 0)
            NO-LOCK NO-ERROR.
      
        IF (NOT AVAIL tt-job-mat OR prmSnum EQ 0)  AND
           NOT AVAIL job-mat                    /*AND
           INT(prmSnum) NE 0*/ THEN DO:

            ll-new-mat = NO.
       
             ASSIGN  cError =  "Update item on Job file?" .
             RETURN.
             /* UPDATE ll-new-mat.*/

          /*IF ll-new-mat THEN DO:
            RUN replace-job-mat.
            IF NOT AVAIL tt-job-mat THEN
              FIND tt-job-mat WHERE RECID(tt-job-mat) EQ fil_id NO-LOCK NO-ERROR.
            ELSE
            IF NOT AVAIL job-mat AND INDEX("MOXY789",ITEM.mat-type) EQ 0 THEN
              DELETE tt-job-mat.
            IF AVAIL tt-job-mat THEN ttJobhdrLookup.blank-no = (tt-job-mat.blank-no).
            RELEASE job-mat.
          END.*/
        END.
  create ttJobhdrLookup.       
        IF AVAIL tt-job-mat OR AVAIL job-mat THEN DO:
           IF NOT AVAIL tt-job-mat THEN DO:
              EMPTY TEMP-TABLE tt-job-mat.
              CREATE tt-job-mat.
              BUFFER-COPY job-mat EXCEPT rec_key TO tt-job-mat.
           END.
          
           ELSE
           IF tt-job-mat.blank-no EQ 0 THEN
              tt-job-mat.blank-no = INT(prmBnum).
           
           FIND FIRST job-hdr
               WHERE job-hdr.company   EQ g_company
                 AND job-hdr.job       EQ tt-job-mat.job
                 AND job-hdr.job-no    EQ tt-job-mat.job-no
                 AND job-hdr.job-no2   EQ tt-job-mat.job-no2
                 AND job-hdr.frm       EQ tt-job-mat.frm
                 AND (job-hdr.blank-no EQ tt-job-mat.blank-no OR tt-job-mat.blank-no EQ 0)
               NO-LOCK NO-ERROR.
       
           IF NOT AVAIL job-hdr THEN
           FIND FIRST job-hdr
               WHERE job-hdr.company   EQ g_company
                 AND job-hdr.job       EQ tt-job-mat.job
                 AND job-hdr.job-no    EQ tt-job-mat.job-no
                 AND job-hdr.job-no2   EQ tt-job-mat.job-no2
               NO-LOCK NO-ERROR.
          
           IF AVAIL job-hdr THEN
           FIND FIRST oe-ordl
               WHERE oe-ordl.company EQ job-hdr.company
                 AND oe-ordl.ord-no  EQ job-hdr.ord-no
                 AND oe-ordl.i-no    EQ job-hdr.i-no
               NO-LOCK NO-ERROR.
          
           IF AVAIL oe-ordl THEN
           FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
          
           IF AVAIL oe-ord THEN
             ASSIGN
             ttJobhdrLookup.cust-no = oe-ord.cust-no
             ttJobhdrLookup.ord-no  = STRING(oe-ord.ord-no).
        
           /* S-8-POQTY JOBQTY or NETSHTS */
           IF v-po-qty                        OR
              tt-job-mat.n-up EQ 0            OR
              NOT CAN-DO("B,P",item.mat-type) THEN
             ld-line-qty = tt-job-mat.qty.  /* Job Qty */
          
           ELSE DO:
             ASSIGN
              ld-line-qty = 0
              ld-part-qty = 0
              ld-ord-qty  = 0
              ld-job-qty  = 0.
          
             IF AVAIL job-hdr THEN
             FIND FIRST job
                 WHERE job.company EQ tt-job-mat.company
                   AND job.job     EQ tt-job-mat.job
                   AND job.job-no  EQ tt-job-mat.job-no
                   AND job.job-no2 EQ tt-job-mat.job-no2
                 NO-LOCK NO-ERROR.
          
             IF AVAIL job THEN
             FIND FIRST est
                 WHERE est.company EQ job.company
                   AND est.est-no  EQ job.est-no
                 NO-LOCK NO-ERROR.
          
             FOR EACH job-hdr NO-LOCK
                 WHERE job-hdr.company   EQ tt-job-mat.company
                   AND job-hdr.job       EQ tt-job-mat.job
                   AND job-hdr.job-no    EQ tt-job-mat.job-no
                   AND job-hdr.job-no2   EQ tt-job-mat.job-no2
                   AND (job-hdr.frm      EQ tt-job-mat.frm OR
                        (AVAIL est AND (est.est-type EQ 2 OR est.est-type EQ 6)))
                 BREAK BY job-hdr.i-no:
          
               ld-job-qty = ld-job-qty + job-hdr.qty.
          
               IF LAST-OF(job-hdr.i-no) THEN DO:
                 ld-ord-qty = 0.
                 FOR EACH b-job-hdr FIELDS(qty) NO-LOCK
                     WHERE b-job-hdr.company EQ job-hdr.company
                       AND b-job-hdr.job     EQ job-hdr.job
                       AND b-job-hdr.job-no  EQ job-hdr.job-no
                       AND b-job-hdr.job-no2 EQ job-hdr.job-no2
                       AND b-job-hdr.i-no    EQ job-hdr.i-no
                       AND b-job-hdr.ord-no  EQ job-hdr.ord-no:
          
                   ld-ord-qty = ld-ord-qty + b-job-hdr.qty.
                 END.

                 ASSIGN
                    ld-job-qty = ld-ord-qty / ld-job-qty
                    ld-ord-qty = 0.
          
                 FOR EACH oe-ordl FIELDS(qty job-no job-no2)
                     WHERE oe-ordl.company EQ job-hdr.company
                       AND oe-ordl.ord-no  EQ job-hdr.ord-no
                       AND oe-ordl.i-no    EQ job-hdr.i-no
                     NO-LOCK:
          
                   IF (oe-ordl.job-no EQ job-hdr.job-no AND
                       oe-ordl.job-no2 EQ job-hdr.job-no2) OR 
                       oe-ordl.job-no EQ "" THEN
                       ld-ord-qty = ld-ord-qty + oe-ordl.qty.
                 END.
          
                 ASSIGN
                  ld-line-qty = ld-line-qty + (ld-ord-qty * ld-job-qty)
                  ld-job-qty  = 0.

               END.
             END.

             IF AVAIL est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN
             DO:
                IF tt-job-mat.frm NE ? THEN
                   FOR EACH eb FIELDS(yld-qty)
                       WHERE eb.company EQ job.company
                         AND eb.est-no  EQ job.est-no
                         AND eb.form-no EQ tt-job-mat.frm
                       NO-LOCK:
                  
                     ld-part-qty = ld-part-qty +
                                   (ld-line-qty * IF eb.yld-qty LT 0 THEN (-1 / eb.yld-qty)
                                                                     ELSE eb.yld-qty).
                   END.
                 ELSE
                    FOR EACH w-po-ordl
                        BREAK BY w-po-ordl.s-num:
                    
                        IF FIRST-OF(w-po-ordl.s-num) THEN
                           FOR EACH eb FIELDS(yld-qty) WHERE
                               eb.company EQ job.company AND
                               eb.est-no  EQ job.est-no AND
                               eb.form-no EQ w-po-ordl.s-num
                               NO-LOCK:
                    
                               ld-part-qty = ld-part-qty +
                                             (ld-line-qty * IF eb.yld-qty LT 0 THEN (-1 / eb.yld-qty)
                                             ELSE eb.yld-qty).
                           END.
                    END.
                 
             END.
             ELSE
                ld-part-qty = ld-line-qty.
        
             ld-line-qty = ld-part-qty / tt-job-mat.n-up.
             IF ld-line-qty = 0 THEN DO:
               ld-line-qty = tt-job-mat.qty.  /* Job Qty */
               find first sys-ctrl
                 where sys-ctrl.company eq cocode
                   and sys-ctrl.name eq "JOBQTYCUST"
                 no-lock no-error.
               IF AVAIL sys-ctrl AND sys-ctrl.log-fld AND avail(job-hdr) 
                  AND v-po-qty = NO THEN DO:
                 FIND cust WHERE cust.cust-no = job-hdr.cust-no
                           NO-LOCK NO-ERROR.
                 IF avail(cust) AND cust.over-pct > 0 THEN
                   ld-line-qty = ld-line-qty / (1 + (cust.over-pct / 100)).
               END.
             END.

             IF prmPrqtyuom EQ "EA" THEN DO:
               {sys/inc/roundup.i ld-line-qty}             
             END.
           END.
   
           ASSIGN
            ttJobhdrLookup.JOB  = tt-job-mat.job-no
            ttJobhdrLookup.JOB2 = (tt-job-mat.job-no2)
            ttJobhdrLookup.form-no   = (tt-job-mat.frm)
            ttJobhdrLookup.blank-no   = (tt-job-mat.blank-no)
            lv-save-job                  = prmJobno
            lv-save-job2                 = string(prmJob2)
            lv-save-s-num                = string(prmSnum)
            lv-save-b-num                = string(prmBnum)
            v-len                        = tt-job-mat.len
            v-wid                        = tt-job-mat.wid
            v-dep                        = item.s-dep
            ld-line-cst                  = tt-job-mat.std-cost.
                 

           IF tt-job-mat.qty-uom NE prmPrqtyuom THEN
              RUN sys/ref/convquom.p (tt-job-mat.qty-uom, prmPrqtyuom,
                                      tt-job-mat.basis-w, v-len, v-wid, v-dep,
                                      ld-line-qty, OUTPUT ld-line-qty).
          
           IF tt-job-mat.sc-uom NE prmPruom THEN
              RUN sys/ref/convcuom.p (tt-job-mat.sc-uom, prmPruom,
                                      tt-job-mat.basis-w, v-len, v-wid, v-dep,
                                      ld-line-cst, OUTPUT ld-line-cst).
          
           
          ASSIGN
            {po/calc16.i v-len}
            {po/calc16.i v-wid}
            {po/calc16.i v-dep}
            ttJobhdrLookup.s-len   = (v-len)
            ttJobhdrLookup.s-wid   = (v-wid)
            ttJobhdrLookup.s-dep   = (v-dep)
            ll-upd-job-qty = YES
            ll-qty-changed = DEC(prmOrdqty) NE ld-line-qty
            ll-update-cost = YES.

           /*IF ip-type EQ "Update" THEN
              MESSAGE "Import Job Quantity?"
                 VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                 UPDATE ll-upd-job-qty.*/

           IF ll-upd-job-qty EQ YES THEN
              ttJobhdrLookup.ord-qty = STRING(ld-line-qty).

           /*IF ip-type EQ "Update" THEN
           DO:
              ll-update-cost = NO.

             /* IF ll-qty-changed THEN
                 MESSAGE "Import Cost?"
                 VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                 UPDATE ll-update-cost.*/
           END.*/

           IF ll-update-cost THEN
           DO:
              IF tt-job-mat.orig-lot-cost-upd EQ NO THEN
                 ttJobhdrLookup.cost  = ROUND(ld-line-cst,5).
              ELSE
                 ttJobhdrLookup.cost  = ROUND(tt-job-mat.orig-lot-cost,5).
           END.
               
           RUN sys\inc\decfrac2.p(INPUT DEC(v-wid), INPUT 32, OUTPUT v-wid-frac).
           RUN sys\inc\decfrac2.p(INPUT DEC(v-len), INPUT 32, OUTPUT v-len-frac).
           RUN sys\inc\decfrac2.p(INPUT DEC(v-dep), INPUT 32, OUTPUT v-dep-frac).


            ASSIGN
             ttJobhdrLookup.v-wid-frac = v-wid-frac
             ttJobhdrLookup.v-len-frac = v-len-frac
             ttJobhdrLookup.v-dep-frac = v-dep-frac.

            ASSIGN 
                prmOrdqty = ttJobhdrLookup.ord-qty
                prmChar   =  string(ttJobhdrLookup.cost) .

           IF ll-update-cost AND v-pocost1 BEGINS "Vendor" THEN DO:
              RUN vend-cost (YES).
              IF addersText NE "" THEN DO:
              ASSIGN
                ttJobhdrLookup.adder = "Adder Charges        "
                                      /*+ FILL(" ",12 - LENGTH("Cost/" + prmpruom)*/
                                      + "Cost/" + prmpruom 
                                      + CHR(10) + "=============================="  
                                      + CHR(10) + addersText .
              END.
           END.
         
           IF NOT ll-new-mat THEN EMPTY TEMP-TABLE tt-job-mat.
          
           END.
     END.
  

    END.	 /* FOR EACH prmAction */



  PROCEDURE vend-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  PARAMs:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-calc-cost AS LOG NO-UNDO.  

  DEF VAR v-qty  AS DEC NO-UNDO.
  DEF VAR v-cost AS DEC NO-UNDO.
  DEF VAR v-pb-qty AS DEC NO-UNDO.
  DEF VAR v-pb-stp AS DEC NO-UNDO.
  DEF VAR v-pb-cst AS DEC NO-UNDO.
  DEF VAR v-pb-cns AS DEC NO-UNDO.
  DEF VAR v-save-qty AS DEC NO-UNDO.
  DEF VAR v-setup AS DEC NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-added-cost AS DEC NO-UNDO.
  DEF VAR lv-added-cons-cost AS DEC NO-UNDO.
  DEF VAR lv-adder-setup AS DEC NO-UNDO.
  DEF VAR lv-recid AS RECID NO-UNDO.
  DEF VAR lv-t-cost AS DEC NO-UNDO.
  DEF VAR ld-dim-charge AS DEC NO-UNDO.
  DEF VAR v-index AS INT NO-UNDO.

  EMPTY TEMP-TABLE tt-ei.
  EMPTY TEMP-TABLE tt-eiv.
  
  FIND FIRST po-ordl WHERE 
       po-ordl.company eq cocode and 
           po-ordl.po-no EQ INT(prmpoNo)   and
           po-ordl.LINE EQ INT(prmLine) NO-LOCK NO-ERROR.
        
        FIND FIRST po-ord WHERE 
            po-ord.company eq cocode and 
            po-ord.po-no EQ INT(prmpoNo)   NO-LOCK NO-ERROR. 
 
  
    RUN set-dims.

    /* for adders */
    RELEASE job-mat.
    FIND FIRST job NO-LOCK
        WHERE job.company EQ cocode
          AND job.job-no  EQ prmJobno
          AND job.job-no2 EQ INT(prmJob2)
        NO-ERROR.
    IF AVAIL job THEN
    FIND FIRST job-mat NO-LOCK
        WHERE job-mat.company  EQ job.company
          AND job-mat.job      EQ job.job
          AND job-mat.job-no   EQ job.job-no
          AND job-mat.job-no2  EQ job.job-no2
          AND job-mat.frm      EQ INT(prmSnum)
          AND job-mat.blank-no EQ INT(prmBnum) 
        USE-INDEX seq-idx NO-ERROR.
        
    IF AVAIL job-mat THEN lv-recid = RECID(job-mat).

    v-ord-qty = DEC(prmOrdqty).

    IF prmItemType EQ "RM" THEN DO:
       FIND FIRST e-item NO-LOCK
           WHERE e-item.company EQ cocode
             AND e-item.i-no    EQ prmItem
           NO-ERROR.
      
       IF AVAIL e-item THEN DO:
          CREATE tt-ei.
          ASSIGN tt-ei.std-uom = e-item.std-uom.
      
          FIND FIRST e-item-vend NO-LOCK
              WHERE e-item-vend.company EQ e-item.company
                AND e-item-vend.i-no    EQ e-item.i-no
                AND e-item-vend.vend-no EQ po-ord.vend-no
              NO-ERROR.
      
          IF AVAIL e-item-vend THEN DO:

             CREATE tt-eiv.
             tt-eiv.rec_key = e-item-vend.rec_key.
             DO v-index = 1 TO 10:
                ASSIGN
                   tt-eiv.run-qty[v-index] = e-item-vend.run-qty[v-index]
                   tt-eiv.run-cost[v-index] = e-item-vend.run-cost[v-index]
                   tt-eiv.setups[v-index] = e-item-vend.setups[v-index].
             END.

             FIND FIRST b-qty WHERE
                  b-qty.reftable = "vend-qty" AND
                  b-qty.company = e-item-vend.company AND
                          b-qty.CODE    = e-item-vend.i-no AND
                  b-qty.code2   = e-item-vend.vend-no
                  NO-LOCK NO-ERROR.
         
             IF AVAIL b-qty THEN
             DO:
                FIND FIRST b-cost WHERE
                     b-cost.reftable = "vend-cost" AND
                     b-cost.company = e-item-vend.company AND
                             b-cost.CODE    = e-item-vend.i-no AND
                     b-cost.code2   = e-item-vend.vend-no
                     NO-LOCK NO-ERROR.

                FIND FIRST b-setup WHERE
                     b-setup.reftable = "vend-setup" AND
                     b-setup.company = e-item-vend.company AND
                             b-setup.CODE    = e-item-vend.i-no AND
                     b-setup.code2   = e-item-vend.vend-no
                     NO-LOCK NO-ERROR.
             
                DO v-index = 1 TO 10:
                   ASSIGN
                      tt-eiv.run-qty[v-index + 10] = b-qty.val[v-index]
                      tt-eiv.run-cost[v-index + 10] = b-cost.val[v-index]
                      tt-eiv.setups[v-index + 10] = b-setup.val[v-index].
                END.
             END.
          END.
       END.
    END.

    ELSE DO:
      FIND FIRST e-itemfg NO-LOCK
          WHERE e-itemfg.company EQ cocode
            AND e-itemfg.i-no    EQ prmItem
          NO-ERROR.

      IF AVAIL e-itemfg THEN DO:
        CREATE tt-ei.
        ASSIGN tt-ei.std-uom = e-itemfg.std-uom.

        IF prmCust NE "" THEN
           FIND FIRST e-itemfg-vend NO-LOCK
               WHERE e-itemfg-vend.company EQ e-itemfg.company
                 AND e-itemfg-vend.i-no    EQ e-itemfg.i-no
                 AND e-itemfg-vend.vend-no EQ po-ord.vend-no
                 AND e-itemfg-vend.cust-no EQ prmCust
               NO-ERROR.

        /* gdm - 06040918 - check for vendor */
        IF NOT AVAIL e-itemfg-vend THEN
           FIND FIRST e-itemfg-vend NO-LOCK
               WHERE e-itemfg-vend.company EQ e-itemfg.company
                 AND e-itemfg-vend.i-no    EQ e-itemfg.i-no
                 AND e-itemfg-vend.vend-no EQ po-ord.vend-no
               NO-ERROR.

        /* gdm - check for blank vendor */
        IF NOT AVAIL e-itemfg-vend THEN
           FIND FIRST e-itemfg-vend NO-LOCK
               WHERE e-itemfg-vend.company EQ e-itemfg.company
                 AND e-itemfg-vend.i-no    EQ e-itemfg.i-no 
                 AND e-itemfg-vend.vend-no EQ "" NO-ERROR.

        IF AVAIL e-itemfg-vend THEN DO:            
          CREATE tt-eiv.
          tt-eiv.rec_key = e-itemfg-vend.rec_key.
          DO v-index = 1 TO 10:
             ASSIGN
                tt-eiv.run-qty[v-index] = e-itemfg-vend.run-qty[v-index]
                tt-eiv.run-cost[v-index] = e-itemfg-vend.run-cost[v-index]
                tt-eiv.setups[v-index] = e-itemfg-vend.setups[v-index].
          END.
          RELEASE e-itemfg-vend.
        END.
      END.
    END.

    IF AVAIL tt-eiv THEN DO:                
      ASSIGN
       v-cost = decimal(prmChar) /*cost*/
       v-qty  = DEC(prmOrdqty).
    

      IF tt-ei.std-uom NE prmPrqtyuom          AND
        ( /*po-ordl.item-type                                        OR*/
         LOOKUP(tt-ei.std-uom,fg-uom-list)                  EQ 0 OR
         LOOKUP(prmPrqtyuom,fg-uom-list) EQ 0)  THEN
        RUN sys/ref/convquom.p(prmPrqtyuom,
                               tt-ei.std-uom, v-basis-w,
                               v-len, v-wid, v-dep,
                               v-qty, OUTPUT v-qty).

      v-save-qty = v-qty.
      IF prmJobno NE "" THEN
        RUN po/groupcst.p (prmJobno,
                           INT(prmJob2),
                           prmItem,
                           INT(prmSnum),
                           INT(prmBnum),
                           INPUT-OUTPUT v-qty).

      ASSIGN
       v-save-qty = v-qty - v-save-qty
       v-setup    = 0
       v-pb-qty   = 0.
            
      RUN est/dim-charge.p (tt-eiv.rec_key,
                            v-wid,
                            v-len,
                            INPUT-OUTPUT ld-dim-charge).
     
      DO li = 1 TO EXTENT(tt-eiv.run-qty):
        IF tt-eiv.run-qty[li] LT v-qty THEN NEXT.
        ASSIGN
         v-cost   = (tt-eiv.run-cost[li] + ld-dim-charge) * v-qty
         v-setup  = tt-eiv.setups[li]
         v-pb-qty = tt-eiv.run-qty[li] - v-save-qty.
        IF li LT EXTENT(tt-eiv.run-qty) THEN
          ASSIGN
           v-pb-cst = tt-eiv.run-cost[li + 1] + ld-dim-charge
           v-pb-stp = tt-eiv.setups[li + 1].
        LEAVE.
      END.

      IF poqty-log THEN DO:
        IF v-pb-qty GE 9999999 THEN v-pb-qty = 0.

        IF v-pb-qty EQ 0 THEN v-pb-cst = 0.
        ELSE DO:
          v-pb-qty = v-pb-qty + .001.

          v-pb-cst = v-pb-cst * v-pb-qty.

          IF v-pb-qty NE 0 THEN v-pb-cst = (v-pb-cst /*+ v-pb-stp*/) / v-pb-qty.  
          ELSE v-pb-cst = (v-pb-cst /*+ v-pb-stp*/).
        END.

        IF tt-ei.std-uom NE prmPrqtyuom           AND
           ( /*po-ordl.item-type                                        OR*/
            LOOKUP(tt-ei.std-uom,fg-uom-list)                  EQ 0 OR
            LOOKUP(prmPrqtyuom,fg-uom-list) EQ 0)  THEN
          RUN sys/ref/convquom.p(tt-ei.std-uom,
                                 prmPrqtyuom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-pb-qty, OUTPUT v-pb-qty).

        IF tt-ei.std-uom NE prmPruom           AND
           ( /*po-ordl.item-type                                    OR*/
            LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
            LOOKUP(prmPruom,fg-uom-list) EQ 0)  THEN
          RUN sys/ref/convcuom.p(tt-ei.std-uom,
                                 prmPruom, v-basis-w,
                                 v-len, v-wid, v-dep,
                                 v-pb-cst, OUTPUT v-pb-cst).

        IF prmPruom NE prmConsuom AND
           ( /*po-ordl.item-type                                      OR*/
            LOOKUP(prmPruom,fg-uom-list)   EQ 0 OR
            LOOKUP(prmConsuom,fg-uom-list) EQ 0)     THEN
          RUN sys/ref/convcuom.p(prmPruom,
                                 prmConsuom, v-basis-w,
                                 v-len, v-wid, v-dep,
                                 v-pb-cst, OUTPUT v-pb-cns).

        ttJobhdrLookup.nxtprcqty = IF ROUND(v-pb-qty,2) LE 0 THEN 0 ELSE ROUND(v-pb-qty,2).
      END.

      IF v-qty <> 0 THEN v-cost = (v-cost /*+ v-setup*/) / v-qty.  
      ELSE v-cost = (v-cost /*+ v-setup*/).

      IF ip-calc-cost NE ? THEN DO:
        IF ip-calc-cost THEN DO:            
          IF tt-ei.std-uom NE prmPruom           AND
             ( /*po-ordl.item-type                                    OR*/
              LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
              LOOKUP(prmPruom,fg-uom-list) EQ 0)  THEN
            RUN sys/ref/convcuom.p(tt-ei.std-uom,
                                   prmPruom, v-basis-w,
                                   (IF prmPrqtyuom EQ "ROLL" THEN 12 ELSE v-len),
                                   v-wid, v-dep,
                                   v-cost, OUTPUT v-cost).
          ASSIGN
            ip-calc-cost = YES
            ttJobhdrLookup.cost = ROUND(v-cost,5)
            ttJobhdrLookup.setup = v-setup  .

          

          IF prmPruom NE prmConsuom AND
             ( /*po-ordl.item-type                                      OR*/
              LOOKUP(prmPruom,fg-uom-list)   EQ 0 OR
              LOOKUP(prmConsuom,fg-uom-list) EQ 0)     THEN
            RUN sys/ref/convcuom.p(prmPruom,
                                   prmConsuom, v-basis-w,
                                   (IF prmPrqtyuom EQ "ROLL" THEN 12 ELSE v-len),
                                   v-wid, v-dep,
                                   v-cost, OUTPUT v-cost).

         ttJobhdrLookup.cons-cost = v-cost.     
          
        END.

        ELSE
        IF v-hold-op1 AND po-ord.stat NE "H" THEN DO:
          IF tt-ei.std-uom NE prmPruom           AND
             ( /*po-ordl.item-type                                    OR*/
              LOOKUP(tt-ei.std-uom,fg-uom-list)              EQ 0 OR
              LOOKUP(prmPruom,fg-uom-list) EQ 0)  THEN
            RUN sys/ref/convcuom.p(tt-ei.std-uom,
                                   prmPruom, v-basis-w,
                                   v-len, v-wid, v-dep,
                                   v-cost, OUTPUT v-cost).              
          IF AVAIL job-mat THEN
            RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                           DEC(prmOrdqty),
                           v-cost,
                           DEC(ttJobhdrLookup.cons-cost),
                           OUTPUT v-cost,
                           OUTPUT lv-added-cons-cost,
                           OUTPUT lv-adder-setup).

          IF DEC(ttJobhdrLookup.cost) GT v-cost THEN DO:
            FIND CURRENT po-ord.
            po-ord.stat = "H".
            FIND CURRENT po-ord NO-LOCK.
          END.          
        END.
      END.
    END.

    IF AVAIL job-mat THEN DO:

      IF poqty-log THEN
        RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                       DEC(0),
                       v-pb-cst,
                       v-pb-cns,
                       OUTPUT v-pb-cst,
                       OUTPUT v-pb-cns,
                       OUTPUT lv-adder-setup).

      RUN po-adder2 (RECID(po-ordl), lv-recid, po-ord.vend-no,
                     DEC(prmOrdqty),
                     DEC(ttJobhdrLookup.cost),
                     DEC(ttJobhdrLookup.cons-cost),
                     OUTPUT lv-added-cost,
                     OUTPUT lv-added-cons-cost,
                     OUTPUT lv-adder-setup).

      IF ip-calc-cost THEN
        ASSIGN
         ttJobhdrLookup.cost = ROUND(lv-added-cost,5)
         ttJobhdrLookup.cons-cost = (lv-added-cons-cost).

      
    END.

    IF poqty-log THEN DO:
      IF CAN-DO("L,LOT",prmPruom) THEN
        lv-t-cost = (v-pb-cst + v-pb-stp) *
                    IF decimal(ttJobhdrLookup.ord-qty) LT 0 THEN -1 ELSE 1.

      ELSE DO:
        v-ord-qty = DEC(ttJobhdrLookup.nxtprcqty).

        IF prmPrqtyuom NE prmPruom AND
           (po-ordl.item-type                                        OR
            LOOKUP(prmPrqtyuom,fg-uom-list) EQ 0 OR
            LOOKUP(prmPruom,fg-uom-list)     EQ 0)     THEN
   
          RUN sys/ref/convquom.p(prmPrqtyuom,
                                 prmPruom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-ord-qty, OUTPUT v-ord-qty).
     
        lv-t-cost = (v-ord-qty * v-pb-cst) + v-pb-stp.
      END.
      

      IF DEC(prmDis) NE 0 THEN
        lv-t-cost = lv-t-cost * (1 - (DEC(prmDis) / 100)).

      ttJobhdrLookup.nxtprcst = lv-t-cost.

      IF ttJobhdrLookup.nxtprcst LE 0 THEN ttJobhdrLookup.nxtprcst = 0.
    END.

    IF ip-calc-cost NE ? THEN DO:
      IF CAN-DO("L,LOT",prmPruom) THEN
        lv-t-cost = (DEC(ttJobhdrLookup.cost) +
                     DEC(ttJobhdrLookup.setup)) *
                    IF decimal(ttJobhdrLookup.ord-qty) LT 0 THEN -1 ELSE 1.

      ELSE DO:
        v-ord-qty = DEC(prmOrdqty).

        IF prmPrqtyuom NE prmPruom AND
           (po-ordl.item-type                                        OR
            LOOKUP(prmPrqtyuom,fg-uom-list) EQ 0 OR
            LOOKUP(prmPruom,fg-uom-list)     EQ 0)     THEN
   
          RUN sys/ref/convquom.p(prmPrqtyuom,
                                 prmPruom,
                                 v-basis-w, v-len, v-wid, v-dep,
                                 v-ord-qty, OUTPUT v-ord-qty).
     
        lv-t-cost = (v-ord-qty * DEC(ttJobhdrLookup.cost)) +
                    DEC(ttJobhdrLookup.setup).
      END.

      IF DEC(prmDis) NE 0 THEN
         lv-t-cost = lv-t-cost * (1 - (DEC(prmDis) / 100)).
      totcost = (lv-t-cost).
    END.
  /*END.  */

END PROCEDURE.

PROCEDURE set-dims :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  
  /*  ASSIGN
     v-len = DEC(prmSlen)
     v-wid = DEC(prmSwid)
     {po/calc10.i v-len}
     {po/calc10.i v-wid}.*/

    FIND FIRST ITEM
        WHERE item.company EQ cocode
          AND item.i-no    EQ prmItem
        NO-LOCK NO-ERROR.

    ASSIGN
      v-basis-w = IF AVAIL ITEM THEN item.basis-w ELSE 0
      v-dep     = IF AVAIL ITEM THEN item.s-dep ELSE 0.
 

END PROCEDURE.



PROCEDURE po-adder2 :
/*------------------------------------------------------------------------------
  Purpose:     
  PARAMs:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-recid  as recid.
DEF INPUT PARAM ip-recid1 as recid.
DEF INPUT PARAM ip-vend-no LIKE po-ord.vend-no NO-UNDO.
DEF INPUT PARAM ip-qty as DEC NO-UNDO.
DEF INPUT PARAM ip-cost as DEC NO-UNDO.
DEF INPUT PARAM ip-cons-cost as DEC NO-UNDO.

DEF OUTPUT PARAM op-cost AS DEC NO-UNDO.
DEF OUTPUT PARAM op-cons-cost AS DEC NO-UNDO.
DEF OUTPUT PARAM op-adder-setup AS DEC NO-UNDO.

def var v-tot-cost as dec no-undo.
def var v-cost     as dec no-undo.
def var v-add-cost as dec no-undo.
def var v-qty-comp as dec no-undo.
def var v-setup like e-item-vend.setup no-undo.
def var v-adder as dec extent 2 NO-UNDO.
DEF VAR v-index AS INT NO-UNDO.

def buffer xjob-mat for job-mat.

find xjob-mat where recid(xjob-mat) eq ip-recid1 no-lock.

assign
   addersText = ''
   op-cost = ip-cost
   op-cons-cost = ip-cons-cost.


   FIND first item where 
        item.company  eq job-mat.company AND
        item.i-no     eq prmItem
        NO-LOCK NO-ERROR.

   IF AVAIL ITEM AND
      ITEM.mat-type NE "B" THEN
      LEAVE.

   ASSIGN
      v-adder[1] = ip-cost
      v-adder[2] = ip-cons-cost.

  IF prmPruom EQ "EA"                    OR
     (NOT po-ordl.item-type AND
      LOOKUP(prmPruom,fg-uom-list) EQ 0) THEN
     v-tot-cost = ip-cost.

  ELSE
    RUN sys/ref/convcuom.p(prmPruom, "EA",
                           v-basis-w, v-len, v-wid, v-dep,
                           ip-cost, OUTPUT v-tot-cost).

 
  for each job-mat no-lock
      where job-mat.company  eq xjob-mat.company
        and job-mat.job      eq xjob-mat.job
        and job-mat.frm      eq xjob-mat.frm
        and job-mat.job-no   eq xjob-mat.job-no
        and job-mat.job-no2  eq xjob-mat.job-no2
      use-index seq-idx,

      first item no-lock
      where item.company  eq job-mat.company
        and item.i-no     eq job-mat.i-no
        and item.mat-type eq "A":

    find first e-item no-lock
        where e-item.company eq cocode
          and e-item.i-no    eq prmItem
        no-error.
    
    find first e-item-vend no-lock
        where e-item-vend.company eq item.company
          and e-item-vend.i-no    eq item.i-no
          and e-item-vend.vend-no eq ip-vend-no
        no-error.

    if avail e-item and avail e-item-vend AND ip-vend-no NE "" then do:
      if prmPrqtyuom eq e-item.std-uom then
           v-qty-comp = ip-qty.
      else
        run sys/ref/convquom.p(prmPrqtyuom, e-item.std-uom,
                               v-basis-w, v-len, v-wid, v-dep,
                               ip-qty, output v-qty-comp).
        

      v-setup = 0.

      EMPTY TEMP-TABLE tt-eiv-2.
      CREATE tt-eiv-2.

      DO v-index = 1 TO 10:
         ASSIGN
            tt-eiv-2.run-qty[v-index] = e-item-vend.run-qty[v-index]
            tt-eiv-2.run-cost[v-index] = e-item-vend.run-cost[v-index]
            tt-eiv-2.setups[v-index] = e-item-vend.setups[v-index].
      END.

      FIND FIRST b-qty WHERE
           b-qty.reftable = "vend-qty" AND
           b-qty.company = e-item-vend.company AND
               b-qty.CODE    = e-item-vend.i-no AND
           b-qty.code2   = e-item-vend.vend-no
           NO-LOCK NO-ERROR.
      
      IF AVAIL b-qty THEN
      DO:
         FIND FIRST b-cost WHERE
              b-cost.reftable = "vend-cost" AND
              b-cost.company = e-item-vend.company AND
                  b-cost.CODE    = e-item-vend.i-no AND
              b-cost.code2   = e-item-vend.vend-no
              NO-LOCK NO-ERROR.

         FIND FIRST b-setup WHERE
              b-setup.reftable = "vend-setup" AND
              b-setup.company = e-item-vend.company AND
                  b-setup.CODE    = e-item-vend.i-no AND
              b-setup.code2   = e-item-vend.vend-no
              NO-LOCK NO-ERROR.
   
         DO v-index = 1 TO 10:
            ASSIGN
               tt-eiv-2.run-qty[v-index + 10] = b-qty.val[v-index]
               tt-eiv-2.run-cost[v-index + 10] = b-cost.val[v-index]
               tt-eiv-2.setups[v-index + 10] = b-setup.val[v-index].
         END.
      END.
DEF VAR i AS INT NO-UNDO.
      do i = 1 to EXTENT(tt-eiv-2.run-qty):
         if v-qty-comp le tt-eiv-2.run-qty[i] then
            leave.
      end.
    /*  if i eq 1 then v-setup = e-item-vend.setup. */
      IF i GT EXTENT(tt-eiv-2.run-qty) THEN i = EXTENT(tt-eiv-2.run-qty).
      ASSIGN
        v-setup = tt-eiv-2.setups[i]
        op-adder-setup = op-adder-setup + v-setup
        v-cost = ((tt-eiv-2.run-cost[i] * v-qty-comp) + v-setup) / v-qty-comp.
      /* This adds the Adder cost in */
      IF e-item.std-uom NE prmPruom THEN
        RUN sys/ref/convcuom.p(e-item.std-uom, prmPruom, job-mat.basis-w,
                               job-mat.len, job-mat.wid, item.s-dep,
                               v-cost, OUTPUT v-cost).
    END.

    ELSE DO:
      v-cost = job-mat.std-cost.
      
      IF job-mat.sc-uom NE prmPruom THEN
        RUN sys/ref/convcuom.p(job-mat.sc-uom, prmPruom, job-mat.basis-w,
                               job-mat.len, job-mat.wid, item.s-dep,
                               job-mat.std-cost, OUTPUT v-cost).
    END.
    IF v-cost = ? THEN v-cost = 0.
    ASSIGN
     addersText = addersText + SUBSTR(item.i-name,1,18) +
                  FILL(' ',19 - LENGTH(SUBSTR(item.i-name,1,18))) +
                  STRING(v-cost,'-z,zz9.9999') + CHR(10)
     v-add-cost = v-add-cost + v-cost.

    /* gdm - */     
    /*IF v-cost NE 0                         
      THEN RUN po-adder3 (INPUT v-cost).   */
    /* gdm - end */                                
  END.

  IF prmPruom NE "EA" THEN 
    RUN sys/ref/convcuom.p("EA", prmPruom,
                           v-basis-w, v-len, v-wid, v-dep,
                           v-tot-cost, OUTPUT v-tot-cost).
 
  op-cost = v-add-cost + v-tot-cost.

  IF prmPruom NE prmConsuom THEN
    RUN sys/ref/convcuom.p(prmPruom, prmConsuom,
                           v-basis-w, v-len, v-wid, v-dep,
                           ip-cost, OUTPUT op-cons-cost).

/*  display po-ordl.cost po-ordl.cons-cost.  */

assign
 v-adder[1] = op-cost      - v-adder[1]
 v-adder[2] = op-cons-cost - v-adder[2].
/*END.*/

END PROCEDURE.





