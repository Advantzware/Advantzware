/* ---------------------------------------------------- po/do-po.p  04/99 FWK */
/* Add PO from Order Entry Program - P/O Module
                               
   Similar logic in do-po-best.p                                              */
/* -------------------------------------------------------------------------- */
/* This is a placeholder for compatability with new version */
DEF INPUT PARAMETER iplPromptForRM AS LOG NO-UNDO.
{sys/inc/var.i shared}

def new shared var v-basis-w like item.basis-w no-undo.
def new shared var v-len like item.s-len no-undo.
def new shared var v-wid like item.s-wid no-undo.
def new shared var v-dep like item.s-dep no-undo.
def new shared var v-gl-desc as char format "x(30)" no-undo.
def new shared var v-tot-msf as dec format ">>,>>9.999" init 0 no-undo.
def new shared var v-adder as dec extent 2.
def new shared var head as ch format "x(80)" extent 2.
def new shared var v-unline as char format "x(78)".
def new shared var v-part-dscr1 as char format "x(30)".
def new shared var v-part-dscr2 as char format "x(30)".
def new shared var v-report-cost as dec init 0 format ">>,>>9.9999" no-undo.
def new shared var v-cont-upd as log init yes no-undo.


DEF BUFFER b-po-ordl FOR po-ordl.
DEF BUFFER bf-itemfg FOR itemfg.

/* Program is based on this variable */
DEF shared var fil_id as recid no-undo.
DEF VAR cFilIdSource AS CHAR NO-UNDO.
DEF VAR lv-recid AS RECID NO-UNDO.
def var nufile as log no-undo.
def var sel as int no-undo.
def var call_id as recid no-undo.
def var choice as log no-undo.
DEF VAR v-tot-cost AS DEC DECIMALS 4 NO-UNDO.
DEF VAR v-access-close AS LOG.
DEF VAR v-access-list AS CHAR.

def var v-old-i-no like po-ordl.i-no no-undo.
def var save_id as recid.
def var v-op-type as log no-undo.
def var v-hld-cost as dec format "->>>>>>>9.99<<".
def var v-hld-line-qty as dec format "->>>>>>>>9.99<<".
def var v-uom as char format "x(4)".
def var v-ord-qty like po-ordl.ord-qty.
def var v-uom-help as char.
def var count-mat as int init 0 no-undo.
def var v-frm like job-mat.frm no-undo.
def var v-blk like job-mat.blank-no no-undo.
def var v-lastkey as int no-undo.
def var v_item_lu as log no-undo.
def var v-qty as dec.
def var v-cost as dec.
def var num-job-mats as int init 0 no-undo.
def var v-job-no like po-ordl.job-no no-undo.
def var v-job-no2 like po-ordl.job-no2 no-undo.
def var v-new-board as log init no.
def var item-recid as recid no-undo.
def var jm-recid as recid no-undo.
def var v-exist-item as log init yes no-undo.
def var v-uom-list  as char no-undo init "C,CS,EA,L,LB,LF,LOT,M,MSF,SHT,TON,BF".
def var pr-uom-list as char no-undo init "EA,LB,M,MSF,TON,BF".
def var cons-uom-list  as char no-undo init "M,LF,EA,LB,TON".
def var fg-uom-list  as char NO-UNDO.
def var v-item-cost as dec format ">>,>>9.9999" init 0 no-undo.
def var v-setup-cost as dec format ">>,>>9.9999" init 0 no-undo.
def var v-vend-no like vend.vend-no init "" no-undo.
DEF VAR v-vend-item LIKE ITEM.vend-item NO-UNDO.
def var v-setup like e-item-vend.setup no-undo.
def var v-recid as recid.
DEF VAR v-autopo-sec AS LOG NO-UNDO.
DEF VAR v-autofg-sec AS LOG NO-UNDO.
DEF VAR v-autoprep-sec AS LOG NO-UNDO.
DEF VAR nk1-oeautopo-int AS INT NO-UNDO.
def var v-ord-no like oe-ordl.ord-no no-undo.
DEF VAR li AS INT NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR v-hold AS LOG NO-UNDO.
DEF VAR v-vendor-chosen-report AS RECID NO-UNDO.
DEF VAR v-sel-uom AS CHAR NO-UNDO.
DEF VAR rOrderPoRow AS ROWID NO-UNDO.
DEF VAR po-found AS LOG NO-UNDO.
def new shared var v-new-i-no like item.i-no init "" no-undo.
def new shared var v-new-wid like item.s-wid init 0 no-undo.
def new shared var v-new-len like item.s-len init 0 no-undo.

{ce/msfcalc.i}
{sa/sa-sls01.i}

def new shared var vmatch as ch.
def new shared var v-open as log init yes.
def new shared var v-findlines as log init yes.
def new shared var v-tot-ord as dec format "->>>,>>>,>>9.99".
def new shared var v-neword as log no-undo.
def new shared var v-abortord as log format "Yes/No".
def new shared var save_hdr as recid no-undo.
def new shared var v-sname like shipto.ship-name.
def new shared var v-saddr like shipto.ship-addr.
def new shared var v-sadd1 as char format "x(30)" no-undo.
def new shared var v-sadd2 as char format "x(30)" no-undo.
def new shared var v-scity like shipto.ship-city.
def new shared var v-sstate like shipto.ship-state.
def new shared var v-szip like shipto.ship-zip.
def new shared var errormsg as char format "x(80)".
def new shared var v-pocost1 as char.
def new shared var v-hold-op1 as log.
def new shared var v-all-msf as dec format ">>,>>9.999" init 0 no-undo.
def new shared var v-po-qty as log init true no-undo.

def var v-exp-limit as int no-undo init 10.
def var v-hld-sel as int.
def var tmp-recid as recid no-undo.
def var v-multijob like sys-ctrl.log-fld init no no-undo.
def var v-uom-comp like po-ordl.pr-qty-uom no-undo.
def var v-qty-comp like job-mat.qty no-undo.
def var v-qty-comp1 like job-mat.qty no-undo.
def var v-po-date as date no-undo.
def var v-due-date as date no-undo.
def var v-format as char format "x(8)" no-undo.
def var v-new-avail as log init no no-undo.
def var v-job as char no-undo.
DEF VAR v-job-mat-qty LIKE job-mat.qty NO-UNDO.
DEF VAR v-job-mat-uom LIKE job-mat.qty-uom NO-UNDO.
DEF VAR ld-line-qty LIKE po-ordl.ord-qty NO-UNDO.
DEF VAR ld-part-qty AS DEC NO-UNDO.
DEF VAR ll-drop AS LOG NO-UNDO.
DEF VAR ls-drop-custno AS cha NO-UNDO.
DEF VAR ls-ship-choice AS cha NO-UNDO.
DEF VAR ll-canceled AS LOG NO-UNDO.
DEF VAR ld-dim-charge AS DEC NO-UNDO.
DEF VAR oeautoprep-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR v-job-recid AS RECID NO-UNDO.
DEF VAR v-actnum AS CHAR NO-UNDO.
DEF VAR v-charge AS CHAR NO-UNDO.
DEF VAR v-index AS INT NO-UNDO.

def buffer tmp-po-ord for po-ord.
def buffer xjob-mat for job-mat.
def buffer bpo-ordl for po-ordl.
def buffer b-item   for item.
def buffer b-oe-ordl  for oe-ordl.
def buffer bf-ordl for oe-ordl.
def buffer bf-ord for oe-ord.
DEF BUFFER b-orderpo FOR reftable.
DEF BUFFER b-jc-calc FOR reftable.
def buffer b-ref1 for reftable.
def buffer b-ref2 for reftable.
DEF BUFFER b-job-mat FOR job-mat.
DEF BUFFER b-job-hdr FOR job-hdr.
DEF BUFFER xest FOR est.
DEF BUFFER xeb FOR eb.

DEF TEMP-TABLE tt-ref1 NO-UNDO LIKE reftable.
DEF BUFFER tt-ref2 FOR tt-ref1.
DEF VAR v-po-best AS LOG NO-UNDO.
DEF VAR v-from-po-entry AS LOG NO-UNDO.

def new shared workfile work-vend no-undo
    field cost as dec format ">>,>>9.9999"
    field v-cost-num as int 
    field v-recid as recid.

DEF TEMP-TABLE w-job-mat NO-UNDO LIKE job-mat
    FIELD w-rowid AS ROWID
    FIELD w-recid AS RECID
    FIELD this-is-a-rm AS LOG
    FIELD isaset AS LOG
    FIELD isacomponent AS LOG
    FIELD fg-i-no LIKE job-hdr.i-no
    FIELD est-no LIKE eb.est-no
    FIELD eqty LIKE eb.eqty
    FIELD prep AS LOG.

DEF TEMP-TABLE tt-itemfg NO-UNDO FIELD isaset LIKE itemfg.isaset
                                 FIELD isacomponent AS LOG 
                                 FIELD pur-man LIKE itemfg.pur-man
                                 FIELD form-no LIKE eb.form-no
                                 FIELD blank-no LIKE eb.blank-no
                                 FIELD qty LIKE oe-ordl.qty                                 
                                 FIELD pur-uom LIKE itemfg.pur-uom
                                 FIELD row-id AS ROWID.

DEF BUFFER b-tt-itemfg FOR tt-itemfg.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD company AS CHAR
    FIELD i-no AS CHAR
    FIELD std-uom AS CHAR
    INDEX i-no company i-no.

DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD company AS CHAR
    FIELD vend-no AS CHAR
    FIELD i-no AS CHAR
    FIELD vend-i-no AS CHAR
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20
    FIELD roll-w AS DEC DECIMALS 4 EXTENT 30
    FIELD est-no AS CHAR
    FIELD form-no AS INT
    FIELD blank-no AS INT
    FIELD item-type AS LOG
    FIELD rec_key AS CHAR
    FIELD rec-id AS RECID
    FIELD std-uom AS CHAR
    INDEX i-no company item-type i-no vend-no
    INDEX vend-no company i-no vend-no.

IF INDEX(PROGRAM-NAME(2),"add-po-best") GT 0 THEN
   v-po-best = YES.

IF INDEX(PROGRAM-NAME(2),"w-purord") GT 0
   OR INDEX(PROGRAM-NAME(3),"w-purord") GT 0
   OR INDEX(PROGRAM-NAME(4),"w-purord") GT 0 THEN
   v-from-po-entry = TRUE.
{fg/fullset.i NEW}

{sys/ref/pocost.i}
assign
 v-pocost1  = v-pocost
 v-hold-op1 = v-hold-op
 lv-recid   = fil_id.

DO TRANSACTION:

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name eq "POQTY"
      no-lock no-error.
  if not avail sys-ctrl then DO:  
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "POQTY"
     sys-ctrl.descrip  = "Auto PO QTY to Use Job QTY or Net Sheets?"
     sys-ctrl.char-fld = "JobQty"
     sys-ctrl.log-fld  = no.
     
    run po/d-poqty.w (output sys-ctrl.char-fld).       
  END.
  v-po-qty = if sys-ctrl.char-fld eq "Net Shts" then false else true.
END.

DO TRANSACTION:
  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name eq "OEAUTOPO"
      no-lock no-error.
  if not avail sys-ctrl then DO:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "OEAUTOPO"
     sys-ctrl.descrip  = "Auto/Manual PO Creation from Order Entry? Multiple Jobs per PO?"
     sys-ctrl.char-fld = "Manual"
     sys-ctrl.log-fld  = no.
     run po/d-oepo.w (output sys-ctrl.char-fld).
     MESSAGE "Create PO with Multiple Jobs?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE sys-ctrl.log-fld.
  END.
  assign
   v-format   = sys-ctrl.char-fld
   v-multijob = sys-ctrl.log-fld
   nk1-oeautopo-int = sys-ctrl.int-fld.
END.


/* Check if authorized to create PO's */
RUN methods/prgsecur.p
    (INPUT "OEAutoPO",
     INPUT "ALL",
     INPUT NO,
     INPUT NO,
     INPUT NO,
     OUTPUT v-autopo-sec,
     OUTPUT v-access-close,
     OUTPUT v-access-list).
/* Security check only for order entry */
IF v-from-po-entry THEN
    v-autopo-sec = TRUE.

DO TRANSACTION:

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "OEAUTOPREP"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "OEAUTOPREP"
     sys-ctrl.log-fld  = NO
     sys-ctrl.descrip  = "Auto Prep PO Creation from Order Entry?".
  END.
  
  oeautoprep-log = sys-ctrl.log-fld.
END.

/* Check if authorized to create PO's */
IF oeautoprep-log THEN
RUN methods/prgsecur.p
    (INPUT "OEAutoPrep", /* program */
     INPUT "ALL",        /*Basis */
     INPUT NO,
     INPUT NO,
     INPUT NO,
     OUTPUT v-autoprep-sec,
     OUTPUT v-access-close,
     OUTPUT v-access-list).

/* Security only for entry from order entry */
IF v-from-po-entry THEN
    v-autoprep-sec = TRUE.
{sys/inc/ap-gl#.i}

DO TRANSACTION:
  {sys/inc/oeautofg.i}
  {sys/inc/pouom.i}
  {sys/inc/aptax.i}
END.

/* Check if authorized to create PO's */
IF oeautofg-log THEN
RUN methods/prgsecur.p
    (INPUT "OEAutoFG", /* Program master program name */
     INPUT "ALL",      /* Security based on */
     INPUT NO,
     INPUT NO,
     INPUT NO,
     OUTPUT v-autofg-sec,
     OUTPUT v-access-close,
     OUTPUT v-access-list).
/* only check security from order entry */
IF v-from-po-entry THEN 
    v-autofg-sec = TRUE.
FIND FIRST company NO-LOCK WHERE company.company EQ cocode NO-ERROR.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

/* End definitions */


FIND bf-ordl WHERE RECID(bf-ordl) EQ fil_id NO-LOCK NO-ERROR.

/* wfk - Build tt-itemfg based on job */
IF AVAIL bf-ordl THEN DO:
  /* bf-itemfg holds buffer for item of bf-ordl since itemfg is used for set components below */
  cFilIdSource = "oe-ordl".
  FIND FIRST bf-itemfg
    WHERE bf-itemfg.company EQ bf-ordl.company
      AND bf-itemfg.i-no    EQ bf-ordl.i-no
    NO-LOCK NO-ERROR.
  v-ord-no = bf-ordl.ord-no.

  FIND FIRST bf-ord
      WHERE bf-ord.company EQ bf-ordl.company
        AND bf-ord.ord-no  EQ bf-ordl.ord-no
        AND bf-ord.opened  EQ YES
        AND bf-ord.stat    NE "H"
      NO-LOCK NO-ERROR.

  IF AVAIL bf-ord THEN DO:
     IF TRIM(bf-ordl.job-no) NE "" THEN
        FIND FIRST job
             WHERE job.company EQ cocode
               AND job.job-no  EQ bf-ordl.job-no
               AND job.job-no2 EQ bf-ordl.job-no2
             NO-LOCK NO-ERROR.

      
     IF NOT AVAIL job OR NOT po-found THEN DO:
        FIND FIRST itemfg
            WHERE itemfg.company EQ bf-ordl.company
              AND itemfg.i-no    EQ bf-ordl.i-no
            NO-LOCK NO-ERROR.
        IF AVAIL itemfg THEN DO:
          IF itemfg.isaset THEN DO:
            RUN fg/fullset.p (ROWID(itemfg)).
       
            RELEASE itemfg.
       
            FOR EACH tt-fg-set,
                FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ bf-ordl.company
                  AND itemfg.i-no    EQ tt-fg-set.part-no:
              CREATE tt-itemfg.
              BUFFER-COPY itemfg EXCEPT rec_key TO tt-itemfg
              ASSIGN
               tt-itemfg.isacomponent = TRUE
               tt-itemfg.form-no  = 0
               tt-itemfg.blank-no = 0
               tt-itemfg.qty      = bf-ordl.qty * tt-fg-set.part-qty-dec
               tt-itemfg.pur-uom  = "EA"
               tt-itemfg.row-id   = ROWID(itemfg).
              
              /* WFK - 06051407 */
               FIND FIRST eb WHERE
                    eb.company EQ bf-ordl.company AND
                    eb.est-no EQ bf-ordl.est-no AND
                    eb.stock-no EQ tt-fg-set.part-no AND
                    eb.blank-no EQ tt-itemfg.blank-no AND
                    eb.form-no EQ tt-itemfg.form-no
                    NO-LOCK NO-ERROR.

               IF AVAIL eb THEN
                 tt-itemfg.pur-man = eb.pur-man.
            END.
          END.
       
          ELSE DO:
            CREATE tt-itemfg.
            BUFFER-COPY itemfg EXCEPT rec_key TO tt-itemfg
            ASSIGN
             tt-itemfg.form-no  = bf-ordl.form-no
             tt-itemfg.blank-no = bf-ordl.blank-no
             tt-itemfg.qty      = bf-ordl.qty
             tt-itemfg.pur-uom  = "EA"
             tt-itemfg.row-id   = ROWID(itemfg).
               FIND FIRST eb WHERE
                    eb.company EQ bf-ordl.company AND
                    eb.est-no EQ bf-ordl.est-no AND
                    eb.stock-no EQ bf-ordl.i-no AND
                    eb.blank-no EQ bf-ordl.blank-no AND
                    eb.form-no EQ bf-ordl.form-no
                    NO-LOCK NO-ERROR.

               IF AVAIL eb THEN
                 tt-itemfg.pur-man = eb.pur-man.
          END.
        END.
     END.
  END.
END.

ELSE DO:
  FIND job WHERE RECID(job) EQ fil_id NO-LOCK NO-ERROR.
  cFilIdSource = "JOB".
  FOR EACH job-hdr NO-LOCK
      WHERE job-hdr.company EQ job.company
        AND job-hdr.job     EQ job.job
        AND job-hdr.job-no  EQ job.job-no
        AND job-hdr.job-no2 EQ job.job-no2
        AND job-hdr.ord-no  NE 0:
    FIND FIRST bf-ord
        WHERE bf-ord.company EQ cocode
          AND bf-ord.ord-no  EQ job-hdr.ord-no
          AND bf-ord.opened  EQ YES
          AND bf-ord.stat    NE "H"
        NO-LOCK NO-ERROR.
    IF NOT AVAIL bf-ord THEN DO:
      RELEASE job.
      LEAVE.
    END.

    FIND FIRST bf-ordl
        WHERE bf-ordl.company EQ job-hdr.company
          AND bf-ordl.ord-no  EQ job-hdr.ord-no
          AND bf-ordl.i-no    EQ job-hdr.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL bf-ordl THEN LEAVE.
  END.
END.






/* Check that cFilIDSource is JOB since job record may be available here    */
/* even though the fil_id is related to oe-ordl and could cause a duplicate */
/* tt-itemfg                                                                */
IF AVAIL job AND cFilIdSource = "JOB" THEN DO:
   FOR EACH job-hdr NO-LOCK
       WHERE job-hdr.company EQ job.company
         AND job-hdr.job     EQ job.job
         AND job-hdr.job-no  EQ job.job-no
         AND job-hdr.job-no2 EQ job.job-no2,
       FIRST itemfg NO-LOCK
       WHERE itemfg.company EQ job-hdr.company
         AND itemfg.i-no    EQ job-hdr.i-no:
     
     FIND FIRST eb WHERE
          eb.company EQ job-hdr.company AND
          eb.est-no EQ job-hdr.est-no AND
          eb.stock-no EQ job-hdr.i-no AND
          eb.blank-no EQ job-hdr.blank-no AND
          eb.form-no EQ job-hdr.frm
          NO-LOCK NO-ERROR.

     CREATE tt-itemfg.
     BUFFER-COPY itemfg EXCEPT rec_key pur-man TO tt-itemfg
     ASSIGN
      tt-itemfg.form-no  = job-hdr.frm
      tt-itemfg.blank-no = job-hdr.blank-no
      tt-itemfg.qty      = IF AVAIL bf-ordl THEN bf-ordl.qty ELSE job-hdr.qty
      tt-itemfg.row-id   = ROWID(itemfg). 
      tt-itemfg.pur-man = IF AVAIL eb THEN eb.pur-man ELSE itemfg.pur-man.
   END.
  
   FIND b-tt-itemfg NO-ERROR.
  
   IF AVAIL b-tt-itemfg THEN DO:
      IF b-tt-itemfg.isaset THEN DO:
         b-tt-itemfg.pur-man = NO.
        
         FOR EACH b-jc-calc NO-LOCK
             WHERE b-jc-calc.reftable EQ "jc/jc-calc.p"
               AND b-jc-calc.company  EQ job.company
               AND b-jc-calc.loc      EQ ""
               AND b-jc-calc.code     EQ STRING(job.job,"999999999"),
             FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ b-jc-calc.company
               AND itemfg.i-no    EQ b-jc-calc.code2
               AND itemfg.pur-man:
        
           CREATE tt-itemfg.
           BUFFER-COPY itemfg EXCEPT rec_key TO tt-itemfg
           ASSIGN
            tt-itemfg.form-no  = b-jc-calc.val[12]
            tt-itemfg.blank-no = b-jc-calc.val[13]
            tt-itemfg.row-id   = ROWID(itemfg). 
           IF AVAIL bf-ordl THEN
             FIND FIRST bf-itemfg
                WHERE bf-itemfg.company EQ bf-ordl.company
                  AND bf-itemfg.i-no    EQ bf-ordl.i-no
                NO-LOCK NO-ERROR.
           IF AVAIL bf-itemfg THEN
           RUN fg/fullset.p (ROWID(bf-itemfg)).
        
           FOR EACH tt-fg-set WHERE tt-fg-set.part-no EQ b-jc-calc.code2:
               tt-itemfg.qty = tt-itemfg.qty + (b-tt-itemfg.qty * tt-fg-set.part-qty-dec).
           END.
         END.
      END.
     
      IF b-tt-itemfg.pur-man EQ NO THEN DELETE b-tt-itemfg.
   END.
END.




/* wfk - Create w-job-mat based on tt-itemfg created above */
/*       w-job-mat handling both fg and rm                 */
IF oeautofg-log AND v-autofg-sec AND
   AVAIL bf-ord  AND
   AVAIL bf-ordl THEN
  FOR EACH tt-itemfg WHERE tt-itemfg.pur-man,
      FIRST itemfg WHERE ROWID(itemfg) EQ tt-itemfg.row-id NO-LOCK:
  
    IF ((oeautofg-chr EQ "NonStock" OR oeautofg-chr EQ "Any") AND
        NOT itemfg.stocked)                                         OR
       ((oeautofg-chr EQ "LotCntrl" OR oeautofg-chr EQ "Any") AND
        NOT itemfg.ord-policy)                                      OR
       ((oeautofg-chr EQ "Avail<0"  OR oeautofg-chr EQ "Any") AND
        itemfg.q-avail LT 0)                                        THEN DO:
      CREATE w-job-mat.
      ASSIGN
       w-job-mat.w-recid      = ?
       w-job-mat.rm-i-no      = itemfg.i-no
       w-job-mat.i-no         = itemfg.i-no
       w-job-mat.fg-i-no      = itemfg.i-no
       w-job-mat.est-no       = bf-ordl.est-no
       w-job-mat.frm          = tt-itemfg.form-no
       w-job-mat.blank-no     = tt-itemfg.blank-no
       w-job-mat.isaset       = itemfg.isaset
       w-job-mat.isacomponent = tt-itemfg.isacomponent
       w-job-mat.this-is-a-rm = NO
       w-job-mat.wid          = itemfg.t-wid
       w-job-mat.len          = itemfg.t-len
       w-job-mat.dep          = 0
       w-job-mat.basis-w      = itemfg.t-wid * itemfg.t-len * 100
       w-job-mat.basis-w      = itemfg.weight-100 /
                                (if v-corr then (w-job-mat.basis-w * .007)
                                           else (w-job-mat.basis-w / 144) /
                                 1000)
       w-job-mat.qty-uom      = "EA"
       w-job-mat.sc-uom       = itemfg.pur-uom
       w-job-mat.qty          = tt-itemfg.qty.
    END. /* Do create of w-job-mat */
  END. /* each tt-itemfg */
 





/* Create w-job-mat from job-mat */
IF AVAIL job AND ((v-format NE "Manual" AND v-autopo-sec AND v-po-best EQ NO) OR v-po-best) THEN
  FOR EACH job-mat
      WHERE job-mat.company EQ job.company
        AND job-mat.job     EQ job.job
        AND job-mat.job-no  EQ job.job-no
        AND job-mat.job-no2 EQ job.job-no2
        AND NOT CAN-FIND(FIRST tt-itemfg
                         WHERE tt-itemfg.form-no EQ job-mat.frm
                           AND tt-itemfg.pur-man EQ YES
                           AND tt-itemfg.isaset EQ NO)
      NO-LOCK,
  
      FIRST item
      WHERE item.company EQ job-mat.company
        AND item.i-no    EQ job-mat.rm-i-no
      NO-LOCK:
  
    IF v-po-best = NO AND NOT CAN-DO("1,2,3,4,B,P,R",item.mat-type) THEN NEXT.
    ELSE IF v-po-best AND ITEM.mat-type NE "B" THEN NEXT.
  
    FIND FIRST b-jc-calc NO-LOCK
        WHERE b-jc-calc.reftable EQ "jc/jc-calc.p"
          AND b-jc-calc.company  EQ job.company
          AND b-jc-calc.loc      EQ ""
          AND b-jc-calc.code     EQ STRING(job.job,"999999999")
          AND b-jc-calc.val[12]  EQ job-mat.frm
          AND (b-jc-calc.val[13] EQ job-mat.blank-no OR job-mat.blank-no EQ 0)
          AND b-jc-calc.code2    NE bf-ordl.i-no
        NO-ERROR.
  
    CREATE w-job-mat.
    BUFFER-COPY job-mat TO w-job-mat
    ASSIGN
     w-job-mat.w-rowid      = ROWID(job-mat)
     w-job-mat.w-recid      = RECID(job-mat)
     w-job-mat.this-is-a-rm = YES
     w-job-mat.dep          = item.s-dep
     w-job-mat.basis-w      = item.basis-w
     w-job-mat.fg-i-no      = IF AVAIL b-jc-calc THEN b-jc-calc.code2
                                                 ELSE bf-ordl.i-no.
  END. /* each job-mat */

IF AVAIL job AND oeautoprep-log AND v-autoprep-sec AND
   AVAIL bf-ordl /*AND bf-ordl.type-code EQ "N"*/ AND
   v-po-best EQ NO THEN
   DO:
 


   /* Create w-job-mat  from job-prep */
   FOR EACH job-prep WHERE
       job-prep.company EQ job.company AND
       job-prep.job     EQ job.job AND
       job-prep.job-no  EQ job.job-no AND
       job-prep.job-no2 EQ job.job-no2 AND
       NOT CAN-FIND(FIRST tt-itemfg WHERE
                          tt-itemfg.form-no EQ job-prep.frm AND
                          tt-itemfg.pur-man EQ YES AND
                          tt-itemfg.isaset EQ NO)
      NO-LOCK,
      FIRST prep FIELDS(i-no number-up) WHERE
            prep.company EQ job-prep.company AND
            prep.CODE EQ job-prep.CODE AND
            prep.i-no NE ""
            NO-LOCK,
      FIRST ITEM WHERE
            item.company EQ job-prep.company AND
            item.i-no    EQ prep.i-no
            NO-LOCK:
   
      FIND FIRST b-jc-calc WHERE
            b-jc-calc.reftable EQ "jc/jc-calc.p" AND
            b-jc-calc.company  EQ job.company AND
            b-jc-calc.loc      EQ "" AND
            b-jc-calc.code     EQ STRING(job.job,"999999999") AND
            b-jc-calc.val[12]  EQ job-prep.frm AND
            (b-jc-calc.val[13] EQ job-prep.blank-no OR job-prep.blank-no EQ 0) AND
            b-jc-calc.code2    NE bf-ordl.i-no
            NO-LOCK NO-ERROR.
      
       CREATE w-job-mat.
       BUFFER-COPY job-prep TO w-job-mat
       ASSIGN
        w-job-mat.w-rowid      = ROWID(job-prep)
        w-job-mat.w-recid      = RECID(job-prep)
        w-job-mat.this-is-a-rm = YES
        w-job-mat.dep          = item.s-dep
        w-job-mat.basis-w      = item.basis-w
        w-job-mat.fg-i-no      = IF AVAIL b-jc-calc THEN b-jc-calc.code2
                                                    ELSE bf-ordl.i-no
        w-job-mat.prep         = YES
        w-job-mat.rm-i-no      = prep.i-no
        w-job-mat.i-no         = prep.i-no
        w-job-mat.qty-uom      = "EA"
        w-job-mat.n-up         = prep.number-up.
   END.
 
   
   
   
   /* Create w-job-mat from item */
   FOR EACH b-job-mat WHERE
       b-job-mat.company EQ job.company AND
       b-job-mat.job     EQ job.job AND
       b-job-mat.job-no  EQ job.job-no AND
       b-job-mat.job-no2 EQ job.job-no2 AND
       NOT CAN-FIND(FIRST tt-itemfg WHERE
                          tt-itemfg.form-no EQ b-job-mat.frm AND
                          tt-itemfg.pur-man EQ YES)
      NO-LOCK,
      FIRST prep FIELDS(i-no number-up) WHERE
            prep.company EQ b-job-mat.company AND
            prep.i-no EQ b-job-mat.i-no
            NO-LOCK,
       FIRST ITEM WHERE
            item.company EQ b-job-mat.company AND
            item.i-no    EQ prep.i-no AND
            CAN-DO("7,8,M,X,Y",item.mat-type)
            NO-LOCK:

       FIND FIRST b-jc-calc WHERE
            b-jc-calc.reftable EQ "jc/jc-calc.p" AND
            b-jc-calc.company  EQ job.company AND
            b-jc-calc.loc      EQ "" AND
            b-jc-calc.code     EQ STRING(job.job,"999999999") AND
            b-jc-calc.val[12]  EQ b-job-mat.frm AND
            (b-jc-calc.val[13] EQ b-job-mat.blank-no OR b-job-mat.blank-no EQ 0) AND
            b-jc-calc.code2    NE bf-ordl.i-no
            NO-LOCK NO-ERROR.
      
       CREATE w-job-mat.
       BUFFER-COPY b-job-mat TO w-job-mat
       ASSIGN
        w-job-mat.w-rowid      = ROWID(b-job-mat)
        w-job-mat.w-recid      = RECID(b-job-mat)
        w-job-mat.this-is-a-rm = YES
        w-job-mat.dep          = item.s-dep
        w-job-mat.basis-w      = item.basis-w
        w-job-mat.fg-i-no      = IF AVAIL b-jc-calc THEN b-jc-calc.code2
                                                    ELSE bf-ordl.i-no
        w-job-mat.prep         = YES
        w-job-mat.rm-i-no      = b-job-mat.i-no
        w-job-mat.i-no         = b-job-mat.i-no
        w-job-mat.qty-uom      = "EA"
        w-job-mat.n-up         = b-job-mat.n-up.
   END. /* for each b-job-mat */
END. /* If avail job ... */


/* Up to this section is in new program */


/* wfk - Create of tt-eiv based on w-job-mat,
 *       process each w-job-mat, creating po-ordl,
 *       creating report records to feed into vendor selection screen */
outers:
FOR EACH w-job-mat
    
    BREAK BY w-job-mat.frm
          BY w-job-mat.blank-no
          BY w-job-mat.i-no.

  EMPTY TEMP-TABLE tt-ei.
  EMPTY TEMP-TABLE tt-eiv.

  v-vendor-chosen-report = ?.



  /**********************************/
  /* create-tt-eiv-from-e-item-vend */
  /* ****************************** */
  IF w-job-mat.this-is-a-rm THEN DO:

    RELEASE ITEM.

    IF w-job-mat.prep EQ NO THEN
    DO:
       IF v-po-best EQ NO THEN
          FIND FIRST item
               WHERE item.company  EQ cocode
                 AND item.i-no     EQ w-job-mat.rm-i-no
                 AND index("1234BPR",item.mat-type) GT 0
               NO-LOCK NO-ERROR.
       ELSE
          FIND FIRST item
               WHERE item.company  EQ cocode
                 AND item.i-no     EQ w-job-mat.rm-i-no
                 AND item.mat-type EQ "B"
               NO-LOCK NO-ERROR.
    END. /* If w-job-mat.prep EQ NO */
    ELSE
       FIND FIRST item
            WHERE item.company  EQ cocode
              AND item.i-no     EQ w-job-mat.rm-i-no
              NO-LOCK NO-ERROR.

    IF AVAIL item THEN
    FOR EACH e-item OF item NO-LOCK:
        CREATE tt-ei.
        ASSIGN
           tt-ei.company = e-item.company
           tt-ei.i-no    = e-item.i-no
           tt-ei.std-uom = e-item.std-uom.
       
        FOR EACH e-item-vend OF e-item NO-LOCK:
            IF NOT CAN-FIND(FIRST tt-eiv
                            WHERE tt-eiv.company   EQ e-item-vend.company
                              AND tt-eiv.i-no      EQ e-item-vend.i-no
                              AND tt-eiv.vend-no   EQ e-item-vend.vend-no) THEN DO:
               CREATE tt-eiv.
               ASSIGN
                  tt-eiv.rec-id = RECID(e-item-vend)
                  tt-eiv.company = e-item-vend.company
                  tt-eiv.vend-no = e-item-vend.vend-no
                  tt-eiv.i-no    = e-item-vend.i-no
                  tt-eiv.est-no  = e-item-vend.est-no
                  tt-eiv.form-no  = e-item-vend.form-no
                  tt-eiv.blank-no  = e-item-vend.blank-no
                  tt-eiv.item-type = e-item-vend.item-type
                  tt-eiv.vend-i-no = e-item-vend.vend-item
                  tt-eiv.rec_key   = e-item-vend.rec_key.
              
               DO v-index = 1 TO 10:
                  ASSIGN
                     tt-eiv.run-qty[v-index] = e-item-vend.run-qty[v-index]
                     tt-eiv.run-cost[v-index] = e-item-vend.run-cost[v-index]
                     tt-eiv.setups[v-index] = e-item-vend.setups[v-index]
                     tt-eiv.roll-w[v-index] = e-item-vend.roll-w[v-index].
               END.
              
               DO v-index = 11 TO 30:
                  tt-eiv.roll-w[v-index] = e-item-vend.roll-w[v-index].
               END.
              

              
               IF AVAIL e-item-vend THEN
               DO:

               
                  DO v-index = 1 TO 10:
                     ASSIGN
                        tt-eiv.run-qty[v-index + 10] = e-item-vend.runQtyXtra[v-index]
                        tt-eiv.run-cost[v-index + 10] = e-item-vend.runCostXtra[v-index]
                        tt-eiv.setups[v-index + 10] = e-item-vend.setupsXtra[v-index].
                  END. /* v-index = 1 to 10 */
               END. /* if avail b-qty */
            END. /* if tt-eiv doesn't already exist */
        END. /* for each e-item-vend */
    END. /* for each e-item */
   /* end create-tt-eiv-from-e-item-vend */
  END. /* If this-is-rm */

  ELSE DO:
    /*****************************/
    /* create-tt-eiv-from-itemfg */
    /*****************************/
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ w-job-mat.rm-i-no
        NO-LOCK NO-ERROR.

    IF AVAIL itemfg THEN
      RUN create-tt-eiv.
    /* END. /*end each e-itemfg*/ */
    /* end create-tt-eiv-from-itemfg */
  END. /*FG*/

  ASSIGN v-job-recid = ?.

  IF AVAIL job THEN
     FOR EACH job-hdr NO-LOCK
         WHERE job-hdr.company EQ job.company
           AND job-hdr.job     EQ job.job
           AND job-hdr.job-no  EQ job.job-no
           AND job-hdr.job-no2 EQ job.job-no2
         BREAK BY job-hdr.frm      DESC
               BY job-hdr.blank-no DESC:
       IF (job-hdr.frm EQ w-job-mat.frm AND
           (job-hdr.blank-no EQ w-job-mat.blank-no OR
            w-job-mat.blank-no EQ 0)) OR
          LAST(job-hdr.blank-no) THEN DO:
          
          ASSIGN v-ord-no = job-hdr.ord-no
                 v-job-recid = RECID(job-hdr).
          LEAVE.
       END.
     END. /* each job-hdr */

  /*once out of loop above, not pointing to correct job-hdr*/
  IF v-job-recid NE ? THEN
     FIND job-hdr 
          WHERE RECID(job-hdr) EQ v-job-recid
        NO-LOCK.

  ASSIGN
   sel        = 1
   fil_id     = ?
   v-sname    = ""
   v-saddr[1] = ""
   v-saddr[2] = ""
   v-scity    = ""
   v-sstate   = ""
   v-szip     = ""
   call_id    = fil_id
   nufile     = YES
   v-neword   = YES
   v-vend-no  = "".
  
  RELEASE po-ord.
  RELEASE po-ordl.
  RELEASE b-orderpo.
  rOrderPoRow = ?.
  /* 05281404 - added first po-ordl to check item # */
  FOR EACH  b-orderpo no-lock
      where b-orderpo.reftable eq "ORDERPO"
        and b-orderpo.company  eq cocode
        and b-orderpo.loc      eq string(v-ord-no,"9999999999")
        and b-orderpo.code     eq string(w-job-mat.job,"9999999999") +
                                  string(w-job-mat.frm,"9999999999")
        and b-orderpo.code2    eq w-job-mat.rm-i-no
      ,
      FIRST po-ord WHERE po-ord.company EQ cocode
        AND po-ord.po-no EQ INTEGER(b-orderpo.val[1])
        AND po-ord.stat NE "C"
        NO-LOCK,
      FIRST po-ordl WHERE po-ordl.company EQ po-ord.company
        AND po-ordl.po-no EQ po-ord.po-no
        AND po-ordl.i-no  EQ w-job-mat.rm-i-no
        AND po-ordl.s-num EQ w-job-mat.frm
      NO-LOCK
     :
    rOrderPoRow = ROWID(b-orderpo).
    
  END. /* Each b-orderpo */

  IF rOrderPoRow NE ? THEN
    FIND b-orderpo WHERE ROWID(b-orderpo) EQ rOrderPoRow.

  if not avail b-orderpo then do:
    create b-orderpo.
    assign
     b-orderpo.reftable = "ORDERPO"
     b-orderpo.company  = cocode
     b-orderpo.loc      = string(v-ord-no,"9999999999")
     b-orderpo.code     = string(w-job-mat.job,"9999999999") +
                          string(w-job-mat.frm,"9999999999")
     b-orderpo.code2    = w-job-mat.rm-i-no.

    IF AVAIL bf-ordl AND bf-ordl.vend-no NE "" AND bf-ordl.po-no-po NE 0 THEN DO:
      find first po-ordl
          where po-ordl.company eq cocode
            and po-ordl.po-no   eq bf-ordl.po-no-po
            and po-ordl.job-no  eq w-job-mat.job-no
            and po-ordl.job-no2 eq w-job-mat.job-no2
            and po-ordl.s-num   eq w-job-mat.frm
            and po-ordl.i-no    eq w-job-mat.rm-i-no
          no-lock no-error.
      if avail po-ordl THEN DO:         
        b-orderpo.val[1] = po-ordl.po-no.        
      END.
    end. /* If avail bf-ordl */

    FIND CURRENT b-orderpo NO-LOCK NO-ERROR.
  end. /* If not avail b-orderpo */
  



  find first po-ord
      where po-ord.company   eq cocode
        and po-ord.po-no     eq int(b-orderpo.val[1])
        and b-orderpo.val[1] ne 0
      no-lock no-error.
  if avail po-ord then v-vend-no = po-ord.vend-no.
  


  IF AVAIL job THEN 
    assign
     v-job  = job.job-no
     v-job  = fill(" ",6 - length(trim(v-job))) + trim(v-job) +
              "-" + string(job.job-no2,"99").
  


  choice = NO.
  IF v-vend-no EQ "" AND ((v-autopo-sec AND w-job-mat.this-is-a-rm) OR (v-autofg-sec AND NOT w-job-mat.this-is-a-rm)) AND NOT w-job-mat.isaset THEN DO ON ENDKEY UNDO, LEAVE:

    MESSAGE "Do you wish to create a PO line for " +
            (IF w-job-mat.this-is-a-rm
                          THEN ("Job/Form/RM#: " + TRIM(v-job) + "/" +
                                TRIM(STRING(w-job-mat.frm,"99")))
                          ELSE ("Order/FG#: " +
                                TRIM(STRING(v-ord-no,">>>>>>>>>>")))) +
            "/" + TRIM(w-job-mat.rm-i-no) + "?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
  END. /* Prompt to create po line */



  /*********************************************************/
  /* find first tt-ei and process to create report records */
  /*********************************************************/
  ASSIGN
   v-len         = w-job-mat.len
   v-wid         = w-job-mat.wid
   v-dep         = w-job-mat.dep
   v-basis-w     = w-job-mat.basis-w
   ll-canceled   = NO.

  FIND FIRST tt-ei
      WHERE tt-ei.company EQ cocode
        AND tt-ei.i-no    EQ w-job-mat.rm-i-no
      NO-LOCK NO-ERROR.

  IF AVAIL tt-ei THEN DO:
    
    IF w-job-mat.qty-uom EQ "BF" THEN DO:
      RUN sys/ref/convquom.p(w-job-mat.qty-uom, "EA",
                             v-basis-w, v-len, v-wid, v-dep,
                             w-job-mat.qty, OUTPUT v-job-mat-qty).

      {sys/inc/roundup.i v-job-mat-qty}
      v-job-mat-uom = "EA".
    END. /* w-job-mat.qty-uom eq "Bf */

    ELSE
      ASSIGN
       v-job-mat-uom = w-job-mat.qty-uom
       v-job-mat-qty = w-job-mat.qty.

    v-uom-comp = IF w-job-mat.this-is-a-rm THEN
                   IF CAN-DO("1,2,3,4",item.mat-type) THEN "BF" ELSE "MSF"
             ELSE tt-ei.std-uom.

    IF v-job-mat-uom EQ v-uom-comp                 OR
       (NOT w-job-mat.this-is-a-rm             AND
        LOOKUP(v-job-mat-uom,fg-uom-list) GT 0 AND
        LOOKUP(v-uom-comp,fg-uom-list)    GT 0)    THEN
      v-qty-comp = v-job-mat-qty.
    ELSE
        RUN sys/ref/convquom.p(v-job-mat-uom, v-uom-comp,
                               v-basis-w, v-len, v-wid, v-dep,
                               v-job-mat-qty, OUTPUT v-qty-comp).

    IF w-job-mat.job-no NE "" THEN
      RUN po/groupcst.p (w-job-mat.job-no,
                         w-job-mat.job-no2,
                         w-job-mat.rm-i-no,
                         w-job-mat.frm,
                         w-job-mat.blank-no,
                         INPUT-OUTPUT v-qty-comp).

    v-uom-comp = "TON".

    IF v-job-mat-uom EQ v-uom-comp                 OR
       (NOT w-job-mat.this-is-a-rm             AND
        LOOKUP(v-job-mat-uom,fg-uom-list) GT 0 AND
        LOOKUP(v-uom-comp,fg-uom-list)    GT 0)    THEN
      v-qty-comp1 = v-job-mat-qty.
    ELSE
      RUN sys/ref/convquom.p(v-job-mat-uom, v-uom-comp,
                             v-basis-w, v-len, v-wid, v-dep,
                             v-job-mat-qty, OUTPUT v-qty-comp1).




    /*****************************************/
    /* Create report records                 */
    /*****************************************/
    FOR EACH tt-eiv
        WHERE tt-eiv.company    EQ cocode
          AND tt-eiv.i-no       EQ tt-ei.i-no
          AND tt-eiv.item-type  EQ w-job-mat.this-is-a-rm
          AND tt-eiv.vend-no    NE ""
          
          AND (tt-eiv.item-type EQ NO OR
               (v-wid                GE tt-eiv.roll-w[27] AND
                v-wid                LE tt-eiv.roll-w[28] AND
                v-len                GE tt-eiv.roll-w[29] AND
                v-len                LE tt-eiv.roll-w[30]))
        NO-LOCK,

        FIRST vend
        WHERE vend.company EQ cocode
          AND vend.vend-no EQ tt-eiv.vend-no
          AND vend.active  EQ "A"
        NO-LOCK:

      DO i = 1 TO EXTENT(tt-eiv.run-qty):
        IF v-qty-comp LE tt-eiv.run-qty[i] THEN LEAVE.
      END.
      IF i > 20 THEN
          i = 20.

      v-setup = tt-eiv.setups[i].
          
      ld-dim-charge = 0.

      RUN est/dim-charge.p (tt-eiv.rec_key,
                            v-wid,
                            v-len,
                            INPUT-OUTPUT ld-dim-charge).     
      CREATE report.
      ASSIGN
       report.term-id = v-term
       report.key-01  = STRING(tt-eiv.run-cost[i] + ld-dim-charge,"9999999999.9999")
       report.key-02  = STRING(v-qty-comp) /* msf or tt-ei.std-uom */
       report.key-03  = tt-eiv.vend-no
       report.key-04  = STRING(v-qty-comp1) /* tons */
       report.key-05  = STRING((v-setup / v-qty-comp),"9999999999.9999")
       report.key-06  = STRING(v-setup,"9999999999.9999")
       report.key-07  = tt-eiv.vend-i-no
       report.rec-id  = tt-eiv.rec-id.
    END. /* for each tt-eiv */

    RELEASE report.

    
    
    IF choice THEN DO:
      RUN po/d-vndcst.w (v-term, w-job-mat.w-recid,
                         w-job-mat.this-is-a-rm, w-job-mat.i-no,
                         INPUT v-qty-comp, INPUT v-job-mat-uom).
      IF fil_id EQ ? THEN ll-canceled = YES.
      ELSE FIND report WHERE RECID(report) EQ fil_id NO-LOCK NO-ERROR.
      IF AVAIL report THEN DO:
          v-vendor-chosen-report = report.REC-ID.
          RUN create-tt-eiv.
      END.
                
    END. /* If choice = true */

    ELSE DO:
      FIND FIRST tt-eiv WHERE tt-eiv.vend-no EQ v-vend-no NO-ERROR.
      IF AVAIL tt-eiv THEN
      FIND FIRST report
          WHERE report.term-id EQ v-term
            AND report.rec-id  EQ tt-eiv.rec-id
          NO-LOCK NO-ERROR.
    END. /* If not choice = true */



    IF AVAIL report THEN DO:
        
      ASSIGN
       fil_id       = report.rec-id
       v-item-cost  = DEC(report.key-01)
       v-setup-cost = DEC(report.key-05)
       v-setup      = DEC(report.key-06)
       v-vend-item  = report.key-07.
       
      FIND FIRST vend
          WHERE vend.company EQ cocode
            AND vend.vend-no EQ report.key-03 
          NO-LOCK NO-ERROR.
      IF AVAIL vend THEN DO:        
        v-vend-no = vend.vend-no.          
        IF AVAIL bf-ordl AND FIRST(w-job-mat.frm) THEN
        FOR EACH b-oe-ordl
            WHERE b-oe-ordl.company EQ bf-ordl.company
              AND b-oe-ordl.ord-no  EQ bf-ordl.ord-no
              AND b-oe-ordl.job-no  EQ bf-ordl.job-no
              AND b-oe-ordl.job-no2 EQ bf-ordl.job-no2:
          b-oe-ordl.vend-no = vend.vend-no.
        END. /* each b-oe-ordl */
      END. /* if avail vend */
    END. /* If avail report */


    FOR EACH report
        {sys/look/reportW.i}
           AND CAN-FIND(FIRST tt-eiv WHERE tt-eiv.rec-id EQ report.rec-id):
      DELETE report.
    END.


  
    
    
  END. /* avail tt-ei */
  /**************************************/
  /* end creating report records     ****/
  /**************************************/





  /*****************************************************/
  /* Process Error on no vendor matrix                 */
  /*****************************************************/
  IF v-vend-no EQ "" AND choice AND NOT ll-canceled THEN
    MESSAGE "Cannot create a PO for " +
            (IF AVAIL job THEN ("Job/Form/RM#: " + TRIM(v-job) + "/" +
                                TRIM(STRING(w-job-mat.frm,"99")))
                          ELSE ("Order/FG#: " +
                                TRIM(STRING(v-ord-no,">>>>>>>>>>")))) +
            "/" + TRIM(w-job-mat.rm-i-no) +
            ", Vendor Matrix does not exist..."
        VIEW-AS ALERT-BOX ERROR.
  if v-vend-no eq "" OR ll-canceled then next.

  
  


  /* vend.disc-days eq lead time */
  if avail vend then v-po-date = today.
  else v-po-date = IF AVAIL bf-ord THEN bf-ord.ord-date ELSE job.start-date.

  if avail vend then v-due-date = today + vend.disc-days.
  else v-due-date = IF AVAIL bf-ord THEN bf-ord.ord-date ELSE job.start-date.
      



  /*********************************************/
  /** Prompt for Drop Shipment                 */
  /*********************************************/
  ll-drop = NO.
  IF nk1-oeautopo-int EQ 1 THEN
    MESSAGE "Is this a Drop Shipment?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll-drop.




  /****************************************************/
  /* If PO Already Exists, ask to Update it           */
  /****************************************************/
  FIND LAST po-ord
      WHERE po-ord.company   EQ cocode
        AND po-ord.po-no     EQ INT(b-orderpo.val[1])
        AND b-orderpo.val[1] NE 0
        AND po-ord.vend-no   EQ v-vend-no
        AND (po-ord.type     EQ "D" OR NOT ll-drop)
      NO-ERROR.

  IF AVAIL po-ord AND NOT po-ord.opened THEN NEXT outers.
  
  choice = AVAIL po-ord.
  
  IF choice AND po-ord.opened AND v-autopo-sec THEN DO:
    MESSAGE "Do you wish to update PO #" +
            TRIM(STRING(po-ord.po-no,">>>>>>>>")) + " for " +
            (IF AVAIL job THEN ("Job/Form/RM#: " + TRIM(v-job) + "/" +
                                TRIM(STRING(w-job-mat.frm,"99")))
                          ELSE ("Order/FG#: " +
                                TRIM(STRING(v-ord-no,">>>>>>>>>>")))) +
            "/" + TRIM(w-job-mat.rm-i-no) + "?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.

    FIND FIRST tt-eiv WHERE tt-eiv.vend-no EQ po-ord.vend-no NO-ERROR.
  END.

  ELSE find first tt-eiv where tt-eiv.rec-id eq fil_id no-lock no-error.





  if NOT choice then do:
    /****************************************/
    /** Chose not to update the existing PO */
    /****************************************/
    if not avail po-ord then do:
       run po/d-podate.w ("PO",input-output v-po-date, v-job, w-job-mat.frm, w-job-mat.rm-i-no).
       if v-due-date le v-po-date then v-due-date = v-po-date + 1.
       run po/d-podate.w ("Due",input-output v-due-date, v-job, w-job-mat.frm, w-job-mat.rm-i-no).
    end.
    ELSE RETURN.  /* don't update po if user said no */

    /*    status default "Please Wait Creating Purchase Order".  */
    choice = no.

    if v-vend-no eq "" then do:
      if avail tt-eiv then
        find first po-ord
            where po-ord.company  eq cocode
              and po-ord.po-date  eq v-po-date
              and po-ord.due-date eq v-due-date
              and po-ord.vend-no  eq tt-eiv.vend-no
              AND (po-ord.type     EQ "D" OR NOT ll-drop)
            no-error.
            
      else do:

        find first vend  where vend.company eq cocode
              and vend.vend-no eq v-vend-no
            no-lock no-error.
        if avail vend then do:
          v-vend-no = vend.vend-no.
             
          IF AVAIL bf-ordl AND FIRST-OF(w-job-mat.frm) THEN
          for each b-oe-ordl
              where b-oe-ordl.company EQ bf-ordl.company
                and b-oe-ordl.ord-no  eq bf-ordl.ord-no
                and b-oe-ordl.job-no  eq bf-ordl.job-no
                and b-oe-ordl.job-no2 eq bf-ordl.job-no2
                AND b-oe-ordl.i-no    EQ w-job-mat.fg-i-no:
            assign
             b-oe-ordl.po-no-po = po-ord.po-no
             b-oe-ordl.vend-no  = v-vend-no.
          end.
        end.
      end.
      
      find first po-ord
          where po-ord.company  eq cocode
            and po-ord.po-date  eq v-po-date
            and po-ord.due-date eq v-due-date
            and po-ord.vend-no  eq v-vend-no
            AND (po-ord.type     EQ "D" OR NOT ll-drop)
          no-error.
    end.
  end. /* not choice (chose not to update the PO */ 
  
  ELSE DO:
    /***********************************/
    /* Chose to update the existing PO */
    /***********************************/
    assign
       v-po-date  = po-ord.po-date
       v-due-date = po-ord.due-date.
  END. /* if choice */




  FIND LAST po-ord
      WHERE po-ord.company   EQ cocode
        AND po-ord.po-date   EQ v-po-date
        AND po-ord.due-date  EQ v-due-date
        AND po-ord.po-no     EQ INT(b-orderpo.val[1])
        AND b-orderpo.val[1] NE 0
        AND po-ord.vend-no   EQ v-vend-no
        AND po-ord.opened    EQ YES
        AND (po-ord.type     EQ "D" OR NOT ll-drop)
      NO-ERROR.
     
  IF NOT AVAIL po-ord THEN DO:
    FIND po-ord NO-LOCK
        WHERE po-ord.company  EQ cocode
          AND po-ord.due-date EQ v-due-date
          AND po-ord.vend-no  EQ v-vend-no
          AND po-ord.opened   EQ YES
          AND (po-ord.type    EQ "D" OR NOT ll-drop)
        NO-ERROR.

    choice = AMBIG po-ord.

    IF choice THEN
      RUN windows/l-povndt.w (cocode, v-vend-no, v-due-date, BUFFER po-ord).
    
    IF AVAIL po-ord THEN DO:
        
      IF NOT choice AND v-multijob THEN
        MESSAGE "PO exists for given Vendor and Date." SKIP
                "Do you want to update existing PO? " 
            VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE choice.

      IF v-multijob = NO THEN
         choice = NO.

      IF choice THEN FIND CURRENT po-ord NO-ERROR.
      ELSE RELEASE po-ord.
    END.
   




    /**********************************/
    /* create po-ord if doesn't exist */
    /**********************************/
    IF NOT AVAIL po-ord THEN DO:
      {po/po-ord.a}
      ASSIGN
       po-ord.po-date        = v-po-date
       po-ord.due-date       = v-due-date
       po-ord.last-ship-date = po-ord.due-date
       po-ord.vend-no        = v-vend-no.

      IF AVAIL bf-ord THEN
        ASSIGN
         ls-drop-custno = bf-ord.cust-no
         ls-ship-choice = "C". /* task# 09160518*/

      IF ll-drop THEN RUN is-dropship.

      ELSE
      IF AVAIL company THEN
        ASSIGN
         po-ord.ship-id      = company.company
         po-ord.ship-name    = company.NAME
         po-ord.ship-addr[1] = company.addr[1]
         po-ord.ship-addr[2] = company.addr[2]
         po-ord.ship-city    = company.city
         po-ord.ship-state   = company.state
         po-ord.ship-zip     = company.zip.
    END. /* Not avail po-ord then add it */




  END. /* Not avail po-ord by due-date */
  
  ELSE do: 
    choice = YES.
  END. /* avail po-ord */




  /***********************************************************/
  /* Assign PO ord values from vend and sales order  *********/
  /***********************************************************/
  FIND CURRENT b-orderpo.
  b-orderpo.val[1] = po-ord.po-no.
  FIND CURRENT b-orderpo NO-LOCK.
   
  IF AVAIL bf-ordl AND FIRST-OF(w-job-mat.frm) THEN
  for each b-oe-ordl
      where b-oe-ordl.company eq bf-ordl.company
        and b-oe-ordl.ord-no  eq bf-ordl.ord-no
        and b-oe-ordl.job-no  eq bf-ordl.job-no
        and b-oe-ordl.job-no2 eq bf-ordl.job-no2
        AND b-oe-ordl.i-no    EQ w-job-mat.fg-i-no:
    assign
     b-oe-ordl.po-no-po = po-ord.po-no
     b-oe-ordl.vend-no  = v-vend-no.
  end.

  find first vend
      where vend.company eq cocode 
        and vend.vend-no eq po-ord.vend-no 
      use-index vend no-lock no-error.
  if avail vend then do:
    assign
     po-ord.last-ship-date = po-ord.due-date
     po-ord.over-pct       = vend.over-pct
     po-ord.under-pct      = vend.under-pct
     po-ord.carrier        = vend.carrier
     po-ord.contact        = vend.contact
     po-ord.terms          = vend.terms
     po-ord.fob-code       = vend.fob-code
     po-ord.frt-pay        = vend.frt-pay
     po-ord.tax-gr         = vend.tax-gr.
     
    if po-ord.fob-code eq "" then po-ord.fob = "DEST".
    if po-ord.frt-pay  eq "" then po-ord.frt-pay = "B".
    
    if po-ord.due-date lt po-ord.po-date then
      assign
       po-ord.due-date       = po-ord.po-date
       po-ord.last-ship-date = po-ord.due-date.
  end. /* If avail vend */

  IF AVAIL bf-ordl        AND po-ord.type EQ "D"    AND
     po-ord.cust-no NE "" AND po-ord.frt-pay NE "P" THEN
  DO:
     FIND FIRST oe-rel OF bf-ordl NO-LOCK NO-ERROR.
     IF AVAIL oe-rel THEN po-ord.carrier = oe-rel.carrier.
  END.

  IF AVAIL bf-ord THEN
     ASSIGN po-ord.frt-pay = bf-ord.frt-pay
            po-ord.fob-code = bf-ord.fob-code.
  




  IF AVAIL item THEN
  FIND b-item WHERE RECID(b-item) EQ RECID(item) NO-LOCK.

  v-new-avail = no.
  /* If bf-itemfg found, then this is an FG item and don't join on frm */
  /* 05281404 - added join to rm-i-no */
  FIND FIRST bf-itemfg
    WHERE bf-itemfg.company EQ bf-ordl.company
      AND bf-itemfg.i-no    EQ /* wfk - 05281404 - bf-ordl.i-no */ w-job-mat.rm-i-no
    NO-LOCK NO-ERROR.

  if choice then
  find first po-ordl
      where po-ordl.company    eq cocode
        and po-ordl.job-no     eq w-job-mat.job-no
        and po-ordl.job-no2    eq w-job-mat.job-no2
        and (po-ordl.s-num      eq w-job-mat.frm OR AVAIL(bf-itemfg))
        and po-ordl.i-no       eq w-job-mat.rm-i-no
        and po-ordl.item-type  eq w-job-mat.this-is-a-rm
        and (w-job-mat.job-no ne "" or
             po-ordl.ord-no    eq v-ord-no)
      no-error.
        
  if not avail po-ordl and v-format eq "AutoRM" AND v-autopo-sec and w-job-mat.this-is-a-rm then do:
    find first po-ordl
        where po-ordl.company eq cocode
          and po-ordl.job-no  eq w-job-mat.job-no
          and po-ordl.job-no2 eq w-job-mat.job-no2
          and po-ordl.s-num   eq w-job-mat.frm
          and po-ordl.i-no    eq
                if length(w-job-mat.i-no) le 10 then w-job-mat.i-no
                else substr(w-job-mat.i-no,length(w-job-mat.i-no) - 9,10)
        no-error.
        
    if avail po-ordl then do:
      find first b-item
          where b-item.company eq cocode
            and b-item.i-no    eq
                  if length(w-job-mat.i-no) le 10 then w-job-mat.i-no
                  else substr(w-job-mat.i-no,length(w-job-mat.i-no) - 9,10)
            no-lock no-error.
            
      if avail b-item then v-new-avail = yes.
      else
      DO:
        IF w-job-mat.prep EQ NO THEN
        DO:
           IF v-po-best EQ NO THEN
              find first b-item
                  where b-item.company  eq cocode 
                    and b-item.i-no     eq w-job-mat.rm-i-no 
                    and index("1234BPR",b-item.mat-type) gt 0 
                  no-lock no-error.
           ELSE
              find first b-item
               where b-item.company  eq cocode 
                 and b-item.i-no     eq w-job-mat.rm-i-no 
                 and b-item.mat-type EQ "B" 
               no-lock no-error.
        END. /* if w-job-mat.prep eq no */
        ELSE
           find first b-item
               where b-item.company  eq cocode 
                 and b-item.i-no     eq w-job-mat.rm-i-no
               no-lock no-error.
      END. /* not avail b-item */
    end. /* if avail po-ordl */
  end. /* Not avail po-ordl and this is RM */



  /*************************************/
  /* create po-ordl if doesn't exists  */
  /************************************/
  if not avail po-ordl then do:
    {po/po-ordl.a}
    ASSIGN
     po-ordl.tax       = po-ord.tax-gr NE "" AND
                         (aptax-chr EQ "Vendor" OR 
                          (aptax-chr EQ "Item" AND
                           (AVAIL b-item AND b-item.tax-rcpt) OR
                           (AVAIL itemfg AND itemfg.taxable)))
     po-ordl.item-type = w-job-mat.this-is-a-rm.
  end. /* Not avail po-ordl then add it */
  
  /***********************************/
  /* Assign po-ordl values from item */
  /***********************************/
  IF po-ordl.item-type THEN DO:
    ASSIGN v-actnum = "".
    find first costtype no-lock
        where costtype.company   eq po-ordl.company
          and costtype.loc       eq po-ord.loc
          and costtype.cost-type eq b-item.cost-type
        no-error.
    if avail costtype and ap-gl#-log then
      po-ordl.actnum = IF ap-gl#-cha EQ "Asset"   THEN costtype.inv-asset
                       ELSE
                       IF ap-gl#-cha BEGINS "Exp"  AND
                          (ap-gl#-cha EQ "Expense" OR costtype.cons-exp NE "")
                                                  THEN costtype.cons-exp
                       ELSE                            po-ordl.actnum.


    /* populate GL# from job-hdr.i-no + itemfg tables, then reftable AH 02-24-10*/
    IF v-job-recid <> ? THEN DO:
       FIND b-job-hdr NO-LOCK WHERE RECID(b-job-hdr) = v-job-recid NO-ERROR.
       IF AVAIL b-job-hdr THEN 
          RUN get-itemfg-gl (INPUT b-job-hdr.company, b-job-hdr.i-no, OUTPUT v-actnum).
       IF v-actnum <> "" THEN 
          ASSIGN po-ordl.actnum = v-actnum.
    END.
    RELEASE b-job-hdr.

    find first tt-ei
        where tt-ei.company eq cocode
          and tt-ei.i-no    eq b-item.i-no
        no-lock no-error.

    assign
     po-ordl.s-num      = w-job-mat.frm
     po-ordl.b-num      = w-job-mat.blank-no
     po-ordl.job-no     = job.job-no
     po-ordl.job-no2    = job.job-no2
     po-ordl.i-no       = b-item.i-no
     po-ordl.i-name     = b-item.i-name
     po-ordl.pr-qty-uom = w-job-mat.qty-uom
     po-ordl.cons-uom   = b-item.cons-uom
     po-ordl.pr-uom     = b-item.pur-uom
     po-ordl.cons-cost  = b-item.last-cost
     v-part-dscr1       = b-item.i-dscr
     v-part-dscr2       = b-item.est-dscr
     v-op-type          = yes.
    
  END. /* if R/M*/

  ELSE DO:
  


    /**************************************/
    /*** Assign PO Values from itemfg  ***/
    /*************************************/
    IF AVAIL itemfg THEN DO:
      find first tt-ei
          where tt-ei.company eq cocode
            and tt-ei.i-no    eq itemfg.i-no
          no-lock no-error.
     IF AVAIL job THEN
     FIND FIRST xest
         where xest.company EQ po-ordl.company
           and xest.est-no  EQ job.est-no
         NO-ERROR.
     IF AVAIL(xest) THEN
       FIND FIRST xeb where xeb.company = po-ordl.company
                       and xeb.est-no = xest.est-no
                       AND xeb.stock-no EQ itemfg.i-no
                     NO-LOCK NO-ERROR.
  
  
      ASSIGN
       po-ordl.i-no       = itemfg.i-no
       po-ordl.i-name     = bf-ordl.i-name
       po-ordl.pr-qty-uom = IF pouom-chr EQ "Purchase" THEN itemfg.pur-uom
                                                       ELSE itemfg.prod-uom
       po-ordl.cons-uom   = itemfg.prod-uom
       po-ordl.pr-uom     = itemfg.pur-uom
       po-ordl.cons-cost  = itemfg.last-cost
       v-part-dscr1       = bf-ordl.part-dscr1
       v-part-dscr2       = bf-ordl.part-dscr2
       po-ordl.ord-qty    = w-job-mat.qty
       v-op-type          = NO.
  
       IF AVAIL xeb THEN
         ASSIGN
          po-ordl.s-num      = xeb.form-no
          po-ordl.b-num      = xeb.blank-no.
            
       /* Uncomment below to implement a vendor matrix UOM per vendor */
  /*      IF v-vendor-chosen-report NE ? THEN DO:                                           */
  /*          FIND FIRST e-itemfg-vend WHERE recid(e-itemfg-vend) EQ v-vendor-chosen-report */
  /*              NO-LOCK.                                                                  */
  /*          IF AVAIL e-itemfg-vend THEN                                                   */
  /*          po-ordl.pr-uom = e-itemfg-vend.std-uom.                                       */
  /*      END.                                                          
                        */
       IF NOT AVAIL tt-ei THEN
           FIND FIRST tt-ei
             WHERE tt-ei.company EQ cocode
             AND tt-ei.i-no    EQ w-job-mat.rm-i-no
            NO-LOCK NO-ERROR.
  
  
       IF AVAIL tt-ei THEN
           po-ordl.pr-uom = tt-ei.std-uom.
           
       
      FIND oe-ordl WHERE RECID(oe-ordl) EQ lv-recid EXCLUSIVE-LOCK NO-ERROR.
  
      IF AVAIL oe-ordl THEN
      DO:
         IF po-ordl.cons-uom EQ "M" THEN
            oe-ordl.cost = po-ordl.cons-cost.
         ELSE
            RUN sys/ref/convcuom.p (po-ordl.cons-uom, "M", 0, 0, 0, 0,
                                    po-ordl.cons-cost, OUTPUT oe-ordl.cost).
         
         RELEASE oe-ordl. 
      END. /* oe-ordl */
  
      IF bf-ordl.i-no NE itemfg.i-no THEN
        ASSIGN
         po-ordl.i-name = itemfg.i-name
         v-part-dscr1   = itemfg.part-dscr1
         v-part-dscr2   = itemfg.part-dscr2.
  
      IF po-ordl.pr-qty-uom NE "EA"                    AND
         (po-ordl.item-type OR
          LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0) THEN
        RUN sys/ref/convquom.p("EA", po-ordl.pr-qty-uom,
                               w-job-mat.basis-w, w-job-mat.len, w-job-mat.wid, w-job-mat.dep,
                               po-ordl.ord-qty, OUTPUT po-ordl.ord-qty).
  
      FOR EACH prodl FIELDS(prolin)
          WHERE prodl.company EQ cocode
            AND prodl.procat  EQ itemfg.procat
          NO-LOCK,
          FIRST prod FIELDS(fg-mat)
          WHERE prod.company EQ cocode
            AND prod.prolin  EQ prodl.prolin
          NO-LOCK:
  
        po-ordl.actnum = prod.fg-mat.
        LEAVE.
      END. /* each prodl */
  
      if itemfg.vend-no eq po-ord.vend-no then
               po-ordl.vend-i-no = itemfg.vend-item.
      ELSE if itemfg.vend2-no eq po-ord.vend-no then
               po-ordl.vend-i-no = itemfg.vend2-item.
  
      /* populate GL# from reftable if it exists using itemfg AH 02-23-10*/
      ASSIGN v-charge = "".
      FIND FIRST surcharge WHERE surcharge.company = cocode
                             AND surcharge.charge <> "" NO-LOCK NO-ERROR.
      IF AVAIL surcharge THEN
         ASSIGN v-charge = surcharge.charge.
      FIND FIRST reftable WHERE reftable.reftable EQ "chargecode"
             AND reftable.company  EQ itemfg.company
             AND reftable.loc      EQ itemfg.procat
             AND reftable.code     EQ v-charge
            /* AND reftable.code2 = "" */
             NO-LOCK NO-ERROR.
      IF AVAIL reftable AND reftable.dscr <> "" THEN 
         ASSIGN po-ordl.actnum = reftable.dscr.
  
    END. /* Type is fg and avail itemfg, assign values from itemfg */
  END. /* Type is FG */
  
  
  
  po-ordl.ord-no = if avail bf-ordl then bf-ordl.ord-no else 0.




  /*******************************************/
  /* Assign more values to po-ordl if a RM  **/
  /*******************************************/
  IF po-ordl.item-type THEN DO:
    if avail tt-ei then do:
      if (tt-ei.std-uom eq "MSF" or tt-ei.std-uom eq "EA" or
          tt-ei.std-uom eq "M" or tt-ei.std-uom eq "MSH"  or
          tt-ei.std-uom eq "BF") then
        po-ordl.cons-uom = "EA".
      else
        po-ordl.cons-uom = tt-ei.std-uom.
      
      po-ordl.pr-uom = tt-ei.std-uom.

      IF AVAIL bf-ordl AND po-ordl.item-type then do:
        /* Adding code to create new RM from Estimated RM */
        assign
         v-new-i-no = if length(bf-ordl.i-no) le 10 then bf-ordl.i-no
                      else substring(bf-ordl.i-no,(length(bf-ordl.i-no) - 9),10)
         v-new-len  = w-job-mat.len
         v-new-wid  = w-job-mat.wid.
                       
        if v-format eq "AutoRM" AND v-autopo-sec and not v-new-avail then do:
          fil_id = recid(b-item).
          run rm/itemcopy.p.   /* not done */
        end. /* format eq "AutoRM" ... */
      END. /* avail bf-ordl ... */
    end. /* AVail tt-ei */

    IF pouom-int EQ 1 AND b-item.mat-type EQ "P" THEN po-ordl.cons-uom = "TON".
  END. /* po-ordl.item-type = yes */


  /****************************************/
  /** Calculate len & width values       **/
  /****************************************/
  assign
   v-len = 0
   v-wid = 0
   v-dep = 0.

  IF AVAIL b-item THEN DO:

    FIND FIRST e-item-vend WHERE
         e-item-vend.company EQ cocode AND
         e-item-vend.i-no EQ po-ordl.i-no AND
         e-item-vend.vend-no EQ po-ord.vend-no
         NO-LOCK NO-ERROR.

    IF AVAIL e-item-vend AND e-item-vend.vend-item NE "" THEN
       po-ordl.vend-i-no = e-item-vend.vend-item.
    ELSE
      if b-item.vend-no eq po-ord.vend-no then
         po-ordl.vend-i-no = b-item.vend-item.
    else
    if b-item.vend2-no eq po-ord.vend-no then
      po-ordl.vend-i-no = b-item.vend2-item.

    if index("1234BPR",b-item.mat-type) gt 0 then do:
      assign
       v-basis-w = b-item.basis-w
       v-len     = b-item.s-len
       v-wid     = b-item.s-wid
       v-dep     = b-item.s-dep.
     
      if v-wid eq 0 then v-wid = b-item.r-wid.
    end. /* if index(... */
  END. /* Avail b-item */

  
  
  
  /* Cust-no from order or job */
  po-ordl.cust-no = IF AVAIL bf-ord THEN bf-ord.cust-no
                    ELSE
                    IF AVAIL job-hdr THEN job-hdr.cust-no
                    ELSE "".






  /****************************************/
  /**  If an RM, get values from estimate */
  /****************************************/
  IF po-ordl.item-type THEN DO:
    /* S-8-POQTY JOBQTY or NETSHTS */

    IF v-po-qty OR w-job-mat.n-up EQ 0 OR
      INDEX("BP",item.mat-type) LE 0 THEN ld-line-qty = w-job-mat.qty.  /* Job Qty */

    ELSE DO:
      ASSIGN
       ld-line-qty = IF AVAIL bf-ordl THEN
                        bf-ordl.qty
                     ELSE
                     IF AVAIL job-hdr THEN
                        job-hdr.qty
                     ELSE
                        w-job-mat.qty
       ld-part-qty = 0.

      FIND FIRST est
          WHERE est.company EQ job.company
            AND est.est-no  EQ job.est-no
          NO-LOCK NO-ERROR.

      IF AVAIL est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN
         FOR EACH eb FIELDS(quantityPerSet)
             WHERE eb.company EQ job.company
               AND eb.est-no  EQ job.est-no
               AND eb.form-no EQ w-job-mat.frm
             NO-LOCK:
           ld-part-qty = ld-part-qty +
                         (ld-line-qty * IF eb.quantityPerSet lt 0 THEN (-1 / eb.quantityPerSet)
                                                           ELSE eb.quantityPerSet).
         END. /* Each eb */

      ELSE ld-part-qty = ld-line-qty.

      IF NOT(AVAIL est AND (est.est-type EQ 4 OR est.est-type EQ 8)) THEN
         ld-line-qty = ld-part-qty / w-job-mat.n-up.

      IF po-ordl.pr-qty-uom EQ "EA" THEN DO:
        {sys/inc/roundup.i ld-line-qty}             
      END.
    END. /* NOT v-po-qty OR w-job-mat.n-up EQ 0 OR ... */

    po-ordl.ord-qty = ld-line-qty.
  END. /* If po-ordl.item-type */
  
  assign
   v-len              = w-job-mat.len
   v-wid              = w-job-mat.wid.
  IF po-ordl.s-num EQ 0 AND po-ordl.b-num EQ 0 THEN
    ASSIGN po-ordl.s-num      = w-job-mat.frm
           po-ordl.b-num      = w-job-mat.blank-no.

  IF po-ordl.pr-qty-uom EQ "BF" THEN DO:
    RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",
                           v-basis-w, v-len, v-wid, v-dep,
                           po-ordl.ord-qty, output po-ordl.ord-qty).

    {sys/inc/roundup.i po-ordl.ord-qty}
    po-ordl.pr-qty-uom = "EA".
  END. /* po-ordl.pr-qty-uom EQ "BF" */
  

  /************************************/
  /* Assign cost-setup-pruom based on tt-ei      */
  /************************************/
  IF NOT AVAIL tt-ei THEN
    FIND FIRST tt-ei
      WHERE tt-ei.company EQ cocode
        AND tt-ei.i-no    EQ w-job-mat.rm-i-no
      NO-LOCK NO-ERROR.

  IF NOT AVAIL tt-eiv THEN
    FIND FIRST tt-eiv
          WHERE tt-eiv.company   EQ w-job-mat.company
            AND tt-eiv.i-no      EQ w-job-mat.i-no
            AND tt-eiv.vend-no   EQ v-vend-no
          NO-LOCK NO-ERROR.
  
  IF AVAIL tt-ei  THEN DO:
  
    ASSIGN
     po-ordl.cost   = v-item-cost
     po-ordl.setup  = v-setup     
     po-ordl.vend-i-no = v-vend-item.
    /* Uncomment below to implement vendor UOM per matrix */
    /* IF v-vendor-chosen-report EQ ? THEN */
        po-ordl.pr-uom = tt-ei.std-uom.
  END. /* Avail tt-ei */
  ELSE
    ASSIGN
     po-ordl.cost   = w-job-mat.std-cost
     po-ordl.setup  = 0
     po-ordl.pr-uom = w-job-mat.sc-uom.





  /***********************************/
  /* Total MSF Calculation           */
  /**********************************/
  if po-ordl.pr-qty-uom eq "EA" then
    v-tot-msf = if v-corr then ((((po-ordl.s-len * po-ordl.s-wid) * .007) *
                                 po-ordl.ord-qty) / 1000)
                          else ((((po-ordl.s-len * po-ordl.s-wid) / 144) *
                                 po-ordl.ord-qty) / 1000).
  else
    v-tot-msf = 0.


  /** Appears to be here so that it is assigned after the Total MSF Calculation */
  assign
   po-ordl.s-len = v-len
   po-ordl.s-wid = v-wid.



  /************************************/
  /* Handle PO Adders                 */
  /************************************/
  FIND job-mat WHERE RECID(job-mat) EQ w-job-mat.w-recid NO-LOCK NO-ERROR.

  if avail job-mat                        and 
     avail b-item                         and
     index("1234BPR",b-item.mat-type) gt 0 and
     b-item.i-code   eq "E"               then 
    run po/po-adder.p (recid(po-ordl), recid(job-mat)).

  FIND CURRENT po-ordl.

  if (v-pocost1 BEGINS "Vendor" or po-ordl.job-no eq "") and
      v-op-type                                          and
      nufile                                             then
     {po/po-vendc.i}
    




  /*****************************/
  /* Calculate of po-ordl.cost */
  /*****************************/
  IF po-ordl.item-type         AND
     v-pocost1 EQ "Vendor/MSH" AND
     AVAIL tt-ei               AND
     tt-ei.std-uom EQ "TON"    AND
     v-basis-w NE 0            AND
     v-wid NE 0                THEN DO:
    RUN sys/ref/convcuom.p (tt-ei.std-uom, "MSH",
                            v-basis-w, v-len, v-wid, v-dep,
                            po-ordl.cost, OUTPUT po-ordl.cost).
    po-ordl.pr-uom = "MSH".
  END. /* if po-ordl.item-type ... */

  RELEASE b-item.





  /***********************************/
  /** hande a zero len or width      */
  /***********************************/
  IF po-ordl.item-type THEN /*DO*/
    find first b-item
        where b-item.company eq cocode
          and b-item.i-no    eq po-ordl.i-no
        use-index i-no no-lock no-error.
        
    if avail b-item and b-item.i-code eq "E"
                       and index("1234BPRWF",b-item.mat-type) gt 0 then do:
      if (v-len eq 0 or v-wid eq 0) then do:
        message "Invalid Length or Width. Enter Valid Job or Non-Zero Length and Width"
                 view-as alert-box error.
        /* return ??*/          
      end. /* v-len eq 0 ... */
    end. /* avail b-item ... */





  IF (avail b-item and index("1234BPR",b-item.mat-type) gt 0) OR
     NOT po-ordl.item-type                                   then do:


    if v-len               eq 0                       AND
       po-ordl.cons-uom    eq "EA"                    AND
      (po-ordl.item-type OR
       LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0 OR
       LOOKUP(po-ordl.pr-uom,fg-uom-list)     EQ 0)   AND
      (po-ordl.pr-qty-uom ne po-ordl.cons-uom or
       po-ordl.pr-uom     ne po-ordl.cons-uom)        THEN do:
      message "Length must be entered"  view-as alert-box error.
    end. /* if v-len eq 0 ... */




    /***********************************************/
    /* Handle a zero line quantity or cost         */
    /***********************************************/
    /** Purchase UOM and Consuption UOM are equal **/
    if not po-ord.received and (v-hld-line-qty eq 0 or v-hld-cost eq 0) then do:



       if po-ordl.pr-qty-uom eq po-ordl.cons-uom           OR
         (NOT po-ordl.item-type                       AND
          LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) GT 0 AND
          LOOKUP(po-ordl.cons-uom,fg-uom-list)   GT 0)     THEN
         po-ordl.cons-qty = po-ordl.ord-qty.
       else
         run sys/ref/convquom.p(po-ordl.pr-qty-uom, po-ordl.cons-uom,
                                v-basis-w, v-len, v-wid, v-dep,
                                po-ordl.ord-qty, output po-ordl.cons-qty).
      
       if po-ordl.pr-uom eq po-ordl.cons-uom           OR
         (NOT po-ordl.item-type                     AND
          LOOKUP(po-ordl.pr-uom,fg-uom-list)   GT 0 AND
          LOOKUP(po-ordl.cons-uom,fg-uom-list) GT 0)   THEN
         po-ordl.cons-cost = po-ordl.cost.
       else
         run sys/ref/convcuom.p(po-ordl.pr-uom, po-ordl.cons-uom,
                                v-basis-w, v-len, v-wid, v-dep,
                                po-ordl.cost, output po-ordl.cons-cost).





       /*FG*/
       /**************************************/
       /* Calculate oe-ordl.cost             */
       /*************************************/
       IF NOT po-ordl.item-type THEN
       DO:
          FIND oe-ordl WHERE RECID(oe-ordl) EQ lv-recid EXCLUSIVE-LOCK NO-ERROR.
      
          IF AVAIL oe-ordl THEN
          DO:
             IF po-ordl.cons-uom EQ "M" THEN
                oe-ordl.cost = po-ordl.cons-cost.
             ELSE
                RUN sys/ref/convcuom.p (po-ordl.cons-uom, "M", 0, 0, 0, 0,
                                        po-ordl.cons-cost, OUTPUT oe-ordl.cost).
          END. /* avail oe-ordl */
      
          FIND FIRST reftable WHERE
               reftable.reftable EQ 'e-itemfg-vend.markup' AND
               reftable.company EQ po-ordl.company AND
               reftable.loc EQ po-ordl.i-no AND
               reftable.code EQ po-ord.vend-no
               NO-LOCK NO-ERROR.
          
          IF AVAIL reftable THEN
          DO:
             oe-ordl.cost = oe-ordl.cost * (1 + (reftable.val[1]/ 100.0 )).
             RELEASE reftable.
          END. /* avail reftable */

          RELEASE oe-ordl.
       END. /* not po-ordl.item-type */



    END. /* not po-ord.received  ... */




    /**************************************************************/
    /**  Calculate Extended cost, order quantity is based on UOM **/
    /**************************************************************/
    IF LOOKUP(po-ordl.pr-uom,"L,LOT") GT 0 THEN
      po-ordl.t-cost = (po-ordl.cost + po-ordl.setup) *
                       IF po-ordl.ord-qty LT 0 THEN -1 ELSE 1.

    ELSE DO:
      v-ord-qty = po-ordl.ord-qty.

      IF po-ordl.pr-qty-uom NE po-ordl.pr-uom            AND
         (po-ordl.item-type                           OR
          LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0 OR
          LOOKUP(po-ordl.pr-uom,fg-uom-list)     EQ 0)   THEN
 
        RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, po-ordl.pr-uom,
                               v-basis-w, v-len, v-wid, v-dep,
                               v-ord-qty, OUTPUT v-ord-qty).
   
      po-ordl.t-cost = (v-ord-qty * po-ordl.cost) + po-ordl.setup.
    END. /* NOT LOOKUP(po-ordl.pr-uom,"L,LOT") GT 0 */

    po-ordl.cons-cost = po-ordl.t-cost / po-ordl.cons-qty.

    IF po-ordl.disc NE 0 THEN po-ordl.t-cost = po-ordl.t-cost * (1 - (po-ordl.disc / 100)).




    /****************************************************************/
    /** Put back out total cost of line item from total cost of PO **/
    /****************************************************************/
    assign
     v-old-i-no = po-ordl.i-no
     v-tot-ord  = v-tot-ord + po-ordl.t-cost
     po-ordl.dscr[1] = v-part-dscr1
     po-ordl.dscr[2] = v-part-dscr2.

    if v-format eq "AutoRM" AND v-autopo-sec then do:
      find first b-item
          where b-item.company eq cocode
            and b-item.i-no    eq v-new-i-no
            AND b-item.i-no    NE ""
          no-lock no-error.
      if avail b-item then po-ordl.i-no = v-new-i-no.
      find first xjob-mat where recid(xjob-mat) eq recid(job-mat) no-error.
      if avail xjob-mat then w-job-mat.rm-i-no = v-new-i-no.
    END. /* v-format eq "AutoRM" ... */ 


  END. /* if Board, paper or a FG ... */

  
  
  
  
  
  
  EMPTY TEMP-TABLE tt-ref1.

  {po/po-ordls.i}

  IF NOT po-ordl.item-type THEN
    RUN jc/writeFarmFromPO.p (ROWID(po-ordl), po-ordl.job-no, STRING(po-ordl.job-no2),
                   STRING(po-ordl.ord-no), po-ordl.i-no, STRING(po-ordl.s-num),
                   STRING(po-ordl.b-num)).
  

  /********************************************/
  /* Process Scoring Changes ******************/
  /********************************************/
  IF AVAIL b-ref1 THEN DO:
    CREATE tt-ref1.
    BUFFER-COPY b-ref1 TO tt-ref1.
    DELETE b-ref1.
  END.

  IF AVAIL b-ref2 THEN DO:
    CREATE tt-ref2.
    BUFFER-COPY b-ref2 TO tt-ref2.
    DELETE b-ref2.
  END.

  RUN po/po-ordls.p (RECID(po-ordl)).

  IF AVAIL tt-ref1 OR AVAIL tt-ref2 THEN DO:
    {po/po-ordls.i}

    ll = NO.
    IF AVAIL b-ref1 AND AVAIL tt-ref1 THEN 
      DO li = 1 TO EXTENT(b-ref1.val):
        IF b-ref1.val[li] NE tt-ref1.val[li] THEN ll = YES.
        IF ll THEN LEAVE.
      END. /* avail b-ref1 ... */
    IF NOT ll                         AND
       AVAIL b-ref2 AND AVAIL tt-ref2 THEN 
      DO li = 1 TO EXTENT(b-ref2.val):
        IF b-ref2.val[li] NE tt-ref2.val[li] THEN ll = YES.
        IF ll THEN LEAVE.
      END. /* Not ll */

    IF ll THEN DO:
      ll = NO.
      MESSAGE "Scoring allowances have changed for PO/RM#: " +
              TRIM(STRING(po-ordl.po-no,">>>>>>>>"))         +
              "/" + po-ordl.i-no                             +
              ", would you like to apply the new scores?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.
    END. /* If ll */

    IF NOT ll THEN DO:
      IF AVAIL b-ref1 AND AVAIL tt-ref1 THEN BUFFER-COPY tt-ref1 TO b-ref1.
      IF AVAIL b-ref2 AND AVAIL tt-ref2 THEN BUFFER-COPY tt-ref2 TO b-ref2.
    END. /* not ll */

  END. /* AVail tt-ref1 */




  /* Calculate PO Header Totals */
  RUN po/po-total.p (RECID(po-ord)).






  /*******************************************/
  /*** Validate Maximum P.O. Cost          ***/
  /*******************************************/
  IF NOT v-hold AND po-ord.stat NE "H" AND
     AVAIL vend AND vend.rebate-% NE 0 THEN
  DO:
     v-tot-cost = 0.
     FOR EACH b-po-ordl fields(t-cost) WHERE
         b-po-ordl.company EQ po-ord.company AND
         b-po-ordl.po-no EQ po-ord.po-no
         NO-LOCK:

         v-tot-cost = v-tot-cost + b-po-ordl.t-cost.
     END.

     IF v-tot-cost GT vend.rebate-% THEN
     DO:
         MESSAGE "Purchase Order Cost Has Exceeded Vendor's Max P.O. Cost." SKIP
                 "Purchase Order Will Be Placed On Hold."
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
         ASSIGN v-hold = YES
                po-ord.stat = "H".
     END. /* v-tot-cost gt vend.rebate-% */
  END. /* not v-hold ...*/
  

  /* Update item inventory totals */
  RUN po/poordlup.p (RECID(po-ordl), 1, v-cont-upd).


END.  /* if not avail po-ordl and v-format eq "AutoRM" */

RETURN.

PROCEDURE is-dropship.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR look-recid AS RECID NO-UNDO.
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR rec-val AS RECID NO-UNDO.

  DEF VAR ship-choice AS CHAR LABEL "  Ship To"
      VIEW-AS RADIO-SET HORIZONTAL
      RADIO-BUTTONS "Vendor", "Vendor",
                    "Customer", "Customer"
      SIZE 28 BY 1 NO-UNDO.

  DEF BUTTON Btn_OK AUTO-GO 
      LABEL "OK" 
      SIZE 15 BY 1
      BGCOLOR 8.

  DEF FRAME f-drop
      ship-choice SKIP
      btn_ok AT ROW 2 COL 11
      WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER TITLE "Drop Ship PO"
           SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE.

  ON "value-changed" OF ship-choice
  DO:
    ls-ship-choice = SUBSTR(ship-choice:SCREEN-VALUE,1,1).
  END.

  ON 'choose':U OF btn_ok
  DO:
    APPLY "go" TO FRAME f-drop.
  END.

  po-ord.type = "D".
  RELEASE itemfg.
  IF AVAIL bf-ordl AND NOT w-job-mat.this-is-a-rm THEN
  FIND FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ bf-ordl.company
        AND itemfg.i-no    EQ bf-ordl.i-no
      NO-ERROR.

  CREATE WIDGET-POOL "w-drop".

  IF ls-drop-custno EQ "" THEN ls-ship-choice = "V".
  ELSE 
  IF NOT AVAIL itemfg  /* task# 09160518*/ THEN DO:
    ENABLE ship-choice btn_ok WITH FRAME f-drop.
    APPLY "value-changed" TO ship-choice.
    APPLY "entry" TO ship-choice.
    WAIT-FOR GO OF FRAME f-drop.
  END.

  IF ls-ship-choice EQ "C" THEN DO WHILE TRUE:
    IF AVAIL itemfg THEN DO:  /* task# 09160518*/
      FIND FIRST oe-rel OF bf-ordl NO-LOCK NO-ERROR.
      IF AVAIL oe-rel THEN
      FIND FIRST shipto NO-LOCK
          WHERE shipto.company eq bf-ordl.company
            and shipto.cust-no eq bf-ordl.cust-no
            and shipto.ship-id eq oe-rel.ship-id
          use-index ship-id NO-ERROR.
      IF AVAIL shipto THEN rec-val = RECID(shipto).
    END. /* if avail itemfg */

    IF NOT AVAIL itemfg OR rec-val EQ ? THEN  /* task# 09160518*/
    RUN windows/l-shipt2.w (cocode, locode, ls-drop-custno, po-ord.ship-id, OUTPUT char-val, OUTPUT rec-val).

    FIND shipto WHERE RECID(shipto) EQ rec-val NO-LOCK NO-ERROR.
    IF AVAIL shipto THEN DO:
      ASSIGN
       po-ord.cust-no       = ls-drop-custno
       po-ord.ship-id       = shipto.ship-id
       po-ord.ship-name     = shipto.ship-name
       po-ord.ship-addr[1]  = shipto.ship-addr[1]
       po-ord.ship-addr[2]  = shipto.ship-addr[2]
       po-ord.ship-city     = shipto.ship-city
       po-ord.ship-state    = shipto.ship-state
       po-ord.ship-zip      = shipto.ship-zip.

      IF po-ord.frt-pay NE "P" THEN po-ord.carrier = shipto.carrier.
      LEAVE.
    END. /* if avail shipto */
  END. /* if ls-ship-choice eq "C" */

  ELSE
  DO WHILE TRUE:
    RUN windows/l-vendno.w (cocode, "A", po-ord.ship-id, OUTPUT char-val).
    IF char-val NE "" THEN DO:
      po-ord.ship-id = ENTRY(1,char-val).
      FIND FIRST vend NO-LOCK
          WHERE vend.company EQ cocode
            AND vend.vend-no EQ po-ord.ship-id
          NO-ERROR.
      IF AVAIL vend THEN DO:
        ASSIGN
         po-ord.cust-no      = ""
         po-ord.ship-name    = vend.name
         po-ord.ship-addr[1] = vend.add1
         po-ord.ship-addr[2] = vend.add2
         po-ord.ship-city    = vend.city
         po-ord.ship-state   = vend.state
         po-ord.ship-zip     = vend.zip
         po-ord.carrier      = vend.carrier.
        LEAVE.
      END. /* avail vend */
    END. /* char-val ne "" */
  END. /* NOT ls-ship-choice eq "C" */
  
END PROCEDURE. /* is-dropship */

PROCEDURE get-itemfg-gl.
DEFINE INPUT PARAMETER ip-comp LIKE job-hdr.company.
DEFINE INPUT PARAMETER ip-i-no LIKE itemfg.i-no.
DEFINE OUTPUT PARAMETER out-actnum LIKE po-ordl.actnum.

DEFINE VARIABLE v-charge AS CHAR NO-UNDO.
     
 /* populate GL# from reftable if it exists using itemfg AH 02-23-10 */

 FIND itemfg NO-LOCK WHERE itemfg.company = ip-comp
                       AND itemfg.i-no = ip-i-no NO-ERROR.
 IF AVAIL itemfg THEN DO:
    ASSIGN v-charge = "".
    FIND FIRST surcharge WHERE surcharge.company = ip-comp
                           AND surcharge.charge <> "" NO-LOCK NO-ERROR.
    IF AVAIL surcharge THEN
       ASSIGN v-charge = surcharge.charge.
    FIND FIRST reftable WHERE reftable.reftable EQ "chargecode"
           AND reftable.company  EQ itemfg.company
           AND reftable.loc      EQ itemfg.procat
           AND reftable.code     EQ v-charge
         /* AND reftable.code2 = "" */
           NO-LOCK NO-ERROR.

    IF AVAIL reftable AND reftable.code2 <> "" THEN 
       ASSIGN out-actnum = reftable.code2.
    RELEASE reftable.
 END. /* avail itemfg */
END PROCEDURE. /* get-itemfg-gl */

PROCEDURE create-tt-eiv:

    FOR EACH e-itemfg
        WHERE e-itemfg.company EQ itemfg.company
          AND e-itemfg.i-no    EQ itemfg.i-no
        NO-LOCK:
        CREATE tt-ei.
        ASSIGN
           tt-ei.company = e-itemfg.company
           tt-ei.i-no    = e-itemfg.i-no
           tt-ei.std-uom = e-itemfg.std-uom.
        

        IF w-job-mat.est-no NE "" THEN DO:
           FOR EACH e-itemfg-vend NO-LOCK
               WHERE e-itemfg-vend.company  EQ itemfg.company
                 AND e-itemfg-vend.est-no   EQ w-job-mat.est-no
                 AND e-itemfg-vend.form-no  EQ w-job-mat.frm
                 AND e-itemfg-vend.blank-no EQ w-job-mat.blank-no
               BREAK BY e-itemfg-vend.eqty:
             IF LAST(e-itemfg-vend.eqty)            OR
                e-itemfg-vend.eqty GE w-job-mat.qty THEN DO:
                w-job-mat.eqty = e-itemfg-vend.eqty.
                LEAVE.
             END.
           END.
           FOR EACH e-itemfg-vend NO-LOCK
               WHERE e-itemfg-vend.company  EQ itemfg.company
                 AND e-itemfg-vend.est-no   EQ w-job-mat.est-no
                 AND e-itemfg-vend.eqty     EQ w-job-mat.eqty
                 AND e-itemfg-vend.form-no  EQ w-job-mat.frm
                 AND e-itemfg-vend.blank-no EQ w-job-mat.blank-no:
                 IF NOT CAN-FIND(FIRST tt-eiv
                                 WHERE tt-eiv.company   EQ e-itemfg-vend.company
                                   AND tt-eiv.i-no      EQ w-job-mat.i-no
                                   AND tt-eiv.vend-no   EQ e-itemfg-vend.vend-no) THEN DO:
                    CREATE tt-eiv.
                    
                    ASSIGN
                     tt-eiv.rec-id   = RECID(e-itemfg-vend)
                     tt-eiv.est-no   = ""
                     tt-eiv.i-no     = w-job-mat.i-no
                     tt-eiv.form-no  = 0
                     tt-eiv.blank-no = 0
                     tt-eiv.company = e-itemfg-vend.company
                     tt-eiv.vend-no = e-itemfg-vend.vend-no
                     tt-eiv.vend-i-no = e-itemfg-vend.vend-item
                     tt-eiv.item-type = e-itemfg-vend.item-type
                     tt-eiv.rec_key   = e-itemfg-vend.rec_key.
                   
                    DO v-index = 1 TO 10:
                       ASSIGN
                          tt-eiv.run-qty[v-index] = e-itemfg-vend.run-qty[v-index]
                          tt-eiv.run-cost[v-index] = e-itemfg-vend.run-cost[v-index]
                          tt-eiv.setups[v-index] = e-itemfg-vend.setups[v-index]
                          tt-eiv.roll-w[v-index] = e-itemfg-vend.roll-w[v-index].
                    END. /* do v-index ... */
                   
                    DO v-index = 11 TO 30:
                       tt-eiv.roll-w[v-index] = e-itemfg-vend.roll-w[v-index].
                    END. /* do v-index ... */
                 END. /* can-find(first tt-eiv ... */
           END. /* for each itemfg-vend */
        END. /* w-job-mat.est-no NE "" */
        
        IF NOT CAN-FIND(FIRST tt-eiv) THEN
        FOR EACH e-itemfg-vend OF e-itemfg NO-LOCK:
           IF NOT CAN-FIND(FIRST tt-eiv
                           WHERE tt-eiv.company   EQ e-itemfg-vend.company
                             AND tt-eiv.i-no      EQ e-itemfg-vend.i-no
                             AND tt-eiv.vend-no   EQ e-itemfg-vend.vend-no) THEN DO:
             CREATE tt-eiv.
             ASSIGN
                tt-eiv.rec-id   = RECID(e-itemfg-vend)
                tt-eiv.est-no   = e-itemfg-vend.est-no
                tt-eiv.i-no     = e-itemfg-vend.i-no
                tt-eiv.form-no  = e-itemfg-vend.form-no
                tt-eiv.blank-no = e-itemfg-vend.blank-no
                tt-eiv.company = e-itemfg-vend.company
                tt-eiv.vend-no = e-itemfg-vend.vend-no
                tt-eiv.vend-i-no = e-itemfg-vend.vend-item
                tt-eiv.item-type = e-itemfg-vend.item-type
                tt-eiv.rec_key   = e-itemfg-vend.rec_key.
          
                DO v-index = 1 TO 10:
                   ASSIGN
                      tt-eiv.run-qty[v-index] = e-itemfg-vend.run-qty[v-index]
                      tt-eiv.run-cost[v-index] = e-itemfg-vend.run-cost[v-index]
                      tt-eiv.setups[v-index] = e-itemfg-vend.setups[v-index]
                      tt-eiv.roll-w[v-index] = e-itemfg-vend.roll-w[v-index].
                END.
          
                DO v-index = 11 TO 30:
                   tt-eiv.roll-w[v-index] = e-itemfg-vend.roll-w[v-index].
                END.
           END. /* not can-find .. */
        END. /* each e-itemfg-vend */
    END. /* each e-itemfg */
END PROCEDURE. /* create-tt-eiv */
