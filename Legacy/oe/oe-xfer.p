&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT PARAMETER ip-cust AS CHAR NO-UNDO. 
DEF INPUT PARAMETER ip-sold-id AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-type AS CHAR NO-UNDO. /* order type */
DEF INPUT PARAMETER ip-ord-from AS ROWID NO-UNDO. /* order to copy from */
DEF INPUT PARAMETER ip-item-types AS CHAR NO-UNDO. /* all, components, header */
DEF INPUT PARAMETER ip-use-defaults AS LOG NO-UNDO. /* no prompts when possible */
DEF OUTPUT PARAMETER op-ord-no      AS INT NO-UNDO. /* new order # */

DEFINE VARIABLE h_s-oelib AS HANDLE     NO-UNDO. 
DEFINE VARIABLE h_s-oellib AS HANDLE     NO-UNDO. 

DEFINE VARIABLE r-current-ord AS ROWID NO-UNDO.
DEFINE VARIABLE r-current-ordl AS ROWID NO-UNDO.

RUN oe/oelibupd.p PERSISTENT SET h_s-oelib. 
RUN oe/oelibupl.p PERSISTENT SET h_s-oellib. 

THIS-PROCEDURE:ADD-SUPER-PROCEDURE(h_s-oelib,SEARCH-SELF).
IF VALID-HANDLE(h_s-oelib) THEN
    RUN set-hcaller IN h_s-oelib (INPUT THIS-PROCEDURE).

THIS-PROCEDURE:ADD-SUPER-PROCEDURE(h_s-oelib,SEARCH-SELF).
IF VALID-HANDLE(h_s-oellib) THEN
    RUN set-hcaller IN h_s-oellib (INPUT THIS-PROCEDURE).

DEF TEMP-TABLE tt-item-list
    FIELD i-no AS CHAR
    FIELD price AS DEC
    FIELD qty AS INT.

/* Copied from v-ord for safety */
{custom/globdefs.i}
{oe/ordholdstat.i} 

/* for oecomm.i */
def new shared var v-upd-comm as log initial yes no-undo.
DEF NEW SHARED VAR v-misc AS LOG INIT NO NO-UNDO.
DEF NEW SHARED VAR v-fr-tax LIKE oe-ctrl.f-tax NO-UNDO.
{sys/inc/var.i "new shared" }
def buffer bfx-ord for oe-ord.
def var li-lead-days as int no-undo.
def var ll-f-bill as log no-undo.
def var li-sold-no as int no-undo.
def var ls-ship-i as cha extent 4 no-undo.
def var v-slow-ord as log no-undo.
def var v-beeler as log no-undo.
def var v-ilwalker as log no-undo.
def new shared var v-create-job as log no-undo.
def var v-custype like cust.type no-undo.
def var v-ord-limit like cust.ord-lim no-undo.
def var v-crd-limit like cust.cr-lim no-undo.
def var v-valdcode as cha init "ON,BY,MH" no-undo.
def var v-valtype as cha init "O,R,C" no-undo.
def var v-duelist as cha init "AM,ASAP,BY,CPU,CR,HFR,HOLD,HOT,INK,MH,MUST,NB4,OE,ON,PPR,RWRK,RUSH,TOOL,WO,$$$" no-undo.
def var v-oecount like sys-ctrl.log-fld no-undo.
def var v-full-cost as log no-undo.
def var oeprompt like sys-ctrl.log-fld no-undo.
def var v-quo-price like sys-ctrl.log-fld no-undo.
def var v-inactive as log no-undo.
def var save_id as recid no-undo.
def new shared var fil_id as recid no-undo.
def var v-job-no like oe-ord.job-no no-undo.
def var v-job-no2 like oe-ord.job-no2 no-undo.
def var v-exp-limit as int init 10 no-undo.
def var v-n-ord like oe-ctrl.n-ord no-undo.
def var v-estord-id as recid extent 10 no-undo.
def var v-multord as log no-undo.

{ce/print4.i "new shared"}
{ce/print42.i "new shared"}
def NEW SHARED WORKFILE work-ordl LIKE oe-ordl.
def new shared var nufile as log no-undo.
def new shared buffer xoe-ord for oe-ord.
def new shared var lv-qty as int no-undo.  /* for oe-ordl.qty and oe-ordm calc */
DEF NEW SHARED VAR v-d-rel AS INT NO-UNDO.
def var lv-new-row-id as rowid no-undo.  /* first creation error */
def new shared var v-qty-mod as log no-undo.
def new shared var qty as INT no-undo.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.
def var ll-order-from-est as log no-undo.  /* is order created from estimate */
def var ll-cust-displayed as log no-undo.
def var ll-est-no-mod as log no-undo.
DEF VAR ld-lastship-dec AS DEC NO-UNDO.
DEF VAR ld-lastship-cha AS CHAR NO-UNDO.
DEF VAR ll-valid-po-no AS LOG NO-UNDO.
def var ll-is-new-rec as log no-undo.
DEF VAR ll-from-tandem AS LOG NO-UNDO.
DEF VAR lv-old-cust-no LIKE oe-ord.cust-no NO-UNDO.
DEF VAR ll-new-po AS LOG NO-UNDO.
DEF VAR ll-new-due AS LOG NO-UNDO.
DEF VAR lv-type-codes AS CHAR NO-UNDO.
DEF VAR lv-type-dscrs AS CHAR NO-UNDO.
DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.

DEFINE VARIABLE prodDateChanged AS LOGICAL NO-UNDO.
DEFINE VARIABLE dueDateChanged AS LOGICAL NO-UNDO.
DEFINE VARIABLE scheduleHndl AS HANDLE NO-UNDO.
DEFINE VARIABLE copyRecord AS LOGICAL NO-UNDO.
DEFINE VARIABLE copyRowID AS ROWID NO-UNDO.
DEF VAR v-margin AS DEC NO-UNDO.


def NEW SHARED buffer xest for est.
def NEW SHARED buffer xeb for eb.
def NEW SHARED buffer xef for ef.

DEF BUFFER oe-ord-whs-order FOR reftable.
DEF BUFFER oe-ordl-whs-item FOR reftable.

&Scoped-define sman-fields oe-ord.sman oe-ord.s-pct oe-ord.s-comm

DEF NEW SHARED TEMP-TABLE w-ord NO-UNDO FIELD w-ord-no LIKE oe-ord.ord-no.

DEF TEMP-TABLE old-oe-ord NO-UNDO LIKE oe-ord.

DEF NEW SHARED TEMP-TABLE tt-oe-ordl NO-UNDO LIKE oe-ordl
    FIELD to-be-deleted AS LOG INIT YES
    FIELD row-id AS ROWID
    INDEX row-id row-id. 

ASSIGN
 cocode = g_company
 locode = g_loc.

{oe/tt-item-qty-price.i}

{oe/oe-sysct1.i NEW}
        
  DO TRANSACTION:
    {sys/inc/oedate.i}
    {sys/inc/oecomb.i}
    {sys/inc/job#.i}
    {sys/inc/graphic.i}
    {sys/inc/oeestcom.i}
    {sys/inc/OEPrepTaxCode.i}
  END.
    

/* transaction */
{sys/inc/f16to32.i}

/* transaction */
 {sys/inc/ceprepprice.i} 

RUN sys/ref/ordtypes.p (OUTPUT lv-type-codes, OUTPUT lv-type-dscrs).
/* end copied from v-ord for safety */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF BUFFER bf-ord FOR oe-ord.
DEF BUFFER bf-ordl FOR oe-ordl.

IF VALID-HANDLE(h_s-oelib) THEN
    RUN set-hcaller IN h_s-oelib (INPUT THIS-PROCEDURE).

RUN order-hdr-create (INPUT ip-ord-from, OUTPUT r-current-ord).
DO TRANSACTION:
FIND oe-ord WHERE ROWID(oe-ord) EQ r-current-ord EXCLUSIVE-LOCK.
op-ord-no = oe-ord.ord-no.
END.

IF VALID-HANDLE(h_s-oellib) THEN DO:

    RUN set-hcaller IN h_s-oellib (INPUT THIS-PROCEDURE).
    RUN set-params IN h_s-oellib (INPUT ? /* current oe-ordl */,
                                  INPUT oe-ord.ord-no,
                                  INPUT "ADD",
                                  INPUT YES,
                                  INPUT oe-ord.company,
                                  INPUT oe-ord.loc).
    RUN set-current-oe-ord IN h_s-oellib (INPUT r-current-ord, INPUT r-current-ordl).
END.

/* Begin processing items */
RUN create-item-list.
RUN process-item-list.


DELETE OBJECT h_s-oelib.
DELETE OBJECT h_s-oellib.

/*
    RUN display-fgpart IN h_s-oellib (INPUT RECID(itemfg), INPUT r-current-ord, INPUT r-current-ordl).
    RUN display-fgitem IN h_s-oellib (INPUT r-current-ord, INPUT r-current-ordl).
    RUN final-steps IN h_s-oellib.
    RUN final-steps2 IN h_s-oellib.
*/
/* from history button...
        /* Process new oe-ordl records if multi-select */               
        IF AVAIL b-oe-ordl THEN DO:
          
          RUN create-item.
          FIND bf-new-oe-ordl WHERE RECID(bf-new-oe-ordl) = lv-item-recid
                              EXCLUSIVE-LOCK NO-ERROR.
          bf-new-oe-ordl.i-no = b-oe-ordl.i-no.
          IF lv-on-screen-item NE bf-new-oe-ordl.i-no THEN
            op-rowid-list = op-rowid-list + STRING(ROWID(bf-new-oe-ordl)) + ",".
          
          FIND FIRST itemfg NO-LOCK
                            WHERE itemfg.company EQ cocode
                            AND itemfg.i-no EQ bf-new-oe-ordl.i-no NO-ERROR.
          
          IF AVAIL itemfg THEN
          ASSIGN
            bf-new-oe-ordl.part-no = itemfg.part-no
            bf-new-oe-ordl.i-name = itemfg.i-name
            bf-new-oe-ordl.part-dscr1 = itemfg.part-dscr1
            bf-new-oe-ordl.part-dscr2 = itemfg.part-dscr2.
          
          ASSIGN
            bf-new-oe-ordl.price = b-oe-ordl.price
            bf-new-oe-ordl.pr-uom = b-oe-ordl.pr-uom
            price-ent = YES.
          
          /* IF INTEGER(bf-new-oe-ordl.qty) EQ 0 THEN */
            bf-new-oe-ordl.qty = lv-qty.
          
          IF bf-new-oe-ordl.est-no NE "" 
            AND oeestcom-log = YES THEN
              RUN get-est-comm (INPUT ROWID(bf-new-oe-ordl), INPUT NO).
          
        END. /* if avail */
        

        ELSE NEXT. /* RETURN NO-APPLY. */
        
        IF setFromHistory AND bf-new-oe-ordl.qty EQ 0 THEN
          bf-new-oe-ordl.qty = historyQty.
        
        ASSIGN
          save_id      = RECID(bf-new-oe-ordl)
          v-i-item     = bf-new-oe-ordl.i-no
          v-i-qty      = bf-new-oe-ordl.qty
          price-ent    = NO
          matrixExists = NO.
        
        /* Code modified to operate on bf-new-oe-ordl - bring in multiple from history */
        lr-save-xoeordl-buffer = ?.
        IF AVAIL xoe-ordl THEN
        lr-save-xoeordl-buffer = ROWID(xoe-ordl).
        
        FIND xoe-ordl WHERE rowid(xoe-ordl) EQ ROWID(bf-new-oe-ordl)
          EXCLUSIVE-LOCK NO-ERROR.
        /* Depends on v-i-item */        
        RUN oe/oe-price.p.
*/

/* from ok
  DEF VAR ll-price-mod      AS   LOG  NO-UNDO.
  DEF VAR lv-price          AS   CHAR NO-UNDO.
  DEF VAR ll-pruom-mod      AS   LOG  NO-UNDO.
  DEF VAR lv-pruom          AS   CHAR NO-UNDO.
  DEF VAR lv-prev-req-date  AS   DATE NO-UNDO.
  DEF VAR lv-stat           AS   CHAR NO-UNDO.
  DEF VAR ll                AS   LOG  NO-UNDO.
  DEF VAR ld                AS   DEC  NO-UNDO.
  DEF VAR ll-reopen         AS   LOG  NO-UNDO.
  DEF VAR ll-runship        AS   LOG  NO-UNDO.
  DEF VAR v-job-rec_key     AS   CHAR NO-UNDO.
  DEF VAR v-runsh           AS   INT  NO-UNDO.
  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER b-oe-ord FOR oe-ord.


  DISABLE TRIGGERS FOR LOAD OF xoe-ord.

  /* display spec notes for the item */   
  RUN windows/d-spnote.w (oe-ordl.i-no:SCREEN-VALUE).

  IF ip-type EQ "view" THEN DO:
    APPLY "go" TO FRAME {&FRAME-NAME}.
    RETURN.
  END.

  RUN custom/framechk.p (2, FRAME {&FRAME-NAME}:HANDLE).

  ll-reopen = framechk-i-changed AND oe-ordl.stat EQ "C".

  /* gdm - 10220907 */
  IF TRIM(oe-ordl.pr-uom:SCREEN-VALUE) EQ "" THEN DO:

    MESSAGE "UOM can't be blank. Please enter a valid UOM"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    APPLY "entry" TO oe-ordl.pr-uom.
    RETURN.

  END.
  /* gdm - 10220907 end */

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     v-qty-mod       = oe-ordl.qty NE li-prev-ord-qty
     li-prev-qty     = oe-ordl.qty
     li-prev-ord-qty = oe-ordl.qty
     ll-price-mod    = oe-ordl.price:MODIFIED
     lv-price        = oe-ordl.price:SCREEN-VALUE
     ll-pruom-mod    = oe-ordl.pr-uom:MODIFIED
     lv-pruom        = oe-ordl.pr-uom:SCREEN-VALUE.
  END.

  lv-prev-req-date = oe-ordl.req-date.

  RUN itemfg-cost.

  IF oe-ordl.est-no:SCREEN-VALUE <> "" THEN DO:
     RUN check-quote-qty NO-ERROR.
     IF ERROR-STATUS:ERROR THEN do:
        APPLY 'entry' TO oe-ordl.qty.
        RETURN . 
     END.
  END.

  RUN validate-all NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  IF runship-char EQ "RUN&SHIP Prompt" AND ip-type = "ADD" THEN DO:
    IF oe-ordl.est-no:SCREEN-VALUE GT ""  THEN 
      asi.oe-ordl.whsed:SCREEN-VALUE = "YES".
    ELSE DO:
        ll-runship = LOGICAL(asi.oe-ordl.whsed:SCREEN-VALUE).
  
        RUN oe/d-runsh.w (INPUT ll-runship, OUTPUT v-runsh).
        
        IF v-runsh = 1 THEN
           ASSIGN asi.oe-ordl.whsed:SCREEN-VALUE = "YES".
        IF v-runsh = 2 THEN DO:
           ASSIGN tb_whs-item:SCREEN-VALUE = "YES" tb_whs-item = YES.
        END.        
    END.
    DO TRANSACTION:
      FIND FIRST oe-ordl-whs-item EXCLUSIVE-LOCK
      WHERE oe-ordl-whs-item.reftable EQ "oe-ordl.whs-item"
        AND oe-ordl-whs-item.company  EQ oe-ordl.company
        AND oe-ordl-whs-item.loc      EQ STRING(oe-ordl.ord-no,"9999999999")
        AND oe-ordl-whs-item.code     EQ oe-ordl.i-no
        AND oe-ordl-whs-item.code2    EQ STRING(oe-ordl.line,"9999999999")
      NO-ERROR.

      IF AVAIL oe-ordl-whs-item THEN DO:
         IF tb_whs-item THEN
             oe-ordl-whs-item.val[1] = 1.
         ELSE
             oe-ordl-whs-item.val[1] = 0.
         FIND CURRENT oe-ordl-whs-item NO-LOCK.
      END.
    END.

  END.


  IF oepricecheck-log AND oe-ordl.est-no:SCREEN-VALUE EQ "" AND
     ll-new-record THEN
     RUN prev-quote-proc(INPUT-OUTPUT lv-price,
                         INPUT-OUTPUT lv-pruom).

  DO WITH FRAME {&frame-name}:
    IF ll-price-mod THEN oe-ordl.price:SCREEN-VALUE = lv-price.
    IF ll-pruom-mod THEN oe-ordl.pr-uom:SCREEN-VALUE = lv-pruom.

    {oe/ordltot.i oe-ordl qty oe-ordl}
  END.

  IF ll-reopen THEN DO:
    ll-reopen = NO.
    MESSAGE "This line item is closed, REOPEN?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE ll-reopen.
  END.

  SESSION:SET-WAIT-STATE ("general").

  DO TRANSACTION:
  FIND CURRENT oe-ordl EXCLUSIVE.

  IF ll-reopen THEN oe-ordl.stat = "".

  IF NOT ll-new-record THEN do:
      RUN oe/upinvqty.p (RECID(oe-ordl)).
  END.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}}
           tb_whs-item.
    IF asi.oe-ordl.whsed:HIDDEN = FALSE THEN
        ASSIGN oe-ordl.whsed.

  END.

  RUN whs-item (1).
  find xoe-ord where recid(xoe-ord) = recid(oe-ord) EXCLUSIVE.
  find first itemfg where itemfg.company eq cocode
                      and itemfg.i-no eq oe-ordl.i-no no-lock no-error.
  if avail itemfg then do:
       assign 
        xoe-ord.t-weight = xoe-ord.t-weight - oe-ordl.t-weight
        oe-ordl.t-weight = ( oe-ordl.qty / 100 ) * itemfg.weight-100
        xoe-ord.t-weight = xoe-ord.t-weight + oe-ordl.t-weight.

    /*IF TRIM(oe-ordl.est-no) NE "" AND
       TRIM(xoe-ord.est-no) EQ "" AND
       ll-new-record              THEN
      RUN fg/makenote.p (BUFFER oe-ordl, ?, itemfg.rec_key).*/
  end.
  FIND CURRENT xoe-ord NO-LOCK.

  if lv-change-prom-date then do:  
     for each xoe-ordl where xoe-ordl.company eq g_company
                         and xoe-ordl.ord-no eq oe-ord.ord-no
                        and recid(xoe-ordl) ne recid(oe-ordl):
         assign xoe-ordl.prom-date = oe-ordl.prom-date.
     end.
  end.
  if lv-change-cst-po then do:  
     for each xoe-ordl where xoe-ordl.company eq g_company
                         and xoe-ordl.ord-no eq oe-ord.ord-no
                        and recid(xoe-ordl) ne recid(oe-ordl):
         assign xoe-ordl.po-no = oe-ordl.po-no.
     end.
  end.
  RELEASE xoe-ordl.

  RUN update-itemfg.

  ASSIGN {&list-2} .  /* job-no job-no2 */

  FIND CURRENT oe-ordl NO-LOCK.
  END. /* trans */

  IF ip-type NE "update" AND oe-ordl.est-no NE "" THEN
    RUN oe/ordlmisc.p (ROWID(oe-ordl), oe-ordl.qty).

  IF oereleas-log THEN 
    IF ll-new-record THEN RUN create-release.
                     ELSE RUN update-release.

  DO TRANSACTION:
  FIND CURRENT oe-ordl EXCLUSIVE.
  FIND CURRENT oe-ord EXCLUSIVE.

  RUN final-steps.

  IF ll-new-record THEN DO:
    RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT oe-ordl.t-freight).
    xoe-ord.t-freight = xoe-ord.t-freight + oe-ordl.t-freight.
  END.

  RUN oe/ordfrate.p (ROWID(oe-ord)).

  RUN oe/oe-comm.p.

  RUN oe/calcordt.p (ROWID(oe-ord)).

  IF ld-prev-t-price NE oe-ordl.t-price OR ip-type BEGINS "update-" THEN
     RUN oe/creditck.p (ROWID(oe-ord), YES).

  IF oe-ordl.job-no NE "" THEN
     RUN oe/palchk.p(ROWID(oe-ord), oe-ordl.i-no).

  ld-prev-t-price = oe-ordl.t-price.

  /* gdm - 11090905 */
  IF ip-type EQ "Update" AND
     v-ponoUp THEN DO:

     IF lv-change-cst-po THEN
       FOR EACH job-hdr WHERE
           job-hdr.company EQ oe-ordl.company AND
           job-hdr.job-no  EQ oe-ordl.job-no AND
           job-hdr.job-no2 EQ oe-ordl.job-no2 AND
           job-hdr.ord-no  EQ oe-ordl.ord-no:
         ASSIGN job-hdr.po-no = oe-ordl.po-no.
       END.
     
     ELSE
      FOR EACH job-hdr WHERE
          job-hdr.company EQ oe-ordl.company AND
          job-hdr.job-no  EQ oe-ordl.job-no AND
          job-hdr.job-no2 EQ oe-ordl.job-no2 AND
          job-hdr.ord-no EQ oe-ordl.ord-no AND
          job-hdr.i-no EQ oe-ordl.i-no:
      
          ASSIGN job-hdr.po-no = oe-ordl.po-no.
      END.
      
      RELEASE job-hdr.
  END.
  /* gdm - 11090905 end */

  IF ip-type EQ "Update" AND
     TRIM(oe-ordl.job-no) EQ "" AND
     TRIM(oe-ord.est-no) NE "" THEN
     DO:
        FIND FIRST job-hdr WHERE
             job-hdr.company EQ oe-ordl.company AND
             job-hdr.ord-no EQ oe-ordl.ord-no AND
             job-hdr.i-no EQ oe-ordl.i-no
             NO-LOCK NO-ERROR.

        IF AVAIL job-hdr THEN
        DO:
           ASSIGN
              oe-ordl.job-no = job-hdr.job-no
              oe-ordl.job-no2 = job-hdr.job-no2.

           IF TRIM(oe-ord.job-no) EQ "" THEN
              ASSIGN
                 oe-ord.job-no = job-hdr.job-no
                 oe-ord.job-no2 = job-hdr.job-no2.

           RELEASE job-hdr.
        END.
     END.

  /* end of job update */
  FIND CURRENT oe-ord NO-LOCK.
  FIND CURRENT oe-ordl NO-LOCK.

  IF ll-new-record AND TRIM(v-duplicateFGDayClient) = "DuplicateFGDayClient" THEN DO:
    RUN check-duplicateFGDayClient.
  END.

  END.

  RUN sys/inc/ordlcomp.p (ROWID(oe-ordl)).

  RUN final-steps2.

  /* need to assign oe-ordl.est-type = eb.est-type  
     job */

  ASSIGN
   v-qty-mod         = NO
   lv-add-mode       = NO
   ll-new-fg-created = NO.

  DO WITH FRAME {&frame-name}:
    DISPLAY {&DISPLAYED-FIELDS}.
  END.
      
  DO TRANSACTION:
  FIND CURRENT oe-ordl EXCLUSIVE.
  FIND CURRENT oe-ord EXCLUSIVE.

  /* assign rec_key to oe-ord for notes */

  IF oe-ord.est-no <> "" THEN
  DO:
     /*if notes frozen from jc/jobnotes.p, don't update rec_key*/

     FIND FIRST job-hdr WHERE
          job-hdr.company eq cocode AND
          job-hdr.job-no  EQ oe-ordl.job-no AND
          job-hdr.job-no2 EQ oe-ordl.job-no2
          NO-LOCK NO-ERROR.

     IF AVAIL job-hdr THEN
     DO:
        FIND FIRST job WHERE
             job.company EQ cocode AND
             job.job EQ job-hdr.job AND
             job.job-no EQ job-hdr.job-no AND
             job.job-no2 EQ job-hdr.job-no2
             NO-LOCK NO-ERROR.

        IF AVAIL job THEN
        DO:
           v-job-rec_key = job.rec_key.
           RELEASE job.
        END.

        RELEASE job-hdr.
     END.

     IF oe-ordl.rec_key EQ "" OR
        (v-job-rec_key NE oe-ordl.rec_key) THEN
        oe-ordl.rec_key = est.rec_key.
  END.
    
  
  FIND FIRST b-oe-ordl WHERE  b-oe-ordl.company EQ oe-ordl.company
          AND b-oe-ordl.ord-no  EQ oe-ordl.ord-no
          AND RECID(b-oe-ordl) <> RECID(oe-ordl) NO-LOCK NO-ERROR.
  IF NOT AVAIL b-oe-ordl AND oe-ordl.est-no <> "" THEN DO:
    FIND b-oe-ord WHERE RECID(b-oe-ord) = RECID(oe-ord) EXCLUSIVE.
    b-oe-ord.rec_key = oe-ordl.rec_key.
    RELEASE b-oe-ord.
  END.
  ELSE DO:
     FIND FIRST b-oe-ordl WHERE  b-oe-ordl.company EQ oe-ordl.company
            AND b-oe-ordl.ord-no  EQ oe-ordl.ord-no
            AND RECID(b-oe-ordl) <> RECID(oe-ordl)
            AND b-oe-ordl.est-no <> oe-ordl.est-no
            NO-LOCK NO-ERROR.
     IF NOT AVAIL b-oe-ordl AND oe-ordl.est-no <> "" THEN DO:
         FIND b-oe-ord WHERE RECID(b-oe-ord) = RECID(oe-ord) EXCLUSIVE.
             b-oe-ord.rec_key = oe-ordl.rec_key.
             RELEASE b-oe-ord.
     END.
  END.

  /* end of job update */
  FIND CURRENT oe-ord NO-LOCK.
  FIND CURRENT oe-ordl NO-LOCK.
  END. /* trans */

  DO TRANSACTION:
    FIND CURRENT oe-ord.
   
    RUN oe/ordfrate.p (ROWID(oe-ord)). /* strange problem with freight */

    ll = NO.
    IF AVAIL oe-ord AND oe-ord.due-date GT oe-ordl.req-date THEN
      MESSAGE "Change order header due date to " + TRIM(STRING(oe-ordl.req-date)) "?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.
    IF ll THEN oe-ord.due-date = oe-ordl.req-date.

    FIND CURRENT oe-ord NO-LOCK NO-ERROR.
  END.

  IF (oe-ordl.req-date NE lv-prev-req-date OR ip-type EQ "ADD"
      /*OR ip-type = "UPdate-2" doen in v-ord.w order-from-est proc */)
    /* update job's start-date when req-date is changed */
     AND oe-ordl.est-no:SCREEN-VALUE NE "" /*AND lv-update-job-stdate */ 
     AND (v-run-schedule OR schedule-log)
  THEN RUN update-start-date.

  IF oe-ordl.job-no NE '' THEN RUN update-due-date.

  SESSION:SET-WAIT-STATE ("").

  /*RUN oe/sman-upd.p (ROWID(oe-ordl)).*/

  APPLY "go" TO FRAME {&FRAME-NAME}.

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-assign-name) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-name Procedure 
PROCEDURE assign-name :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM ip-i-no AS CHAR NO-UNDO.
    FIND oe-ordl WHERE ROWID(oe-ordl) = r-current-ordl EXCLUSIVE-LOCK.
    oe-ordl.i-no = ip-i-no.
    FIND FIRST itemfg NO-LOCK
                      WHERE itemfg.company EQ oe-ordl.company
                      AND itemfg.i-no EQ oe-ordl.i-no NO-ERROR.
  
    IF AVAIL itemfg THEN
    ASSIGN
      oe-ordl.part-no     = itemfg.part-no
      oe-ordl.i-name      = itemfg.i-name
      oe-ordl.part-dscr1  = itemfg.part-dscr1
      oe-ordl.part-dscr2  = itemfg.part-dscr2.
    RELEASE oe-ordl.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-item-list) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item-list Procedure 
PROCEDURE create-item-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bf-ord FOR oe-ord.
DEF BUFFER bf-ordl FOR oe-ordl.

FOR FIRST bf-ord WHERE rowid(bf-ord) EQ ip-ord-from NO-LOCK,
    EACH bf-ordl WHERE bf-ordl.company EQ bf-ord.company
                   AND bf-ordl.ord-no  EQ bf-ord.ord-no                   
                 NO-LOCK,
    FIRST itemfg WHERE itemfg.company = bf-ordl.company
                  AND itemfg.i-no EQ bf-ordl.i-no
                  AND itemfg.isaset
                NO-LOCK.
                  
    FOR EACH fg-set WHERE fg-set.set-no EQ itemfg.i-no.
        CREATE tt-item-list.
        ASSIGN tt-item-list.i-no = fg-set.part-no
               tt-item-list.qty  = bf-ordl.qty * fg-set.qtyPerSet
               tt-item-list.price = 0.
    END.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-sv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-sv Procedure 
PROCEDURE get-sv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-field AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER op-value AS CHAR NO-UNDO.
IF NOT ip-field BEGINS "asi" THEN
    ip-field = "asi." + ip-field.
DEF VAR cValue AS CHAR NO-UNDO.

FIND oe-ord WHERE ROWID(oe-ord) = r-current-ord NO-LOCK.

FIND oe-ordl WHERE ROWID(oe-ordl) = r-current-ordl NO-LOCK NO-ERROR.

DO:

CASE ip-field:
WHEN "asi.oe-ord.addr[1]" THEN cValue = string(asi.oe-ord.addr[1]).
WHEN "asi.oe-ord.addr[2]" THEN cValue = string(asi.oe-ord.addr[2]).
WHEN "asi.oe-ord.carrier" THEN cValue = string(asi.oe-ord.carrier).
WHEN "asi.oe-ord.cc-auth" THEN cValue = string(asi.oe-ord.cc-auth).
WHEN "asi.oe-ord.cc-expiration" THEN cValue = string(asi.oe-ord.cc-expiration).
WHEN "asi.oe-ord.cc-num" THEN cValue = string(asi.oe-ord.cc-num).
WHEN "asi.oe-ord.cc-type" THEN cValue = string(asi.oe-ord.cc-type).
WHEN "asi.oe-ord.city" THEN cValue = string(asi.oe-ord.city).
WHEN "asi.oe-ord.contact" THEN cValue = string(asi.oe-ord.contact).
WHEN "asi.oe-ord.cust-name" THEN cValue = string(asi.oe-ord.cust-name).
WHEN "asi.oe-ord.cust-no" THEN cValue = string(asi.oe-ord.cust-no).
WHEN "asi.oe-ord.due-code" THEN cValue = string(asi.oe-ord.due-code).
WHEN "asi.oe-ord.due-date" THEN cValue = string(asi.oe-ord.due-date).
WHEN "asi.oe-ord.est-no" THEN cValue = string(asi.oe-ord.est-no).
WHEN "asi.oe-ord.fob-code" THEN cValue = string(asi.oe-ord.fob-code).
WHEN "asi.oe-ord.frt-pay" THEN cValue = string(asi.oe-ord.frt-pay).
WHEN "asi.oe-ord.job-no" THEN cValue = string(asi.oe-ord.job-no).
WHEN "asi.oe-ord.job-no2" THEN cValue = string(asi.oe-ord.job-no2).
WHEN "asi.oe-ord.last-date" THEN cValue = string(asi.oe-ord.last-date).
WHEN "asi.oe-ord.ord-date" THEN cValue = string(asi.oe-ord.ord-date).
WHEN "asi.oe-ord.ord-no" THEN cValue = string(asi.oe-ord.ord-no).
WHEN "asi.oe-ord.over-pct" THEN cValue = string(asi.oe-ord.over-pct).
WHEN "asi.oe-ord.po-no" THEN cValue = string(asi.oe-ord.po-no).
WHEN "asi.oe-ord.prod-date" THEN cValue = string(asi.oe-ord.prod-date).
WHEN "asi.oe-ord.s-comm[1]" THEN cValue = string(asi.oe-ord.s-comm[1]).
WHEN "asi.oe-ord.s-comm[2]" THEN cValue = string(asi.oe-ord.s-comm[2]).
WHEN "asi.oe-ord.s-comm[3]" THEN cValue = string(asi.oe-ord.s-comm[3]).
WHEN "asi.oe-ord.sman[1]" THEN cValue = string(asi.oe-ord.sman[1]).
WHEN "asi.oe-ord.sman[2]" THEN cValue = string(asi.oe-ord.sman[2]).
WHEN "asi.oe-ord.sman[3]" THEN cValue = string(asi.oe-ord.sman[3]).
WHEN "asi.oe-ord.sname[1]" THEN cValue = string(asi.oe-ord.sname[1]).
WHEN "asi.oe-ord.sname[2]" THEN cValue = string(asi.oe-ord.sname[2]).
WHEN "asi.oe-ord.sname[3]" THEN cValue = string(asi.oe-ord.sname[3]).
WHEN "asi.oe-ord.sold-addr[1]" THEN cValue = string(asi.oe-ord.sold-addr[1]).
WHEN "asi.oe-ord.sold-addr[2]" THEN cValue = string(asi.oe-ord.sold-addr[2]).
WHEN "asi.oe-ord.sold-city" THEN cValue = string(asi.oe-ord.sold-city).
WHEN "asi.oe-ord.sold-id" THEN cValue = string(asi.oe-ord.sold-id).
WHEN "asi.oe-ord.sold-name" THEN cValue = string(asi.oe-ord.sold-name).
WHEN "asi.oe-ord.sold-state" THEN cValue = string(asi.oe-ord.sold-state).
WHEN "asi.oe-ord.sold-zip" THEN cValue = string(asi.oe-ord.sold-zip).
WHEN "asi.oe-ord.spare-char-1" THEN cValue = string(asi.oe-ord.spare-char-1).
WHEN "asi.oe-ord.spare-char-2" THEN cValue = string(asi.oe-ord.spare-char-2).
WHEN "asi.oe-ord.s-pct[1]" THEN cValue = string(asi.oe-ord.s-pct[1]).
WHEN "asi.oe-ord.s-pct[2]" THEN cValue = string(asi.oe-ord.s-pct[2]).
WHEN "asi.oe-ord.s-pct[3]" THEN cValue = string(asi.oe-ord.s-pct[3]).
WHEN "asi.oe-ord.stat" THEN cValue = string(asi.oe-ord.stat).
WHEN "asi.oe-ord.state" THEN cValue = string(asi.oe-ord.state).
WHEN "asi.oe-ord.tax-gr" THEN cValue = string(asi.oe-ord.tax-gr).
WHEN "asi.oe-ord.terms" THEN cValue = string(asi.oe-ord.terms).
WHEN "asi.oe-ord.terms-d" THEN cValue = string(asi.oe-ord.terms-d).
WHEN "asi.oe-ord.under-pct" THEN cValue = string(asi.oe-ord.under-pct).
WHEN "asi.oe-ord.user-id" THEN cValue = string(asi.oe-ord.user-id).
WHEN "asi.oe-ord.zip" THEN cValue = string(asi.oe-ord.zip).
WHEN "asi.oe-ordl.est-no" THEN cValue = string( asi.oe-ordl.est-no).
WHEN "asi.oe-ordl.job-no" THEN cValue = string( asi.oe-ordl.job-no).
WHEN "asi.oe-ordl.job-no2" THEN cValue = string( asi.oe-ordl.job-no2).
WHEN "asi.oe-ordl.qty" THEN cValue = string( asi.oe-ordl.qty).
WHEN "asi.oe-ordl.whsed" THEN cValue = string( asi.oe-ordl.whsed).
WHEN "asi.oe-ordl.i-no" THEN cValue = string( asi.oe-ordl.i-no).
WHEN "asi.oe-ordl.part-no" THEN cValue = string( asi.oe-ordl.part-no).
WHEN "asi.oe-ordl.i-name" THEN cValue = string( asi.oe-ordl.i-name).
WHEN "asi.oe-ordl.part-dscr1" THEN cValue = string( asi.oe-ordl.part-dscr1).
WHEN "asi.oe-ordl.part-dscr2" THEN cValue = string( asi.oe-ordl.part-dscr2).
WHEN "asi.oe-ordl.po-no" THEN cValue = string( asi.oe-ordl.po-no).
WHEN "asi.oe-ordl.e-num" THEN cValue = string( asi.oe-ordl.e-num).
WHEN "asi.oe-ordl.po-no-po" THEN cValue = string( asi.oe-ordl.po-no-po).
WHEN "asi.oe-ordl.vend-no" THEN cValue = string( asi.oe-ordl.vend-no).
WHEN "asi.oe-ordl.price" THEN cValue = string( asi.oe-ordl.price).
WHEN "asi.oe-ordl.pr-uom" THEN cValue = string( asi.oe-ordl.pr-uom).
WHEN "asi.oe-ordl.tax" THEN cValue = string( asi.oe-ordl.tax).
WHEN "asi.oe-ordl.disc" THEN cValue = string( asi.oe-ordl.disc).
WHEN "asi.oe-ordl.cas-cnt" THEN cValue = string( asi.oe-ordl.cas-cnt).
WHEN "asi.oe-ordl.t-price" THEN cValue = string( asi.oe-ordl.t-price).
WHEN "asi.oe-ordl.partial" THEN cValue = string( asi.oe-ordl.partial).
WHEN "asi.oe-ordl.cost" THEN cValue = string( asi.oe-ordl.cost).
WHEN "asi.oe-ordl.cases-unit" THEN cValue = string( asi.oe-ordl.cases-unit).
WHEN "asi.oe-ordl.type-code" THEN cValue = string( asi.oe-ordl.type-code).
WHEN "asi.oe-ordl.s-man[1]" THEN cValue = string( asi.oe-ordl.s-man[1]).
WHEN "asi.oe-ordl.s-pct[1]" THEN cValue = string( asi.oe-ordl.s-pct[1]).
WHEN "asi.oe-ordl.s-comm[1]" THEN cValue = string( asi.oe-ordl.s-comm[1]).
WHEN "asi.oe-ordl.s-man[2]" THEN cValue = string( asi.oe-ordl.s-man[2]).
WHEN "asi.oe-ordl.s-pct[2]" THEN cValue = string( asi.oe-ordl.s-pct[2]).
WHEN "asi.oe-ordl.s-comm[2]" THEN cValue = string( asi.oe-ordl.s-comm[2]).
WHEN "asi.oe-ordl.s-man[3]" THEN cValue = string( asi.oe-ordl.s-man[3]).
WHEN "asi.oe-ordl.s-pct[3]" THEN cValue = string( asi.oe-ordl.s-pct[3]).
WHEN "asi.oe-ordl.s-comm[3]" THEN cValue = string( asi.oe-ordl.s-comm[3]).
WHEN "asi.oe-ordl.over-pct" THEN cValue = string( asi.oe-ordl.over-pct).
WHEN "asi.oe-ordl.under-pct" THEN cValue = string( asi.oe-ordl.under-pct).
WHEN "asi.oe-ordl.req-code" THEN cValue = string( asi.oe-ordl.req-code).
WHEN "asi.oe-ordl.prom-code" THEN cValue = string( asi.oe-ordl.prom-code).
WHEN "asi.oe-ordl.req-date" THEN cValue = string( asi.oe-ordl.req-date).
WHEN "asi.oe-ordl.prom-date" THEN cValue = string( asi.oe-ordl.prom-date).
WHEN "asi.oe-ordl.spare-char-1" THEN cValue = string( asi.oe-ordl.spare-char-1).
WHEN "asi.oe-ordl.part-dscr3" THEN cValue = string( asi.oe-ordl.part-dscr3).
WHEN "asi.oe-ordl.spare-dec-1" THEN cValue = string( asi.oe-ordl.spare-dec-1).
WHEN "asi.oe-ordl.spare-char-2" THEN cValue = string( asi.oe-ordl.spare-char-2).

/* WHEN "fi_prev_order" THEN cValue = string( fi_prev_order. */
/* WHEN "fi_s-comm-lbl" THEN cValue = fi_s-comm-lbl. */
/* WHEN "fi_sman-lbl" THEN cValue = fi_sman-lbl.     */
/* WHEN "fi_sname-lbl" THEN cValue = fi_sname-lbl.   */
/* WHEN "fi_s-pct-lbl" THEN cValue = fi_s-pct-lbl.   */
END CASE.
op-value = cValue.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-order-hdr-create) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE order-hdr-create Procedure 
PROCEDURE order-hdr-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER  ip-orig-ord AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER op-ord-row AS ROWID NO-UNDO.
DEF VAR rOrd AS ROWID NO-UNDO.
DEF VAR rOrdln AS ROWID NO-UNDO.
DEF BUFFER bf-orig-ord FOR oe-ord. 
/*
  DEF OUTPUT PARAMETER op-ord-row AS ROWID NO-UNDO.
  DEF INPUT PARAMETER ip-company AS CHAR NO-UNDO.
  DEF INPUT PARAMETER ip-loc     AS CHAR NO-UNDO.
  DEF INPUT PARAMETER ip-type    AS CHAR NO-UNDO.
*/

RUN lib-create-record (INPUT g_company, INPUT g_loc, INPUT ip-type, ip-orig-ord,
                       OUTPUT rOrd).
ASSIGN r-current-ord = rOrd
       op-ord-row    = rOrd.
FIND bf-orig-ord WHERE ROWID(bf-orig-ord) EQ ip-orig-ord NO-LOCK NO-ERROR.
FIND oe-ord WHERE ROWID(oe-ord) EQ rOrd EXCLUSIVE-LOCK NO-ERROR.
ASSIGN oe-ord.cust-no = ip-cust /* input to main procedure */
       oe-ord.ship-id = ip-sold-id.
IF AVAIL bf-orig-ord THEN
       oe-ord.pord-no = bf-orig-ord.ord-no.
/* Obtains customer related data and displays or assigns */
RUN new-cust-no (INPUT rOrd, INPUT YES).

RUN new-sman (INPUT rOrd, INPUT 1).

RUN new-type (INPUT rOrd).

RUN lib-pre-assign-record (INPUT rOrd).

/* Creates first oe-ordl */
RUN lib-post-assign-record (INPUT rOrd, YES).

/*
FIND FIRST oe-ord WHERE oe-ord.company = '001' AND ord-no = 205746 EXCLUSIVE-LOCK.
rOrd = ROWID(oe-ord).
r-current-ord = rOrd.
/* order line create */
RUN create-item (INPUT rOrd, OUTPUT rOrdln). 

FIND oe-ordl WHERE ROWID(oe-ordl) EQ rOrdln EXCLUSIVE-LOCK.

RUN display-fgpart (INPUT recid(itemfg), INPUT rOrd, INPUT rOrdln).

RUN display-fgitem (INPUT rOrd).
  */




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-process-item-list) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-item-list Procedure 
PROCEDURE process-item-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND oe-ord WHERE ROWID(oe-ord) EQ r-current-ord.
FOR EACH tt-item-list:

    FIND itemfg WHERE itemfg.company = oe-ord.company AND 
        itemfg.i-no = tt-item-list.i-no NO-LOCK NO-ERROR.
    IF NOT AVAIL itemfg THEN
        NEXT.

    RUN create-item IN h_s-oellib (INPUT r-current-ord, 
                                   INPUT tt-item-list.qty, 
                                   OUTPUT r-current-ordl).
    FIND oe-ordl WHERE ROWID(oe-ordl) = r-current-ordl NO-LOCK NO-ERROR.

    RUN set-current-oe-ord IN h_s-oellib (INPUT r-current-ord, INPUT r-current-ordl).    
    FIND oe-ordl WHERE ROWID(oe-ordl) = r-current-ordl NO-LOCK.
    /* oe-ordl.i-no = tt-item-list.i-no. */


    RUN assign-name (tt-item-list.i-no).
    FIND oe-ordl WHERE ROWID(oe-ordl) = r-current-ordl NO-LOCK.
   
    FIND FIRST itemfg NO-LOCK
                      WHERE itemfg.company EQ oe-ordl.company
                      AND itemfg.i-no EQ oe-ordl.i-no NO-ERROR.
  
   
    FIND oe-ordl WHERE ROWID(oe-ordl) = r-current-ordl NO-LOCK.
    RUN display-fgpart IN h_s-oellib (INPUT RECID(itemfg), INPUT r-current-ord, INPUT r-current-ordl).
    FIND oe-ordl WHERE ROWID(oe-ordl) = r-current-ordl NO-LOCK.
    RUN display-fgitem IN h_s-oellib (INPUT r-current-ord, INPUT r-current-ordl).

    /* RUN display-item. */
    RUN CHOOSE_btn_go IN h_s-oellib (INPUT r-current-ord, INPUT r-current-ordl, YES).

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-order) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-order Procedure 
PROCEDURE set-order :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipr-ord-row AS ROWID.
r-current-ord = ipr-ord-row.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-sv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-sv Procedure 
PROCEDURE set-sv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-field AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-value AS CHAR NO-UNDO.

IF NOT ip-field BEGINS "asi" THEN
    ip-field = "asi." + ip-field.
DO:
FIND oe-ord WHERE ROWID(oe-ord) = r-current-ord EXCLUSIVE-LOCK.
IF r-current-ordl NE ? THEN
    FIND oe-ordl WHERE ROWID(oe-ordl) = r-current-ordl EXCLUSIVE-LOCK.
CASE ip-field:
WHEN "asi.oe-ord.addr[1]" THEN asi.oe-ord.addr[1] = ip-value.
WHEN "asi.oe-ord.addr[2]" THEN asi.oe-ord.addr[2] = ip-value. 
WHEN "asi.oe-ord.carrier" THEN asi.oe-ord.carrier = ip-value.
WHEN "asi.oe-ord.cc-auth" THEN asi.oe-ord.cc-auth = ip-value.
WHEN "asi.oe-ord.cc-expiration" THEN asi.oe-ord.cc-expiration = date(ip-value).
WHEN "asi.oe-ord.cc-num" THEN asi.oe-ord.cc-num = ip-value.
WHEN "asi.oe-ord.cc-type" THEN asi.oe-ord.cc-type = ip-value.
WHEN "asi.oe-ord.city" THEN asi.oe-ord.city = ip-value.
WHEN "asi.oe-ord.contact" THEN asi.oe-ord.contact = ip-value.
WHEN "asi.oe-ord.cust-name" THEN asi.oe-ord.cust-name = ip-value.
WHEN "asi.oe-ord.cust-no" THEN asi.oe-ord.cust-no = ip-value.
WHEN "asi.oe-ord.due-code" THEN asi.oe-ord.due-code = ip-value.
WHEN "asi.oe-ord.due-date" THEN asi.oe-ord.due-date = date(ip-value).
WHEN "asi.oe-ord.est-no" THEN asi.oe-ord.est-no = ip-value.
WHEN "asi.oe-ord.fob-code" THEN asi.oe-ord.fob-code = ip-value.
WHEN "asi.oe-ord.frt-pay" THEN asi.oe-ord.frt-pay = ip-value.
WHEN "asi.oe-ord.job-no" THEN asi.oe-ord.job-no = ip-value.
WHEN "asi.oe-ord.job-no2" THEN asi.oe-ord.job-no2 = integer(ip-value).
WHEN "asi.oe-ord.last-date" THEN asi.oe-ord.last-date = date(ip-value).
WHEN "asi.oe-ord.ord-date" THEN asi.oe-ord.ord-date = date(ip-value).
WHEN "asi.oe-ord.ord-no" THEN asi.oe-ord.ord-no = integer(ip-value).
WHEN "asi.oe-ord.over-pct" THEN asi.oe-ord.over-pct = dec(ip-value).
WHEN "asi.oe-ord.po-no" THEN asi.oe-ord.po-no = ip-value.
WHEN "asi.oe-ord.prod-date" THEN asi.oe-ord.prod-date = date(ip-value).
WHEN "asi.oe-ord.s-comm[1]" THEN asi.oe-ord.s-comm[1] = dec(ip-value).
WHEN "asi.oe-ord.s-comm[2]" THEN asi.oe-ord.s-comm[2] = dec(ip-value).
WHEN "asi.oe-ord.s-comm[3]" THEN asi.oe-ord.s-comm[3] = dec(ip-value).
WHEN "asi.oe-ord.sman[1]" THEN asi.oe-ord.sman[1] = ip-value.
WHEN "asi.oe-ord.sman[2]" THEN asi.oe-ord.sman[2] = ip-value.
WHEN "asi.oe-ord.sman[3]" THEN asi.oe-ord.sman[3] = ip-value.
WHEN "asi.oe-ord.sname[1]" THEN asi.oe-ord.sname[1] = ip-value.
WHEN "asi.oe-ord.sname[2]" THEN asi.oe-ord.sname[2] = ip-value.
WHEN "asi.oe-ord.sname[3]" THEN asi.oe-ord.sname[3] = ip-value.
WHEN "asi.oe-ord.sold-addr[1]" THEN asi.oe-ord.sold-addr[1] = ip-value.
WHEN "asi.oe-ord.sold-addr[2]" THEN asi.oe-ord.sold-addr[2] = ip-value.
WHEN "asi.oe-ord.sold-city" THEN asi.oe-ord.sold-city = ip-value.
WHEN "asi.oe-ord.sold-id" THEN asi.oe-ord.sold-id = ip-value.
WHEN "asi.oe-ord.sold-name" THEN asi.oe-ord.sold-name = ip-value.
WHEN "asi.oe-ord.sold-state" THEN asi.oe-ord.sold-state = ip-value.
WHEN "asi.oe-ord.sold-zip" THEN asi.oe-ord.sold-zip = ip-value.
WHEN "asi.oe-ord.spare-char-1" THEN asi.oe-ord.spare-char-1 = ip-value.
WHEN "asi.oe-ord.spare-char-2" THEN asi.oe-ord.spare-char-2 = ip-value.
WHEN "asi.oe-ord.s-pct[1]" THEN asi.oe-ord.s-pct[1] = dec(ip-value).
WHEN "asi.oe-ord.s-pct[2]" THEN asi.oe-ord.s-pct[2] = dec(ip-value).
WHEN "asi.oe-ord.s-pct[3]" THEN asi.oe-ord.s-pct[3] = dec(ip-value).
WHEN "asi.oe-ord.stat" THEN asi.oe-ord.stat = ip-value.
WHEN "asi.oe-ord.state" THEN asi.oe-ord.state = ip-value.
WHEN "asi.oe-ord.tax-gr" THEN asi.oe-ord.tax-gr = ip-value.
WHEN "asi.oe-ord.terms" THEN asi.oe-ord.terms = ip-value.
WHEN "asi.oe-ord.terms-d" THEN asi.oe-ord.terms-d = ip-value.
WHEN "asi.oe-ord.under-pct" THEN asi.oe-ord.under-pct = dec(ip-value).
WHEN "asi.oe-ord.user-id" THEN asi.oe-ord.user-id = ip-value.
WHEN "asi.oe-ord.zip" THEN asi.oe-ord.zip = ip-value.
WHEN "asi.oe-ordl.est-no" THEN asi.oe-ordl.est-no = string(ip-value).
WHEN "asi.oe-ordl.job-no" THEN asi.oe-ordl.job-no = string(ip-value).
WHEN "asi.oe-ordl.job-no2" THEN asi.oe-ordl.job-no2 = integer(ip-value).
WHEN "asi.oe-ordl.qty" THEN asi.oe-ordl.qty = decimal(ip-value).
WHEN "asi.oe-ordl.whsed" THEN asi.oe-ordl.whsed = logical(ip-value).
WHEN "asi.oe-ordl.i-no" THEN asi.oe-ordl.i-no = string(ip-value).
WHEN "asi.oe-ordl.part-no" THEN asi.oe-ordl.part-no = string(ip-value).
WHEN "asi.oe-ordl.i-name" THEN asi.oe-ordl.i-name = string(ip-value).
WHEN "asi.oe-ordl.part-dscr1" THEN asi.oe-ordl.part-dscr1 = string(ip-value).
WHEN "asi.oe-ordl.part-dscr2" THEN asi.oe-ordl.part-dscr2 = string(ip-value).
WHEN "asi.oe-ordl.po-no" THEN asi.oe-ordl.po-no = string(ip-value).
WHEN "asi.oe-ordl.e-num" THEN asi.oe-ordl.e-num = integer(ip-value).
WHEN "asi.oe-ordl.po-no-po" THEN asi.oe-ordl.po-no-po = integer(ip-value).
WHEN "asi.oe-ordl.vend-no" THEN asi.oe-ordl.vend-no = string(ip-value).
WHEN "asi.oe-ordl.price" THEN asi.oe-ordl.price = decimal(ip-value).
WHEN "asi.oe-ordl.pr-uom" THEN asi.oe-ordl.pr-uom = string(ip-value).
WHEN "asi.oe-ordl.tax" THEN asi.oe-ordl.tax = logical(ip-value).
WHEN "asi.oe-ordl.disc" THEN asi.oe-ordl.disc = decimal(ip-value).
WHEN "asi.oe-ordl.cas-cnt" THEN asi.oe-ordl.cas-cnt = integer(ip-value).
WHEN "asi.oe-ordl.t-price" THEN asi.oe-ordl.t-price = decimal(ip-value).
WHEN "asi.oe-ordl.partial" THEN asi.oe-ordl.partial = decimal(ip-value).
WHEN "asi.oe-ordl.cost" THEN asi.oe-ordl.cost = decimal(ip-value).
WHEN "asi.oe-ordl.cases-unit" THEN asi.oe-ordl.cases-unit = integer(ip-value).
WHEN "asi.oe-ordl.type-code" THEN asi.oe-ordl.type-code = string(ip-value).
WHEN "asi.oe-ordl.s-man[1]" THEN asi.oe-ordl.s-man[1] = STRING(ip-value).
WHEN "asi.oe-ordl.s-pct[1]" THEN asi.oe-ordl.s-pct[1] = DECIMAL(ip-value).
WHEN "asi.oe-ordl.s-comm[1]" THEN asi.oe-ordl.s-comm[1] = DECIMAL(ip-value).
WHEN "asi.oe-ordl.s-man[2]" THEN asi.oe-ordl.s-man[2] = STRING(ip-value).
WHEN "asi.oe-ordl.s-pct[2]" THEN asi.oe-ordl.s-pct[2] = DECIMAL(ip-value).
WHEN "asi.oe-ordl.s-comm[2]" THEN asi.oe-ordl.s-comm[2] = DECIMAL(ip-value).
WHEN "asi.oe-ordl.s-man[3]" THEN asi.oe-ordl.s-man[3] = STRING(ip-value).
WHEN "asi.oe-ordl.s-pct[3]" THEN asi.oe-ordl.s-pct[3] = DECIMAL(ip-value).
WHEN "asi.oe-ordl.s-comm[3]" THEN asi.oe-ordl.s-comm[3] = DECIMAL(ip-value).
WHEN "asi.oe-ordl.over-pct" THEN asi.oe-ordl.over-pct = decimal(ip-value).
WHEN "asi.oe-ordl.under-pct" THEN asi.oe-ordl.under-pct = decimal(ip-value).
WHEN "asi.oe-ordl.req-code" THEN asi.oe-ordl.req-code = string(ip-value).
WHEN "asi.oe-ordl.prom-code" THEN asi.oe-ordl.prom-code = string(ip-value).
WHEN "asi.oe-ordl.req-date" THEN asi.oe-ordl.req-date = date(ip-value).
WHEN "asi.oe-ordl.prom-date" THEN asi.oe-ordl.prom-date = date(ip-value).
WHEN "asi.oe-ordl.spare-char-1" THEN asi.oe-ordl.spare-char-1 = string(ip-value).
WHEN "asi.oe-ordl.part-dscr3" THEN asi.oe-ordl.part-dscr3 = string(ip-value).
WHEN "asi.oe-ordl.spare-dec-1" THEN asi.oe-ordl.spare-dec-1 = decimal(ip-value).
WHEN "asi.oe-ordl.spare-char-2" THEN asi.oe-ordl.spare-char-2 = string(ip-value).

/* WHEN "fi_prev_order" THEN fi_prev_order = ip-value. */
/* WHEN "fi_s-comm-lbl" THEN fi_s-comm-lbl = ip-value. */
/* WHEN "fi_sman-lbl" THEN fi_sman-lbl = ip-value.     */
/* WHEN "fi_sname-lbl" THEN fi_sname-lbl = ip-value.   */
/* WHEN "fi_s-pct-lbl" THEN fi_s-pct-lbl = ip-value.   */
END CASE.

END.
IF AVAIL oe-ordl THEN
    RELEASE oe-ordl.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

