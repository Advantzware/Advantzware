

/*------------------------------------------------------------------------
    File        : ss_lodtg.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}

    DEFINE TEMP-TABLE ttSharpShooterLoadTag NO-UNDO
        FIELD ldtag  AS CHAR
        FIELD ldtag2 AS CHAR
        FIELD ord-no           AS CHAR  
        FIELD job-no           AS CHAR  
        FIELD job-no2          AS INT   
        FIELD i-no             AS CHAR  
        FIELD ord-qty          AS INT   
        FIELD over-pct         AS DEC   
        FIELD pcs              AS INT   
        FIELD bundle           AS INT   
        FIELD total-unit       AS INT   
        FIELD total-tags       AS INT  
        FIELD partial          AS INT   
        FIELD unit-wt          AS DEC   
        FIELD pallt-wt         AS DEC   
        FIELD lot              AS CHAR  
        FIELD i-name           AS CHAR  
        FIELD cust-po-no       AS CHAR 
        FIELD tagno            AS CHAR 
        FIELD fileval          AS CHAR 
        .
        

DEFINE DATASET dsSharpShooterLoadTag FOR ttSharpShooterLoadTag.
    DEFINE INPUT PARAMETER  prmUser         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOrd_lst      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmJob_lst      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBeg_ord      AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmEnd_ord      AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmBeg_job      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBeg_job2     AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmEnd_job      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEnd_job2     AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmBeg_itm      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEnd_itm      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmJobPur_rct   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmReturn       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmReprnt_tg    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmScnCs_lbl    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOrdJb_stat   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPrntPo_frm   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmFrm_dt       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmTo_dt        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPrntSt_comp  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmTrnsRls_lot  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPrntDpt_not  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPrntPst_bol  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPrntShp_id   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmIncl_Over    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmLwd_16th     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAuto_prnt    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmFrez_Chos    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmLabl_matrx   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmLabl_pallt   AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmPrnt_form    AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmText_file    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmDpt_lst      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmShp_id       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmExtra        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  ip_rowid        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  extra           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmReckey       AS CHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER cError          AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsSharpShooterLoadTag.

     IF prmUser        = ? THEN ASSIGN     prmUser         = "".   
     IF prmAction      = ? THEN ASSIGN     prmAction       = "". 
     IF prmOrd_lst     = ? THEN ASSIGN     prmOrd_lst      = "". 
     IF prmJob_lst     = ? THEN ASSIGN     prmJob_lst      = "". 
     IF prmBeg_ord     = ? THEN ASSIGN     prmBeg_ord      = 0.  
     IF prmEnd_ord     = ? THEN ASSIGN     prmEnd_ord      = 0.  
     IF prmBeg_job     = ? THEN ASSIGN     prmBeg_job      = "". 
     IF prmBeg_job2    = ? THEN ASSIGN     prmBeg_job2     = 0. 
     IF prmEnd_job     = ? THEN ASSIGN     prmEnd_job      = "". 
     IF prmEnd_job2    = ? THEN ASSIGN     prmEnd_job2     = 0. 
     IF prmBeg_itm     = ? THEN ASSIGN     prmBeg_itm      = "". 
     IF prmEnd_itm     = ? THEN ASSIGN     prmEnd_itm      = "". 
     IF prmJobPur_rct  = ? THEN ASSIGN     prmJobPur_rct   = "". 
     IF prmReturn      = ? THEN ASSIGN     prmReturn       = "".
     IF prmReprnt_tg   = ? THEN ASSIGN     prmReprnt_tg    = "".
     IF prmScnCs_lbl   = ? THEN ASSIGN     prmScnCs_lbl    = "".   
     IF prmOrdJb_stat  = ? THEN ASSIGN     prmOrdJb_stat   = "". 
     IF prmPrntPo_frm  = ? THEN ASSIGN     prmPrntPo_frm   = "". 
     IF prmFrm_dt      = ? THEN ASSIGN     prmFrm_dt       = "". 
     IF prmTo_dt       = ? THEN ASSIGN     prmTo_dt        = "".  
     IF prmPrntSt_comp = ? THEN ASSIGN     prmPrntSt_comp  = "".  
     IF prmTrnsRls_lot = ? THEN ASSIGN     prmTrnsRls_lot  = "". 
     IF prmPrntDpt_not = ? THEN ASSIGN     prmPrntDpt_not  = "". 
     IF prmPrntPst_bol = ? THEN ASSIGN     prmPrntPst_bol  = "". 
     IF prmPrntShp_id  = ? THEN ASSIGN     prmPrntShp_id   = "". 
     IF prmIncl_Over   = ? THEN ASSIGN     prmIncl_Over    = "". 
     IF prmLwd_16th    = ? THEN ASSIGN     prmLwd_16th     = "". 
     IF prmAuto_prnt   = ? THEN ASSIGN     prmAuto_prnt    = "". 
     IF prmFrez_Chos   = ? THEN ASSIGN     prmFrez_Chos    = "".
     IF prmLabl_matrx  = ? THEN ASSIGN     prmLabl_matrx   = "".
     IF prmLabl_pallt  = ? THEN ASSIGN     prmLabl_pallt   = 0.   
     IF prmPrnt_form   = ? THEN ASSIGN     prmPrnt_form    = 0. 
     IF prmText_file   = ? THEN ASSIGN     prmText_file    = "". 
     IF prmDpt_lst     = ? THEN ASSIGN     prmDpt_lst      = "". 
     IF prmShp_id      = ? THEN ASSIGN     prmShp_id       = "".  
     IF prmExtra       = ? THEN ASSIGN     prmExtra        = "".  
     IF prmOut         = ? THEN ASSIGN     prmOut          = "". 
     IF prmReckey      = ? THEN ASSIGN     prmReckey       = "".
     IF extra          = ? THEN ASSIGN     extra           = "".
     




DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE NEW SHARED VAR gcompany AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.


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


assign
 cocode = prmComp 
 v-today = TODAY 
 g_company = cocode
 g_user    = prmUser
 gcompany = cocode  .



FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .



DEF var save_id AS RECID.

DEF var time_stamp AS ch.
ASSIGN time_stamp = string(TIME, "hh:mmam").

DEF var v-ford-no AS int FORMAT ">>>>>>" extent 2 no-undo.
def var v-orders as char format "x(78)" extent 10.
DEF var v-fitem AS char FORMAT "x(15)" extent 2 init ["","zzzzzzzzzzzzzzz"].
DEF var v-po-no-source AS char FORMAT "!" init "R".
def var v-stat as char format "!" init "O".

DEF var v-out AS char FORMAT "x(40)" NO-UNDO.
DEF var v-job AS char FORMAT "x(9)" NO-UNDO.
DEF var num-rec AS int init 0 NO-UNDO.
DEF var by-release AS log init NO NO-UNDO.
DEF VAR lv-rd_print  AS CHAR NO-UNDO.

/* 9812 CAH: */
DEF VAR v-loadtag       AS CHAR NO-UNDO INIT "ASI".  /* sys ctrl option */
DEF VAR v-mult          AS INT  NO-UNDO INIT 0.  /* sys ctrl option */
DEF VAR v-cas-lab       AS LOG  NO-UNDO.  /* sys ctrl option */
DEF VAR v-tags          AS DEC  NO-UNDO INIT 0.  /* sys ctrl option */
DEF VAR v-count         AS INT  NO-UNDO INIT 0.
DEF VAR v-fgrecpt       AS LOG  NO-UNDO.  /* sys ctrl option */
DEF VAR create-count AS INT .
ASSIGN
    create-count = 0 .

/* mdp var used for posting to finish goods */

DEF VAR lv-r-no LIKE rm-rctd.r-no NO-UNDO.

/* 9812 CAH: Variables for Intermec Support */
def var stx as char format 'x(01)' no-undo initial "~002".
def var etx as char format 'x(01)' no-undo initial "~003".
def var esc as char format 'x(01)' no-undo initial "~033".
def var etb as char format 'x(01)' no-undo initial "~027".
def var cr  as char format 'x(01)' no-undo initial "~015".
def var can as char format 'x(01)' no-undo initial "~030".
def var rs  as char format 'x(01)' no-undo initial "~036".
def var us  as char format 'x(01)' no-undo initial "~037".

def stream s-form.
def stream s-bar.

def var form_fid        as char no-undo initial "barcode.frm" FORMAT "X(40)".
def var form#           as int  no-undo format "9" initial 3.
def var char_units      as char no-undo.
def var copy_count      as int no-undo initial 2.
def var n               as int no-undo initial 0.
DEF VAR var-display-warning AS LOG NO-UNDO.
def var fg-uom-list  as char NO-UNDO.

/* Vars for create-text-file */
DEF VAR lv-text AS cha NO-UNDO.
DEF VAR v-dept-note AS cha FORM "x(80)" EXTENT 18 NO-UNDO.
DEF VAR lv-middlesex-job AS CHAR FORMAT "x(9)" NO-UNDO.
DEF VAR lv-middlesex-po AS CHAR FORMAT "x(9)" NO-UNDO.
DEF VAR lv-tag-no AS INT NO-UNDO.
DEF VAR lv-how-many-tags AS INT NO-UNDO.
DEF VAR lv-total-tags AS INT NO-UNDO.
DEF VAR str-tit AS CHAR NO-UNDO.
DEF VAR coname AS CHAR NO-UNDO.
DEF VAR loname AS CHAR NO-UNDO.
DEF VAR str-tit2 AS CHAR NO-UNDO.

/* gdm - 10160905*/
DEF VAR v-fgdsc1 LIKE itemfg.part-dscr1 NO-UNDO.
DEF VAR v-fgdsc2 LIKE itemfg.part-dscr2 NO-UNDO.
DEF VAR v-fgdsc3 LIKE itemfg.part-dscr3 NO-UNDO.

DEF TEMP-TABLE tt-ordjobs
    FIELD job-no LIKE job.job-no
    FIELD job-no2 LIKE job.job-no2.

def workfile w-file field w-key AS ROWID.
DEF TEMP-TABLE tt-tag FIELD tt-recid AS RECID.
DEF WORKFILE w-shipto LIKE shipto
                      FIELD stat AS CHAR
                      FIELD row-id AS ROWID.

DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER ref-lot-no FOR reftable.

DEFINE TEMP-TABLE ttblJob NO-UNDO
  FIELD company AS CHARACTER
  FIELD job-no AS CHARACTER
  FIELD job-no2 AS INTEGER
  FIELD ord-no AS INTEGER
    INDEX ttblJob IS PRIMARY UNIQUE
      company job-no job-no2 ord-no
    INDEX ord-no company ord-no.

{oerep/r-loadtg.i NEW}

{fg/fullset.i NEW}

ASSIGN  
  tmpstore = FILL("_",50).



DEF VAR lv-ok-ran AS LOG NO-UNDO.
{custom/formtext.i NEW}

DEF WORKFILE w-fg-rctd LIKE fg-rctd FIELD row-id   AS ROWID
                                    FIELD invoiced AS LOG INIT NO.

{fg/fg-post3.i NEW}
DEF VAR v-fgpostgl AS CHAR NO-UNDO.
{jc/jcgl-sh.i NEW}

DEFINE VARIABLE ordNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobNo2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNo AS CHARACTER NO-UNDO.

/* gdm - 04090909 */
DEF VAR v-barflg AS LOG NO-UNDO.
DEF VAR v-auto-print AS LOG NO-UNDO.
DEF VAR UserlabelPath AS cha NO-UNDO.

/* gdm - 06100901 */
DEF VAR v-txtflg AS LOG NO-UNDO.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

DO TRANSACTION:
   {sys/inc/fgpofrt.i}
   {sys/inc/fgrecpt.i} /* gdm - 12010901*/
   {sys/inc/rfidtag.i}
END.

/* gdm - 09210907 */
DEF VAR v-bardir AS LOG NO-UNDO.
DEF VAR v-bardir-chr AS CHAR NO-UNDO.
DEF BUFFER bf-oe-ord  FOR oe-ord.
DEF BUFFER bf-oe-ordl FOR oe-ordl.

/* gdm - 0930916 */
DEF BUFFER bf-po-ord  FOR po-ord.
DEF BUFFER bf-po-ordl FOR po-ordl.

DEF BUFFER bf-jobhdr FOR job-hdr.
 DEF VAR v-weight-100 LIKE itemfg.weight-100 NO-UNDO.

 DEF TEMP-TABLE tt-loadtag NO-UNDO                                                       
        FIELD ord-no           AS CHAR 
        FIELD cust-no          AS CHAR
        FIELD job-no           AS CHAR  
        FIELD job-no2          AS INT 
        FIELD i-no             AS CHAR  
        FIELD ord-qty          AS INT   
        FIELD over-pct         AS DEC   
        FIELD pcs              AS INT   
        FIELD bundle           AS INT   
        FIELD total-unit       AS INT   
        FIELD total-tags       AS INT  
        FIELD partial          AS INT   
        FIELD unit-wt          AS DEC   
        FIELD pallt-wt         AS DEC   
        FIELD lot              AS CHAR  
        FIELD i-name           AS CHAR  
        FIELD cust-po-no       AS CHAR
        FIELD tagrowid          AS CHAR  .
                            

/*DEF WORKFILE w-fg-rctd LIKE fg-rctd FIELD row-id   AS ROWID
                                    FIELD invoiced AS LOG INIT NO.*/

/*{fg/fg-post3.i NEW}
DEF VAR v-fgpostgl AS CHAR NO-UNDO.
{jc/jcgl-sh.i NEW}   */

/*DEFINE VARIABLE ordNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobNo2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNo AS CHARACTER NO-UNDO.
/* gdm - 04090909 */
DEF VAR v-barflg AS LOG NO-UNDO.
DEF VAR v-auto-print AS LOG NO-UNDO.
DEF VAR UserlabelPath AS cha NO-UNDO.    */

/* gdm - 06100901 */


/*RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).*/


    DEFINE VARIABLE iReturnResult AS INTEGER NO-UNDO.
      DEFINE VARIABLE cProgramName AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.


      DEFINE VARIABLE v-job-list AS CHARACTER NO-UNDO.
      DEFINE VARIABLE v-ord-list AS CHARACTER NO-UNDO.
      DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
      DEFINE VARIABLE begin_filename AS CHARACTER FORMAT "X(256)":U INITIAL "ccc" NO-UNDO.
      DEFINE VARIABLE begin_form AS INTEGER FORMAT ">>>":U INITIAL 3 NO-UNDO.
      DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U NO-UNDO.
      DEFINE VARIABLE begin_job AS CHARACTER FORMAT "X(6)":U NO-UNDO.
      DEFINE VARIABLE begin_job2 AS INTEGER FORMAT "99":U INITIAL 0 NO-UNDO.
      DEFINE VARIABLE begin_labels AS INTEGER FORMAT ">>>>":U INITIAL 2 NO-UNDO.
      DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 NO-UNDO.
      DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 NO-UNDO.
      DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
      DEFINE VARIABLE end_job AS CHARACTER FORMAT "X(6)":U NO-UNDO.
      DEFINE VARIABLE end_job2 AS INTEGER FORMAT "99":U INITIAL 99 NO-UNDO.
      DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 NO-UNDO.
      DEFINE VARIABLE fi_cas-lab AS CHARACTER FORMAT "X(30)":U NO-UNDO.
      DEFINE VARIABLE lbl_po-no AS CHARACTER FORMAT "X(256)":U INITIAL "Print PO from:" NO-UNDO.
      DEFINE VARIABLE scr-label-file AS CHARACTER FORMAT "X(256)":U NO-UNDO.
      DEFINE VARIABLE statusLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Order/Job Status:" NO-UNDO.
      DEFINE VARIABLE typeLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Enter Orders separated by comma" NO-UNDO.
      DEFINE VARIABLE v-dept-list AS CHARACTER FORMAT "X(256)":U NO-UNDO.
      DEFINE VARIABLE v-ship-id AS CHARACTER FORMAT "X(256)":U NO-UNDO.
      DEFINE VARIABLE loadtagFunction AS CHARACTER INITIAL "Order" NO-UNDO.
      DEFINE VARIABLE rd_comps AS CHARACTER INITIAL "B" NO-UNDO.
      DEFINE VARIABLE rd_order-sts AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rd_print AS CHARACTER INITIAL "H" NO-UNDO.
      DEFINE VARIABLE scr-auto-print AS LOGICAL INITIAL NO NO-UNDO.
      DEFINE VARIABLE scr-freeze-label AS LOGICAL INITIAL NO NO-UNDO.
      DEFINE VARIABLE tb_16ths AS LOGICAL INITIAL NO NO-UNDO.
      DEFINE VARIABLE tb_dept-note AS LOGICAL INITIAL NO NO-UNDO.
      DEFINE VARIABLE tb_over AS LOGICAL INITIAL NO NO-UNDO.
      DEFINE VARIABLE tb_rel AS LOGICAL INITIAL NO NO-UNDO.
      DEFINE VARIABLE tb_reprint-tag AS LOGICAL INITIAL NO NO-UNDO.
      DEFINE VARIABLE tb_ret AS LOGICAL INITIAL no NO-UNDO.
      DEFINE VARIABLE tb_ship-id AS LOGICAL INITIAL NO NO-UNDO.
      DEFINE VARIABLE tb_xfer-lot AS LOGICAL INITIAL NO NO-UNDO. 




  /*main Block*/ 

  FIND FIRST company WHERE company.company EQ gcompany NO-LOCK.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "CEMENU"
      NO-LOCK NO-ERROR.
  ASSIGN
   tb_16ths  = AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "Corrware".

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company eq gcompany
        AND sys-ctrl.name    eq "LOADTAG"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = gcompany
     sys-ctrl.name     = "LOADTAG"
     sys-ctrl.descrip  = "Special Load tag print options, e.g. barcode printer"
     sys-ctrl.char-fld = "ASI".
    FIND CURRENT sys-ctrl NO-LOCK.
  END.

  ASSIGN v-loadtag = sys-ctrl.char-fld
         v-mult    = sys-ctrl.int-fld
         v-cas-lab = sys-ctrl.log-fld
         v-tags    = sys-ctrl.dec-fld.

  /* gdm - 09210907 */
  FIND FIRST sys-ctrl NO-LOCK 
    WHERE sys-ctrl.company EQ gcompany
      AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
  IF AVAIL sys-ctrl THEN ASSIGN v-bardir = sys-ctrl.log-fld
                                v-bardir-chr = sys-ctrl.char-fld.
  /* gdm - 09210907 end */

  
     {sys/inc/closejob.i FGPost}
     {sys/inc/fgpostgl.i}   
     {sys/ref/oecount.i}
     /*{sys/inc/sspostfg.i}*/
     {sys/inc/bardir.i}     
  
  
  ASSIGN v-fgpostgl  = fgpostgl.

  if v-loadtag eq "TRIAD" then begin_form = 4.

  if v-mult le 0 then v-mult = 1.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "FGRECPT"
      NO-LOCK NO-ERROR.
  ASSIGN
  v-fgrecpt = AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "LoadTag".
  
    IF bardir-int = 1 THEN DO:
       FIND FIRST users WHERE users.user_id EQ USERID("NOSWEAT") NO-LOCK NO-ERROR.
       IF AVAIL users AND users.user_program[3] NE "" THEN
           ASSIGN prmText_file = users.user_program[3]
                  userLabelPath = users.USER_program[3].       
    END.

       
    /*IF v-loadtag NE "TRIAD" THEN
       ASSIGN
          begin_form:HIDDEN = YES
          begin_labels:HIDDEN = YES.*/
    
    FIND FIRST sys-ctrl NO-LOCK 
      WHERE sys-ctrl.company EQ gcompany 
        AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
    IF AVAIL sys-ctrl 
      THEN ASSIGN v-barflg = sys-ctrl.log-fld
                  v-auto-print   = sys-ctrl.log-fld.
     prmAuto_prnt = STRING(sys-ctrl.log-fld).
    /* gdm - 04090909 end */

    /*{custom/usrprint.i}   */
    ASSIGN 
       /* prmShp_id = "NO"*/
        tb_ship-id = NO
        prmShp_id = ""
        /*v-ship-id = "".*/ .
   /* DISABLE v-ship-id.*/
    ASSIGN prmOrd_lst = "".    

   /* ASSIGN
       prmScnCs_lbl = "" /*calling from OU1 brings back old wrong value*/
       prmText_file = bardir-desc.

    IF bardir-int = 1 THEN DO:
       FIND FIRST users WHERE users.user_id EQ USERID("NOSWEAT") NO-LOCK NO-ERROR.
       IF AVAIL users AND users.user_program[3] NE "" THEN
           ASSIGN prmText_file = users.user_program[3].                  
    END.*/

    /* gdm - 06100901 */
  /*  IF SEARCH('OU1-loadtag.txt') NE ? OR
       SEARCH('IU2-loadtag.txt') NE ?
      THEN ASSIGN v-txtflg = YES.
    /* gdm - 06100901 end */

    IF SEARCH('OU1-loadtag.txt') NE ? THEN DO:
      /* gdm - 06100901 end */
      ASSIGN
        v-ord-list  = ""
        v-job-list  = ""
        prmOrd_lst  = ""
        prmJob_lst  = ""
        prmBeg_ord  = 0
        prmEnd_ord  = 0 
        prmBeg_job  = ""
        prmEnd_job  = ""
        prmBeg_job2 = 0
        prmEnd_job2 = 0
        prmBeg_itm  = ""  
        prmEnd_itm  = ""  
          .
      /* gdm - 06100901 end */

      INPUT FROM 'OU1-loadtag.txt' NO-ECHO.
      IMPORT ordNo jobNo jobNo2 iNo.
      INPUT CLOSE.
      OS-DELETE 'OU1-loadtag.txt'.
      IF ordNo NE '' THEN
      ASSIGN
        prmBeg_ord = INT(ordNo)
        prmEnd_ord   = INT(ordNo).
      IF jobNo NE '' THEN
      ASSIGN
        prmBeg_job    = jobNo
        prmEnd_job    = jobNo
        prmBeg_job2   = INT(jobNo2)
        prmEnd_job2   = INT(jobNo2).
      IF iNo NE '' THEN
      ASSIGN
        prmBeg_itm   = iNo
        prmEnd_itm     = iNo.
      prmJobPur_rct = "order".
    END.    */
    
   /* IF SEARCH('IU2-loadtag.txt') NE ? THEN DO:
      /* gdm - 06100901 end */
      ASSIGN
        v-ord-list  = ""
        v-job-list  = ""
        prmOrd_lst  = ""
        prmJob_lst  = ""
        prmBeg_ord  = 0
        prmEnd_ord  = 0 
        prmBeg_job  = ""
        prmEnd_job  = ""
        prmBeg_job2 = 0
        prmEnd_job2 = 0  
        prmBeg_itm  = ""  
        prmEnd_itm  = "" .
      /* gdm - 06100901 end */

      DEF VAR v-fg-tag AS cha FORM "x(25)" NO-UNDO.
      INPUT FROM 'IU2-loadtag.txt' NO-ECHO.
      IMPORT v-fg-tag.
      INPUT CLOSE.
      OS-DELETE 'IU2-loadtag.txt'.
      IF v-fg-tag NE '' THEN
      ASSIGN tb_reprint-tag = YES
             prmReprnt_tg = "Yes"
             prmScnCs_lbl = v-fg-tag
             .
      prmJobPur_rct = "order".
      /* APPLY 'value-changed' TO tb_reprint-tag. */
    END.   */     
    
   

   /* IF v-cas-lab THEN DO:
      ASSIGN
       fi_cas-lab:HIDDEN    = NO
       fi_cas-lab:SENSITIVE = YES.

      APPLY "entry" TO fi_cas-lab.
    END.*/
FUNCTION removeChars RETURNS CHARACTER
  (ipField AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE invalidChars AS CHARACTER NO-UNDO INITIAL "~"".
  DEFINE VARIABLE replaceChars AS CHARACTER NO-UNDO INITIAL "'',".
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE k AS INTEGER NO-UNDO.

  /*k = NUM-ENTRIES(invalidChars).
  DO i = 1 TO k: */

    ipField = REPLACE(ipField,ENTRY(1,invalidChars),ENTRY(1,replaceChars)).
  /*END.*/
  RETURN ipField.

END FUNCTION.
    

    IF prmReprnt_tg = "YES" THEN DO:
       tb_reprint-tag = YES.
      
       /* tb_reprint-tag:SCREEN-VALUE = "yes". */
    END.

 
  IF prmAction = "ldtag" THEN DO:

   ASSIGN
             
       v-ord-list           =   prmOrd_lst     
       v-job-list           =   prmJob_lst     
       begin_ord-no         =   prmBeg_ord
       end_ord-no           =   prmEnd_ord     
       begin_job            =   prmBeg_job     
       begin_job2           =   prmBeg_job2      
       end_job              =   prmEnd_job         
       end_job2             =   prmEnd_job2    
       begin_i-no           =   prmBeg_itm     
       end_i-no             =   prmEnd_itm     
       loadtagFunction      =   prmJobPur_rct  
       tb_ret               =   IF prmReturn = "True" THEN TRUE ELSE FALSE     
       tb_reprint-tag       =   IF prmReprnt_tg = "True" THEN TRUE ELSE FALSE  
       fi_cas-lab           =   prmScnCs_lbl   
       rd_order-sts         =   prmOrdJb_stat  
       rd_print             =   prmPrntPo_frm  
       begin_date           =   date(prmFrm_dt)      
       end_date             =   date(prmTo_dt)       
       rd_comps             =   prmPrntSt_comp 
       tb_xfer-lot          =   IF prmTrnsRls_lot = "True" THEN TRUE ELSE FALSE
       tb_dept-note         =   IF prmPrntDpt_not = "True" THEN TRUE ELSE FALSE
       tb_rel               =   IF prmPrntPst_bol = "True" THEN TRUE ELSE FALSE
       tb_ship-id           =   IF prmPrntShp_id  = "True" THEN TRUE ELSE FALSE  
       tb_over              =   IF prmIncl_Over = "True" THEN TRUE ELSE FALSE   
       tb_16ths             =   IF prmLwd_16th = "True" THEN TRUE ELSE FALSE   
       scr-auto-print       =   IF prmAuto_prnt = "True" THEN TRUE ELSE FALSE  
       scr-freeze-label     =   IF prmFrez_Chos = "True" THEN TRUE ELSE FALSE   
       scr-label-file       =   prmLabl_matrx  
       begin_labels         =   prmLabl_pallt  
       begin_form           =   prmPrnt_form   
       begin_filename       =   prmText_file   
       v-dept-list          =   prmDpt_lst     
       v-ship-id            =   prmShp_id .     
        

        lines-per-page = 55 .


        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "LoadTag" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vTextFile2 =  "LoadTag" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .



        ASSIGN v-auto-print = scr-auto-print.
        
    
        IF tb_reprint-tag AND prmScnCs_lbl = "" THEN DO:
            cError = "Enter tag# to reprint loadtag." .
            RETURN.
        END.

        IF tb_ship-id AND prmShp_id = "" THEN DO:
            cError = "Ship ID field cannot be blank." .
            RETURN.
        END.
        ELSE IF tb_ship-id AND prmShp_id = "" THEN
            IF scr-auto-print AND scr-label-file = "" THEN
                DO:
                cError = "Label Matrix Label File cannot be blank.".
                RETURN .
            END.

            RUN ok-button.
            /*lv-ok-ran = NO.
            IF fi_cas-lab:SCREEN-VALUE <> ""  THEN DO:
                APPLY "entry" TO fi_cas-lab.
                RETURN .
            END.*/
    
   
  /*CREATE ttSharpShooterLoadTag.
  
    ASSIGN ttSharpShooterLoadTag.ldtag = vTextFile .*/

  END.
/*****************************************************************************************/

  PROCEDURE ok-button :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF prmText_file = "" AND
     userLabelPath <> "" THEN        
     prmText_file = userLabelPath.
  
  
  
  /* gdm - 04090909 */
  ASSIGN v-out = begin_filename.
  IF v-out EQ ""  THEN DO:

      FIND FIRST sys-ctrl NO-LOCK 
          WHERE sys-ctrl.company EQ gcompany
            AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
      IF NOT AVAIL sys-ctrl THEN
          DO TRANSACTION:
          CREATE sys-ctrl.
          ASSIGN
              sys-ctrl.company  = gcompany
              sys-ctrl.name     = "BARDIR"
              sys-ctrl.descrip  = "C:\BA\Label\".
          FIND CURRENT sys-ctrl NO-LOCK.
      END.
      v-out = sys-ctrl.descrip.
  END.
  /* gdm - 04090909 end */

  

  /*FILE-INFO:FILE-NAME = begin_filename.
  if begin_filename <> "" AND FILE-INFO:FILE-type eq ? then do:
     cError = "Form file/path does not exist. Do you want to create it?".
  END.
     ELSE do:
         cError = "Loadtag file path is not valid. Can't create.".
         return .
     END.*/
   

  /* gdm - 09290903 */
  IF prmOrd_lst NE "" THEN DO:
    ASSIGN begin_ord-no = 0
           end_ord-no   = 0
/*            begin_i-no   = "" */
/*            end_i-no     = "" */
           begin_job    = ""
           end_job      = "".
  END.
  /* gdm - 09290903 end*/
  /*wfk  */
  
  IF tb_reprint-tag THEN RUN reprint-tag.
  ELSE RUN run-report NO-ERROR. 
  
  
END PROCEDURE.

PROCEDURE reprint-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
  FIND FIRST loadtag WHERE loadtag.company     EQ cocode
                 AND loadtag.item-type   EQ NO
                 AND loadtag.tag-no  eq TRIM(fi_cas-lab) NO-LOCK NO-ERROR.
  IF NOT AVAIL loadtag THEN DO:
      cError = "Invalid Loadtag. Try Help." .
      RETURN.
  END.          
  RUN create-w-ord.
  
  /*{sys/inc/print1.i}*/

  if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.

  {sys/inc/outprint.i value(lines-per-page)} 

      VIEW FRAME r-top.
      VIEW FRAME top.
  IF v-out = "" THEN v-out = "c:~\ba~\label~\loadtag.txt".
  ELSE do:
     IF SUBSTRING(v-out,LENGTH(v-out),1) = "/" OR
        SUBSTRING(v-out,LENGTH(v-out),1) = "\" THEN .
     ELSE v-out = v-out + "/".
     v-out = v-out + "loadtag.txt".
  END.
  
  RUN create-text-file.
  cError = "Loadtag reprint is completed." .
  RUN AutoPrint.

END PROCEDURE.

PROCEDURE AutoPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*IF scr-auto-print THEN
  RUN custom/lmprint.p (INPUT scr-label-file).*/

END PROCEDURE.

PROCEDURE create-w-ord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-rel-date AS DATE NO-UNDO.
   DEF BUFFER b-job FOR job.
   DEF BUFFER b-job-hdr FOR job-hdr.
   
   FIND FIRST company WHERE company.company = loadtag.company NO-LOCK NO-ERROR.
   FIND FIRST itemfg WHERE itemfg.company = loadtag.company
                       AND itemfg.i-no = loadtag.i-no NO-LOCK NO-ERROR.
   FIND FIRST oe-ord WHERE oe-ord.company = loadtag.company
                       AND oe-ord.ord-no = loadtag.ord-no NO-LOCK NO-ERROR.

   IF AVAIL oe-ord THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = loadtag.company
                           AND oe-ordl.ord-no = loadtag.ord-no
                           AND oe-ordl.i-no = loadtag.i-no NO-LOCK NO-ERROR.
      FIND FIRST cust WHERE cust.company = loadtag.company
                        AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.

      FIND FIRST b-job NO-LOCK WHERE b-job.company = loadtag.company
                                 AND b-job.job-no  = loadtag.job-no
                                 AND b-job.job-no2 = loadtag.job-no2  NO-ERROR.
      IF AVAIL b-job THEN
         FIND FIRST b-job-hdr WHERE b-job-hdr.company EQ b-job.company
                                AND b-job-hdr.job     EQ b-job.job
                                AND b-job-hdr.job-no  EQ b-job.job-no
                                AND b-job-hdr.job-no2 EQ b-job.job-no2
                                AND b-job-hdr.i-no    EQ loadtag.i-no NO-LOCK NO-ERROR.
      

      CREATE w-ord.
      ASSIGN w-ord.ord-no      = loadtag.ord-no
            w-ord.job-no       = loadtag.job-no
            w-ord.job-no2      = loadtag.job-no2
            w-ord.cust-no      = oe-ord.cust-no
            w-ord.cust-name    = oe-ord.cust-name
            w-ord.i-no         = loadtag.i-no
            w-ord.cust-part-no = oe-ordl.part-no
            w-ord.ord-qty      = loadtag.qty
            w-ord.po-no        = oe-ordl.po-no-po
            w-ord.i-name       = loadtag.i-name
            w-ord.due-date     = if oe-ord.due-date ne ? then
                                   oe-ord.due-date
                                 else
                                 if oe-ordl.req-date ne ? then
                                   oe-ordl.req-date
                                 else today
            w-ord.est-no       = oe-ordl.est-no
            w-ord.form-no      = oe-ordl.form-no
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = if cust.int-field[1] ne 0 then
                                   cust.int-field[1] else v-mult
            w-ord.dont-run-set = oe-ordl.is-a-component
            w-ord.ord-desc1    = oe-ordl.part-dscr1
            w-ord.ord-desc2    = oe-ordl.part-dscr2
            w-ord.sold-code    = oe-ord.sold-id
            w-ord.sold-name    = oe-ord.sold-name
            w-ord.sold-add1    = oe-ord.sold-add[1]
            w-ord.sold-add2    = oe-ord.sold-add[2]
            w-ord.sold-city    = oe-ord.sold-city
            w-ord.sold-state   = oe-ord.sold-state
            w-ord.sold-zip     = oe-ord.sold-zip
            w-ord.linenum      = oe-ordl.e-num
            w-ord.lot          = loadtag.misc-char[2].
              ASSIGN
              w-ord.rec_key = STRING(TODAY,"99999999") + STRING(prmScnCs_lbl,"99999999") + string(create-count)   
              create-count = create-count + 1.

      IF AVAIL b-job-hdr THEN
         w-ord.due-date-jobhdr = IF b-job-hdr.due-date <> ? THEN STRING(b-job-hdr.due-date, "99/99/9999") ELSE "".
      IF AVAIL b-job THEN
         w-ord.due-date-job = IF b-job.due-date <> ? THEN STRING(b-job.due-date, "99/99/9999") ELSE "".
             
      RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                        OUTPUT w-ord.rel-date,
                        OUTPUT w-ord.rel-lot#).
      IF tb_xfer-lot THEN w-ord.lot# = w-ord.rel-lot#.

      IF AVAIL itemfg THEN
         ASSIGN w-ord.upc-no  = itemfg.upc-no
             w-ord.box-len = itemfg.l-score[50]
             w-ord.box-wid = itemfg.w-score[50]
             w-ord.box-dep = itemfg.d-score[50]
             w-ord.flute   = itemfg.flute
             w-ord.test    = itemfg.test
             w-ord.pcs     = loadtag.qty-case
             w-ord.bundle  = loadtag.case-bundle
             w-ord.style   = itemfg.style.

      IF w-ord.style NE "" THEN
      DO:
         FIND FIRST style WHERE
              style.company EQ cocode AND
              style.style EQ w-ord.style
              NO-LOCK NO-ERROR.

         IF AVAIL style THEN
         DO:
            w-ord.style-desc = style.dscr.
            RELEASE style.
         END.
      END.
     /* IF v-ship-id EQ "" THEN v-ship-id = oe-ord.cust-no.*/
      FIND FIRST shipto WHERE shipto.company eq cocode
            AND shipto.cust-no eq oe-ord.cust-no
            /*AND shipto.ship-id eq v-ship-id*/
            USE-INDEX ship-id NO-LOCK NO-ERROR.
      IF AVAIL shipto THEN
         ASSIGN
            w-ord.ship-name  = shipto.ship-name
            w-ord.ship-add1  = shipto.ship-add[1]
            w-ord.ship-add2  = shipto.ship-add[2]
            w-ord.ship-city  = shipto.ship-city
            w-ord.ship-state = shipto.ship-state
            w-ord.ship-zip   = shipto.ship-zip.

          IF NOT AVAIL eb AND AVAIL itemfg AND itemfg.est-no NE "" THEN
          FIND FIRST eb
              WHERE eb.company  EQ itemfg.company
                AND eb.est-no   EQ itemfg.est-no
                AND eb.stock-no EQ itemfg.i-no
              NO-LOCK NO-ERROR.

          IF AVAIL eb THEN
            ASSIGN
             w-ord.flute  = eb.flute
             w-ord.test   = eb.test
             w-ord.pcs    = eb.cas-cnt
             w-ord.bundle = eb.cas-pal
             w-ord.cas-no = eb.cas-no
             w-ord.pallt-no = eb.tr-no.

          ASSIGN w-ord.total-tags = 1
            w-ord.ord-qty = loadtag.qty 
            w-ord.pcs = loadtag.qty-case
            w-ord.bundle = loadtag.case-bundle
            w-ord.partial =loadtag.partial
            w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial .      
   END.  /* avail oe-ord*/
   ELSE IF loadtag.job-no <> "" THEN DO:
      FIND FIRST job NO-LOCK WHERE job.company = loadtag.company
                               AND job.job-no = loadtag.job-no
                               AND job.job-no2 = loadtag.job-no2  NO-ERROR.
      IF AVAIL job THEN
         FIND FIRST job-hdr WHERE job-hdr.company EQ job.company
                AND job-hdr.job     EQ job.job
                AND job-hdr.job-no  EQ job.job-no
                AND job-hdr.job-no2 EQ job.job-no2
                AND job-hdr.i-no    EQ loadtag.i-no NO-LOCK NO-ERROR.
      IF AVAIL job-hdr THEN DO:
      
         FIND FIRST cust WHERE cust.company eq cocode
                          AND cust.cust-no eq job-hdr.cust-no NO-LOCK NO-ERROR.
         FIND FIRST itemfg WHERE itemfg.company eq cocode
                            AND itemfg.i-no    eq job-hdr.i-no NO-LOCK NO-ERROR.
         
         CREATE w-ord.
         ASSIGN
            w-ord.ord-no       = job-hdr.ord-no
            w-ord.job-no       = job-hdr.job-no
            w-ord.job-no2      = job-hdr.job-no2
            w-ord.cust-no      = cust.cust-no
            w-ord.cust-name    = cust.name
            w-ord.i-no         = job-hdr.i-no
            w-ord.ord-qty      = job-hdr.qty
            w-ord.due-date     = job.start-date
            w-ord.est-no       = job.est-no
            w-ord.form-no      = job-hdr.frm
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = if cust.int-field[1] ne 0 then
                                   cust.int-field[1] else v-mult
            w-ord.lot          = loadtag.misc-char[2].
              ASSIGN
              w-ord.rec_key = STRING(TODAY,"99999999") + STRING(prmScnCs_lbl,"99999999") + string(create-count)   
              create-count = create-count + 1.

          IF AVAIL itemfg THEN
             ASSIGN
                w-ord.cust-part-no = itemfg.part-no
                w-ord.style        = itemfg.style
                w-ord.i-name       = itemfg.i-name
                w-ord.upc-no       = itemfg.upc-no
                w-ord.upc-no       = itemfg.upc-no
                w-ord.box-len      = itemfg.l-score[50]
                w-ord.box-wid      = itemfg.w-score[50]
                w-ord.box-dep      = itemfg.d-score[50].

          IF w-ord.style NE "" THEN
          DO:
             FIND FIRST style WHERE
                  style.company EQ cocode AND
                  style.style EQ w-ord.style
                  NO-LOCK NO-ERROR.

             IF AVAIL style THEN
             DO:
                w-ord.style-desc = style.dscr.
                RELEASE style.
             END.
          END.
          /*IF v-ship-id EQ "" THEN v-ship-id = job-hdr.cust-no.*/
          FIND FIRST shipto
              WHERE shipto.company eq cocode
                AND shipto.cust-no eq job-hdr.cust-no
                AND shipto.ship-id eq job-hdr.cust-no
              USE-INDEX ship-id NO-LOCK NO-ERROR.
          IF AVAIL shipto THEN
          ASSIGN
            w-ord.ship-name  = shipto.ship-name
            w-ord.ship-add1  = shipto.ship-add[1]
            w-ord.ship-add2  = shipto.ship-add[2]
            w-ord.ship-city  = shipto.ship-city
            w-ord.ship-state = shipto.ship-state
            w-ord.ship-zip   = shipto.ship-zip.

          FIND FIRST est WHERE est.company eq job.company
                AND est.est-no  eq job.est-no
              NO-LOCK NO-ERROR.
          RELEASE eb.
          IF AVAIL est THEN
          FIND FIRST eb
              WHERE eb.company   EQ est.company
                AND eb.est-no    EQ est.est-no
                AND eb.form-no   EQ job-hdr.frm
                AND (eb.blank-no EQ job-hdr.blank-no OR job-hdr.blank-no EQ 0)
              NO-LOCK NO-ERROR.

          IF AVAIL eb THEN
            ASSIGN
             w-ord.flute      = eb.flute
             w-ord.test       = eb.test
             w-ord.pcs        = eb.cas-cnt
             w-ord.bundle     = eb.cas-pal
             w-ord.total-unit = w-ord.pcs * w-ord.bundle
             w-ord.partial    = 0 /* w-ord.ord-qty - w-ord.total-unit*/
             w-ord.cas-no     = eb.cas-no
             w-ord.pallt-no   = eb.tr-no.

          ASSIGN w-ord.total-tags = 1
            w-ord.ord-qty = loadtag.qty 
            w-ord.pcs = loadtag.qty-case
            w-ord.bundle = loadtag.case-bundle
            w-ord.partial =loadtag.partial
            w-ord.total-unit = w-ord.pcs * w-ord.bundle  .      

       END.  /* avail job*/
   END. /* job-no <> "" */
   ELSE IF loadtag.po-no <> 0 THEN DO:
      FIND FIRST po-ord WHERE po-ord.company = loadtag.company
                           AND po-ord.po-no = loadtag.po-no NO-LOCK NO-ERROR.
      IF AVAIL po-ord THEN
         FIND FIRST po-ordl NO-LOCK WHERE po-ordl.company EQ po-ord.company
                                    AND po-ordl.po-no EQ po-ord.po-no
                                    AND po-ordl.i-no = loadtag.i-no
                                    USE-INDEX po-no  NO-ERROR.
      IF AVAIL po-ordl THEN DO:
         FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                                AND cust.cust-no EQ po-ord.cust-no NO-ERROR.
         FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                                AND vend.vend-no EQ po-ord.vend-no NO-ERROR.
         FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode
                                  AND itemfg.i-no EQ po-ordl.i-no NO-ERROR.
         
         CREATE w-ord.
         ASSIGN
            w-ord.cust-name = IF AVAILABLE cust THEN cust.name ELSE ''
            w-ord.cust-no = po-ord.cust-no
            w-ord.due-date = po-ord.due-date
            w-ord.i-no = po-ordl.i-no
            w-ord.i-name = po-ordl.i-name
            w-ord.mult = IF AVAILABLE cust AND cust.int-field[1] NE 0 THEN
                         cust.int-field[1] ELSE v-mult
            w-ord.ord-qty = po-ordl.ord-qty
            w-ord.po-no = po-ord.po-no
            w-ord.tare-wt = 10
            w-ord.uom = 'EA'
            w-ord.vendor = IF AVAILABLE vend THEN vend.name ELSE ''
            w-ord.lot    = loadtag.misc-char[2]. 
              ASSIGN
              w-ord.rec_key = STRING(TODAY,"99999999") + STRING(prmScnCs_lbl,"99999999") + string(create-count)   
              create-count = create-count + 1.
         IF AVAILABLE itemfg THEN
            ASSIGN w-ord.est-no = itemfg.est-no
                w-ord.upc-no = itemfg.upc-no
                w-ord.box-len = itemfg.l-score[50]
                w-ord.box-wid = itemfg.w-score[50]
                w-ord.box-dep = itemfg.d-score[50]
                w-ord.flute = itemfg.flute
                w-ord.test = itemfg.test
                w-ord.pcs = itemfg.case-count
                w-ord.bundle = IF itemfg.case-pall NE 0 THEN itemfg.case-pall ELSE 1
                w-ord.style = itemfg.style.

         IF w-ord.style NE "" THEN
         DO:
            FIND FIRST style WHERE
                 style.company EQ cocode AND
                 style.style EQ w-ord.style
                 NO-LOCK NO-ERROR.
         
            IF AVAIL style THEN
            DO:
               w-ord.style-desc = style.dscr.
               RELEASE style.
            END.
         END.

         IF AVAILABLE itemfg AND itemfg.est-no NE '' THEN
            FIND FIRST eb NO-LOCK WHERE eb.company EQ itemfg.company
                              AND eb.est-no EQ itemfg.est-no
                              AND eb.stock-no EQ itemfg.i-no NO-ERROR.
         IF AVAILABLE eb THEN
             ASSIGN w-ord.flute = eb.flute
                    w-ord.test = eb.test
                    w-ord.pcs = eb.cas-cnt
                    w-ord.bundle = eb.cas-pal
                    w-ord.cas-no = eb.cas-no
                    w-ord.pallt-no = eb.tr-no.
        /* IF v-ship-id EQ "" THEN v-ship-id = po-ord.cust-no.*/
         FIND FIRST shipto NO-LOCK WHERE shipto.company EQ cocode
                                  AND shipto.cust-no EQ po-ord.cust-no
                                 /* AND shipto.ship-id EQ v-ship-id*/
                                USE-INDEX ship-id NO-ERROR.
         IF AVAILABLE shipto THEN
            ASSIGN w-ord.ship-name = shipto.ship-name
                    w-ord.ship-add1 = shipto.ship-add[1]
                    w-ord.ship-add2 = shipto.ship-add[2]
                    w-ord.ship-city = shipto.ship-city
                    w-ord.ship-state = shipto.ship-state
                    w-ord.ship-zip = shipto.ship-zip.
      
         ASSIGN w-ord.total-tags = 1
            w-ord.ord-qty = loadtag.qty 
            w-ord.pcs = loadtag.qty-case
            w-ord.bundle = loadtag.case-bundle
            w-ord.partial =loadtag.partial
            w-ord.total-unit = w-ord.pcs * w-ord.bundle  .      

    END. /* AVAIL PO-ORDL */
   END. /* po-no <> ""*/
   ELSE DO:
       FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode
                                 AND itemfg.i-no EQ loadtag.i-no NO-ERROR.
       IF AVAIL itemfg THEN DO:
          FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                              AND vend.vend-no EQ itemfg.vend-no NO-ERROR.
          FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                              AND cust.cust-no EQ itemfg.cust-no NO-ERROR.
          
          CREATE w-ord.
          ASSIGN w-ord.i-no = itemfg.i-no
                 w-ord.i-name = itemfg.i-name
                 w-ord.cust-no = itemfg.cust-no
                 w-ord.cust-name = itemfg.cust-name
                 w-ord.cust-part-no = itemfg.part-no
                 w-ord.mult = IF AVAILABLE cust AND cust.int-field[1] NE 0 THEN
                              cust.int-field[1] ELSE v-mult
                 w-ord.box-len = itemfg.l-score[50]
                 w-ord.box-wid = itemfg.w-score[50]
                 w-ord.box-dep = itemfg.d-score[50]
                 w-ord.flute = itemfg.flute
                 w-ord.upc-no = itemfg.upc-no
                 w-ord.test = itemfg.test
                 w-ord.vendor = IF AVAILABLE vend THEN vend.name ELSE company.name
                 w-ord.tare-wt = 10
                 w-ord.uom = "EA"
                 w-ord.pcs = itemfg.case-count
                 w-ord.bundle = itemfg.case-pall
                 w-ord.total-tags = 1
                 w-ord.ord-qty = loadtag.qty 
                 w-ord.pcs = loadtag.qty-case
                 w-ord.bundle = loadtag.case-bundle
                 w-ord.partial =loadtag.partial
                 w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial
                 w-ord.style        = itemfg.style
                 w-ord.lot          = loadtag.misc-char[2].
                   ASSIGN
              w-ord.rec_key = STRING(TODAY,"99999999") + STRING(prmScnCs_lbl,"99999999") + string(create-count)   
              create-count = create-count + 1.

          IF w-ord.style NE "" THEN
          DO:
             FIND FIRST style WHERE
                  style.company EQ cocode AND
                  style.style EQ w-ord.style
                  NO-LOCK NO-ERROR.
          
             IF AVAIL style THEN
             DO:
                w-ord.style-desc = style.dscr.
                RELEASE style.
             END.
          END.
       END. /* avail itemfg */
   END.
   /* task 11230523 */
   IF tb_reprint-tag THEN DO:
      FIND FIRST fg-bin WHERE fg-bin.company = loadtag.company
                          AND fg-bin.i-no = w-ord.i-no
                          AND fg-bin.tag = fi_cas-lab
                          AND fg-bin.qty > 0 NO-LOCK NO-ERROR.
      IF AVAIL fg-bin AND AVAIL w-ord THEN
         ASSIGN w-ord.pcs = fg-bin.case-count
                w-ord.bundle = /*fg-bin.cases-unit*/ TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                w-ord.partial = fg-bin.partial-count
                w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial .      
   END.

   
END PROCEDURE.

PROCEDURE get-rel-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-pono LIKE w-ord.cust-po-no NO-UNDO.
  DEF OUTPUT PARAM op-date LIKE w-ord.rel-date NO-UNDO.
  DEF OUTPUT PARAM op-lot# LIKE w-ord.rel-lot# NO-UNDO.
    
  /*DEF VAR op-pono LIKE w-ord.cust-po-no NO-UNDO.
  DEF VAR op-date LIKE w-ord.rel-date NO-UNDO.
  DEF VAR op-lot# LIKE w-ord.rel-lot# NO-UNDO.*/


  RELEASE oe-rell.
  RELEASE oe-rel.

  IF v-po-no-source EQ "R" THEN DO:
    FOR EACH oe-rell NO-LOCK
        WHERE oe-rell.company  EQ oe-ordl.company
          AND oe-rell.ord-no   EQ oe-ordl.ord-no
          AND oe-rell.i-no     EQ oe-ordl.i-no
          AND oe-rell.line     EQ oe-ordl.line,

        FIRST oe-relh NO-LOCK
        WHERE oe-relh.r-no     EQ oe-rell.r-no
          AND oe-relh.posted   EQ NO
          AND oe-relh.rel-date GE begin_date
          AND oe-relh.rel-date LE end_date
        BY oe-relh.rel-date
        BY oe-relh.r-no:

      ASSIGN
       op-pono = oe-rell.po-no
       op-date = oe-relh.rel-date.
      LEAVE.
    END.

    IF AVAIL oe-rell THEN
    FIND FIRST oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no NO-LOCK NO-ERROR.

    ELSE
    FOR EACH oe-rel NO-LOCK
        WHERE oe-rel.company  EQ oe-ordl.company
          AND oe-rel.ord-no   EQ oe-ordl.ord-no
          AND oe-rel.i-no     EQ oe-ordl.i-no
          AND oe-rel.line     EQ oe-ordl.line
          AND oe-rel.rel-no   EQ 0
          AND oe-rel.rel-date GE begin_date
          AND oe-rel.rel-date LE end_date
        BY oe-rel.rel-date
        BY oe-rel.r-no:

      ASSIGN
       op-pono = oe-rel.po-no
       op-date = oe-rel.rel-date.
      LEAVE.
    END.
  END.

  IF NOT AVAIL oe-rel THEN
  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company  EQ oe-ordl.company
        AND oe-rel.ord-no   EQ oe-ordl.ord-no
        AND oe-rel.i-no     EQ oe-ordl.i-no
        AND oe-rel.line     EQ oe-ordl.line
      BY oe-rel.rel-date
      BY oe-rel.r-no:

    op-date = oe-rel.rel-date.
    LEAVE.
  END.

  IF AVAIL oe-rel THEN DO:
    FIND FIRST ref-lot-no NO-LOCK
        WHERE ref-lot-no.reftable EQ "oe-rel.lot-no"
          AND ref-lot-no.company  EQ STRING(oe-rel.r-no,"9999999999")
        NO-ERROR.
    IF AVAIL ref-lot-no THEN op-lot# = ref-lot-no.code.
  END.

  IF v-po-no-source NE "R"                    OR
     (NOT AVAIL oe-rel AND NOT AVAIL oe-rell) THEN
    op-pono = IF v-po-no-source EQ "L" THEN oe-ordl.po-no
                                       ELSE IF AVAIL oe-ord THEN oe-ord.po-no
                                       ELSE "".

END PROCEDURE.

PROCEDURE create-text-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i AS INT NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR iPalletID AS INT NO-UNDO.
  DEF VAR iStartPalletID AS INT NO-UNDO.
  DEF VAR iEndPalletID AS INT NO-UNDO.
  DEF VAR cTotalUnit AS CHAR NO-UNDO.
  DEF VAR cRFIDTag AS cha NO-UNDO.
  DEF VAR vError AS LOG NO-UNDO.
  FIND FIRST w-ord NO-ERROR.
  DEF BUFFER bf-cust FOR cust.

   IF v-loadtag = "TRIAD" THEN DO:
        if form_fid > "" then do:   /* download the form file into the printer ~*/
          input stream s-form from value(form_fid) no-echo.
          _form: do while true:
                readkey stream s-form.
            if lastkey < 0 then leave _form.
              put stream s-bar CONTROL chr(lastkey).
          end.
          input stream s-form close.
        end.
        FOR EACH w-ord:
           v-job = w-ord.job-no + "-" + string(w-ord.job-no2,"99").
           IF v-job BEGINS "-" or v-job = ? /* 9901 CAH */
                THEN v-job = string(W-ORD.ORD-NO).   /* 9812 CAH in case blank */
           find first itemfg where itemfg.company = cocode
                    and itemfg.i-no = w-ord.i-no no-lock no-error.
        IF w-ord.total-tags gt -1 THEN DO:
          DO i = 1 TO (w-ord.total-tags + 1):
            /* select the form */
            put stream s-bar control stx esc "E" string(form#) ",1" can etx.
            /* 9901 CAH: done above ... 
            /* clear the variable data fields */
            put stream s-bar control stx can etx.
            */
            char_units = (if i <= w-ord.total-tags 
            then string(w-ord.total-unit) else "    ").  
            def var char_date as char format 'x(10)' no-undo.
            char_date = string(today,"99/99/9999").
            /* 9901 CAH: Only room for 19 chars in the standard 48 pt font */
            if length(w-ord.ship-name) > 19
            then w-ord.ship-name = substring(w-ord.ship-name,1,19).
            
            def var vcFGItem as char no-undo.
            vcFGItem = 
                if avail itemfg then itemfg.i-no else w-ord.i-no.
           do n = copy_count to 1 by -1:
            /* send the variable data to the printer */
            put stream s-bar unformatted
                stx w-ord.cust-po-no    cr etx
                stx w-ord.cust-po-no    cr etx
                stx w-ord.cust-part-no  cr etx
                stx w-ord.cust-part-no  cr etx
                stx char_units          cr etx
                stx char_units          cr etx
                stx char_date           cr etx
                stx v-job               cr etx
                stx w-ord.ord-qty       cr etx /* 9902 CAH was total-unit */
                stx string(i)           cr etx /* 08.20 was n */
                stx string(w-ord.total-tags + 1) cr etx /* 08.20 was copy_count */
                stx w-ord.ship-name     cr etx
                stx vcFGItem            cr etx.
            /* issue the print command */    
            put stream s-bar control     
                stx rs "1" us "1" etb etx.
           end.
          end.   /* tag count loop */
        end.  /* non zero */  
        END.    /* each w-ord */
      /*  {sys/inc/close.i "" "stream s-bar"} */
        OUTPUT CLOSE.
    END.    /* TRIAD INTERMEC BARCODE PRINT ROUTINE */
    ELSE
    DO:
      OUTPUT TO VALUE(v-out).
      PUT UNFORMATTED
          "CUSTOMER,ORDNUMBER,JOBNUMBER,ITEM,CUSTPARTNO,CUSTPONO,PCS,BUNDLE,TOTAL," +
          "SHIPCODE,SHIPNAME,SHIPADD1,SHIPADD2,SHIPCITY,SHIPSTATE,SHIPCOUNTRY,SHIPZIP," +
          "SOLDCODE,SOLDNAME,SOLDADD1,SOLDADD2,SOLDCITY,SOLDSTATE,SOLDCOUNTRY,SOLDZIP," +
          "INAME,DUEDATE,RELDATE,UPCNO,LENGTH,WIDTH,DEPTH,FLUTE,TEST,VENDOR,GROSSWGT," +
          "TAREWGT,NETWGT,SHEETWGT,UOM,STYLE,STYLEDESC,RELLOTNO,MIDDLESEXJOBNUMBER,MIDDLESEXCUSTPONO,"
          "TAG#,PARTIAL,CASECODE,SN1,SN2,SN3,SN4,SN5,SN6,SN7,SN8,PONO,DN1,DN2,DN3,DN4,"+
          "DN5,DN6,DN7,DN8,DN9,DN10,EST#,ORDDESC1,ORDDESC2".
      IF LOOKUP(v-loadtag,"ASI,SSLABEL") GT 0 THEN
         PUT UNFORMATTED ",COUNTER#,RFIDTag".

      PUT UNFORMATTED ",DUEDATEJOBLINE,DUEDATEJOB,LINE#,UnitWt,PalletWt,FGdesc1,FGdesc2,FGdesc3,FG Lot#,PalletCode,PalletID".
      
      PUT SKIP.

      FOR EACH w-ord:

        IF tb_16ths THEN
          ASSIGN
           w-ord.box-len = ROUND((w-ord.box-len - TRUNC(w-ord.box-len,0)) / 6.25,2) +
                           TRUNC(w-ord.box-len,0)
           w-ord.box-wid = ROUND((w-ord.box-wid - TRUNC(w-ord.box-wid,0)) / 6.25,2) +
                           TRUNC(w-ord.box-wid,0)
           w-ord.box-dep = ROUND((w-ord.box-dep - TRUNC(w-ord.box-dep,0)) / 6.25,2) +
                           TRUNC(w-ord.box-dep,0).

        ASSIGN
        lv-text = ""
        v-dept-note = ""

        /* gdm - 10160905 */
        v-fgdsc1 = ""
        v-fgdsc2 = ""
        v-fgdsc3 = "".

        find first itemfg where itemfg.company eq cocode
                            and itemfg.i-no    eq w-ord.i-no no-lock no-error.
        if avail itemfg THEN DO:        
           ASSIGN w-ord.net-wt   = itemfg.weight-100 * w-ord.total-unit / 100
                  w-ord.sheet-wt = itemfg.weight-100 / 100 
                  w-ord.cust-part-no = itemfg.part-no.

           FOR EACH tt-formtext:
               DELETE tt-formtext.
           END.
           FOR EACH notes NO-LOCK WHERE notes.rec_key = itemfg.rec_key
                                     AND notes.note_code = "SN" :
                lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
           END.
           DO li = 1 TO 8:
               CREATE tt-formtext.
               ASSIGN tt-line-no = li
                      tt-length  = 80.
           END.
           RUN custom/formtext.p (lv-text).
           i = 0.           
           FOR EACH tt-formtext:
               i = i + 1.
               IF  i <= 8 THEN v-dept-note[i] = tt-formtext.tt-text.      
           END.

           /* gdm - 101610905 */
           ASSIGN v-fgdsc1 = itemfg.part-dscr1
                  v-fgdsc2 = itemfg.part-dscr2
                  v-fgdsc3 = itemfg.part-dscr3.

        END.  /* avail itemfg */

        IF tb_dept-note THEN DO:
           lv-text = "".
           FOR EACH tt-formtext:
               DELETE tt-formtext.
           END.

           IF w-ord.ord-no NE 0 THEN DO:
              FOR EACH job-hdr NO-LOCK
                  WHERE job-hdr.company EQ cocode
                    AND job-hdr.ord-no  EQ w-ord.ord-no 
                    AND job-hdr.job-no  EQ w-ord.job-no
                    AND job-hdr.job-no2 EQ w-ord.job-no2
                  BREAK BY job-hdr.job
                        BY job-hdr.job-no
                        BY job-hdr.job-no2:
                 IF LAST-OF(job-hdr.job-no2) THEN
                FOR EACH job NO-LOCK
                     WHERE job.company EQ job-hdr.company
                       AND job.job     EQ job-hdr.job
                       AND job.job-no  EQ job-hdr.job-no
                       AND job.job-no2 EQ job-hdr.job-no2,
                     EACH notes NO-LOCK
                     WHERE notes.rec_key EQ job.rec_key
                       AND CAN-DO(v-dept-list,notes.note_code):
                    IF notes.note_form_no = 0 OR notes.note_form_no = w-ord.form-no THEN
                        lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
                 END.
              END.

/*              FOR EACH oe-ordl NO-LOCK
                  WHERE oe-ordl.company EQ cocode
                    AND oe-ordl.ord-no  EQ w-ord.ord-no,
                  EACH notes NO-LOCK WHERE notes.rec_key EQ oe-ordl.rec_key
                       AND CAN-DO(v-dept-list,notes.note_code)
                       AND notes.note_form_no = w-ord.form-no
                  BY oe-ordl.LINE:
                    lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
              END.
*/
           END.
           IF lv-text NE "" THEN DO:
              DO li = 1 TO 10:
                 CREATE tt-formtext.
                 ASSIGN tt-line-no = li
                        tt-length  = 80.
              END.
              RUN custom/formtext.p (lv-text).
              i = 8.           
              FOR EACH tt-formtext:
                  i = i + 1.
                  IF i <= 18 THEN v-dept-note[i] = tt-formtext.tt-text.      

              END.
           END.
        END. /* tb_dept-note*/

        ASSIGN
        w-ord.gross-wt = w-ord.net-wt + w-ord.tare-wt
        v-job = w-ord.job-no + "-" + string(w-ord.job-no2,"99").
        IF v-job BEGINS "-" THEN v-job = "".
        ASSIGN
         lv-middlesex-po  = SUBSTR(TRIM(w-ord.job-no),1,6)
         lv-middlesex-job = IF lv-middlesex-job EQ "" THEN "" ELSE
                            "%MX" +
                            FILL("0",6 - LENGTH(TRIM(lv-middlesex-job))) +
                            TRIM(lv-middlesex-job)
         lv-middlesex-po  = SUBSTR(TRIM(w-ord.cust-po-no),1,6)
         lv-middlesex-po  = IF lv-middlesex-po EQ "" THEN "" ELSE
                            "BNJ" +
                            FILL("0",6 - LENGTH(TRIM(lv-middlesex-po))) +
                            TRIM(lv-middlesex-po).

        IF w-ord.total-tags gt 0 THEN DO:
          lv-how-many-tags =  IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 OR w-ord.total-tags = 1 THEN w-ord.total-tags
                              ELSE (w-ord.total-tags - 1).
          FIND bf-cust WHERE bf-cust.company = cocode
                         AND bf-cust.cust-no EQ w-ord.cust-no
                       NO-LOCK NO-ERROR.

           RUN incrementPalletID (BUFFER bf-cust, lv-how-many-tags * w-ord.mult,
                                  OUTPUT iStartPalletID, OUTPUT iEndPalletID).
           IF iEndPalletID EQ -1 THEN DO:
              cError = "The Pallet ID has reached its limit, Please reset it ". 
              RETURN.
           END.


          iPalletId = iStartPalletID.
          DO i = 1 TO (lv-how-many-tags * w-ord.mult):
             /* loadtags generation */
             IF i MOD w-ord.mult = 1 OR i = 1 OR w-ord.mult = 1  THEN DO:
                IF i = 1 THEN lv-tag-no = i.
                RUN create-loadtag (INPUT-OUTPUT lv-tag-no, w-ord.total-unit).
             END.

             IF LOOKUP(v-loadtag,"ASI,SSLABEL") GT 0 THEN DO:
                
                FIND FIRST rfidtag OF loadtag NO-LOCK NO-ERROR.
                cRFIDTag = IF AVAIL rfidtag THEN rfidtag.rfidtag ELSE "".
 
             END.
             cTotalUnit = string(w-ord.total-unit, ">>>>>>>9").
             RUN write-loadtag-line (INPUT cRFIDTag, INPUT cTotalUnit, INPUT iPalletID).
             iPalletID = iPalletID + 1.
          end.

          IF lookup(v-loadtag,"SSLABEL,CentBox") = 0 THEN DO:

  
              RUN incrementPalletID (BUFFER bf-cust, w-ord.mult,
                       OUTPUT iStartPalletID, OUTPUT iEndPalletID).
              IF iEndPalletID EQ -1 THEN DO:
                   cError = "The Pallet ID has reached its limit, Please reset it ". 
                   RETURN.
              END.
                     

              iPalletId = iStartPalletID.
              do v-count = 1 to w-ord.mult: /* for partial print */
                  /* loadtags generation */
                 IF v-count EQ 1 THEN RUN create-loadtag (INPUT-OUTPUT lv-tag-no, 0).
                 cTotalUnit = "".
                 RUN write-loadtag-line (INPUT cRFIDTag, cTotalUnit, INPUT iPalletID).
                 iPalletID = iPalletID + 1.
              end.
          END.

        end.
        delete w-ord.
      end.
      output close.
    end.    /* NOT TRIAD */

END PROCEDURE.

PROCEDURE write-loadtag-line :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ipc-rfid AS CHAR NO-UNDO.
 DEF INPUT PARAMETER ipc-totalUnit AS CHAR NO-UNDO.
 DEF INPUT PARAMETER ipi-pallet-id AS INT NO-UNDO.

    /*DEF VAR ipc-rfid AS CHAR NO-UNDO.
 DEF VAR ipc-totalUnit AS CHAR NO-UNDO.
 DEF VAR ipi-pallet-id AS INT NO-UNDO.*/

 PUT UNFORMATTED "~""  removeChars(w-ord.cust-name)  "~","
  w-ord.ord-no  ","
  "~""  v-job  "~","
  "~""  caps(removeChars(w-ord.i-no))  FORM "x(15)" "~","
  "~""  removeChars(w-ord.cust-part-no) "~","
  "~""  removeChars(w-ord.cust-po-no)  "~","
  w-ord.pcs  ","
  w-ord.bundle  ","
  trim(ipc-totalUnit) ","
  "~""  removeChars(w-ord.ship-code)  "~","
  "~""  removeChars(w-ord.ship-name)  "~","
  "~""  removeChars(w-ord.ship-add1)  "~","
  "~""  removeChars(w-ord.ship-add2)  "~","
  "~""  removeChars(w-ord.ship-city)  "~","
  "~""  removeChars(w-ord.ship-state) "~","
  "~""  removeChars(w-ord.ship-ctry)  "~","
  "~""  removeChars(w-ord.ship-zip)   "~","
  "~""  removeChars(w-ord.sold-code)  "~","
  "~""  removeChars(w-ord.sold-name)  "~","
  "~""  removeChars(w-ord.sold-add1)  "~","
  "~""  removeChars(w-ord.sold-add2)  "~","
  "~""  removeChars(w-ord.sold-city)  "~","
  "~""  removeChars(w-ord.sold-state) "~","
  "~""  removeChars(w-ord.sold-ctry)  "~","
  "~""  removeChars(w-ord.sold-zip)   "~","
  "~""  removeChars(w-ord.i-name) FORMAT "X(30)"  "~","
  "~""  w-ord.due-date  "~","
  "~""  w-ord.rel-date  "~","
  "~""  w-ord.upc-no  "~","
  "~""  w-ord.box-len FORMAT ">>>9.99<<<" "~","
  "~""  w-ord.box-wid FORMAT ">>>9.99<<<" "~","
  "~""  w-ord.box-dep FORMAT ">>>9.99<<<" "~","
  "~""  w-ord.flute  "~","
  "~""  w-ord.test  "~","
  "~""  w-ord.vendor  "~","
  w-ord.gross-wt  ","
  w-ord.tare-wt  ","
  w-ord.net-wt  ","
  w-ord.sheet-wt  ","
  "~""  w-ord.uom  "~","
  "~""  removeChars(w-ord.style) "~","
  "~""  removeChars(w-ord.style-desc) "~","
  "~""  removeChars(w-ord.rel-lot#) "~","
  "~""  lv-middlesex-job  "~","
  "~""  lv-middlesex-po  "~","
  "~""  loadtag.tag-no "~"," 
  "~""  loadtag.partial "~","
  "~""  w-ord.cas-no  "~","
  "~""  removeChars(v-dept-note[1]) "~","
  "~""  removeChars(v-dept-note[2]) "~","
  "~""  removeChars(v-dept-note[3]) "~","
  "~""  removeChars(v-dept-note[4]) "~","
  "~""  removeChars(v-dept-note[5]) "~","
  "~""  removeChars(v-dept-note[6]) "~","
  "~""  removeChars(v-dept-note[7]) "~","
  "~""  removeChars(v-dept-note[8]) "~","
  w-ord.po-no ","
  "~""  removeChars(v-dept-note[9]) "~","
  "~""  removeChars(v-dept-note[10]) "~","
  "~""  removeChars(v-dept-note[11]) "~","
  "~""  removeChars(v-dept-note[12]) "~","
  "~""  removeChars(v-dept-note[13]) "~","
  "~""  removeChars(v-dept-note[14]) "~","
  "~""  removeChars(v-dept-note[15]) "~","
  "~""  removeChars(v-dept-note[16]) "~","   
  "~""  removeChars(v-dept-note[17]) "~","
  "~""  removeChars(v-dept-note[18]) "~","
  "~""  removeChars(w-ord.est-no) "~","
  "~""  removeChars(w-ord.ord-desc1)    "~","
  "~""  removeChars(w-ord.ord-desc2)    "~","
  .
 IF LOOKUP(v-loadtag,"ASI,SSLABEL") GT 0 THEN DO:
    
/*    FIND FIRST rfidtag OF loadtag NO-LOCK NO-ERROR.
    cRFIDTag = IF AVAIL rfidtag THEN rfidtag.rfidtag ELSE "".  */

    PUT UNFORMATTED "~"" SUBSTR(loadtag.tag-no,16,5) "~"," 
                   "~"" ipc-rfid "~"," .
 END.
 PUT UNFORMATTED 
    "~"" w-ord.due-date-jobhdr "~"," 
    "~"" w-ord.due-date-job "~","
 /* gdm - 08130804 */
    "~"" w-ord.linenum "~","
 /* gdm - 07170905 */
    "~"" w-ord.unit-wt  "~","
    "~"" w-ord.pallt-wt  "~","          
 
 /* gdm - 10160905 */
    "~"" removeChars(v-fgdsc1) "~","
    "~"" removeChars(v-fgdsc2) "~","
    "~"" removeChars(v-fgdsc3) "~","
    "~"" removeChars(w-ord.lot) "~","
 /*bpv 05231205 */   
    "~"" w-ord.pallt-no "~"," 
    "~"" ipi-pallet-id "~"," .

 PUT SKIP.



END PROCEDURE.


PROCEDURE create-loadtag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAM io-tag-no AS INT NO-UNDO.
  DEF INPUT PARAM ip-total-unit LIKE w-ord.total-unit NO-UNDO.
   /* DEF VAR io-tag-no AS INT NO-UNDO.
  DEF VAR ip-total-unit LIKE w-ord.total-unit NO-UNDO.*/

  DEF VAR SSPostFG-log AS CHAR NO-UNDO.
  DEF VAR SSPostFG-char AS CHAR  NO-UNDO.

  DEF BUFFER b-loadtag FOR loadtag.
  DEF BUFFER b-po-ordl FOR po-ordl.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-got-job AS LOG NO-UNDO.
  DEF VAR lv-out-cost AS DEC DECIMALS 4 NO-UNDO.
  DEF VAR lv-out-qty as dec no-undo.
  DEF VAR lv-from-uom AS CHAR NO-UNDO.
  DEF VAR lv-cost-uom AS CHAR NO-UNDO.
  DEF VAR lv-ord-qty AS INT NO-UNDO.
  DEF VAR lv-ord-uom AS CHAR NO-UNDO.
  DEF VAR lv-setup-included AS LOG NO-UNDO.
  DEF VAR lv-setup-per-cost-uom AS DEC NO-UNDO.
  def var v-bwt like po-ordl.s-len no-undo.
  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo.
  DEF VAR dRFIDTag AS DEC NO-UNDO.
  DEF BUFFER bf-eb FOR eb.

  IF tb_reprint-tag THEN DO:
     FIND FIRST loadtag NO-LOCK
         WHERE loadtag.company   EQ cocode
           AND loadtag.item-type EQ NO
           AND loadtag.tag-no    EQ TRIM(prmScnCs_lbl)
         USE-INDEX tag NO-ERROR.
     IF AVAIL loadtag THEN
       io-tag-no = (IF AVAIL loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.
     RETURN.
  END.

  FIND FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-ord.i-no
      NO-ERROR.

  FIND LAST loadtag NO-LOCK
      WHERE loadtag.company     EQ cocode
        AND loadtag.item-type   EQ NO
        AND loadtag.is-case-tag EQ NO
        AND loadtag.tag-no      BEGINS w-ord.i-no 
        AND SUBSTR(loadtag.tag-no,1,15) EQ w-ord.i-no
      USE-INDEX tag NO-ERROR.
  io-tag-no = (IF AVAIL loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.
  CREATE loadtag.
  ASSIGN
   loadtag.company      = cocode
   loadtag.tag-no       = /*string(io-tag-no,"99999") + STRING(w-ord.i-no,"x(15)") */
                          STRING(CAPS(w-ord.i-no),"x(15)") + STRING(io-tag-no,"99999") 
   loadtag.item-type    = NO /*FGitem*/
   loadtag.job-no       = w-ord.job-no
   loadtag.job-no2      = w-ord.job-no2
   loadtag.ord-no       = IF can-find(FIRST cust WHERE cust.company = cocode
                                      AND cust.cust-no = itemfg.cust-no
                                      AND cust.active = "X")
                          THEN 0 ELSE w-ord.ord-no /* task# 07120508*/
   loadtag.i-no         = CAPS(w-ord.i-no)
   loadtag.i-name       = w-ord.i-name
   loadtag.qty          = w-ord.ord-qty
   loadtag.qty-case     = w-ord.pcs
   loadtag.case-bundle  = w-ord.bundle
   loadtag.pallet-count = ip-total-unit /*w-ord.pcs * w-ord.bundle*/
   loadtag.partial      = w-ord.partial /*w-ord.total-unit MOD w-ord.pcs*/
   loadtag.sts = "Printed"  /* task 10190414 */
   loadtag.tag-date = TODAY
   loadtag.tag-time = TIME
   /* gdm - 07170905 */
   loadtag.misc-dec[1] = w-ord.unit-wt 
   loadtag.misc-dec[2] = w-ord.pallt-wt
   loadtag.misc-char[2] = w-ord.lot.
   /* gdm - 07170905  end */

   /* gdm - 08260916 */
   IF loadtagFunction EQ 'PO' 
    THEN loadtag.po-no = INT(w-ord.po-no).   
   

  IF v-fgrecpt AND tb_ret THEN loadtag.tot-cases  = (loadtag.pallet-COUNT - loadtag.partial) / loadtag.case-bundle.

  IF v-loadtag = "CentBox" THEN DO:
     ASSIGN loadtag.loc = itemfg.def-loc
            loadtag.loc-bin = itemfg.def-loc-bin.
     FIND FIRST fg-bin WHERE fg-bin.company EQ itemfg.company
                          AND fg-bin.i-no    EQ itemfg.i-no
                          AND fg-bin.job-no  EQ w-ord.job-no
                          AND fg-bin.tag = loadtag.tag-no
                         NO-LOCK NO-ERROR.
      IF AVAIL fg-bin THEN
         ASSIGN loadtag.loc     = fg-bin.loc
                loadtag.loc-bin = fg-bin.loc-bin.
                
  END.
  ELSE RUN fg/autopost.p (ROWID(itemfg), w-ord.job-no, w-ord.job-no2,
                         OUTPUT loadtag.loc , OUTPUT loadtag.loc-bin).
  
  IF RFIDTag-log THEN DO:
     FIND FIRST oe-ctrl WHERE oe-ctrl.company = loadtag.company NO-ERROR.
     dRFIDTag = IF AVAIL oe-ctrl AND oe-ctrl.spare-char-1 <> "" 
                    THEN dec(oe-ctrl.spare-char-1) ELSE 111110000000000000000001. 
     oe-ctrl.spare-char-1 = string(dRFIDTag + 1).
     CREATE rfidtag.
     ASSIGN rfidtag.company = loadtag.company
            rfidtag.item-type = loadtag.item-type
            rfidtag.tag-no = loadtag.tag-no
            rfidtag.rfidtag = string(dRFIDTag).
     RELEASE oe-ctrl.
  END.
  
  IF v-fgrecpt AND NOT tb_ret THEN DO:
    IF AVAIL itemfg THEN DO:
      li = 0.
      FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
      IF AVAIL fg-rctd AND fg-rctd.r-no GT li THEN li = fg-rctd.r-no.

      FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
      IF AVAIL fg-rcpth AND fg-rcpth.r-no GT li THEN li = fg-rcpth.r-no.

      DO WHILE TRUE:
        li = li + 1.
        FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ li USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAIL fg-rcpth THEN NEXT.
        FIND FIRST fg-rctd WHERE fg-rctd.r-no EQ li USE-INDEX fg-rctd NO-LOCK NO-ERROR.
        IF AVAIL fg-rctd THEN NEXT.
        LEAVE.
      END.

      CREATE fg-rctd.
      ASSIGN
       fg-rctd.r-no       = li + 1
       fg-rctd.rct-date   = TODAY
       fg-rctd.trans-time = TIME
       fg-rctd.company    = cocode
       fg-rctd.rita-code  = "R"
       fg-rctd.i-name     = itemfg.i-name
       fg-rctd.i-no       = loadtag.i-no
       fg-rctd.job-no     = loadtag.job-no
       fg-rctd.job-no2    = loadtag.job-no2
       fg-rctd.t-qty      = loadtag.pallet-count /*loadtag.qty*/
       fg-rctd.pur-uom    = itemfg.prod-uom
       fg-rctd.cost-uom   = itemfg.prod-uom
  /*     fg-rctd.std-cost   = IF AVAIL fg-bin THEN fg-bin.std-tot-cost ELSE itemfg.std-tot-cost */
       fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
       fg-rctd.qty-case   = loadtag.qty-case
       fg-rctd.partial    = loadtag.partial
       fg-rctd.cases      = TRUNC(fg-rctd.t-qty / fg-rctd.qty-case,0)
       fg-rctd.cases-unit = loadtag.case-bundle
       fg-rctd.loc        = loadtag.loc
       fg-rctd.loc-bin    = loadtag.loc-bin
       fg-rctd.tag        = loadtag.tag-no
       fg-rctd.stack-code = loadtag.misc-char[2]
       fg-rctd.tot-wt     = loadtag.misc-dec[1] .

      IF loadtagFunction EQ 'PO' THEN
         fg-rctd.po-no = TRIM(STRING(loadtag.po-no,">>>>>>>>>>")).

      RELEASE job.
      RELEASE reftable.

      IF TRIM(fg-rctd.job-no) NE "" THEN
      FIND FIRST job
          WHERE job.company EQ fg-rctd.company
            AND job.job-no  EQ fg-rctd.job-no
            AND job.job-no2 EQ fg-rctd.job-no2
          USE-INDEX job NO-LOCK NO-ERROR.

      IF AVAIL job THEN DO:
        FIND FIRST job-hdr NO-LOCK
            WHERE job-hdr.company EQ cocode
              AND job-hdr.job-no  EQ loadtag.job-no
              AND job-hdr.job-no2 EQ loadtag.job-no2
              AND job-hdr.i-no    EQ itemfg.i-no
            NO-ERROR.
        IF AVAIL job-hdr THEN fg-rctd.std-cost = job-hdr.std-tot-cost.

        ELSE
        FIND FIRST reftable
            WHERE reftable.reftable EQ "jc/jc-calc.p"
              AND reftable.company  EQ job.company
              AND reftable.loc      EQ ""
              AND reftable.code     EQ STRING(job.job,"999999999")
              AND reftable.code2    EQ fg-rctd.i-no
            USE-INDEX reftable NO-LOCK NO-ERROR.

        IF AVAIL reftable AND reftable.val[5] NE 0 THEN
          fg-rctd.std-cost = reftable.val[5].
      END.

      IF NOT AVAIL job-hdr AND NOT AVAIL reftable THEN DO:
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ itemfg.company
              AND fg-bin.i-no    EQ itemfg.i-no
              AND fg-bin.job-no  EQ loadtag.job-no
              AND fg-bin.job-no2 EQ loadtag.job-no2 
              /*AND fg-bin.tag = loadtag.tag-no*/
            NO-LOCK NO-ERROR.
        fg-rctd.std-cost = IF AVAIL fg-bin THEN fg-bin.std-tot-cost
                                           ELSE itemfg.std-tot-cost.
      END.
      IF w-ord.po-no NE 0 AND (loadtagFunction EQ 'Order' OR
                               loadtagFunction EQ 'PO') THEN
      DO:
         fg-rctd.po-no = TRIM(STRING(w-ord.po-no,">>>>>>>>>>")).
        
         FIND FIRST b-po-ordl WHERE
              b-po-ordl.company EQ cocode AND
              b-po-ordl.po-no EQ w-ord.po-no AND
              b-po-ordl.item-type EQ NO AND
              b-po-ordl.i-no EQ loadtag.i-no
              NO-LOCK NO-ERROR.
        
         IF AVAIL b-po-ordl THEN
         DO:
            lv-setup-included = NO.
            lv-ord-qty = b-po-ordl.ord-qty.
            lv-ord-uom = b-po-ordl.pr-qty-uom.
            RUN rm/convquom.p(lv-ord-uom, 'EA',                   
                v-bwt, v-len, v-wid, v-dep,
                lv-ord-qty, OUTPUT lv-ord-qty).

            ASSIGN
               fg-rctd.cost-uom = b-po-ordl.pr-uom
               lv-out-cost = b-po-ordl.cost * (IF b-po-ordl.disc NE 0 THEN (1 - (b-po-ordl.disc / 100)) ELSE 1).
               lv-out-qty = (fg-rctd.qty-case * fg-rctd.cases) + fg-rctd.partial.

            /* if the quantity is less than po quantity, assume setup cost is 
               included in the po cost */
            IF (fg-rctd.qty-case * fg-rctd.cases) < lv-ord-qty THEN
               ASSIGN lv-out-cost = b-po-ordl.cons-cost
                      lv-from-uom = b-po-ordl.cons-uom
                      lv-setup-included = YES.
            ELSE 
              lv-from-uom = fg-rctd.cost-uom.

            RUN convert-vend-comp-curr(INPUT b-po-ordl.po-no, INPUT-OUTPUT lv-out-cost).
        
            ASSIGN
               fg-rctd.std-cost = lv-out-cost
               lv-cost-uom = itemfg.prod-uom
               v-len = b-po-ordl.s-len
               v-wid = b-po-ordl.s-wid.

            IF lv-from-uom EQ "L" THEN
               ASSIGN
                  lv-from-uom = "EA"
                  lv-out-cost = lv-out-cost / lv-out-qty.

            /* convert cost pr-uom*/
            IF lv-from-uom EQ lv-cost-uom OR
               (LOOKUP(lv-from-uom,fg-uom-list) GT 0 AND
                LOOKUP(lv-cost-uom,fg-uom-list) GT 0) THEN.
            ELSE
               RUN rm/convcuom.p(lv-from-uom, lv-cost-uom,                   
                                 v-bwt, v-len, v-wid, v-dep,
                                 lv-out-cost, OUTPUT lv-out-cost).

            IF LOOKUP(lv-cost-uom,fg-uom-list) EQ 0 THEN
               RUN rm/convquom.p("EA", lv-cost-uom,                   
                                 v-bwt, v-len, v-wid, v-dep,
                                 lv-out-qty, OUTPUT lv-out-qty).
            IF lv-setup-included THEN
                lv-setup-per-cost-uom = 0.
            ELSE 
               lv-setup-per-cost-uom = b-po-ordl.setup / lv-out-qty.

            ASSIGN
               fg-rctd.cost-uom = lv-cost-uom
               fg-rctd.std-cost = lv-out-cost + lv-setup-per-cost-uom.
               fg-rctd.ext-cost = (lv-out-qty * lv-out-cost) + fg-rctd.frt-cost
                                  + lv-setup-per-cost-uom.

            IF fgpofrt-log THEN 
            DO:
               RUN get-freight-cost (OUTPUT fg-rctd.frt-cost).
               fg-rctd.ext-cost = fg-rctd.ext-cost + fg-rctd.frt-cost.
            END.
         END.
      END. /*info from PO on Order*/

      ELSE DO:
         RUN calc-ext-cost .
      END.

    END.  /* avail itemfg */
        /* mdp adds logic to post loadtags 07/24/08 */

    

  /*  IF SSPostFG-log AND 
      SSPostFG-char = "Loadtag" THEN DO:
      IF AVAIL fg-rctd THEN DO:
         ASSIGN 
            lv-r-no  = fg-rctd.r-no
            fg-rctd.r-no      = 0
            fg-rctd.r-no      = lv-r-no
            fg-rctd.rita-code = "R"
            fg-rctd.post-date = TODAY.
            /** not quite sure what this does
             fg-rctd.tag2      = w-fg-rctd.tag2. **/

         FOR EACH fg-rcpts WHERE 
             fg-rcpts.company EQ fg-rctd.company AND
             fg-rcpts.r-no    EQ fg-rctd.r-no EXCLUSIVE-LOCK:
             fg-rcpts.rita-code = fg-rctd.rita-code.
         END.
            
         RUN oerep/r-ltpost.p (INPUT ROWID(fg-rctd),NO).
      
      END. /* if avail fg-rctd */
    END.  */
    /* gdm - */
    IF v-fgrecpt AND w-ord.est-no NE "" AND AVAIL fg-rctd THEN DO:
      FIND FIRST bf-eb
        WHERE bf-eb.company  EQ cocode
          AND bf-eb.est-no   EQ w-ord.est-no
          AND bf-eb.stock-no EQ w-ord.i-no NO-LOCK NO-ERROR.
      IF AVAIL bf-eb THEN DO:
        IF bf-eb.pur-man THEN DO:
          IF TRIM(fg-rctd.job-no) NE "" AND TRIM(fg-rctd.po-no) NE "" THEN
             ASSIGN fg-rctd.job-no = "" 
                    fg-rctd.job-no2 = 0
                    fg-rctd.po-no = TRIM(STRING(w-ord.po-no,">>>>>>>>>>")).
        END.
        ELSE IF NOT bf-eb.pur-man THEN DO:
          IF TRIM(fg-rctd.po-no) NE "" AND TRIM(loadtag.job-no) NE "" THEN
             ASSIGN fg-rctd.po-no   = ""
                    fg-rctd.job-no  = loadtag.job-no
                    fg-rctd.job-no2 = loadtag.job-no2.
        END.
      END.
    END.
    /* gdm - */
    /*BV - added the following call to add Set Parts to IU1 ( */
    RUN fg/comprcpt.p (ROWID(fg-rctd)).
   /* mdp posting logic ends here */
  END.  /* v-fgrecpt */
  

  ELSE IF v-fgrecpt AND tb_ret AND AVAIL itemfg THEN DO:
      
       RUN post-return (RECID(fg-rctd)).

  END.

  
  FIND CURRENT loadtag NO-LOCK NO-ERROR.
  FIND CURRENT fg-rctd NO-LOCK NO-ERROR.
  

END PROCEDURE.



PROCEDURE incrementPalletID :
/*------------------------------------------------------------------------------
  Purpose:     Increment the pallet number for a given customer and return the
                new value
  Parameters:  INPUT: cust buffer OUTPUT: next pallet #
  Notes:       Defaults value if not set for given cust
               Returns error code of -1   
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipb-cust FOR cust.
DEFINE INPUT PARAMETER ipi-tags AS INT NO-UNDO.
DEFINE OUTPUT PARAMETER op-start-pallet-no LIKE cust.spare-int-1.
DEFINE OUTPUT PARAMETER op-end-pallet-no LIKE cust.spare-int-1.

/*DEFINE BUFFER ipb-cust FOR cust.
DEFINE VAR ipi-tags AS INT NO-UNDO.
DEFINE VAR op-start-pallet-no LIKE cust.spare-int-1.
DEFINE VAR op-end-pallet-no LIKE cust.spare-int-1.*/



DEF BUFFER bf-cust FOR cust.
DEF VAR li AS INT INIT 0.
DEF VAR lj AS INT INIT 0.

FIND bf-cust WHERE ROWID(bf-cust) EQ ROWID(ipb-cust) EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL bf-cust THEN DO:
    op-end-pallet-no = 0.
    RETURN.
END.

IF bf-cust.spare-int-1 EQ 0 THEN DO:
    op-end-pallet-no = 0.
    RETURN.
END.
li = bf-cust.spare-int-1.
IF li MOD 1000000 = 999999 THEN DO:
        /*protection code*/
    op-end-pallet-no = -1.
    RETURN.
END.
op-start-pallet-no = li + 1.
DO lj = 1 TO ipi-tags:
    li = li + 1.
    IF li MOD 1000000 = 999999 THEN DO:
        /*protection code*/
        op-end-pallet-no = -1.
        RETURN.
    END.
END.

ASSIGN 
    op-end-pallet-no = li
    bf-cust.spare-int-1 = li.
END PROCEDURE.


PROCEDURE run-report :

MESSAGE "test  " .
{oerep/r-loadtg1.i}

RUN AutoPrint.
                                                                   


end procedure.

/*PROCEDURE temp-ord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*DEF INPUT PARAM ip-ord-no LIKE oe-ord.ord-no NO-UNDO.*/
    DEF VAR lv-ord-no LIKE oe-ord.ord-no NO-UNDO.

    DO i = 1 TO NUM-ENTRIES(v-ord-list).
    lv-ord-no = INT(ENTRY(i,v-ord-list)) NO-ERROR.

  FOR EACH oe-ord
      WHERE oe-ord.company EQ cocode
        AND oe-ord.ord-no  EQ lv-ord-no
      NO-LOCK:
    /*RUN temp-create (ROWID(oe-ord)).*/
    
    CREATE w-file.
    w-key = (ROWID(oe-ord)).

  END.

END PROCEDURE. */

PROCEDURE run-barone :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*DEF INPUT PARAM ip-TagText AS cha NO-UNDO.*/
   
   DEFINE VARIABLE iReturnResult AS INTEGER NO-UNDO.
   DEFINE VARIABLE cProgramName AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

   cProgramName =  "c:\program files\bar-one 6 pro-plus\labels.exe ".
   cFileName    = "R:\ASI_GUI9\SOURCE\custom\century.lab".


  /* OS-COPY VALUE(ip-tagtext) VALUE("r:\asi_gui9\source\custom\"). */
   RUN WinExec (INPUT cProgramName + CHR(32) + cFileName , INPUT 1, OUTPUT
   iReturnResult).
/*
   IF iReturnResult >= 32 THEN
     MESSAGE "Application was Started" VIEW-AS ALERT-BOX.
   ELSE
     MESSAGE "Application Failed:" iReturnResult VIEW-AS ALERT-BOX.

*/
END PROCEDURE.

PROCEDURE run-lmw :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*DEF INPUT PARAM ip-TagText AS cha NO-UNDO.*/
   
   DEFINE VARIABLE iReturnResult AS INTEGER NO-UNDO.
   DEFINE VARIABLE cProgramName AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

/*   cProgramName =  "c:\program files\bar-one 6 pro-plus\labels.exe ".
*/
   cFileName    = "custom\interpack.qdf".
   FILE-INFO:FILE-NAME = cFileName.
   cFileName = FILE-INFO:FULL-PATHNAME.

   RUN custom/runlmw.p (OUTPUT cprogramname).

/*   OS-COPY VALUE(ip-tagtext) VALUE("c:\tmp\").*/

   RUN WinExec (INPUT cProgramName + CHR(32) + cFileName , INPUT 1, OUTPUT
   iReturnResult).
/*
   IF iReturnResult >= 32 THEN
     MESSAGE "Application was Started" VIEW-AS ALERT-BOX.
   ELSE
     MESSAGE "Application Failed:" iReturnResult VIEW-AS ALERT-BOX.

*/

END PROCEDURE.

PROCEDURE temp-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

   /* DEF VAR ip-rowid AS ROWID NO-UNDO.*/
  
  CREATE w-file.
  w-key = ip-rowid.

END PROCEDURE.


PROCEDURE WinExec EXTERNAL "KERNEL32.DLL":
     /*DEFINE INPUT  PARAMETER ProgramName AS CHARACTER.
     DEFINE INPUT  PARAMETER VisualStyle AS LONG.
     DEFINE RETURN PARAMETER StatusCode  AS LONG.*/
END PROCEDURE.

PROCEDURE from-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAM op-warning AS LOG NO-UNDO.

   /* DEF VAR ip-rowid AS ROWID NO-UNDO.
  DEF VAR op-warning AS LOG NO-UNDO.*/

  DEF BUFFER b-job-hdr-2 FOR job-hdr.

  DEF VAR lv-rel-date AS DATE NO-UNDO.
  DEF VAR lv-tt-created AS LOG NO-UNDO.

  FIND FIRST job NO-LOCK
      WHERE ROWID(job) EQ ip-rowid
        AND (v-stat EQ "A"                      OR
             (v-stat EQ "C" AND NOT job.opened) OR
             (v-stat EQ "O" AND job.opened))
      NO-ERROR.
  
  IF NOT AVAILABLE job THEN RETURN.
  IF (v-ord-list NE '' OR begin_ord-no NE 0 OR end_ord-no NE 0) AND
     NOT CAN-FIND(ttblJob WHERE ttblJob.company EQ job.company
                            AND ttblJob.job-no EQ job.job-no
                            AND ttblJob.job-no2 EQ job.job-no2) THEN DO:

    FOR EACH b-job-hdr-2 FIELDS(company job-no job-no2 ord-no) WHERE
        b-job-hdr-2.company EQ job.company AND
        b-job-hdr-2.job     EQ job.job AND
        b-job-hdr-2.job-no  EQ job.job-no AND
        b-job-hdr-2.job-no2 EQ job.job-no2 AND
        b-job-hdr-2.i-no    GE v-fitem[1] AND
        b-job-hdr-2.i-no    LE v-fitem[2]
        NO-LOCK:
    
       IF NOT CAN-FIND(FIRST ttblJob WHERE
          ttblJob.company EQ b-job-hdr-2.company AND
          ttblJob.job-no EQ b-job-hdr-2.job-no AND
          ttblJob.job-no2 EQ b-job-hdr-2.job-no2 AND
          ttblJob.ord-no  EQ b-job-hdr-2.ord-no) THEN
          DO:
             CREATE ttblJob.
             ASSIGN
                ttblJob.company = b-job-hdr-2.company
                ttblJob.job-no = b-job-hdr-2.job-no
                ttblJob.job-no2 = b-job-hdr-2.job-no2
                ttblJob.ord-no  = b-job-hdr-2.ord-no
                lv-tt-created = YES.
             RELEASE ttblJob.
          END.
    END.

    IF lv-tt-created THEN
       RETURN.
  END.
    
    IF AVAIL job THEN
    FOR EACH job-hdr
        WHERE job-hdr.company EQ job.company
          AND job-hdr.job     EQ job.job
          AND job-hdr.job-no  EQ job.job-no
          AND job-hdr.job-no2 EQ job.job-no2
          AND job-hdr.i-no    GE v-fitem[1]
          AND job-hdr.i-no    LE v-fitem[2]
         /*AND job-hdr.ord-no  EQ 0
        USE-INDEX ord-no*/
        /*ESP - Task 04180703 don't look at order number */
        
        NO-LOCK,
        FIRST cust
        WHERE cust.company eq cocode
          AND cust.cust-no eq job-hdr.cust-no
        NO-LOCK,
        FIRST itemfg
        WHERE itemfg.company eq cocode
          AND itemfg.i-no    eq job-hdr.i-no
        NO-LOCK:

          CREATE w-ord.
          ASSIGN
            w-ord.ord-no       = job-hdr.ord-no
            w-ord.job-no       = job-hdr.job-no
            w-ord.job-no2      = job-hdr.job-no2
            w-ord.cust-no      = cust.cust-no
            w-ord.cust-name    = cust.name
            w-ord.i-no         = job-hdr.i-no
            w-ord.cust-part-no = itemfg.part-no
            w-ord.over-pct     = IF tb_over THEN cust.over-pct ELSE 0
            w-ord.qty-before   = job-hdr.qty
            w-ord.ord-qty      = w-ord.qty-before *
                                 (1 + (w-ord.over-pct / 100))
            w-ord.i-name       = itemfg.i-name
            w-ord.upc-no       = itemfg.upc-no
            w-ord.due-date     = job.start-date
            w-ord.est-no       = job.est-no
            w-ord.form-no      = job-hdr.frm
            w-ord.upc-no       = itemfg.upc-no
            w-ord.box-len      = itemfg.l-score[50]
            w-ord.box-wid      = itemfg.w-score[50]
            w-ord.box-dep      = itemfg.d-score[50]
            w-ord.style        = itemfg.style
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = if cust.int-field[1] ne 0 then
                                   cust.int-field[1] else v-mult
            num-rec            = num-rec + 1
            w-ord.due-date-job = IF job.due-date <> ? THEN STRING(job.due-date, "99/99/9999") ELSE "".
            w-ord.due-date-jobhdr = IF job-hdr.due-date <> ? THEN STRING(job-hdr.due-date, "99/99/9999") ELSE "".

              ASSIGN
              w-ord.rec_key = STRING(TODAY,"99999999") + STRING(prmScnCs_lbl,"99999999") + string(create-count)   
              create-count = create-count + 1.

      
          IF w-ord.style NE "" THEN
          DO:
             FIND FIRST style WHERE
                  style.company EQ cocode AND
                  style.style EQ w-ord.style
                  NO-LOCK NO-ERROR.
          
             IF AVAIL style THEN
             DO:
                w-ord.style-desc = style.dscr.
                RELEASE style.
             END.
          END.

          IF job-hdr.ord-no NE 0 THEN
          DO:
             FIND FIRST oe-ordl WHERE
                  oe-ordl.company EQ cocode AND
                  oe-ordl.ord-no  EQ job-hdr.ord-no AND
                  oe-ordl.i-no    EQ job-hdr.i-no
                  NO-LOCK NO-ERROR.
            
             IF AVAIL oe-ordl THEN
             DO:
                FIND FIRST oe-ord WHERE
                     oe-ord.company EQ cocode AND
                     oe-ord.ord-no  EQ job-hdr.ord-no
                     NO-LOCK NO-ERROR.

                RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                                  OUTPUT w-ord.rel-date,
                                  OUTPUT w-ord.rel-lot#).
                IF tb_xfer-lot THEN w-ord.lot# = w-ord.rel-lot#.

                ASSIGN
                 w-ord.ord-desc1    = oe-ordl.part-dscr1
                 w-ord.ord-desc2    = oe-ordl.part-dscr2.

                RELEASE oe-ordl.
                RELEASE oe-ord.
             END.
          END.
          ELSE
             op-warning = YES.
          /*IF NOT tb_ship-id THEN v-ship-id = job-hdr.cust-no.*/
          FOR EACH shipto
              WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ job-hdr.cust-no
              USE-INDEX ship-id NO-LOCK
              BREAK BY shipto.ship-no DESC:
           /* IF shipto.ship-id EQ v-ship-id OR
               LAST(shipto.ship-no)              THEN DO:
              ASSIGN
               w-ord.ship-code  = shipto.ship-id
               w-ord.ship-name  = shipto.ship-name
               w-ord.ship-add1  = shipto.ship-add[1]
               w-ord.ship-add2  = shipto.ship-add[2]
               w-ord.ship-city  = shipto.ship-city
               w-ord.ship-state = shipto.ship-state
               w-ord.ship-ctry  = shipto.country
               w-ord.ship-zip   = shipto.ship-zip.
              LEAVE.
            END.*/
          END.

          FIND FIRST est
              WHERE est.company eq job.company
                AND est.est-no  eq job.est-no
              NO-LOCK NO-ERROR.
          RELEASE eb.
          IF AVAIL est THEN
          FIND FIRST eb
              WHERE eb.company   EQ est.company
                AND eb.est-no    EQ est.est-no
                AND eb.form-no   EQ job-hdr.frm
                AND (eb.blank-no EQ job-hdr.blank-no OR job-hdr.blank-no EQ 0)
              NO-LOCK NO-ERROR.

          IF AVAIL eb THEN
            ASSIGN
             w-ord.flute      = eb.flute
             w-ord.test       = eb.test
             w-ord.pcs        = eb.cas-cnt
             w-ord.bundle     = eb.cas-pal
             w-ord.total-unit = w-ord.pcs * w-ord.bundle
             w-ord.partial    = 0 /* w-ord.ord-qty - w-ord.total-unit*/
             w-ord.cas-no     = eb.cas-no
             w-ord.pallt-no   = eb.tr-no.

          IF NOT v-oecount THEN
            ASSIGN
             w-ord.pcs    = w-ord.total-unit
             w-ord.bundle = 1.

          /* Add .49 to round up and add 1 for extra tag   */
          w-ord.total-tags = ((job-hdr.qty / w-ord.total-unit) + .49) +  IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1.
    END.
    
END PROCEDURE.

PROCEDURE convert-vend-comp-curr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-po-no AS INT NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER ip-cost AS DEC DECIMALS 4 NO-UNDO.

   /*DEFINE VAR ip-po-no AS INT NO-UNDO.
   DEFINE VAR ip-cost AS DEC DECIMALS 4 NO-UNDO.*/
   
   DEF BUFFER b-po-ord FOR po-ord.
   DEF BUFFER b-company FOR company.

   FIND FIRST b-po-ord WHERE
        b-po-ord.company EQ cocode AND
        b-po-ord.po-no EQ ip-po-no
        NO-LOCK NO-ERROR.

   IF AVAIL b-po-ord THEN
   DO:
      FIND FIRST vend WHERE
           vend.company EQ b-po-ord.company AND
           vend.vend-no EQ b-po-ord.vend-no
           NO-LOCK NO-ERROR.

      IF AVAIL vend THEN
      DO:
         FIND FIRST b-company WHERE
              b-company.company EQ cocode
              NO-LOCK.

         IF vend.curr-code NE b-company.curr-code THEN
         DO:
            FIND FIRST currency WHERE
                 currency.company EQ b-po-ord.company AND
                 currency.c-code EQ vend.curr-code
                 NO-LOCK NO-ERROR.

            IF AVAIL currency THEN
            DO:
               ip-cost = ip-cost * currency.ex-rate.
               RELEASE currency.
            END.
         END.

         RELEASE b-company.
         RELEASE vend.
      END.

      RELEASE b-po-ord.
   END.
END PROCEDURE.
PROCEDURE get-freight-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF OUTPUT PARAM op-cost LIKE fg-rctd.frt-cost NO-UNDO.

   /* DEF VAR op-cost LIKE fg-rctd.frt-cost NO-UNDO.*/

   DEF BUFFER b-po-ordl-2 FOR po-ordl.

   FIND FIRST b-po-ordl-2 WHERE
        b-po-ordl-2.company   EQ fg-rctd.company AND
        b-po-ordl-2.po-no     EQ INT(fg-rctd.po-no) AND
        b-po-ordl-2.i-no      EQ fg-rctd.i-no AND
        b-po-ordl-2.job-no    EQ fg-rctd.job-no AND
        b-po-ordl-2.job-no2   EQ fg-rctd.job-no2 AND
        b-po-ordl-2.item-type EQ NO
        NO-LOCK NO-ERROR.
  
   IF AVAIL b-po-ordl-2 THEN
      RUN po/getfrtcs.p (ROWID(b-po-ordl-2),
                         fg-rctd.t-qty,
                         OUTPUT op-cost).
  
   RUN convert-vend-comp-curr(INPUT b-po-ordl-2.po-no, INPUT-OUTPUT op-cost).

END PROCEDURE.

PROCEDURE calc-ext-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  def var lv-out-qty as dec no-undo.
  def var lv-out-cost as dec no-undo.
  DEF VAR lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.
  DEF VAR v-rec-qty AS INT NO-UNDO.


  if not avail fg-rctd then return.  /* no records */


find itemfg where itemfg.company eq cocode and itemfg.i-no  eq fg-rctd.i-no
            use-index i-no no-lock no-error.

ASSIGN
 lv-cost-uom = itemfg.prod-uom
 v-bwt       = 0
 v-len       = itemfg.t-len
 v-wid       = itemfg.t-wid
 v-dep       = 0.

  find first po-ordl where po-ordl.company = fg-rctd.company
                       and po-ordl.po-no = int(fg-rctd.po-no)
                       and po-ordl.i-no  = fg-rctd.i-no
                       and po-ordl.job-no = fg-rctd.job-no
                       and po-ordl.job-no2 = fg-rctd.job-no2
                       and po-ordl.item-type = no
                       no-lock no-error.
  
  IF AVAIL po-ordl THEN DO:
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid.
  END.
  
  ASSIGN lv-out-qty  = fg-rctd.t-qty
         lv-out-cost = fg-rctd.std-cost.

  IF fg-rctd.cost-uom NE lv-cost-uom THEN
    RUN rm/convcuom.p(fg-rctd.cost-uom, lv-cost-uom,                   
                      v-bwt, v-len, v-wid, v-dep,
                      fg-rctd.std-cost, OUTPUT lv-out-cost).

  IF lv-cost-uom NE "EA" THEN
     RUN rm/convquom.p("EA", lv-cost-uom,                   
                       v-bwt, v-len, v-wid, v-dep,
                       lv-out-qty, OUTPUT lv-out-qty).
ASSIGN fg-rctd.ext-cost = (lv-out-qty * lv-out-cost) + fg-rctd.frt-cost.
  
END PROCEDURE.

PROCEDURE post-return :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM ip-fg-recid AS RECID NO-UNDO.

   DEF BUFFER bf-fg-rctd FOR fg-rctd.
   DEF BUFFER b-fg-bin FOR fg-bin.
   DEF BUFFER b-itemfg FOR itemfg.

   DEF VAR li AS INT NO-UNDO.   
   def var v-dec as dec decimals 10 NO-UNDO.
   def var v-overrun-qty like fg-rctd.qty no-undo.
   def var v-underrun-qty like fg-rctd.qty no-undo.
   DEF VAR v-reduce-qty AS INT NO-UNDO.
   DEF var v-newhdr as log no-undo. 
   def var v-fin-qty as dec no-undo.
   DEF VAR v-est-no AS cha NO-UNDO.
   DEF VAR v-one-item AS LOG NO-UNDO.
   def var ld-cvt-qty as dec no-undo.
   def var ld-cvt-cost as dec no-undo.
   DEF VAR v-binqty AS INT NO-UNDO.
   DEF VAR v-qty AS INT NO-UNDO.
   DEF VAR v-tagcost AS DEC NO-UNDO.
   DEF VAR v-cost AS DEC NO-UNDO.
   DEF VAR choice AS LOG NO-UNDO.
   DEF VAR v-post-date AS DATE INIT TODAY NO-UNDO.
   DEF VAR fg-uom-list AS cha NO-UNDO.
   DEF VAR li-tag-no AS INT NO-UNDO.
   DEF VAR ll-qty-changed AS LOG NO-UNDO.
   DEF VAR ll-whs-item AS LOG NO-UNDO.


   RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).
    
   FOR EACH w-fg-rctd:
       DELETE w-fg-rctd.
   END.

   /* create w/h transfer record*/   
   FIND FIRST itemfg WHERE itemfg.company EQ cocode
                    AND itemfg.i-no    EQ loadtag.i-no NO-ERROR.

   li = 1.
   FOR EACH bf-fg-rctd NO-LOCK BY bf-fg-rctd.r-no DESC:
       LEAVE.
   END.
   IF AVAIL bf-fg-rctd then li = bf-fg-rctd.r-no.

   FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
   IF AVAIL fg-rcpth AND fg-rcpth.r-no GT li THEN li = fg-rcpth.r-no.
   FIND FIRST fg-bin WHERE fg-bin.company = cocode
                       AND fg-bin.i-no = loadtag.i-no
                       AND fg-bin.tag = ""
                       AND fg-bin.qty >= loadtag.pallet-count NO-LOCK NO-ERROR.
   IF NOT AVAIL fg-bin THEN RETURN.  

   CREATE bf-fg-rctd.
   ASSIGN
       bf-fg-rctd.r-no       = li + 1
       bf-fg-rctd.rct-date   = TODAY
       bf-fg-rctd.trans-time = TIME
       bf-fg-rctd.company    = cocode
       bf-fg-rctd.rita-code  = "I"
       bf-fg-rctd.i-name     = itemfg.i-name
       bf-fg-rctd.i-no       = loadtag.i-no
       bf-fg-rctd.job-no     = loadtag.job-no
       bf-fg-rctd.job-no2    = loadtag.job-no2
       bf-fg-rctd.t-qty      = loadtag.pallet-count /*loadtag.qty*/
       bf-fg-rctd.pur-uom    = itemfg.prod-uom
       bf-fg-rctd.cost-uom   = itemfg.prod-uom
  /*     bf-fg-rctd.std-cost   = IF AVAIL fg-bin THEN fg-bin.std-tot-cost ELSE itemfg.std-tot-cost */
       bf-fg-rctd.ext-cost   = (bf-fg-rctd.t-qty / 1000) * bf-fg-rctd.std-cost
       bf-fg-rctd.qty-case   = loadtag.qty-case
       
       bf-fg-rctd.partial    = loadtag.partial
       bf-fg-rctd.cases      = TRUNC(bf-fg-rctd.t-qty / bf-fg-rctd.qty-case,0)
       bf-fg-rctd.cases-unit = loadtag.case-bundle
       bf-fg-rctd.loc        = loadtag.loc
       bf-fg-rctd.loc-bin    = loadtag.loc-bin
       bf-fg-rctd.tag        = loadtag.tag-no
       bf-fg-rctd.loc2        = ""
       bf-fg-rctd.loc-bin2    = ""
       bf-fg-rctd.tag2        = ""
       .
 /* post later*/
   CREATE w-fg-rctd.
   BUFFER-COPY bf-fg-rctd TO w-fg-rctd.
   ASSIGN w-fg-rctd.row-id = ROWID(bf-fg-rctd).
   {fg/fg-post.i w-fg-rctd w-fg-rctd}

   FIND CURRENT po-ordl NO-LOCK NO-ERROR.
   FIND CURRENT fg-bin NO-LOCK NO-ERROR.
   FIND CURRENT itemfg NO-LOCK NO-ERROR.

   ASSIGN bf-fg-rctd.rita-code = "P"  /* posted */
          bf-fg-rctd.post-date = v-post-date
          bf-fg-rctd.tag2      = w-fg-rctd.tag2. 

END PROCEDURE.


PROCEDURE from-ord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
    /*DEF VAR ip-rowid AS ROWID NO-UNDO.*/

  DEF VAR lv-got-shipto AS LOG NO-UNDO.
  DEF VAR lv-stat AS cha NO-UNDO.
  DEF VAR lv-over LIKE oe-ordl.over-pct NO-UNDO.
  DEF VAR lv-rel-date AS DATE NO-UNDO.
  DEF VAR lv-job-no2 LIKE job-hdr.job-no2 NO-UNDO.
  DEF VAR lv-job-no LIKE job.job-no NO-UNDO.
  DEF BUFFER b-job-hdr FOR job-hdr. /* rtc */
  DEF BUFFER b-job FOR job.         /* rtc */

  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER b-loadtag FOR loadtag. 
MESSAGE "from-order " string(ip-rowid) .
    FIND FIRST oe-ord
        WHERE ROWID(oe-ord) EQ ip-rowid
          AND (v-stat EQ "A"                                    OR
               (v-stat EQ "C" AND INDEX("CZ",oe-ord.stat) GT 0) OR
               (v-stat EQ "O" AND INDEX("CZ",oe-ord.stat) EQ 0))
        NO-LOCK NO-ERROR.

    IF AVAIL oe-ord THEN
    FIND FIRST cust
        WHERE cust.company eq cocode
          AND cust.cust-no eq oe-ord.cust-no
        NO-LOCK NO-ERROR.

    IF AVAIL oe-ord THEN
    FIND FIRST soldto NO-LOCK
        WHERE soldto.company EQ cocode
          AND soldto.cust-no EQ oe-ord.cust-no
          AND soldto.sold-id EQ oe-ord.sold-id
        USE-INDEX sold-id NO-ERROR. 

    IF AVAIL cust THEN
    FOR EACH oe-ordl
        WHERE oe-ordl.company eq oe-ord.company
        AND oe-ordl.i-no    ge v-fitem[1]
        AND oe-ordl.i-no    le v-fitem[2]
        AND oe-ordl.ord-no  eq oe-ord.ord-no
        AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})
        AND (NOT CAN-FIND(FIRST ttblJob)
         OR (oe-ordl.job-no NE "" AND CAN-FIND(FIRST ttblJob WHERE ttblJob.company EQ oe-ordl.company
                                     AND ttblJob.job-no EQ oe-ordl.job-no
                                     AND ttblJob.job-no2 EQ oe-ordl.job-no2))
         OR (oe-ordl.job-no EQ "" AND
             CAN-FIND(FIRST ttblJob WHERE ttblJob.company EQ oe-ordl.company
                                     AND ttblJob.ord-no EQ oe-ordl.ord-no)))
        use-index ord-no NO-LOCK BREAK BY oe-ordl.i-no:
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq oe-ordl.i-no
          no-lock no-error.

      ASSIGN lv-job-no2 = 0 lv-job-no = "".
      FIND FIRST ttbljob WHERE ttbljob.company = cocode
                           AND ttbljob.ord-no = oe-ordl.ord-no
                           AND ttbljob.job-no = oe-ordl.job-no
                           AND ttbljob.job-no2 = oe-ordl.job-no2
                         NO-LOCK NO-ERROR.
      IF NOT AVAIL ttbljob THEN
        FIND FIRST ttbljob WHERE ttbljob.company = cocode
                           AND ttbljob.ord-no = oe-ordl.ord-no
                         NO-LOCK NO-ERROR.
      IF AVAIL ttbljob THEN
          FIND FIRST b-job-hdr WHERE b-job-hdr.company = cocode
                                   AND b-job-hdr.ord-no  = oe-ordl.ord-no
                                   AND b-job-hdr.job-no = ttbljob.job-no
                                   AND b-job-hdr.job-no2 = ttbljob.job-no2
                                   AND b-job-hdr.i-no    = oe-ordl.i-no NO-LOCK NO-ERROR.
      ELSE
        FIND FIRST b-job-hdr WHERE b-job-hdr.company = cocode 
                               AND b-job-hdr.ord-no  = oe-ordl.ord-no  
                               AND b-job-hdr.i-no    = oe-ordl.i-no NO-LOCK NO-ERROR.
      IF AVAIL b-job-hdr THEN
         FIND FIRST b-job WHERE b-job.company = b-job-hdr.company
                            AND b-job.job     = b-job-hdr.job
                            AND b-job.job-no  = b-job-hdr.job-no
                            AND b-job.job-no2 = b-job-hdr.job-no2 NO-LOCK NO-ERROR.

      IF lv-job-no = "" THEN DO:
        IF AVAIL b-job-hdr then
           FIND FIRST ttbljob WHERE ttbljob.company = b-job-hdr.company
                                AND ttbljob.job-no = b-job-hdr.job-no
                                AND ttbljob.job-no2 = b-job-hdr.job-no2
                                AND ttbljob.ord-no = oe-ordl.ord-no
                                NO-LOCK NO-ERROR.
        ELSE FIND FIRST ttblJob WHERE ttblJob.company EQ oe-ordl.company
                             AND ttblJob.ord-no EQ oe-ordl.ord-no
                        NO-LOCK NO-ERROR.
          
        IF AVAIL ttblJob THEN
            ASSIGN lv-job-no  = ttblJob.job-no
                   lv-job-no2 = ttblJob.job-no2.
        ELSE
            IF AVAIL b-job-hdr THEN
                ASSIGN lv-job-no = b-job-hdr.job-no
                       lv-job-no2 = b-job-hdr.job-no2.
      END.
      IF lv-job-no = "" THEN lv-job-no = oe-ord.job-no.
      IF lv-job-no2 = 0 THEN lv-job-no2 = oe-ord.job-no2.
      IF oe-ordl.est-no NE "" THEN
      FIND FIRST eb
          WHERE eb.company  EQ oe-ordl.company
            AND eb.est-no   EQ oe-ordl.est-no
            AND eb.stock-no EQ oe-ordl.i-no
          NO-LOCK NO-ERROR.

      lv-over = IF tb_over THEN oe-ordl.over-pct ELSE 0.

      IF NOT by-release OR NOT AVAIL oe-ordl THEN
      DO:
        IF FIRST-OF(oe-ordl.i-no) THEN
        DO: 
          CREATE w-ord.
          ASSIGN
            w-ord.ord-no       = oe-ord.ord-no
            w-ord.job-no       = lv-job-no
            w-ord.job-no2      = lv-job-no2
            w-ord.cust-no      = oe-ord.cust-no
            w-ord.cust-name    = oe-ord.cust-name
            w-ord.i-no         = oe-ordl.i-no
            w-ord.cust-part-no = oe-ordl.part-no
            w-ord.over-pct     = lv-over
            w-ord.qty-before   = oe-ordl.qty
            w-ord.ord-qty      = w-ord.qty-before *
                                 (1 + (w-ord.over-pct / 100))
            w-ord.po-no        = oe-ordl.po-no-po
            w-ord.sold-code    = oe-ord.sold-id
            w-ord.sold-name    = oe-ord.sold-name
            w-ord.sold-add1    = oe-ord.sold-add[1]
            w-ord.sold-add2    = oe-ord.sold-add[2]
            w-ord.sold-city    = oe-ord.sold-city
            w-ord.sold-state   = oe-ord.sold-state
            w-ord.sold-zip     = oe-ord.sold-zip
            w-ord.i-name       = oe-ordl.i-name
            w-ord.due-date     = if oe-ord.due-date ne ? then
                                   oe-ord.due-date
                                 else
                                 if oe-ordl.req-date ne ? then
                                   oe-ordl.req-date
                                 else today
            w-ord.est-no       = oe-ordl.est-no
            w-ord.form-no      = oe-ordl.form-no
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = if cust.int-field[1] ne 0 then
                                   cust.int-field[1] else v-mult
            w-ord.dont-run-set = oe-ordl.is-a-component
            w-ord.ord-desc1    = oe-ordl.part-dscr1
            w-ord.ord-desc2    = oe-ordl.part-dscr2
            
            /* gdm - 08130804*/
            w-ord.linenum      = oe-ordl.e-num

            num-rec            = num-rec + 1.

             ASSIGN
              w-ord.rec_key = STRING(TODAY,"99999999") + STRING(prmScnCs_lbl,"99999999") + string(create-count)   
              create-count = create-count + 1.

          IF AVAIL b-job-hdr THEN
             w-ord.due-date-jobhdr = IF b-job-hdr.due-date <> ? THEN STRING(b-job-hdr.due-date, "99/99/9999") ELSE "".
          IF AVAIL b-job THEN
             w-ord.due-date-job = IF b-job.due-date <> ? THEN STRING(b-job.due-date, "99/99/9999") ELSE "".
          IF w-ord.job-no EQ "" AND fi_cas-lab NE "" THEN
          DO:
             FIND FIRST b-loadtag WHERE
                  b-loadtag.company EQ oe-ordl.company AND
                  b-loadtag.item-type EQ NO AND
                  b-loadtag.is-case-tag EQ YES AND
                  b-loadtag.tag-no EQ fi_cas-lab
                  NO-LOCK NO-ERROR.

             IF AVAIL b-loadtag THEN
             DO:

                ASSIGN
                   w-ord.job-no = b-loadtag.job-no
                   w-ord.job-no2 = b-loadtag.job-no2.

                RELEASE b-loadtag.
             END.
          END.
          RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                            OUTPUT w-ord.rel-date,
                            OUTPUT w-ord.rel-lot#).
          IF tb_xfer-lot THEN w-ord.lot# = w-ord.rel-lot#.

          IF AVAIL itemfg THEN
            ASSIGN
             w-ord.upc-no  = itemfg.upc-no
             w-ord.box-len = itemfg.l-score[50]
             w-ord.box-wid = itemfg.w-score[50]
             w-ord.box-dep = itemfg.d-score[50]
             w-ord.flute   = itemfg.flute
             w-ord.test    = itemfg.test
             w-ord.pcs     = itemfg.case-count
             w-ord.bundle  = itemfg.case-pall
             w-ord.style   = itemfg.style.

          IF w-ord.style NE "" THEN
          DO:
             FIND FIRST style WHERE
                  style.company EQ cocode AND
                  style.style EQ w-ord.style
                  NO-LOCK NO-ERROR.
          
             IF AVAIL style THEN
             DO:
                w-ord.style-desc = style.dscr.
                RELEASE style.
             END.
          END.

          IF NOT AVAIL eb AND AVAIL itemfg AND itemfg.est-no NE "" THEN
          FIND FIRST eb
              WHERE eb.company  EQ itemfg.company
                AND eb.est-no   EQ itemfg.est-no
                AND eb.stock-no EQ itemfg.i-no
              NO-LOCK NO-ERROR.

          IF AVAIL eb THEN
            ASSIGN
             w-ord.flute  = eb.flute
             w-ord.test   = eb.test
             w-ord.pcs    = eb.cas-cnt
             w-ord.bundle = eb.cas-pal
             w-ord.cas-no = eb.cas-no
             w-ord.form-no = eb.form-no
             w-ord.pallt-no = eb.tr-no.

          /* get it from order    task# 04120602 */
          ASSIGN w-ord.pcs    = oe-ordl.cas-cnt
                 w-ord.bundle = oe-ordl.cases-unit.

          /* get shipto from open oe-rel  */
          
          lv-got-shipto = NO.
          FOR EACH w-shipto:
            DELETE w-shipto.
          END.

          FOR EACH oe-rel NO-LOCK
              WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.i-no    EQ oe-ordl.i-no
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.line    EQ oe-ordl.line:
            IF NOT tb_ship-id THEN v-ship-id = oe-rel.ship-id.
            RUN oe/custxship.p (oe-rel.company,
                                oe-rel.cust-no,
                                v-ship-id,
                                BUFFER shipto).

            IF AVAIL shipto THEN DO:
              RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

              CREATE w-shipto.
              BUFFER-COPY shipto EXCEPT rec_key TO w-shipto
              ASSIGN
               w-shipto.stat   = lv-stat
               w-shipto.row-id = ROWID(oe-rel).
            END.
          END.

          FOR EACH w-shipto,
              FIRST oe-rel WHERE ROWID(oe-rel) EQ w-shipto.row-id NO-LOCK
              BREAK BY oe-rel.rel-date
                    BY oe-rel.po-no
                    BY oe-rel.ship-no 
                    BY oe-rel.qty:

            IF CAN-DO("L,S,I",w-shipto.stat) OR
               LAST(oe-rel.rel-date)          THEN DO:
              ASSIGN
               lv-got-shipto    = YES
               w-ord.ship-code  = w-shipto.ship-id
               w-ord.ship-name  = w-shipto.ship-name
               w-ord.ship-add1  = w-shipto.ship-add[1]
               w-ord.ship-add2  = w-shipto.ship-add[2]
               w-ord.ship-city  = w-shipto.ship-city
               w-ord.ship-state = w-shipto.ship-state
               w-ord.ship-ctry  = w-shipto.country
               w-ord.ship-zip   = w-shipto.ship-zip.
              LEAVE.
            END.
          END.
          FOR EACH w-shipto:
            DELETE w-shipto.
          END.

          IF NOT lv-got-shipto THEN
          FOR EACH shipto
              WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ oe-ord.cust-no
              USE-INDEX ship-id NO-LOCK
              BREAK BY shipto.ship-no DESC:
            IF NOT tb_ship-id THEN v-ship-id = oe-ord.cust-no.
            IF shipto.ship-id EQ v-ship-id OR
               LAST(shipto.ship-no)             THEN DO:
              ASSIGN
               w-ord.ship-code  = shipto.ship-id
               w-ord.ship-name  = shipto.ship-name
               w-ord.ship-add1  = shipto.ship-add[1]
               w-ord.ship-add2  = shipto.ship-add[2]
               w-ord.ship-city  = shipto.ship-city
               w-ord.ship-state = shipto.ship-state
               w-ord.ship-ctry  = shipto.country
               w-ord.ship-zip   = shipto.ship-zip.
              LEAVE.
            END.
          END.

          FIND FIRST soldto NO-LOCK
              WHERE soldto.company EQ cocode
                AND soldto.cust-no EQ oe-ord.cust-no
                AND soldto.sold-id EQ oe-ord.sold-id
              USE-INDEX sold-id NO-ERROR.

          IF AVAIL soldto THEN w-ord.sold-ctry = soldto.country.

          ASSIGN
            w-ord.total-unit = w-ord.pcs * w-ord.bundle
            /* Add .49 to round up and add 1 for extra tag   */
            w-ord.total-tags = ((oe-ordl.qty / w-ord.total-unit) + .49) +  (IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1).

        END.  /* first-of */
      END.  /* not by-release */

      ELSE
      FOR EACH oe-rel
          WHERE oe-rel.company eq cocode
          AND oe-rel.i-no      eq oe-ordl.i-no
          AND oe-rel.ord-no    eq oe-ordl.ord-no
          AND oe-rel.line      eq oe-ordl.line
          AND oe-rel.link-no   ne 0 NO-LOCK:

        CREATE w-ord.
        ASSIGN
          w-ord.ord-no       = oe-ord.ord-no
          w-ord.job-no       = oe-ordl.job-no
          w-ord.job-no2      = oe-ordl.job-no2
          w-ord.cust-no      = oe-ord.cust-no
          w-ord.cust-name    = oe-ord.cust-name
          w-ord.i-no         = oe-ordl.i-no
          w-ord.cust-part-no = oe-ordl.part-no
          w-ord.cust-po-no   = IF v-po-no-source eq "L" THEN oe-ordl.po-no
                                                        ELSE
                               IF v-po-no-source eq "R" THEN oe-rel.po-no
                                                        ELSE oe-ord.po-no 
          w-ord.over-pct     = lv-over
          w-ord.qty-before   = oe-rel.qty
          w-ord.ord-qty      = w-ord.qty-before *
                               (1 + (w-ord.over-pct / 100))
          w-ord.po-no        = oe-ordl.po-no-po
          w-ord.ship-code    = oe-rel.ship-id
          w-ord.ship-add1    = oe-rel.ship-add[1]
          w-ord.ship-add2    = oe-rel.ship-add[2]
          w-ord.ship-city    = oe-rel.ship-city
          w-ord.ship-state   = oe-rel.ship-state
          w-ord.ship-zip     = oe-rel.ship-zip
          w-ord.sold-code    = oe-ord.sold-id
          w-ord.sold-name    = oe-ord.sold-name
          w-ord.sold-add1    = oe-ord.sold-add[1]
          w-ord.sold-add2    = oe-ord.sold-add[2]
          w-ord.sold-city    = oe-ord.sold-city
          w-ord.sold-state   = oe-ord.sold-state
          w-ord.sold-zip     = oe-ord.sold-zip
          w-ord.i-name       = oe-ordl.i-name
          w-ord.due-date     = 
            (if oe-ord.due-date <> ? 
             then oe-ord.due-date
             else if oe-ordl.req-date <> ?  /* 9901 CAH */
             then oe-ordl.req-date
             else today)
          w-ord.rel-date     = oe-rel.rel-date
          w-ord.est-no       = oe-ordl.est-no
          w-ord.form-no      = oe-ordl.form-no
          w-ord.vendor       = company.name
          w-ord.tare-wt      = 10
          w-ord.uom          = "EA"
          w-ord.mult         = if cust.int-field[1] ne 0 then
                                 cust.int-field[1] else v-mult
          w-ord.dont-run-set = oe-ordl.is-a-component
          w-ord.ord-desc1    = oe-ordl.part-dscr1
          w-ord.ord-desc2    = oe-ordl.part-dscr2

          /* gdm - 08130804*/
          w-ord.linenum      = oe-ordl.e-num.

          num-rec            = num-rec + 1
           .

           ASSIGN
              w-ord.rec_key = STRING(TODAY,"99999999") + STRING(prmScnCs_lbl,"99999999") + string(create-count)   
              create-count = create-count + 1.
         
        FIND FIRST ref-lot-no WHERE
             ref-lot-no.reftable EQ "oe-rel.lot-no" AND
             ref-lot-no.company  EQ STRING(oe-rel.r-no,"9999999999")
             NO-LOCK NO-ERROR.

        IF AVAIL ref-lot-no THEN
        DO:
           w-ord.rel-lot# = ref-lot-no.CODE.
           RELEASE ref-lot-no.
        END.
        IF tb_xfer-lot THEN w-ord.lot# = w-ord.rel-lot#.

        IF AVAIL itemfg THEN
          ASSIGN
           w-ord.upc-no  = itemfg.upc-no
           w-ord.box-len = itemfg.l-score[50]
           w-ord.box-wid = itemfg.w-score[50]
           w-ord.box-dep = itemfg.d-score[50]
           w-ord.flute   = itemfg.flute
           w-ord.test    = itemfg.test
           w-ord.pcs     = itemfg.case-count
           w-ord.bundle  = itemfg.case-pall
           w-ord.style   = itemfg.style.

        IF w-ord.style NE "" THEN
        DO:
           FIND FIRST style WHERE
                style.company EQ cocode AND
                style.style EQ w-ord.style
                NO-LOCK NO-ERROR.
        
           IF AVAIL style THEN
           DO:
              w-ord.style-desc = style.dscr.
              RELEASE style.
           END.
        END.

        IF NOT AVAIL eb AND AVAIL itemfg AND itemfg.est-no NE "" THEN
        FIND FIRST eb
            WHERE eb.company  EQ itemfg.company
              AND eb.est-no   EQ itemfg.est-no
              AND eb.stock-no EQ itemfg.i-no
            NO-LOCK NO-ERROR.

        IF AVAIL eb THEN
          ASSIGN
           w-ord.flute  = eb.flute
           w-ord.test   = eb.test
           w-ord.cas-no = eb.cas-no
           w-ord.pallt-no = eb.tr-no. 
    
        RUN oe/custxship.p (oe-rel.company,
                            oe-rel.cust-no,
                            oe-rel.ship-id,
                            BUFFER shipto).

        IF AVAIL shipto THEN
          ASSIGN
           w-ord.ship-code  = shipto.ship-id
           w-ord.ship-name  = shipto.ship-name
           w-ord.ship-add1  = shipto.ship-add[1]
           w-ord.ship-add2  = shipto.ship-add[2]
           w-ord.ship-city  = shipto.ship-city
           w-ord.ship-state = shipto.ship-state
           w-ord.ship-ctry  = shipto.country
           w-ord.ship-zip   = shipto.ship-zip.

        IF AVAIL soldto THEN w-ord.sold-ctry = soldto.country.
          
        ASSIGN
          w-ord.pcs        = oe-rel.qty-case
          w-ord.bundle     = oe-rel.cases
          w-ord.total-unit = w-ord.pcs * w-ord.bundle
          /* Add .49 to round up and add 1 for extra tag   */
          w-ord.total-tags = ((oe-rel.qty / w-ord.total-unit) + .49) +  IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1.
      END.
    END.


END PROCEDURE.


PROCEDURE from-po :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH po-ordl NO-LOCK WHERE po-ordl.company EQ po-ord.company
                             AND po-ordl.po-no EQ po-ord.po-no
                             AND po-ordl.item-type EQ NO
                             AND po-ordl.i-no GE v-fitem[1]
                             AND po-ordl.i-no LE v-fitem[2]
                           USE-INDEX po-no BREAK BY po-ordl.i-no:
    IF FIRST-OF(po-ordl.i-no) THEN DO:
      FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                                AND cust.cust-no EQ po-ord.cust-no NO-ERROR.
      FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                                AND vend.vend-no EQ po-ord.vend-no NO-ERROR.
      FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode
                                  AND itemfg.i-no EQ po-ordl.i-no NO-ERROR.
      CREATE w-ord.
      ASSIGN
        w-ord.cust-name = IF AVAILABLE cust THEN cust.name ELSE ''
        w-ord.cust-no = po-ord.cust-no
        w-ord.ship-code = po-ord.ship-id
        w-ord.due-date = po-ord.due-date
        w-ord.i-no = po-ordl.i-no
        w-ord.i-name = po-ordl.i-name
        w-ord.mult = IF AVAILABLE cust AND cust.int-field[1] NE 0 THEN
                     cust.int-field[1] ELSE v-mult
        w-ord.ord-qty = po-ordl.ord-qty
        w-ord.po-no = po-ord.po-no
        w-ord.tare-wt = 10
        w-ord.uom = 'EA'
        w-ord.vendor = IF AVAILABLE vend THEN vend.name ELSE ''
        num-rec = num-rec + 1.
         ASSIGN
              w-ord.rec_key = STRING(TODAY,"99999999") + STRING(prmScnCs_lbl,"99999999") + string(create-count)   
              create-count = create-count + 1.
      
      IF AVAILABLE itemfg THEN
      ASSIGN
        w-ord.est-no = itemfg.est-no
        w-ord.upc-no = itemfg.upc-no
        w-ord.box-len = itemfg.l-score[50]
        w-ord.box-wid = itemfg.w-score[50]
        w-ord.box-dep = itemfg.d-score[50]
        w-ord.flute = itemfg.flute
        w-ord.test = itemfg.test
        w-ord.pcs = itemfg.case-count
        w-ord.bundle = IF itemfg.case-pall NE 0 THEN itemfg.case-pall ELSE 1
        w-ord.style   = itemfg.style.
      
      IF w-ord.style NE "" THEN
      DO:
         FIND FIRST style WHERE
              style.company EQ cocode AND
              style.style EQ w-ord.style
              NO-LOCK NO-ERROR.

         IF AVAIL style THEN
         DO:
            w-ord.style-desc = style.dscr.
            RELEASE style.
         END.
      END.

      IF AVAILABLE itemfg AND itemfg.est-no NE '' THEN
      FIND FIRST eb NO-LOCK WHERE eb.company EQ itemfg.company
                              AND eb.est-no EQ itemfg.est-no
                              AND eb.stock-no EQ itemfg.i-no NO-ERROR.

      IF AVAILABLE eb THEN
      ASSIGN
        w-ord.flute = eb.flute
        w-ord.test = eb.test
        w-ord.pcs = eb.cas-cnt
        w-ord.bundle = eb.cas-pal
        w-ord.cas-no = eb.cas-no
        w-ord.pallt-no = eb.tr-no.
      IF NOT tb_ship-id THEN v-ship-id = po-ord.ship-id.
      FIND FIRST shipto NO-LOCK
          WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ po-ord.cust-no
            AND shipto.ship-id EQ v-ship-id
          USE-INDEX ship-id NO-ERROR.
      IF AVAIL shipto THEN
        ASSIGN
          w-ord.ship-name  = shipto.ship-name
          w-ord.ship-add1  = shipto.ship-add[1]
          w-ord.ship-add2  = shipto.ship-add[2]
          w-ord.ship-city  = shipto.ship-city
          w-ord.ship-state = shipto.ship-state
          w-ord.ship-ctry  = shipto.country
          w-ord.ship-zip   = shipto.ship-zip.
      
      ASSIGN
        w-ord.total-unit = w-ord.pcs * w-ord.bundle
        /* Add .49 to round up and add 1 for extra tag   */
        w-ord.total-tags = ((w-ord.ord-qty / w-ord.total-unit) + .49) +
                           (IF CAN-DO("SSLABEL,CentBox",v-loadtag) THEN 0 ELSE 1).
    END. /* first-of */
  END. /* each po-ordl */
 

END PROCEDURE.

PROCEDURE CreateWOrdFromItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipBeginItem AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipEndItem AS CHARACTER NO-UNDO.    

   /*DEFINE VAR ipBeginItem AS CHARACTER NO-UNDO.
  DEFINE VAR ipEndItem AS CHARACTER NO-UNDO.*/

  FOR EACH itemfg NO-LOCK WHERE itemfg.company EQ cocode
                            AND itemfg.i-no GE ipBeginItem
                            AND itemfg.i-no LE ipEndItem:
    FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                              AND vend.vend-no EQ itemfg.vend-no NO-ERROR.
    FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                              AND cust.cust-no EQ oe-ord.cust-no NO-ERROR.
     
    CREATE w-ord.
    ASSIGN
      w-ord.i-no = itemfg.i-no
      w-ord.i-name = itemfg.i-name
      w-ord.cust-no = itemfg.cust-no
      w-ord.cust-name = itemfg.cust-name
      w-ord.cust-part-no = itemfg.part-no
      w-ord.mult = IF AVAILABLE cust AND cust.int-field[1] NE 0 THEN
                   cust.int-field[1] ELSE v-mult
      w-ord.box-len = itemfg.l-score[50]
      w-ord.box-wid = itemfg.w-score[50]
      w-ord.box-dep = itemfg.d-score[50]
      w-ord.flute = itemfg.flute
      w-ord.upc-no = itemfg.upc-no
      w-ord.test = itemfg.test
      w-ord.vendor = IF AVAILABLE vend THEN vend.name ELSE company.name
      w-ord.tare-wt = 10
      w-ord.uom = "EA"
      w-ord.pcs = itemfg.case-count
      w-ord.bundle = itemfg.case-pall
      w-ord.style   = itemfg.style.
        ASSIGN
              w-ord.rec_key = STRING(TODAY,"99999999") + STRING(prmScnCs_lbl,"99999999") + string(create-count)   
              create-count = create-count + 1.


   /* task 02081202 */
   IF tb_reprint-tag THEN DO:
      FIND FIRST fg-bin WHERE fg-bin.company = itemfg.company
                          AND fg-bin.i-no = w-ord.i-no
                          AND fg-bin.tag = fi_cas-lab
                          AND fg-bin.qty > 0 NO-LOCK NO-ERROR.
      IF AVAIL fg-bin AND AVAIL w-ord THEN
         ASSIGN w-ord.pcs = fg-bin.case-count
                w-ord.bundle = /*fg-bin.cases-unit*/ TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                w-ord.partial = fg-bin.partial-count
                w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial .      
   END.

    IF w-ord.style NE "" THEN
    DO:
       FIND FIRST style WHERE
            style.company EQ cocode AND
            style.style EQ w-ord.style
            NO-LOCK NO-ERROR.

       IF AVAIL style THEN
       DO:
          w-ord.style-desc = style.dscr.
          RELEASE style.
       END.
    END.
  END. /* each itemfg */

END PROCEDURE.



PROCEDURE final-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH w-ord:
    FIND FIRST cust
        WHERE cust.company eq cocode
          AND cust.cust-no eq w-ord.cust-no
        NO-LOCK NO-ERROR.

    IF v-cas-lab THEN DO:
      FIND FIRST loadtag
          WHERE loadtag.company     EQ cocode
            AND loadtag.tag-no      EQ fi_cas-lab
            AND loadtag.item-type   EQ NO
            AND loadtag.is-case-tag EQ YES
          NO-LOCK NO-ERROR.
      IF AVAIL loadtag AND loadtag.tag-no NE "" THEN
        ASSIGN
         w-ord.pcs        = loadtag.qty-case
         w-ord.bundle     = loadtag.case-bundle
         w-ord.total-unit = w-ord.pcs * w-ord.bundle
         w-ord.lot        = loadtag.misc-char[2].
    END.

    IF v-tags EQ 0 THEN
       w-ord.total-tags = 1.
    ELSE
    IF v-tags EQ ? THEN w-ord.total-tags = 0.
  END.

END PROCEDURE.
