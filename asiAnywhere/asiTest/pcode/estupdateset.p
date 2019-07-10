

                                 
/*------------------------------------------------------------------------
    File        : estupdateset.p
    Purpose     : Validation for add order

    Syntax      :

    Description : Validation for add order

    Author(s)   : Sewa
    Created     : july 4, 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttEstUpdateSet NO-UNDO
    FIELD vStock       AS CHAR 
    FIELD vPartno       AS CHAR
    FIELD vPartDscr     AS CHAR
    FIELD vPartDscr2    AS CHAR
    FIELD vProcat       AS CHAR
    FIELD vLen          AS DECIMAL
    FIELD vWid          AS DECIMAL
    FIELD vDep          AS DECIMAL
    FIELD vQty          AS INT
    FIELD vMsf          AS DECIMAL
    FIELD vAllo         AS CHAR
    FIELD vUnit         AS LOGICAL 
    .

DEFINE DATASET dsEstUpdateSet FOR ttEstUpdateSet.

DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmStock       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmPartno      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmPartDscr    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmPartDscr2   AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmProcat      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmlen         AS DECIMAL NO-UNDO. 
DEFINE INPUT PARAMETER prmWid         AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmDep         AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmAllo        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmUnit        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEst         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmType        AS INT NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEstUpdateSet .
                                    
DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 

IF prmUser        = ?  THEN ASSIGN    prmUser       = "".
IF prmAction      = ?  THEN ASSIGN    prmAction     = "Select".
IF prmStock       = ?  THEN ASSIGN    prmStock      = "".
IF prmPartno      = ?  THEN ASSIGN    prmPartno     = "".
IF prmPartDscr    = ?  THEN ASSIGN    prmPartDscr   = "".
IF prmPartDscr2   = ?  THEN ASSIGN    prmPartDscr2  = "".      
IF prmProcat      = ?  THEN ASSIGN    prmProcat     = "".
IF prmlen         = ?  THEN ASSIGN    prmlen        = 0.  
IF prmWid         = ?  THEN ASSIGN    prmWid        = 0.  
IF prmDep         = ?  THEN ASSIGN    prmDep        = 0.  
IF prmAllo        = ?  THEN ASSIGN    prmAllo       = "".  
IF prmUnit        = ?  THEN ASSIGN    prmUnit       = "Yes".
                                                 


DEF VAR prmLoc AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF BUFFER bf-est FOR est.
DEF BUFFER bf-set FOR eb.
    DEF BUFFER b-eb FOR eb.
DEF VAR lv-set-recid AS RECID NO-UNDO.
DEF VAR lv-new-set AS LOG NO-UNDO.
DEF VAR ld-yld AS DEC NO-UNDO.
DEF VAR ld-sqin AS DEC NO-UNDO.
DEF VAR ld-msf AS DEC NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR ll-alloc AS LOG NO-UNDO.
DEFINE VAR ll AS LOGICAL NO-UNDO.
DEF VAR ip-est-type AS INT NO-UNDO.
DEFINE VAR vEstimate AS CHAR NO-UNDO.
 DEF VAR li AS INT NO-UNDO.
 DEF VAR setall AS LOGICAL NO-UNDO.

 
    
 IF prmAllo = "Yes"  THEN ASSIGN setall = YES.
 IF prmAllo = "No"  THEN ASSIGN setall = NO .
 IF prmAllo = "?"  THEN ASSIGN  setall = ? .

ASSIGN
    ip-est-type = prmType.
{sys/inc/var.i "new shared" }
   
   FIND FIRST usercomp WHERE
        usercomp.user_id = prmUser AND
        usercomp.loc = '' AND
        usercomp.company_default = YES
        NO-LOCK NO-ERROR.

   prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN
cocode = prmComp
locode    = "Main" .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.company = prmComp AND
     usercomp.loc NE "" AND
     usercomp.loc_default = yes
     NO-LOCK NO-ERROR.

prmLoc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".
{cec/msfcalc.i}
    {sys/inc/setprint.i}
    ll-alloc = /*IF ip-est-type LE 4 THEN v-allocf ELSE*/ v-alloc.

   def var k_frac as dec init "6.25" no-undo.
   {sys/inc/f16to32.i}

 vEstimate =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst).

IF prmType = 2 OR prmType = 6  THEN DO:


FIND FIRST eb WHERE eb.est-no = vEstimate   AND eb.company = prmComp AND eb.form-no NE 0 NO-LOCK NO-ERROR  .
FIND bf-eb WHERE RECID(bf-eb) EQ RECID(eb) NO-LOCK NO-ERROR.
   
   FIND FIRST style WHERE
        style.company EQ bf-eb.company AND
        style.style   EQ bf-eb.style
        NO-LOCK NO-ERROR.

/*   IF AVAIL style AND style.TYPE = "P" THEN
      eb.dep:LABEL = "Height".*/

   FIND FIRST bf-set WHERE bf-set.company = bf-eb.company
                       AND bf-set.est-no = bf-eb.est-no
                       AND bf-set.form-no = 0
                       NO-LOCK NO-ERROR.
   MESSAGE "bf-set" bf-set.est-no .

   IF AVAIL bf-set THEN lv-set-recid = RECID(bf-set).
   ELSE DO:
     ll = bf-eb.est-type EQ ip-est-type OR
          CAN-FIND(FIRST eb
                   WHERE eb.company EQ bf-eb.company
                     AND eb.est-no  EQ bf-eb.est-no
                     AND eb.eqty    EQ bf-eb.eqty
                     AND ROWID(eb)  NE ROWID(bf-eb)).
     IF NOT ll AND ((bf-eb.est-type GE 5 AND bf-eb.yld-qty GT 1) OR
                    (bf-eb.est-type LE 4 AND bf-eb.cust-% GT 1)) THEN DO:
       
       /*RUN check-use NO-ERROR.*/
       RUN create-set.
       FIND bf-set WHERE RECID(bf-set) EQ lv-set-recid NO-ERROR.
     END.
   END.

   /* update set info from eb */
   i = 0.
   FOR EACH bf-eb WHERE bf-eb.company EQ bf-set.company
                    AND bf-eb.est-no  EQ bf-set.est-no
                    AND bf-eb.form-no NE 0
                  NO-LOCK BREAK BY bf-eb.est-no:
       i = i + 1.
       IF LAST(bf-eb.est-no) THEN LEAVE.
   END.
   IF i LE 1 AND ((bf-eb.est-type GE 5 AND bf-eb.yld-qty EQ 2) OR
                  (bf-eb.est-type LE 4 AND bf-eb.cust-% EQ 2)) THEN DO:
     /*RUN check-use NO-ERROR.*/
    /* RUN upd-2box. */    
   END.

   FIND eb WHERE RECID(eb) = lv-set-recid NO-LOCK.
  
   FIND FIRST est
       WHERE est.company EQ eb.company
         AND est.est-no  EQ eb.est-no
       NO-LOCK.

   FIND FIRST est-qty
       WHERE est-qty.company EQ eb.company
         AND est-qty.est-no  EQ eb.est-no
       NO-LOCK NO-ERROR.

   ASSIGN
    ld-msf     = 0  .
    /*rd_alloc   = bf-set.set-is-assembled
    tb_unitize = bf-set.pur-man.*/

   /*IF rd_alloc NE ? THEN rd_alloc = NOT rd_alloc.*/

   FOR EACH b-eb
       WHERE b-eb.company EQ eb.company
         AND b-eb.est-no  EQ eb.est-no
         AND b-eb.form-no NE 0
         AND ROWID(b-eb)  NE ROWID(eb)
       NO-LOCK
       BREAK BY b-eb.est-no:
     ASSIGN
      ld-yld   = IF b-eb.est-type GE 5 THEN
                   (IF b-eb.yld-qty LT 0 THEN -1 / b-eb.yld-qty ELSE b-eb.yld-qty)
                 ELSE
                   (IF b-eb.cust-%  LT 0 THEN -1 / b-eb.cust-%  ELSE b-eb.cust-%)
      ld-sqin  = est.est-qty[1] * ld-yld * b-eb.t-sqin
      ld-msf   = ld-msf + ((IF v-corr THEN (ld-sqin * .007) ELSE (ld-sqin / 144)) / 1000).

    
   END.

   /*ASSIGN
      fi_msf = ld-msf
      btn_qty-msf:LABEL = TRIM(est-qty.eqty:LABEL) + ": " +
                          TRIM(STRING(est-qty.eqty,est-qty.eqty:FORMAT)) +
                          FILL(" ",10) +
                          TRIM(fi_msf:LABEL) + ": " +
                          TRIM(STRING(fi_msf,fi_msf:FORMAT)).*/

   FIND FIRST itemfg
       WHERE itemfg.company EQ eb.company
         AND itemfg.i-no    EQ eb.stock-no
         AND itemfg.i-no    NE ""
       NO-LOCK NO-ERROR.
  /* IF AVAIL itemfg THEN rd_alloc = itemfg.alloc.*/

  /* RUN enable_UI.
   IF lv-new-set THEN RUN enable-all.*/ 



IF prmAction = "Update"  THEN DO:

    li = 0.
    FOR EACH b-eb WHERE b-eb.company EQ prmComp
        AND b-eb.est-no  EQ vEstimate
        AND b-eb.form-no NE 0 NO-LOCK:
        li = li + 1.
     END.

        IF li GT 1 THEN
            IF prmPartno EQ "" OR
            CAN-FIND(FIRST b-eb WHERE b-eb.company   EQ est.company
                     AND b-eb.est-no    EQ est.est-no
                     AND (b-eb.part-no  EQ prmPartno OR
                          b-eb.stock-no EQ prmPartno)
                     AND b-eb.form-no   NE 0) THEN DO:
                IF prmPartno EQ "" THEN DO:
                    cError =  "Part-no  must be entered..." .
                    RETURN.
                END.
                ELSE DO:
                    cError =   "Cust Part  already exists on estimate..." .
                    RETURN .
                END.
        END.

    IF prmProcat = "" THEN DO:
       cError =  "Category must be entered. Try help.".
       RETURN .
    END.
    IF NOT CAN-FIND(FIRST fgcat WHERE fgcat.company = prmComp and
                                      fgcat.procat = prmProcat)
    THEN DO:
        cError = "Invalid Category. Try help." .
        RETURN .
    END.
        
 END.  /*of update action*****/

  
IF prmAction = "Update" THEN DO:
MESSAGE "test" prmAllo setall .
    FIND FIRST  est-qty WHERE est-qty.est-no = vEstimate  AND est-qty.company = "001"   EXCLUSIVE-LOCK NO-ERROR.
        FIND FIRST eb WHERE eb.est-no = est-qty.est-no AND eb.company = est-qty.company AND eb.blank-no = 0 AND eb.form-no = 0 EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL eb THEN DO:
                ASSIGN
                    eb.stock-no          =   prmStock                     
                    eb.part-no           =   prmPartno                   
                    eb.part-dscr1        =   prmPartDscr                
                    eb.part-dscr2        =   prmPartDscr2              
                    eb.procat            =   prmProcat                
                    eb.len               =   prmlen                       
                    eb.wid               =   prmWid                       
                    eb.dep               =   prmDep      
                    eb.set-is-assembled  =  setall
                    eb.pur-man           =  IF  prmUnit = "Yes" THEN TRUE ELSE FALSE  .
                END.
           ASSIGN 
               prmAction = "Select".
 END.  /*end of action*/


IF prmAction = "Select" THEN DO:
    FIND FIRST  est-qty WHERE est-qty.est-no = vEstimate  AND est-qty.company = prmComp   NO-LOCK NO-ERROR.
        FIND FIRST  eb WHERE eb.est-no = est-qty.est-no AND eb.company = est-qty.company AND eb.blank-no = 0 AND eb.form-no = 0 NO-LOCK NO-ERROR.
           MESSAGE "eb" eb.est-no .
        IF AVAIL eb THEN DO:
                
                CREATE ttEstUpdateSet.
                ASSIGN
                    ttEstUpdateSet.vStock          = eb.stock-no 
                    ttEstUpdateSet.vPartno         = eb.part-no
                    ttEstUpdateSet.vPartDscr       = eb.part-dscr1
                    ttEstUpdateSet.vPartDscr2      = eb.part-dscr2
                    ttEstUpdateSet.vProcat         = eb.procat
                    ttEstUpdateSet.vLen            = eb.len
                    ttEstUpdateSet.vWid            = eb.wid
                    ttEstUpdateSet.vDep            = eb.dep
                    ttEstUpdateSet.vQty            = est-qty.eqty
                    ttEstUpdateSet.vMsf            = ld-msf 
                    ttEstUpdateSet.vUnit           = eb.pur-man  
                        .
                    IF eb.set-is-assembled = YES THEN ASSIGN  ttEstUpdateSet.vAllo = "Yes".
                    IF eb.set-is-assembled = NO THEN ASSIGN  ttEstUpdateSet.vAllo = "No".
                    IF eb.set-is-assembled = ? THEN ASSIGN  ttEstUpdateSet.vAllo = "?".
            END. 
       
END.  /*end of action*/

END.

/***************************************procuder*****************************************/
PROCEDURE create-set :

  FIND FIRST bf-est
      WHERE bf-est.company EQ bf-eb.company
        AND bf-est.est-no  EQ bf-eb.est-no
      NO-LOCK NO-ERROR.

  {ce/set-info.a ip-est-type "bf-" "bf-"}

  bf-eb.set-is-assembled = ll-alloc.
  IF bf-eb.set-is-assembled NE ? THEN
    bf-eb.set-is-assembled = NOT bf-eb.set-is-assembled.

  ASSIGN
   bf-eb.pur-man          = bf-eb.set-is-assembled NE NO
   lv-new-set             = YES
   lv-set-recid           = RECID(bf-eb).

END PROCEDURE.




