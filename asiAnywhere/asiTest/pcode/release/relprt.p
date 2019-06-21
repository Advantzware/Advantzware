

/*------------------------------------------------------------------------
    File        : relprt.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
/*{sys/inc/var.i new shared}*/
    
    DEFINE TEMP-TABLE ttPrintReleaseOrd NO-UNDO
        FIELD relprt AS CHAR
        FIELD extra  AS CHAR.
        
       

DEFINE DATASET dsPrintReleaseOrd FOR ttPrintReleaseOrd.
    DEFINE INPUT PARAMETER  prmUser         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmrelprt       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegcust      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegrel       AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegord       AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdate      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdelz      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegloc       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbeglocbin    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendcust      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendrel       AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmendord       AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddate      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddelz      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendloc       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendlocbin    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmpsted        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmprinted      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmrel_prfm     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbinloc       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmsrtdelz      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmprtdelz      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmasscomp      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmcustprt      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmprtpric      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmsrtbinloc    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmprtwht       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmpstrel       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmmulrel       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmextagbin     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmextra        AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError          AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPrintReleaseOrd.

     IF prmUser         = ? THEN ASSIGN     prmUser          = "".   
     IF prmrelprt       = ? THEN ASSIGN    prmrelprt         = "". 
     IF prmbegcust       = ? THEN ASSIGN    prmbegcust       = "". 
     IF prmbegrel        = ? THEN ASSIGN    prmbegrel        = 0. 
     IF prmbegord        = ? THEN ASSIGN    prmbegord        = 0. 
     IF prmbegdate       = ? THEN ASSIGN    prmbegdate       = "".   
     IF prmbegdelz       = ? THEN ASSIGN    prmbegdelz       = "". 
     IF prmbegloc        = ? THEN ASSIGN    prmbegloc        = "". 
     IF prmbeglocbin     = ? THEN ASSIGN    prmbeglocbin     = "". 
     IF prmendcust       = ? THEN ASSIGN    prmendcust       = "". 
     IF prmendrel        = ? THEN ASSIGN    prmendrel        = 0.   
     IF prmendord        = ? THEN ASSIGN    prmendord        = 0. 
     IF prmenddate       = ? THEN ASSIGN    prmenddate       = "". 
     IF prmenddelz       = ? THEN ASSIGN    prmenddelz       = "". 
     IF prmendloc        = ? THEN ASSIGN    prmendloc        = "". 
     IF prmendlocbin     = ? THEN ASSIGN    prmendlocbin     = "". 
     IF prmrel_prfm      = ? THEN ASSIGN    prmrel_prfm      = "". 
     IF prmpstrel        = ? THEN ASSIGN    prmpstrel        = "". 
     IF prmmulrel        = ? THEN ASSIGN    prmmulrel        = "".  
     IF prmbinloc        = ? THEN ASSIGN    prmbinloc        = "". 
     IF prmsrtdelz       = ? THEN ASSIGN    prmsrtdelz       = "".   
     IF prmprtdelz       = ? THEN ASSIGN    prmprtdelz       = "". 
     IF prmasscomp       = ? THEN ASSIGN    prmasscomp       = "". 
     IF prmcustprt       = ? THEN ASSIGN    prmcustprt       = "". 
     IF prmprtpric       = ? THEN ASSIGN    prmprtpric       = "". 
     IF prmsrtbinloc     = ? THEN ASSIGN    prmsrtbinloc     = "".   
     IF prmprtwht        = ? THEN ASSIGN    prmprtwht        = "". 
     IF prmextra         = ? THEN ASSIGN    prmextra         = "". 
     



DEFINE VARIABLE begin_cust-no           AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_relnum            AS INTEGER FORMAT ">>>>>>>>" INITIAL 0  NO-UNDO.
DEFINE VARIABLE begin_ord-no            AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE begin_date              AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE begin_del-zone          AS CHARACTER FORMAT "X(5)" NO-UNDO.
DEFINE VARIABLE begin_loc               AS CHARACTER FORMAT "X(5)" NO-UNDO.
DEFINE VARIABLE begin_loc-bin           AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE end_cust-no             AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_relnum              AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 NO-UNDO.
DEFINE VARIABLE end_ord-no              AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999  NO-UNDO.
DEFINE VARIABLE end_date                AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE end_del-zone            AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" NO-UNDO.
DEFINE VARIABLE end_loc                 AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" NO-UNDO.
DEFINE VARIABLE end_loc-bin             AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE tb_exl-tg-bin           AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_more                 AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_p-bin                AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE tb_post-rel             AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_posted               AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_printed              AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_pricing              AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_print-component      AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_prt-part-no          AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_whs-bin-sort         AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_zone-p               AS LOGICAL INITIAL yes  NO-UNDO.
DEFINE VARIABLE tb_zone-s               AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tgMultipleReleases      AS LOGICAL INITIAL yes  NO-UNDO.
DEFINE VARIABLE rd-print-what           AS CHARACTER INITIAL "I" NO-UNDO.
DEFINE VARIABLE lines-per-page  AS INTEGER FORMAT ">>" INITIAL 99 NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.
DEF VAR lv-post AS LOG NO-UNDO.
DEFINE BUFFER bf-chk FOR ap-chk.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEF VAR lv-comp-curr AS cha NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF NEW SHARED VAR vuser AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF VAR v-VERSION AS CHAR NO-UNDO.
DEF VAR  tmp-path AS CHAR NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEF VAR v-num-custs AS INT NO-UNDO.
DEF VAR tfile AS CHAR INIT "c:\tmp\rptfile.txt" NO-UNDO.
DEF VAR v-1st-page AS LOG NO-UNDO.
DEF VAR v-multi-cust AS LOG NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.

{oe/rep/oe-pick1.i new}

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
IF AVAIL company THEN lv-comp-curr = company.curr-code.


DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.

DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.

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
 vuser     = prmUser
 v-today   = TODAY 
 g_company = cocode
 g_user    = prmUser .


 

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

  FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
  
def var v-ddate as date no-undo.

DEF VAR lv-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.

{custom/xprint.i}
DEF NEW SHARED VAR vPrinted AS LOG NO-UNDO.
DEF NEW SHARED VAR v-exc-bin AS LOG NO-UNDO.
DEF NEW SHARED VAR v-print-components AS LOG NO-UNDO.
DEF NEW SHARED VAR s-print-part-no AS LOG NO-UNDO.
DEF NEW SHARED VAR s-print-what-item AS cha NO-UNDO.
DEF NEW SHARED VAR s-print-loc-from AS cha NO-UNDO.
DEF NEW SHARED VAR s-print-loc-to AS cha NO-UNDO.
DEF NEW SHARED VAR s-print-bin-from AS cha NO-UNDO.
DEF NEW SHARED VAR s-print-bin-to AS cha NO-UNDO.
DEF NEW SHARED VAR s-print-pricing AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR vcDefaultForm AS CHAR NO-UNDO.
DEF BUFFER b-oe-relh FOR oe-relh.
DEF VAR lv-pdf-file       AS CHAR FORMAT "x(400)" NO-UNDO.
{oe/oe-relp1.i NEW}

{sys/ref/relpost.i}


/* gdm - 02020902 */
DEF VAR v-hldflg AS LOG NO-UNDO.
DEF VAR v-chkflg AS LOG NO-UNDO.

find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "RELPRINT"
      no-lock no-error.
  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "RELPRINT"
     sys-ctrl.date-fld = 12/31/99
     sys-ctrl.descrip  = "Print Release headers on Release/Packing List Form?".
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  end.

  assign
   v-relprint    = sys-ctrl.char-fld
   vcDefaultForm = sys-ctrl.char-fld
  /* tb_more       = v-relprint eq "Century"
   v-ddate       = sys-ctrl.date-fld
   v-headers     = sys-ctrl.log-fld
   tb_zone-p     = lookup(v-relprint,"Argrov,Fibre") gt 0
   tb_zone-s     = v-relprint eq "Argrov"
   v-more        = tb_more */ .


IF prmrelprt = "getvalue" THEN do:
   /* find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "RELPACK"
      no-lock no-error.
  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "RELPACK"
     sys-ctrl.descrip  = "The heading to be printed for releases/packing slips"
     sys-ctrl.char-fld = "RELEASE".
    message "System control record NOT found - enter heading :"
    update sys-ctrl.char-fld format "x(14)".
  end.
 /* rel-pack = sys-ctrl.char-fld.*/*/

                          

  do transaction:
     {sys/inc/xmlorder.i} 
  end.

    FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name EQ "RELCREDT" NO-ERROR.
    IF AVAIL sys-ctrl THEN  ASSIGN v-hldflg = sys-ctrl.log-fld.
    
    CREATE ttPrintReleaseOrd.

    ASSIGN 
        ttPrintReleaseOrd.extra = v-relprint .

END. /* getvalue */


  IF prmrelprt = "relprt " THEN DO:

      ASSIGN
       begin_cust-no         =  prmbegcust                                                                     
       begin_relnum          =  prmbegrel                                                        
       begin_ord-no          =  prmbegord                                                           
       begin_date            =  date(prmbegdate)
       begin_del-zone        =  prmbegdelz                         
       begin_loc             =  prmbegloc                               
       begin_loc-bin         =  prmbeglocbin                          
       end_cust-no           =  prmendcust                        
       end_relnum            =  prmendrel                             
       end_ord-no            =  prmendord                             
       end_date              =  date(prmenddate)
       end_del-zone          =  prmenddelz                              
       end_loc               =  prmendloc                               
       end_loc-bin           =  prmendlocbin                     
       tb_exl-tg-bin         =  IF prmextagbin = "yes" THEN TRUE ELSE FALSE                         
       tb_more               =  IF prmrel_prfm = "yes" THEN TRUE ELSE FALSE    
       tb_p-bin              =  IF prmbinloc = "yes" THEN TRUE ELSE FALSE   
       tb_post-rel           =  IF prmpstrel = "yes" THEN TRUE ELSE FALSE   
       tb_posted             =  IF prmpsted = "yes" THEN TRUE ELSE FALSE  
       tb_printed            =  IF prmprinted = "yes" THEN TRUE ELSE FALSE
       tb_pricing            =  IF prmprtpric = "yes" THEN TRUE ELSE FALSE 
       tb_print-component    =  IF prmasscomp = "yes" THEN TRUE ELSE FALSE   
       tb_prt-part-no        =  IF prmcustprt = "yes" THEN TRUE ELSE FALSE    
       tb_whs-bin-sort       =  IF prmsrtbinloc = "yes" THEN TRUE ELSE FALSE  
       tb_zone-p             =  IF prmprtdelz = "yes" THEN TRUE ELSE FALSE   
       tb_zone-s             =  IF prmsrtdelz = "yes" THEN TRUE ELSE FALSE     
       tgMultipleReleases    =  IF prmmulrel = "yes" THEN TRUE ELSE FALSE 
       rd-print-what         =  string(prmprtwht) . 
                              
                              
       FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
           sys-ctrl.NAME = "X-VERSION" NO-LOCK NO-ERROR.
       
       IF NOT AVAIL sys-ctrl THEN
           DO:
           CREATE sys-ctrl.
           ASSIGN
               sys-ctrl.company  = cocode
               sys-ctrl.name     = "X-VERSION"
               sys-ctrl.descrip  = "Server Name"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "Server 2003".
           END.
           IF AVAIL sys-ctrl  THEN
               v-VERSION = sys-ctrl.char-fld .
           RELEASE sys-ctrl.
           
           FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
               sys-ctrl.NAME = "Xspool" NO-LOCK NO-ERROR.
           
           IF NOT AVAIL sys-ctrl THEN
               DO:
               CREATE sys-ctrl.
               ASSIGN
                   sys-ctrl.company  = cocode
                   sys-ctrl.name     = "Xspool"
                   sys-ctrl.descrip  = "Default path To Create temp File for Web pdf "
                   sys-ctrl.log-fld = YES
                   sys-ctrl.char-fld = "c:\spool\".
               END.
               IF AVAIL sys-ctrl  THEN
                   tmp-path = sys-ctrl.char-fld .
                   RELEASE sys-ctrl.

                   ASSIGN  
                       init-dir    = v-webrootpath 
                       lv-pdf-file = init-dir + 'PrintPO'
                       lv-pdf-file = lv-pdf-file + string(prmbegrel) + STRING(TIME)
                       vPdfFile    = 'PrintPO' + string(prmbegrel) + STRING(TIME) + '.pdf'.
            

 
  IF v-hldflg THEN
    DO TRANSACTION:
      IF (begin_relnum EQ end_relnum) THEN DO:
         
        FIND FIRST oe-relh NO-LOCK
            WHERE oe-relh.company  EQ cocode
              AND oe-relh.release# EQ INT(begin_relnum) NO-ERROR.
        IF AVAIL oe-relh THEN DO:
            IF tb_posted THEN DO:
                RETURN.
            END.
            ELSE do:
                FIND FIRST sys-ctrl NO-LOCK
                    WHERE sys-ctrl.company EQ cocode
                    AND sys-ctrl.name EQ "RELCREDT" NO-ERROR.
                IF AVAIL sys-ctrl AND sys-ctrl.log-fld AND oe-relh.w-ord THEN do:
                    cError = "Release # is on CREDIT HOLD, Please contact your supervisor." .
                    RETURN.
                END.
            END.
        END.
      END.
      ELSE 
        IF NOT tb_posted THEN do: /* gdm - 03200904 */
          cError = "Please be advised,all releases that are on credit hold will not be printed." .
              RETURN.
        END.
  END.

  v-print-components = tb_print-component.
  s-print-part-no = tb_prt-part-no.
  v-num-custs = 0.
  FOR EACH b-oe-relh FIELDS(company cust-no ship-id r-no release#) WHERE
               b-oe-relh.company EQ cocode AND
               b-oe-relh.release# GE begin_relnum AND
               b-oe-relh.release# LE end_relnum AND
               b-oe-relh.cust-no GE begin_cust-no AND
               b-oe-relh.cust-no LE end_cust-no AND
               b-oe-relh.rel-date ge begin_date AND
               b-oe-relh.rel-date le end_date AND
               b-oe-relh.posted   eq tb_posted  AND    /* 09131307*/
               b-oe-relh.printed  EQ tb_printed AND
               b-oe-relh.stat     NE "W" 
               AND CAN-FIND(FIRST oe-rell
                       WHERE oe-rell.company EQ cocode
                         AND oe-rell.r-no    EQ b-oe-relh.r-no
                         AND oe-rell.ord-no  GE begin_ord-no
                         AND oe-rell.ord-no  LE end_ord-no
                       USE-INDEX r-no) NO-LOCK  USE-INDEX post
                BREAK BY b-oe-relh.company
                      BY b-oe-relh.cust-no:
      
               IF FIRST-OF(b-oe-relh.cust-no) THEN
                   v-num-custs = v-num-custs + 1.
  END.
  IF v-num-custs > 1 THEN
      v-multi-cust = YES.
  FOR EACH b-oe-relh FIELDS(company cust-no ship-id r-no release#) WHERE
               b-oe-relh.company EQ cocode AND
               b-oe-relh.release# GE begin_relnum AND
               b-oe-relh.release# LE end_relnum AND
               b-oe-relh.cust-no GE begin_cust-no AND
               b-oe-relh.cust-no LE end_cust-no AND
               b-oe-relh.rel-date ge begin_date AND
               b-oe-relh.rel-date le end_date AND
               b-oe-relh.posted   eq tb_posted AND   /* 09131307*/
               b-oe-relh.printed  EQ tb_printed AND
               b-oe-relh.stat     NE "W" 
               AND CAN-FIND(FIRST oe-rell
                       WHERE oe-rell.company EQ cocode
                         AND oe-rell.r-no    EQ b-oe-relh.r-no
                         AND oe-rell.ord-no  GE begin_ord-no
                         AND oe-rell.ord-no  LE end_ord-no
                       USE-INDEX r-no) NO-LOCK  USE-INDEX post
                BREAK BY b-oe-relh.company
                      BY b-oe-relh.cust-no:
      
               IF FIRST-OF(b-oe-relh.cust-no) THEN
               DO:
                  FIND FIRST sys-ctrl-shipto WHERE
                       sys-ctrl-shipto.company = cocode AND
                       sys-ctrl-shipto.NAME = "RELPRINT" AND
                       sys-ctrl-shipto.cust-vend = YES AND
                       sys-ctrl-shipto.cust-vend-no = b-oe-relh.cust-no AND
                       sys-ctrl-shipto.ship-id = b-oe-relh.ship-id AND
                       sys-ctrl-shipto.char-fld > ''
                       NO-LOCK NO-ERROR.

                  IF NOT AVAIL sys-ctrl-shipto THEN
                     FIND FIRST sys-ctrl-shipto WHERE
                          sys-ctrl-shipto.company = cocode AND
                          sys-ctrl-shipto.NAME = "RELPRINT" AND
                          sys-ctrl-shipto.cust-vend = YES AND
                          sys-ctrl-shipto.cust-vend-no = b-oe-relh.cust-no AND
                          sys-ctrl-shipto.char-fld > ''
                          NO-LOCK NO-ERROR.
    
                  IF AVAIL sys-ctrl-shipto THEN
                  DO:
                     v-relprint = sys-ctrl-shipto.char-fld.
                  END.
                  ELSE DO:
                      v-relprint = vcDefaultForm .
                  END.
                    
                  RUN set-report.
                  run run-report.
                  IF v-multi-cust THEN DO:
                    tfile = tfile + "1".
                    PAUSE 1.
                    OUTPUT CLOSE.
                    DOS SILENT COPY VALUE(list-name) VALUE(tfile).
                  END.
          END.

 END.
 IF v-multi-cust THEN DO:
   PAGE.
   OUTPUT CLOSE.
   DOS SILENT COPY "c:\tmp\rptfile.txt*" VALUE(list-name).
   DOS SILENT DEL "c:\tmp\rptfile.txt*".
 END.

 IF tb_post-rel AND NOT tb_posted THEN do:
     FOR EACH tt-except:
      DELETE tt-except.
     END.
 
     FOR EACH tt-fg-bin:
      DELETE tt-fg-bin.
     END.

     DEF VAR lv-foreachr AS CHAR NO-UNDO.


    lv-foreachr = STRING(TODAY,"99/99/9999") + STRING(TIME,"999999").

    /* gdm - 06220907 - CHECK FOR RELEASE HOLD*/        
    DEF VAR v-crdhld AS LOG NO-UNDO.

    FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name EQ "RELCREDT" NO-ERROR.
    IF AVAIL sys-ctrl THEN  ASSIGN v-crdhld = sys-ctrl.log-fld.
    /* gdm - 06220907 - CHECK FOR RELEASE HOLD end */ 

    IF NOT AVAIL oe-ctrl THEN
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK.
   
    foreachr:
    FOR EACH oe-relh NO-LOCK
        WHERE oe-relh.company  EQ cocode
          AND oe-relh.posted   EQ NO
          AND oe-relh.rel-date GE v-fdate
          AND oe-relh.rel-date LE v-tdate
          AND oe-relh.release# GE v-s-rel
          AND oe-relh.release# LE v-e-rel
          AND oe-relh.cust-no  GE v-s-cust-no
          AND oe-relh.cust-no  LE v-e-cust-no
          AND oe-relh.printed  EQ v-printed 
          AND oe-relh.stat     NE "W"
          AND CAN-FIND(FIRST oe-rell
                       WHERE oe-rell.company EQ cocode
                         AND oe-rell.r-no    EQ oe-relh.r-no
                         AND oe-rell.ord-no  GE v-s-ord
                         AND oe-rell.ord-no  LE v-e-ord
                       USE-INDEX r-no)
        USE-INDEX post:

      /* gdm - 06220907 - CHECK FOR RELEASE HOLD*/ 
      IF v-crdhld AND oe-relh.w-ord THEN NEXT.
        
      /* gdm - 06220907 - CHECK FOR RELEASE HOLD end */ 


      FOR EACH reftable
          WHERE reftable.reftable EQ "oe-relh.can-print"
            AND reftable.rec_key  EQ oe-relh.rec_key
          USE-INDEX rec_key:
        DELETE reftable.
      END.

      IF NOT oe-ctrl.p-pick THEN
      FOR EACH oe-rell
          WHERE oe-rell.company EQ oe-relh.company
            AND oe-rell.r-no    EQ oe-relh.r-no
            AND CAN-FIND(FIRST oe-ord
                         WHERE oe-ord.company EQ oe-rell.company
                           AND oe-ord.ord-no  EQ oe-rell.ord-no
                           AND oe-ord.stat    EQ "H")
          NO-LOCK:
        NEXT foreachr.
      END.

      CREATE reftable.
      ASSIGN
       reftable.reftable = "oe-relh.can-print"
       reftable.rec_key  = oe-relh.rec_key
       reftable.company  = lv-foreachr.
    END.

    RELEASE oe-relh.

    FOR EACH reftable NO-LOCK
        WHERE reftable.reftable EQ "oe-relh.can-print"
          AND reftable.company  EQ lv-foreachr,
        FIRST oe-relh WHERE oe-relh.rec_key EQ reftable.rec_key :
        RUN oe/relcheck.p (ROWID(oe-relh), OUTPUT ll).
      ll = NOT ll.
      IF NOT ll THEN DO:
        FIND FIRST tt-except NO-ERROR.
        cError =  "Release Ticket " +
                TRIM(IF oe-relh.deleted THEN "has been DELETED" ELSE
                     IF tt-except.reason EQ 1 THEN "is in use"  ELSE
                                                "has insufficient inventory") +
                " and cannot be posted..." .
        LEAVE.
      END.
    END.
    
    
  IF ll THEN do:
    RUN post-releases.

    cError =  "Posting Complete" .
  END.
 END.

        

 /*IF v-VERSION = "Server 2008" THEN do:
     OS-COPY VALUE(list-name) VALUE (tmp-path).
     PAUSE 1.
 END.
 ELSE
     RUN printFile(list-name).*/

    
    CREATE ttPrintReleaseOrd.
    ASSIGN ttPrintReleaseOrd.relprt = vPdfFile.
    
  END.
/*****************************************************************************************/

 
PROCEDURE set-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF v-relprint EQ "MultiWll" THEN
    ASSIGN
     lv-program     = "oe/rep/relmulti.p"
     lines-per-page = 60.

  ELSE
  IF v-relprint EQ "HOP" THEN 
    ASSIGN
     lv-program     = "oe/rep/relhop.p"
     lines-per-page = 60.

  ELSE
  IF v-relprint EQ "TriState" THEN
    ASSIGN
     lv-program     = "oe/rep/reltrist.p"
     lines-per-page = 60.

  ELSE
  IF v-relprint EQ "Fibre" THEN
    ASSIGN
     lv-program     = "oe/rep/relfibre.p"
     lines-per-page = 59. /* 60*/

  ELSE
  IF v-relprint EQ "Premier" THEN
    ASSIGN
     lv-program     = "oe/rep/relprem.p"
     lines-per-page = 60.
  ELSE
  IF v-relprint EQ "Carded" THEN
    ASSIGN
     lv-program     = "oe/rep/relcard.p"
     lines-per-page = 60.
  
  ELSE
  IF v-relprint EQ "Pacific" THEN
    ASSIGN
     lv-program     = "oe/rep/relpacif.p"
     lines-per-page = 75
     is-xprint-form = YES.

  ELSE
  IF v-relprint EQ "Xprint" THEN
   ASSIGN
    lv-program     = "oe/rep/relxprnt.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE
  IF v-relprint EQ "APC" THEN
   ASSIGN
    lv-program     = "oe/rep/relxapc.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE
  IF v-relprint EQ "HPB" THEN
   ASSIGN
    lv-program     = "oe/rep/relxhpb.p"
    lines-per-page = 75
    is-xprint-form = YES  .

  ELSE
  IF v-relprint EQ "PPI" THEN
   ASSIGN
    lv-program     = "oe/rep/relppi.p"
    lines-per-page = 75
    is-xprint-form = YES  .

  ELSE IF v-relprint EQ "Xprint2" THEN
    ASSIGN lv-program     = "oe/rep/relxprn2.p"
           lines-per-page = 75
           is-xprint-form = YES  . 

  ELSE IF v-relprint EQ "Sonoco" THEN DO:
   ASSIGN 
    lines-per-page = 75
    is-xprint-form = YES
    lv-program     = "oe/rep/relsonoc.p". 
  END.
  ELSE IF v-relprint EQ "Indiana" THEN
   ASSIGN
    lv-program     = "oe/rep/relindc.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Hopx" THEN
   ASSIGN
    lv-program     = "oe/rep/relhopx.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "ACPI" THEN
   ASSIGN
    lv-program     = "oe/rep/relacpi.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "CardedX" THEN
   ASSIGN
    lv-program     = "oe/rep/relcardx.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Peachtree" THEN
   ASSIGN
    lv-program     = "oe/rep/relpchtr.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/
  
  ELSE IF v-relprint EQ "Multicell" THEN
   ASSIGN
    lv-program     = "oe/rep/relmcell.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "PremierX" THEN
   ASSIGN
    lv-program     = "oe/rep/relpremx.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Frank" THEN
   ASSIGN
    lv-program     = "oe/rep/relfrnkx.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Prystup" THEN
   ASSIGN
    lv-program     = "oe/rep/relprysx.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "NStock" THEN
   ASSIGN
    lv-program     = "oe/rep/relnstok.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Soule" THEN
   ASSIGN
    lv-program     = "oe/rep/relsoule.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "StClair" THEN
   ASSIGN
    lv-program     = "oe/rep/relStClair.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "CSC-GA" THEN
   ASSIGN
    lv-program     = "oe/rep/relcsc.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Protagon" THEN
   ASSIGN
    lv-program     = "oe/rep/relprogn.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Fibrex" THEN
   ASSIGN
    lv-program     = "oe/rep/relfibx.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Metro" THEN
  ASSIGN
   lv-program     = "oe/rep/relmetro.p"
   lines-per-page = 75
   is-xprint-form = YES  . /*60*/

  ELSE
  IF v-relprint EQ "CentBox" THEN
   ASSIGN
    lv-program     = "oe/rep/relcntbx.p"
    lines-per-page = 59.

  ELSE
  IF v-relprint EQ "Keystone" THEN
   ASSIGN
    lv-program     = "oe/rep/relkeyst.p"
    lines-per-page = 59.

  ELSE
  IF v-relprint EQ "Frankstn" THEN
   ASSIGN
    lv-program     = "oe/rep/relfrank.p"
    lines-per-page = 57. /*was 59*/  

  ELSE
  IF v-zone-s THEN
    ASSIGN
     lv-program     = "oe/rep/oe-pick1.p"
     lines-per-page = 60.

  ELSE
  IF v-relprint EQ "Hughes" THEN
    ASSIGN
     lv-program     = "oe/rep/relhughs.p"
     lines-per-page = 59. /* 60*/

  ELSE IF v-relprint EQ "Allwest" THEN
    ASSIGN lv-program     = "oe/rep/relallws.p"
           lines-per-page = 75
           is-xprint-form = YES  . 
  ELSE IF v-relprint EQ "CCC" THEN
    ASSIGN lv-program     = "oe/rep/relccc.p"
           lines-per-page = 75
           is-xprint-form = YES  . 
  /* gdm - 10080912*/
  ELSE IF v-relprint EQ "Rosmar" THEN DO:
   ASSIGN 
    lines-per-page = 75
    is-xprint-form = YES
    lv-program     = "oe/rep/relrosmr.p".    
  END.
  /* gdm - 09220907 */
  ELSE IF v-relprint EQ "Loylang" THEN DO:  
   ASSIGN
    lv-program     = "oe/rep/relloyl.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/
  END.
  ELSE
  IF v-more THEN
    ASSIGN
     lv-program     = "oe/rep/rel-more.p"
     lines-per-page = 55.
  ELSE
    ASSIGN
     lv-program     = "oe/rep/oe-pick2.p"
     lines-per-page = 60.

  v-lines-per-page = lines-per-page.

END PROCEDURE.


 
PROCEDURE run-report :
/* -------------------------------------------- oe/rep/oe-pick.p 6/94 rd ---- */
/* print oe Release/Picking tickets                                           */
/* -------------------------------------------------------------------------- */
    
{sys/FORM/r-top.i}

ASSIGN
 v-s-cust-no  = b-oe-relh.cust-no
 v-e-cust-no  = b-oe-relh.cust-no
 v-s-rel      = begin_relnum
 v-e-rel      = end_relnum
 v-s-ord      = begin_ord-no
 v-e-ord      = end_ord-no
 v-fdate      = begin_date
 v-tdate      = end_date
 v-s-ter      = begin_del-zone
 v-e-ter      = end_del-zone
 v-printed    = tb_printed
 v-posted     = tb_posted
 v-more       = tb_more
 v-p-bin      = tb_p-bin
 v-posted     = tb_posted
 v-zone-s     = tb_zone-s
 v-zone-p     = tb_zone-p
 v-sort-loc-bin = tb_whs-bin-sort
 s-print-what-item = rd-print-what
 s-print-pricing = tb_pricing
 vPrinted        = tb_printed
 v-exc-bin    = tb_exl-tg-bin.

IF lookup(v-relprint,"Hopx,ACPI,Fibrex,Metro,Carded,Loylang,PremierX,Frank,NSTOCK,CSC-GA,Protagon,CardedX,Peachtree,Multicell,CCC,Soule,Prystup,StClair") > 0 AND
   LOOKUP(s-print-what-item,"I,S") > 0 THEN 
   ASSIGN s-print-loc-from = begin_loc
          s-print-loc-to = END_loc
          s-print-bin-from = begin_loc-bin
          s-print-bin-to = END_loc-bin.

if v-posted then DO:
  find first oe-relh
      where oe-relh.company  eq cocode
        and oe-relh.posted   eq yes
        and oe-relh.release# eq begin_relnum
      no-error.

  IF AVAIL oe-relh THEN 
     ASSIGN oe-relh.posted = no
            v-date         = oe-relh.rel-date.
  tb_printed     = YES.

  DISPLAY tb_printed.

  IF AVAIL oe-relh THEN
  find first oe-bolh
      where oe-bolh.company  eq cocode
        AND oe-bolh.release# EQ oe-relh.release#
      use-index release# no-lock no-error.
  if AVAIL oe-relh AND not avail oe-bolh THEN
    message "Please enter new Release Date:"
            update oe-relh.rel-date format "99/99/9999".

  if AVAIL oe-relh AND oe-relh.rel-date ne v-date then do:
    for each oe-rell
        where oe-rell.company eq cocode
          and oe-rell.r-no    eq oe-relh.r-no
        USE-INDEX r-no,

        first oe-rel
        where oe-rel.link-no eq oe-rell.r-no
        use-index seq-no:

      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq oe-rel.ord-no
            and oe-ordl.i-no    eq oe-rel.i-no
            AND oe-ordl.LINE    EQ oe-rell.LINE
          no-error.
      if avail oe-ordl then
        oe-ordl.t-rel-qty = oe-ordl.t-rel-qty - oe-rel.qty.

      assign
       oe-rell.posted  = no
       oe-rel.rel-date = oe-relh.rel-date
       oe-rel.link-no  = 0.
    end.
  end.
end.

FIND CURRENT oe-rell NO-LOCK NO-ERROR.
FIND CURRENT oe-rel NO-LOCK NO-ERROR.

/*{sys/inc/print1.i}*/
 if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\tmp" + string(time)
       init-dir = tmp-dir.

v-count = v-count + 1.
list-name = list-name + STRING(v-count).
{sys/inc/outprint.i value(lines-per-page)}


v-lines-per-page = lines-per-page.

PUT "<PDF=DIRECT><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-LEFT=2mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(320)".

RUN value(lv-program).
if xmlorder-log then run createxmlrelease.

IF AVAIL oe-relh AND v-posted AND oe-relh.rel-date EQ v-date THEN DO TRANSACTION:
  oe-relh.posted = YES.
END.

FIND CURRENT oe-relh NO-LOCK NO-ERROR.


/* end ---------------------------------- copr. 2002 Advanced Software, Inc. */

end procedure.


PROCEDURE post-releases :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR ll-exception AS LOG NO-UNDO.
DEF VAR v-first-release AS LOG NO-UNDO.
DEF VAR v-royal AS LOG NO-UNDO.

DEF BUFFER upd-oe-relh FOR oe-relh.

{sa/sa-sls01.i}

DISABLE TRIGGERS FOR LOAD OF itemfg.

IF relpost-chr EQ "Nothing" THEN DO:
  /*{oe/rep/foreachr3.i}

  headblok:
  {oe/rep/foreachr2.i}.

    {oe/oe-relp.i}
  END. /* each oe-relh */*/
END.

ASSIGN
 v-frel     = begin_relnum
 v-trel     = end_relnum
 v-fdat     = begin_date
 v-tdat     = end_date
 v-fcus     = begin_cust-no
 v-tcus     = end_cust-no
 v-ford     = begin_ord-no
 v-tord     = end_ord-no.

/*RUN oe/oe-relp2.p (v-term, v-royal).*/

/*FOR EACH report WHERE report.term-id EQ v-term,
    FIRST oe-boll
    WHERE RECID(oe-boll) EQ report.rec-id
      AND CAN-FIND(FIRST oe-bolh
                   WHERE oe-bolh.b-no    EQ oe-boll.b-no
                     AND oe-bolh.printed EQ NO)
    NO-LOCK:
  DELETE report.
END.*/

/*RUN oe/oe-bolp3.p (v-term).*/

FIND CURRENT oe-relh NO-LOCK NO-ERROR.
FIND CURRENT oe-rell NO-LOCK NO-ERROR.
FIND CURRENT itemfg NO-LOCK NO-ERROR.
FIND CURRENT oe-ordl NO-LOCK NO-ERROR.
/*FIND CURRENT upd-oe-relh NO-LOCK NO-ERROR.*/

END PROCEDURE.


