

/*------------------------------------------------------------------------
    File        : printpo.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttPrintPoReport NO-UNDO
        FIELD prtpo AS CHAR
        FIELD extra  AS CHAR.
        
       

DEFINE DATASET dsPrintPoReport FOR ttPrintPoReport.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmprtpo         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegpo         AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmendpo         AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegvend       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendvend       AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmreprt         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmreprtcl       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmdelete        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmprttrm        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmspec          AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmcorr          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmgrpnts        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmsmmritm       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmitmdsr        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmscrtyp        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmmetric        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmprtprice      AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmysno           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPrintPoReport.

     IF prmUser         = ? THEN ASSIGN     prmUser          = "".   
     IF prmprtpo        = ? THEN ASSIGN     prmprtpo         = "". 
     IF prmbegpo        = ? THEN ASSIGN     prmbegpo         = 0. 
     IF prmendpo        = ? THEN ASSIGN     prmendpo         = 0. 
     IF prmbegvend      = ? THEN ASSIGN     prmbegvend       = "". 
     IF prmendvend      = ? THEN ASSIGN     prmendvend       = "".   
     IF prmreprt        = ? THEN ASSIGN     prmreprt         = "". 
     IF prmreprtcl      = ? THEN ASSIGN     prmreprtcl       = "". 
     IF prmdelete       = ? THEN ASSIGN     prmdelete        = "". 
     IF prmprttrm       = ? THEN ASSIGN     prmprttrm        = "". 
     IF prmspec         = ? THEN ASSIGN     prmspec          = "".   
     IF prmcorr         = ? THEN ASSIGN     prmcorr          = "". 
     IF prmgrpnts       = ? THEN ASSIGN     prmgrpnts        = "". 
     IF prmsmmritm      = ? THEN ASSIGN     prmsmmritm       = "". 
     IF prmitmdsr       = ? THEN ASSIGN     prmitmdsr        = "". 
     IF prmscrtyp       = ? THEN ASSIGN     prmscrtyp        = "". 
     IF prmmetric       = ? THEN ASSIGN     prmmetric        = "". 
     IF prmprtprice     = ? THEN ASSIGN     prmprtprice      = "". 
     IF prmOut          = ? THEN ASSIGN     prmOut           = "".  
     IF prmysno         = ? THEN ASSIGN     prmysno          = "". 



DEFINE VARIABLE begin_po-no         AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 NO-UNDO.
DEFINE VARIABLE begin_vend-no       AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE end_po-no           AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 NO-UNDO.
DEFINE VARIABLE end_vend-no         AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE tb_corr             AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_delete           AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_group-notes      AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_itemDescription  AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_metric           AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_print-prices     AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_print-terms      AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_reprint          AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_reprint-closed   AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_score-types      AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_spec             AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE tb_Summarize-by-item AS LOGICAL INITIAL no NO-UNDO.
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
  

DEF VAR v-program         AS CHAR NO-UNDO.
DEF VAR is-xprint-form    AS LOG  NO-UNDO.
DEF VAR ls-fax-file       AS CHAR NO-UNDO.
DEF VAR lv-multi-faxout   AS LOG  NO-UNDO.
DEF VAR lv-fax-image      AS CHAR NO-UNDO.
DEF VAR lv-pdf-file       AS CHAR FORMAT "x(400)" NO-UNDO.
DEF VAR lv-exp-form-list  AS CHAR NO-UNDO INIT "CorrTrim,Alliance,HRMS,CorSuply,Corr-U-KraftII,GP,Kiwi,Smurfit,CorrChoice,Pratt".
DEF VAR lv-exp-prog-list  AS CHAR NO-UNDO INIT "po-ctexp,po-alexp,po-hrexp,po-csexp,po-ckexp,po-gpexp,po-kwexp,po-smurfi,po-ccexp,po-prexp".
DEF VAR vcDefaultForm     AS CHAR NO-UNDO.
DEF VAR lv-fax-type       AS CHAR NO-UNDO.

DEF TEMP-TABLE w-export NO-UNDO
    FIELD w-exp-prog   AS CHAR.
 
{po/po-print.i NEW}   
{custom/xprint.i}

DEF NEW SHARED VAR s-group-notes AS LOG NO-UNDO.
DEF NEW SHARED VAR s-print-prices AS LOG NO-UNDO.

DEFINE NEW SHARED 
       TEMP-TABLE tt-filelist
            FIELD tt-FileCtr  AS INT
            FIELD tt-FileName AS CHAR
            INDEX filelist    IS PRIMARY TT-FILECTR.

DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHAR NO-UNDO.

{sys/inc/poexport.i}
{sys/inc/poimage.i}

DEFINE BUFFER b1-po-ord   FOR po-ord.
DEF BUFFER bf-attach FOR attach.
DEFINE VARIABLE vcErrorMsg AS CHARACTER  NO-UNDO.
DEF VAR li-lineperpage AS INT NO-UNDO.


FUNCTION AttachmentExists RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF SEARCH (lv-pdf-file) EQ ? THEN DO:

      IF SEARCH (lv-pdf-file + '.pdf') EQ ? THEN DO:

        IF SEARCH (list-name) NE ? THEN DO:

          OS-RENAME VALUE (list-name) VALUE (list-name + '.txt').

          IF OS-ERROR NE 0 THEN DO:

            cError = "Failed to rename attachment file." .

            RETURN FALSE.
          END.

          ELSE lv-pdf-file = list-name + '.txt'.
        END.
      END.

      ELSE
        lv-pdf-file = lv-pdf-file + '.pdf'.

      IF SEARCH (lv-pdf-file) = ? THEN DO:
        cError = "Attachment File is missing.".
        RETURN FALSE.
      END.
    END.

    RETURN TRUE.

END FUNCTION.


IF prmprtpo = "getvalue" THEN do:
    find first sys-ctrl
        where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "POPRINT"
        no-lock no-error.
    if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        assign
            sys-ctrl.company  = cocode
            sys-ctrl.name     = "POPRINT"
            sys-ctrl.descrip  = "Print Sheet Size?   16th's or 32nd's?"
            sys-ctrl.char-fld = "32nd's"
            sys-ctrl.log-fld  = no.
        cError = "System control record NOT found. Print Sheet Size? ".
        UPDATE sys-ctrl.log-fld.
        cError = "16th's or 32nd's display? ". update sys-ctrl.char-fld.
    END.
    assign
        v-print-fmt = sys-ctrl.char-fld
        v-shtsiz    = sys-ctrl.log-fld.
    
    CREATE ttPrintPoReport.

    ASSIGN 
        ttPrintPoReport.extra = v-print-fmt .

END. /* getvalue */


IF prmprtpo = "prtpo " THEN DO:
    
    find first sys-ctrl
        where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "POPRINT"
        no-lock no-error.
    if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        assign
            sys-ctrl.company  = cocode
            sys-ctrl.name     = "POPRINT"
            sys-ctrl.descrip  = "Print Sheet Size?   16th's or 32nd's?"
            sys-ctrl.char-fld = "32nd's"
            sys-ctrl.log-fld  = no.
        cError = "System control record NOT found. Print Sheet Size? ".
        UPDATE sys-ctrl.log-fld.
        cError = "16th's or 32nd's display? ". update sys-ctrl.char-fld.
    END.
    assign
        v-print-fmt = sys-ctrl.char-fld
        v-shtsiz    = sys-ctrl.log-fld.
    
    find first po-ctrl where po-ctrl.company eq cocode no-lock no-error.
    assign
        v-pre-printed-forms = po-ctrl.pre-printed-forms
        v-company           = po-ctrl.prcom
        vcDefaultForm = v-print-fmt.

    FIND FIRST users WHERE
        users.user_id EQ USERID("NOSWEAT")
        NO-LOCK NO-ERROR.
    
    IF AVAIL users AND users.user_program[2] NE "" THEN
        init-dir = users.user_program[2].
    ELSE
        init-dir = "c:\tmp".
        
        RUN SetPOPrintForm (v-print-fmt).

        ASSIGN
            lines-per-page              = li-lineperpage .

      /*  IF NOT CAN-DO('Brick,CSC,Southpak,Xprint,PeachTree,Asixprnt,PPI,CSC-GA,Indiana,Packrite,Allwest,ACPI,CCC,Protagon,SouleMed,Soule',v-print-fmt) THEN DISABLE tb_spec.*/

    IF NOT CAN-DO('Xprint,PeachTree,Protagon,PPI,Packrite',v-print-fmt) THEN DO:
       IF v-print-fmt NE "CentBox" THEN
          ASSIGN
             tb_itemDescription = NO
             prmitmdsr = 'NO'
      
         tb_score-types              = CAN-DO("Premierx,PremierCX,Fibrex,MWFibre,Protagon",v-print-fmt)
         prmscrtyp = STRING(tb_score-types) .
        /* tb_score-types:SENSITIVE    = YES.*/
      
      /* DISABLE tb_itemDescription
               tb_score-types. */
    END.

   /* IF v-print-fmt EQ "CentBox" THEN
       ASSIGN
          tb_itemDescription:LABEL = "Print P.O. Description Lines"
          tb_itemDescription:SENSITIVE = YES. */

  /*  IF LOOKUP(v-print-fmt,"xprint,PeachTree,Protagon,ppi,Packrite") = 0 THEN 
       DISABLE tb_metric.*/

    IF v-print-fmt NE "Indiana" THEN
     ASSIGN prmprtprice = "NO" .
           /* tb_print-prices:SENSITIVE = NO.*/

   /* IF NOT poexport-log THEN DISABLE tb_corr.
    APPLY "entry" TO begin_po-no IN FRAME {&FRAME-NAME}. */

   

END.


  IF prmprtpo = "prtpo " THEN DO:

      ASSIGN
       begin_po-no              =  prmbegpo                                                       
       begin_vend-no            =  prmbegvend                                                    
       end_po-no                =  prmendpo                                                          
       end_vend-no              =  prmendvend                                                          
       tb_corr                  =  IF prmcorr = "yes" THEN TRUE ELSE FALSE                          
       tb_delete                =  IF prmdelete = "yes" THEN TRUE ELSE FALSE                             
       tb_group-notes           =  IF prmgrpnts = "yes" THEN TRUE ELSE FALSE                           
       tb_itemDescription       =  IF prmitmdsr = "yes" THEN TRUE ELSE FALSE                       
       tb_metric                =  IF prmmetric = "yes" THEN TRUE ELSE FALSE                           
       tb_print-prices          =  IF prmprtprice = "yes" THEN TRUE ELSE FALSE                           
       tb_print-terms           =  IF prmprttrm = "yes" THEN TRUE ELSE FALSE                         
       tb_reprint               =  IF prmreprt = "yes" THEN TRUE ELSE FALSE                             
       tb_reprint-closed        =  IF prmreprtcl = "yes" THEN TRUE ELSE FALSE                           
       tb_score-types           =  IF prmscrtyp = "yes" THEN TRUE ELSE FALSE                       
       tb_spec                  =  IF prmspec = "yes" THEN TRUE ELSE FALSE                          
       tb_Summarize-by-item     =  IF prmsmmritm = "yes" THEN TRUE ELSE FALSE .  



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
                       lv-pdf-file = lv-pdf-file + string(prmbegpo)
                       vPdfFile    = 'PrintPO' + string(prmbegpo) + '.pdf'.
       
        

        

        ASSIGN
    v-start-po          = begin_po-no
    v-end-po            = end_po-no 
    v-reprint-po        = tb_reprint
    v-printde-po        = tb_delete
    v-print-sn          = tb_spec
    v-corrugator        = tb_corr
    v-sendfax           = NO
    v-faxprog           = ""
    v-tmp-fax           = ""
    s-group-notes       = tb_group-notes
    v-summarize-by-item = tb_summarize-by-item
    v-itemDescription   = tb_itemDescription
    v-score-types       = tb_score-types
    v-metric            = tb_metric
    s-print-prices      = tb_print-prices
    v-print-terms       = tb_print-terms.
 
  IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
     sys-ctrl-shipto.company = cocode AND
     sys-ctrl-shipto.NAME = "POPRINT") THEN
     DO:
        IF CAN-FIND(FIRST b1-po-ord
            WHERE  b1-po-ord.company EQ cocode
              AND (b1-po-ord.stat    EQ "N" OR 
                   b1-po-ord.stat    EQ "O" OR 
                   b1-po-ord.stat    EQ "U" OR
                  (tb_reprint-closed AND b1-po-ord.stat EQ "C"))
              AND  b1-po-ord.printed EQ v-reprint-po
              AND  b1-po-ord.po-no   GE v-start-po
              AND  b1-po-ord.po-no   LE v-end-po
              AND  b1-po-ord.vend-no GE begin_vend-no
              AND  b1-po-ord.vend-no LE end_vend-no) THEN
            FOR EACH  b1-po-ord /* FIELDS(vend-no company) */
                WHERE  b1-po-ord.company EQ cocode
                  AND (b1-po-ord.stat    EQ "N" OR 
                       b1-po-ord.stat    EQ "O" OR 
                       b1-po-ord.stat    EQ "U" OR
                      (tb_reprint-closed AND b1-po-ord.stat EQ "C"))
                  AND  b1-po-ord.printed EQ v-reprint-po
                  AND  b1-po-ord.po-no   GE v-start-po
                  AND  b1-po-ord.po-no   LE v-end-po
                  AND  b1-po-ord.vend-no GE begin_vend-no
                  AND  b1-po-ord.vend-no LE end_vend-no
              NO-LOCK
             BREAK BY b1-po-ord.company
                   BY b1-po-ord.vend-no:
            
               IF FIRST-OF (b1-po-ord.vend-no) THEN DO:
                 
                 FIND FIRST sys-ctrl-shipto
                      WHERE sys-ctrl-shipto.company      = cocode
                        AND sys-ctrl-shipto.NAME         = "POPRINT"
                        AND sys-ctrl-shipto.cust-vend    = NO
                        AND sys-ctrl-shipto.cust-vend-no = b1-po-ord.vend-no 
                        AND sys-ctrl-shipto.char-fld > '' 
                      NO-LOCK NO-ERROR.
            
                 IF AVAIL sys-ctrl-shipto THEN
                 DO:
                    RUN SetPOPrintForm (sys-ctrl-shipto.char-fld) .
                    v-print-fmt = sys-ctrl-shipto.char-fld.
                 END.
                 ELSE
                 DO:
                    RUN SetPOPrintForm (vcDefaultForm).
                    v-print-fmt = vcDefaultForm.
                 END.
                 
                 RUN SetGlobalVariables(INPUT b1-po-ord.po-no).
                 
                 RUN run-report(b1-po-ord.vend-no, TRUE) . 
            
                 
               END.
            END. /*FOR EACH*/
         ELSE
            cError = "No Purchase Orders Were Printed.".
     END.
     ELSE
         DO:
        v-print-fmt = vcDefaultForm.
        RUN SetGlobalVariables(INPUT begin_po-no).
        RUN run-report("", FALSE) .
        
     END.

        

 IF v-VERSION = "Server 2008" THEN do:
     OS-COPY VALUE(list-name) VALUE (tmp-path).
     PAUSE 1.
 END.
 ELSE
     RUN printFile(list-name).

    
    CREATE ttPrintPoReport.
    ASSIGN ttPrintPoReport.prtpo = vPdfFile.
    
  END.
/*****************************************************************************************/
PROCEDURE SetPOPrintForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAM icPrintFormat  AS CHAR NO-UNDO.

  v-print-fmt = icPrintFormat.

  CASE icPrintFormat:
    
    WHEN 'MiddleSx'     THEN ASSIGN v-program = "po/pomidsex.p"     li-lineperpage = 64.
    WHEN 'Century'      THEN ASSIGN v-program = "po/po-cent.p"      li-lineperpage = 56. /* was 63*/    
    WHEN 'Rudd'         THEN ASSIGN v-program = "po/po-rudd.p"      li-lineperpage = 60.
    WHEN 'Brick'        THEN ASSIGN v-program = "po/po-brick.p"     li-lineperpage = 60.
    WHEN 'Fibre'        THEN ASSIGN v-program = "po/po-fibre.p"     li-lineperpage = 56.
    WHEN 'P&P'          THEN ASSIGN v-program = "po/po-pnp.p"       li-lineperpage = 60.
    WHEN 'Pacific'      THEN ASSIGN v-program = "po/po-pacif.p"     li-lineperpage = 80.
    WHEN 'Elite'        THEN ASSIGN v-program = "po/po-elite.p"     li-lineperpage = 80.
    WHEN 'CSC'          THEN ASSIGN v-program = "po/po-xcsc.p"      li-lineperpage = 80.
    WHEN 'Xprint'       THEN ASSIGN v-program = "po/po-xprnt.p"     li-lineperpage = 80.
    WHEN 'PPI'          THEN ASSIGN v-program = "po/po-ppi.p"       li-lineperpage = 80.
    WHEN 'FibreX'       THEN ASSIGN v-program = "po/po-fibx.p"      li-lineperpage = 60.
    WHEN 'ConsBox'      THEN ASSIGN v-program = "po/po-consb.p"     li-lineperpage = 80.
    WHEN 'APC'          THEN ASSIGN v-program = "po/po-consb.p"     li-lineperpage = 80.
    WHEN 'Xprint2'      THEN ASSIGN v-program = "po/po-xprt2.p"     li-lineperpage = 80.
    WHEN 'OTTpkg'       THEN ASSIGN v-program = "po/po-ott.p"       li-lineperpage = 80.
    WHEN 'Hughes'       THEN ASSIGN v-program = "po/po-hughs.p"     li-lineperpage = 80.
    WHEN 'southpak'     THEN ASSIGN v-program = "po/po-sthpk.p"     li-lineperpage = 80.
    WHEN 'Indiana'      THEN ASSIGN v-program = "po/po-indiana.p"   li-lineperpage = 80.
    WHEN 'CSC-GA'       THEN ASSIGN v-program = "po/po-cscga.p"     li-lineperpage = 80.
    WHEN "southpak-xl"  THEN ASSIGN v-program = "po/po-sthpk-xl.p"  li-lineperpage = 80.  
    WHEN "asixprnt"     THEN ASSIGN v-program = "po/po-asix.p"      li-lineperpage = 80.  
    WHEN "PremierX"     THEN ASSIGN v-program = "po/po-xprem.p"     li-lineperpage = 80.  
    WHEN "PremierCX"    THEN ASSIGN v-program = "po/po-cxprem.p"     li-lineperpage = 80.
    WHEN "Protagon"     THEN ASSIGN v-program = "po/po-protg.p"     li-lineperpage = 85.  
    WHEN "Protagon2"    THEN ASSIGN v-program = "po/po-protg2.p"    li-lineperpage = 85.
    WHEN "Centbox"      THEN ASSIGN v-program = "po/po-centx.p"     li-lineperpage = 80.  
    WHEN "Valley"       THEN ASSIGN v-program = "po/po-valy.p"      li-lineperpage = 80.  
    WHEN "Oracle"       THEN ASSIGN v-program = "po/po-oracl.p"     li-lineperpage = 80.
    WHEN "HPB"          THEN ASSIGN v-program = "po/po-hpb.p"       li-lineperpage = 80. 
    WHEN "metro"        THEN ASSIGN v-program = "po/po-metro.p"     li-lineperpage = 80.  
    WHEN "MWFibre"      THEN ASSIGN v-program = "po/po-mwfiber.p"   li-lineperpage = 80.
    WHEN 'Packrite'     THEN ASSIGN v-program = "po/po-pkrit.p"     li-lineperpage = 80.
    WHEN 'Allwest'      THEN ASSIGN v-program = "po/po-allws.p"     li-lineperpage = 80.
    WHEN 'ACPI'         THEN ASSIGN v-program = "po/po-acpi.p"      li-lineperpage = 80.                                                   
    WHEN 'Badger'       THEN ASSIGN v-program = "po/po-badgr.p"     li-lineperpage = 80.
    WHEN 'CCC'          THEN ASSIGN v-program = "po/po-ccc.p"       li-lineperpage = 80.
    WHEN 'Soule'        THEN ASSIGN v-program = "po/po-soule.p"     li-lineperpage = 80.
    WHEN 'SouleMed'     THEN ASSIGN v-program = "po/po-soulemed.p"  li-lineperpage = 80.
    WHEN 'PeachTree'    THEN ASSIGN v-program = "po/po-pchtree.p"   li-lineperpage = 80.
    OTHERWISE                
       ASSIGN v-program       = "po/po-asi.p"       
              li-lineperpage  =  IF v-print-fmt EQ "Sonoco" THEN 66 ELSE 60.
  END CASE.

END PROCEDURE.

PROCEDURE SetGlobalVariables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-po-ord-no AS INT NO-UNDO.

  IF lookup(v-print-fmt,"Pacific,Xprint,PeachTree,Xprint2,Southpak,Hughes,CENTbox,Oracle,metro,PremierX,PremierCX,Protagon,Protagon2,CSC,Elite,ottpkg,APC,consbox,FibreX,ASIXprnt,Valley,PPI,CSC-GA,HPB,Indiana,MWFibre,Packrite,Allwest,ACPI,Badger,CCC,SouleMed,Soule") > 0 
    THEN is-xprint-form = YES.
    ELSE is-xprint-form = NO.

  ASSIGN
      lv-multi-faxout = /*IF (begin_vend-no NE end_vend-no  OR
                                            is-xprint-form) THEN
                          YES
                       ELSE*/
                          NO
    lv-fax-type = IF lv-multi-faxout THEN "MULTI" 
                                      ELSE "VENDOR" .
    /* lv-pdf-file = init-dir + (IF v-print-fmt EQ "Centbox" THEN
                                  "\CBXPO" 
                               ELSE
                                  "\PrintPO") + string(ip-po-ord-no) NO-ERROR. */
END PROCEDURE.

PROCEDURE run-report :
/* --------------------------------------------------- po/po-print.p 10/94 rd */
/* Purchase Order Print Program - P/O Module                                  */
/* -------------------------------------------------------------------------- */

  DEFINE INPUT PARAM icVendNo AS CHAR NO-UNDO.
  DEFINE INPUT PARAM ip-sys-ctrl-shipto AS LOG NO-UNDO.

  {sys/form/r-top.i}
  
  ASSIGN
    v-start-po          = begin_po-no
    v-end-po            = end_po-no
    v-reprint-po        = tb_reprint
    v-printde-po        = tb_delete
    v-print-sn          = tb_spec
    v-corrugator        = tb_corr
    v-sendfax           = NO
    v-faxprog           = ""
    v-tmp-fax           = ""
    s-group-notes       = tb_group-notes
    v-summarize-by-item = tb_summarize-by-item
    v-itemDescription   = tb_itemDescription
    v-score-types       = tb_score-types
    v-metric            = tb_metric
    s-print-prices      = tb_print-prices
    v-print-terms       = tb_print-terms.

  IF ip-sys-ctrl-shipto THEN
     ASSIGN
        v-start-vend = icVendNo
        v-end-vend   = icVendNo.
  ELSE
     ASSIGN
        v-start-vend = begin_vend-no
        v-end-vend   = end_vend-no.
  
 
  {sa/sa-sls01.i}
  
  v-term-id = v-term.
  
  FOR EACH po-ord NO-LOCK
     WHERE po-ord.company EQ cocode
       AND po-ord.printed EQ v-reprint-po
       AND po-ord.po-no   GE v-start-po
       AND po-ord.po-no   LE v-end-po
       AND po-ord.vend-no GE v-start-vend
       AND po-ord.vend-no LE v-end-vend:

    IF NOT(po-ord.stat EQ "N" OR po-ord.stat EQ "O" OR po-ord.stat EQ "U" OR
       (tb_reprint-closed AND po-ord.stat EQ "C")) THEN
       NEXT.
  
    CREATE report.
    ASSIGN
     report.term-id = v-term
     report.key-01  = po-ord.vend-no
     report.key-02  = STRING(po-ord.po-no,"9999999999")
     report.rec-id  = RECID(po-ord).
  END.
  
  {sys/inc/print1.i}
  
 /* {sys/inc/outprint.i VALUE(lines-per-page)}*/
  
{sys/inc/outprint.i VALUE(300)}
  
  v-lines-per-page = lines-per-page.
  
 

  IF v-print-fmt = "Centbox" OR v-print-fmt = "VALLEY" 
                THEN PUT "<PDF=DIRECT><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-LEFT=2mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(320)".
                ELSE PUT "<PDF=DIRECT><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-LEFT=2mm><PDF-OUTPUT=" + lv-pdf-file  + ".pdf>" FORM "x(300)".


  IF LOOKUP(v-print-fmt,"SOUTHPAK,SouthPak-xl,CENTBOX,Oracle,metro,ASIXprnt,Valley,CSC-GA,HPB,Indiana,XPRINT,PeachTree,ACPI,CCC,SouleMed,Soule") > 0 THEN 
      RUN VALUE(v-program) (lv-multi-faxout,lines-per-page). 
  ELSE  
    RUN VALUE(v-program). 
  
  FOR EACH reftable WHERE reftable.reftable EQ "vend.poexport" TRANSACTION:
    FIND FIRST vend
         WHERE vend.company   EQ reftable.company
           AND vend.vend-no   EQ reftable.code
           AND vend.po-export EQ "" NO-ERROR.
    IF AVAIL vend THEN vend.po-export = reftable.dscr.
    DELETE reftable.
  END.
  
  IF v-corrugator AND poexport-log THEN DO:
    FOR EACH w-export:
      DELETE w-export.
    END.
  
    IF poexport-cha EQ "Vendor" THEN
    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
       FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id NO-LOCK,
       FIRST vend NO-LOCK
       WHERE vend.company   EQ po-ord.company
         AND vend.vend-no   EQ po-ord.vend-no
         AND vend.po-export NE ""
       BREAK 
          BY vend.po-export:
  
      IF FIRST-OF(vend.po-export) THEN RUN create-export (vend.po-export).
    END.
  
    ELSE RUN create-export (poexport-cha).
  
   FOR EACH w-export WHERE TRIM(w-exp-prog) NE "":
        RUN VALUE("po/" + TRIM(w-exp-prog) + ".p") (v-print-fmt) NO-ERROR.
    END.
  END.
  
  FOR EACH report WHERE report.term-id EQ v-term-id: 
    DELETE report.
  END.
  
  
  /*OUTPUT CLOSE.*/

end procedure.

PROCEDURE create-export :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-export AS CHAR NO-UNDO.


  CREATE w-export.
  w-exp-prog = ENTRY(LOOKUP(ip-export,lv-exp-form-list),lv-exp-prog-list).

END PROCEDURE.
