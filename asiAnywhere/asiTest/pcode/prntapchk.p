

/*------------------------------------------------------------------------
    File        : prntapchk.p
    Purpose     : Print AP Check
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttPrintAPCheck NO-UNDO
        FIELD apchek AS CHAR
        FIELD extra  AS CHAR .
        
       

DEFINE DATASET dsPrintAPCheck FOR ttPrintAPCheck.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmapchek        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmChekdate      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbnkcode       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbnkname       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmstrtchek      AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmBegVend       AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmEndVend       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPrintAPCheck.

     IF prmUser      = ? THEN ASSIGN    prmUser       = "".   
     IF prmapchek    = ? THEN ASSIGN    prmapchek     = "". 
     IF prmChekdate  = ? THEN ASSIGN    prmChekdate   = "". 
     IF prmbnkcode   = ? THEN ASSIGN    prmbnkcode    = "". 
     IF prmbnkname   = ? THEN ASSIGN    prmbnkname    = "".
     IF prmstrtchek  = ? THEN ASSIGN    prmstrtchek   = 0. 
     IF prmBegVend   = ? THEN ASSIGN    prmBegVend    = "". 
     IF prmEndVend   = ? THEN ASSIGN    prmEndVend    = "".
     IF prmOut       = ? THEN ASSIGN    prmOut        = "". 

DEFINE VARIABLE bank-code       AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE bank-name       AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE VARIABLE begin_vend-no   AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE check-date      AS DATE FORMAT "99/99/9999" NO-UNDO.
DEFINE VARIABLE end_vend-no     AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE start_check-no  AS INTEGER FORMAT ">>>>>9" INITIAL 0 NO-UNDO.



DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.

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

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
IF AVAIL company THEN lv-comp-curr = company.curr-code.

def new shared var v-post as log init NO NO-UNDO.
def new shared var v-trnum as INT NO-UNDO.


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
  ASSIGN  
      init-dir    = v-webrootpath .



DEF VAR tmp-dir AS cha NO-UNDO.

{ap/ap-chk.i NEW }
def var laser-list as char no-undo
   init "l,Laser,Raritan,Hartford,ASILaser,TriadLas,ASI2000,AllLaser,Argvlas,Action".

def var next-program as char no-undo init "ap/ap-chks.p".    /* std */
DEF VAR lv-prt-bypass AS LOG NO-UNDO.
DEF VAR ll-warned AS LOG NO-UNDO.
DEF VAR num-of-chks AS INT NO-UNDO.
DEF VAR sel-per-chk AS INT NO-UNDO.
DEF VAR vcDefaultForm AS CHAR NO-UNDO.
DEF BUFFER b-ap-chk FOR ap-chk.
DEF BUFFER b-vend FOR vend.

  ASSIGN
     check-date = TODAY
     vcDefaultForm = sys-ctrl.char-fld.

  RUN SetChkForm(INPUT sys-ctrl.char-fld).
  
  IF prmapchek = "apchek" THEN DO:
     
        ASSIGN
       bank-code       =  prmbnkcode                                 
       bank-name       =  prmbnkname                                 
       begin_vend-no   =  prmBegVend                                 
       check-date      =  date(prmChekdate)                                
       end_vend-no     =  prmEndVend                                   
       start_check-no  =  prmstrtchek .                                   



        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "APCheck" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vTextFile2 =  "APCheck" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .
        

        FIND FIRST period NO-LOCK
        WHERE period.company EQ cocode
          AND period.pst     LE DATE(prmChekdate)
          AND period.pend    GE DATE(prmChekdate)
          AND period.pstat
         NO-ERROR.
        
        IF NOT AVAIL period THEN DO:
            cError = "Check Date period closed..." .
            RETURN .
        END.

       FIND FIRST bank NO-LOCK
            WHERE bank.company   EQ cocode
            AND bank.bank-code EQ prmbnkcode
            NO-ERROR.

        IF NOT AVAIL bank THEN DO:
            cError =  "Invalid bank, try help...".
            RETURN .
        END.

        IF INT(prmstrtchek) EQ 0 THEN 
            prmstrtchek = (bank.last-chk + 1).
        
        
        stnum = START_check-no.

        IF CAN-FIND(FIRST sys-ctrl-shipto
                    WHERE sys-ctrl-shipto.company = cocode
                    AND sys-ctrl-shipto.NAME = "CHKFMT") THEN DO:
            IF CAN-FIND(FIRST ap-chk
                        where ap-chk.company   eq cocode
                        and ap-chk.vend-no   ge begin_vend-no
                        and ap-chk.vend-no   le end_vend-no
                        and ap-chk.man-check eq no
                        and can-find(first ap-sel
                                     where ap-sel.company   eq cocode
                                     and ap-sel.vend-no   eq ap-chk.vend-no
                                     and ap-sel.man-check eq no)) THEN
                for each b-ap-chk
                where b-ap-chk.company   eq cocode
                AND b-ap-chk.vend-no   ge begin_vend-no
                and b-ap-chk.vend-no   le end_vend-no
                and b-ap-chk.man-check eq no
                and can-find(first ap-sel
                             where ap-sel.company   eq cocode
                               and ap-sel.vend-no   eq b-ap-chk.vend-no
                               and ap-sel.man-check eq no),
                first b-vend
                where b-vend.company eq cocode
                  and b-vend.vend-no eq b-ap-chk.vend-no
                break by b-ap-chk.company
                      BY b-ap-chk.vend-no:
      
                IF FIRST-OF(b-ap-chk.vend-no) THEN
                DO:
                   FIND FIRST sys-ctrl-shipto
                   WHERE sys-ctrl-shipto.company      = cocode
                     AND sys-ctrl-shipto.NAME         = "CHKFMT"
                     AND sys-ctrl-shipto.cust-vend    = NO
                     AND sys-ctrl-shipto.cust-vend-no = b-ap-chk.vend-no 
                     AND sys-ctrl-shipto.char-fld > '' 
                     NO-LOCK NO-ERROR.

                   IF AVAIL sys-ctrl-shipto THEN
                      RUN SetChkForm (sys-ctrl-shipto.char-fld).
                   ELSE
                      RUN SetChkForm (vcDefaultForm).
      
                   RUN run-report(b-ap-chk.vend-no, TRUE).
                    
                END.
           END. /* FOR EACH */
        ELSE
           cError = "No Checks Were Printed.".
     END. /* if can-find sys-ctrl-shipto*/  
  else /*not can-find sys-ctrl-shipto*/
  DO:                                    
     RUN run-report("", FALSE).
     
  END.


  
        
   
  CREATE ttPrintAPCheck.
    ASSIGN ttPrintAPCheck.apchek = vTextFile .

    
  END.
/*****************************************************************************************/

  PROCEDURE SetChkForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAM icFormName AS CHAR NO-UNDO.
    
   ASSIGN
      laser-chk = NO
      lv-prt-bypass = NO.

   if lookup(icFormName,laser-list) gt 0 then
      assign
         laser-chk = yes
         v-print-fmt = "s".
   else
      if icFormName eq "s" or icFormName eq "ASI" then
         v-print-fmt = "s".
   ELSE
      v-print-fmt = "n".

   /*================*/
   CASE icFormName:
       WHEN "Brick" THEN
          assign
             max-per-chk  = 20
             next-program = "ap/ap-ckbrk.p".
       WHEN "AIHalper" THEN
          assign
             max-per-chk  = 13
             next-program = "ap/ap-ckaih.p".
       WHEN "P&P" THEN
          assign
             max-per-chk  = 13
             next-program = "ap/ap-ckpnp.p".
       WHEN "ContSrvc" THEN
          assign
             max-per-chk  = 13
             next-program = "ap/ap-ckcsc.p".
       WHEN "Pacific" THEN
          assign
             max-per-chk  = 8
             next-program = "ap/ap-ckpqp.p".
       WHEN "Triad" THEN
          assign
             max-per-chk  = 16
             next-program = "ap/ap-cktri.p".
       WHEN "Royal" THEN
          assign
             max-per-chk  = 17
             next-program = "ap/ap-ckroy.p".
       WHEN "Danbury" THEN
          assign
             max-per-chk  = 16
             next-program = "ap/ap-ckdan.p".
       WHEN "Rudd" then
          assign
             max-per-chk  = 13
             next-program = "ap/ap-ckrud.p".
       WHEN "Hartford" then
          assign
             max-per-chk  = 12
             next-program = "ap/ap-ckhar.p".
       WHEN "Inland" then
          assign
             max-per-chk  = 12
             next-program = "ap/ap-ckinl.p".
       WHEN "Fibre" THEN
          assign
             max-per-chk  = 13
             next-program = "ap/ap-ckfib.p".
       WHEN "Herman" THEN
          assign
             max-per-chk  = 11
             next-program = "ap/ap-ckher.p".
       WHEN "Chillic" then
          assign
             max-per-chk  = 7
             next-program = "ap/ap-ckchl.p".
        WHEN "CAPLasAL" then
          assign
             max-per-chk  = 12
             next-program = "ap/ap-ckcap.p".
       WHEN "Midwest" THEN
          assign
             max-per-chk  = 14
             next-program = "ap/ap-ckmwf.p".
       WHEN "Middlesx" THEN
          assign
             max-per-chk  = 7
             next-program = "ap/ap-ckmid.p".
       WHEN "Hughes" THEN
          assign
             laser-chk = yes
             max-per-chk  = 16
             next-program = "ap/ap-ckhug.p".
       WHEN "Unipak" then
          assign
             laser-chk = yes
             max-per-chk  = 13
             next-program = "ap/ap-ckuni.p".
       WHEN "FibreLsr" THEN
          ASSIGN
             laser-chk = yes
             max-per-chk  = 28
             next-program = "ap/ap-ckfibl.p".
       WHEN "RFC" THEN
          assign
             laser-chk = yes
             max-per-chk  = 12
             next-program = "ap/ap-ckrfc.p".
       WHEN "Prefered" THEN
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckprf.p".
       WHEN "ASI2000" then
          assign
             max-per-chk  = 9
             next-program = "ap/ap-ckasi.p".
       WHEN "TriadLas" THEN
          assign
             max-per-chk  = 9
             next-program = "ap/chktriad.p".
       WHEN "Action" THEN
          assign
             max-per-chk  = 9
             next-program = "ap/chkaction.p".

       WHEN "IPLaser" THEN /*Interpak Laser*/
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckitp.p".
       WHEN "ASILaser" THEN
          assign
             max-per-chk  = 12
             next-program = "ap/ap-ckasi.p".
       WHEN "Carded" THEN
          assign
             max-per-chk  = 12
             next-program = "ap/ap-ckcard.p".

       WHEN "Frankstn" then
          assign
             max-per-chk  = 12
             next-program = "ap/ap-ckfnk.p".
       WHEN "Imperial" THEN
          assign
             max-per-chk  = 12
             next-program = "ap/ap-ckimp.p".
       WHEN "Harwllas" THEN
          assign
             max-per-chk  = 12               
             next-program = "ap/ap-ckhrw.p".
       WHEN "TRILAKES" THEN
          assign
             max-per-chk  = 10
             next-program = "ap/ap-cklak.p". /* same as ap-ckasi.p */
       WHEN "COLORLAS" THEN /* color carton laser format very close to TRILAKES*/
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckclr.p". /* same as ap-ckasi.p */
       WHEN "ADVLaser" then
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckadv.p". /* same as ap-ckasi.p */
       WHEN "ASSILaser" then
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckassi.p". /* same as ap-ckasi.p */
       WHEN "Soule" then
          assign
             max-per-chk  = 10
             next-program = "ap/ap-cksoule.p". /* new format */
       WHEN "MidYork" then
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckmyork.p". /* new format */
       WHEN "STCLaser" THEN
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckstc.p". /* same as ap-ckadv.p */
       WHEN "Lakelas" then
          assign
             max-per-chk  = 10
             next-program = "ap/ap-cklks.p". /* same as ap-ckasi.p */
       WHEN "IndianaL" THEN
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckind.p". /* same as ap-ckasi.p */
       WHEN "Adaptls" THEN
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckada.p". /* same as ap-ckasi.p */
       WHEN "Vineland" THEN
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckvnl.p". /* from as ap-ckadv.p */
       WHEN "Oracle" THEN
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckora.p". /* almost same as ap-ckadb.p */
       WHEN "Dayton" THEN
          assign
             max-per-chk  = 12
             next-program = "ap/ap-ckday.p".
       WHEN "AllLaser" THEN /* all package Laser*/
          assign
             max-per-chk  = 20
             next-program = "ap/ap-ckall.p".
       WHEN "Argvlas" THEN
          assign
             max-per-chk  = 20
             next-program = "ap/ap-ckarg.p".
       WHEN "Woodland" THEN
          assign
             laser-chk    = yes
             max-per-chk  = 12
             next-program = "ap/ap-ckwdl.p".
       WHEN "Hamilton" THEN
          assign
             max-per-chk  = 13
             next-program = "ap/ap-ckham.p". /* same as ap-ckasi.p */
       WHEN "PrePkgLS" then
          assign
             max-per-chk  = 10
             next-program = "ap/ap-ckpre.p".
       WHEN "ACPI" then
          assign
             laser-chk = yes
             max-per-chk  = 13
             next-program = "ap/ap-ckacp.p".
       OTHERWISE DO:
          assign
             max-per-chk  = if v-print-fmt eq "s" then 20 else 12
             next-program = "ap/ap-chk" + v-print-fmt + ".p".
   
          if search(next-program) eq ? then
             next-program = "ap/ap-chk" + v-print-fmt + ".r".
       END.

   END CASE.

END PROCEDURE.


PROCEDURE run-report :

DEFINE INPUT PARAM icVendNo AS CHAR NO-UNDO.
DEFINE INPUT PARAM ip-sys-ctrl-mode AS LOG NO-UNDO.

DEF VAR lFlag AS LOGICAL NO-UNDO INIT YES.

/* Finds/creates tmp directory */
/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.

   IF LOOKUP(next-program,"ap/ap-ckfibl.p,ap/ap-ckuni.p") EQ 0 THEN DO:
     {sys/inc/outprint.i 0}  /* output to value(list-name) page-size {1} */
   END.
  ELSE  /*Fibre and Unipak Laser page statement needs PAGED*/
     OUTPUT TO VALUE(list-name) PAGED.




outers:
do on error undo outers, leave outers :

  ASSIGN num-of-chks = 0
         sel-per-chk = 0
         x-bank = bank-code
         wdate = check-date.

  IF ip-sys-ctrl-mode THEN
     ASSIGN
        wvend-no = icVendNo
        evend-no = icVendNo.
  ELSE
     ASSIGN
        wvend-no = begin_vend-no
        evend-no = end_vend-no.

  find first bank
      where bank.company   eq cocode
        and bank.bank-code eq bank-code
      no-error.

  for each ap-chk
      where ap-chk.company   eq cocode
        and ap-chk.vend-no   ge wvend-no
        and ap-chk.vend-no   le evend-no
        and ap-chk.man-check eq no
        and can-find(first ap-sel
                     where ap-sel.company   eq cocode
                       and ap-sel.vend-no   eq ap-chk.vend-no
                       and ap-sel.man-check eq no),
       
      first vend
      where vend.company eq cocode
        and vend.vend-no eq ap-chk.vend-no
        
      break by ap-chk.vend-no:
    assign
     ap-chk.check-no  = ?
     ap-chk.check-act = bank.actnum
     sel-per-chk      = 0
     num-of-chks      = num-of-chks + 1.
      
    for each ap-sel
        where ap-sel.company   eq cocode
          and ap-sel.vend-no   eq ap-chk.vend-no
          and ap-sel.man-check eq no
        no-lock
       
        break by ap-sel.inv-no:
       
      sel-per-chk = sel-per-chk + 1.
      
      if sel-per-chk eq max-per-chk and not last(ap-sel.inv-no) then
        assign
         num-of-chks = num-of-chks + 1
         sel-per-chk = 0.
    end.
  end.
  
  /* Code Added for Validating that check numbers have not been posted  */
  /* num-of-chks is actually one less then then number of checks so     */
  /* check range is appropriate number                                  */

  if num-of-chks gt 0 then
  find first ap-pay
      where ap-pay.company   eq cocode      
        AND ap-pay.bank-code EQ bank.bank-code  /* gdm - */    
        and ap-pay.check-act eq bank.actnum
        and ap-pay.check-no  ge stnum
        and ap-pay.check-no  le stnum + num-of-chks - 1
        and ap-pay.posted    eq yes
      no-lock no-error.
  /*if avail ap-pay then do:
      ASSIGN
     cError =
            "           At Least One of the Check Numbers Between </br>  
             string(stnum) " and " string(stnum + num-of-chks) </br>
                             Has Already Been Posted! </br>
             Please Rerun Checks with Different Starting Check Number.".
                
    undo outers, leave outers.
  end.*/

  /* ========= print check =========*/
  
  v-print-mode = "PROD".  /* need it to see for test */


  run value(next-program).
  /*======== end printing =========*/

  find first bank where bank.company   eq cocode
                    and bank.bank-code eq bank-code.
  bank.last-chk = stnum - 1.

  for each ap-chk
      where ap-chk.company   eq cocode
        and ap-chk.man-check eq no
      no-lock,
      each ap-sel EXCLUSIVE-LOCK
      where ap-sel.company   eq cocode
        and ap-sel.vend-no   eq ap-chk.vend-no
        and ap-chk.man-check eq no:

    assign
     ap-sel.check-no  = ap-chk.check-no
     ap-sel.bank-code = ap-chk.bank-code
     ap-sel.actnum    = ap-chk.check-act.
  end.
END. /* outers */


end procedure.

PROCEDURE valid-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG NO-UNDO.


 
    FIND FIRST period NO-LOCK
        WHERE period.company EQ cocode
          AND period.pst     LE DATE(prmChekdate)
          AND period.pend    GE DATE(prmChekdate)
          AND period.pstat
        NO-ERROR.
    IF NOT AVAIL period THEN DO:
      cError = TRIM(prmChekdate) + " period closed...".
          
      RETURN .
    END.

    IF NOT ll-warned THEN DO:
      ll = NO.

      FOR EACH period NO-LOCK
          WHERE period.company EQ cocode
            AND period.pst     LE TODAY
            AND period.pend    GE TODAY
          BY period.pst:

        IF period.pst  GT DATE(prmChekdate) OR
           period.pend LT DATE(prmChekdate) THEN DO:
          ll = YES.
          cError = TRIM(prmChekdate) + " Check date is not in current period, would you like to re-enter...".
              UPDATE ll.
        END.

        IF ll THEN DO:
          RETURN.
        END.

        LEAVE.
      END.

      ll-warned = YES.
    END.


END PROCEDURE.
