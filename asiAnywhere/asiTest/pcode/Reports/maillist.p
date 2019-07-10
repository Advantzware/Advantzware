

/*------------------------------------------------------------------------
    File        : maillist.p
    Purpose     : AP Mailing List
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttAPMailingList NO-UNDO
        FIELD mail AS CHAR
        FIELD extra  AS CHAR .
        
       

DEFINE DATASET dsAPMailingList FOR ttAPMailingList.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmmail          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegstat        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegtyp        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegbuy        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendstat        AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmendtyp       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendbuy        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmactiv        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmoutput        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAPMailingList.

     IF prmUser     = ? THEN ASSIGN   prmUser      = "".   
     IF prmmail     = ? THEN ASSIGN   prmmail      = "". 
     IF prmbegstat  = ? THEN ASSIGN   prmbegstat   = "". 
     IF prmbegtyp   = ? THEN ASSIGN   prmbegtyp    = "". 
     IF prmbegbuy   = ? THEN ASSIGN   prmbegbuy    = "".
     IF prmendstat  = ? THEN ASSIGN   prmendstat   = "". 
     IF prmendtyp   = ? THEN ASSIGN   prmendtyp    = "". 
     IF prmendbuy   = ? THEN ASSIGN   prmendbuy    = "".
     IF prmactiv    = ? THEN ASSIGN   prmactiv     = "". 
     IF prmoutput   = ? THEN ASSIGN   prmoutput    = "".
     IF prmOut      = ? THEN ASSIGN   prmOut       = "".
     

DEFINE VARIABLE begin_state     AS CHARACTER FORMAT "XX" NO-UNDO.
DEFINE VARIABLE begin_type      AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_buyer     AS CHARACTER FORMAT "XXX"     NO-UNDO.
DEFINE VARIABLE end_state       AS CHARACTER FORMAT "AA":U INITIAL "zz" NO-UNDO.
DEFINE VARIABLE end_type        AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" no-UNDO.
DEFINE VARIABLE end_buyer       AS CHARACTER FORMAT "XXX":U INITIAL "zzz" NO-UNDO.
DEFINE VARIABLE rd_active       AS CHARACTER NO-UNDO.
DEFINE VARIABLE rd_output       AS CHARACTER NO-UNDO.

DEF VAR g_company AS CHAR NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
def var list-name as cha no-undo.
def var list-name2 as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-r-no LIKE rm-rctd.r-no NO-UNDO.
DEF VAR t-setup AS LOG INITIAL NO NO-UNDO.
DEF NEW SHARED VAR vuser AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VARIABLE tb_excel      AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)"  NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
DEF VAR lv-list-name LIKE list-name EXTENT 2 NO-UNDO.
DEFINE VARIABLE tb_runExcel   AS LOGICAL INITIAL no NO-UNDO.

DEF VAR lv-txt-file AS cha NO-UNDO.
DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(256)":U   /*INITIAL "C:\Inetpub\wwwroot\pdfs\openord.csv" */       NO-UNDO.

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
 g_company = prmComp
 vuser     = prmUser .

    
FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.


DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF STREAM excel.




  IF prmmail = "mail" THEN DO:
     
        ASSIGN
        v-today      = TODAY
        begin_state  = prmbegstat
        begin_type   = prmbegtyp 
        begin_buyer  = prmbegbuy 
        end_state    = prmendstat
        end_type     = prmendtyp 
        end_buyer    = prmendbuy 
        rd_active    = prmactiv   
        rd_output    = prmoutput . 
    
        assign
        init-dir    = v-webrootpath
        lv-txt-file =  'MailList' + STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) +  ".txt" .
       
        
        v-excel-file =  'MailList' +
             STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".
        ASSIGN
            fi_file = init-dir + "\" + v-excel-file .

        
        run run-report. 

        CREATE ttAPMailingList.

        IF prmOut = "Yes" THEN
        ASSIGN ttAPMailingList.mail = v-excel-file.
        ELSE
            ASSIGN ttAPMailingList.mail = lv-txt-file .


 


  END.
/*****************************************************************************************/
PROCEDURE run-report :
/* -------------------------------------------- ap/rep/mailing.p ---- */
/* Extracting Vendor Information                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.f}

def var vfst like vend.state.
def var vtst like vend.state initial "ZZ".
def var vfty like vend.type.
def var vtty like vend.type  initial "ZZZZZ".
def var vfsm like vend.buyer.
def var vtsm like vend.buyer initial "ZZZZZ".
def var vfsu like vend.code-1099.
def var vtsu like vend.code-1099 initial "ZZZZZ".
def var vdest  as char format "x(11)".

def var xx as inte initial 1.
def var save_id as recid.
def var v-ans as logical.
def var plabel as char format "x(24)" extent 9 .
def var cnt as integer.
DEF VAR excelheader AS CHAR NO-UNDO.

assign
 str-tit2 = "Mailing List"
 {sys/inc/ctrtext.i str-tit2 56}
 
 vfst      = begin_state
 vtst      = end_state
 vfty      = begin_type
 vtty      = end_type
 vfsm      = begin_buyer
 vtsm      = end_buyer
 vdest     = rd_output.

 /*{sys/inc/print1.i}*/
   if tmp-dir = "" then tmp-dir = v-webrootpath .
   assign list-name = tmp-dir + lv-txt-file
       init-dir = tmp-dir.


{sys/inc/outprint.i 0 } /*value(lines-per-page)}*/

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Contact,Vend #,Name,Address 1,Address 2,City,State,Zip".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.


FOR EACH vend NO-LOCK
    WHERE vend.company   EQ cocode
      AND vend.state     GE vfst
      AND vend.state     LE vtst
      AND vend.type      GE vfty
      AND vend.type      LE vtty
      AND vend.buyer     GE vfsm
      AND vend.buyer     LE vtsm
      AND vend.code-1099 GE vfsu
      AND vend.code-1099 LE vtsu
      AND (vend.active   EQ SUBSTR(rd_active,1,1) OR rd_active BEGINS "All")
    BY vend.vend-no:

  if vdest eq "Labels" then do:
    cnt = 0.

    if vend.contact ne "" then do:
      cnt = cnt + 1.
      plabel[cnt] = trim(vend.contact).  /* 1: contact */
    end.
    
    if vend.vend-no ne "" then do:
      if cnt eq 0 then do:
        cnt = cnt + 1.
        plabel[cnt] = vend.vend-no.       
      end.
      
      else
        plabel[cnt] = plabel[cnt] + " " + vend.vend-no.   /* 1: vend-no*/
    end.
    
    if vend.name ne "" then do:
      cnt = cnt + 1.
      plabel[cnt] = trim(vend.name).        /* 2: vend.name */
    end.
    
    if vend.add1 ne "" then do:
      cnt = cnt + 1.
      plabel[cnt] = vend.add1.    /* 3: addr1 */
    end.
    
    if vend.add2 ne "" then do:
      cnt = cnt + 1.
      plabel[cnt] = vend.add2.          /* 4: addr2 */
    end.
    
    if vend.city ne "" or vend.state ne "" or vend.zip ne "" then do:
      cnt = cnt + 1.
      if vend.city ne "" then
        plabel[cnt] = trim(vend.city) + ", ".
      if vend.state ne "" then
        plabel[cnt] = plabel[cnt] + vend.state + " ".
      if vend.zip ne "" then
        plabel[cnt] = plabel[cnt] + vend.zip.            /* 5: city,state,zip*/
    end.

    do cnt = 1 TO 9 :
      put plabel[cnt] skip.
          plabel[cnt] = "".
    end.
  end.
   
  else
  DO:
    put unformatted
        vend.contact format "x(15)"
         vend.vend-no format "x(8)"
         vend.name    format "x(30)"
         vend.add1    format "x(30)" 
         vend.add2    format "x(30)"
         vend.city    format "x(16)"
         vend.state   format "x(2)"
         vend.zip     format "x(10)" skip.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
           '"' vend.contact                  '",'
           '"' vend.vend-no                  '",'
           '"' vend.name                     '",'
           '"' vend.add1                     '",'
           '"' vend.add2                     '",'
           '"' vend.city                     '",'
           '"' vend.state                    '",'
           '"' vend.zip                      '",'
           SKIP.
  END.
end.

 IF tb_excel THEN DO:
    OUTPUT STREAM excel CLOSE.
    
 END.

 

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.
