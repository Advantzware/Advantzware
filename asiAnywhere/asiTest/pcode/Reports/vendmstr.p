

/*------------------------------------------------------------------------
    File        : vendmstr.p
    Purpose     : Vendor Master List
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttVendorMasterList NO-UNDO
        FIELD venmstr AS CHAR
        FIELD extra  AS CHAR .
        
       

DEFINE DATASET dsVendorMasterList FOR ttVendorMasterList.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmvenmstr       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegvend        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegtyp        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegbuy        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendvend        AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmendtyp       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendbuy        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmdetail        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVendorMasterList.

     IF prmUser     = ? THEN ASSIGN    prmUser     = "".   
     IF prmvenmstr  = ? THEN ASSIGN    prmvenmstr  = "". 
     IF prmbegvend  = ? THEN ASSIGN    prmbegvend  = "". 
     IF prmbegtyp   = ? THEN ASSIGN    prmbegtyp   = "". 
     IF prmbegbuy   = ? THEN ASSIGN    prmbegbuy   = "".
     IF prmendvend  = ? THEN ASSIGN    prmendvend  = "". 
     IF prmendtyp   = ? THEN ASSIGN    prmendtyp   = "". 
     IF prmendbuy   = ? THEN ASSIGN    prmendbuy   = "".
     IF prmdetail   = ? THEN ASSIGN    prmdetail   = "". 
     IF prmOut      = ? THEN ASSIGN    prmOut      = "".
     

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_type AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_buyer AS CHARACTER FORMAT "XXX" NO-UNDO.
DEFINE VARIABLE end_vend     AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_type    AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" no-UNDO.
DEFINE VARIABLE end_buyer   AS CHARACTER FORMAT "XXX":U INITIAL "zzz" NO-UNDO.
DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no NO-UNDO.


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

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
     NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.


DEF STREAM excel.




  IF prmvenmstr = "venmstr" THEN DO:
     
        ASSIGN
        v-today        = TODAY
        begin_vend     = prmbegvend
        begin_type     = prmbegtyp 
        begin_buyer    = prmbegbuy 
        end_vend       = prmendvend
        end_type       = prmendtyp 
        end_buyer      = prmendbuy 
        tb_detailed    = IF prmdetail = "Yes" THEN TRUE ELSE FALSE . 
    
       assign
        init-dir    = v-webrootpath
        lv-txt-file =  'VendMaster' + STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .
       
        
        v-excel-file =  'VendMaster' +
             STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".
        ASSIGN
            fi_file = init-dir + "\" + v-excel-file .

        
        run run-report. 

        CREATE ttVendorMasterList.

        IF prmOut = "Yes" THEN
        ASSIGN ttVendorMasterList.venmstr = v-excel-file.
        ELSE
            ASSIGN ttVendorMasterList.venmstr = lv-txt-file .


 


  END.
/*****************************************************************************************/

  PROCEDURE run-report :
/* --------------------------------------------------- ap/vendlist.p 11/94 RM */
/* Vendor Master List Report Program - A/P Module                             */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.f}

def var fco as ch.
def var tco like fco.
def var fvend as ch.
def var tvend like fvend initial "ZZZZZZZZ".
def var ftype as ch.
def var ttype like ftype initial "ZZZZZZZZ".
def var fbuy like vend.buyer.
def var tbuy like vend.buyer initial "ZZZ".
def var detailed as logical format "Yes/No" initial no.
DEF VAR excelheader AS CHAR NO-UNDO.

form
   skip(1)
   "Vendor:" at 1
   vend.vend-no at 9 skip
   vend.name at 9
   "Type:" to 52 vend.type
   "Active ?:" to 73 vend.active skip
   vend.add1 at 9
   "Contact:" to 52 vend.contact skip
   vend.add2 at 9
   "Telephone:" to 52 vend.area-code vend.phone skip
   vend.city at 9
   "Fax:" to 52 vend.fax-area vend.fax skip
   vend.country at 9
   vend.postal at 21 
   "Default GL#:" to 52 vend.actnum skip
   with frame vend2 overlay no-labels stream-io width 80 down.

form
   skip(1)
   "Vendor:" at 1
   vend.vend-no at 9 skip
   vend.name at 9
   "Type:" to 52 vend.type
   "Active ?:" to 73 vend.active skip
   vend.add1 at 9
   "Contact:" to 52 vend.contact skip
   vend.add2 at 9
   "Telephone:" to 52 vend.area-code vend.phone skip
   vend.city at 9
   "Fax:" to 52 vend.fax-area vend.fax skip
   vend.country at 9
   vend.postal at 21 
   "Default GL#:" to 52 vend.actnum skip(1)
   "Buyer Code:" to 14 vend.buyer
   "Company Code:" to 52 vend.company skip
   "Buyer Name:" to 14 vend.buyer-n format "x(20)"
   "Carrier :" to 52 vend.carrier skip
   "Terms Code:" to 14 vend.terms  terms.dscr format "x(15)"
   "FOB Code:" to 52 vend.fob-code skip
   "Discount %:" to 14 vend.disc-%
   "1099 Code:" to 52 vend.code-1099 skip
   "Discount Days:" to 14 vend.disc-days
   "Date Last Paid:" to 52 vend.lpay-date skip
   skip(1)
   with frame vend overlay no-labels stream-io width 80 down.


assign
 str-tit2 = "Vendor Master List"
 {sys/inc/ctrtext.i str-tit2 56}
 
 fco      = cocode
 tco      = cocode
 fvend    = begin_vend
 tvend    = end_vend
 ftype    = begin_type
 ttype    = end_type
 fbuy     = begin_buyer
 tbuy     = end_buyer
 detailed = tb_detailed. 

/*{sys/inc/print1.i}*/
   if tmp-dir = "" then tmp-dir = v-webrootpath .
   assign list-name = tmp-dir + lv-txt-file
       init-dir = tmp-dir.

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Vendor,Name,Type,Active,Contact,Address 1,Address 2,City,State,"
              + "Zip,Country,Postal Code,Telephone,Fax,Default GL#,".
  IF detailed THEN
     excelheader = excelheader
                 + "Buyer Code,Company Code,Buyer Name,Carrier,FOB Code,"
                 + "Terms Code,Terms Description,Discount %,1099 Code,Discount Days,Date Last Paid".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.


  display "" with frame r-top.


  for each vend where (vend.company >= fco and vend.company <= tco)     and
                          (vend.vend-no >= fvend and vend.vend-no <= tvend) and 
                          (vend.type >= ftype and vend.type <= ttype) and       
                          (vend.buyer >= fbuy and vend.buyer <= tbuy)
                          NO-LOCK
                          break by vend.vend-no with frame vend2:
      
         if detailed then do with frame vend:

            find first terms where terms.t-code = vend.terms no-lock no-error.

            display
                  vend.vend-no
                  vend.name
                  vend.add1
                  vend.add2
                  vend.city + ", " + vend.state + " " + vend.zip
                       format "x(30)" @ vend.city
                  vend.type
                  vend.active
                  vend.contact
                  vend.area-code
                  vend.phone
                  vend.fax-area
                  vend.fax
                  vend.country
                  vend.postal
                  vend.actnum
                  vend.buyer
                  vend.company
                  vend.buyer-n
                  vend.carrier
                  vend.fob-code
                  vend.terms
                  "" @ terms.dscr
                  terms.dscr when available terms
                  vend.code-1099
                  vend.disc-%
                  vend.lpay-date
                  vend.disc-days
                  vend.actnum.
            down.

            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                   '"' vend.vend-no                        '",'
                   '"' vend.NAME                           '",'
                   '"' vend.TYPE                           '",'
                   '"' vend.active                         '",'              
                   '"' vend.contact                        '",'
                   '"' vend.add1                           '",'
                   '"' vend.add2                           '",'
                   '"' vend.city                           '",'
                   '"' vend.state                          '",'
                   '"' vend.zip                            '",'
                   '"' vend.country                        '",'
                   '"' vend.postal                         '",'
                   '"' STRING(vend.area-code,"(999)") +
                       " " + STRING(vend.phone,"999-9999") '",'
                   '"' STRING(vend.fax-area,"(999)") +
                       " " STRING(vend.fax,"999-9999")     '",'
                   '"' vend.actnum                         '",'
                   '"' vend.buyer                          '",'
                   '"' vend.company                        '",'
                   '"' vend.buyer-n                        '",'
                   '"' vend.carrier                        '",'
                   '"' vend.fob-code                       '",'
                   '"' vend.terms                          '",'
                   '"' IF AVAIL terms THEN terms.dscr
                       ELSE ""                             '",'
                   '"' STRING(vend.disc-%,">>9.99%")       '",'
                   '"' vend.code-1099                      '",'
                   '"' vend.disc-days                      '",'
                   '"' IF vend.lpay-date NE ? THEN
                          STRING(vend.lpay-date) ELSE ""   '",'
                   SKIP.
         end.

         else
         DO:
             
            display 
                    vend.vend-no
                    vend.name
                    vend.type
                    vend.active
                    vend.add1
                    vend.contact
                    vend.add2
                    vend.area-code
                    vend.phone
                    vend.city + ", " + vend.state + " " + vend.zip
                       format "x(30)" @ vend.city
                    vend.fax-area
                    vend.fax
                    vend.country
                    vend.postal
                    vend.actnum.

            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                   '"' vend.vend-no                        '",'
                   '"' vend.NAME                           '",'
                   '"' vend.TYPE                           '",'
                   '"' vend.active                         '",'
                   '"' vend.contact                        '",'
                   '"' vend.add1                           '",'
                   '"' vend.add2                           '",'
                   '"' vend.city                           '",'
                   '"' vend.state                          '",'
                   '"' vend.zip                            '",'
                   '"' vend.country                        '",'
                   '"' vend.postal                         '",'
                   '"' STRING(vend.area-code,"(999)") +
                       " " + STRING(vend.phone,"999-9999") '",'
                   '"' STRING(vend.fax-area,"(999)") +
                       " " + STRING(vend.fax,"999-9999")   '",'
                   '"' vend.actnum                         '",'
                   SKIP.
         END.
         down.
         
      end.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  /*IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).*/
END.


/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

