
DEFINE VARIABLE v-tot-msf AS DECIMAL FORMAT ">>>>,>>9.999":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE fi_cust-part AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE TEMP-TABLE ttInvoicePo NO-UNDO
BEFORE-TABLE beforeInvoicePo
        FIELD po-no       AS INTEGER FORMAT ">>>>>9"
        FIELD po-date     AS DATE    FORMAT "99/99/9999" 
        FIELD TYPE        AS CHAR    FORMAT "x"
        FIELD stat        AS CHAR    FORMAT "x"
        FIELD loc         AS CHAR    FORMAT "x(5)"
        FIELD msf         AS DECIMAL FORMAT ">>>>,>>9.999" INITIAL 0
        FIELD i-no        AS CHAR    FORMAT "x(15)"
        FIELD i-name      AS CHAR    FORMAT "x(30)"
        FIELD cust-no     AS CHAR    FORMAT "x(8)"
        FIELD part        AS CHAR    
        FIELD due-date1   AS DATE    FORMAT "99/99/9999" 
        FIELD ord-qty     AS DECIMAL FORMAT "->>>,>>>,>>9.9<<<<<"
        FIELD cost        AS DECIMAL FORMAT "->,>>>,>>9.99<<<<<"
        FIELD tot-cost    AS DECIMAL FORMAT "->>>,>>9.99<<"
        FIELD job-no      AS CHAR    FORMAT "x(6)"   
        FIELD job-no2     AS INTEGER FORMAT "99"
        FIELD s-num       AS INTEGER FORMAT "99"
        FIELD inv-no      AS CHAR    FORMAT "X(12)"
        FIELD inv-date    AS DATE    FORMAT "99/99/9999" 
        FIELD Dscr        AS CHAR    FORMAT "X(40)"
        FIELD qty         AS DECIMAL FORMAT "->>>,>>>,>>9.9<<<<<" 
        FIELD uom         AS CHAR    FORMAT "X(4)"
        FIELD amt         AS DECIMAL FORMAT "->>>>,>>9.99"
    .
DEFINE DATASET dsInvoicePo FOR ttInvoicePo .
DEFINE QUERY q-InvoicePoQuery FOR ttInvoicePo.
DEFINE DATA-SOURCE src-InvoicePo  FOR QUERY q-InvoicePoQuery.
BUFFER ttInvoicePo :ATTACH-DATA-SOURCE(DATA-SOURCE src-InvoicePo  :HANDLE).



/*********************************************************************************************/

PROCEDURE local-display-fields :
  fi_cust-part = "".

  IF AVAIL po-ordl THEN DO:
    
    IF NOT po-ordl.item-type THEN
    FIND FIRST itemfg
        WHERE itemfg.company EQ po-ordl.company
          AND itemfg.i-no    EQ po-ordl.i-no
        NO-LOCK NO-ERROR.

    IF AVAIL itemfg THEN fi_cust-part = itemfg.part-no.

    ELSE DO:
      FIND FIRST item
          WHERE item.company EQ po-ordl.company
            AND item.i-no    EQ po-ordl.i-no
          NO-LOCK NO-ERROR.
      IF AVAIL ITEM THEN
      FOR EACH job-hdr
          WHERE job-hdr.company EQ po-ordl.company
            AND job-hdr.job-no  EQ po-ordl.job-no
            AND job-hdr.job-no2 EQ po-ordl.job-no2
          NO-LOCK
          BY job-hdr.frm DESC:
        fi_cust-part = job-hdr.i-no.
        IF job-hdr.frm EQ po-ordl.s-num THEN LEAVE.
      END.
    END.
  END.


END PROCEDURE.

PROCEDURE display-msf :
  
DEFINE VARIABLE   v-basis-w AS DEC NO-UNDO. /* for po/po-adder2.p */
DEFINE VARIABLE   v-len LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE   v-wid LIKE po-ordl.s-wid NO-UNDO.
DEFINE VARIABLE   v-dep LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE   factor# as decimal no-undo.
DEFINE VARIABLE   v-default-gl-log as log no-undo.
DEFINE VARIABLE   v-default-gl-cha as cha no-undo.
DEFINE VARIABLE  v-po-qty as log initial true no-undo.
DEFINE VARIABLE  v-po-msf like sys-ctrl.int-fld no-undo.

DEFINE VARIABLE  lv-copy-from-po-num AS INT NO-UNDO.

  DEFINE VARIABLE  v-out-qty AS DEC NO-UNDO.
  DEFINE VARIABLE  lv-cons-qty AS DEC NO-UNDO.
  DEFINE VARIABLE  lv-cons-cost AS DEC NO-UNDO.
  DEFINE VARIABLE  lv-tot-msf LIKE v-tot-msf NO-UNDO.
  DEFINE VARIABLE  v-ord-qty AS dec NO-UNDO.
  DEFINE VARIABLE  lv-uom-list as cha init "C,CS,EA,L,LB,LF,LOT,M,MSF,SHT,TON,BF" no-undo.
  DEFINE VARIABLE  pr-uom-list AS cha NO-UNDO INIT "EA,LB,M,MSF,TON,BF".
  DEFINE VARIABLE  cons-uom-list AS CHA NO-UNDO INIT "M,LF,EA,LB,TON".
 

   /* {ce/msfcalc.i}*/

def var v-corr as log.
IF AVAIL po-ordl THEN DO:
find first sys-ctrl
    where sys-ctrl.company eq po-ordl.company
      and sys-ctrl.name    eq "MSFCALC"
    no-lock no-error.

ASSIGN
v-corr = avail sys-ctrl and sys-ctrl.char-fld eq "Corrware".

  ASSIGN 
      v-tot-msf = 0 .

  FOR EACH po-ordl WHERE
      po-ordl.company EQ po-ord.company AND
      po-ordl.po-no   EQ po-ord.po-no NO-LOCK:
  

  FIND FIRST ITEM WHERE ITEM.company = po-ordl.company AND ITEM.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
  ASSIGN
  v-basis-w = IF AVAIL ITEM THEN ITEM.basis-w ELSE v-basis-w
  v-dep = IF AVAIL ITEM THEN ITEM.s-dep ELSE v-dep

   v-len = po-ordl.s-len
   v-wid = po-ordl.s-wid
   v-ord-qty = po-ordl.ord-qty
   {po/calc10.i v-len}
   {po/calc10.i v-wid}.

  IF po-ordl.pr-qty-uom = "EA" THEN
     lv-tot-msf = IF v-corr THEN ((v-len * v-wid * .007 * po-ordl.ord-qty) / 1000)
                           ELSE ((((v-len * v-wid) / 144) * po-ordl.ord-qty) / 1000).
  else do:
                  /*convert whatever the UOM is into "EACH" first*/
      
                    lv-tot-msf = 0.
                  if po-ordl.pr-qty-uom ne "EA" then do:
                        lv-tot-msf = 0.
                        run sys/ref/convquom.p(po-ordl.pr-qty-uom,
                                               "EA",
                                               v-basis-w,
                                               v-len,
                                               v-wid,
                                               v-dep,
                                               v-ord-qty,
                                               output v-out-qty).

                        /*now convert from "EACH" into MSF*/   
                        lv-tot-msf = if v-corr
                          then
                            ((v-len * v-wid * .007 * v-out-qty) / 1000)
                          else
                            ((((v-len * v-wid) / 144) * v-out-qty) / 1000).
                       IF po-ordl.pr-qty-uom EQ "ROLL" THEN
                         lv-tot-msf = lv-tot-msf * (12 / v-len).
                  end. 
  end.
      v-tot-msf = v-tot-msf + round(lv-tot-msf,3).
  END. /* each po-ordl */
END.  /*if avail po-ordl*/

END PROCEDURE.
  
/*****************/
FUNCTION get-acct-dscr RETURNS CHARACTER :
 
  
  find first account
      where account.company eq ap-invl.company
        and account.actnum  eq ap-invl.actnum
      no-lock no-error.
  IF AVAIL account THEN RETURN account.dscr. ELSE RETURN "".   /* Function return value. */

END FUNCTION.
