/* ------------------------------------------------------------ po/po-fibx.p  */
/*                                                                            */
/* Purchase Order Print Program - P/O Module - Fibre  Xprint                  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer xjob-mat for job-mat.
def buffer xitem for item.
def buffer b-ref1 for reftable.
def buffer b-ref2 for reftable.
    DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
    DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
    DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
    DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
    DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-email AS cha FORM "x(40)" NO-UNDO.
{po/po-print.i}

def var v-wid like po-ordl.s-wid format ">>9.99" no-undo.
def var v-len like po-ordl.s-len format ">>9.99" no-undo.
def var save_id as recid NO-UNDO.
DEF VAR v-text AS CHAR NO-UNDO.
def var time_stamp as char NO-UNDO.
def var v-print-lines as int NO-UNDO.
def var v-inst-lines as int NO-UNDO.
def var v-sname like shipto.ship-name NO-UNDO.
def var v-saddr like shipto.ship-addr NO-UNDO.
def var v-scity like shipto.ship-city NO-UNDO.
def var v-sstate like shipto.ship-state NO-UNDO.
def var v-szip like shipto.ship-zip NO-UNDO.
def var v-job as char format "x(12)" NO-UNDO.
def var v-po-tot like po-ord.t-cost extent 2 NO-UNDO.
def var v-t-freight like po-ord.t-freight extent 2 NO-UNDO.
def var v-sqft as dec NO-UNDO.
def var v-tot-sqft as dec extent 2 NO-UNDO.
def var v-ratio as dec NO-UNDO.
def var xg-flag as log init no no-undo.
def var same-score as char no-undo.
def var v-test-scr as log no-undo.
def var v-bottom as int init 0 no-undo.
def var v-change-dscr as char format "x(7)" no-undo.
def var v-change-ord as char format "x(35)" no-undo.
def var v-contact like po-ord.contact NO-UNDO.
def var v-mach as char extent 4 no-undo.
DEF VAR v-basis-w AS DEC NO-UNDO.
DEF VAR v-dep AS dec FORM ">>9.99<<" NO-UNDO.
DEF VAR v-dep2 LIKE v-dep NO-UNDO.
DEF VAR v-qty LIKE po-ordl.ord-qty NO-UNDO.
DEF VAR v-cost AS DEC NO-UNDO.
DEF VAR v-count AS INT no-undo.

def var len-score as char NO-UNDO.
def var v-space as log init yes NO-UNDO. 
def var v-adder like item.i-no extent 6 no-undo.
def var v-num-add as int init 0 no-undo.
def var v-counter as int init 0 no-undo.
def var v-tax like po-ordl.t-cost extent 3 NO-UNDO.
def var v-ord-qty as char NO-UNDO.
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 9 NO-UNDO.
DEF VAR v-tmp-lines AS dec NO-UNDO.
DEF VAR lv-got-return AS int NO-UNDO.
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR fg-uom-list AS CHAR NO-UNDO.
DEF VAR v-line-cnt AS INT NO-UNDO.
DEF VAR v-signature AS cha NO-UNDO.
DEF VAR v-sig-image AS cha NO-UNDO.
DEF VAR v-username AS cha NO-UNDO.
DEF VAR li-page AS INT NO-UNDO.

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
ASSIGN
   ls-image1 = "images\fibrelog.bmp"
   FILE-INFO:FILE-NAME = ls-image1
   ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-overrun AS cha NO-UNDO.
DEF VAR aa AS INT NO-UNDO.
DEF VAR v-setup AS CHAR NO-UNDO.
DEF VAR v-wid-frac AS CHAR NO-UNDO.
DEF VAR v-len-frac AS CHAR NO-UNDO.

{custom/formtext.i NEW}

FUNCTION FNformat RETURNS char (INPUT v-text AS CHAR, v-len AS INT):

  DEF VAR vreturn AS CHAR NO-UNDO.
  DEF VAR ventry  AS CHAR NO-UNDO.
  ASSIGN v-text = REPLACE(v-text,CHR(10),"`")
         v-text = REPLACE(v-text,CHR(13)," ").
                   
  DO i = 1 TO NUM-ENTRIES(v-text,"`"):
     ASSIGN ventry = ENTRY(i,v-text,"`").

     DO WHILE TRUE:
     
        IF LENGTH(ventry) < v-len THEN DO:
           ASSIGN vreturn = vreturn + 
                            (IF vreturn <> "" THEN "`" ELSE "") + 
                            ventry.
           LEAVE.
        END.   
                        
        ASSIGN vreturn = vreturn + 
                         (IF vreturn <> "" THEN "`" ELSE "") + 
                         SUBSTRING(ventry,1,v-len)
               ventry = SUBSTRING(ventry,v-len + 1).

     END. /* DO WHILE TRUE: */
     
  END. /* DO i = 1 TO NUM-ENTRIES(v-text,"`"): */

  RETURN vreturn.
 
END FUNCTION.

DEF VAR vsTmp-v-sqft AS CHAR NO-UNDO.
DEF VAR vsTmpLine AS CHAR FORMAT "X(11)" NO-UNDO.
DEF VAR v-line-2 AS CHAR FORMAT "X(30)" NO-UNDO.

FORM "Overrun/Underrun:" AT 8 v-overrun FORM "x(13)"
     v-adder[3]             at 39   format "x(9)"
     v-setup                AT 49   FORMAT "X(16)"
     v-tax[1]               to 80   format "->>>,>>9.99"
    WITH FRAME po-line-2 stream-io width 80 no-box no-labels NO-UNDERLINE DOWN.

form header
     skip(5)
     "** Continued **"      to 80
     skip(1)
    with frame po-cont page-bottom no-box no-labels NO-UNDERLINE STREAM-IO.

def stream last-page.
DEF VAR v-image AS cha FORM "x(200)" NO-UNDO.
v-image = "<C1><#1><R+7><C+35><IMAGE#1=" + ls-full-img1 + "<=1>" .

output stream last-page to value("po-fibx.txt") page-size VALUE(v-lines-per-page).
PAGE STREAM last-page.

{ce/msfcalc.i}
ASSIGN v-comp-add1 = ""
        v-comp-add2 = "" 
        v-comp-add3 = ""
        v-comp-add4 = ""
        v-comp-add5 = "".
find first po-ctrl where po-ctrl.company eq cocode no-lock.
find first company where company.company eq cocode no-lock.

FIND FIRST cust WHERE cust.company = cocode AND
                      cust.active = "X" NO-LOCK NO-ERROR.
IF AVAIL cust THEN
   ASSIGN v-comp-add1 = cust.addr[1]
          v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
          lv-email    = "Email:  " + cust.email
          v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
          v-comp-add5 = "Fax  :  " + string(cust.fax,"(999)999-9999").

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

print-po-blok:
FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
    break by po-ord.po-no:  
  
        FIND FIRST b-ref1 WHERE
             b-ref1.reftable EQ "users.phone-no" AND
             b-ref1.company EQ po-ord.buyer
             NO-LOCK NO-ERROR.
       
        IF AVAIL b-ref1 THEN
        DO:
           IF b-ref1.CODE NE "" THEN
              v-comp-add4 = "Phone:  " + string(b-ref1.CODE,"(999)999-9999").
           ELSE IF AVAIL cust THEN
              v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999").
       
           RELEASE b-ref1.
        END.
       
        FIND FIRST b-ref1 WHERE
             b-ref1.reftable EQ "users.fax-no" AND
             b-ref1.company EQ po-ord.buyer
             NO-LOCK NO-ERROR.
       
        IF AVAIL b-ref1 THEN
        DO:
           IF b-ref1.CODE NE "" THEN
              v-comp-add5 = "Fax  :  " + string(b-ref1.CODE,"(999)999-9999").
           ELSE IF AVAIL cust THEN
              v-comp-add5 = "Fax  :  " + string(cust.fax,"(999)999-9999").
       
           RELEASE b-ref1.
        END.

        FIND FIRST users WHERE
             users.user_id EQ po-ord.buyer
             NO-LOCK NO-ERROR.

        IF AVAIL users AND users.image_filename <> "" THEN
           lv-email = "Email:  " + users.image_filename.
    

  assign
   v-contact      = po-ord.contact
   v-change-ord   = "".
   
  {po/exportpo.i}
   
  if po-ord.stat eq "N" then po-ord.stat = "O".

  assign
   v-sname    = company.name
   v-saddr[1] = company.addr[1]
   v-saddr[2] = company.addr[2]
   v-scity    = company.city
   v-sstate   = company.state
   v-szip     = company.zip.
 
  if po-ord.type eq "D" then
    assign
     v-sname    = po-ord.ship-name
     v-saddr[1] = po-ord.ship-addr[1]
     v-saddr[2] = po-ord.ship-addr[2]
     v-scity    = po-ord.ship-city
     v-sstate   = po-ord.ship-state
     v-szip     = po-ord.ship-zip.
 
  find first vend
      where vend.company eq po-ord.company
        and vend.vend-no eq po-ord.vend-no
      no-lock.

  find first terms
      where terms.company eq po-ord.company
        and terms.t-code  eq po-ord.terms
      no-lock no-error.        

  aa = 1.
  {po/po-fibx2.i "stream last-page" "(last-page)"}    

  ASSIGN
     v-page-tot = PAGE-NUMBER(last-page) - v-last-page
     aa = 2.

  {po/po-fibx2.i}
   
  v-last-page = PAGE-NUMBER.
END.

OUTPUT STREAM last-page CLOSE.

RETURN.

PROCEDURE PR-HEADER:
  DEF INPUT PARAM ip-first AS LOG NO-UNDO.

  IF aa EQ 1 THEN DO:     
    IF NOT ip-first THEN PAGE STREAM last-page.
    li-page = PAGE-NUMBER (last-page).
    PUT STREAM last-page 
        SKIP(1)
        {po/po-fibx3.i}
  END.

  ELSE DO:  
    IF NOT ip-first THEN PAGE.
    li-page = PAGE-NUMBER.
    PUT SKIP(1)
        {po/po-fibx3.i}               
  END.
END.

/* end ----------------------------------- Copr. 2000  Advanced Software Inc. */
