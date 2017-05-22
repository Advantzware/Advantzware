/* --------------------------------------------- po/po-sthpk.p  */
/* Purchase Order XPrint Program for N-K-POPRINT = SOUTHPAK                        */
/* -------------------------------------------------------------------------- */

/* SKB - 2/1/07 Output selection for the report */
DEFINE SHARED VARIABLE LvOutputSelection AS CHAR NO-UNDO.

DEF INPUT PARAMETER ip-multi-faxout AS LOG NO-UNDO. /* fax multiple recipents or single */
DEF INPUT PARAMETER ip-lines-per-page AS INT NO-UNDO.
DEF STREAM st-fax.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer b-ref1  for reftable.
def buffer b-ref2  for reftable.

{po/po-print.i}
{custom/notesdef.i}
{custom/formtext.i NEW}
DEF VAR v-tmp-note-length AS INT NO-UNDO.
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
def var v-wid like po-ordl.s-wid format ">>9.99" no-undo.
def var v-len like po-ordl.s-len format ">>9.99" no-undo.
def var pol-counter as int no-undo.
def var save_id as recid.
def var time_stamp as char.
def var v-exp-limit as int no-undo init 10.
def var v-line-number as int.
def var v-page-counter as int format ">>9".
def var v-lines-to-skip as int.
def var v-sname like shipto.ship-name.
def var v-sid like shipto.ship-id.
def var v-saddr like shipto.ship-addr.
def var v-scity like shipto.ship-city.
def var v-sstate like shipto.ship-state.
def var v-szip like shipto.ship-zip.
def var v-po-type as char format "x(10)".
def var v-freight-dscr as char format "x(7)".
def var v-change-dscr as char format "x(7)".
def var v-dash-line as char format "x(80)" extent 3.
def var v-adders as log.
def var xg-flag as log init no no-undo.
def var v-space as log init yes.
def var len-score as char.
def buffer xjob-mat for job-mat.
def buffer xitem for item.
def var same-score as ch no-undo.
def var v-test-scr as log no-undo.
def var v-hdr as char format "x(15)" initial "" no-undo.
def var v-ino-job as char format "x(15)" initial "" no-undo.
def var v-change-ord as char format "x(35)" initial "" no-undo.

DEF VAR v-inst-lines AS INT NO-UNDO.
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.

/* === with xprint ====*/

DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(60)" NO-UNDO.

ASSIGN ls-image1 = "images\southpak.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR v-basis-w AS DEC NO-UNDO.
DEF VAR v-dep AS DEC NO-UNDO.
DEF VAR v-qty LIKE po-ordl.ord-qty NO-UNDO.
DEF VAR v-tot-sqft AS DEC NO-UNDO.
DEF VAR v-vend-item AS cha NO-UNDO.
def var v-adder AS cha FORM "x(15)" extent 5 no-undo.
def var v-num-add as int initial 0 no-undo.
DEF VAR v-job-no AS cha NO-UNDO.
DEF VAR v-cost AS DEC NO-UNDO.
DEF VAR v-setup AS DEC NO-UNDO.
DEF VAR lv-item-rec AS cha NO-UNDO.

v-dash-line = fill ("_",80).


/* skb 1/2/07 - Variables for excel Automation  */
DEFINE NEW SHARED VARIABLE chExcelApplication   AS COM-HANDLE 						  NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorkBook           AS COM-HANDLE 							NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorksheet          AS COM-HANDLE 							NO-UNDO.
DEFINE NEW SHARED VARIABLE chHyper              AS COM-HANDLE 							NO-UNDO. 
DEFINE VARIABLE v-cell                          AS CHARACTER  							NO-UNDO.
DEFINE VARIABLE t-dwg                           AS CHAR       							NO-UNDO.
DEFINE VARIABLE t-name                          AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE t-fnd                           AS LOGICAL  INIT "False"    NO-UNDO.
DEFINE VARIABLE t-seq                           AS INTEGER  								NO-UNDO.
DEFINE VARIABLE inRowCount                      AS INTEGER    							NO-UNDO    INITIAL 1.
DEFINE VARIABLE chFile AS CHAR NO-UNDO.
DEFINE VARIABLE LvLineCnt AS INT NO-UNDO.
DEFINE VARIABLE CurrDir AS CHAR NO-UNDO.
DEFINE VARIABLE LvCtr as int no-undo.


assign  tmpstore = fill("-",130).

/* Build a Table to keep sequence of pdf files */
DEFINE SHARED TEMP-TABLE tt-filelist
    FIELD tt-FileCtr AS INT
    FIELD tt-FileName AS CHAR
INDEX filelist IS PRIMARY TT-FILECTR.

DEFINE VARIABLE CurActivePrinter AS CHAR NO-UNDO.
DEFINE VARIABLE AdobePrinter     AS CHAR NO-UNDO.
define variable CommandString    AS CHAR NO-UNDO.
define variable WshNetwork as com-handle.
DEFINE VARIABLE LvFirstTimePrint AS LOGICAL INIT NO NO-UNDO.

for each tt-filelist :
  delete tt-filelist.
end.

IF LvOutputSelection = "email" THEN
assign CurActivePrinter = SESSION:PRINTER-NAME
       AdobePrinter     = "PDFcamp Printer".

/* Capture the current active printer */
CREATE "WScript.Network" WshNetwork NO-ERROR.
IF NOT(VALID-HANDLE(WshNetwork)) THEN
DO :
  MESSAGE "Unable to Create Wscript.Network" VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

IF LvOutputSelection = "Email" THEN
DO:
   WshNetwork:SetDefaultPrinter(AdobePrinter). 
END.
CREATE "Excel.Application" chExcelApplication NO-ERROR.

IF NOT(VALID-HANDLE(chExcelApplication)) THEN
DO :
  MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

FILE-INFO:FILE-NAME = "Template\po-sp.xlt".

/* Set the Excel Template to be used. */
ASSIGN chFile = search (FILE-INFO:FULL-PATHNAME) no-error.
  
if search (chFile) = ? then do:
   MESSAGE 'Template File: ' FILE-INFO:FULL-PATHNAME
           'cannot be found. Please verify that the file exists.'
   VIEW-AS ALERT-BOX INFO BUTTONS OK.
   apply 'CLOSE':U to this-procedure.
end.

ASSIGN CurrDir = SUBSTRING (chFile, 1, INDEX (chFile, "Template\po-sp.xlt") - 2)
       no-error.

chExcelApplication:VISIBLE = TRUE.
IF LvOutputSelection = "Email" or LvOutputSelection = "Printer" THEN
chExcelApplication:VISIBLE = FALSE.

/*chExcelApplication:ScreenUpdating = False.*/

/*==============*/
DEF VAR lv-file-name AS cha FORM "x(60)" NO-UNDO.
OS-CREATE-DIR VALUE("c:\temp\fax") NO-ERROR.
/*
IF ip-multi-faxout THEN DO:

  INPUT FROM OS-DIR ("C:\temp\fax") NO-ECHO.
  REPEAT:
      SET lv-file-name.  
      IF lv-file-name <> "." AND lv-file-name <> ".." THEN DO:     
         OS-DELETE VALUE("C:\temp\fax\" + lv-file-name) .       
      END.
  END.
END.
*/
/*==================*/


{po/po-print.f}

assign v-hdr = "VEND ITEM".
       
find first company where company.company eq cocode NO-LOCK. 
if avail company then
assign
 v-comp-add1 = company.addr[1]
 v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
 v-comp-add3 = "Phone: 604.533.2545" 
 v-comp-add4 = "Fax  : 604.533.2633"
 v-tot-sqft = 0.

    print-po-blok:

    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
      FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
      BREAK BY po-ord.vend-no BY PO-ORD.PO-NO:

      IF NOT CAN-FIND(FIRST po-ordl WHERE
         po-ordl.company EQ po-ord.company AND
         po-ordl.po-no EQ po-ord.po-no) THEN NEXT.
      
      find first vend where 
      vend.company eq po-ord.company and 
      vend.vend-no eq po-ord.vend-no no-lock no-error.

      if avail company then
        assign
         v-sid       = company.company
         v-sname     = company.name
         v-saddr [1] = company.addr [1]
         v-saddr [2] = company.addr [2]
         v-scity     = company.city
         v-sstate    = company.state
         v-szip      = company.zip.

      if po-ord.type eq "D" then
        assign v-sid       = po-ord.ship-id 
               v-sname     = po-ord.ship-name
               v-saddr[1]  = po-ord.ship-addr[1]
               v-saddr[2]  = po-ord.ship-addr[2]
               v-scity     = po-ord.ship-city
               v-sstate    = po-ord.ship-state
               v-szip      = po-ord.ship-zip.

      /* Updating PO Printed */
      {po/exportpo.i}

      assign v-page-counter  = 1
             v-change-ord    = "".

      /* Updating PO Stat */
      if po-ord.stat eq "N" then
        assign po-ord.stat = "O".
      else
      if po-ord.stat eq "U" then
        v-change-ord = "(CHANGED ORDER ONLY)".

      find first terms where terms.t-code eq po-ord.terms no-lock no-error.
      find first carrier where carrier.company eq po-ord.company 
                           and carrier.carrier eq po-ord.carrier no-lock no-error.

      if po-ord.type eq "R" then
        assign v-po-type = "Regular".
      else
        assign v-po-type = "Drop Ship".

      if po-ord.frt-pay eq "P" then
        assign v-freight-dscr = "Prepaid".
      else if po-ord.frt-pay eq "C" then
        assign v-freight-dscr = "Collect".
      else
        assign v-freight-dscr = "Bill".


      /********** FORM HEADER SKIP ******************/
      IF FIRST-OF(PO-ORD.PO-NO) THEN
      DO:
        
        ASSIGN chWorkbook = chExcelApplication:Workbooks:Open(chfile)

        /* Vendor Phone */
               v-cell = "R7C1".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = vend.area-code + vend.phone.

        /* Vendor Fax */
        ASSIGN v-cell = "R7C6".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = vend.area-code + vend.fax.

        /* Vendor Email */
        ASSIGN v-cell = "R7C11".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = "".

        /* Vendor Name */
        ASSIGN v-cell = "R8C1".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = vend.name.
        
        /* Vendor Add1 */
        ASSIGN v-cell = "R9C1".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = vend.add1.
        
        /* Vendor Add2 */
        ASSIGN v-cell = "R10C1".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = vend.add2.
        
        /* Vendor City State Zip */
        ASSIGN v-cell = "R11C1".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = vend.city .
        
        /* Vendor City State Zip */
        ASSIGN v-cell = "R11C11".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value =  vend.state .

        /* Vendor City State Zip */
        ASSIGN v-cell = "R11C13".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = vend.zip.

        /* Ship To */
        ASSIGN v-cell = "R7C18".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-sid.

        ASSIGN v-cell = "R8C18".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-sname.
                
        /* Ship Add1 */
        ASSIGN v-cell = "R9C18".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-saddr[1].
        
        /* Ship Add2 */
        ASSIGN v-cell = "R10C18".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-saddr[2].

        /* Ship City State Zip  */
        ASSIGN v-cell = "R11C18".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-scity .
        
        /* Ship City State Zip  */
        ASSIGN v-cell = "R11C28".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-sstate.

        /* Ship City State Zip  */
        ASSIGN v-cell = "R11C30".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-szip.

        /* Our PO# */
        ASSIGN v-cell = "R5C34".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = string(po-ord.po-no).

        /* Date */
        ASSIGN v-cell = "R5C40".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = string(po-ord.po-date).
        
        /* Due date */
        ASSIGN v-cell = "R5C46".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = po-ord.due-date.

        /* PMT terms */
        ASSIGN v-cell = "R8C40".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = terms.dscr.

        /* Vend Contact */
        ASSIGN v-cell = "R8C46".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = po-ord.contact.

        /* Ship Via */
        ASSIGN v-cell = "R11C34".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = carrier.dscr.

        /* Frt. Paid By */
        ASSIGN v-cell = "R11C40".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = "".

        /* FRT terms */
        ASSIGN v-cell = "R11C46".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-freight-dscr
               inrowcount = 14.
      END.
      v-printline = 0.
      
      /*========*/
      for each po-ordl WHERE
          po-ordl.company EQ po-ord.company AND
          po-ordl.po-no EQ po-ord.po-no BREAK by po-ordl.line:
        assign xg-flag = no.
        if not v-printde-po and po-ordl.deleted then next.
        assign v-change-dscr = "".

        if po-ordl.stat eq "A" THEN ASSIGN v-change-dscr = "Added".
        else if po-ordl.stat eq "U" THEN assign v-change-dscr = "Updated".
        else if po-ordl.stat eq "O" THEN assign v-change-dscr = "Open".
        else if po-ordl.stat eq "P" then assign v-change-dscr = "Partial".
        else if po-ordl.stat eq "C" then assign v-change-dscr = "Closed".

        if po-ordl.deleted eq yes then   assign v-change-dscr = "Deleted".

        assign v-ino-job = po-ordl.vend-i-no.
        
        find item where item.company eq po-ordl.company
                    and item.i-no    eq po-ordl.i-no
                    and po-ordl.item-type
                  no-lock no-error.
        v-vend-item = (IF (AVAIL ITEM AND ITEM.vend-no = po-ord.vend) THEN ITEM.vend-item ELSE "")
                        +
                      (IF (AVAIL ITEM AND ITEM.vend2-no = po-ord.vend) THEN (" " + ITEM.vend2-item) ELSE "").
        ASSIGN v-wid = po-ordl.s-wid
               v-len = po-ordl.s-len
               v-vend-item = po-ordl.vend-i-no.

        if avail item and item.mat-type eq "B" then do:
          if v-shtsiz then do:
            assign v-wid = po-ordl.s-wid - truncate(po-ordl.s-wid,0)
                   v-wid = ( v-wid * 16 ) / 100
                   v-wid = truncate(po-ordl.s-wid,0) + v-wid
                   v-len = po-ordl.s-len - truncate(po-ordl.s-len,0)
                   v-len = ( v-len * 16 ) / 100
                   v-len = truncate(po-ordl.s-len,0) + v-len
                   v-num-add = 0.

            find first job where job.company eq cocode 
                             and job.job-no eq string(fill(" ",6 - length(
                                                trim(po-ordl.job-no)))) +
                                                trim(po-ordl.job-no) 
                             and job.job-no2 eq po-ordl.job-no2
                           no-lock no-error.
            if avail job then
            do:
              for each job-mat
                  where job-mat.company  eq cocode
                    and job-mat.job      eq job.job
                    and job-mat.job-no   eq job.job-no
                    and job-mat.job-no2  eq job.job-no2
                    and job-mat.i-no     eq po-ordl.i-no
                    and job-mat.frm      eq po-ordl.s-num
                  use-index job no-lock
                  break by job-mat.blank-no desc:
                if last(job-mat.blank-no)            or
                   job-mat.blank-no eq po-ordl.b-num then leave.
              end.

              if avail job-mat then
              do:
                /* Adder i-no and i-name to po of exist */
                for each xjob-mat where xjob-mat.company  eq cocode
                                    and xjob-mat.job      eq job-mat.job
                                    and xjob-mat.job-no   eq job-mat.job-no
                                    and xjob-mat.job-no2  eq job-mat.job-no2
                                    and xjob-mat.frm      eq job-mat.frm
                                    and xjob-mat.blank-no eq job-mat.blank-no
                                    and xjob-mat.i-no     ne job-mat.i-no
                                  no-lock:
                  find first xitem where xitem.company        eq cocode
                                     and xitem.i-no      eq xjob-mat.i-no
                                     and xitem.mat-type  eq "A" no-lock no-error.
                  if avail xitem then
                  do:
                      assign v-num-add = v-num-add + 1.
                      if      v-num-add eq 1 THEN assign v-adder[1] = xitem.i-name.
                      else if v-num-add eq 2 THEN assign v-adder[2] = xitem.i-name.
                      else if v-num-add eq 3 THEN assign v-adder[3] = xitem.i-name.
                      else if v-num-add eq 4 THEN assign v-adder[4] = xitem.i-name.
                      else if v-num-add eq 5 THEN assign v-adder[5] = xitem.i-name.
                  end.

                end.

                find first ef where EF.COMPANY EQ JOB.COMPANY
                                AND ef.est-no  EQ job.est-no
                                and ef.form-no eq job-mat.frm
                              no-lock no-error.
                if avail ef and (ef.xgrain eq "S" or ef.xgrain eq "B") THEN ASSIGN xg-flag = yes.
              end. /* avail job-mat */
            end. /* avail job */
          end. /* v-shtsiz */        
        end. /* avail item and item.mat-type eq "B" */

        v-job-no = po-ordl.job-no + "-" + STRING(po-ordl.job-no2,"99").
        IF v-job-no = "-00" THEN v-job-no = "".
        
        IF FIRST-OF(po-ordl.line) THEN
        DO:
           FIND FIRST oe-ord WHERE
                oe-ord.company EQ po-ordl.company AND
                oe-ord.ord-no  EQ po-ordl.ord-no
                NO-LOCK NO-ERROR.

           IF AVAIL oe-ord THEN
           DO:
              /* Cust Po# */
              chExcelApplication:Goto("R8C34") NO-ERROR.
              chExcelApplication:ActiveCell:Value = oe-ord.po-no.
              RELEASE oe-ord.
           END.
        END.

        ASSIGN inrowcount = inrowcount + 1.
        
        /* Line# */
        ASSIGN v-cell = "R" + string(inrowcount) + "C1".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = po-ordl.LINE.
        


        /* Item Desc */
        ASSIGN v-cell = "R" + string(inrowcount) + "C2".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = po-ordl.i-name.
        
        {po/po-ordls.i}
        
        /*
        len-score = ''.

        DO i = 1 TO 12:
          IF b-ref1.val [i] > 0 THEN
          len-score = len-score + STRING (b-ref1.val [i]) + ' '.
        END.

        /* Score */
        ASSIGN v-cell = "R" + string(inrowcount) + "C13".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = len-score.

        /* Adder */
        ASSIGN v-cell = "R" + string(inrowcount) + "C18".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-adder[1].

        */  
          
        /* Vendor Item */
        ASSIGN v-cell = "R" + string(inrowcount) + "C24".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = v-vend-item.
        
        /* Our Item */
        ASSIGN v-cell = "R" + string(inrowcount) + "C28".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = po-ordl.i-no.
        
        /* Order Qty */
        ASSIGN v-cell = "R" + string(inrowcount) + "C32".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = po-ordl.ord-qty.
        
        /* Order UM */
        ASSIGN v-cell = "R" + string(inrowcount) + "C41".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = po-ordl.pr-qty-uom.
                
        /* Unit Price */
        ASSIGN v-cell = "R" + string(inrowcount) + "C43".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = po-ordl.cost.

        /* extended Price */
        ASSIGN v-cell = "R" + string(inrowcount) + "C46".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        ASSIGN chExcelApplication:ActiveCell:Value = po-ordl.t-cost.
        
        ASSIGN v-cell = "R" + string(inrowcount) + "C50".
        chExcelApplication:Goto(v-cell) NO-ERROR.
        if po-ordl.tax = true then
        ASSIGN chExcelApplication:ActiveCell:Value = "Y".
        else
        ASSIGN chExcelApplication:ActiveCell:Value = "N".

        v-printline = v-printline + 2.
        assign v-line-number = v-line-number + 3.
        
        if po-ordl.dscr[1] ne "" OR v-adder[3] <> "" then do:
          ASSIGN
             v-line-number = v-line-number + 1
             v-printline = v-printline + 1.
        end.

        if po-ordl.dscr[2] ne "" then do:
          ASSIGN
             v-line-number = v-line-number + 1
             v-printline = v-printline + 1.
        end.
        
        FIND FIRST itemfg WHERE itemfg.company = po-ordl.company
                            AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
        IF AVAIL itemfg AND itemfg.part-dscr3 <> "" THEN DO:
           ASSIGN 
              v-line-number = v-line-number + 1
              v-printline = v-printline + 1.
        END.
        
        IF v-vend-item <> "" THEN DO:
           ASSIGN
              v-line-number = v-line-number + 1
              v-printline = v-printline + 1.
        END.

        v-setup = po-ordl.setup.

        IF po-ordl.item-type THEN DO:
           assign v-line-number = v-line-number + 1
                  v-printline = v-printline + 1.
        END.
        
        run po/po-ordls.p (recid(po-ordl)).
            
        {po/poprints.i}
            
            if not v-test-scr then do:
                      
                  v-line-number = v-line-number + 1.
                  v-printline = v-printline + 1.
            end.
            ELSE if dec(trim(len-score)) ne v-wid then do:
                 ASSIGN 
                    v-line-number = v-line-number + 1
                    v-printline = v-printline + 1.
            end.
          end.  /* if v-lscore-c ne "" from poprints.i */
          END.
        end.  /* avail reftable from poprints.i*/
   
        assign v-line-number = v-line-number + 1
               v-printline = v-printline + 1.

      /* calc total sq feet */
    
        ASSIGN v-basis-w = 0
               v-dep     = 0.

    IF po-ordl.item-type THEN
    FIND FIRST ITEM
        WHERE ITEM.company EQ po-ord.company
          AND ITEM.i-no    EQ po-ordl.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN
      ASSIGN
       v-basis-w = item.basis-w
       v-dep     = item.s-dep.

    IF po-ordl.pr-qty-uom EQ "MSF" THEN
      v-qty = po-ordl.ord-qty.
    ELSE
      RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
                             v-basis-w, po-ordl.s-len, po-ordl.s-wid, v-dep,
                             po-ordl.ord-qty, OUTPUT v-qty).

     v-tot-sqft = v-tot-sqft + (v-qty * 1000).
    
     FOR EACH tt-formtext:
         DELETE tt-formtext.
     END.
     lv-text = "".
     FOR EACH notes WHERE notes.rec_key = po-ordl.rec_key NO-LOCK:
             lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
     END.
     IF lv-text <> "" THEN
     DO li = 1 TO 4:
            CREATE tt-formtext.
            ASSIGN
             tt-line-no = li
             tt-length  = 78.
     END.
     RUN custom/formtext.p (lv-text).
     i = 0.
     FOR EACH tt-formtext:
        i = i + 1.
        IF  i <= 4 THEN do:
            v-printline = v-printline + 1.
        END.
     END.
    
     /* === spec note print */
     IF v-print-sn THEN DO:
        ASSIGN v-tmp-lines = 0
               v-inst-lines = 0
               lv-item-rec = "".

        lv-item-rec = IF po-ordl.item-type AND AVAIL ITEM THEN ITEM.rec_key
                      ELSE IF AVAIL itemfg THEN itemfg.rec_key
                      ELSE "".

        IF lv-item-rec <> "" THEN DO:
           FOR EACH notes WHERE notes.rec_key = lv-item-rec AND 
               notes.note_code = "PO" NO-LOCK:
              IF notes.note_text <> "" THEN DO:
                 v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                {SYS/INC/ROUNDUP.I v-tmp-lines}
                v-inst-lines = v-inst-lines + v-tmp-lines. 
              END.
           END.
           if v-inst-lines gt 0 then v-inst-lines = v-inst-lines + 1.
           v-printline = v-printline + v-inst-lines + 1.
           IF v-printline > 46 AND v-inst-lines > 0 THEN DO:         
              PAGE.
              v-printline = 0.
           END.     
    
           FOR EACH notes WHERE notes.rec_key = lv-item-rec AND
               notes.note_code = "PO" NO-LOCK:
               v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
               {SYS/INC/ROUNDUP.I v-tmp-lines}
               IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
                  v-printline = v-printline + 1.
               END.
           end.
        END. /* lv-item-spec <> "" */
     END.
     /* === end of specnote print */

  end. /* for each po-ordl record */
  
  v-inst = "".
  FOR EACH notes WHERE notes.rec_key = po-ord.rec_key NO-LOCK:
                v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                {SYS/INC/ROUNDUP.I v-tmp-lines}
                IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
                   IF i < 5  THEN  /* display upto 4 lines */
                       ASSIGN v-inst[i] =  substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80)
                              v-printline = v-printline + 1.
                   ELSE LEAVE.
                END.

   end.
  
  IF LAST-OF(PO-ORD.PO-NO) THEN
  DO :
    /* Total */
    ASSIGN v-cell = "R332C45".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN chExcelApplication:ActiveCell:Value = po-ord.t-cost.
    
    
    RUN util/GetBottomRow.p (INPUT 14, INPUT 30, INPUT 3, INPUT inrowcount, OUTPUT inrowcount).
    ASSIGN v-cell = STRING(inrowcount) + ":331". 
    chExcelApplication:Rows(v-cell):SELECT.
    chExcelApplication:SELECTION:DELETE.
    
    ASSIGN v-cell = "R1C1".
    chExcelApplication:Goto(v-cell) NO-ERROR.

    os-delete value("c:\tmp\" + STRING(PO-ORD.PO-NO) + ".xls").     
    os-delete value("c:\tmp\asi.pdf").
    os-delete value("c:\tmp\" + STRING(PO-ORD.PO-NO) + ".pdf").

    IF LvOutputSelection = "PRINTER" THEN
    DO:
       IF LvFirstTimePrint = NO THEN
       DO :
         chExcelApplication:Dialogs(8):Show.
         chWorkbook:Close(no) no-error.
         ASSIGN LvFirstTimePrint = YES.
       END.
       ELSE DO :
         chExcelApplication:ActiveWindow:SelectedSheets:PrintOut().
         chWorkbook:Close(no) no-error. 
       END.
    END.

    IF LvOutputSelection = "Email" THEN
   	DO:
      chExcelApplication:ActiveSheet:SaveAs("c:\tmp\" + STRING(PO-ORD.PO-NO) + ".xls") no-error. 	   

      chExcelApplication:ActiveWindow:SelectedSheets:PrintOut().
   	  chWorkbook:Close(no) no-error.   
       		
   	  pause 3.
      		
   	  OS-DELETE VALUE("c:\tmp\" + STRING(PO-ORD.PO-NO) + ".xls").
       		
      OS-RENAME c:\tmp\asi.pdf VALUE("c:\tmp\" + STRING(PO-ORD.PO-NO) + ".pdf").
       		
      ASSIGN LvCtr = LvCtr + 1.
      CREATE tt-filelist.
      ASSIGN tt-FileCtr  = LvCtr
          	tt-FileName = "c:\tmp\" + STRING(PO-ORD.PO-NO) + ".pdf"
       		.
    END.
    ELSE IF LvOutputSelection = "Screen" THEN
    DO:
      chExcelApplication:ActiveSheet:Protect("advance4me").
    END.
    
    /* RELEASE OBJECTS */
    RELEASE OBJECT chWorkbook NO-ERROR.
    RELEASE OBJECT chWorkSheet NO-ERROR.
    RELEASE OBJECT chHyper NO-ERROR.
    
      
  END.
  
end. /* for each po-ord record */.

/* Clean up and extra processing */
/* Merge all the PDF Files */
ASSIGN CommandString = CurrDir + "\util\pdftk ".

os-delete c:\tmp\PO.pdf.

FOR EACH tt-filelist :
  assign CommandString = CommandString + " " + tt-FileName .
END.

assign CommandString = CommandString + " cat output c:\tmp\po.pdf".
os-command silent value(CommandString).

WshNetwork:SetDefaultPrinter(CurActivePrinter).

IF LvOutputSelection = "PRINTER" OR LvOutputSelection = "EMAIL" THEN
DO:
  chExcelApplication:Quit() no-error.
END.

/*chExcelApplication:ScreenUpdating = TRUE.*/

RELEASE OBJECT WshNetwork NO-ERROR.
RELEASE OBJECT chExcelApplication NO-ERROR.

/* END ---------------------------- Copr. 1992 - 1994  Advanced Software Inc. */
