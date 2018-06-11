/* ----------------------------------------------  */
/*  cecrep/jobmich.p  Corrugated factory ticket  for Michcor */
/* -------------------------------------------------------------------------- */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

define input parameter v-format as character.
DEFINE VARIABLE prt-copies       AS INTEGER NO-UNDO.
DEFINE VARIABLE v-start-compress AS cha     NO-UNDO.
DEFINE VARIABLE v-end-compress   AS cha     NO-UNDO.
DEFINE VARIABLE k_frac           AS DECIMAL INIT 6.25 NO-UNDO.
DEFINE VARIABLE v-ink-1          AS cha     FORM "X(30)" NO-UNDO.
DEFINE VARIABLE v-ink-2          AS cha     FORM "X(30)" NO-UNDO.
DEFINE VARIABLE v-ink-3          AS cha     FORM "X(30)" NO-UNDO.
DEFINE VARIABLE v-ink-4          AS cha     FORM "X(30)" NO-UNDO.
DEFINE variable v-dept-note      AS cha     FORM "x(52)" EXTENT 6 NO-UNDO.
DEFINE variable v-spec-note      AS cha     FORM "x(124)" EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-deptnote       AS cha     NO-UNDO.
DEFINE VARIABLE v-dept-length    AS DECIMAL NO-UNDO.
DEFINE VARIABLE lv-under-run     AS cha     NO-UNDO.
DEFINE VARIABLE lv-over-run      AS cha     NO-UNDO.
DEFINE VARIABLE lv-part-name     AS cha     FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-fg-name       AS cha     NO-UNDO.

{jcrep/r-ticket.i "shared"}

{cecrep/jobmich.i "new shared"}

{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}

define new shared variable v-out1-id      as recid     no-undo.  /* YSK 06/08/01  was~ local var */
define new shared variable v-out2-id      as recid     no-undo.  /* YSK 06/08/01  was~ local var */
 
define            variable laser          as log       init no format "Y/N" no-undo.
define            variable v-vend-no      like oe-ordl.vend-no no-undo.
define            variable v-po-no        like oe-ordl.po-no-po no-undo.
define            variable v-qty-or-sup   as character format "x(38)" no-undo.
define            variable v-i-line       as character extent 4 format "x(38)" no-undo.
define            variable v-flag         as log       init no no-undo.
define            variable v-local-copies as integer   no-undo.
define            variable v-local-loop   as integer   init 1 no-undo.
define            variable v-print-score  as log       init yes no-undo.
define            variable v-pqty         as decimal   no-undo.
DEFINE            VARIABLE lv-part-no     AS cha       FORM "x(15)" NO-UNDO.
DEFINE            VARIABLE lv-rt-num      AS INTEGER   NO-UNDO.
define stream ctl.
DEFINE VARIABLE lv-add-entry  AS INTEGER NO-UNDO.
DEFINE VARIABLE v-loop-cnt    AS INTEGER NO-UNDO.
DEFINE VARIABLE v-note-cnt    AS INTEGER NO-UNDO.
DEFINE VARIABLE v-note-length AS INTEGER NO-UNDO.
DEFINE VARIABLE v-die-loc     AS cha     FORM "x(15)" NO-UNDO.
{custom/notesdef.i}
{cecrep/jc-prem.i}
DEFINE BUFFER b-ef FOR ef.
DEFINE workfile tt-wm LIKE w-m.
DEFINE VARIABLE v-xg-flag    AS LOG NO-UNDO.
DEFINE VARIABLE v-tmp-stype  AS cha NO-UNDO.
DEFINE VARIABLE v-len-score2 AS cha EXTENT 13 NO-UNDO.
DEFINE VARIABLE v-tmp-score  AS cha NO-UNDO.
DEFINE BUFFER bf-eb FOR eb.
DEFINE        VARIABLE lv-spec-qty      LIKE ef.spec-qty FORMAT ">>>,>>9.9<<<<" NO-UNDO.
DEFINE SHARED VARIABLE s-prt-set-header AS LOG NO-UNDO.
DEFINE        VARIABLE v-dept-inst      AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
DEFINE        VARIABLE v-inst2          AS cha EXTENT 6 NO-UNDO.    
DEFINE BUFFER b-eb FOR eb.
DEFINE VARIABLE tb_app-unprinted AS LOG NO-UNDO.

DO TRANSACTION:
    {sys/inc/tspostfg.i}
END.

assign
    v-line[1]      = chr(95) + fill(chr(95),40) + chr(95) + "  " +
             chr(95) + fill(chr(95),40) + chr(95) + "  " +
             chr(95) + fill(chr(95),40) + chr(95)  
    v-line[2]      = v-line[1]
    v-line[3]      = chr(95) + fill(chr(95),128) + chr(95)
    v-line[4]      = v-line[3]
    v-line[5]      = chr(95) + fill(chr(95),84) + chr(95) + "  " +
                chr(95) + fill(chr(95),40) + chr(95)
    v-line[6]      = v-line[5]
    v-line[7]      = chr(95) + fill(chr(95),25) + chr(95) + "  " +
             chr(95) + fill(chr(95),99) + chr(95)
    v-line[8]      = v-line[7]
    v-qty-or-sup   = if lookup(v-format,"TriState,RFC,Boxtech,Brick,Corrugat") gt 0
                then ("Supplier:"     + fill("_",28))
                else ("Qty Received: " + fill("_",24))

    v-local-copies = 1
    prt-copies     = 1.

do v-local-loop = 1 to v-local-copies:
    {cecrep/jobprem.i}
      break by job.job-no BY job.job-no2 BY job-hdr.frm:

v-break = first-of(job.job-no2).

release xest.
release xef.
release xeb.
release xoe-ord.
release xoe-ordl.
release xoe-rel.

run cecrep/jobtick1.p (recid(job-hdr), v-format,
    v-local-loop, v-local-copies).

for each w-ef WHERE (w-ef.frm = job-hdr.frm OR est.est-type <> 8),
    EACH b-eb NO-LOCK WHERE b-eb.company = job-hdr.company
    AND b-eb.est-no = job-hdr.est-no 
    AND b-eb.form-no = w-ef.frm
    AND (b-eb.blank-no = job-hdr.blank-no OR est.est-type NE 8)
    BREAK BY w-ef.frm BY b-eb.blank-no:
    release xef.
    release xeb.
    release xstyle.
    release xxprep.
        
    run cecrep/jobtick2.p (recid(w-ef), recid(job-hdr)).

    v-pqty = 1.
    v-cp = "".
    if available xeb then 
    do:
        if xeb.stock-no ne "" then v-fg = xeb.stock-no.
        v-cp = xeb.part-no.
          
        ASSIGN 
            lv-fg-name = itemfg.i-name.

        {cec/rollfac.i}
        v-pqty = if v-rollfac OR xeb.est-type EQ 8 then 1 else
            if xeb.quantityPerSet lt 0 then (-1 / xeb.quantityPerSet)
            else xeb.quantityPerSet.
    end.
        
    assign
        v-loc     = ""
        v-loc-bin = "".
         
    if v-format eq "Brick" or v-format eq "Corrugat" then 
    do: 
        v-iso = "ISO# CS-05-1-F".

        release fg-rdtlh.
          
        find first fg-bin
            where fg-bin.company   eq cocode
            and fg-bin.i-no      eq job-hdr.i-no
            and fg-bin.job-no    eq job-hdr.job-no
            and fg-bin.job-no2   eq job-hdr.job-no2
            and fg-bin.loc       eq job-hdr.loc
            and fg-bin.qty       ne 0
            no-lock no-error.
        if available fg-bin then 
        do:
            assign
                v-loc     = "Whs: " + fg-bin.loc
                v-loc-bin = "Bin: " + fg-bin.loc-bin.
        end.
        else
            if available itemfg then 
            do:                             
                assign
                    v-loc     = "Whs: " + itemfg.def-loc
                    v-loc-bin = "Bin: " + itemfg.def-loc-bin.
            end.

    end. /*brick format*/

    ASSIGN 
        lv-over-run  = IF AVAILABLE xoe-ordl THEN trim(string(xoe-ordl.over-pct,">>9.99%")) ELSE
                             IF AVAILABLE xoe-ord  THEN trim(string(xoe-ord.over-pct,">>9.99%"))  ELSE ""
        lv-under-run = IF AVAILABLE xoe-ordl THEN trim(string(xoe-ordl.under-pct,">>9.99%")) ELSE
                              IF AVAILABLE xoe-ord  THEN trim(string(xoe-ord.under-pct,">>9.99%"))  ELSE ""
        lv-part-name = xeb.part-dscr1    .

    PUT    "</PROGRESS>"
        "<P12><B>JOB TICKET" AT 9
        "Production Specification </B><P10>" AT 50  " Part item name:" lv-part-name SKIP
        "<#1><C1><FROM><C105><R+45><RECT><|3>" 
        "<=1><C32><FROM><R+10><C32><LINE><|3>"
        "<=1><C66><FROM><R+10><C66><LINE><|3>"
        "<=1><R+10><C44><FROM><R+35><C44><LINE><|3>"
        "<=1><R+10><C76><FROM><R+18><C76><LINE><|3>"
        "<=1><R+4><C1><FROM><C105><LINE><|3>"
        "<=1><C90><FROM><R+4><C90><LINE><|3>"
        "<=#1><R+10><C1><FROM><C105><LINE><|3>"
        "<=1><R+28><C44><FROM><C105><LINE><|3>"
        "<=1><R+17><C76><FROM><C105><LINE><|3>"
        .

    view frame head.  /* factory header display  */  

    if v-format eq "RFC" or v-format eq "Boxtech" then
        assign
            v-i-line[1] = itemfg.i-name
            v-i-line[2] = itemfg.part-dscr1
            v-i-line[3] = itemfg.part-dscr2
            v-i-line[4] = itemfg.part-dscr3.
    else
        assign
            v-i-line[1] = "ITEM DESCRIPTION"
            v-i-line[2] = "Style: " + if available xstyle then xstyle.dscr else ""
            v-i-line[3] = "Size: "  + if available xeb    then
                     trim(string({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                     trim(string({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                     trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99")) else ""
            v-i-line[4] = "Joint: " + if available xeb then v-joint-dscr else "".
        
    lv-part-no = IF AVAILABLE xoe-ordl THEN xoe-ordl.part-no 
    ELSE itemfg.part-no.
                           
    RUN sys/ref/getpo#.p (IF AVAILABLE xoe-ordl AND est.est-type NE 6 THEN ROWID(xoe-ordl) ELSE ROWID(job),
        w-ef.frm, OUTPUT v-po-no).

    RELEASE po-ord.
          
    IF v-po-no NE 0 THEN
        FIND FIRST po-ord
            WHERE po-ord.company EQ cocode
            AND po-ord.po-no   EQ v-po-no
            NO-LOCK NO-ERROR.
          
    ASSIGN
        v-vend-no    = IF AVAILABLE po-ord THEN po-ord.vend-no ELSE ""
        v-qty-or-sup = "Supplier: ".

    if v-vend-no ne "" then 
    do:
        v-qty-or-sup = v-qty-or-sup + trim(v-vend-no).
        IF v-po-no ne 0 THEN v-qty-or-sup = v-qty-or-sup + " PO#:" +
                trim(string(v-po-no,">>>>>>>>>>")).
    end.
        
    i = 0.
    for each w-i:
        i = i + 1.
    end.
    if i lt 4 then 
    do i = i + 1 to 4:
        create w-i.
    end.

    find first w-i.
    v-ink-1 =  w-i.i-dscr +
        (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
        IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
    find next w-i.
    v-ink-2 =  w-i.i-dscr +
        (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
        IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
    find next w-i.
    v-ink-3 =  w-i.i-dscr +
        (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
        IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
    find NEXT w-i.
    v-ink-4 =  w-i.i-dscr +
        (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
        IF w-i.i-dscr <> "" THEN "LBS" ELSE "".

    v-form-sqft = round(if v-corr then (v-form-len * v-form-wid * .007)
    else (v-form-len * v-form-wid / 144),3).
    find first xxprep where xxprep.company eq cocode
        and xxprep.code eq xeb.die-no
        no-lock no-error.
    v-die-loc = IF AVAILABLE xxprep THEN xxprep.loc + " " + xxprep.loc-bin ELSE "".

    DISPLAY  v-cus[1] AT 2 "PO #:" AT 40 xoe-ord.po-no 
        WHEN AVAILABLE xoe-ord 
        "Set Qty:" trim(string(if available xoe-ordl then xoe-ordl.qty
        else job-hdr.qty,">>>,>>9"))
        when available xeb and xeb.est-type eq 6    format "x(9)"
        "Style:" AT 80 xstyle.dscr 
        WHEN AVAILABLE xstyle              
        SKIP
        v-cus[2] AT 2  
        "Job Qty:" AT 40 trim(string(job-hdr.qty * v-pqty,">>>,>>9"))    format "x(7)"
        "Order Qty:" trim(string((if available xoe-ordl then xoe-ordl.qty
        else job-hdr.qty) *
        if est.form-qty le 1 then 1
        else v-pqty,">>>,>>9"))
        format "x(7)"
        "   "
        "Size:" (trim(string({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
        trim(string({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
        trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99"))) FORM "x(30)" 
        WHEN AVAILABLE xeb            
        SKIP
        v-cus[3] AT 2 "Cust Part#:" AT 40 lv-part-no
        "Joint:" AT 80 v-joint-dscr             
        SKIP
        v-cus[4] AT 2
             
        "Item Name: " AT 40 lv-fg-name FORM "x(26)"
        "Adders:" AT 80 v-adders FORM "x(23)"
        "<P7>" SKIP(1)
        "<c2>Machine Routing" "<c45>Printing:" /*"Die Cutting, Slit, & Saw"*/"<c77>Board:" "<P8>" SKIP
        "<c45>PLATE #:" xeb.plate-no 
        when available xeb "<c77>Shts Req'd:" trim(string(v-sht-qty))   format "x(9)"
        "<c93> Sq Ft:" trim(string(v-form-sqft)) format "x(7)" SKIP
        "<c45>Ink 1:" v-ink-1 "<c77>" "W:" + trim(string({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
        "  " +
        "L:" + trim(string({sys/inc/k16v.i v-form-len},">,>>9.99"))  format "x(22)" 
        "MSF:"  + trim(string(v-sht-qty * v-form-sqft / 1000,">>>9.9<"))
        format "x(11)" SKIP
        "<c45>Ink 2:" v-ink-2 "<C77>Board:" v-form-code FORM "x(30)" SKIP
        "<c45>Ink 3:" v-ink-3 "<C79>" v-form-dscr FORM "x(30)" SKIP
        "<c45>Ink 4:" v-ink-4  "<c77>" v-qty-or-sup FORM "x(36)" SKIP
        "<c45>Color Desc:" xeb.i-coldscr 
        when available xeb "<c77>" "Score:" substring(v-len-score,1,30) 
        WHEN xstyle.TYPE <> "F" format "x(30)" SKIP
        "<c45>" SUBSTRING(v-len-score,31,40) FORM "x(40)" SKIP(1) 
               
        "<c45>Die Cutting, Slit, & Saw" SKIP
        "<c45>Die #" xeb.die-no 
        when available xeb "<c66> Loc:" v-die-loc SKIP
        "<c45>Gross Size: W:"  trim(string({sys/inc/k16v.i xef.gsh-wid},">>>9.99")) +
        "  " + "L:" + trim(string({sys/inc/k16v.i xef.gsh-len},">>>9.99")) format "x(20)"

        "<c66>Slit: W:"  string(v-outw) + " L:" + trim(string(v-outl)) FORM "x(15)" SKIP 

        "<c45>Net   Size: W:"  trim(string({sys/inc/k16v.i xef.nsh-wid},">>>9.99")) + "  " +
        "L:" + trim(string({sys/inc/k16v.i xef.nsh-len},">>>9.99")) FORMAT "x(22)" SKIP
        "<c45>Die   Size: W:"  trim(string({sys/inc/k16v.i xef.trim-w},">>>9.99")) + "  " +
        "L:" + trim(string({sys/inc/k16v.i xef.trim-l},">>>9.99")) format "x(22)"
        "<c67.8>Up:" "W:" + string(v-upl) + " L:" + trim(string(v-upw)) FORM "x(9)" SKIP
        "<c45>Blank Size: W:"  trim(string({sys/inc/k16v.i xeb.t-wid},">>>9.99")) + "  " +
        "L:" + trim(string({sys/inc/k16v.i xeb.t-len},">>>9.99")) format "x(22)" SKIP
        "<c45>Impressions: " trim(string(v-dc-qty))    format "x(7)" SKIP
        "<c45>D/C Style:<P10>"
        WITH FRAME job1 NO-LABELS NO-BOX WIDTH 150 STREAM-IO.
    PUT "<=6><R+21><c84><b>Sheets Received</b>" SKIP(1)
        "<=1><R+20><C78><FROM><C103><LINE><|3>" 
        "<=1><R+22><C78><FROM><C103><LINE><|3>"
        "<=1><R+24><C78><FROM><C103><LINE><|3>"
        "<=1><p12><R+25><c85><b>Totals</b><p10>".
        
    i = 0.
    for each w-m:
        i = i + 1.
    end.
    if i lt 3 then 
    do i = i + 1 to 3:
        create w-m.
        w-m.dseq = 999999999.
    end.
    /* box for route */
        
    lv-rt-num = i + 3.

    i = 0.
    for each w-m by w-m.dseq:

        i = i + 1.

        v-letter = substr("UTE",i,1).
        PUT "<=1><R+" + string(i + 10) + "><c2>" + w-m.dscr FORMAT "x(50)" SKIP .
                  
        v-lines = v-lines + 1.
          
    end.

    FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
        AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
        
    IF AVAILABLE b-ef AND b-ef.form-no = w-ef.frm THEN 
        FOR EACH w-m:
            CREATE tt-wm.
            BUFFER-COPY w-m TO tt-wm.
        END.
       
    /* dept notes */
        
    ASSIGN 
        v-note-length   = 52
        v-tmp-lines     = 0
        j               = 0
        K               = 0
        lv-got-return   = 0
        v-dept-note     = "" 
        v-prev-note-rec = ?.

    FOR EACH notes WHERE notes.rec_key = job.rec_key and
        (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0)
        NO-LOCK:
        IF v-prev-note-rec <> ? AND
            v-prev-note-rec <> RECID(notes) THEN v-prev-extent = v-prev-extent + k.

        DO i = 1 TO LENGTH(notes.note_text):
            IF i - j >= v-note-length THEN ASSIGN j             = i
                    lv-got-return = lv-got-return + 1.

            v-tmp-lines = ( i - j ) / v-note-length.
            {SYS/INC/ROUNDUP.I v-tmp-lines}

            k = v-tmp-lines + lv-got-return + 
                IF (v-prev-note-rec <> RECID(notes) AND v-prev-note-rec <> ?) THEN v-prev-extent ELSE 0.
            IF k < 7 THEN v-dept-note[k] = v-dept-note[k] + IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1) 
                ELSE "" .              

            IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
                THEN 
            do:
                lv-got-return = lv-got-return + 1.
                j = i.
            END.         
        END.
            
        ASSIGN 
            v-prev-note-rec = RECID(notes)
            j               = 0
            lv-got-return   = 0.
    END.

    DISPLAY "<=1><R+28><C45>Production Notes" SKIP
        "<C45>" v-dept-note[1] SKIP
        "<C45>" v-dept-note[2] SKIP
        "<C45>" v-dept-note[3] SKIP
        "<C45>" v-dept-note[4] SKIP
        "<C45>" v-dept-note[5] SKIP
        "<C45>" v-dept-note[6] SKIP
        with no-box no-labels frame m8 width 170 no-attr-space STREAM-IO.

    PAGE.

    PUT "<#2><C1><R+2><FROM><C117><R+11><RECT><|3>"
        "<=2><R+2><C28><FROM><R+11><C28><LINE><|3>"
        "<=2><R+2><C45><FROM><R+11><C45><LINE><|3>"
        "<=2><R+2><C60><FROM><R+11><C60><LINE><|3>"
        "<=2><R+2><C80><FROM><R+11><C80><LINE><|3>"
        "<=2><R+6><C62><FROM><C78><LINE><|3>" 
        "<=2><R+7><C62><FROM><C78><LINE><|3>"
        "<=2><R+8><C62><FROM><C78><LINE><|3>"
        "<=2><R+9><C62><FROM><C78><LINE><|3>"
        .

    RUN cecrep/jobxpr3.p (RECID(job-hdr),v-format,cust.terms).
    RUN stackImage.

    /*spec notes*/
    ASSIGN 
        v-inst        = ""
        v-note-length = 124
        v-spec-note   = ""
        v-tmp-lines   = 0
        j             = 0
        K             = 0
        lv-got-return = 0.

    FOR EACH notes WHERE notes.rec_key = itemfg.rec_key NO-LOCK.
         
        DO i = 1 TO LENGTH(notes.note_text) :        
            IF i - j >= v-note-length THEN ASSIGN j             = i
                    lv-got-return = lv-got-return + 1.
                   
            v-tmp-lines = ( i - j ) / v-note-length.
            {SYS/INC/ROUNDUP.I v-tmp-lines}

            k = v-tmp-lines + lv-got-return.
            IF k < 9 THEN v-spec-note[k] = v-spec-note[k] + IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1) 
                ELSE "" .              
           
            IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
                THEN 
            do:
                lv-got-return = lv-got-return + 1.
                j = i.
            END.         
        END.
    END.
      
    if print-box and available xest then 
    do:
            
        run cec/desprntL.p (recid(xef),
            input-output v-lines,
            recid(xest)).
        PAGE.
    end.
    ELSE PAGE.
end.  /* for each w-ef */

IF s-prt-set-header AND last-of(job.job-no2) AND est.est-type = 6 THEN 
DO: /* print set header */
    i = 0.
    FOR EACH bf-eb WHERE bf-eb.company = est.company
        AND bf-eb.est-no = est.est-no
        AND bf-eb.form-no > 0 NO-LOCK:
        i = i + 1.
    END.   

    IF i > 1 THEN 
    DO:
        DEFINE VARIABLE v-set-qty   AS INTEGER NO-UNDO.
        DEFINE VARIABLE v-ord-qty   AS INTEGER NO-UNDO.
        DEFINE VARIABLE v-over-run  AS cha     NO-UNDO.
        DEFINE VARIABLE v-under-run AS cha     NO-UNDO.
        DEFINE VARIABLE v-fg-set    AS cha     FORM "x(15)" NO-UNDO.

        ASSIGN
            v-fg-set    = job-hdr.i-no
            v-set-qty   = if available xeb and xeb.est-type eq 6 THEN
                           if available xoe-ordl then xoe-ordl.qty else job-hdr.qty
                         ELSE 0
            v-ord-qty   = (if available xoe-ordl then xoe-ordl.qty else job-hdr.qty) *
                         if est.form-qty le 1 then 1 else v-pqty
            v-over-run  = IF AVAILABLE xoe-ordl THEN trim(string(xoe-ordl.over-pct,">>9.99%")) ELSE
                          IF AVAILABLE xoe-ord  THEN trim(string(xoe-ord.over-pct,">>9.99%"))  ELSE ""
            v-under-run = IF AVAILABLE xoe-ordl THEN trim(string(xoe-ordl.under-pct,">>9.99%")) ELSE
                           IF AVAILABLE xoe-ord  THEN trim(string(xoe-ord.under-pct,">>9.99%"))  ELSE "".

        PUT "<R3><C1><#15><C30><P16><B> SET HEADER<P7></B>" SKIP(2)
            "Job #: " AT 3 v-job-prt "<C25>Our Order #: " v-ord-no 
            "<C60>Our Date: " v-ord-date SKIP
            "Est #: " AT 3 v-est-no "<C25>FG #: " v-fg-set "<C60>Due Date: " v-due-date SKIP
            "<=1><R+6><C2><From><R+5><C78><RECT><||3>" SKIP
            "<=1><R+6><C2>CUSTOMER INFORMATION <C25> ORDER INFORMATION <C53>ITEM DESCRIPTION" SKIP
            v-cus[1] AT 3 " PO#: " v-po-no " Set Qty: "  v-set-qty
            v-i-line[2] AT 90
            SKIP
            v-cus[2] AT 3 " Job Qty:" trim(string(job-hdr.qty * v-pqty,">>>,>>9"))    format "x(7)"
            " Order Qty:" string(v-ord-qty) format "x(7)"
            v-i-line[3] AT 90 SKIP
            v-cus[3] AT 3  " Cust Part #:" lv-part-no 
            v-i-line[4] AT 90 SKIP
            v-cus[4]  AT 3 " Overrun:"  format "x(7)"  
            " Underrun:" format "x(7)"  
            "Adders:" v-adders FORM "x(33)" AT 90 SKIP
            "<=1><R+11><C30><P8><B>Set Components<P7></B> <C50>Set item: " v-fg-set SKIP
            "<C2>FINISHED GOOD #                 DESCRIPTION                       RATIO PER SET" SKIP.
        /* each components */
        DEFINE VARIABLE v-tmp-line AS INTEGER NO-UNDO.
        DEFINE VARIABLE v-shipto   AS cha     NO-UNDO.
        
        v-tmp-line = 0.
        FOR EACH xeb WHERE xeb.company = est.company
            AND xeb.est-no = est.est-no
            AND xeb.form-no > 0 NO-LOCK:
            PUT xeb.stock-no AT 3 space(14) xeb.part-dscr1 space(5) xeb.quantityPerSet SKIP.
            v-tmp-line = v-tmp-line + 1.
        END.
        v-tmp-line = v-tmp-line + 1.
        /* print raw materials from misc/fram of Est */ 
        FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
            AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
        DO i = 1 TO 8:
            IF b-ef.spec-no[i] <> "" THEN 
            DO:
                RUN custom/extradec.p (.0001, b-ef.spec-qty[i],
                    OUTPUT lv-spec-qty[i]).
                PUT b-ef.spec-dscr[i] AT 32 space(16) lv-spec-qty[i] SKIP.
                v-tmp-line = v-tmp-line + 1.
            END.
        END.
        PUT "<=1><R+12><C2><FROM><R+" + string(v-tmp-line) + "><C78><RECT><||3>" FORM "x(150)" SKIP.
        v-tmp-line = v-tmp-line + 12 .
        
        i = 0.
        for each tt-wm WHERE lookup(tt-wm.m-code,tspostfg-char) > 0:
            i = i + 1.
        END.
        i = i + 2.
        PUT "  Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP
            "<=1><R+" + string(v-tmp-line + 1) + "><C2><FROM><R+" + string(i) + "><C78><RECT><||3>" FORM "x(150)" SKIP
            "<=1><R+" + string(v-tmp-line + 1) + ">" FORM "x(20)".
        
        i = 0.
        for each tt-wm WHERE lookup(tt-wm.m-code,tspostfg-char) > 0  by tt-wm.dseq:
            i = i + 1.
            display tt-wm.dscr AT 3
                tt-wm.s-hr 
                when tt-wm.s-hr ne 0
                fill("_",7)  format "x(7)"    to 38   
                when tt-wm.dscr ne ""
                fill("_",7)  format "x(7)"    to 46   
                when tt-wm.dscr ne ""
                fill("_",7)  format "x(7)"    to 54   
                when tt-wm.dscr ne ""
                space(2)
                tt-wm.r-sp 
                when tt-wm.r-sp ne 0
                fill("_",7)  format "x(7)"    to 69   
                when tt-wm.dscr ne ""
                fill("_",7)  format "x(7)"    to 77   
                when tt-wm.dscr ne ""
                fill("_",7)  format "x(7)"    to 85   
                when tt-wm.dscr ne ""
                fill("_",8)  format "x(8)"    to 99   
                when tt-wm.dscr ne ""
                fill("_",8)  format "x(8)"    to 108  
                when tt-wm.dscr ne ""
                fill("_",8)  format "x(8)"    to 117  
                when tt-wm.dscr ne ""
                fill("_",8)  format "x(8)"    to 129  
                when tt-wm.dscr ne ""
                /*chr(124) format "x"           at 131   */                  
                with no-box no-labels frame o21 width 132 no-attr-space down STREAM-IO.
        
        end.
        FOR EACH tt-wm:
            DELETE tt-wm.
        END.

        ASSIGN
            v-tmp-line = v-tmp-line + 3 + i /* 4 and add machine routing lines */
            v-shipto   = IF AVAILABLE xoe-rel THEN xoe-rel.ship-id 
                           ELSE IF available xeb THEN xeb.ship-id
                           ELSE IF available xoe-ord THEN xoe-ord.sold-id 
                           ELSE "".

        FIND FIRST tt-prem WHERE tt-prem.tt-job-no  EQ job-hdr.job-no
            AND tt-prem.tt-job-no2  EQ job-hdr.job-no2 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE tt-prem THEN CREATE tt-prem.
        
        ASSIGN 
            v-tmp-lines   = 0
            j             = 0
            K             = 0
            lv-got-return = 0
            v-dept-inst   = "".
        
        {custom/notespr2.i job v-inst2 6 "notes.rec_key = job.rec_key and
                              notes.note_form_no = 0" }
        DO i = 1 TO 6:
            v-dept-inst[i] = v-inst2[i].
        END.
        IF v-ship <> "" THEN v-dept-inst[6] = v-ship.  /* shipto notes */
        PUT "<=1><R+" + string(v-tmp-line) + ">" form "X(20)".
        v-tmp-line = v-tmp-line + 1.
        PUT "Unitizing Bale <C24>Date <C44>Units <C62>Complete" AT 3 SKIP
            "# Per Bndl: " AT 3 tt-prem.tt-#-bundle "<C20>_____________________ <C40>____________________  <C60>________________" skip
            "# Per Unit: " AT 3 tt-prem.tt-#-unit "<C20>_____________________ <C40>____________________  <C62>Partial" skip
            "Pattern: " AT 3 tt-prem.tt-pattern "<C20>_____________________ <C40>____________________  <C60>________________" skip
            "Pallet: " AT 3 tt-prem.tt-pallet "<C20>_____________________ <C40>____________________ " skip
            "<=1><R+" + string(v-tmp-line) + "><C2><FROM><R+6><C78><RECT><||3>" FORM "x(150)" SKIP
            "<=1><R+" + string(v-tmp-line + 7) + "><C2><FROM><R+7><C78><RECT><||3>" FORM "x(150)" SKIP
        
            "<=1><R+" + string(v-tmp-line + 7) + "><C2>Special instructions  <C51>SHIPPING INFO       Ship to: " + v-shipto FORM "x(250)" SKIP
            v-dept-inst[1] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[1] SKIP
            v-dept-inst[2] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[2] SKIP
            v-dept-inst[3] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[3] SKIP
            v-dept-inst[4] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[4] SKIP
            v-dept-inst[5] AT 3 FORM "x(82)" chr(124) format "xx" "Item PO #:" v-po-no SKIP
            v-dept-inst[6] 
            .
        
        PAGE.
    END. /* set header printing */
END. /* est.est-type = 6 */
/* end of set header printing */

end.  /* each job */
end.  /* end v-local-loop  */
 
hide all no-pause.


PROCEDURE stackImage:
    DEFINE BUFFER pattern      FOR reftable.
    DEFINE BUFFER stackPattern FOR stackPattern.
    IF v-stackcode EQ '' THEN RETURN.
    FIND FIRST stackPattern NO-LOCK
        WHERE stackPattern.stackCode EQ SUBSTR(v-stackcode,9,1) NO-ERROR.
    IF AVAILABLE stackPattern AND SEARCH(stackPattern.stackImage) NE ? THEN
        PUT UNFORMATTED "<C28.5><R-6.5><#1><R+9><C+35><IMAGE#1=" stackPattern.stackImage ">" FORMAT "x(200)" SKIP(2) . 
/* PUT UNFORMATTED
   "<=2><C30><R+4><FROM><C45><R+12>"
   "<IMAGE#71=" stackPattern.stackImage ">"
   "<R+15>".*/
END PROCEDURE.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
