/* fgrep/r-spcard.p  07/07/04  YSK*/
/*  Accord Carton Finished goods spec card */   
/*  factory ticket  for folding , LivingSton Box                              */
/* -------------------------------------------------------------------------- */

def input parameter ip-rowid AS ROWID.
DEF INPUT PARAM ip-dept AS cha NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

{custom/xprint.i}
{custom/formtext.i NEW}
{sys/inc/notes.i}

DEF VAR list-name AS cha NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF VAR ls-mail-file AS cha NO-UNDO.
DEF VAR ret-code AS INT NO-UNDO.
DEF VAR v-style LIKE eb.style NO-UNDO.
DEF VAR v-len LIKE eb.len NO-UNDO.
DEF VAR v-wid LIKE eb.wid NO-UNDO.
DEF VAR v-dep LIKE eb.dep NO-UNDO.
DEF VAR v-blen LIKE eb.t-len NO-UNDO.
DEF VAR v-bwid LIKE eb.t-wid NO-UNDO.
DEF VAR v-cad LIKE eb.cad-no NO-UNDO.
DEF VAR v-die LIKE eb.die-no NO-UNDO.
DEF VAR v-plate LIKE eb.plate NO-UNDO.
DEF VAR v-board AS cha NO-UNDO.
DEF VAR v-ink-cnt AS INT NO-UNDO.
DEF VAR v-coat-cnt AS INT NO-UNDO.
DEF VAR v-ink AS cha FORM "x(10)" EXTENT 10 NO-UNDO.
DEF VAR v-coat AS cha FORM "x(10)" EXTENT 10 NO-UNDO.
DEF VAR v-case-count LIKE itemfg.case-count NO-UNDO.
DEF VAR v-pallet-count LIKE itemfg.case-count NO-UNDO.
DEF VAR v-cases-per-pallet LIKE itemfg.case-count NO-UNDO.
DEF VAR v-dept-note AS cha EXTENT 5 NO-UNDO.
DEF VAR v-pack-note AS cha EXTENT 3 NO-UNDO.
DEF VAR v-spec-note AS cha NO-UNDO.
DEF VAR lv-text AS cha NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-est-rec-key AS cha NO-UNDO.
DEF VAR v-tr-no AS cha NO-UNDO.
DEF VAR v-cas-no AS cha NO-UNDO.
DEF VAR v-leadtime AS INT NO-UNDO.
DEF VAR v-comp-add1 AS cha FORM "x(70)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(50)" NO-UNDO.
DEF VAR v-comp-name AS cha FORM "x(50)" NO-UNDO.



FIND itemfg WHERE ROWID(itemfg) = ip-rowid NO-LOCK NO-ERROR.
IF NOT AVAIL itemfg THEN RETURN.

ASSIGN
   list-name = "c:/tmp/fgspcard.xpr"
   v-leadtime = itemfg.lead-days.

FIND FIRST est WHERE est.company = itemfg.company
                 AND est.est-no = itemfg.est-no NO-LOCK NO-ERROR.
lv-est-rec-key = IF AVAIL est THEN est.rec_key ELSE "".

IF itemfg.ship-meth THEN
   ASSIGN v-case-count = itemfg.case-count
          v-pallet-count = 0.

ELSE
   ASSIGN v-case-count = 0
          v-pallet-count = itemfg.case-count.

FIND FIRST eb WHERE eb.company = itemfg.company
                AND eb.est-no = itemfg.est-no 
                AND eb.stock-no = itemfg.i-no NO-LOCK NO-ERROR.
IF AVAIL eb THEN DO:
   FIND FIRST ef WHERE ef.company = itemfg.company
                   AND ef.est-no = itemfg.est-no 
                   AND ef.form-no = eb.form-no NO-LOCK NO-ERROR.
   FIND FIRST style WHERE style.company = itemfg.company
                      AND style.style = eb.style NO-LOCK NO-ERROR.

   ASSIGN v-style = IF AVAIL style THEN style.dscr ELSE eb.style
          v-len = eb.len
          v-wid = eb.wid
          v-dep = eb.dep
          v-blen = eb.t-len
          v-bwid = eb.t-wid
          v-cad = eb.cad-no
          v-die = eb.die-no
          v-plate = eb.plate-no
          v-tr-no = eb.tr-no
          v-cas-no = eb.cas-no
          v-board = IF AVAIL ef THEN ef.board + ", " + ef.brd-dscr ELSE ""
          v-case-count = eb.cas-cnt
          v-pallet-count = eb.tr-cnt
          v-cases-per-pallet = eb.cas-pal
          .
   FIND FIRST ITEM WHERE ITEM.company = eb.company
                     AND ITEM.i-no = eb.tr-no NO-LOCK NO-ERROR.
   IF AVAIL ITEM THEN v-tr-no = ITEM.i-NAME.
   FIND FIRST ITEM WHERE ITEM.company = eb.company
                     AND ITEM.i-no = eb.cas-no NO-LOCK NO-ERROR.
   IF AVAIL ITEM THEN v-cas-no = ITEM.i-NAME.

   ASSIGN v-ink-cnt = 1
          v-coat-cnt = 1.

   DO i = 1 TO 10:
     IF eb.est-type <= 4 THEN DO:
        IF eb.i-code2[i] <> "" THEN DO:
           FIND FIRST ITEM WHERE ITEM.company = itemfg.company
                        AND ITEM.i-no = eb.i-code2[i] NO-LOCK NO-ERROR.
           IF AVAIL ITEM AND ITEM.mat-type = "V" THEN 
              ASSIGN v-coat[v-coat-cnt] = eb.i-code2[i]
                     v-coat-cnt = v-coat-cnt + 1.
           ELSE ASSIGN v-ink[v-ink-cnt] = eb.i-code2[i]
                       v-ink-cnt = v-ink-cnt + 1.
        END.
     END.
     else DO:
          IF eb.i-code[i] <> "" THEN DO:
             FIND FIRST ITEM WHERE ITEM.company = itemfg.company
                             AND ITEM.i-no = eb.i-code[i] NO-LOCK NO-ERROR.
             IF AVAIL ITEM AND ITEM.mat-type = "V" THEN 
                   ASSIGN v-coat[v-coat-cnt] = eb.i-code[i]
                      v-coat-cnt = v-coat-cnt + 1.
             ELSE ASSIGN v-ink[v-ink-cnt] = eb.i-code[i]
                         v-ink-cnt = v-ink-cnt + 1.
          END.
     END.
   END.

END.
   
IF v-case-count EQ 0 AND v-cases-per-pallet EQ 0 THEN
DO:
   FIND FIRST fg-bin WHERE
        fg-bin.company = itemfg.company AND
        fg-bin.i-no = itemfg.i-no
        NO-LOCK NO-ERROR.
   IF AVAIL fg-bin THEN DO:
      FOR EACH fg-rcpth WHERE fg-rcpth.company = itemfg.company
                          AND fg-rcpth.i-no = itemfg.i-no 
                          AND fg-rcpth.rita-code = "R" NO-LOCK 
                          BY fg-rcpth.trans-date DESC.
          FIND FIRST fg-rdtlh WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code NO-LOCK NO-ERROR.
          IF AVAIL fg-rdtlh THEN ASSIGN v-case-count = fg-rdtlh.qty-case
                                        v-cases-per-pallet = fg-rdtlh.stacks-unit.
      END.
   END.
END.
          
 v-spec-note = "".
  FIND FIRST notes WHERE notes.rec_key = itemfg.rec_key AND
                         notes.note_code = ip-dept NO-LOCK no-error.
  IF AVAIL notes THEN v-spec-note = notes.note_title.

   /* ===== packing notes =*/
   FOR EACH tt-formtext:
        DELETE tt-formtext.
   END.
   lv-text = "".
   FOR EACH notes WHERE notes.rec_key = lv-est-rec-key 
                    AND notes.note_code = "GL" NO-LOCK:
       lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
   END.
   DO li = 1 TO 3:
      CREATE tt-formtext.
      ASSIGN tt-line-no = li
             tt-length  = 65.
   END.
   RUN custom/formtext.p (lv-text).
   i = 0.
   v-pack-note = "".
   FOR EACH tt-formtext:
       i = i + 1.
       IF  i <= 3 THEN v-pack-note[i] = tt-formtext.tt-text.     
   END.

   /* ===== Special instruction notes =*/
   FOR EACH tt-formtext:
        DELETE tt-formtext.
   END.
   lv-text = "".
   FOR EACH notes WHERE notes.rec_key = lv-est-rec-key 
                    AND notes.note_code <> "GL" NO-LOCK:
       lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
   END.
   DO li = 1 TO 4:
      CREATE tt-formtext.
      ASSIGN tt-line-no = li
             tt-length  = 90.
   END.
   RUN custom/formtext.p (lv-text).
   i = 0.
   v-dept-note = "".
   FOR EACH tt-formtext:
       i = i + 1.
       IF  i <= 4 THEN v-dept-note[i] = tt-formtext.tt-text.      
   END.

ASSIGN v-comp-add1 = ""
       v-comp-add2 = ""
       v-comp-name = "".

FIND FIRST cust WHERE cust.company = itemfg.company AND
                      cust.active = "X" NO-LOCK NO-ERROR.
IF AVAIL cust THEN
   ASSIGN v-comp-add1 = cust.addr[1] + " " +
                        cust.city + ", " + cust.state + "  " + cust.zip
          v-comp-add2 = "Phone: " + string(cust.area-code,"(999)") +
                         string(cust.phone,"999-9999") + " - " +
                        "Fax  : " + string(cust.fax,"(999)999-9999") 
          v-comp-name = cust.NAME.   

PUT unformatted
    "<#1><R+1><C24><B><FTimes New Roman><P12>" fill(" ",32 - length(v-comp-name))  /*Accord Carton*/ v-comp-name SKIP
    "<C25>Finished Goods Specification Card </B>" SKIP
    "<P10><C26>" v-comp-add1 SKIP
    "<C25>" v-comp-add2 SKIP(1).
PUT
    "<#2><C10><From><R+56><C+62><RECT><||5>" skip
     /* product information */
    "<=2><R+2><C11><From><R+8><c+60><RECT><||3>" skip
    "<=2><R+3><C11><From><c+60><Line><||3>" skip
    "<=2><R+4><C11><From><c+60><Line><||3>" skip
    "<=2><R+5><C11><From><c+60><Line><||3>" SKIP
    "<=2><R+6><C11><From><c+60><Line><||3>" skip
    "<=2><R+7><C11><From><c+60><Line><||3>" skip
    "<=2><R+8><C11><From><c+60><Line><||3>" skip
    "<=2><R+9><C11><From><c+60><Line><||3>" skip
    "<=2><R+2><C27><From><R+8><C27><Line><||3>" skip
      /* carton specifications */
    "<=2><R+11><C10><From><c+62><Line><||3>" skip
    "<=2><R+13><C11><From><R+14><c+60><RECT><||3>" skip
    "<=2><R+14><C11><From><c+60><Line><||3>" 
    "<=2><R+15><C11><From><c+60><Line><||3>" 
    "<=2><R+16><C11><From><c+60><Line><||3>" 
    "<=2><R+17><C11><From><c+60><Line><||3>" 
    "<=2><R+18><C11><From><c+60><Line><||3>" 
    "<=2><R+19><C11><From><c+60><Line><||3>" 
    "<=2><R+20><C11><From><c+60><Line><||3>" 
    "<=2><R+21><C11><From><c+60><Line><||3>" 
    "<=2><R+22><C11><From><c+60><Line><||3>" 
    "<=2><R+23><C11><From><c+60><Line><||3>" 
    "<=2><R+24><C11><From><c+60><Line><||3>" 
    "<=2><R+25><C11><From><c+60><Line><||3>" 
    "<=2><R+26><C11><From><c+60><Line><||3>" 
    "<=2><R+13><C26><From><R+14><C26><Line><||3>" skip
    /* color detail line */
    "<=2><R+20><C32.3><From><R+1><C32.3><Line><||3>" skip
    "<=2><R+20><C39.1><From><R+1><C39.1><Line><||3>" skip
    "<=2><R+20><C45.3><From><R+1><C45.3><Line><||3>" skip
    "<=2><R+20><C51.8><From><R+1><C51.8><Line><||3>" skip
    "<=2><R+20><C58.2><From><R+1><C58.2><Line><||3>" skip
    "<=2><R+20><C65.3><From><R+1><C65.3><Line><||3>" skip
    /* coat detail line */
    "<=2><R+22><C32.3><From><R+1><C32.3><Line><||3>" skip
    "<=2><R+22><C39.1><From><R+1><C39.1><Line><||3>" skip
    "<=2><R+22><C45.3><From><R+1><C45.3><Line><||3>" skip
    "<=2><R+22><C51.8><From><R+1><C51.8><Line><||3>" skip
    "<=2><R+22><C58.2><From><R+1><C58.2><Line><||3>" skip
    "<=2><R+22><C65.3><From><R+1><C65.3><Line><||3>" skip
      /*purchasing/inventory */
    "<=2><R+28><C10><From><c+62><Line><||3>" skip
    "<=2><R+30><C11><From><R+9><c+60><RECT><||3>" skip
    "<=2><R+31><C11><From><c+60><Line><||3>" skip
    "<=2><R+32><C11><From><c+60><Line><||3>" skip
    "<=2><R+33><C11><From><c+60><Line><||3>" skip
    "<=2><R+34><C11><From><c+60><Line><||3>" skip
    "<=2><R+35><C11><From><c+60><Line><||3>" skip
    "<=2><R+36><C11><From><c+60><Line><||3>" skip
    "<=2><R+37><C11><From><c+60><Line><||3>" skip
    "<=2><R+38><C11><From><c+60><Line><||3>" skip
    "<=2><R+30><C26><From><R+9><C26><Line><||3>" skip
    /* packing information */
    "<=2><R+40><C10><From><c+62><Line><||3>" skip
    "<=2><R+42><C11><From><R+6><c+60><RECT><||3>" skip
    "<=2><R+43><C11><From><c+60><Line><||3>" skip
    "<=2><R+44><C11><From><c+60><Line><||3>" skip
    "<=2><R+42><C29><From><R+6><C29><Line><||3>" skip
    /* special instruction*/
    "<=2><R+49><C10><From><c+62><Line><||3>" SKIP.


PUT
    /* TEXT <FNew Courier>*/
    "<=2><C12><FTimes New Roman><B><I><U>PRODUCTION INFORMATION:</I></U>" SKIP
    "<=2><R+11><C12><B><I><U>CARTON SPECIFICATIONS:</I></U>" SKIP
    "<=2><R+28><C12><B><I><U>PURCHASING / INVENTORY INFORMATION:</I></U>" SKIP
    "<=2><R+40><C12><B><I><U>PACKING INFORMATION:</I></U>" SKIP
    "<=2><R+49><C12><B><I><U>SPECIAL INSTRUCTIONS:</I></U></B>" SKIP
    "<=2><R+2><C12>ITEM NAME:" itemfg.i-name AT 48 SKIP
    "<=2><R+3><C12>FG ITEM#:" itemfg.i-no AT 52 SKIP
    "<=2><R+4><C12>CUSTOMER PART#:" itemfg.part-no AT 40 SKIP
    "<=2><R+5><C12>DESCRIPTION 1:" itemfg.part-dscr1 AT 45 SKIP
    "<=2><R+6><C12>DESCRIPTION 2:" itemfg.part-dscr2 AT 45 SKIP
    "<=2><R+7><C12>DESCRIPTION 3:" itemfg.part-dscr3 AT 45 SKIP
    "<=2><R+8><C12>SPC/QC#:" itemfg.spc-no AT 52 SKIP
    "<=2><R+9><C12>UPC#:" itemfg.upc-no AT 55 SKIP

    "<=2><R+13><C12>STYLE:" v-style FORM "x(30)" AT 51  SKIP
    "<=2><R+14><C12>BOARD:" v-board FORM "x(40)" AT 50 SKIP
    "<=2><R+15><C12>LENGTH:" v-len FORM ">>9.99999" AT 48 SKIP
    "<=2><R+16><C12>WIDTH:"  v-wid FORM ">>9.99999" AT 50 SKIP
    "<=2><R+17><C12>DEPTH:" v-dep FORM ">>9.99999" AT 50 SKIP
    "<=2><R+18><C12>BLANK LENGTH:" v-blen AT 41 SKIP
    "<=2><R+19><C12>BLANK WIDTH:" v-bwid AT 43 SKIP
    "<=2><R+20><C12>COLORS:" "1               2                3                 4                5                 6                 7" AT 55 SKIP
    "<=2><R+21><P8><FCourier New>" v-ink[1] AT 67  v-ink[2] v-ink[3] v-ink[4] v-ink[5] v-ink[6] v-ink[7] SKIP
    "<P10><=2><R+22><C12><FTimes New Roman>COATINGS:" 
      "1               2                3                 4                5                 6                 7" AT 75 SKIP
    "<=2><R+23><P8><FCourier New>" v-coat[1] AT 67 v-coat[2] v-coat[3] v-coat[4] v-coat[5] v-coat[6] v-coat[7] 
    "<P10><FTimes New Roman>" SKIP
    "<=2><R+24><C12>CAD#:" v-cad AT 51 SKIP
    "<=2><R+25><C12>DIE#:" v-die AT 53 SKIP
    "<=2><R+26><C12>PLATE#:" v-plate AT 49 SKIP

    "<=2><R+30><C12>PURCHASED UOM:"  itemfg.pur-uom AT 39  SKIP
    "<=2><R+31><C12>REORDER LEVEL:" "<C25><FCourier New>" itemfg.ord-level FORM ">>>>>,>>>,>>9.999" /*AT 39*/  "<FTimes New Roman>" SKIP
    "<=2><R+32><C12>MIN-ORDER:" "<C27><FCourier New>"     itemfg.ord-min   FORM ">>>,>>>,>>>,>>9" /*AT 52*/ "<FTimes New Roman>" SKIP
    "<=2><R+33><C12>MAX-ORDER:" "<C27><FCourier New>" itemfg.ord-max       FORM ">>>,>>>,>>>,>>9" /*AT 46*/  "<FTimes New Roman>" SKIP
    "<=2><R+34><C12>CASE COUNT:" "<C27><FCourier New>"  v-case-count       FORM ">>>,>>>,>>>,>>9" /*AT 58*/ "<FTimes New Roman>"  SKIP
    "<=2><R+35><C12>CASES/PALLET:" "<C27><FCourier New>" v-cases-per-pallet FORM ">>>,>>>,>>>,>>9"   /*AT 64*/ "<FTimes New Roman>"
    "<=2><R+36><C12>PALLET COUNT:" "<C27><FCourier New>" v-pallet-count FORM ">>>,>>>,>>>,>>9" /*AT 54*/ "<FTimes New Roman>" SKIP
    "<=2><R+37><C12>LEAD TIME:" "<C27><FCourier New>" v-leadtime FORM ">>>,>>>,>>>,>>9"  /* AT 64 */  "<FTimes New Roman>" 
    "<=2><R+38><C12>SHELF LIFE:" "<C27>" v-spec-note FORM "x(40)" SKIP
    
    "<=2><R+42><C12>PALLET REQUIREMENT:" v-tr-no AT 41 FORM "x(30)" SKIP
    "<=2><R+43><C12>CORRUGATED:" v-cas-no AT 51 FORM "x(30)" SKIP
    "<=2><R+44><C12>PACKING NOTES:" v-pack-note[1] AT 49 FORM "x(65)" SKIP
    "<=2><R+45>" v-pack-note[2] AT 94 FORM "x(65)" SKIP
    "<=2><R+46>" v-pack-note[3] AT 94 FORM "x(65)" SKIP

    "<=2><R+50><C13>" v-dept-note[1] FORM "x(90)" SKIP
    "<=2><R+51><C13>" v-dept-note[2] FORM "x(90)" SKIP
    "<=2><R+52><C13>" v-dept-note[3] FORM "x(90)" SKIP
    "<=2><R+53><C13>" v-dept-note[4] FORM "x(90)" SKIP.
