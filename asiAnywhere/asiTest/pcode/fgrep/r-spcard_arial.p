/* fgrep/r-spcard.p  07/07/04  YSK*/
/*  Accord Carton Finished goods spec card */   
/*  factory ticket  for folding , LivingSton Box                              */
/* -------------------------------------------------------------------------- */

def input parameter ip-rowid AS ROWID.


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

DEF VAR v-ink-cnt AS INT NO-UNDO.
DEF VAR v-coat-cnt AS INT NO-UNDO.
DEF VAR v-ink AS cha FORM "x(10)" EXTENT 7 NO-UNDO.
DEF VAR v-coat AS cha FORM "x(10)" EXTENT 7 NO-UNDO.
DEF VAR v-case-count LIKE itemfg.case-count NO-UNDO.
DEF VAR v-pallet-count LIKE itemfg.case-count NO-UNDO.
DEF VAR v-dept-note AS cha EXTENT 5 NO-UNDO.
DEF VAR lv-text AS cha NO-UNDO.
DEF VAR li AS INT NO-UNDO.


list-name = "c:/tmp/testcard.xpr".

FIND itemfg WHERE ROWID(itemfg) = ip-rowid NO-LOCK NO-ERROR.
IF NOT AVAIL itemfg THEN RETURN.

FIND FIRST eb WHERE eb.company = itemfg.company
                AND eb.est-no = itemfg.est-no 
                AND eb.stock-no = itemfg.i-no NO-LOCK NO-ERROR.

IF AVAIL eb THEN DO:
   ASSIGN v-style = eb.style
          v-len = eb.len
          v-wid = eb.wid
          v-dep = eb.dep
          v-blen = eb.t-len
          v-bwid = eb.t-wid
          v-cad = eb.cad-no
          v-die = eb.die-no
          v-plate = eb.plate-no.

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
   
          
OUTPUT TO VALUE(list-name).
IF itemfg.ship-meth  THEN ASSIGN v-case-count = itemfg.case-count
                                 v-pallet-count = 0.
ELSE ASSIGN v-case-count = 0
            v-pallet-count = itemfg.case-count.

lv-pdf-file = "c:/tmp/" /*+ trim(itemfg.i-no)*/ + "testcard".

   /* ===== notes =*/
   FOR EACH tt-formtext:
        DELETE tt-formtext.
   END.

   lv-text = "".

   FOR EACH notes WHERE notes.rec_key = itemfg.rec_key NO-LOCK:
       lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
   END.

   DO li = 1 TO 4:
      CREATE tt-formtext.
      ASSIGN tt-line-no = li
             tt-length  = 70.
    END.

    RUN custom/formtext.p (lv-text).
    i = 0.
    v-dept-note = "".
    FOR EACH tt-formtext:
        i = i + 1.
        IF  i <= 4 THEN v-dept-note[i] = tt-formtext.tt-text.      
    END.

/*<PREVIEW>*/
PUT "<PRINT=NO><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(100)".

PUT "<#1><R+1><C33><B><FNew Courier><P12>Accord Carton" SKIP
    "<C25>Finished Goods Specification Card </B>" SKIP
    "<P10><C27>940 West 94th Street Chicago, IL 60620" SKIP
    "<C25>Phone (773)445-5030 - Fax (773)445-1636" SKIP(1)
    "<#2><C10><From><R+55><C+62><RECT><||5>" skip
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
    "<=2><R+14><C11><From><c+60><Line><||3>" skip
    "<=2><R+15><C11><From><c+60><Line><||3>" skip
    "<=2><R+16><C11><From><c+60><Line><||3>" skip
    "<=2><R+17><C11><From><c+60><Line><||3>" skip
    "<=2><R+18><C11><From><c+60><Line><||3>" skip
    "<=2><R+19><C11><From><c+60><Line><||3>" skip
    "<=2><R+20><C11><From><c+60><Line><||3>" skip
    "<=2><R+21><C11><From><c+60><Line><||3>" skip
    "<=2><R+22><C11><From><c+60><Line><||3>" skip
    "<=2><R+23><C11><From><c+60><Line><||3>" skip
    "<=2><R+24><C11><From><c+60><Line><||3>" skip
    "<=2><R+25><C11><From><c+60><Line><||3>" skip
    "<=2><R+26><C11><From><c+60><Line><||3>" skip
    "<=2><R+13><C26><From><R+14><C26><Line><||3>" skip
    /* color detail line */
    "<=2><R+20><C33><From><R+1><C33><Line><||3>" skip
    "<=2><R+20><C39.5><From><R+1><C39.5><Line><||3>" skip
    "<=2><R+20><C46><From><R+1><C46><Line><||3>" skip
    "<=2><R+20><C51.5><From><R+1><C51.5><Line><||3>" skip
    "<=2><R+20><C58><From><R+1><C58><Line><||3>" skip
    "<=2><R+20><C63.5><From><R+1><C63.5><Line><||3>" skip
    /* coat detail line */
    "<=2><R+22><C35><From><R+1><C35><Line><||3>" skip

      /*purchasing/inventory */
    "<=2><R+28><C10><From><c+62><Line><||3>" skip
    "<=2><R+30><C11><From><R+8><c+60><RECT><||3>" skip
    "<=2><R+31><C11><From><c+60><Line><||3>" skip
    "<=2><R+32><C11><From><c+60><Line><||3>" skip
    "<=2><R+33><C11><From><c+60><Line><||3>" skip
    "<=2><R+34><C11><From><c+60><Line><||3>" skip
    "<=2><R+35><C11><From><c+60><Line><||3>" skip
    "<=2><R+36><C11><From><c+60><Line><||3>" skip
    "<=2><R+37><C11><From><c+60><Line><||3>" skip
    "<=2><R+30><C26><From><R+8><C26><Line><||3>" skip
    /* packing information */
    "<=2><R+39><C10><From><c+62><Line><||3>" skip
    "<=2><R+41><C11><From><R+6><c+60><RECT><||3>" skip
    "<=2><R+42><C11><From><c+60><Line><||3>" skip
    "<=2><R+43><C11><From><c+60><Line><||3>" skip
    "<=2><R+41><C29><From><R+6><C29><Line><||3>" skip
    /* special instruction*/
    "<=2><R+48><C10><From><c+62><Line><||3>" SKIP.
PUT
    /* TEXT */
    "<=2><C12><FNew Courier><B><I><U>PRODUCTION INFORMATION:</I></U>" SKIP
    "<=2><R+11><C12><B><I><U>CARTON SPECIFICATIONS:</I></U>" SKIP
    "<=2><R+28><C12><B><I><U>PURCHASING / INVENTORY INFORMATION:</I></U>" SKIP
    "<=2><R+39><C12><B><I><U>PACKING INFORMATION:</I></U>" SKIP
    "<=2><R+48><C12><B><I><U>SPECIAL INSTRUCTIONS:</I></U></B>" SKIP
    "<=2><R+2><C12><FArial>ITEM NAME:" itemfg.i-name AT 53 SKIP
    "<=2><R+3><C12>FG ITEM#:" itemfg.i-no AT 47 SKIP
    "<=2><R+4><C12>CUSTOMER PART#:" itemfg.part-no AT 36 SKIP
    "<=2><R+5><C12>DESCRIPTION 1:" itemfg.part-dscr1 AT 41 SKIP
    "<=2><R+6><C12>DESCRIPTION 2" itemfg.part-dscr2 AT 41 SKIP
    "<=2><R+7><C12>DESCRIPTION 3" itemfg.part-dscr3 AT 41 SKIP
    "<=2><R+8><C12>SPC/QC#:" itemfg.spc-no AT 47 SKIP
    "<=2><R+9><C12>UPC#:" itemfg.upc-no AT 50 SKIP

    "<=2><R+13><C12>STYLE:" v-style AT 47 SKIP
    "<=2><R+14><C12>MATERIAL:" SKIP
    "<=2><R+15><C12>LENGTH:" v-len AT 45 SKIP
    "<=2><R+16><C12>WIDTH:"  v-wid AT 47 SKIP
    "<=2><R+17><C12>DEPTH:" v-dep AT 47 SKIP
    "<=2><R+18><C12>BLANK LENGTH:" v-blen AT 37 SKIP
    "<=2><R+19><C12>BLANK WIDTH:" v-bwid AT 39 SKIP
    "<=2><R+20><C12>COLORS:" SKIP
    "<=2><R+21><C30><P9>" v-ink[1] AT 30  v-ink[2] v-ink[2] v-ink[4] v-ink[5] v-ink[6] v-ink[7] SKIP
    "<P10><=2><R+22><C12>COATINGS:" SKIP
    "<=2><R+23><C30><P9>" v-coat[1] v-coat[2] v-coat[3] v-coat[4] v-coat[5] v-coat[6] v-coat[7] 
    "<P10>" SKIP
    "<=2><R+24><C12>CAD#:" v-cad AT 49 SKIP
    "<=2><R+25><C12>DIE#:" v-die AT 51 SKIP
    "<=2><R+26><C12>PLATE#:" v-plate AT 47 SKIP

    "<=2><R+30><C12>PURCHASED UOM:" itemfg.pur-uom AT 36 SKIP
    "<=2><R+31><C12>REORDER LEVEL:" itemfg.ord-level AT 38 SKIP
    "<=2><R+32><C12>MIN-ORDER:" itemfg.ord-min FORM ">>>,>>>,>>9" AT 50 SKIP
    "<=2><R+33><C12>MAX-ORDER:" itemfg.ord-max FORM ">>>,>>>,>>9" AT 49 SKIP
    "<=2><R+34><C12>CASE COUNT:" v-case-count FORM ">>>,>>9" AT 55 SKIP
    "<=2><R+35><C12>PALLET COUNT:" v-pallet-count FORM ">>>,>>9" AT 50 SKIP
    "<=2><R+37><C12>SHELF LIFE:" AT 50 SKIP

    "<=2><R+41><C12>PALLET REQUIREMENT:" SKIP
    "<=2><R+42><C12>CORRUGATED:" SKIP
    "<=2><R+43><C12>PACKING NOTES:" itemfg.prod-notes AT 45 SKIP

    "<=2><R+49><C13>" v-dept-note[1] FORM "x(70)" SKIP
    "<=2><R+50><C13>" v-dept-note[2] FORM "x(70)" SKIP
    "<=2><R+51><C13>" v-dept-note[3] FORM "x(70)" SKIP
    "<=2><R+52><C13>" v-dept-note[4] FORM "x(70)" SKIP
    .








OUTPUT CLOSE.



RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

 ls-mail-file = lv-pdf-file + ".pdf".

  run custom/xpmail.p ("CUSTOMER",ls-mail-file,"",
                                'FG Spec Card',
                                'FG SPec Card',OUTPUT ret-code).
/*
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= '"A"'
                             &END_cust='"A"'
                             &mail-subject= 'FG Spec Card'
                             &mail-body='FG Spec Card'
                             &mail-file=lv-pdf-file + ".pdf" }
*/                             
