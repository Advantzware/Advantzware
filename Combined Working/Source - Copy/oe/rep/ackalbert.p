/*----------------------------------------------- oe/rep/ackalbert.p */
/* Albert Order Acknowledgement                                     */
/*----------------------------------------------------------------*/

{sys/inc/var.i shared}

{oe/rep/acknowl.i}

DEF VAR viWorkSheetCount AS INT NO-UNDO.
DEFINE SHARED VARIABLE LvOutputSelection    AS CHAR NO-UNDO.
DEFINE VAR CurActivePrinter     AS CHAR         NO-UNDO.
DEFINE VAR AdobePrinter         AS CHAR         NO-UNDO.
DEFINE VAR vcTemplateFile       AS CHAR    NO-UNDO.
DEFINE VARIABLE chExcelApplication   AS COM-HANDLE   NO-UNDO.
define var WshNetwork           as com-handle.
DEFINE VARIABLE chFile          AS CHAR NO-UNDO.
DEFINE VARIABLE chWorkBook  AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE chWorksheet AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE CurrDir AS CHAR NO-UNDO.
DEF VAR CommandString AS CHAR NO-UNDO.
DEF VAR LvCtr as int no-undo.
DEF VAR v-inst AS CHAR FORMAT "X(80)" EXTENT 5 NO-UNDO.
DEF VAR v-tmp-lines AS DEC NO-UNDO.
DEF VAR lv-got-return AS INT NO-UNDO.
DEF VAR v-prev-note-rec AS RECID NO-UNDO.
DEF VAR v-prev-extent AS INT NO-UNDO.
DEF VAR lv-line-chars AS INT INIT 60 NO-UNDO.
DEF VAR sch-rel-count AS INT NO-UNDO.
DEF VAR file-text AS CHAR NO-UNDO.
DEF VAR v-wid-frac AS CHAR NO-UNDO.
DEF VAR v-len-frac AS CHAR NO-UNDO.
DEF VAR v-dep-frac AS CHAR NO-UNDO.
DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.

file-text = "Order" + STRING(ford-no).

DEFINE NEW SHARED TEMP-TABLE tt-filelist
                       FIELD tt-FileCtr         AS INT
                       FIELD tt-FileName        AS CHAR
                       INDEX filelist           IS PRIMARY 
                             TT-FILECTR.

FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

IF AVAIL users AND users.user_program[2] NE "" THEN
   v-dir = users.user_program[2] + "\".
ELSE
   v-dir = "c:\tmp\".

run InitializeExcel.
run MainLoop.
run Cleanup.

PROCEDURE FillData:

for each report where report.term-id eq v-term-id no-lock,
    FIRST oe-ord WHERE RECID(oe-ord) EQ report.rec-id
    BREAK BY oe-ord.ord-no:

  IF FIRST-OF(oe-ord.ord-no) then
  DO:
     viWorkSheetCount = viWorkSheetCount + 1.

     IF viWorkSheetCount GT 1 THEN
        chWorkbook:WorkSheets(1):COPY(chExcelApplication:Sheets:item(1)) NO-ERROR.
  END.
END.

ASSIGN
   viWorkSheetCount = 0.

for each report where report.term-id eq v-term-id no-lock,
    FIRST oe-ord WHERE RECID(oe-ord) EQ report.rec-id
    BREAK BY oe-ord.ord-no:

  if FIRST-OF(oe-ord.ord-no) then do:
    
    viWorkSheetCount = viWorkSheetCount + 1.

    /* Go to the Active Sheet. */
    chWorkbook:WorkSheets(viWorkSheetCount):Activate no-error.

    ASSIGN
      chWorkSheet = chExcelApplication:Sheets:item(viWorkSheetCount)
      chWorkSheet:name = STRING(oe-ord.ord-no)
      chExcelApplication:activeSheet:PageSetup:PrintArea = "$A$1:$L$223"
      chWorkSheet:Range("L2"):VALUE = oe-ord.ord-no
      chWorkSheet:Range("L58"):VALUE = oe-ord.ord-no
      chWorkSheet:Range("L114"):VALUE = oe-ord.ord-no
      chWorkSheet:Range("L170"):VALUE = oe-ord.ord-no

      chWorkSheet:Range("D2"):VALUE = oe-ord.cust-no
      chWorkSheet:Range("D58"):VALUE = oe-ord.cust-no
      chWorkSheet:Range("D114"):VALUE = oe-ord.cust-no
      chWorkSheet:Range("D170"):VALUE = oe-ord.cust-no

      chWorkSheet:Range("C3"):VALUE = oe-ord.cust-name
      chWorkSheet:Range("C59"):VALUE = oe-ord.cust-name
      chWorkSheet:Range("C115"):VALUE = oe-ord.cust-name
      chWorkSheet:Range("C171"):VALUE = oe-ord.cust-name

      chWorkSheet:Range("C4"):VALUE = oe-ord.addr[1]
      chWorkSheet:Range("C60"):VALUE = oe-ord.addr[1]
      chWorkSheet:Range("C116"):VALUE = oe-ord.addr[1]
      chWorkSheet:Range("C172"):VALUE = oe-ord.addr[1]

      chWorkSheet:Range("C5"):VALUE = oe-ord.addr[2]
      chWorkSheet:Range("C61"):VALUE = oe-ord.addr[2]
      chWorkSheet:Range("C117"):VALUE = oe-ord.addr[2]
      chWorkSheet:Range("C173"):VALUE = oe-ord.addr[2]

      chWorkSheet:Range("C6"):VALUE = TRIM(oe-ord.city) + ", "
                                    + TRIM(oe-ord.state) + " " + oe-ord.zip
      chWorkSheet:Range("C62"):VALUE = chWorkSheet:Range("C6"):VALUE
      chWorkSheet:Range("C118"):VALUE = chWorkSheet:Range("C6"):VALUE
      chWorkSheet:Range("C174"):VALUE = chWorkSheet:Range("C6"):VALUE
      
      chWorkSheet:Range("I5"):VALUE = oe-ord.sold-name
      chWorkSheet:Range("I61"):VALUE = oe-ord.sold-name
      chWorkSheet:Range("I117"):VALUE = oe-ord.sold-name
      chWorkSheet:Range("I173"):VALUE = oe-ord.sold-name

      chWorkSheet:Range("I6"):VALUE = oe-ord.sold-addr[1]
      chWorkSheet:Range("I62"):VALUE = oe-ord.sold-addr[1]
      chWorkSheet:Range("I118"):VALUE = oe-ord.sold-addr[1]
      chWorkSheet:Range("I174"):VALUE = oe-ord.sold-addr[1]

      chWorkSheet:Range("I7"):VALUE = oe-ord.sold-addr[2]
      chWorkSheet:Range("I63"):VALUE = oe-ord.sold-addr[2]
      chWorkSheet:Range("I119"):VALUE = oe-ord.sold-addr[2]
      chWorkSheet:Range("I175"):VALUE = oe-ord.sold-addr[2]

      chWorkSheet:Range("I8"):VALUE = TRIM(oe-ord.sold-city) + ", "
                                    + TRIM(oe-ord.sold-state)
                                    + " " + oe-ord.sold-zip
      chWorkSheet:Range("I64"):VALUE = chWorkSheet:Range("I8"):VALUE
      chWorkSheet:Range("I120"):VALUE = chWorkSheet:Range("I8"):VALUE
      chWorkSheet:Range("I176"):VALUE = chWorkSheet:Range("I8"):VALUE

      chWorkSheet:Range("B12"):VALUE = oe-ord.ord-date
      chWorkSheet:Range("B68"):VALUE = oe-ord.ord-date
      chWorkSheet:Range("B124"):VALUE = oe-ord.ord-date
      chWorkSheet:Range("B180"):VALUE = oe-ord.ord-date

      chWorkSheet:Range("D12"):VALUE = oe-ord.po-no
      chWorkSheet:Range("D68"):VALUE = oe-ord.po-no
      chWorkSheet:Range("D124"):VALUE = oe-ord.po-no
      chWorkSheet:Range("D180"):VALUE = oe-ord.po-no

      chWorkSheet:Range("F12"):VALUE = oe-ord.due-date
      chWorkSheet:Range("F68"):VALUE = oe-ord.due-date
      chWorkSheet:Range("F124"):VALUE = oe-ord.due-date
      chWorkSheet:Range("F180"):VALUE = oe-ord.due-date

      chWorkSheet:Range("K12"):VALUE = oe-ord.terms-d
      chWorkSheet:Range("K68"):VALUE = oe-ord.terms-d
      chWorkSheet:Range("K124"):VALUE = oe-ord.terms-d
      chWorkSheet:Range("K180"):VALUE = oe-ord.terms-d

      chWorkSheet:Range("B14"):VALUE = oe-ord.over-pct
      chWorkSheet:Range("B70"):VALUE = oe-ord.over-pct
      chWorkSheet:Range("B126"):VALUE = oe-ord.over-pct
      chWorkSheet:Range("B182"):VALUE = oe-ord.over-pct.

    FIND FIRST carrier WHERE
         carrier.company EQ cocode AND
         carrier.loc     EQ locode AND
         carrier.carrier EQ oe-ord.carrier
         NO-LOCK NO-ERROR.

    IF AVAIL carrier THEN
       ASSIGN
          chWorkSheet:Range("I4"):VALUE = carrier.dscr
          chWorkSheet:Range("I60"):VALUE = carrier.dscr
          chWorkSheet:Range("I116"):VALUE = carrier.dscr
          chWorkSheet:Range("I172"):VALUE = carrier.dscr.

    IF oe-ord.fob-code EQ "DEST" THEN
       ASSIGN
          chWorkSheet:Range("B8"):VALUE = TRUE
          chWorkSheet:Range("B64"):VALUE = TRUE
          chWorkSheet:Range("B120"):VALUE = TRUE
          chWorkSheet:Range("B176"):VALUE = TRUE.
    ELSE
       ASSIGN
          chWorkSheet:Range("B9"):VALUE = TRUE
          chWorkSheet:Range("B65"):VALUE = TRUE
          chWorkSheet:Range("B121"):VALUE = TRUE
          chWorkSheet:Range("B177"):VALUE = TRUE.

    CASE oe-ord.frt-pay:
    
        WHEN "P" THEN
           ASSIGN
              chWorkSheet:Range("D8"):VALUE = TRUE
              chWorkSheet:Range("D64"):VALUE = TRUE
              chWorkSheet:Range("D120"):VALUE = TRUE
              chWorkSheet:Range("D176"):VALUE = TRUE.
        WHEN "C" THEN
           ASSIGN
              chWorkSheet:Range("D9"):VALUE = TRUE
              chWorkSheet:Range("D65"):VALUE = TRUE
              chWorkSheet:Range("D121"):VALUE = TRUE
              chWorkSheet:Range("D177"):VALUE = TRUE.
        WHEN "B" THEN
           ASSIGN
              chWorkSheet:Range("E9"):VALUE = TRUE
              chWorkSheet:Range("E65"):VALUE = TRUE
              chWorkSheet:Range("E121"):VALUE = TRUE
              chWorkSheet:Range("E177"):VALUE = TRUE.
        WHEN "T" THEN
           ASSIGN
              chWorkSheet:Range("E8"):VALUE = TRUE
              chWorkSheet:Range("E64"):VALUE = TRUE
              chWorkSheet:Range("E120"):VALUE = TRUE
              chWorkSheet:Range("E176"):VALUE = TRUE.
    END CASE.

    FIND FIRST oe-ordl WHERE
         oe-ordl.company EQ cocode AND
         oe-ordl.ord-no EQ oe-ord.ord-no AND
         oe-ordl.LINE EQ 1
         NO-LOCK NO-ERROR.

    IF AVAIL oe-ordl THEN
    DO:
       ASSIGN
          chWorkSheet:Range("G12"):VALUE = IF LOOKUP(oe-ordl.type-code,"O,N") GT 0 THEN "N"
                                           ELSE "R"
          chWorkSheet:Range("G68"):VALUE = chWorkSheet:Range("G12"):VALUE
          chWorkSheet:Range("G124"):VALUE = chWorkSheet:Range("G12"):VALUE
          chWorkSheet:Range("G180"):VALUE = chWorkSheet:Range("G12"):VALUE

          chWorkSheet:Range("I12"):VALUE = oe-ordl.s-man[1]
          chWorkSheet:Range("I68"):VALUE = oe-ordl.s-man[1]
          chWorkSheet:Range("I124"):VALUE = oe-ordl.s-man[1]
          chWorkSheet:Range("I180"):VALUE = oe-ordl.s-man[1]

          chWorkSheet:Range("J12"):VALUE = oe-ordl.s-comm[1]
          chWorkSheet:Range("J68"):VALUE = oe-ordl.s-comm[1]
          chWorkSheet:Range("J124"):VALUE = oe-ordl.s-comm[1]
          chWorkSheet:Range("J180"):VALUE = oe-ordl.s-comm[1]

          chWorkSheet:Range("C14"):VALUE = oe-ordl.qty
          chWorkSheet:Range("C70"):VALUE = oe-ordl.qty
          chWorkSheet:Range("C126"):VALUE = oe-ordl.qty
          chWorkSheet:Range("C182"):VALUE = oe-ordl.qty

          chWorkSheet:Range("D14"):VALUE = oe-ordl.i-name
          chWorkSheet:Range("D70"):VALUE = oe-ordl.i-name
          chWorkSheet:Range("D126"):VALUE = oe-ordl.i-name
          chWorkSheet:Range("D182"):VALUE = oe-ordl.i-name

          chWorkSheet:Range("K14"):VALUE = oe-ordl.price
          chWorkSheet:Range("K70"):VALUE = oe-ordl.price
          chWorkSheet:Range("K126"):VALUE = oe-ordl.price
          chWorkSheet:Range("K182"):VALUE = oe-ordl.price

          chWorkSheet:Range("L14"):VALUE = oe-ordl.pr-uom
          chWorkSheet:Range("L70"):VALUE = oe-ordl.pr-uom
          chWorkSheet:Range("L126"):VALUE = oe-ordl.pr-uom
          chWorkSheet:Range("L182"):VALUE = oe-ordl.pr-uom

          chWorkSheet:Range("D20"):VALUE = oe-ordl.part-dscr1
          chWorkSheet:Range("D76"):VALUE = oe-ordl.part-dscr1
          chWorkSheet:Range("D132"):VALUE = oe-ordl.part-dscr1
          chWorkSheet:Range("D188"):VALUE = oe-ordl.part-dscr1

          chWorkSheet:Range("D21"):VALUE = oe-ordl.part-dscr2
          chWorkSheet:Range("D77"):VALUE = oe-ordl.part-dscr2
          chWorkSheet:Range("D133"):VALUE = oe-ordl.part-dscr2
          chWorkSheet:Range("D189"):VALUE = oe-ordl.part-dscr2.

       IF oe-ordl.type-code EQ "O" THEN
          ASSIGN
             chWorkSheet:Range("F2"):VALUE = TRUE
             chWorkSheet:Range("F58"):VALUE = TRUE
             chWorkSheet:Range("F114"):VALUE = TRUE
             chWorkSheet:Range("F170"):VALUE = TRUE.

       ELSE IF LOOKUP(oe-ordl.type-code,"C,R") GT 0 THEN
          ASSIGN
             chWorkSheet:Range("H2"):VALUE = TRUE
             chWorkSheet:Range("H58"):VALUE = TRUE
             chWorkSheet:Range("H114"):VALUE = TRUE
             chWorkSheet:Range("H170"):VALUE = TRUE.

       FIND FIRST itemfg WHERE
            itemfg.company EQ cocode AND
            itemfg.i-no EQ oe-ordl.i-no
            NO-LOCK NO-ERROR.

       IF AVAIL itemfg THEN
          ASSIGN
             chWorkSheet:Range("H12"):VALUE = itemfg.procat
             chWorkSheet:Range("H68"):VALUE = itemfg.procat
             chWorkSheet:Range("H124"):VALUE = itemfg.procat
             chWorkSheet:Range("H180"):VALUE = itemfg.procat
             chWorkSheet:Range("H14"):VALUE = itemfg.style
             chWorkSheet:Range("H70"):VALUE = itemfg.style
             chWorkSheet:Range("H126"):VALUE = itemfg.style
             chWorkSheet:Range("H182"):VALUE = itemfg.style.

       FIND FIRST eb WHERE
            eb.company EQ cocode AND
            eb.est-no EQ oe-ordl.est-no AND
            eb.form-no EQ oe-ordl.form-no AND
            eb.stock-no EQ oe-ordl.i-no
            NO-LOCK NO-ERROR.

       IF AVAIL eb THEN
       DO:
          RUN sys\inc\decfrac2.p(INPUT eb.wid,
                                 INPUT 32, OUTPUT v-wid-frac).
          
          IF v-wid-frac EQ "" THEN
             v-wid-frac = "0".

          RUN sys\inc\decfrac2.p(INPUT eb.len,
                                 INPUT 32, OUTPUT v-len-frac).

          IF v-len-frac EQ "" THEN
             v-len-frac = "0".

          RUN sys\inc\decfrac2.p(INPUT eb.dep,
                                 INPUT 32, OUTPUT v-dep-frac).
       
          IF v-dep-frac EQ "" THEN
             v-dep-frac = "0".

          ASSIGN
             chWorkSheet:Range("D16"):VALUE = v-len-frac + " X " + v-wid-frac
                                            + " X " + v-dep-frac
             chWorkSheet:Range("D72"):VALUE = chWorkSheet:Range("D16"):VALUE
             chWorkSheet:Range("D128"):VALUE = chWorkSheet:Range("D16"):VALUE
             chWorkSheet:Range("D184"):VALUE = chWorkSheet:Range("D16"):VALUE.

          FIND FIRST ef WHERE
               ef.company EQ cocode AND
               ef.est-no EQ eb.est-no AND
               ef.form-no EQ eb.form-no
               NO-LOCK NO-ERROR.

          IF AVAIL ef THEN
             ASSIGN
                chWorkSheet:Range("D18"):VALUE = ef.board
                chWorkSheet:Range("D74"):VALUE = ef.board
                chWorkSheet:Range("D130"):VALUE = ef.board
                chWorkSheet:Range("D186"):VALUE = ef.board.
       END.

       IF oe-ordl.po-no-po NE 0 THEN
       DO:
          FIND FIRST po-ord WHERE
               po-ord.company EQ cocode AND
               po-ord.po-no EQ po-no-po
               NO-LOCK NO-ERROR.
       
          IF AVAIL po-ord THEN
          DO:
             ASSIGN
                chWorkSheet:Range("E28"):VALUE = po-ord.vend-no
                chWorkSheet:Range("E84"):VALUE = po-ord.vend-no
                chWorkSheet:Range("E140"):VALUE = po-ord.vend-no
                chWorkSheet:Range("E196"):VALUE = po-ord.vend-no.
             
             FIND FIRST po-ordl WHERE
                  po-ordl.company EQ cocode AND
                  po-ordl.po-no EQ po-ord.po-no AND
                  po-ordl.i-no  EQ oe-ordl.i-no
                  NO-LOCK NO-ERROR.
       
             IF AVAIL po-ordl THEN
                ASSIGN
                   chWorkSheet:Range("I28"):VALUE = po-ordl.cost
                   chWorkSheet:Range("I84"):VALUE = po-ordl.cost
                   chWorkSheet:Range("I140"):VALUE = po-ordl.cost
                   chWorkSheet:Range("I196"):VALUE = po-ordl.cost.
          END.
       END.

       /*Ink*/
       FOR EACH job-mat FIELDS(rm-i-no) WHERE
           job-mat.company EQ cocode AND
           job-mat.rm-i-no GT "" AND
           job-mat.job-no EQ oe-ordl.job-no AND
           job-mat.job-no2 EQ oe-ordl.job-no2
           NO-LOCK,
           FIRST ITEM FIELDS(i-name) WHERE
                 ITEM.company EQ cocode AND
                 ITEM.i-no EQ job-mat.rm-i-no AND
                 ITEM.mat-type EQ "I"
                 NO-LOCK:

           ASSIGN
              chWorkSheet:Range("H18"):VALUE = ITEM.i-name
              chWorkSheet:Range("H74"):VALUE = ITEM.i-name
              chWorkSheet:Range("H130"):VALUE = ITEM.i-name
              chWorkSheet:Range("H186"):VALUE = ITEM.i-name.

           LEAVE.
       END.

       sch-rel-count = 0.

       FOR EACH oe-rel FIELDS(tot-qty rel-date) WHERE
           oe-rel.company EQ oe-ordl.company AND
           oe-rel.ord-no  EQ oe-ordl.ord-no AND
           oe-rel.i-no    EQ oe-ordl.i-no AND
           oe-rel.line    EQ oe-ordl.line AND
           oe-rel.link-no EQ 0
           NO-LOCK:

           sch-rel-count = sch-rel-count + 1.

           IF sch-rel-count EQ 5 THEN
              LEAVE.

           IF sch-rel-count EQ 1 THEN
              ASSIGN
                 chWorkSheet:Range("D41"):VALUE = STRING(oe-rel.tot-qty) + "    " + STRING(oe-rel.rel-date)
                 chWorkSheet:Range("D97"):VALUE = chWorkSheet:Range("D41"):VALUE
                 chWorkSheet:Range("D153"):VALUE = chWorkSheet:Range("D41"):VALUE
                 chWorkSheet:Range("D209"):VALUE = chWorkSheet:Range("D41"):VALUE.
           ELSE IF sch-rel-count EQ 2 THEN
              ASSIGN
                 chWorkSheet:Range("D42"):VALUE = STRING(oe-rel.tot-qty) + "    " + STRING(oe-rel.rel-date)
                 chWorkSheet:Range("D98"):VALUE = chWorkSheet:Range("D42"):VALUE
                 chWorkSheet:Range("D154"):VALUE = chWorkSheet:Range("D42"):VALUE
                 chWorkSheet:Range("D210"):VALUE = chWorkSheet:Range("D42"):VALUE.
           ELSE IF sch-rel-count EQ 3 THEN
              ASSIGN
                 chWorkSheet:Range("D43"):VALUE = STRING(oe-rel.tot-qty) + "    " + STRING(oe-rel.rel-date)
                 chWorkSheet:Range("D99"):VALUE = chWorkSheet:Range("D43"):VALUE
                 chWorkSheet:Range("D155"):VALUE = chWorkSheet:Range("D43"):VALUE
                 chWorkSheet:Range("D211"):VALUE = chWorkSheet:Range("D43"):VALUE.
           ELSE
              ASSIGN
                 chWorkSheet:Range("D44"):VALUE = STRING(oe-rel.tot-qty) + "    " + STRING(oe-rel.rel-date)
                 chWorkSheet:Range("D100"):VALUE = chWorkSheet:Range("D44"):VALUE
                 chWorkSheet:Range("D156"):VALUE = chWorkSheet:Range("D44"):VALUE
                 chWorkSheet:Range("D212"):VALUE = chWorkSheet:Range("D44"):VALUE.
       END.

       FOR EACH oe-ordm FIELDS(charge amt) WHERE
           oe-ordm.company EQ cocode AND
           oe-ordm.ord-no EQ oe-ordl.ord-no
           NO-LOCK,
           FIRST prep WHERE
                 prep.company EQ cocode AND
                 prep.loc EQ locode AND
                 prep.CODE EQ oe-ordm.charge AND
                 prep.mat-type EQ "P"
                 NO-LOCK:

           ASSIGN
              chWorkSheet:Range("I41"):VALUE = TRUE
              chWorkSheet:Range("I97"):VALUE = TRUE
              chWorkSheet:Range("I153"):VALUE = TRUE
              chWorkSheet:Range("I209"):VALUE = TRUE
              chWorkSheet:Range("K41"):VALUE = oe-ordm.amt
              chWorkSheet:Range("K97"):VALUE = oe-ordm.amt
              chWorkSheet:Range("K153"):VALUE = oe-ordm.amt
              chWorkSheet:Range("K209"):VALUE = oe-ordm.amt.

           LEAVE.
       END.

       FOR EACH oe-ordm FIELDS(charge amt) WHERE
           oe-ordm.company EQ cocode AND
           oe-ordm.ord-no EQ oe-ordl.ord-no
           NO-LOCK,
           FIRST prep WHERE
                 prep.company EQ cocode AND
                 prep.loc EQ locode AND
                 prep.CODE EQ oe-ordm.charge AND
                 prep.mat-type EQ "D"
                 NO-LOCK:

           ASSIGN
             chWorkSheet:Range("I42"):VALUE = TRUE
             chWorkSheet:Range("I98"):VALUE = TRUE
             chWorkSheet:Range("I154"):VALUE = TRUE
             chWorkSheet:Range("I210"):VALUE = TRUE
             chWorkSheet:Range("K42"):VALUE = oe-ordm.amt
             chWorkSheet:Range("K98"):VALUE = oe-ordm.amt
             chWorkSheet:Range("K154"):VALUE = oe-ordm.amt
             chWorkSheet:Range("K210"):VALUE = oe-ordm.amt.

           LEAVE.
       END.

       FOR EACH oe-ordm FIELDS(charge amt) WHERE
           oe-ordm.company EQ cocode AND
           oe-ordm.ord-no EQ oe-ordl.ord-no
           NO-LOCK,
           FIRST prep WHERE
                 prep.company EQ cocode AND
                 prep.loc EQ locode AND
                 prep.CODE EQ oe-ordm.charge AND
                 prep.mat-type EQ "M"
                 NO-LOCK:

           ASSIGN
             chWorkSheet:Range("I43"):VALUE = TRUE
             chWorkSheet:Range("I99"):VALUE = TRUE
             chWorkSheet:Range("I155"):VALUE = TRUE
             chWorkSheet:Range("I211"):VALUE = TRUE
             chWorkSheet:Range("K43"):VALUE = oe-ordm.amt
             chWorkSheet:Range("K99"):VALUE = oe-ordm.amt
             chWorkSheet:Range("K155"):VALUE = oe-ordm.amt
             chWorkSheet:Range("K211"):VALUE = oe-ordm.amt.
           LEAVE.
       END.

       IF oe-ord.f-bill THEN
          ASSIGN
            chWorkSheet:Range("I44"):VALUE = TRUE
            chWorkSheet:Range("I100"):VALUE = TRUE
            chWorkSheet:Range("I156"):VALUE = TRUE
            chWorkSheet:Range("I212"):VALUE = TRUE
            chWorkSheet:Range("K44"):VALUE = oe-ord.t-freight
            chWorkSheet:Range("K100"):VALUE = oe-ord.t-freight
            chWorkSheet:Range("K156"):VALUE = oe-ord.t-freight
            chWorkSheet:Range("K212"):VALUE = oe-ord.t-freight.

       DO i = 2 TO 4:
          FIND FIRST oe-ordl WHERE
               oe-ordl.company EQ cocode AND
               oe-ordl.ord-no EQ oe-ord.ord-no AND
               oe-ordl.LINE EQ i
               NO-LOCK NO-ERROR.

          IF AVAIL oe-ordl THEN
          DO:
             FIND FIRST itemfg WHERE
                  itemfg.company EQ cocode AND
                  itemfg.i-no EQ oe-ordl.i-no
                  NO-LOCK NO-ERROR.

             FIND FIRST eb WHERE
                  eb.company EQ cocode AND
                  eb.est-no EQ oe-ordl.est-no AND
                  eb.form-no EQ oe-ordl.form-no AND
                  eb.stock-no EQ oe-ordl.i-no
                  NO-LOCK NO-ERROR.

             CASE i:
                 
                WHEN 2 THEN
                DO:
                   ASSIGN
                     chWorkSheet:Range("B29"):VALUE = oe-ord.over-pct
                     chWorkSheet:Range("B85"):VALUE = oe-ord.over-pct
                     chWorkSheet:Range("B141"):VALUE = oe-ord.over-pct
                     chWorkSheet:Range("B197"):VALUE = oe-ord.over-pct

                     chWorkSheet:Range("C29"):VALUE = oe-ordl.qty
                     chWorkSheet:Range("C85"):VALUE = oe-ordl.qty
                     chWorkSheet:Range("C141"):VALUE = oe-ordl.qty
                     chWorkSheet:Range("C197"):VALUE = oe-ordl.qty

                     chWorkSheet:Range("D29"):VALUE = oe-ordl.i-name
                     chWorkSheet:Range("D85"):VALUE = oe-ordl.i-name
                     chWorkSheet:Range("D141"):VALUE = oe-ordl.i-name
                     chWorkSheet:Range("D197"):VALUE = oe-ordl.i-name.
               
                   IF AVAIL itemfg THEN
                      ASSIGN
                         chWorkSheet:Range("G29"):VALUE = itemfg.style
                         chWorkSheet:Range("G85"):VALUE = itemfg.style
                         chWorkSheet:Range("G141"):VALUE = itemfg.style
                         chWorkSheet:Range("G197"):VALUE = itemfg.style.
               
                   IF AVAIL eb THEN
                   DO:
                      RUN sys\inc\decfrac2.p(INPUT eb.wid,
                                             INPUT 32, OUTPUT v-wid-frac).

                      IF v-wid-frac EQ "" THEN
                         v-wid-frac = "0".

                      RUN sys\inc\decfrac2.p(INPUT eb.len,
                                             INPUT 32, OUTPUT v-len-frac).

                      IF v-len-frac EQ "" THEN
                         v-len-frac = "0".

                      RUN sys\inc\decfrac2.p(INPUT eb.dep,
                                             INPUT 32, OUTPUT v-dep-frac).

                      IF v-dep-frac EQ "" THEN
                         v-dep-frac = "0".

                      ASSIGN
                         chWorkSheet:Range("D30"):VALUE = v-len-frac + " X "
                                                        + v-wid-frac + " X "
                                                        + v-dep-frac
                         chWorkSheet:Range("D86"):VALUE = chWorkSheet:Range("D30"):VALUE
                         chWorkSheet:Range("D142"):VALUE = chWorkSheet:Range("D30"):VALUE
                         chWorkSheet:Range("D198"):VALUE = chWorkSheet:Range("D30"):VALUE.

                      FIND FIRST ef WHERE
                           ef.company EQ cocode AND
                           ef.est-no EQ oe-ordl.est-no AND
                           ef.form-no EQ eb.form-no
                           NO-LOCK NO-ERROR.

                      IF AVAIL ef THEN
                         ASSIGN
                           chWorkSheet:Range("G30"):VALUE = ef.board
                           chWorkSheet:Range("G86"):VALUE = ef.board
                           chWorkSheet:Range("G142"):VALUE = ef.board
                           chWorkSheet:Range("G198"):VALUE = ef.board.
                   END.
                END.
               
                WHEN 3 THEN
                DO:
                   ASSIGN
                     chWorkSheet:Range("B31"):VALUE = oe-ord.over-pct
                     chWorkSheet:Range("B87"):VALUE = oe-ord.over-pct
                     chWorkSheet:Range("B143"):VALUE = oe-ord.over-pct
                     chWorkSheet:Range("B199"):VALUE = oe-ord.over-pct

                     chWorkSheet:Range("C31"):VALUE = oe-ordl.qty
                     chWorkSheet:Range("C87"):VALUE = oe-ordl.qty
                     chWorkSheet:Range("C143"):VALUE = oe-ordl.qty
                     chWorkSheet:Range("C199"):VALUE = oe-ordl.qty

                     chWorkSheet:Range("D31"):VALUE = oe-ordl.i-name
                     chWorkSheet:Range("D87"):VALUE = oe-ordl.i-name
                     chWorkSheet:Range("D143"):VALUE = oe-ordl.i-name
                     chWorkSheet:Range("D199"):VALUE = oe-ordl.i-name.
               
                   IF AVAIL itemfg THEN
                      ASSIGN
                        chWorkSheet:Range("G31"):VALUE = itemfg.style
                        chWorkSheet:Range("G87"):VALUE = itemfg.style
                        chWorkSheet:Range("G143"):VALUE = itemfg.style
                        chWorkSheet:Range("G199"):VALUE = itemfg.style.
               
                   IF AVAIL eb THEN
                   DO:
                      RUN sys\inc\decfrac2.p(INPUT eb.wid,
                                             INPUT 32, OUTPUT v-wid-frac).

                      IF v-wid-frac EQ "" THEN
                         v-wid-frac = "0".

                      RUN sys\inc\decfrac2.p(INPUT eb.len,
                                             INPUT 32, OUTPUT v-len-frac).

                      IF v-len-frac EQ "" THEN
                         v-len-frac = "0".

                      RUN sys\inc\decfrac2.p(INPUT eb.dep,
                                             INPUT 32, OUTPUT v-dep-frac).

                      IF v-dep-frac EQ "" THEN
                         v-dep-frac = "0".

                      ASSIGN
                        chWorkSheet:Range("D32"):VALUE = v-len-frac + " X " + v-wid-frac
                                                       + " X " + v-dep-frac
                        chWorkSheet:Range("D88"):VALUE = chWorkSheet:Range("D32"):VALUE
                        chWorkSheet:Range("D143"):VALUE = chWorkSheet:Range("D32"):VALUE
                        chWorkSheet:Range("D199"):VALUE = chWorkSheet:Range("D32"):VALUE.

                      FIND FIRST ef WHERE
                           ef.company EQ cocode AND
                           ef.est-no EQ oe-ordl.est-no AND
                           ef.form-no EQ eb.form-no
                           NO-LOCK NO-ERROR.

                      IF AVAIL ef THEN
                         ASSIGN
                            chWorkSheet:Range("G30"):VALUE = ef.board
                            chWorkSheet:Range("G86"):VALUE = ef.board
                            chWorkSheet:Range("G142"):VALUE = ef.board
                            chWorkSheet:Range("G198"):VALUE = ef.board.
                   END.
                END.
               
                WHEN 4 THEN
                DO:
                   ASSIGN
                     chWorkSheet:Range("B33"):VALUE = oe-ord.over-pct
                     chWorkSheet:Range("B89"):VALUE = oe-ord.over-pct
                     chWorkSheet:Range("B145"):VALUE = oe-ord.over-pct
                     chWorkSheet:Range("B201"):VALUE = oe-ord.over-pct

                     chWorkSheet:Range("C33"):VALUE = oe-ordl.qty
                     chWorkSheet:Range("C89"):VALUE = oe-ordl.qty
                     chWorkSheet:Range("C145"):VALUE = oe-ordl.qty
                     chWorkSheet:Range("C201"):VALUE = oe-ordl.qty

                     chWorkSheet:Range("D33"):VALUE = oe-ordl.i-name
                     chWorkSheet:Range("D89"):VALUE = oe-ordl.i-name
                     chWorkSheet:Range("D145"):VALUE = oe-ordl.i-name
                     chWorkSheet:Range("D201"):VALUE = oe-ordl.i-name.
               
                   IF AVAIL itemfg THEN
                      ASSIGN
                         chWorkSheet:Range("G33"):VALUE = itemfg.style
                         chWorkSheet:Range("G89"):VALUE = itemfg.style
                         chWorkSheet:Range("G145"):VALUE = itemfg.style
                         chWorkSheet:Range("G201"):VALUE = itemfg.style.
               
                   IF AVAIL eb THEN
                   DO:
                      RUN sys\inc\decfrac2.p(INPUT eb.wid,
                                             INPUT 32, OUTPUT v-wid-frac).

                      IF v-wid-frac EQ "" THEN
                         v-wid-frac = "0".

                      RUN sys\inc\decfrac2.p(INPUT eb.len,
                                             INPUT 32, OUTPUT v-len-frac).

                      IF v-len-frac EQ "" THEN
                         v-len-frac = "0".

                      RUN sys\inc\decfrac2.p(INPUT eb.dep,
                                             INPUT 32, OUTPUT v-dep-frac).

                      IF v-dep-frac EQ "" THEN
                         v-dep-frac = "0".

                      ASSIGN
                        chWorkSheet:Range("D34"):VALUE = v-len-frac + " X "
                                                       + v-wid-frac + " X "
                                                       + v-dep-frac
                        chWorkSheet:Range("D90"):VALUE = chWorkSheet:Range("D34"):VALUE
                        chWorkSheet:Range("D146"):VALUE = chWorkSheet:Range("D34"):VALUE
                        chWorkSheet:Range("D202"):VALUE = chWorkSheet:Range("D34"):VALUE.

                      FIND FIRST ef WHERE
                           ef.company EQ cocode AND
                           ef.est-no EQ oe-ordl.est-no AND
                           ef.form-no EQ eb.form-no
                           NO-LOCK NO-ERROR.

                      IF AVAIL ef THEN
                         ASSIGN
                           chWorkSheet:Range("G30"):VALUE = ef.board
                           chWorkSheet:Range("G86"):VALUE = ef.board
                           chWorkSheet:Range("G142"):VALUE = ef.board
                           chWorkSheet:Range("G198"):VALUE = ef.board.
                   END.
                END.
             END CASE.
          END.
       END.

    END. /*avail oe-ordl*/

    /*Special Instructions*/

    {custom/notespr2.i oe-ord v-inst 5 "notes.rec_key = oe-ord.rec_key AND notes.note_code = 'SI'"}

    ASSIGN
       chWorkSheet:Range("D23"):VALUE = v-inst[1]
       chWorkSheet:Range("D79"):VALUE = v-inst[1]
       chWorkSheet:Range("D135"):VALUE = v-inst[1]
       chWorkSheet:Range("D191"):VALUE = v-inst[1]

       chWorkSheet:Range("D24"):VALUE = v-inst[2]
       chWorkSheet:Range("D80"):VALUE = v-inst[2]
       chWorkSheet:Range("D136"):VALUE = v-inst[2]
       chWorkSheet:Range("D192"):VALUE = v-inst[2]

       chWorkSheet:Range("D25"):VALUE = v-inst[3]
       chWorkSheet:Range("D81"):VALUE = v-inst[3]
       chWorkSheet:Range("D137"):VALUE = v-inst[3]
       chWorkSheet:Range("D193"):VALUE = v-inst[3]

       chWorkSheet:Range("D26"):VALUE = v-inst[4]
       chWorkSheet:Range("D82"):VALUE = v-inst[4]
       chWorkSheet:Range("D138"):VALUE = v-inst[4]
       chWorkSheet:Range("D194"):VALUE = v-inst[4]

       chWorkSheet:Range("D27"):VALUE = v-inst[5]
       chWorkSheet:Range("D83"):VALUE = v-inst[5]
       chWorkSheet:Range("D139"):VALUE = v-inst[5]
       chWorkSheet:Range("D195"):VALUE = v-inst[5].
  end. /*first-of oe-ord */

end. /* for each report */

chWorkbook:WorkSheets(1):Activate no-error.

OS-DELETE value(v-dir + file-text + ".xls").     
OS-DELETE value(v-dir + "asi.pdf").
OS-DELETE value(v-dir + file-text + ".pdf").

IF LvOutputSelection = "PRINTER" THEN
DO:
   NO-RETURN-VALUE chWorkbook:PrintOut(,,,,,False,).
   chWorkbook:Close(no) no-error.
END.
ELSE IF LvOutputSelection = "Email" THEN
DO:
   chExcelApplication:ActiveSheet:SaveAs(v-dir + file-text + ".xls") no-error. 	   
   NO-RETURN-VALUE chWorkbook:PrintOut(,,,,,False,). 
   chWorkbook:Close(no) no-error.   
   chExcelApplication:Quit() no-error.
   pause 3.
   OS-DELETE VALUE(v-dir + file-text + ".xls").
   OS-RENAME VALUE(v-dir + "asi.pdf") VALUE(v-dir + file-text + ".pdf").
   LvCtr = LvCtr + 1.
   CREATE tt-filelist.
   ASSIGN tt-FileCtr  = LvCtr
          tt-FileName = v-dir + file-text + ".pdf".
END.

END PROCEDURE. /* FillData*/

PROCEDURE InitializeExcel:

   /* Capture the current active printer. */
  IF LvOutputSelection = "Email" THEN
    assign 
      CurActivePrinter = SESSION:PRINTER-NAME
      AdobePrinter     = "PDFcamp Printer".
  
  vcTemplateFile   = "template\ackalbert.xlt".

  /* Connect to the running Excel session. */
  CREATE "Excel.Application" chExcelApplication connect no-error.

  /* If Excel is running close it. */
  if valid-handle (chExcelApplication) then
  do:
    chExcelApplication:Quit()         no-error.
    run CleanUp.
  end.


  /* Network connection checks. */
  CREATE "WScript.Network" WshNetwork NO-ERROR.
  IF NOT(VALID-HANDLE(WshNetwork)) THEN
  DO :
    MESSAGE "Unable to Create Wscript.Network" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
  
  /* Switch Printer to PDFCamp Printer. */
  IF LvOutputSelection = "Email" THEN
     WshNetwork:SetDefaultPrinter(AdobePrinter).

  /* Start a new session of Excel. */
  /*if not (valid-handle (chExcelApplication)) THEN*/
    CREATE "Excel.Application" chExcelApplication NO-ERROR.
  
  /* Check if Excel got initialized. */
  IF not (valid-handle (chExcelApplication)) THEN
  DO :
    MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
  
  FILE-INFO:FILE-NAME = vcTemplateFile.

  /* Set the Excel Template to be used. */
  ASSIGN chFile = search (FILE-INFO:FULL-PATHNAME) no-error.
  
  if search (chFile) = ? then do:
    MESSAGE 'Template File: ' FILE-INFO:FULL-PATHNAME
            'cannot be found. Please verify that the file exists.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    apply 'CLOSE':U to this-procedure.
  end.

  /* Make Excel visible. */
  ASSIGN
     chFile = FILE-INFO:FULL-PATHNAME
     chExcelApplication:VISIBLE = IF LvOutputSelection = "Email" or 
                                     LvOutputSelection = "Printer" THEN  FALSE
                                  ELSE TRUE.
  
  /* Clear tt-FileList. */
  empty temp-table tt-filelist.

END PROCEDURE.

PROCEDURE MainLoop:

  /* Open our Excel Template. */  
  assign chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.
  
  /* Do not display Excel error messages. */
  chExcelApplication:DisplayAlerts = false  no-error.

  /* Disable screen updating so it will go faster */
  chExcelApplication:ScreenUpdating = False.

  /* Go to the Active Sheet. */
  chWorkbook:WorkSheets(1):Activate no-error.
  chWorkSheet      = chExcelApplication:Sheets:item(1).

  /*Fill in Data*/
  run FillData.

  /* enable screen updating */
  chExcelApplication:ScreenUpdating = TRUE.
END PROCEDURE.

PROCEDURE CleanUp:

    /* RELEASE OBJECTS */
  RELEASE OBJECT chWorkbook         NO-ERROR.
  RELEASE OBJECT chWorkSheet        NO-ERROR.

  /* Reset the Active Printer to the Original Printer. */
  if CurActivePrinter <> '' AND
     VALID-HANDLE(WshNetwork) then
     WshNetwork:SetDefaultPrinter(CurActivePrinter).

  /* For E-mail and Printer jobs, close Excel. */
  IF LvOutputSelection = "PRINTER" OR 
     LvOutputSelection = "EMAIL" THEN
    chExcelApplication:Quit() no-error.
  
  /* Release created objects. */
  RELEASE OBJECT WshNetwork         NO-ERROR.
  RELEASE OBJECT chExcelApplication NO-ERROR.
END PROCEDURE.
