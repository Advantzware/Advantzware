/* ---------------------------------------------- oe/rep/bolempir.i 12/99 FWK */
/* PRINT Empire BOL                                                           */
/* -------------------------------------------------------------------------- */

for each report where report.term-id eq v-term-id,

    first oe-boll where recid(oe-boll) eq report.rec-id,

    first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,

    first itemfg
    where itemfg.company eq oe-boll.company
      and itemfg.i-no    eq oe-boll.i-no
    no-lock

    break by report.key-01 /* oe-boll.i-no*/
          :

find first oe-ordl where oe-ordl.company eq cocode 
       and  oe-ordl.ord-no  eq int(report.key-02)
       and oe-ordl.i-no    eq report.key-01
       no-lock no-error.
   
   ASSIGN
        v-shipqty  = v-shipqty + oe-boll.qty
        v-ordqty   = v-ordqty + oe-ordl.qty
        v-untcount = v-untcount + oe-boll.cases
        v-backord  = v-backord + itemfg.q-back.
     
      IF LAST-OF(report.key-01) THEN DO:
          ASSIGN
           v-tot-part = oe-ordl.part-no
           v-tot-desc = oe-ordl.part-dscr1
           v-tot-unittype = oe-ordl.pr-uom.

           PUT {1}
              v-tot-part FORM "x(15)" SPACE(1)
               "<C13>" v-tot-desc   FORM "x(20)" "<C13>"
               "<C41>" v-tot-unittype "<C41>"
               "<C47>" v-ordqty   "<C47>"
               "<C56>" v-untcount "<C56>"
               "<C64>" v-shipqty "<C64>"
               "<C73>" v-backord  "<C73>"
               SKIP. 

     /* rstark 05181205 */
     XMLLineNumber = XMLLineNumber + 1.
     RUN XMLOutput (lXMLOutput,'BOLLine_' + STRING(XMLLineNumber),'','Row').
     RUN XMLOutput (lXMLOutput,'Column_1',v-tot-part,'Col').
     RUN XMLOutput (lXMLOutput,'Column_2',v-tot-desc,'Col').
     RUN XMLOutput (lXMLOutput,'Column_3',v-tot-unittype,'Col').
     RUN XMLOutput (lXMLOutput,'Column_4',v-ordqty,'Col').
     RUN XMLOutput (lXMLOutput,'Column_5',v-untcount,'Col').
     RUN XMLOutput (lXMLOutput,'Column_6',v-shipqty,'Col').
     RUN XMLOutput (lXMLOutput,'Column_7',v-backord,'Col').
     RUN XMLOutput (lXMLOutput,'/BOLLine_' + STRING(XMLLineNumber),'','Row').
     /* rstark 05181205 */

    ASSIGN
         v-ordqty = 0
         v-shipqty  = 0
         v-backord  = 0
         v-untcount = 0.

      v-printline = v-printline + 1.
      END.

  IF v-printline >= 50 THEN DO:
     v-printline = 0.
     PAGE {1}.
     {oe/rep/bollamarpkg2.i}
  END.

end. /* for each report */

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
