 /* oe/rep/bolfibci.i  */ 
 v-row-line = 4.

 PUT unformatted
     "</PROGRESS><FCOURIER NEW><P10>"                        
     "<R" v-row-line "><C50><B>" v-company-name "</B>" SKIP
     "<R" v-row-line + 1 "><C50><FROM><C78><LINE><||3>"
     /*"<R" v-row-line + 1 "><C50><P9>COMPANY NAME<P10>"  SKIP*/
     "<R" v-row-line + 2 "><C50><B>" v-comp-addr1  "</B>"  SKIP
     "<R" v-row-line + 3 "><C50><FROM><C78><LINE><||3>"
    /* "<R" v-row-line + 3 "><C50><P9>COMPANY ADDRESS<P10>"  SKIP     */
     "<R" v-row-line + 4 "><C50><B>" v-comp-addr2  "</B>"  SKIP
     "<R" v-row-line + 4.5 "><C32><P18><B>COMMERCIAL INVOICE</B><P9>"   SKIP
     "<R" v-row-line + 6 "><C2><FROM><C78><LINE><||3>"    SKIP
     "<R" v-row-line + 6 "><C2>INTERNATIONAL"           SKIP
     /*"<R" v-row-line + 7 "><C12><FROM><R+2><C62><RECT><||3>"  /* TRACKING NUMBER BOX*/ */
     "<R" v-row-line + 7 "><C2>AIR WAY BILL NO."
     "<R" v-row-line + 9 "><C2><FROM><C78><LINE><||3>"      SKIP
     "<R" v-row-line + 9 "><C2>DATE OF EXPORTATION <C46>EXPORT REFERENCES(i.e. order #., invoice #)" SKIP

     "<R" v-row-line + 11 "><C2><FROM><C78><LINE><||3>" SKIP
     "<R" v-row-line + 11 "><C2>SHIPPER/EXPORTER <C46>CONSIGNEE(Complete Name and Address)" SKIP
     "<R" v-row-line + 12 "><C2>" v-ship-name  SKIP
     "<R" v-row-line + 13 "><C2>" v-ship-addr1 SKIP
     "<R" v-row-line + 14 "><C2>" v-ship-addr2 SKIP
     "<R" v-row-line + 15 "><C2>" v-ship-addr3 SKIP
     "<R" v-row-line + 17 "><C2><FROM><C80><LINE><||3>"  SKIP
     "<R" v-row-line + 17 "><C2>COUNTRY OF EXPORT <C46>IMPORTER - IF OTHER THAN CONSIGNEE" SKIP
     "<R" v-row-line + 18 "><C21><B><P14>U S A</B><P9>" SKIP
     "<R" v-row-line + 18 "><C46>(Complete Name and Address)" SKIP
     "<R" v-row-line + 20 "><C2><FROM><C45><LINE><||3>" SKIP
     "<R" v-row-line + 20 "><C2>COUNTRY OF MANUFACTURE" SKIP
     "<R" v-row-line + 20 "><C21><B><P14>U S A</B><P9>" SKIP
     "<R" v-row-line + 22 "><C2><FROM><C45><LINE><||3>"
     "<R" v-row-line + 22 "><C2>COUNTRY OF ULTIMATE DESTINATION"
     "<R" v-row-line + 25 "><C2><FROM><C80><LINE><||3>"
     "<R" v-row-line + 25 "><P9><C53>UNIT OF   WEIGHT  UNIT VALUE "
     "<R" v-row-line + 26 "><C2>ORDER# NO. OF  TYPE OF                                             MEASURE  PER CASE  PER EACH,   TOTAL"
     "<R" v-row-line + 27 "><C2>PO#    PKGS   PACKAGING      FULL DESCRIPTION OF GOODS        QTY   EACH    OR PALLET CASE,PALLET VALUE<P10>"
     "<R" v-row-line + 28 "><C2><FROM><C80><LINE><||3>"
     .
     /* line count */
 v-line-cnt = 0.
 FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK:
     v-line-cnt = v-line-cnt + 1.
 END.
 v-line-cnt * 2.
 PUT unformatted                           /*v-line-cnt + 7*/
     "<R" v-row-line + 25 "><C6.5><FROM><R" v-row-line + 47 "><C6.5><LINE><||3>" 
     "<R" v-row-line + 25 "><C12><FROM><R" v-row-line + 47 "><C12><LINE><||3>"
     "<R" v-row-line + 25 "><C21><FROM><R" v-row-line + 40 "><C21><LINE><||3>"
     "<R" v-row-line + 9 "><C45><FROM><R" v-row-line + 40 "><C45><LINE><||3>"
     "<R" v-row-line + 25 "><C52><FROM><R" v-row-line + 40 "><C52><LINE><||3>"
     "<R" v-row-line + 25 "><C58.5><FROM><R" v-row-line + 47 "><C58.5><LINE><||3>"
     "<R" v-row-line + 25 "><C66><FROM><R" v-row-line + 47 "><C66><LINE><||3>"
     "<R" v-row-line + 25 "><C75><FROM><R" v-row-line + 52 "><C75><LINE><||3>"
     "<R" v-row-line + 40 "><C1><FROM><C80><LINE><||3>"
     "<R" v-row-line + 43 "><C6.5><FROM><C12><LINE><||3>"
     "<R" v-row-line + 43 "><C58.5><FROM><C66><LINE><||3>"
     "<R" v-row-line + 43 "><C75><FROM><C80><LINE><||3>"
     "<R" v-row-line + 47 "><C6.5><FROM><C12><LINE><||3>"
     "<R" v-row-line + 47 "><C58.5><FROM><C66><LINE><||3>"
     "<R" v-row-line + 47 "><C75><FROM><C80><LINE><||3>"
     "<R" v-row-line + 52 "><C75><FROM><C80><LINE><||3>"
     "<R" v-row-line + 40 "><C7>TOTAL<C59>TOTAL<C76>TOTAL" SKIP
     "<R" v-row-line + 41 "><C7># OF<C59>WEIGHT<C75.3>INVOICE" SKIP
     "<R" v-row-line + 42 "><C7>PKGS.<C76>VALUE" SKIP
     "<R" v-row-line + 47 "><C75><P8>CHECK ONE<P9>" SKIP
     "<R" v-row-line + 48 "><C78>FOB"
     "<R" v-row-line + 48 "><C76><FROM><R+1><C+1><RECT> C&F"
     "<R" v-row-line + 49 "><C76><FROM><R+1><C+1><RECT> CIF"
     "<R" v-row-line + 50 "><C76><FROM><R+1><C+1><RECT> "
     .
  PUT unformatted
      /*"<R" v-row-line + 45 "><C15><P9>SEE REVERSE SIDE FOR HELP WITH ABOVE SECTION" SKIP */
      "<R" v-row-line + 48 "><C2>FOR U.S. EXPORT ONLY: THESE COMMODITIES, TECHNOLOGY, OR SOFTWARE WERE EXPORTED FROM THE" SKIP
      "<R" v-row-line + 49 "><C2>UNITED STATES IN ACCORDANCE WITH THE EXPORT ADMINISTRATION REGULATIONS. DIVERSION CONTRARY"  SKIP
      "<R" v-row-line + 50 "><C2>TO UNITED STATES LAW IS PROHIBITED." SKIP
      "<R" v-row-line + 52 "><C2>I DECLARE ALL THE INFORMATION CONTAINED IN THIS INVOICE TO BE TRUE AND CORRECT." SKIP
      "<R" v-row-line + 54 "><C2>SIGNATURE OF SHIPPER/EXPORTER <C60>DATE" SKIP
      "<R" v-row-line + 57 "><C2><FROM><C50><LINE><||3>"
      "<R" v-row-line + 57 "><C60><FROM><C80><LINE><||3>"
      .
