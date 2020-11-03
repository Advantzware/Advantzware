 
ASSIGN
     li-mm       = TRUNC(oe-bolh.upd-time / 60,0)
     li-hh       = TRUNC(li-mm / 60,0)
     li-mm       = li-mm - (li-hh * 60)
     li-ss       = oe-bolh.upd-time - (li-hh * 3600) - (li-mm * 60) 
     fi_upd-time = STRING(li-hh,"99") + ":" +
                   STRING(li-mm,"99") + ":" +
                   STRING(li-ss,"99") .
    
  ASSIGN
  chWorkSheet:Range("A1"):VALUE = string(today)
  chWorkSheet:Range("A3"):VALUE = v-comp-name
  chWorkSheet:Range("A4"):VALUE = v-comp-addr[1]
  chWorkSheet:Range("A5"):VALUE = v-comp-addr[2] .
 if v-comp-addr3 ne "" then
  chWorkSheet:Range("A6"):VALUE = v-comp-addr3 .
  
  /* Ship To */
  ASSIGN 
  chWorkSheet:Range("A8"):VALUE = v-ship-name
  chWorkSheet:Range("A9"):VALUE = v-ship-addr[1]
  chWorkSheet:Range("A10"):VALUE = v-ship-addr[2].
if v-ship-city ne "" or v-ship-state ne "" or v-ship-zip ne "" then 
  chWorkSheet:Range("A11"):VALUE = v-ship-city + ", " + v-ship-state + " " +  v-ship-zip .
  
 ASSIGN
   chWorkSheet:Range("O2"):VALUE = oe-bolh.bol-no .
if avail tt-temp-report then
   assign
   chWorkSheet:Range("N7"):VALUE = tt-temp-report.cCarrier 
   chWorkSheet:Range("N8"):VALUE = tt-temp-report.cTrailer
   chWorkSheet:Range("N9"):VALUE = tt-temp-report.key-01
   chWorkSheet:Range("M12"):VALUE = tt-temp-report.cScac
   chWorkSheet:Range("N13"):VALUE = tt-temp-report.cProName

   chWorkSheet:Range("A13"):VALUE = tt-temp-report.key-04
   chWorkSheet:Range("A14"):VALUE = tt-temp-report.key-05
   chWorkSheet:Range("A15"):VALUE = tt-temp-report.key-06 .
   if tt-temp-report.key-07 ne "" or tt-temp-report.key-08 ne "" or tt-temp-report.key-09 ne "" then
  chWorkSheet:Range("A16"):VALUE = tt-temp-report.key-07  + ", " + tt-temp-report.key-08 + " " + tt-temp-report.key-09 .
  chWorkSheet:Range("A18"):VALUE = tt-temp-report.cSpecInst.
  chWorkSheet:Range("J34"):VALUE = tt-temp-report.key-02.

  chWorkSheet:Range("N29"):VALUE = string(oe-bolh.upd-date) + " At " + fi_upd-time .
  chWorkSheet:Range("I39"):VALUE = string(oe-bolh.upd-date) + " At " + fi_upd-time .
  chWorkSheet:Range("T34"):VALUE = "".
  
  

   
  
 
