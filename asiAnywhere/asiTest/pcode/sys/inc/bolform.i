/*{sys/inc/bolform.i}*/
   
   DEFINE INPUT PARAMETER icFormName AS CHAR NO-UNDO.

   ASSIGN
     v-print-mode = ""
     is-xprint-form = NO
     lv-prt-bypass = NO.

   CASE icFormName:
       WHEN "1/2 page" THEN
          ASSIGN
             v-program = "oe/rep/bolhalfp.p"
             lines-per-page = 44.
       WHEN "Royal" THEN
          ASSIGN
             v-program = "oe/rep/bolroyal.p"
             lines-per-page = 56.
       WHEN "ContSrvc" THEN
          ASSIGN
             v-program = "oe/rep/bol-csc.p"
             lines-per-page = 66.
       WHEN "HOP" THEN
          ASSIGN
             v-program = "oe/rep/bolhop.p"
             lines-per-page = 39.
       WHEN "Superior" THEN
          ASSIGN
             v-program = "oe/rep/bolsuper.p"
             lines-per-page = 55.
       WHEN "Premier" THEN
          ASSIGN
             v-program = "oe/rep/bolprem.p"
             lines-per-page = 55.
       WHEN "PremierX" THEN
          ASSIGN
             v-program      = "oe/rep/bolpremx.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "TrilakesX" THEN
          ASSIGN
             v-program      = "oe/rep/boltrilx.p"
             is-xprint-form = YES
             lines-per-page = 80.      
       WHEN "RFC" THEN
          ASSIGN
             v-program = "oe/rep/bolrfc.p"
             lines-per-page = 55.
       WHEN "Sonoco" THEN
          ASSIGN
             v-program = "oe/rep/bolsonoc.p"
             lines-per-page = 51.
       WHEN "Warren" THEN
          ASSIGN
             v-program = "oe/rep/bolwarrn.p"
             lines-per-page = 59.
       WHEN "PAC 1/2" THEN
          ASSIGN
             v-program = "oe/rep/bolpack.p"
             lines-per-page = 44.
       WHEN "Imperial" THEN
          ASSIGN
             v-program = "oe/rep/bolimp.p"
             lines-per-page = 60.
       WHEN "P&P" THEN
          ASSIGN
             v-program = "oe/rep/bolpnp.p"
             lines-per-page = 62.
       WHEN "TriLakes" THEN
          ASSIGN
             v-program = "oe/rep/boltril.p"
             lines-per-page = 62.
       WHEN "Triad" THEN
          ASSIGN
             v-program = "oe/rep/boltriad.p"
             lines-per-page = 62.
       WHEN "TriState" THEN
          ASSIGN
             v-program = "oe/rep/boltrist.p"
             lines-per-page = 41.
       WHEN "BlueRidg" THEN
          ASSIGN
             v-program = "oe/rep/bolbluer.p"
             lines-per-page = 65.
       WHEN "Danbury" THEN
          ASSIGN
             v-program = "oe/rep/boldnbry.p"
             lines-per-page = 42.
       WHEN "Boxtech" THEN
          ASSIGN
             v-program = "oe/rep/bolboxt.p"
             lines-per-page = 55
             lv-prt-bypass = YES.
       WHEN "Empire" THEN
          ASSIGN
             v-program = "oe/rep/bolempir.p"
             lines-per-page = 54.
       WHEN "Herman" THEN
          ASSIGN
             v-program = "oe/rep/bolhermn.p"
             lines-per-page = 54.
       WHEN "pacific" THEN
          ASSIGN
             v-program      = "oe/rep/bolpacif.p"
             is-xprint-form = YES
             lines-per-page = 80.
       WHEN "century" THEN
          ASSIGN
             v-program = "oe/rep/bolcentx.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "ccc" THEN
          ASSIGN
             v-program = "oe/rep/bolccc.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "Xprint" THEN
          ASSIGN
             v-program = "oe/rep/bolxprnt.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "CapCityIN" THEN
          ASSIGN
             v-program      = "oe/rep/bolcapcin.p"
             is-xprint-form = YES
             lines-per-page = 66.      
       WHEN "FibreCI" THEN
          ASSIGN
             v-program      = "oe/rep/bolfibci.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "APC" THEN ASSIGN v-program = "oe/rep/bolxapc.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "P&PX" THEN ASSIGN v-program = "oe/rep/bolpnpx.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "Xprint2" THEN ASSIGN v-program = "oe/rep/bolxprt2.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "CSCIN" THEN ASSIGN v-program = "oe/rep/bolcscin.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "SouthPak" THEN ASSIGN v-program = "oe/rep/bolsouth.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "SouthPak-XL" THEN ASSIGN v-program = "oe/rep/bolsouth-xl.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "MWBox" THEN ASSIGN v-program = "oe/rep/bolmwbox.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "Hughes" THEN ASSIGN v-program = "oe/rep/bolhughs.p" is-xprint-form = YES lines-per-page = 64.
       WHEN "Inland" THEN ASSIGN v-program = "oe/rep/bolxinld.p" is-xprint-form = YES lines-per-page = 80.
       WHEN "concepts" THEN ASSIGN v-program = "oe/rep/bolxcorc.p" is-xprint-form = YES lines-per-page = 80.
       WHEN "Brick" THEN
       DO:
          IF v-headers THEN
             ASSIGN
                v-program = "oe/rep/bolbrck1.p"
                lines-per-page = 64.
          ELSE
             ASSIGN
                v-program = "oe/rep/bolbrick.p"
                lines-per-page = 60.
       END.
       WHEN "AllPkg" THEN
          ASSIGN
             v-program = "oe/rep/bolallpk.p"
             lines-per-page = 60.
       WHEN "Fibre" THEN
          ASSIGN
             v-program = "oe/rep/bolfibre.p"
             lines-per-page = 55. /*gdm 0506095 - ADJUSTED FROM 57*/
       WHEN "MaxPak" THEN
          ASSIGN
             v-program = "oe/rep/bolmaxpk.p"
             lines-per-page = 42.
       WHEN "Oracle" THEN
          ASSIGN
             v-program = "oe/rep/bolora2.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "Frankstn" OR WHEN "Mirpkg" THEN
          ASSIGN
             v-program = "oe/rep/bolfrank.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "PPI" THEN
          ASSIGN
             v-program = "oe/rep/bolppi.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "Indiana" THEN
          ASSIGN
             v-program = "oe/rep/bolindc.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "Ottpkg" THEN
          ASSIGN
             v-program = "oe/rep/bolottpk.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "ConsBox" THEN
          ASSIGN
             v-program = "oe/rep/bolcnsbx.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "Harwell" THEN
          ASSIGN
             v-program = "oe/rep/bolharwl.p"
             lines-per-page = 62.
       WHEN "Chillic" THEN
          ASSIGN 
             v-program = "oe/rep/bolchill.p"
             lines-per-page = 60.
       WHEN "Midwest" THEN
          ASSIGN 
             v-program = "oe/rep/bolmdwst.p"
             lines-per-page = 52.
       WHEN "Intrpack" THEN
          ASSIGN 
             v-program = "oe/rep/bolinter.p"
             lines-per-page = 56.
       WHEN "Dayton" THEN
          ASSIGN
             v-program = "oe/rep/boldaytn.p"
             lines-per-page = 57.
       WHEN "Elite" THEN
          ASSIGN
             v-program      = "oe/rep/bolelite.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Fibrex" THEN
          ASSIGN
             v-program      = "oe/rep/bolfibrex.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Michcor" THEN
          ASSIGN
             v-program = "oe/rep/bolmich.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Express" THEN
          ASSIGN
             v-program = "oe/rep/bolexprs.p"
             lines-per-page = 62.
       WHEN "Hamilton" THEN
          ASSIGN
             v-program = "oe/rep/bolhamil.p"
             lines-per-page = 44.
       WHEN "Allwest" THEN
          ASSIGN
             v-program      = "oe/rep/bolallws.p"                                      
             is-xprint-form = YES
             lines-per-page = 66.      
      
       WHEN "COLOR" THEN  /* gdm - 07140907 */
          ASSIGN v-program =  "oe/rep/bolcolor.p" 
                 is-xprint-form = YES 
                 lines-per-page = 66.
    
       WHEN "Badger" THEN /* gdm - 08270906 */
          ASSIGN
             v-program = "oe/rep/bolbadgr.p"                                 
             is-xprint-form = YES
             lines-per-page = 66.

       WHEN "Loylang" THEN  /* gdm - 10200901 */
          ASSIGN
             v-program      = "oe/rep/bolloyln.p"
             is-xprint-form = YES
             lines-per-page = 66.      

       WHEN "Carded" THEN   /* gdm - 11170903 */
          ASSIGN
             v-program      = "oe/rep/bolcard.p"
             is-xprint-form = YES
             lines-per-page = 66.

            WHEN "Metro" THEN
          ASSIGN
             v-program = "oe/rep/bolmet.p"
             is-xprint-form = YES
             lines-per-page = 64.

       OTHERWISE
          ASSIGN
             v-print-mode   = "PROD"
             v-program      = "oe/rep/oe-lad" +
                              (if v-print-fmt eq "c" then "c" else "s") + ".p"
             lines-per-page = 62.
   END CASE.
  
  v-lines-per-page = lines-per-page.
