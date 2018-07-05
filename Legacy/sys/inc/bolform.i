   /*sys/inc/bolform.i*/
   
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
       WHEN "PremierX" or WHEN "PremierXFooter" THEN
          ASSIGN
             v-program      = "oe/rep/bolpremx.p"
             is-xprint-form = YES
             lines-per-page = 70.
       WHEN "RFCX" THEN
          ASSIGN
             v-program      = "oe/rep/bolrfcx.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "PremierBroker" THEN
          ASSIGN                                        /*Task# 01141406*/
             v-program      = "oe/rep/bolprmbr.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "PremierCX" THEN
          ASSIGN
             v-program      = "oe/rep/bolpremcx.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "PremierPX" THEN
          ASSIGN
             v-program      = "oe/rep/bolprempx.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "KDWILLSX" THEN
          ASSIGN
             v-program      = "oe/rep/bolkdwlx.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "TrilakesX" OR WHEN "TrilakesLot#" THEN
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
       WHEN "BlueRidg2" THEN
          ASSIGN
             v-program = "oe/rep/bolbluer2.p"
             lines-per-page = 50.
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
       WHEN "ccc" OR  WHEN "CCCWPP" THEN 
          ASSIGN
             v-program = "oe/rep/bolccc.p"
             is-xprint-form = YES
             lines-per-page = 74.
       WHEN "Xprint" OR WHEN "bolfmt 1" THEN
          ASSIGN
             v-program = "oe/rep/bolxprnt.p"
             is-xprint-form = YES
             lines-per-page = 80.
       WHEN "bolfmt 10" THEN
          ASSIGN
             v-program = "oe/rep/bolxprnt10.p"
             is-xprint-form = YES
             lines-per-page = 80.
       WHEN "Wingate-BOL" THEN
          ASSIGN
             v-program = "oe/rep/bolwinget.p"
             is-xprint-form = YES
             lines-per-page = 80.
        WHEN "bolfmt10-CAN" THEN
          ASSIGN
             v-program = "oe/rep/bolxprnt10can.p"
             is-xprint-form = YES
             lines-per-page = 80. 
        WHEN "Coburn" THEN
          ASSIGN
             v-program = "oe/rep/bolcobrn.p"
             is-xprint-form = YES
             lines-per-page = 66.
        WHEN "Lakeside" THEN
          ASSIGN
             v-program = "oe/rep/bollake.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Multicell" THEN
          ASSIGN
             v-program = "oe/rep/bolmcell.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Accordbc" THEN
          ASSIGN
             v-program = "oe/rep/bolacrdbc.p"
             is-xprint-form = YES
             lines-per-page = 80.
       WHEN "PackRite" THEN
          ASSIGN
             v-program = "oe/rep/bolprite.p"
             is-xprint-form = YES
             lines-per-page = 99.
       WHEN "MidwestX" THEN
          ASSIGN
             v-program = "oe/rep/bolmidx.p"
             is-xprint-form = YES
             lines-per-page = 99.
       WHEN "Lamar Pkg" THEN
          ASSIGN
             v-program = "oe/rep/bollamarpkg.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Protagon" THEN
          ASSIGN
             v-program = "oe/rep/bolprotg.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "CapCityIN" THEN
          ASSIGN
             v-program      = "oe/rep/bolcapcin.p"
             is-xprint-form = YES
             lines-per-page = 90.     
       WHEN "Axis" THEN
          ASSIGN
             v-program      = "oe/rep/bolaxis.p"
             is-xprint-form = YES
             lines-per-page = 90.  
       WHEN "FibreCI" THEN
          ASSIGN
             v-program      = "oe/rep/bolfibci.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "APC" THEN ASSIGN v-program = "oe/rep/bolxapc.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "P&PX" THEN ASSIGN v-program = "oe/rep/bolpnpx.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "Xprint2" OR WHEN "bolfmt 2" THEN ASSIGN v-program = "oe/rep/bolxprt2.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "bolfmt 20" THEN ASSIGN v-program = "oe/rep/bolxprt20.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "Chillicothe" THEN ASSIGN v-program = "oe/rep/bolchict.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "NSTOCK" THEN ASSIGN v-program = "oe/rep/bolnstok.p" is-xprint-form = YES lines-per-page = 80.
       WHEN "CSCIN" THEN ASSIGN v-program = "oe/rep/bolcscin.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "CSCINStamp" THEN ASSIGN v-program = "oe/rep/bolcstmp.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "SouthPak" THEN ASSIGN v-program = "oe/rep/bolsouth.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "SouthPak-XL" THEN ASSIGN v-program = "oe/rep/bolsouth-xl.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "Prystup-Excel" THEN ASSIGN v-program = "oe/rep/bolpryst-xl.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "MWBox" THEN ASSIGN v-program = "oe/rep/bolmwbox.p" is-xprint-form = YES lines-per-page = 66.
       WHEN "Hughes" THEN ASSIGN v-program = "oe/rep/bolhughm.p" is-xprint-form = YES lines-per-page = 70.
       WHEN "HughesTags" THEN ASSIGN v-program = "oe/rep/bolhughs.p" is-xprint-form = YES lines-per-page = 70.
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
       WHEN "NOSCO" THEN
          ASSIGN
             v-program = "oe/rep/bolkni.p"
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
       WHEN "DEE" THEN
          ASSIGN
             v-program = "oe/rep/boldee.p"
             is-xprint-form = YES
             lines-per-page = 64. 
       WHEN "ConsBox" THEN
          ASSIGN
             v-program = "oe/rep/bolcnsbx.p"
             is-xprint-form = YES
             lines-per-page = 64.
       WHEN "CapitolBC" THEN
          ASSIGN
             v-program = "oe/rep/bolcapbc.p"
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
       WHEN "ACPI" THEN
          ASSIGN
             v-program      = "oe/rep/bolacpi.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "CSC-GA" THEN
          ASSIGN
             v-program      = "oe/rep/bolcscga.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "Fibrex" THEN
          ASSIGN
             v-program      = "oe/rep/bolfibrex.p"
             is-xprint-form = YES
             lines-per-page = 66.
       WHEN "BOLfmt15" THEN
          ASSIGN
             v-program      = "oe/rep/bolfiftnx.p"
             is-xprint-form = YES
             lines-per-page = 66.
      WHEN "BOLFMTX15" THEN
          ASSIGN
             v-program      = "oe/rep/bolfrftn.p"
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
          ASSIGN v-program = "oe/rep/bolcolor.p" 
                 is-xprint-form = YES 
                 lines-per-page = 66.
       
       WHEN "ALLPKG2" THEN  
          ASSIGN v-program = "oe/rep/bolallpk2.p" 
                 is-xprint-form = YES 
                 lines-per-page = 66.
    
       WHEN "Badger" THEN /* gdm - 08270906 */
               ASSIGN
               v-program = "oe/rep/bolbadgr.p"                                 
               is-xprint-form = YES
               lines-per-page = 66.
          
       WHEN "BadgerSoldTo" THEN /* gdm - 09121404 */
               ASSIGN
               v-program = "oe/rep/bolbgrst.p"                                 
               is-xprint-form = YES
               lines-per-page = 66.
          
       WHEN "Loylang" THEN  /* gdm - 10200901 */
          ASSIGN
             v-program      = "oe/rep/bolloyln.p"
             is-xprint-form = YES
             lines-per-page = 66.    

       WHEN "Printers" THEN  /* gdm - 08031501 */
          ASSIGN
             v-program      = "oe/rep/bollprint.p"
             is-xprint-form = YES
             lines-per-page = 66. 

       WHEN "Printers2" THEN  /* gdm - 08031501 */
          ASSIGN
             v-program      = "oe/rep/bollprnt2.p"
             is-xprint-form = YES
             lines-per-page = 75. 

       WHEN "Carded" THEN   /* gdm - 11170903 */
          ASSIGN
             v-program      = "oe/rep/bolcard.p"
             is-xprint-form = YES
             lines-per-page = 66.

       WHEN "CardedBC" THEN   /* 08211406 */
          ASSIGN
             v-program      = "oe/rep/bolcrdbc.p"
             is-xprint-form = YES
             lines-per-page = 66.

       WHEN "Metro" THEN
          ASSIGN
             v-program = "oe/rep/bolmet.p"
             is-xprint-form = YES
             lines-per-page = 64.
      
       WHEN "MetroTags" THEN
          ASSIGN
             v-program = "oe/rep/bolmettg.p"
             is-xprint-form = YES
             lines-per-page = 64.

       WHEN "PEACHTREE" THEN  /* btr */ 
           ASSIGN v-program = "oe/rep/bolptree.p" 
                  is-xprint-form = YES 
                  lines-per-page = 80.

       WHEN "PeachTreeBC" THEN  /* btr */ 
           ASSIGN v-program = "oe/rep/bolptreebc.p" 
                  is-xprint-form = YES 
                  lines-per-page = 80.
       
       WHEN "PeachTreeLotPO" THEN  /* btr */ 
           ASSIGN v-program = "oe/rep/bolptreelot.p" 
                  is-xprint-form = YES 
                  lines-per-page = 80.
       
       WHEN "Soule" THEN  /* btr */ 
           ASSIGN v-program = "oe/rep/bolsoule.p" 
                  is-xprint-form = YES 
                  lines-per-page = 62.

         WHEN "SouleMed" THEN  /* btr */ 
           ASSIGN v-program = "oe/rep/bolsolmed.p" 
                  is-xprint-form = YES 
                  lines-per-page = 62.

       OTHERWISE
          ASSIGN
             v-print-mode   = "PROD"
             v-program      = "oe/rep/oe-lad" +
                              (if v-print-fmt eq "c" then "c" else "s") + ".p"
             lines-per-page = 62.
   END CASE.


  v-lines-per-page = lines-per-page.
