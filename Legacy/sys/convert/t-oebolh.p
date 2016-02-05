/* t-oebolh.p  Load oe-bolh record and assign release# */
DEF VAR li-next-release AS INT NO-UNDO.
/*
FOR EACH oe-bolh:
    DELETE oe-bolh.
END.
*/

INPUT FROM k:\hop\hopdump\oe-bolh.d NO-ECHO.
li-next-release = 1.
REPEAT :
    create oe-bolh.
    SET
       oe-bolh.company                         
       oe-bolh.loc                             
       oe-bolh.ship-no                         
       oe-bolh.b-no                            
       oe-bolh.bol-date                        
       oe-bolh.cust-no                         
       oe-bolh.ord-no                          
       oe-bolh.po-no                           
       oe-bolh.rel-no                          
       oe-bolh.rel-date                        
       oe-bolh.carrier                         
       oe-bolh.trailer                         
       oe-bolh.posted                          
       oe-bolh.w-ord                           
       oe-bolh.ship-date                       
       oe-bolh.tot-qty                         
       oe-bolh.ship-i                          
       oe-bolh.b-ord-no                        
       oe-bolh.printed                         
       oe-bolh.deleted                         
       oe-bolh.bol-no                          
       oe-bolh.r-no                            
       oe-bolh.tot-wt    FORM ">>>>>>>9"                      
       oe-bolh.freight                         
       oe-bolh.cwt                             
       oe-bolh.INV-NO                          
       oe-bolh.ship-id                         
       oe-bolh.tot-pallets  FORM ">>>>,>>9"                   
       oe-bolh.sold-id                         
       oe-bolh.sold-no                         
       oe-bolh.master-bol-no                   
       oe-bolh.master-bol-printed              
       oe-bolh.user-id                         
       oe-bolh.frt-pay                         
       /*  new fields in gui
       oe-bolh.rec_key                         
       oe-bolh.bol-status                      
       oe-bolh.upd-date                        
       oe-bolh.upd-time                        
       oe-bolh.stat                            
       oe-bolh.ship-time                       
       oe-bolh.release#                        
       */
        .

    oe-bolh.po-no = "".  /* not to have posting problem */

    FIND FIRST oe-relh WHERE oe-relh.r-no = oe-bolh.r-no NO-LOCK NO-ERROR.
    li-next-release = IF AVAIL oe-relh THEN oe-relh.release# ELSE li-next-release.

    IF NOT AVAIL oe-relh THEN li-next-release = li-next-release + 1.
    oe-bolh.RELEASE# = li-next-release.

    DISP oe-bolh.b-no oe-bolh.bol-no oe-bolh.release#
          li-next-release WITH FRAME dis DOWN.
    PAUSE 0.

END.
