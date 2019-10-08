/* asn-eb.i  assign eb record from rfq & rfqitem 
   02/13/01  YSK
	Last change:  YSK  21 Feb 2001    4:52 pm
*/

 assign eb.adhesive = rfqitem.adhesive                                      
        eb.cad-no          = rfqitem.cad-no                               
        eb.carr-dscr       = rfqitem.carr-dscr                               
        eb.carrier         = rfqitem.carrier                                
        eb.cas-cnt         = rfqitem.cas-cnt                               
        eb.cas-cost        = rfqitem.cas-cost               
        eb.cas-dep         = rfqitem.cas-dep             
        eb.cas-len         = rfqitem.cas-len              
        eb.cas-no          = rfqitem.cas-no              
        eb.cas-pal         = rfqitem.cas-pal               
        eb.cas-wid         = rfqitem.cas-wid              
        eb.cas-wt          = rfqitem.cas-wt              
        eb.chg-method = substring(rfq.chg-method,1,1)                                    
        eb.comm      = rfq.comm                                     
        eb.company   = est.company                                     
        eb.cust-no   = rfq.cust                                     
        eb.dep       = rfqitem.dep                                        
        eb.die-in    = rfqitem.die-in                                     
        eb.die-no    = rfqitem.die-no                                     
        eb.dust      = rfqitem.dust                                     
        eb.e-num     = est.e-num                                     
        eb.est-no    = est.est-no                                     
        eb.est-type  = est.est-type                                     
        eb.flute     = rfqitem.flute                                      
        eb.fpanel    = rfqitem.fpanel                                     
        eb.form-no   = li-form-no                                     
        eb.blank-no  = li-blank-no                               
        eb.eqty = ef.eqty /*rfqitem.qty[1]*/
        .
 IF est.est-type > 4 THEN
 DO:
   assign eb.gluelap   = rfqitem.gluelap
          eb.i-coat    = rfqitem.i-coat
          eb.i-col     = rfqitem.i-col
          eb.i-pass    = rfqitem.i-pass
          eb.i-coldscr = rfqitem.i-coldscr.
   
   DO viCount = 1 TO MIN(EXTENT(eb.i-%),EXTENT(rfqitem.i-%)):
      ASSIGN
        eb.i-%[viCount]    = rfqitem.i-%[viCount]
        eb.i-code[viCount] = rfqitem.i-code[viCount]
        eb.i-dscr[viCount] = rfqitem.i-dscr[viCount]
        eb.i-ps[viCount]   = rfqitem.i-ps[viCount].
   END.

   DO viCount = 1 TO MIN(EXTENT(eb.k-wid-array2),EXTENT(rfqitem.k-wid-array2)):
      ASSIGN
        eb.k-wid-array2[viCount] = rfqitem.k-wid-array2[viCount]
        eb.k-len-array2[viCount] = rfqitem.k-len-array2[viCount]
        eb.k-wid-scr-type2[viCount] = rfqitem.k-wid-scr-type2[viCount]
        eb.k-len-scr-type2[viCount] = rfqitem.k-len-scr-type2[viCount].
   END.
 END.
 ELSE
 DO:
    assign
       eb.gluelap   = rfqitem.gluelap
       eb.i-coat    = rfqitem.i-coat
       eb.i-col     = rfqitem.i-col
       eb.i-coldscr = rfqitem.i-coldscr
       eb.i-pass    = rfqitem.i-pass.

    DO viCount = 1 TO MIN(EXTENT(eb.i-%2),EXTENT(rfqitem.i-%)):
      ASSIGN
        eb.i-%2[viCount]    = rfqitem.i-%[viCount]
        eb.i-code2[viCount] = rfqitem.i-code[viCount]
        eb.i-dscr2[viCount] = rfqitem.i-dscr[viCount]
        eb.i-ps2[viCount]   = rfqitem.i-ps[viCount].
    END.
 END.

 assign eb.k-len     = rfqitem.k-len                                     
        eb.k-wid     = rfqitem.k-wid                                     
        eb.len       = rfqitem.len                                     
        eb.lin-in    = rfqitem.lin-in                                     
        eb.loc       = est.loc                                     
        eb.lock      = rfqitem.lock                                     
        eb.part-dscr1 = rfqitem.i-name     /* rfqitem.part-dscr1 */
        eb.part-dscr2 = rfqitem.part-dscr1 /* rfqitem.part-dscr2 */
        eb.part-no    = rfqitem.part-no                                    
        eb.plate-no   = rfqitem.plate-no                   
        eb.procat     = rfqitem.procat                                     
        eb.ship-addr[1]  = rfq.ship-addr[1]                                    
        eb.ship-addr[2]  = rfq.ship-addr[2]
        eb.ship-city     = rfq.ship-city                                  
        eb.ship-id       = rfqitem.ship-id                    
        eb.ship-name     = rfq.ship-name                                 
        eb.ship-state    = rfq.ship-state                
        eb.ship-zip      = rfq.ship-zip                    
        eb.sman          = rfq.sman               
        eb.spc-no        = rfqitem.spc-no                                 
        eb.stock-no      = rfqitem.stock-no
        eb.style         = rfqitem.style               
        eb.test          = rfqitem.test                 
        eb.tr-cas        = rfqitem.tr-cas                   
        eb.tr-cnt        = rfqitem.tr-cnt                 
        eb.tr-cost       = rfqitem.tr-cost                 
        eb.tr-dep        = rfqitem.tr-dep                
        eb.tr-len        = rfqitem.tr-len                 
        eb.tr-no         = rfqitem.tr-no                 
        eb.tr-wid        = rfqitem.tr-wid                  
        eb.tuck          = rfqitem.tuck                 
        eb.upc-no        = rfqitem.upc-no                   
        eb.weight-m      = rfqitem.weight-m                 
        eb.wid           = rfqitem.wid
        eb.t-len         = rfqitem.t-len
        eb.t-sqin        = rfqitem.t-sqin
        eb.t-wid         = rfqitem.t-wid
        eb.bl-qty = rfqitem.qty[1]
        eb.yld-qty       = 1 /*rfqitem.qty[1] */                   
        eb.dest-code     = ls-dest-code
        eb.i-coat-p  = IF eb.i-coat EQ 0 THEN 0
                       ELSE 1
        .
/*
        eb.t-win         = rfqitem.t-win
        eb.t-dep         = rfqitem.t-dep
eb.i-coat-p  = rfqitem.i-coat-p                    
eb.cust-seq        
eb.cust-%                       
  
eb.disc-rate = rfqitem.disc-rate                                     
eb.est-int   = 
eb.fr-out-c  = rfqitem.fr-out-c                       
eb.fr-out-m  = rfqitem.fr-out-m                   
eb.i-code2   = rfqitem.i-code2             
eb.i-%2      = rfqitem.i-%2                        
eb.i-dscr2   = rfqitem.i-dscr   
eb.i-ps2     = rfqitem.i-ps2                                     
eb.k-len-array  = rfqitem.k-len-array
eb.k-wid-array  = rfqitem.k-wid-array                    
eb.n-ply     = rfqitem.n-ply
eb.num-dep   = rfqitem.num-dep                      
eb.num-len   = rfqitem.num-len                    
eb.num-up    = rfqitem.num-up                    
eb.num-wid   = rfqitem.num-wid                     
eb.ship-no       = rfqitem.ship-no                                 
eb.stack-code    = rfqitem.stack-code                                 
eb.stacks        = rfqitem.stacks                                 
eb.sty-lock      = rfqitem.sty-lock
eb.tab-in        = rfqitem.tab-in
eb.yrprice       = rfqitem.yrprice        

*/

