/* upd-eb.i  assign eb record from rfq & rfqitem 
   02/13/01  YSK
	Last change:  YSK  21 Feb 2001    4:52 pm
*/
 assign  /* eb.company   = est.company                                     
        eb.loc       = est.loc                                     
        eb.cust-no   = rfq.cust                                      
        eb.e-num     = est.e-num                                     
        eb.est-no    = est.est-no                                     
        eb.est-type  = est.est-type                                   
        */  
        eb.eqty = rfqitem.qty[1]
        eb.adhesive = rfqitem.adhesive                                      
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
        eb.dep       = rfqitem.dep                                        
        eb.die-in    = rfqitem.die-in                                     
        eb.die-no    = rfqitem.die-no                                     
        eb.dust      = rfqitem.dust                                     
        eb.flute     = rfqitem.flute                                      
        eb.fpanel    = rfqitem.fpanel                                     
/*        eb.form-no   = li-form-no                                     
        eb.blank-no  = li-blank-no                               
*/        
        .
IF est.est-type > 4 THEN
 assign eb.gluelap   = rfqitem.gluelap                                     
        eb.i-%[1]    = rfqitem.i-%[1]
        eb.i-%[2]    = rfqitem.i-%[2]
        eb.i-%[3]    = rfqitem.i-%[3]
        eb.i-%[4]    = rfqitem.i-%[4]
        eb.i-%[5]    = rfqitem.i-%[5]
        eb.i-%[6]    = rfqitem.i-%[6]
        eb.i-%[7]    = rfqitem.i-%[7]
        eb.i-%[8]    = rfqitem.i-%[8]
        eb.i-%[9]    = rfqitem.i-%[9]
        eb.i-%[10]    = rfqitem.i-%[10]
        eb.i-coat    = rfqitem.i-coat                                     
        eb.i-code[1] = rfqitem.i-code[1]
        eb.i-code[2] = rfqitem.i-code[2]
        eb.i-code[3] = rfqitem.i-code[3]
        eb.i-code[4] = rfqitem.i-code[4]
        eb.i-code[5] = rfqitem.i-code[5]
        eb.i-code[6] = rfqitem.i-code[6]
        eb.i-code[7] = rfqitem.i-code[7]
        eb.i-code[8] = rfqitem.i-code[8]
        eb.i-code[9] = rfqitem.i-code[9]
        eb.i-code[10] = rfqitem.i-code[10]
        eb.i-col     = rfqitem.i-col                                      
        eb.i-coldscr = rfqitem.i-coldscr                                     
        eb.i-dscr[1] = rfqitem.i-dscr[1]
        eb.i-dscr[2] = rfqitem.i-dscr[2]
        eb.i-dscr[3] = rfqitem.i-dscr[3]
        eb.i-dscr[4] = rfqitem.i-dscr[4]
        eb.i-dscr[5] = rfqitem.i-dscr[5]
        eb.i-dscr[6] = rfqitem.i-dscr[6]
        eb.i-dscr[7] = rfqitem.i-dscr[7]
        eb.i-dscr[8] = rfqitem.i-dscr[8]
        eb.i-dscr[9] = rfqitem.i-dscr[9]
        eb.i-dscr[10] = rfqitem.i-dscr[10]
        eb.i-pass    = rfqitem.i-pass                                     
        eb.i-ps[1]   = rfqitem.i-ps[1]
        eb.i-ps[2]   = rfqitem.i-ps[2]
        eb.i-ps[3]   = rfqitem.i-ps[3]
        eb.i-ps[4]   = rfqitem.i-ps[4]
        eb.i-ps[5]   = rfqitem.i-ps[5]
        eb.i-ps[6]   = rfqitem.i-ps[6]
        eb.i-ps[7]   = rfqitem.i-ps[7]
        eb.i-ps[8]   = rfqitem.i-ps[8]
        eb.i-ps[9]   = rfqitem.i-ps[9]
        eb.i-ps[10]   = rfqitem.i-ps[10]
        .
 ELSE  assign eb.gluelap   = rfqitem.gluelap                                     
        eb.i-%2[1]    = rfqitem.i-%[1]
        eb.i-%2[2]    = rfqitem.i-%[2]
        eb.i-%2[3]    = rfqitem.i-%[3]
        eb.i-%2[4]    = rfqitem.i-%[4]
        eb.i-%2[5]    = rfqitem.i-%[5]
        eb.i-%2[6]    = rfqitem.i-%[6]
        eb.i-%2[7]    = rfqitem.i-%[7]
        eb.i-%2[8]    = rfqitem.i-%[8]
        eb.i-%2[9]    = rfqitem.i-%[9]
        eb.i-%2[10]    = rfqitem.i-%[10]
        eb.i-coat    = rfqitem.i-coat                                     
        eb.i-code2[1] = rfqitem.i-code[1]
        eb.i-code2[2] = rfqitem.i-code[2]
        eb.i-code2[3] = rfqitem.i-code[3]
        eb.i-code2[4] = rfqitem.i-code[4]
        eb.i-code2[5] = rfqitem.i-code[5]
        eb.i-code2[6] = rfqitem.i-code[6]
        eb.i-code2[7] = rfqitem.i-code[7]
        eb.i-code2[8] = rfqitem.i-code[8]
        eb.i-code2[9] = rfqitem.i-code[9]
        eb.i-code2[10] = rfqitem.i-code[10]
        eb.i-col     = rfqitem.i-col                                      
        eb.i-coldscr = rfqitem.i-coldscr                                     
        eb.i-dscr2[1] = rfqitem.i-dscr[1]
        eb.i-dscr2[2] = rfqitem.i-dscr[2]
        eb.i-dscr2[3] = rfqitem.i-dscr[3]
        eb.i-dscr2[4] = rfqitem.i-dscr[4]
        eb.i-dscr2[5] = rfqitem.i-dscr[5]
        eb.i-dscr2[6] = rfqitem.i-dscr[6]
        eb.i-dscr2[7] = rfqitem.i-dscr[7]
        eb.i-dscr2[8] = rfqitem.i-dscr[8]
        eb.i-dscr2[9] = rfqitem.i-dscr[9]
        eb.i-dscr2[10] = rfqitem.i-dscr[10]
        eb.i-pass    = rfqitem.i-pass                                     
        eb.i-ps2[1]   = rfqitem.i-ps[1]
        eb.i-ps2[2]   = rfqitem.i-ps[2]
        eb.i-ps2[3]   = rfqitem.i-ps[3]
        eb.i-ps2[4]   = rfqitem.i-ps[4]
        eb.i-ps2[5]   = rfqitem.i-ps[5]
        eb.i-ps2[6]   = rfqitem.i-ps[6]
        eb.i-ps2[7]   = rfqitem.i-ps[7]
        eb.i-ps2[8]   = rfqitem.i-ps[8]
        eb.i-ps2[9]   = rfqitem.i-ps[9]
        eb.i-ps2[10]   = rfqitem.i-ps[10]
        .

 assign eb.k-len     = rfqitem.k-len                                     
        eb.k-wid     = rfqitem.k-wid                                     
        eb.len       = rfqitem.len                                     
        eb.lin-in    = rfqitem.lin-in                                     
        eb.lock      = rfqitem.lock                                     
        eb.part-dscr1 = rfqitem.part-dscr1                                     
        eb.part-dscr2 = rfqitem.part-dscr2                                    
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
        eb.i-coat-p  = IF eb.i-coat EQ 0 THEN 0
                       ELSE 1
        .
/*
        eb.t-win         = rfqitem.t-win
        eb.t-dep         = rfqitem.t-dep
eb.i-coat-p  = rfqitem.i-coat-p                    
eb.cust-seq        
eb.cust-%                       
eb.dest-code  
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

