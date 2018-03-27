/* asn-efs.i  assign ef record  for set record
   02/13/01  YSK
	Last change:  YSK  21 Feb 2001    4:51 pm
*/

assign ef.company = est.company
       ef.loc = est.loc                                           
       ef.est-type = est.est-type
       ef.e-num    = est.e-num                                       
       ef.est-no   = est.est-no          
       ef.eqty = IF li-est-type = 6 THEN est-qty.eqty ELSE rfqitem.qty[1]
       ef.adder[1] = rfqitem.adder[1]                                          
       ef.adder[2] = rfqitem.adder[2]
       ef.adder[3] = rfqitem.adder[3]
       ef.adder[4] = rfqitem.adder[4]
       ef.adder[5] = rfqitem.adder[5]
       ef.adder[6] = rfqitem.adder[6]       
       ef.adder[7] = rfqitem.adder[7]
       ef.adder[8] = rfqitem.adder[8]
       ef.adder[9] = rfqitem.adder[9]
       ef.adder[10] = rfqitem.adder[10]
       ef.adder[11] = rfqitem.adder[11]
       ef.adder[12] = rfqitem.adder[12]
       ef.board   = rfqitem.board
       ef.brd-dscr = rfqitem.brd-dscr                                      
       ef.cal      = rfqitem.cal                                      
       ef.cost-msh = rfqitem.cost-msh                      
       ef.cost-uom = rfqitem.cost-uom                    
       ef.die-in   = rfqitem.die-in                         
       ef.flute    = rfqitem.flute                      
       ef.fr-msh   = rfqitem.fr-msh                                      
       ef.fr-uom   = rfqitem.fr-uom                                  
       ef.lam-code = rfqitem.leaf[4]  /*rfqitem.lam-code */                     
       ef.leaf[1]  = rfqitem.leaf[1]                 
       ef.leaf[2]  = rfqitem.leaf[2]                 
/*       ef.leaf[3]  = rfqitem.leaf[3]     
       ef.leaf[4]  = rfqitem.leaf[4]              don't copy glue,laminate   */
       ef.leaf-dscr[1] = rfqitem.leaf-dscr[1]            
       ef.leaf-dscr[2] = rfqitem.leaf-dscr[2]
/*       ef.leaf-dscr[3] = rfqitem.leaf-dscr[3]      
       ef.leaf-dscr[4] = rfqitem.leaf-dscr[4]   */
       ef.leaf-l[1]    = rfqitem.leaf-l[1]              
       ef.leaf-l[2]    = rfqitem.leaf-l[2]
/*       ef.leaf-l[3]    = rfqitem.leaf-l[3]  
       ef.leaf-l[4]    = rfqitem.leaf-l[4]  */
       ef.leaf-w[1]    = rfqitem.leaf-w[1]
       ef.leaf-w[2]    = rfqitem.leaf-w[2]
/*       ef.leaf-w[3]    = rfqitem.leaf-w[3]  
       ef.leaf-w[4]    = rfqitem.leaf-w[4]   */
       ef.m-code       = rfqitem.m-code                 
       ef.m-dscr       = rfqitem.m-dscr                  
       ef.medium       = rfqitem.medium                  
       ef.test         = rfqitem.test           
       ef.form-no      = 0
       ef.blank-qty    = li-num-of-blank
       ef.gsh-len  = rfqitem.gsh-len
       ef.gsh-wid  = rfqitem.gsh-wid
       ef.lsh-len      = rfqitem.lsh-len
       ef.lsh-wid      = rfqitem.lsh-wid
       .
    DO li-cnt = 1 TO 8:
       assign  ef.spec-dscr[li-cnt]    = rfqitem.spec-dscr[li-cnt]
               ef.spec-no[li-cnt]      = rfqitem.spec-no[li-cnt]
               ef.spec-qty[li-cnt]     = rfqitem.spec-qty[li-cnt]
               ef.spec-uom[li-cnt]     = rfqitem.spec-uom[li-cnt]
               .
    END.

/*
ef.lsh-lock     = rfqitem.lsh-lock
ef.lsh-dep      = rfqitem.lsh-dep
ef.gsh-dep  = rfqitem.gsh-dep
ef.gsh-qty  = rfqitem.gsh-qty
ef.adh-code                                       
ef.adh-sqin                                       
ef.cust-seq = rfqitem.seq                    
ef.f-coat   = 
ef.f-coat-p = 
ef.f-col    = 
ef.f-pass   = 
ef.i-code  : no-array  = rfqitem.i-code - 10 array
ef.lam-dscr =
ef.leaf-bnum                                      
ef.leaf-cost                                      
ef.leaf-o1                                        
ef.leaf-o2                                        
ef.leaf-o3                                        
ef.leaf-o4                                        
ef.leaf-snum                                      
ef.mis-bnum                                      
ef.mis-cost                                       
ef.mis-labf                                       
ef.mis-labm                                       
ef.mis-matf                                       
ef.mis-matm                                       
ef.mis-mkup                                       
ef.mis-simon                                      
ef.mis-snum                                       
ef.n-cuts                                         
ef.n-other-cut                                    
ef.n-out                                          
ef.n-out-d                                        
ef.n-out-l                                        
ef.n-revs                                         
ef.n-sets                                         
ef.n-slits                                        
ef.nc                                             
ef.nsh-dep    = rfqitem.nsh-dep
ef.nsh-len    = rfqitem.nsh-len                   
ef.nsh-wid    = rfqitem.nsh-wid                   
ef.op-lock    = rfqitem.op-lock                   
ef.or-costmsh                          
ef.plies                                          
ef.roll                                           
ef.roll-wid                                       
ef.trim-d
ef.trim-l                                         
ef.trim-pen                                       
ef.trim-w                                         
ef.weight                                         
ef.xgrain                                         
*/
