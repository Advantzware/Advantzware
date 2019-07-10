
/*------------------------------------------------------------------------
    File        : custship.p
    Purpose     : Customer Ship to

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



DEFINE TEMP-TABLE ttcust1 NO-UNDO
    FIELD vcustomer                    LIKE shipto.cust-no
    FIELD vshipno                      LIKE shipto.ship-no  
    FIELD vshipid                      LIKE shipto.ship-id 
    FIELD vreckey                      LIKE shipto.rec_key

    FIELD vshipname                     LIKE shipto.ship-name                 
    FIELD vshipcity                     LIKE shipto.ship-city               
    FIELD vshipstate                    LIKE shipto.ship-state          
    FIELD vshipzip                      LIKE shipto.ship-zip    

    FIELD vshipaddr1                    LIKE shipto.ship-addr[1]            
    FIELD vshipaddr2                    LIKE shipto.ship-addr[2]                
    FIELD vcontact                      LIKE shipto.contact              
    FIELD vareacode                     LIKE shipto.area-code         
    FIELD vphone                        LIKE shipto.phone         

    FIELD vtaxcode                      LIKE shipto.tax-code           
    FIELD vbroker                       LIKE shipto.broker               
    FIELD vbill                         LIKE shipto.bill            
    FIELD vdockloc                      LIKE shipto.dock-loc        
    FIELD vdockhour                     LIKE shipto.dock-hour    


           
    FIELD vlocbin                       LIKE shipto.loc-bin              
    FIELD vcarrier                      LIKE shipto.carrier 
    FIELD vpallet                       LIKE shipto.pallet  

    FIELD vshipmeth                     LIKE shipto.ship-meth      
    FIELD vdelchg                       LIKE shipto.del-chg              
    FIELD vdeltime                      LIKE shipto.del-time            
    FIELD vdestcode                     LIKE shipto.dest-code       
    FIELD vnotes1                       LIKE shipto.notes[1]  
    FIELD vnotes2                       LIKE shipto.notes[2]            
    FIELD vnotes3                       LIKE shipto.notes[3]      
    FIELD vnotes4                       LIKE shipto.notes[4] 

    FIELD vfaxAreaCode                  LIKE shipto.fax-prefix   
    FIELD vfaxNumber                    LIKE shipto.fax             
    FIELD vfi_jdedid                    LIKE reftable.dscr      
    FIELD vtb_mandatorytax              LIKE reftable.val[1]
    FIELD vloc                          LIKE shipto.loc    

    

    
    .

DEFINE DATASET dscustshipto FOR ttcust1.

    

DEFINE INPUT PARAMETER prmAction            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp              AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser              AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCustomer          AS CHARACTER  NO-UNDO. 
DEFINE INPUT PARAMETER prmshipid            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmreckey            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmshipno            AS INTEGER    NO-UNDO.
DEFINE INPUT PARAMETER prmshipname          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmshipcity          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmshipstate         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmshipzip           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmshipaddr1         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmshipaddr2         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcontact           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmareacode          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmphone             AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmtaxcode           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmbroker            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmbill              AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmdockloc           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmdockhour          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER  prmlocbin           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER  prmcarrier          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER  prmpallet           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER  prmshipmeth         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER  prmdelchg           AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER  prmdeltime          AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER  prmdestcode         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER  prmnotes1           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER  prmnotes2           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER  prmnotes3           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER  prmnotes4           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER  prmfaxAreaCode      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER  prmfaxNumber        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER  prmfi_jdedid        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER  prmtb_mandatorytax  AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER  prmloc              AS CHARACTER  NO-UNDO.
                       

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dscustshipto.
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.
DEFINE BUFFER buff-ship FOR shipto.           
DEFINE BUFFER buff-ref FOR reftable.

     FOR EACH ttcust1:
        DELETE ttcust1.
    END.

     IF prmAction              = ? THEN ASSIGN prmAction            = "Select".
     IF prmComp                = ? THEN ASSIGN prmComp              = "".
     IF prmUser                = ? THEN ASSIGN prmUser              = "".
     IF prmCustomer            = ? THEN ASSIGN prmCustomer          = "".
     IF prmshipid              = ? THEN ASSIGN prmshipid            = "".
     IF prmreckey              = ? THEN ASSIGN prmreckey            = "".
     IF prmshipno              = ? THEN ASSIGN prmshipno            = 0.
     IF prmshipname            = ? THEN ASSIGN prmshipname          = "".
     IF prmshipcity            = ? THEN ASSIGN prmshipcity          = "".
     IF prmshipstate           = ? THEN ASSIGN prmshipstate         = "".
     IF prmshipzip             = ? THEN ASSIGN prmshipzip           = "".
     IF prmshipaddr1           = ? THEN ASSIGN prmshipaddr1         = "".
     IF prmshipaddr2           = ? THEN ASSIGN prmshipaddr2         = "".
     IF prmcontact             = ? THEN ASSIGN prmcontact           = "".
     IF prmareacode            = ? THEN ASSIGN prmareacode          = "".
     IF prmphone               = ? THEN ASSIGN prmphone             = "".
     IF prmtaxcode             = ? THEN ASSIGN prmtaxcode           = "".
     IF prmbroker              = ? THEN ASSIGN prmbroker            = "".
     IF prmbill                = ? THEN ASSIGN prmbill              = "".
     IF prmdockloc             = ? THEN ASSIGN prmdockloc           = "".
     IF prmdockhour            = ? THEN ASSIGN prmdockhour          = "".
     IF  prmlocbin             = ? THEN ASSIGN  prmlocbin           = "".
     IF  prmcarrier            = ? THEN ASSIGN  prmcarrier          = "".
     IF  prmpallet             = ? THEN ASSIGN  prmpallet           = "".
     IF  prmshipmeth           = ? THEN ASSIGN  prmshipmeth         = "".
     IF  prmdelchg             = ? THEN ASSIGN  prmdelchg           = 0.
     IF  prmdeltime            = ? THEN ASSIGN  prmdeltime          = 0.
     IF  prmdestcode           = ? THEN ASSIGN  prmdestcode         = "".
     IF  prmnotes1             = ? THEN ASSIGN  prmnotes1           = "".
     IF  prmnotes2             = ? THEN ASSIGN  prmnotes2           = "".
     IF  prmnotes3             = ? THEN ASSIGN  prmnotes3           = "".
     IF  prmnotes4             = ? THEN ASSIGN  prmnotes4           = "".
     IF  prmfaxAreaCode        = ? THEN ASSIGN  prmfaxAreaCode      = "".
     IF  prmfaxNumber          = ? THEN ASSIGN  prmfaxNumber        = "".
     IF  prmfi_jdedid          = ? THEN ASSIGN  prmfi_jdedid        = "".
     IF  prmtb_mandatorytax    = ? THEN ASSIGN  prmtb_mandatorytax  = 0.
     IF  prmloc                = ? THEN ASSIGN  prmloc              = "".
                                                                    
     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.


     IF prmAction = "Search" THEN DO:
         FOR EACH buff-ship WHERE buff-ship.company = prmComp AND buff-ship.cust-no = prmCustomer
                        AND (buff-ship.ship-id  =  prmshipid OR prmshipid = "") 
                        AND (buff-ship.ship-name  =  prmshipname OR prmshipname = "")  
                       
                        NO-LOCK:
             CREATE ttcust1.
             ASSIGN 
                 ttcust1.vshipid            = buff-ship.ship-id 
                  ttcust1.vreckey           =  buff-ship.rec_key
                 ttcust1.vshipname          = buff-ship.ship-name  
                 ttcust1.vshipcity          = buff-ship.ship-city  
                 ttcust1.vshipstate         = buff-ship.ship-state    
                 ttcust1.vshipzip           = buff-ship.ship-zip   
                  .                                                 
            

      END. /*FOR EACH buff-ship  */
END. /*IF prmAction = "Select" THEN DO:*/

IF prmAction = "Select" THEN DO:
    FOR EACH buff-ship WHERE buff-ship.company = prmComp AND buff-ship.cust-no = prmCustomer 
        NO-LOCK:
        CREATE ttcust1.
           ASSIGN 
                 ttcust1.vshipid            =  buff-ship.ship-id 
                 ttcust1.vreckey           =  buff-ship.rec_key
                 ttcust1.vshipno            =  buff-ship.ship-no
                 ttcust1.vshipname          =  buff-ship.ship-name   
                 ttcust1.vshipcity          =  buff-ship.ship-city   
                 ttcust1.vshipstate         =  buff-ship.ship-state   
                 ttcust1.vshipzip           =  buff-ship.ship-zip    
                  .
            
            
    END. /*FOR EACH buff-ship  */

END. /*IF prmAction = "Select" THEN DO:*/
IF prmAction = "Add" THEN DO:
    FIND FIRST zipcode WHERE zipcode.city = prmshipcity NO-LOCK NO-ERROR.
    IF NOT AVAILABLE zipcode THEN DO:
        ASSIGN cError  = "Invalid City".
        RETURN. 
        END.
        FIND FIRST statecod WHERE statecod.statecod =  prmshipstate  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE statecod THEN DO:
            ASSIGN cError  = "Invalid State".
            RETURN. 
            END.
            FIND FIRST zipcode WHERE zipcode.zipcode = prmshipzip  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE zipcode THEN DO:
                ASSIGN cError  = "Invalid zip".
                RETURN. 
                END.
                
                IF prmloc <> "" THEN DO:
                FIND FIRST loc  WHERE loc.company EQ prmComp AND loc.loc EQ prmloc NO-LOCK NO-ERROR.
                IF NOT AVAIL loc THEN DO:
                    ASSIGN
                        cError = "Invalid Warehouse, try help..." .
                    RETURN.
                 END.
                END.
              IF prmpallet <> "" THEN DO:
                  FIND first item where item.company = prmComp and item.mat-type = "D" and
                      item.i-no = prmpallet NO-LOCK NO-ERROR.
                  IF NOT AVAIL ITEM THEN DO:
                      ASSIGN
                          cError = "Invalid Pallet Code. Try Help."  .
                      RETURN.
                  END.
              END.    
                 
              IF prmlocbin <> "" THEN DO:
              FIND  FIRST fg-bin  WHERE fg-bin.company EQ prmComp  AND fg-bin.loc = prmloc
                   AND fg-bin.loc-bin = prmlocbin NO-LOCK NO-ERROR.
               IF NOT AVAIL fg-bin THEN  DO:
                   ASSIGN 
                        cError = "Bin does not exist in this warehouse..." .
                   RETURN.
                END.
               END.
        IF prmcarrier <> "" THEN DO:
        FIND FIRST carrier  WHERE carrier.company EQ prmComp
                      AND carrier.loc     EQ prmloc
                      AND carrier.carrier EQ prmcarrier NO-LOCK NO-ERROR.
        IF NOT AVAIL carrier THEN DO:
            ASSIGN
                cError = "Invalid carrier, try help..."  .
                RETURN.
         END.
        END.

      IF prmdestcode <> "" THEN DO:
         FIND FIRST carr-mtx  WHERE carr-mtx.company  EQ prmComp AND carr-mtx.loc = prmloc
              AND carr-mtx.del-zone EQ prmdestcode AND carr-mtx.carrier = prmcarrier NO-LOCK NO-ERROR.
         IF NOT AVAIL carr-mtx THEN DO:
             ASSIGN
                 cError = "Invalid zone, try help..." .
             RETURN.
         END.
       END.
       
       IF prmtaxcode <> ""  THEN DO:
           FIND FIRST cust WHERE cust.company EQ prmComp  AND cust.cust-no EQ prmCustomer NO-LOCK NO-ERROR.
           FIND FIRST stax  WHERE stax.company   EQ prmComp 
                       AND stax.tax-group EQ prmtaxcode NO-LOCK NO-ERROR.
           IF NOT AVAIL stax  THEN DO:
               ASSIGN 
                   cError = "Must enter a valid tax code, try help..." .
                   RETURN.
           END.
      END.
      
      


END.

IF prmAction = "Add" THEN DO:
    FIND  shipto  WHERE shipto.ship-id  = prmshipid  AND shipto.company = prmComp AND shipto.cust-no = prmCustomer   NO-LOCK NO-ERROR.
   
    IF  AVAILABLE shipto THEN DO:
        ASSIGN cError = "Ship To ID  Already Exists, Please Enter a Different Ship  ID".
            RETURN.
                
    END.
   
    FIND  LAST shipto  WHERE  shipto.cust-no = prmCustomer   NO-LOCK NO-ERROR.
   IF  AVAILABLE shipto  THEN DO:
       
        ASSIGN prmshipno = shipto.ship-no + 1.
         END.
          ELSE DO:
          ASSIGN prmshipno = 1.
          END.

       
   
    CREATE shipto .

        ASSIGN
            shipto.company               =  prmComp   
            shipto.cust-no               =  prmCustomer
            shipto.ship-id               =  prmshipid  
            shipto.ship-no               =  prmshipno  
            shipto.company               =  prmComp
            shipto.ship-name             =  prmshipname   
            shipto.ship-city             =  prmshipcity   
            shipto.ship-state            =  prmshipstate  
            shipto.ship-zip              =  prmshipzip 
            shipto.ship-addr[1]          = prmshipaddr1  
            shipto.ship-addr[2]          = prmshipaddr2  
            shipto.contact               = prmcontact    
            shipto.area-code             = prmareacode   
            shipto.phone                 = prmphone    
            shipto.tax-code              = prmtaxcode   
            shipto.broker                = IF prmbroker ="YES" THEN TRUE ELSE FALSE     
            shipto.bill                  = IF prmbill ="YES" THEN TRUE ELSE FALSE      
            shipto.dock-loc              = prmdockloc   
            shipto.dock-hour             = prmdockhour 
            shipto.loc-bin               = prmlocbin              
            shipto.carrier               = prmcarrier           
            shipto.pallet                = prmpallet                                                                
            shipto.ship-meth             = IF prmshipmeth ="YES" THEN TRUE ELSE FALSE         
            shipto.del-chg               = prmdelchg            
            shipto.del-time              = prmdeltime           
            shipto.dest-code             = prmdestcode          
            shipto.notes[1]              = prmnotes1            
            shipto.notes[2]              = prmnotes2            
            shipto.notes[3]              = prmnotes3            
            shipto.notes[4]              = prmnotes4 
            shipto.fax-prefix            = prmfaxAreaCode 
            shipto.fax                   = prmfaxNumber
            shipto.loc                   = prmloc   .

            
       FIND FIRST buff-ref WHERE  buff-ref.code2 = prmshipid AND buff-ref.company = prmComp AND buff-ref.reftable = "JDCUST#" AND buff-ref.code = prmCustomer  NO-LOCK NO-ERROR.
       IF NOT AVAIL buff-ref THEN DO:
           CREATE buff-ref.
           ASSIGN
               buff-ref.reftable = "JDCUST#"
               buff-ref.company  = prmComp
               buff-ref.loc      = prmloc
               buff-ref.code     = prmCustomer
               buff-ref.code2    = prmshipid
               buff-ref.dscr = prmfi_jdedid  
               .
           END.
       ELSE DO:
           buff-ref.dscr = prmfi_jdedid.
       END.
       RELEASE buff-ref.

       FIND FIRST buff-ref WHERE  buff-ref.code2 = prmshipid AND buff-ref.company = prmComp AND buff-ref.reftable = "shipmtax"  AND buff-ref.code = prmCustomer  NO-LOCK NO-ERROR.
       IF NOT AVAIL buff-ref THEN DO:
           CREATE buff-ref.
           ASSIGN
               buff-ref.reftable = "shipmtax"
               buff-ref.company  = prmComp
               buff-ref.loc      = prmloc
               buff-ref.code     = prmCustomer
               buff-ref.code2    = prmshipid
               buff-ref.val[1] = INT(prmtb_mandatorytax)  
               .
       END.
       ELSE DO:
           buff-ref.val[1] = INT(prmtb_mandatorytax).
           END.
           RELEASE buff-ref.

 MESSAGE "shipmtax" prmComp   prmloc prmCustomer  prmshipid prmtb_mandatorytax prmfi_jdedid  .
        ASSIGN prmAction="View" . 
END.



 IF prmAction = "Update" THEN DO:
    FIND FIRST zipcode WHERE zipcode.city = prmshipcity NO-LOCK NO-ERROR.
    IF NOT AVAILABLE zipcode THEN DO:
        ASSIGN cError  = "Invalid City".
        RETURN. 
        END.
        FIND FIRST statecod WHERE statecod.statecod =  prmshipstate  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE statecod THEN DO:
            ASSIGN cError  = "Invalid State".
            RETURN. 
            END.
            FIND FIRST zipcode WHERE zipcode.zipcode = prmshipzip  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE zipcode THEN DO:
                ASSIGN cError  = "Invalid zip".
                RETURN. 
                END.
               
              IF prmloc <> "" THEN DO:
                FIND FIRST loc  WHERE loc.company EQ prmComp AND loc.loc EQ prmloc NO-LOCK NO-ERROR.
                IF NOT AVAIL loc THEN DO:
                    ASSIGN
                        cError = "Invalid Warehouse, try help..." .
                    RETURN.
                END.
              END.
              IF prmpallet <> "" THEN DO:
                  FIND first item where item.company = prmComp and item.mat-type = "D" and
                      item.i-no = prmpallet NO-LOCK NO-ERROR.
                  IF NOT AVAIL ITEM THEN DO:
                      ASSIGN
                          cError = "Invalid Pallet Code. Try Help."  .
                      RETURN.
                      END.
                 END.    
                 
              IF prmlocbin <> "" THEN DO:
              FIND  FIRST fg-bin  WHERE fg-bin.company EQ prmComp  AND fg-bin.loc = prmloc
                   AND fg-bin.loc-bin = prmlocbin NO-LOCK NO-ERROR.
               IF NOT AVAIL fg-bin THEN  DO:
                   ASSIGN 
                        cError = "Bin does not exist in this warehouse..." .
                   RETURN.
               END.
             END.
        IF prmcarrier <> "" THEN DO:
        FIND FIRST carrier  WHERE carrier.company EQ prmComp
                      AND carrier.loc     EQ prmloc
                      AND carrier.carrier EQ prmcarrier NO-LOCK NO-ERROR.
        IF NOT AVAIL carrier THEN DO:
            ASSIGN
                cError = "Invalid carrier, try help..."  .
                RETURN.
        END.
      END.
         IF prmdestcode <> "" THEN DO:
         FIND FIRST carr-mtx  WHERE carr-mtx.company  EQ prmComp AND carr-mtx.loc = prmloc
              AND carr-mtx.del-zone EQ prmdestcode AND carr-mtx.carrier = prmcarrier NO-LOCK NO-ERROR.
         IF NOT AVAIL carr-mtx THEN DO:
             ASSIGN
                 cError = "Invalid zone, try help..." .
             RETURN.
         END.
       END.
       
       IF prmtaxcode <> ""  THEN DO:
           FIND FIRST cust WHERE cust.company EQ prmComp  AND cust.cust-no EQ prmCustomer NO-LOCK NO-ERROR.
           FIND FIRST stax  WHERE stax.company   EQ prmComp 
                       AND stax.tax-group EQ prmtaxcode NO-LOCK NO-ERROR.
           IF NOT AVAIL stax  THEN DO:
               ASSIGN 
                   cError = "Must enter a valid tax code, try help..." .
                   RETURN.
           END.
      END.
       

END.                             


IF prmAction = "Update" THEN DO:
    FIND FIRST shipto WHERE shipto.ship-id = prmshipid   AND shipto.company = prmComp AND shipto.cust-no = prmCustomer EXCLUSIVE-LOCK NO-ERROR.
    IF  AVAILABLE shipto  THEN DO:
        ASSIGN
            shipto.ship-name             =  prmshipname   
            shipto.ship-city             =  prmshipcity   
            shipto.ship-state            =  prmshipstate  
            shipto.ship-zip              =  prmshipzip    
            shipto.ship-addr[1]          = prmshipaddr1  
            shipto.ship-addr[2]          = prmshipaddr2  
            shipto.contact               = prmcontact    
            shipto.area-code             = prmareacode   
            shipto.phone                 = prmphone       
            shipto.tax-code              = prmtaxcode   
            shipto.broker                = IF prmbroker ="YES" THEN TRUE ELSE FALSE     
            shipto.bill                  = IF prmbill ="YES" THEN TRUE ELSE FALSE      
            shipto.dock-loc              = prmdockloc   
            shipto.dock-hour             = prmdockhour    
            shipto.loc-bin               = prmlocbin              
            shipto.carrier               = prmcarrier           
            shipto.pallet                = prmpallet      
            shipto.ship-meth             = IF prmshipmeth ="yes" THEN TRUE ELSE FALSE          
            shipto.del-chg               = prmdelchg            
            shipto.del-time              = prmdeltime           
            shipto.dest-code             = prmdestcode          
            shipto.notes[1]              = prmnotes1            
            shipto.notes[2]              = prmnotes2            
            shipto.notes[3]              = prmnotes3            
            shipto.notes[4]              = prmnotes4      
            shipto.fax-prefix            = prmfaxAreaCode        
            shipto.fax                   = prmfaxNumber       
            shipto.loc                   = prmloc   .  

    END.
   


   FIND FIRST buff-ref WHERE  buff-ref.code2 = prmshipid AND buff-ref.company = prmComp AND buff-ref.reftable = "JDCUST#" AND buff-ref.code = prmCustomer  EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL buff-ref THEN DO:
      CREATE buff-ref.
      ASSIGN
       buff-ref.reftable = "JDCUST#"
       buff-ref.company  = prmComp
       buff-ref.loc      = prmloc
       buff-ref.code     = prmCustomer
       buff-ref.code2    = prmshipid
       buff-ref.dscr = prmfi_jdedid  
          .
    END.
    
    ELSE DO:
      buff-ref.dscr = prmfi_jdedid.
      END.
      RELEASE buff-ref.
          
    FIND FIRST buff-ref WHERE  buff-ref.code2 = prmshipid AND buff-ref.company = prmComp AND buff-ref.reftable = "shipmtax"  AND buff-ref.code = prmCustomer  EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL buff-ref THEN DO:
      CREATE buff-ref.
      ASSIGN
       buff-ref.reftable = "shipmtax"
       buff-ref.company  = prmComp
       buff-ref.loc      = prmloc
       buff-ref.code     = prmCustomer
       buff-ref.code2    = prmshipid
       buff-ref.val[1] = INT(prmtb_mandatorytax)  
          .
    END.
    
    ELSE DO:
      buff-ref.val[1] = INT(prmtb_mandatorytax).
            END.
            RELEASE buff-ref.

MESSAGE "shipmtax" prmComp   prmloc prmCustomer  prmshipid prmtb_mandatorytax.
      ASSIGN prmAction ="View" .
     

   END. /*IF prmAction = "Update" THEN DO:*/

IF prmAction = "Delete" THEN DO:

    FIND shipto WHERE shipto.ship-id = prmshipid  AND shipto.company = prmComp AND shipto.cust-no = prmCustomer EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE shipto  THEN DO:
        DELETE shipto.
        END.
        
        FIND  reftable WHERE reftable.company = prmComp AND reftable.code2 = prmshipid AND reftable.CODE = prmCustomer AND reftable.reftable = "shipmtax"  EXCLUSIVE-LOCK NO-ERROR.
        IF  AVAILABLE reftable  THEN DO:
            DELETE reftable .
            END.
            FIND reftable WHERE reftable.company = prmComp AND reftable.code2 = prmshipid AND reftable.CODE = prmCustomer AND  reftable.reftable = "JDCUST#"    EXCLUSIVE-LOCK NO-ERROR.
            IF  AVAILABLE reftable  THEN DO:
                DELETE reftable
                    .
                END.

                FIND LAST shipto WHERE shipto.cust-no = prmCustomer   NO-LOCK NO-ERROR.
                         ASSIGN prmshipid = shipto.ship-id .  

                             ASSIGN prmAction = "View".     

MESSAGE  "viewdelete" prmAction prmshipid .

END. /*IF prmAction = "Delete" THEN DO:*/   




IF prmAction = "View"  THEN DO:
    FIND FIRST  shipto WHERE shipto.company = prmComp AND shipto.ship-id  = prmshipid AND shipto.cust-no = prmCustomer
        NO-LOCK NO-ERROR.
   
    IF AVAILABLE shipto THEN DO:

        CREATE  ttcust1.
            ASSIGN 
                 ttcust1.vshipid             = shipto.ship-id 
                 ttcust1.vshipname           = shipto.ship-name 
                 ttcust1.vshipcity           = shipto.ship-city 
                 ttcust1.vshipstate          = shipto.ship-state   
                 ttcust1.vshipzip            = shipto.ship-zip
                 ttcust1.vshipaddr1          = shipto.ship-addr[1]
                 ttcust1.vshipaddr2          = shipto.ship-addr[2]
                 ttcust1.vcontact            = shipto.contact     
                 ttcust1.vareacode           = shipto.area-code   
                 ttcust1.vphone              = shipto.phone   
                 ttcust1.vtaxcode            = shipto.tax-code    
                 ttcust1.vbroker             = shipto.broker      
                 ttcust1.vbill               = shipto.bill        
                 ttcust1.vdockloc            = shipto.dock-loc    
                 ttcust1.vdockhour           = shipto.dock-hour                        
                 ttcust1.vlocbin             = shipto.loc-bin    
                 ttcust1.vcarrier            = shipto.carrier 
                 ttcust1.vpallet             = shipto.pallet   
                 ttcust1.vshipmeth           = shipto.ship-meth       
                 ttcust1.vdelchg             = shipto.del-chg         
                 ttcust1.vdeltime            = shipto.del-time        
                 ttcust1.vdestcode           = shipto.dest-code       
                 ttcust1.vnotes1             = shipto.notes[1]      
                 ttcust1.vnotes2             = shipto.notes[2]        
                 ttcust1.vnotes3             = shipto.notes[3]        
                 ttcust1.vnotes4             = shipto.notes[4] 
                 ttcust1.vfaxAreaCode        = shipto.fax-prefix 
                 ttcust1.vfaxNumber          = shipto.fax
                 ttcust1.vloc                = shipto.loc   

                 .
  

                FIND FIRST  reftable WHERE reftable.company = prmComp AND reftable.code2 = prmshipid AND reftable.CODE = prmCustomer AND reftable.reftable = "shipmtax"  NO-LOCK NO-ERROR.
                   
                    IF  AVAILABLE reftable  THEN DO:
                      ASSIGN
                       
                        ttcust1.vtb_mandatorytax     =  reftable.val[1] .
                        END.

                         FIND FIRST reftable WHERE reftable.company = prmComp AND reftable.code2 = prmshipid AND reftable.CODE = prmCustomer AND  reftable.reftable = "JDCUST#"    NO-LOCK NO-ERROR.
                        
                            IF  AVAILABLE reftable  THEN DO:
                              ASSIGN
                                       ttcust1.vfi_jdedid     =  reftable.dscr
                                       .
                                       END.
                
                    END.
                                                                                    
    
MESSAGE  "praveen" prmAction vtb_mandatorytax vfi_jdedid  .
END. /*IF prmAction = "View" THEN DO:*/ 

