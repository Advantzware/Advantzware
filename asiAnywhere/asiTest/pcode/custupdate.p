
/*------------------------------------------------------------------------
    File        : custupdate.p
    Purpose     : Customer

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



DEFINE TEMP-TABLE ttcust1 NO-UNDO
    
    FIELD vcustno                   LIKE cust.cust-no               
    FIELD vcustname                 LIKE cust.name                 
    FIELD vcity                     LIKE cust.city                 
    FIELD vstate                    LIKE cust.state                
    FIELD vzip                      LIKE cust.zip                  
    FIELD vtype                     LIKE cust.type                 
    FIELD vsman                     LIKE cust.sman                 
    FIELD vterr                     LIKE cust.terr                 
    FIELD vactive                   LIKE cust.active               
    FIELD vdate1                    LIKE cust.date-field[1]        
    FIELD vaddr1                    LIKE  cust.addr[1]             
    FIELD vaddr2                    LIKE cust.addr[2]              
    FIELD vemail                    LIKE cust.email                
    FIELD vterms                    LIKE cust.terms                
    FIELD vcruse                    LIKE cust.cr-use               
    FIELD vcrrating                 LIKE cust.cr-rating            
                  
    FIELD vcrlim                    AS DECIMAL 
    FIELD vordlim                   LIKE cust.ord-lim              
    FIELD vdisc                     LIKE cust.disc                 
    FIELD vcurrcode                 LIKE cust.curr-code            
    FIELD vcrholdinvdays            LIKE cr-hold-invdays           
    FIELD vcrholdinvdue             LIKE cust.cr-hold-invdue       
    FIELD vcustlevel                LIKE cust.cust-level           
    FIELD vcrhold                   LIKE cust.cr-hold              
    FIELD vfinchg                   LIKE cust.fin-chg              
    FIELD vautoreprice              LIKE cust.auto-reprice         
    FIELD vanedicust                LIKE cust.an-edi-cust          
    FIELD vfactored                 LIKE cust.factored             
    FIELD vsort                     LIKE cust.sort                 
    FIELD vtaxgr                    LIKE cust.tax-gr               
    FIELD vtaxid                    LIKE  cust.tax-id              
    FIELD vdatefield2               LIKE cust.date-field[2]        
    FIELD vcontact                  LIKE cust.contact              
    FIELD vareacode                 LIKE cust.area-code            
    FIELD vphone                    LIKE cust.phone                
    FIELD vfaxprefix                LIKE cust.fax-prefix           
    FIELD vfaxcountry               LIKE cust.fax-country          
    FIELD vfrtpay                   LIKE  cust.frt-pay             
    FIELD vfobcode                  LIKE cust.fob-code             
    FIELD vshippart                 LIKE cust.ship-part            
    FIELD vloc                      LIKE cust.loc                  
    FIELD vcarrier                  LIKE cust.carrier              
    FIELD vdelzone                  LIKE cust.del-zone             
    FIELD vunderpct                 LIKE cust.under-pct            
    FIELD voverpct                  LIKE cust.over-pct             
    FIELD vmarkup                   LIKE cust.markup               
    FIELD vshipdays                 LIKE cust.ship-days            
    FIELD vpallet                   LIKE cust.pallet               
    FIELD vcasebundle               LIKE cust.case-bundle          
    FIELD vintfield1                LIKE cust.int-field[1]         
    FIELD vdescsman                 LIKE sman.sname
    FIELD vdesctype                 LIKE custype.dscr
    FIELD vdescterms                LIKE terms.dscr
    FIELD vdescloc                  LIKE loc.dscr
    FIELD vdescarrier               LIKE carrier.dscr
    FIELD vdesterr                  LIKE terr.dscr   
    FIELD vdeszone                  LIKE carr-mtx.del-dscr
    FIELD vflatcomm                 LIKE reftable.val[1]
    FIELD vinvmeth                  LIKE cust.inv-meth
    FIELD vmandatory                LIKE cust.po-mandatory  
    FIELD vfax                      LIKE cust.fax
    FIELD vfaxcode                  LIKE cust.fax
    FIELD vtaxdscr                  AS CHAR 
   
    .

DEFINE DATASET dscustupdate FOR ttcust1.
    

DEFINE INPUT PARAMETER prmAction             AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp               AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser               AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcustno             AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcustname           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcity               AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmstate              AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmzip                AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmtype               AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmsman               AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmterr               AS CHARACTER  NO-UNDO.

DEFINE INPUT PARAMETER prmactive             AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmdate1              AS DATE   NO-UNDO.
DEFINE INPUT PARAMETER prmaddr1              AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER  prmaddr2             AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmemail              AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmterms              AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcruse              AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcrrating           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmordlim             AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER prmdisc               AS DECIMAL    NO-UNDO.

DEFINE INPUT PARAMETER prmcurrcode         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcrholdinvdays    AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prmcrholdinvdue     AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER prmcustlevel        AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prmcrhold           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmfinchg           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmautoreprice      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmanedicust        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmfactored         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmsort             AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmtaxgr            AS CHARACTER  NO-UNDO.

DEFINE INPUT PARAMETER prmtaxid           AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmdatefield2     AS DATE        NO-UNDO.
DEFINE INPUT PARAMETER prmcontact         AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmareacode        AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmphone           AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmfaxprefix       AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmfaxcountry      AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmfrtpay          AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmfobcode         AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmshippart        AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmloc             AS CHARACTER   NO-UNDO.

DEFINE INPUT PARAMETER prmcarrier         AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmdelzone         AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmunderpct        AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmoverpct         AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmmarkup          AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmshipdays        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmpallet          AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmcasebundle      AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmintfield1       AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmdescsman        AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmdesctype        AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmdescterms       AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmdescloc         AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmdescarrier      AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmdesterr         AS CHARACTER   NO-UNDO.  
DEFINE INPUT PARAMETER prmdeszone         AS CHARACTER   NO-UNDO.  
DEFINE INPUT PARAMETER prmflatcomm        AS DECIMAL     NO-UNDO. 
DEFINE INPUT PARAMETER prminvmeth         AS CHARACTER     NO-UNDO.  
DEFINE INPUT PARAMETER prmmandatory       AS CHARACTER     NO-UNDO.  
DEFINE INPUT PARAMETER prmfax             AS CHARACTER     NO-UNDO.  
DEFINE INPUT PARAMETER prmfaxcode         AS CHARACTER     NO-UNDO. 
DEFINE INPUT PARAMETER prmcrlim           AS DECIMAL    NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dscustupdate.
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.
DEFINE NEW SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.

DEFINE BUFFER buff-cust FOR cust.
DEFINE BUFFER buff-ref FOR reftable.
DEFINE BUFFER buff-shipto FOR shipto.
DEFINE BUFFER buff-soldto FOR soldto .
DEFINE BUFFER buff-usercust FOR usercust .

 MESSAGE "cust" prmcustno prmComp prminvmeth .
     FOR EACH ttcust1:
        DELETE ttcust1.
    END.

     IF prmAction             = ? THEN ASSIGN prmAction          = "Select".
     IF prmComp               = ? THEN ASSIGN prmComp            = "".
     IF prmUser               = ? THEN ASSIGN prmUser            = "".
     IF prmcustno             = ? THEN ASSIGN prmcustno          = "".
     IF prmcustname           = ? THEN ASSIGN prmcustname        = "".
     IF prmcity               = ? THEN ASSIGN prmcity            = "".
     IF prmstate              = ? THEN ASSIGN prmstate           = "".
     IF prmzip                = ? THEN ASSIGN prmzip             = "".
     IF prmtype               = ? THEN ASSIGN prmtype            = "".
     IF prmsman               = ? THEN ASSIGN prmsman            = "".
     IF prmterr               = ? THEN ASSIGN prmterr            = "".
     
     IF prmactive             = ? THEN ASSIGN prmactive          = "".
     IF prmaddr1              = ? THEN ASSIGN prmaddr1           = "".
     IF  prmaddr2             = ? THEN ASSIGN  prmaddr2          = "".
     IF prmemail              = ? THEN ASSIGN prmemail           = "".
     IF prmterms              = ? THEN ASSIGN prmterms           = "".
     IF prmcruse              = ? THEN ASSIGN prmcruse           = "".
     IF prmcrrating           = ? THEN ASSIGN prmcrrating        = "".
     IF prmordlim             = ? THEN ASSIGN prmordlim          = 0.
     IF prmdisc               = ? THEN ASSIGN prmdisc            = 0.
    
     IF prmcurrcode           = ? THEN ASSIGN prmcurrcode        = "".
     IF prmcrholdinvdays      = ? THEN ASSIGN prmcrholdinvdays   = 0.
     IF prmcrholdinvdue       = ? THEN ASSIGN prmcrholdinvdue    = 0.
     IF prmcustlevel          = ? THEN ASSIGN prmcustlevel       = 0.
     IF prmcrhold             = ? THEN ASSIGN prmcrhold          = "".
     IF prmfinchg             = ? THEN ASSIGN prmfinchg          = "".
     IF prmautoreprice        = ? THEN ASSIGN prmautoreprice     = "".
     IF prmanedicust          = ? THEN ASSIGN prmanedicust       = "".
     IF prmfactored           = ? THEN ASSIGN prmfactored        = "".
     IF prmsort               = ? THEN ASSIGN prmsort            = "".
     IF prmtaxgr              = ? THEN ASSIGN prmtaxgr           = "".
     
     IF prmtaxid              = ? THEN ASSIGN prmtaxid           = "".
     IF prmcontact            = ? THEN ASSIGN prmcontact         = "".
     IF prmareacode           = ? THEN ASSIGN prmareacode        = "".
     IF prmphone              = ? THEN ASSIGN prmphone           = "".
     IF prmfaxprefix          = ? THEN ASSIGN prmfaxprefix       = "".
     IF prmfaxcountry         = ? THEN ASSIGN prmfaxcountry      = "".
     IF prmfrtpay             = ? THEN ASSIGN prmfrtpay          = "".
     IF prmfobcode            = ? THEN ASSIGN prmfobcode         = "".
     IF prmshippart           = ? THEN ASSIGN prmshippart        = "".
     IF prmloc                = ? THEN ASSIGN prmloc             = "".
    
     IF prmcarrier            = ? THEN ASSIGN prmcarrier         = "".
     IF prmdelzone            = ? THEN ASSIGN prmdelzone         = "".
     IF prmunderpct           = ? THEN ASSIGN prmunderpct        = 0.
     IF prmoverpct            = ? THEN ASSIGN prmoverpct         = 0.
     IF prmmarkup             = ? THEN ASSIGN prmmarkup          = 0.
     IF prmshipdays           = ? THEN ASSIGN prmshipdays        = 0.
     IF prmpallet             = ? THEN ASSIGN prmpallet          = "".
     IF prmcasebundle         = ? THEN ASSIGN prmcasebundle      = "".
     IF prmintfield1          = ? THEN ASSIGN prmintfield1       = 0.
     IF prmdescsman           = ? THEN ASSIGN prmdescsman        = "".
     IF prmdesctype           = ? THEN ASSIGN prmdesctype        = "".
     IF prmdescterms          = ? THEN ASSIGN prmdescterms       = "".
     IF prmdescloc            = ? THEN ASSIGN prmdescloc         = "".
     IF prmdescarrier         = ? THEN ASSIGN prmdescarrier      = "".
     IF prmdesterr            = ? THEN ASSIGN prmdesterr         = "".
     IF prmdeszone            = ? THEN ASSIGN prmdeszone         = "".
     IF prmflatcomm           = ? THEN ASSIGN prmflatcomm        = 0.
     IF prminvmeth            = ? THEN ASSIGN prminvmeth         = "".
     IF prmmandatory          = ? THEN ASSIGN prmmandatory       = "".
     IF prmfax                = ? THEN ASSIGN prmfax             = "".
     IF prmfaxcode            = ? THEN ASSIGN prmfaxcode         = "".
     IF prmcrlim              = ? THEN ASSIGN prmcrlim           = 0.




     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.
   ASSIGN  g_company = prmComp.




IF prmAction = "Add" THEN DO:

    FIND FIRST cust WHERE cust.cust-no = vcustno  AND cust.company = prmComp   NO-LOCK NO-ERROR.
    IF AVAILABLE cust THEN DO:
        ASSIGN cError = "Customer Code Already Exists, Please Enter a Different Customer".
            ASSIGN prmAction = "View". 
                RETURN.
                
    END.
    
   
    CREATE cust .
 
        ASSIGN
             cust.cust-no          = prmcustno 
             cust.company          = prmComp
             cust.name             = prmcustname
             cust.city             = prmcity
             cust.state            = prmstate
             cust.zip              = prmzip
             cust.type             = prmtype 
             cust.sman             = prmsman
             cust.terr             = prmterr 

              cust.active           = prmactive 
             cust.date-field[1]    = prmdate1 
             cust.addr[1]          =  prmaddr1
             cust.addr[2]          =  prmaddr2   
             cust.email            = prmemail
             cust.terms            = prmterms  
             cust.cr-use           =  prmcruse
             cust.cr-rating        = prmcrrating
            
            
             cust.ord-lim          =  prmordlim
             cust.cr-lim          =  prmcrlim
             cust.disc             = prmdisc 
             cust.curr-code        =  prmcurrcode  
             cust.cr-hold-invdays  =  prmcrholdinvdays
             cust.cr-hold-invdue   =  prmcrholdinvdue
             cust.cust-level       =  prmcustlevel 
             cust.cr-hold          =   IF prmcrhold ="YES" THEN TRUE ELSE FALSE
             cust.fin-chg          = IF prmfinchg ="YES" THEN TRUE ELSE FALSE
             cust.auto-reprice     =  IF prmautoreprice ="YES" THEN TRUE ELSE FALSE
             cust.an-edi-cust      =  IF  prmanedicust ="YES" THEN TRUE ELSE FALSE 
             cust.factored        =  IF prmfactored ="YES" THEN TRUE ELSE FALSE 
             cust.sort             = prmsort
             cust.tax-gr           =  prmtaxgr  
             cust.tax-id           =  prmtaxid 
             cust.date-field[2]    =  prmdatefield2
             cust.contact          = prmcontact 
             cust.area-code        =  prmareacode
             cust.phone            = prmphone 
             cust.fax-prefix       =  prmfaxprefix 
             cust.fax-country      =  prmfaxcountry
             cust.frt-pay          =  prmfrtpay
             cust.fob-code         =  prmfobcode
             cust.ship-part        = IF prmshippart ="YES" THEN TRUE ELSE FALSE 
             cust.po-mandatory     = IF prmmandatory = "yes" THEN TRUE ELSE FALSE

             cust.loc              = prmloc 
             cust.carrier          = prmcarrier 
             cust.del-zone         =  prmdelzone
             cust.under-pct        = prmunderpct 
             cust.over-pct         =  prmoverpct
             cust.markup           =  prmmarkup
             cust.ship-days        =  prmshipdays
             cust.pallet           =  prmpallet
             cust.case-bundle      =  prmcasebundle
             cust.int-field[1]     = prmintfield1 
             
            
             cust.fax              = prmfaxcode + prmfax
             cust.inv-meth         =   IF prminvmeth ="True" THEN TRUE ELSE FALSE . 
             
                
        CREATE buff-ref .
        ASSIGN
            buff-ref.code    = prmcustno
            buff-ref.val[1]  = prmflatcomm.
        RELEASE buff-ref.

       

        CREATE buff-shipto.
        ASSIGN
            buff-shipto.company            = prmComp 
            buff-shipto.cust-no            =  prmcustno  
            buff-shipto.ship-id            =  prmcustno
            buff-shipto.ship-no            = 1
            buff-shipto.ship-name          =  prmcustname    
            buff-shipto.ship-city          =  prmcity        
            buff-shipto.ship-state         =  prmstate       
            buff-shipto.ship-zip           =  prmzip         
            buff-shipto.ship-addr[1]       =  prmaddr1   
            buff-shipto.ship-addr[2]       =  prmaddr2
            buff-shipto.loc                =  prmloc 
            buff-shipto.carrier            =  prmcarrier 
            buff-shipto.del-zone           =  prmdelzone
             .

        RELEASE buff-shipto.
              
           


        CREATE buff-soldto.
        
        ASSIGN
            buff-soldto.company            = prmComp 
            buff-soldto.cust-no            =   prmcustno    
            buff-soldto.sold-id            =   prmcustno 
            buff-soldto.sold-no            =   1
            buff-soldto.sold-name          =   prmcustname  
            buff-soldto.sold-city          =   prmcity      
            buff-soldto.sold-state         =   prmstate     
            buff-soldto.sold-zip           =   prmzip       
            buff-soldto.sold-addr[1]       =    prmaddr1    
            buff-soldto.sold-addr[2]       =    prmaddr2   
             .   
            RELEASE buff-soldto.  

            CREATE buff-usercust.
              ASSIGN
                  buff-usercust.user_id         = prmUser
                  buff-usercust.company         = prmComp
                  buff-usercust.cust-no         = prmcustno  .
              RELEASE buff-usercust.



        ASSIGN prmAction = "View".

END.

    
IF prmAction = "Update" THEN DO:
    
    FIND FIRST cust WHERE cust.cust-no = prmcustno  AND cust.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
    IF  AVAILABLE cust  THEN DO:
        ASSIGN
             cust.NAME             = prmcustname
             cust.city             = prmcity
             cust.state            = prmstate
             cust.zip              = prmzip
             cust.TYPE            = prmtype 
             cust.sman             = prmsman
             cust.terr             = prmterr

             cust.active           = prmactive 
             cust.date-field[1]    = prmdate1 
             cust.addr[1]          =  prmaddr1
             cust.addr[2]          =  prmaddr2   
             cust.email            = prmemail
             cust.terms            = prmterms  
             cust.cr-use           =  prmcruse
             cust.cr-rating        = prmcrrating

            
             cust.ord-lim          =  prmordlim
             cust.cr-lim           =  prmcrlim
             cust.disc             = prmdisc 
             cust.curr-code        =  prmcurrcode  
             cust.cr-hold-invdays  =  prmcrholdinvdays
             cust.cr-hold-invdue   =  prmcrholdinvdue
             cust.cust-level       =  prmcustlevel 
             cust.cr-hold          =   IF prmcrhold ="YES" THEN TRUE ELSE FALSE
             cust.fin-chg          = IF prmfinchg ="YES" THEN TRUE ELSE FALSE
             cust.auto-reprice     =  IF prmautoreprice ="YES" THEN TRUE ELSE FALSE
             cust.an-edi-cust      = IF  prmanedicust ="YES" THEN TRUE ELSE FALSE
             cust.factored        =    IF prmfactored ="YES" THEN TRUE ELSE FALSE  
             cust.sort             = prmsort
             cust.tax-gr           =  prmtaxgr  
             cust.tax-id           =  prmtaxid 
             cust.date-field[2]    =  prmdatefield2
             cust.contact          = prmcontact 
             cust.area-code        =  prmareacode
             cust.phone            = prmphone 
             cust.fax-prefix       =  prmfaxprefix 
             cust.fax-country      =  prmfaxcountry
             cust.frt-pay          =  prmfrtpay
             cust.fob-code         =  prmfobcode
             cust.ship-part        = IF prmshippart ="YES" THEN TRUE ELSE FALSE
             cust.po-mandatory     = IF prmmandatory = "yes" THEN TRUE ELSE FALSE

             cust.loc              = prmloc 
             cust.carrier          = prmcarrier 
             cust.del-zone         =  prmdelzone
             cust.under-pct        = prmunderpct 
             cust.over-pct         =  prmoverpct
             cust.markup           =  prmmarkup
             cust.ship-days        =  prmshipdays
             cust.pallet           =  prmpallet
             cust.case-bundle      =  prmcasebundle
             cust.int-field[1]     = prmintfield1 
             
             cust.fax              = prmfaxcode +  prmfax
             cust.inv-meth         = IF prminvmeth = "Yes" THEN TRUE ELSE FALSE 
             .
             
     END.

     

   FIND FIRST buff-ref WHERE buff-ref.code = prmcustno   EXCLUSIVE-LOCK NO-ERROR.
     IF  AVAILABLE buff-ref  THEN DO:
         ASSIGN
             buff-ref.val[1]     = prmflatcomm.
     END.
     RELEASE buff-ref.


     
  ASSIGN prmAction = "View".
  
END. /*IF prmAction = "Update" THEN DO:*/

IF prmAction = "Delete" THEN DO:
   FIND shipto WHERE shipto.cust-no = prmcustno AND shipto.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE shipto THEN DO:
            DELETE shipto.
        END.
    FIND soldto WHERE soldto.cust-no = prmcustno AND soldto.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE soldto THEN DO:
            DELETE soldto.
        END.
    FIND cust WHERE cust.cust-no = prmcustno  AND cust.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE cust  THEN DO:
            DELETE cust.
    END.
    FIND usercust WHERE usercust.cust-no= prmcustno AND usercust.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE usercust THEN DO:
        DELETE usercust.
    END.
END. /*IF prmAction = "Delete" THEN DO:*/



IF prmAction = "View"  THEN DO:

    FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = prmcustno
        NO-LOCK NO-ERROR.
    IF AVAILABLE cust THEN DO:

        CREATE  ttcust1.
            ASSIGN 
                 ttcust1.vcustno            = cust.cust-no 
                 ttcust1.vcustname          = cust.name 
                 ttcust1.vcity              = cust.city 
                 ttcust1.vstate             = cust.state   
                 ttcust1.vzip               = cust.zip
                 ttcust1.vtype              = cust.type  
                 ttcust1.vsman              = cust.sman
                 ttcust1.vterr              = cust.terr 

                 ttcust1.vactive            = cust.active 
                 ttcust1.vdate1             = cust.date-field[1] 
                 ttcust1.vaddr1             = cust.addr[1] 
                 ttcust1.vaddr2             = cust.addr[2]   
                 ttcust1.vemail             = cust.email
                 ttcust1.vterms             = cust.terms  
                 ttcust1.vcruse             = cust.cr-use
                 ttcust1.vcrrating          = cust.cr-rating 

                
                 ttcust1.vordlim            = cust.ord-lim  
                 ttcust1.vcrlim             = cust.cr-lim
                 ttcust1.vdisc              = cust.disc 
                 ttcust1.vcurrcode          = cust.curr-code   
                 ttcust1.vcrholdinvdays     = cust.cr-hold-invdays
                 ttcust1.vcrholdinvdue      = cust.cr-hold-invdue  
                 ttcust1.vcustlevel         = cust.cust-level
                 ttcust1.vcrhold            = cust.cr-hold 

                 ttcust1.vfinchg            = cust.fin-chg 
                 ttcust1.vautoreprice       = cust.auto-reprice 
                 ttcust1.vanedicust         = cust.an-edi-cust 
                 ttcust1.vfactored          = cust.factored   
                 ttcust1.vsort              = cust.sort
                 ttcust1.vtaxgr             = cust.tax-gr  
                 ttcust1.vtaxid             = cust.tax-id
                 ttcust1.vdatefield2        = cust.date-field[2] 

                 ttcust1.vcontact           = cust.contact 
                 ttcust1.vareacode          = cust.area-code 
                 ttcust1.vphone             = cust.phone 
                 ttcust1.vfaxprefix         = cust.fax-prefix   
                 ttcust1.vfaxcountry        = cust.fax-country
                 ttcust1.vfrtpay            = cust.frt-pay  
                 ttcust1.vfobcode           = cust.fob-code
                 ttcust1.vshippart          = cust.ship-part
                
                 ttcust1.vloc               = cust.loc 
                 ttcust1.vcarrier           = cust.carrier 
                 ttcust1.vdelzone           = cust.del-zone 
                 ttcust1.vunderpct          = cust.under-pct   
                 ttcust1.voverpct           = cust.over-pct
                 ttcust1.vmarkup            = cust.markup  
                 ttcust1.vshipdays          = cust.ship-days
                 ttcust1.vpallet            = cust.pallet 
                 ttcust1.vcasebundle        = cust.case-bundle
                 ttcust1.vintfield1         = cust.int-field[1] 
                 ttcust1.vmandatory         = cust.po-mandatory
                 
                 ttcust1.vfax               = SUBSTRING(cust.fax,4) 
                 ttcust1.vfaxcode           = SUBSTRING(cust.fax,1,3)
                 ttcust1.vinvmeth           =  cust.inv-meth  .
                  . 

  MESSAGE "viewpo" vmandatory cust.po-mandatory.

     FIND FIRST sman WHERE sman.sman = cust.sman NO-LOCK NO-ERROR.
     IF  AVAILABLE sman  THEN DO:
         ASSIGN
             ttcust1.vdescsman = sman.sname.
     END.

     FIND FIRST custype WHERE custype.custype = cust.TYPE NO-LOCK NO-ERROR.
     IF AVAILABLE custype THEN  DO:
         ASSIGN
             ttcust1.vdesctype = custype.dscr.
     END.

     FIND FIRST terms WHERE terms.t-code = cust.terms NO-LOCK NO-ERROR.
     IF  AVAILABLE terms  THEN DO:
         ASSIGN
             ttcust1.vdescterms = terms.dscr.
     END.

     FIND FIRST loc  WHERE loc.loc = cust.loc NO-LOCK NO-ERROR.
      IF  AVAILABLE loc  THEN DO:
        ASSIGN
            ttcust1.vdescloc = loc.dscr.
     END.


     FIND FIRST carrier  WHERE carrier.carrier = cust.carrier NO-LOCK NO-ERROR.
      IF  AVAILABLE carrier  THEN DO:
       ASSIGN
           ttcust1.vdescarrier = carrier.dscr.
     END.

     FIND FIRST terr WHERE terr.terr = cust.terr  NO-LOCK NO-ERROR.
      IF  AVAILABLE terr  THEN DO:
      ASSIGN
          ttcust1.vdesterr = terr.dscr.
     END.

      FIND FIRST carr-mtx WHERE carr-mtx.del-zone = cust.del-zone   NO-LOCK NO-ERROR.
      IF  AVAILABLE carr-mtx  THEN DO:
      ASSIGN
          ttcust1.vdeszone = carr-mtx.del-dscr.
     END.
     FIND FIRST reftable WHERE reftable.code = cust.cust-no   NO-LOCK NO-ERROR.
      IF  AVAILABLE reftable  THEN DO:
      ASSIGN
          ttcust1.vflatcomm = reftable.val[1].
     END.

     FIND FIRST stax WHERE stax.tax-group = cust.tax-gr NO-LOCK NO-ERROR.
   IF  AVAILABLE stax  THEN DO:
       ASSIGN
         ttcust1.vtaxdscr = stax.tax-dscr[1] .
   END.
     


    END.

END. /*IF prmAction = "View" THEN DO:*/
