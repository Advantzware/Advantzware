
/*------------------------------------------------------------------------
    File        : vendupdate.p
    Purpose     : Vendor

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



DEFINE TEMP-TABLE ttVendorUpdate NO-UNDO
    
      FIELD   vactive       AS CHAR
      FIELD   vendor        AS CHAR
      FIELD   vname         AS CHAR
      FIELD   vadd1         AS CHAR
      FIELD   vadd2         AS CHAR
      FIELD   vcity         AS CHAR
      FIELD   vstate        AS CHAR 
      FIELD   vzip          AS CHAR 
      FIELD   vcountry      AS CHAR 
      FIELD   vPostal       AS CHAR 
      FIELD   vtaxid        AS CHAR 
      FIELD   vremit        AS CHAR 
      FIELD   vradd1        AS CHAR 
      FIELD   vradd2        AS CHAR 
      FIELD   vrcity        AS CHAR 
      FIELD   vrstate       AS CHAR 
      FIELD   vrzip         AS CHAR 
      FIELD   vrcountry     AS CHAR 
      FIELD   vrpostal      AS CHAR 
      FIELD   vcheckmemo    AS CHAR 
      FIELD   vtype         AS CHAR 
      FIELD   vcontact      AS CHAR 
      FIELD   vbuyer        AS CHAR 
      FIELD   vareacode     AS CHAR 
      FIELD   vphone        AS CHAR 
      FIELD   vfaxarea      AS CHAR 
      FIELD   vfax          AS CHAR 
      FIELD   vfaxprefix    AS CHAR 
      FIELD   vfaxcountry   AS CHAR 
      FIELD   voverpct      AS DECIMAL 
      FIELD   vunderpct     AS DECIMAL 
      FIELD   vactnum       AS CHAR 
      FIELD   vcurrcode     AS CHAR 
      FIELD   vtaxgr        AS CHAR 
      FIELD   vcode1099     AS CHAR 
      FIELD   vanedivend    AS CHAR 
      FIELD   vterms        AS CHAR 
      FIELD   vdisc         AS DECIMAL 
      FIELD   vrebate       AS DECIMAL 
      FIELD   vfrtpay       AS CHAR 
      FIELD   vdiscdays     AS INT 
      FIELD   vcarrier      AS CHAR 
      FIELD   vfobcode      AS CHAR 
      FIELD   vtypedscr     AS CHAR 
      FIELD   vbuyerdscr    AS CHAR 
      FIELD   vtermsdscr    AS CHAR 
      FIELD   vcarrierdscr  AS CHAR 
      FIELD   vcurrdscr     AS CHAR 
      FIELD   vactdscr      AS CHAR 
      FIELD   vpoexport     AS CHAR 
      FIELD   vreckey       AS CHAR
      . 

DEFINE DATASET dsVendorUpdate FOR ttVendorUpdate.

DEFINE INPUT PARAMETER prmAction           AS CHAR NO-UNDO.    
DEFINE INPUT PARAMETER prmUser             AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmReckey            AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmActive           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmVendor            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmName             AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAdd1             AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAdd2             AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCity             AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmState            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmZip              AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCountry          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPostal           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmTaxid            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRemit            AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmRadd1            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRadd2            AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmRcity            AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRstate           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRzip             AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRcountry         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRpostal          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCheckmemo        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmType             AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmContact          AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmBuyer          AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmAreacode       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPhone          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFaxarea        AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmFax            AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFaxprefix      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmFaxcountry     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOverpct        AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmUnderpct       AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmActnum         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCurrcode       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmTaxgr          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCode1099      AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmAnedivend     AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmTerms        AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmDisc          AS DECIMAL   NO-UNDO.
DEFINE INPUT PARAMETER prmRebate        AS DECIMAL   NO-UNDO.
DEFINE INPUT PARAMETER prmFrtpay        AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmDiscdays      AS INT   NO-UNDO.
DEFINE INPUT PARAMETER prmCarrier       AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmFobcode       AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmTtypedscr      AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmBuyerdscr     AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmTermsdscr     AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmCarrierdscr   AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmCurrdscr      AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmActdscr       AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER prmPoexport      AS CHARACTER   NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVendorUpdate .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.

DEFINE NEW SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.
 DEF VAR prmComp AS CHAR NO-UNDO.

 
     IF prmAction            = ? THEN ASSIGN prmAction         = "Select".
     IF prmUser              = ? THEN ASSIGN prmUser           = "".
     IF prmReckey            = ? THEN ASSIGN prmReckey         = "".
     IF prmActive            = ? THEN ASSIGN prmActive         = "".
     IF prmVendor            = ? THEN ASSIGN prmVendor         = "".
     IF prmName              = ? THEN ASSIGN prmName           = "".
     IF prmAdd1              = ? THEN ASSIGN prmAdd1           = "".
     IF prmAdd2              = ? THEN ASSIGN prmAdd2           = "".
     IF prmCity              = ? THEN ASSIGN prmCity           = "".
     IF prmState             = ? THEN ASSIGN prmState          = "".
     IF prmZip               = ? THEN ASSIGN prmZip            = "".
     IF prmCountry           = ? THEN ASSIGN prmCountry        = "".
     IF prmPostal            = ? THEN ASSIGN prmPostal         = "".
     IF prmTaxid             = ? THEN ASSIGN prmTaxid          = "".
     IF prmRemit             = ? THEN ASSIGN prmRemit          = "".
     IF prmRadd1             = ? THEN ASSIGN prmRadd1          = "".
     IF prmRadd2             = ? THEN ASSIGN prmRadd2          = "".
     IF prmRcity             = ? THEN ASSIGN prmRcity          = "".
     IF prmRstate            = ? THEN ASSIGN prmRstate         = "".
     IF prmRzip              = ? THEN ASSIGN prmRzip           = "".
     IF prmRcountry          = ? THEN ASSIGN prmRcountry       = "".
     IF prmRpostal           = ? THEN ASSIGN prmRpostal        = "".
     IF prmCheckmemo         = ? THEN ASSIGN prmCheckmemo      = "".
     IF prmType              = ? THEN ASSIGN prmType           = "".
     IF prmContact           = ? THEN ASSIGN prmContact        = "".
     IF prmBuyer             = ? THEN ASSIGN prmBuyer          = "".
     IF prmAreacode          = ? THEN ASSIGN prmAreacode       = "".
     IF prmPhone             = ? THEN ASSIGN prmPhone          = "".
     IF prmFaxarea           = ? THEN ASSIGN prmFaxarea        = "".
     IF prmFax               = ? THEN ASSIGN prmFax            = "".
     IF prmFaxprefix         = ? THEN ASSIGN prmFaxprefix      = "".
     IF prmFaxcountry        = ? THEN ASSIGN prmFaxcountry     = "".
     IF prmOverpct           = ? THEN ASSIGN prmOverpct        = 0.
     IF prmUnderpct          = ? THEN ASSIGN prmUnderpct       = 0.
     IF prmActnum            = ? THEN ASSIGN prmActnum         = "".
     IF prmCurrcode          = ? THEN ASSIGN prmCurrcode       = "".
     IF prmTaxgr             = ? THEN ASSIGN prmTaxgr          = "".
     IF prmCode1099          = ? THEN ASSIGN prmCode1099       = "".
     IF prmAnedivend         = ? THEN ASSIGN prmAnedivend      = "".
     IF prmTerms             = ? THEN ASSIGN prmTerms          = "".
     IF prmDisc              = ? THEN ASSIGN prmDisc           = 0.
     IF prmRebate            = ? THEN ASSIGN prmRebate         = 0.
     IF prmFrtpay            = ? THEN ASSIGN prmFrtpay         = "".
     IF prmDiscdays          = ? THEN ASSIGN prmDiscdays       = 0.
     IF prmCarrier           = ? THEN ASSIGN prmCarrier        = "".
     IF prmFobcode           = ? THEN ASSIGN prmFobcode        = "".
     IF prmTtypedscr         = ? THEN ASSIGN prmTtypedscr      = "".
     IF prmBuyerdscr         = ? THEN ASSIGN prmBuyerdscr      = "".
     IF prmTermsdscr         = ? THEN ASSIGN prmTermsdscr      = "".
     IF prmCarrierdscr       = ? THEN ASSIGN prmCarrierdscr    = "".
     IF prmCurrdscr          = ? THEN ASSIGN prmCurrdscr       = "".
     IF prmActdscr           = ? THEN ASSIGN prmActdscr        = "".
     IF prmPoexport          = ? THEN ASSIGN prmPoexport       = "".
     
 {sys/inc/VAR.i NEW SHARED}
     DEF BUFFER b-vend FOR vend.
 DEF VAR v-old-poexport LIKE vend.po-export NO-UNDO .
 DEF VAR vend-char-f AS CHAR NO-UNDO.
DEF VAR vend-log-f AS LOG NO-UNDO.
DEF VAR ls-name-value AS cha NO-UNDO.
DEF VAR ls-field-value AS cha NO-UNDO.
DEF VAR lv-msg AS CHAR NO-UNDO.
DEF VAR thisOne AS CHAR NO-UNDO.
 DEFINE BUFFER buff-vend FOR vend .

     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.
   ASSIGN  g_company = prmComp
           cocode  = prmComp .


   {sys/ref/sys-ctrl.i}

   DO TRANSACTION:
  {sys/inc/aptax.i}
  {sys/inc/poexport.i}

  FOR EACH reftable WHERE reftable.reftable EQ "vend.poexport":
    FIND FIRST b-vend
        WHERE b-vend.company   EQ reftable.company
          AND b-vend.vend-no   EQ reftable.code
          AND b-vend.po-export EQ ""
        NO-ERROR.
    IF AVAIL b-vend THEN DO:
      b-vend.po-export = reftable.dscr.
      FIND CURRENT b-vend NO-LOCK.
    END.
    DELETE reftable.
  END.
  find first sys-ctrl
           where sys-ctrl.company eq g_company
             and sys-ctrl.name    eq "VendXfer"
           no-lock no-error.
         if not avail sys-ctrl then
         do:
           create sys-ctrl.
           assign
            sys-ctrl.company = g_company
            sys-ctrl.name    = "VendXfer"
            sys-ctrl.descrip = "Transfer Vendor Information to Sister Plants?".
           
         end.
         assign
            vend-char-f =  sys-ctrl.char-fld 
            vend-log-f = sys-ctrl.log-fld .
    END.



IF prmAction = "Search" THEN DO:
     FOR EACH vend WHERE vend.company = g_company AND (vend.vend BEGINS prmVendor OR prmVendor = "")
                         AND (vend.NAME BEGINS prmName OR prmName = "")
                         AND (vend.TYPE BEGINS prmType OR prmType = "")
                         AND (vend.buyer BEGINS prmBuyer OR prmBuyer = "")
                         AND (vend.zip BEGINS prmZip OR prmZip = "")  NO-LOCK:
          CREATE  ttVendorUpdate.
            ASSIGN 
                 ttVendorUpdate.vactive          = vend.active 
                 ttVendorUpdate.vendor           = vend.vend
                 ttVendorUpdate.vname            = vend.name 
                 ttVendorUpdate.vtype            = vend.type
                 ttVendorUpdate.vareacode        = vend.area-code    
                 ttVendorUpdate.vphone           = vend.phone
                 ttVendorUpdate.vfaxarea         = vend.fax-area     
                 ttVendorUpdate.vfax             = vend.fax  
                 ttVendorUpdate.vbuyer           = vend.buyer
                 ttVendorUpdate.vadd1            = vend.add1         
                 ttVendorUpdate.vadd2            = vend.add2         
                 ttVendorUpdate.vcity            = vend.city         
                 ttVendorUpdate.vstate           = vend.state        
                 ttVendorUpdate.vzip             = vend.zip          
                 ttVendorUpdate.vcountry         = vend.country      
                 ttVendorUpdate.vPostal          = vend.Postal       
                 ttVendorUpdate.vtaxid           = vend.tax-id       
                 ttVendorUpdate.vremit           = vend.remit 
                 ttVendorUpdate.vreckey          = vend.rec_key      .

     END.
END.


IF prmAction = "Add"  THEN DO:

    IF prmVendor EQ "" OR
       CAN-FIND(FIRST b-vend WHERE b-vend.company EQ cocode
                               AND b-vend.vend-no EQ prmVendor) THEN DO:
      cError = "Vendor " + TRIM(prmVendor) + " " +
              TRIM(IF prmVendor EQ "" THEN "may not be spaces" ELSE "already exists") +
              "..." .
      RETURN .
    END.

    FIND FIRST ventype WHERE ventype.TYPE = prmType NO-LOCK NO-ERROR.
     IF NOT AVAILABLE ventype THEN  DO:
         cError = "Invalid Type , Try help ... " .
         RETURN .
     END.

     FIND FIRST buyer WHERE buyer.buyer = prmBuyer  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE buyer  THEN DO:
         cError = "Invalid Buyer , Try help ... " .
         RETURN .
      END.

     FIND FIRST currency WHERE currency.c-code = prmCurrcode   NO-LOCK NO-ERROR.
      IF NOT AVAILABLE currency  THEN DO:
       cError = "Invalid Currency , Try help ... " .
         RETURN .
      END.
     FIND FIRST account WHERE account.actnum = prmActnum   NO-LOCK NO-ERROR.
      IF NOT  AVAILABLE account  THEN DO:
      cError = "Invalid Account No , Try help ... " .
         RETURN .
     END.

     FIND FIRST terms WHERE terms.t-code = prmTerms NO-LOCK NO-ERROR.
     IF NOT AVAILABLE terms  THEN DO:
        cError = "Invalid Terms , Try help ... " .
         RETURN .
     END.

    
     FIND FIRST carrier  WHERE carrier.carrier = prmCarrier NO-LOCK NO-ERROR.
      IF NOT AVAILABLE carrier  THEN DO:
      cError = "Invalid Carrier, Try help ... " .
         RETURN .
     END.

      FIND FIRST state  WHERE state.state = prmState NO-LOCK NO-ERROR.
      IF NOT AVAILABLE state  THEN DO:
      cError = "Invalid State, Try help ... " .
         RETURN .
     END.

     FIND FIRST stax  WHERE stax.company = cocode 
         AND stax.tax-group = prmTaxgr NO-LOCK NO-ERROR.
      IF NOT AVAILABLE stax  THEN DO:
      cError = "Invalid Tax Group, Try help ... " .
         RETURN .
     END.

     prmCode1099  = CAPS(prmCode1099 ).

    IF LOOKUP(TRIM(prmCode1099 ),",Y,N") LE 0 THEN DO:
      cError =  "1099code" + " " + "may be space, Y, or N..." .
      RETURN .
    END.


    ls-field-value = TRIM(prmPoexport).

    IF ls-field-value GT "" THEN DO:
      IF CAN-DO(name-fld-list,"POEXPORT") THEN DO:
        ls-name-value = str-init[LOOKUP("POEXPORT",name-fld-list)].

        IF ls-field-value EQ "Vendor" THEN
          lv-msg = TRIM(prmPoexport) + "cannot be Vendor...".
        ELSE
        IF CAN-DO(ls-name-value,ls-field-value) THEN
          prmPoexport = ENTRY(LOOKUP(ls-field-value,ls-name-value),ls-name-value).
        ELSE
          lv-msg = TRIM(ls-field-value) + " is not on lookup...".
      END.
    END.

    IF lv-msg NE "" THEN DO:
      cError = TRIM(lv-msg) .
      RETURN .
    END.


    FIND FIRST state  WHERE state.state = prmRstate NO-LOCK NO-ERROR.
      IF NOT AVAILABLE state  THEN DO:
      cError = "Invalid State, Try help ... " .
         RETURN .
     END.

END.


IF prmAction = "Add" THEN DO:

       CREATE vend.

       ASSIGN
        vend.company    =          cocode 
        vend.active     =          prmActive            
        vend.vend       =          prmVendor       
        vend.name       =          prmName         
        vend.add1       =          prmAdd1         
        vend.add2       =          prmAdd2         
        vend.city       =          prmCity         
        vend.state      =          prmState        
        vend.zip        =          prmZip          
        vend.country    =          prmCountry      
        vend.Postal     =          prmPostal       
        vend.tax-id     =          prmTaxid        
        vend.remit      =          prmRemit        
        vend.r-add1     =          prmRadd1        
        vend.r-add2     =          prmRadd2        
        vend.r-city     =          prmRcity        
        vend.r-state    =          prmRstate       
        vend.r-zip      =          prmRzip         
        vend.r-country  =          prmRcountry     
        vend.r-postal   =          prmRpostal      
        vend.check-memo =          prmCheckmemo    
        vend.type       =          prmType         
        vend.contact    =          prmContact      
        vend.buyer      =          prmBuyer        
        vend.area-code  =          prmAreacode     
        vend.phone      =          prmPhone        
        vend.fax-area   =          prmFaxarea      
        vend.fax        =          prmFax          
        vend.fax-prefix =          prmFaxprefix    
        vend.fax-country=          prmFaxcountry   
        vend.over-pct   =          prmOverpct      
        vend.under-pct  =          prmUnderpct     
        vend.actnum     =          prmActnum       
        vend.curr-code  =          prmCurrcode     
        vend.tax-gr     =          prmTaxgr        
        vend.code-1099  =          prmCode1099     
        vend.terms      =          prmTerms        
        vend.disc-%     =          prmDisc         
        vend.rebate-%   =          prmRebate       
        vend.frt-pay    =          prmFrtpay       
        vend.disc-days  =          prmDiscdays     
        vend.carrier    =          prmCarrier      
        vend.fob-code   =          prmFobcode      . 

        ASSIGN v-old-poexport = vend.po-export.
         ASSIGN vend.an-edi-vend = IF prmAnedivend = "yes" THEN YES ELSE NO .
        ASSIGN vend.po-export = prmPoexport .

    IF prmPoexport = "" AND  v-old-poexport NE ""
     THEN ASSIGN vend.po-export = v-old-poexport .
         
       FIND FIRST account WHERE account.company = cocode
                            AND account.actnum = vend.actnum NO-LOCK NO-ERROR.
       IF AVAIL account THEN vend.actdscr = account.dscr.


        IF vend-log-f THEN 
         RUN vend-new-log(vend-char-f).

       ASSIGN
           prmReckey = vend.rec_key
           prmAction = "View" .
                                 


END.


IF prmAction = "Update" THEN DO:

    FIND FIRST vend WHERE vend.company = prmComp AND vend.rec_key = prmReckey
                                                             NO-LOCK NO-ERROR.

   /* IF prmVendor EQ "" OR
       CAN-FIND(FIRST b-vend WHERE b-vend.company EQ cocode
                               AND b-vend.vend-no EQ prmVendor
                               AND ROWID(b-vend)  NE ROWID(vend)) THEN DO:
      cError = TRIM(prmVendor) + " " +
              TRIM(IF prmVendor EQ "" THEN "may not be spaces" ELSE "already exists") +
              "..." .
      RETURN .
    END.*/

    FIND FIRST ventype WHERE ventype.TYPE = prmType NO-LOCK NO-ERROR.
     IF NOT AVAILABLE ventype THEN  DO:
         cError = "Invalid Type , Try help ... " .
         RETURN .
     END.

     FIND FIRST buyer WHERE buyer.buyer = prmBuyer  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE buyer  THEN DO:
         cError = "Invalid Buyer , Try help ... " .
         RETURN .
      END.

     FIND FIRST currency WHERE currency.c-code = prmCurrcode   NO-LOCK NO-ERROR.
      IF NOT AVAILABLE currency  THEN DO:
       cError = "Invalid Currency , Try help ... " .
         RETURN .
      END.
     FIND FIRST account WHERE account.actnum = prmActnum   NO-LOCK NO-ERROR.
      IF NOT  AVAILABLE account  THEN DO:
      cError = "Invalid Account No , Try help ... " .
         RETURN .
     END.

     FIND FIRST terms WHERE terms.t-code = prmTerms NO-LOCK NO-ERROR.
     IF NOT AVAILABLE terms  THEN DO:
        cError = "Invalid Terms , Try help ... " .
         RETURN .
     END.

    
     FIND FIRST carrier  WHERE carrier.carrier = prmCarrier NO-LOCK NO-ERROR.
      IF NOT AVAILABLE carrier  THEN DO:
      cError = "Invalid Carrier, Try help ... " .
         RETURN .
     END.

      FIND FIRST state  WHERE state.state = prmState NO-LOCK NO-ERROR.
      IF NOT AVAILABLE state  THEN DO:
      cError = "Invalid State, Try help ... " .
         RETURN .
     END.

     FIND FIRST stax  WHERE stax.company = cocode 
         AND stax.tax-group = prmTaxgr NO-LOCK NO-ERROR.
      IF NOT AVAILABLE stax  THEN DO:
      cError = "Invalid Tax Group, Try help ... " .
         RETURN .
     END.

     prmCode1099  = CAPS(prmCode1099 ).

    IF LOOKUP(TRIM(prmCode1099 ),",Y,N") LE 0 THEN DO:
      cError =  "1099code" + " " + "may be space, Y, or N..." .
      RETURN .
    END.


    ls-field-value = TRIM(prmPoexport).

    IF ls-field-value GT "" THEN DO:
      IF CAN-DO(name-fld-list,"POEXPORT") THEN DO:
        ls-name-value = str-init[LOOKUP("POEXPORT",name-fld-list)].

        IF ls-field-value EQ "Vendor" THEN
          lv-msg = TRIM(prmPoexport) + "cannot be Vendor...".
        ELSE
        IF CAN-DO(ls-name-value,ls-field-value) THEN
          prmPoexport = ENTRY(LOOKUP(ls-field-value,ls-name-value),ls-name-value).
        ELSE
          lv-msg = TRIM(ls-field-value) + " is not on lookup...".
      END.
    END.

    IF lv-msg NE "" THEN DO:
      cError = TRIM(lv-msg) .
      RETURN .
    END.


    FIND FIRST state  WHERE state.state = prmRstate NO-LOCK NO-ERROR.
      IF NOT AVAILABLE state  THEN DO:
      cError = "Invalid State, Try help ... " .
         RETURN .
     END.
  

END.

    
IF prmAction = "Update" THEN DO:
    
     FIND FIRST vend WHERE vend.company = prmComp AND vend.rec_key = prmReckey
                                                              EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL vend THEN
       ASSIGN
        vend.active     =          prmActive   
        vend.name       =          prmName         
        vend.add1       =          prmAdd1         
        vend.add2       =          prmAdd2         
        vend.city       =          prmCity         
        vend.state      =          prmState        
        vend.zip        =          prmZip          
        vend.country    =          prmCountry      
        vend.Postal     =          prmPostal       
        vend.tax-id     =          prmTaxid        
        vend.remit      =          prmRemit        
        vend.r-add1     =          prmRadd1        
        vend.r-add2     =          prmRadd2        
        vend.r-city     =          prmRcity        
        vend.r-state    =          prmRstate       
        vend.r-zip      =          prmRzip         
        vend.r-country  =          prmRcountry     
        vend.r-postal   =          prmRpostal      
        vend.check-memo =          prmCheckmemo    
        vend.type       =          prmType         
        vend.contact    =          prmContact      
        vend.buyer      =          prmBuyer        
        vend.area-code  =          prmAreacode     
        vend.phone      =          prmPhone        
        vend.fax-area   =          prmFaxarea      
        vend.fax        =          prmFax          
        vend.fax-prefix =          prmFaxprefix    
        vend.fax-country=          prmFaxcountry   
        vend.over-pct   =          prmOverpct      
        vend.under-pct  =          prmUnderpct     
        vend.actnum     =          prmActnum       
        vend.curr-code  =          prmCurrcode     
        vend.tax-gr     =          prmTaxgr        
        vend.code-1099  =          prmCode1099     
        vend.terms      =          prmTerms        
        vend.disc-%     =          prmDisc         
        vend.rebate-%   =          prmRebate       
        vend.frt-pay    =          prmFrtpay       
        vend.disc-days  =          prmDiscdays     
        vend.carrier    =          prmCarrier      
        vend.fob-code   =          prmFobcode      . 

        ASSIGN v-old-poexport = vend.po-export.
        ASSIGN vend.an-edi-vend = IF prmAnedivend = "yes" THEN YES ELSE NO .
        
        ASSIGN vend.po-export = prmPoexport .

    IF prmPoexport = "" AND  v-old-poexport NE ""
     THEN ASSIGN vend.po-export = v-old-poexport .
       
    FIND FIRST account WHERE account.company = cocode
                            AND account.actnum = vend.actnum NO-LOCK NO-ERROR.
       IF AVAIL account THEN vend.actdscr = account.dscr.
 
       IF vend-log-f THEN 
         RUN vend-update-log(vend-char-f).

    ASSIGN
        prmAction = "View" .
  
END. /*IF prmAction = "Update" THEN DO:*/

IF prmAction = "Delete" THEN DO:

   FIND FIRST vend WHERE vend.company = prmComp AND vend.rec_key = prmReckey
            EXCLUSIVE-LOCK NO-ERROR.
   IF vend-log-f THEN do:
      DO I = 1 TO NUM-ENTRIES(vend-char-f):
          ASSIGN thisOne = ENTRY(i,vend-char-f).
          FIND FIRST buff-vend WHERE buff-vend.vend-no = vend.vend-no 
                                  AND buff-vend.company = thisOne EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL buff-vend THEN
              DELETE buff-vend .
      END.
   END.

 IF AVAIL vend  THEN 
     DELETE vend .
 

 FIND LAST vend WHERE vend.company = cocode NO-LOCK NO-ERROR.
 IF AVAIL vend  THEN
     ASSIGN 
     prmAction = "View"
     prmReckey = vend.rec_key .

END. /*IF prmAction = "Delete" THEN DO:*/



IF prmAction = "View"  THEN DO:

    FIND FIRST vend WHERE vend.company = prmComp AND vend.rec_key = prmReckey
        NO-LOCK NO-ERROR.
    IF AVAILABLE vend THEN DO:

        CREATE  ttVendorUpdate.
            ASSIGN 
                 ttVendorUpdate.vactive          = vend.active 
                 ttVendorUpdate.vendor           = vend.vend
                 ttVendorUpdate.vname            = vend.name         
                 ttVendorUpdate.vadd1            = vend.add1         
                 ttVendorUpdate.vadd2            = vend.add2         
                 ttVendorUpdate.vcity            = vend.city         
                 ttVendorUpdate.vstate           = vend.state        
                 ttVendorUpdate.vzip             = vend.zip          
                 ttVendorUpdate.vcountry         = vend.country      
                 ttVendorUpdate.vPostal          = vend.Postal       
                 ttVendorUpdate.vtaxid           = vend.tax-id       
                 ttVendorUpdate.vremit           = vend.remit        
                 ttVendorUpdate.vradd1           = vend.r-add1       
                 ttVendorUpdate.vradd2           = vend.r-add2       
                 ttVendorUpdate.vrcity           = vend.r-city       
                 ttVendorUpdate.vrstate          = vend.r-state      
                 ttVendorUpdate.vrzip            = vend.r-zip        
                 ttVendorUpdate.vrcountry        = vend.r-country    
                 ttVendorUpdate.vrpostal         = vend.r-postal     
                 ttVendorUpdate.vcheckmemo       = vend.check-memo   
                 ttVendorUpdate.vtype            = vend.type         
                 ttVendorUpdate.vcontact         = vend.contact      
                 ttVendorUpdate.vbuyer           = vend.buyer        
                 ttVendorUpdate.vareacode        = vend.area-code    
                 ttVendorUpdate.vphone           = vend.phone        
                 ttVendorUpdate.vfaxarea         = vend.fax-area     
                 ttVendorUpdate.vfax             = vend.fax          
                 ttVendorUpdate.vfaxprefix       = vend.fax-prefix   
                 ttVendorUpdate.vfaxcountry      = vend.fax-country  
                 ttVendorUpdate.voverpct         = vend.over-pct     
                 ttVendorUpdate.vunderpct        = vend.under-pct    
                 ttVendorUpdate.vactnum          = vend.actnum       
                 ttVendorUpdate.vcurrcode        = vend.curr-code    
                 ttVendorUpdate.vtaxgr           = vend.tax-gr       
                 ttVendorUpdate.vcode1099        = vend.code-1099    
                 ttVendorUpdate.vanedivend       =  string(vend.an-edi-vend  )
                 ttVendorUpdate.vterms           = vend.terms        
                 ttVendorUpdate.vdisc            = vend.disc-%       
                 ttVendorUpdate.vrebate          = vend.rebate-%     
                 ttVendorUpdate.vfrtpay          = vend.frt-pay      
                 ttVendorUpdate.vdiscdays        = vend.disc-days    
                 ttVendorUpdate.vcarrier         = vend.carrier      
                 ttVendorUpdate.vfobcode         = vend.fob-code   
                 ttVendorUpdate.vreckey          = vend.rec_key  
                 ttVendorUpdate.vpoexport        = vend.po-export   .
                 
                 
                
                 
                
                 
                /* */
                


     FIND FIRST ventype WHERE ventype.TYPE = vend.TYPE NO-LOCK NO-ERROR.
     IF AVAILABLE ventype THEN  DO:
         ASSIGN
             ttVendorUpdate.vtypedscr        =  ventype.dscr.
     END.

     FIND FIRST terms WHERE terms.t-code = vend.terms NO-LOCK NO-ERROR.
     IF  AVAILABLE terms  THEN DO:
         ASSIGN
              ttVendorUpdate.vtermsdscr       =  terms.dscr.
     END.

    
     FIND FIRST carrier  WHERE carrier.carrier = vend.carrier NO-LOCK NO-ERROR.
      IF  AVAILABLE carrier  THEN DO:
       ASSIGN
          ttVendorUpdate.vcarrierdscr     = carrier.dscr.
     END.

     FIND FIRST buyer WHERE buyer.buyer = vend.buyer  NO-LOCK NO-ERROR.
      IF  AVAILABLE buyer  THEN DO:
      ASSIGN
         ttVendorUpdate.vbuyerdscr       =   buyer.buyer-n.
     END.

      FIND FIRST currency WHERE currency.c-code = vend.curr-code   NO-LOCK NO-ERROR.
      IF  AVAILABLE currency  THEN DO:
      ASSIGN
           ttVendorUpdate.vcurrdscr        =  currency.c-desc .
      END.
     FIND FIRST account WHERE account.actnum = vend.actnum   NO-LOCK NO-ERROR.
      IF  AVAILABLE account  THEN DO:
      ASSIGN
          ttVendorUpdate.vactdscr         =  account.dscr.
     END.

    

    END.

END. /*IF prmAction = "View" THEN DO:*/





PROCEDURE vend-update-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER vend-char AS CHAR NO-UNDO.
 FIND CURRENT vend NO-LOCK.
 DEF VAR thisOne AS CHAR NO-UNDO.
 DEFINE BUFFER bf-vend FOR vend .

   DO I = 1 TO NUM-ENTRIES(vend-char):
     ASSIGN thisOne = ENTRY(i,vend-char).
     FIND FIRST bf-vend WHERE bf-vend.vend-no = vend.vend-no 
                          AND bf-vend.company = thisOne EXCLUSIVE-LOCK NO-ERROR.
     IF AVAIL bf-vend THEN do:
     BUFFER-COPY vend EXCEPT company frt-pay Purch last-year ytd-msf lyytd-msf hibal hibal-date num-inv lpay 
         lpay-date avg-pay acc-bal ord-bal TO bf-vend.
     END.
     ELSE DO:
     CREATE bf-vend .
     BUFFER-COPY vend EXCEPT company frt-pay Purch last-year ytd-msf lyytd-msf hibal hibal-date num-inv lpay 
         lpay-date avg-pay acc-bal ord-bal TO bf-vend.
     ASSIGN bf-vend.company = thisone.
     END.
    END.

END PROCEDURE.


PROCEDURE vend-new-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER vend-char AS CHAR NO-UNDO.

 FIND CURRENT vend NO-LOCK.
 DO I = 1 TO NUM-ENTRIES(vend-char):
     ASSIGN thisOne = ENTRY(i,vend-char).
     CREATE buff-vend .
     BUFFER-COPY vend EXCEPT company  TO buff-vend.
     ASSIGN buff-vend.company = thisone.
 END.

END PROCEDURE.
