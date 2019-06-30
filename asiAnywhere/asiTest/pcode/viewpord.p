

/*------------------------------------------------------------------------
    File        : viewpord.p
    Purpose     : 
    Syntax      :

    Description : Return a Dataset of Estimate Corrugated box
    Author(s)   : 
    Created     : 14 Jan 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttViewPurOrd NO-UNDO
        FIELD vPoNo            AS INT 
        FIELD vPoDate          AS CHAR  
        FIELD vType            AS CHAR FORMAT "x(1)"   
        FIELD vStat            AS CHAR FORMAT "x(1)"
        FIELD vVendNo          AS CHAR FORMAT "x(8)"
        FIELD vVendName        AS CHAR FORMAT "x(30)"         
        FIELD vVendAdd1        AS CHAR FORMAT "x(30)"
        FIELD vVendAdd2        AS CHAR FORMAT "x(30)"        
        FIELD vVendCity        AS CHAR FORMAT "x(16)"        
        FIELD vVendState       AS CHAR FORMAT "x(2)"         
        FIELD vVendZip         AS CHAR FORMAT "x(10)"
        FIELD vVendAreaCode    AS CHAR
        FIELD vVendPhone       AS CHAR
        FIELD vShipId          AS CHAR FORMAT "x(8)"  
        FIELD vShipName        AS CHAR FORMAT "x(30)"
        FIELD vShipAddr        AS CHAR EXTENT 2 
        FIELD vShipCity        AS CHAR  FORMAT "x(16)" 
        FIELD vShipState       AS CHAR  FORMAT "x(2)"  
        FIELD vShipZip         AS CHAR FORMAT "x(10)" 
        FIELD vShipAreaCode    AS CHAR   
        FIELD vShipPhone       AS CHAR 
        FIELD vBuyer           AS CHAR FORMAT "x(10)" 
        FIELD vContact         AS CHAR FORMAT "x(25)"     
        FIELD vDueDate         AS CHAR    
        FIELD vLastShipDate    AS CHAR
        FIELD vUnderPct        AS INT FORMAT ">>9.99%"
        FIELD vOverPct         AS INT FORMAT ">>9.99%" 
        FIELD vCarrier         AS CHAR FORMAT "x(5)"
        FIELD vTaxGr           AS CHAR FORMAT "x(3)"
        FIELD vTerms           AS CHAR FORMAT "x(5)"   
        FIELD vFrtPay          AS CHAR   FORMAT "x(1)"
        FIELD vFobCode         AS CHAR  FORMAT "x(5)"
        FIELD vTFreight        AS INT FORMAT "->>,>>9.99"
        FIELD vTax             AS INT FORMAT "->,>>9.99"
        FIELD vTCost           AS INT FORMAT "->,>>>,>>9.99<<"
        FIELD vRecKey          AS CHAR. 
        
        


DEFINE DATASET dsViewPurOrd FOR ttViewPurOrd .

DEFINE INPUT PARAMETER prmUser            AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmPoNo            AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmPoDate          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmType            AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmStat            AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVendNo          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVendName        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVendAdd1        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVendAdd2        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVendCity        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVendState       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVendZip         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVendAreaCode    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVendPhone       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmShipId          AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER prmShipName        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmShipAddr        AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmShipCity        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmShipState       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmShipZip         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmShipAreaCode    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmShipPhone       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBuyer           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmContact         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmDueDate         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLastShipDate    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmUnderPct        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmOverPct         AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmCarrier         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTaxGr           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTerms           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFrtPay          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFobCode         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTFreight        AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmTax             AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmTCost           AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey          AS CHAR        NO-UNDO.
DEFINE OUTPUT PARAMETER cError            AS CHAR        NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewPurOrd.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
  DEF BUFFER bx-notes FOR notes.
{sys/inc/var.i "new shared"}
 

IF prmUser            = ? THEN ASSIGN prmUser           = "".
IF prmAction          = ? THEN ASSIGN prmAction         = "Select".
IF prmPoNo            = ? THEN ASSIGN prmPoNo           = 0.
IF prmPoDate          = ? THEN ASSIGN prmPoDate         = "".  
IF prmType            = ? THEN ASSIGN prmType           = "".
IF prmStat            = ? THEN ASSIGN prmStat           = "".
IF prmVendNo          = ? THEN ASSIGN prmVendNo         = "".
IF prmVendName        = ? THEN ASSIGN prmVendName       = "".
IF prmVendAdd1        = ? THEN ASSIGN prmVendAdd1       = "".
IF prmVendAdd2        = ? THEN ASSIGN prmVendAdd2       = "".
IF prmVendCity        = ? THEN ASSIGN prmVendCity       = "".
IF prmVendState       = ? THEN ASSIGN prmVendState      = "".
IF prmVendZip         = ? THEN ASSIGN prmVendZip        = "".
IF prmVendAreaCode    = ? THEN ASSIGN prmVendAreaCode   = "".
IF prmVendPhone       = ? THEN ASSIGN prmVendPhone      = "".
IF prmShipId          = ? THEN ASSIGN prmShipId         = "".
IF prmShipName        = ? THEN ASSIGN prmShipName       = "".
IF prmShipAddr        = ? THEN ASSIGN prmShipAddr       = "".
IF prmShipCity        = ? THEN ASSIGN prmShipCity       = "".
IF prmShipState       = ? THEN ASSIGN prmShipState      = "".
IF prmShipZip         = ? THEN ASSIGN prmShipZip        = "".
IF prmShipAreaCode    = ? THEN ASSIGN prmShipAreaCode   = "".
IF prmShipPhone       = ? THEN ASSIGN prmShipPhone    = "".
IF prmBuyer           = ? THEN ASSIGN prmBuyer          = "".
IF prmContact         = ? THEN ASSIGN prmContact        = "".
IF prmDueDate         = ? THEN ASSIGN prmDueDate        = "".
IF prmLastShipDate    = ? THEN ASSIGN prmLastShipDate   = "".
IF prmUnderPct        = ? THEN ASSIGN prmUnderPct       = 0.
IF prmOverPct         = ? THEN ASSIGN prmOverPct        = 0.
IF prmCarrier         = ? THEN ASSIGN prmCarrier        = "".
IF prmTaxGr           = ? THEN ASSIGN prmTaxGr          = "".
IF prmTerms           = ? THEN ASSIGN prmTerms          = "".
IF prmFrtPay          = ? THEN ASSIGN prmFrtPay         = "".
IF prmFobCode         = ? THEN ASSIGN prmFobCode        = "".
IF prmTFreight        = ? THEN ASSIGN prmTFreight       = 0.
IF prmTax             = ? THEN ASSIGN prmTax            = 0.
IF prmTCost           = ? THEN ASSIGN prmTCost          = 0.
IF prmRecKey          = ? THEN ASSIGN prmRecKey         = "".

DEF VAR ls-drop-custno AS cha NO-UNDO.
DEF VAR ll-got-vendor AS LOG NO-UNDO. 
DEF VAR lv-ship-no LIKE shipto.ship-no NO-UNDO.
DEF VAR fil_id AS RECID NO-UNDO.
 DEF VAR ls-key AS cha NO-UNDO.
 DEF BUFFER b-po-ord FOR po-ord .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp
    g_company = prmComp 
    locode  =  "Main"
    g_loc   = locode .


               

IF prmAction = "validateadd" THEN DO: 
    FIND FIRST po-ord WHERE 
       po-ord.company eq cocode and 
           po-ord.po-no EQ prmPoNo NO-LOCK NO-ERROR. 

     FIND FIRST vend
         WHERE vend.company EQ cocode
         AND vend.vend-no EQ prmVendNo NO-LOCK NO-ERROR.
     IF NOT AVAIL vend   OR
         (vend.active NE "A" ) THEN DO:
         IF AVAIL vend THEN
             cError =  TRIM(prmVendNo) + " not active, try help..."  .
         ELSE 
             cError = "Invalid Vendor#" + TRIM(prmVendNo) + ", try help..." .
          RETURN .
     END.

     prmType = CAPS(prmType).

    IF INDEX("RDS",prmType) LE 0 THEN DO:
      cError = TRIM(prmType) +
              " is invalid, enter 'R'egular, 'D'rop ship, or 'S'heets from Roll"  .
         RETURN .
    END.

    IF prmCarrier NE "" THEN DO:
       FIND FIRST carrier WHERE carrier.company = cocode AND
                                carrier.carrier = prmCarrier
                   NO-LOCK NO-ERROR.
       IF NOT AVAIL carrier THEN DO:
          cError =  "Invalid Carrier. Try Help. " .
          RETURN .
       END.
    END.

    IF prmTaxGr NE "" AND
       NOT CAN-FIND(FIRST stax
                    WHERE stax.company   EQ cocode
                      AND stax.tax-group EQ prmTaxGr)
    THEN DO:
      cError =  TRIM(prmTaxGr) + " is invalid, try help..." .
      RETURN .
    END.
 
    IF prmTerms <> "" THEN DO:
       FIND FIRST terms WHERE terms.company = cocode AND
                              terms.t-code = prmTerms NO-LOCK NO-ERROR.
       IF NOT AVAIL terms THEN DO:
          cError = "Invalid Terms. Try Help. " .
          RETURN .
       END.
    END.
    
            
END.  /*** validate add*/  

IF prmAction = "Update" THEN DO: 
    FIND FIRST po-ord WHERE 
       po-ord.company eq cocode and 
           po-ord.po-no EQ prmPoNo NO-LOCK NO-ERROR. 

     FIND FIRST vend
         WHERE vend.company EQ cocode
         AND vend.vend-no EQ prmVendNo NO-LOCK NO-ERROR.
     IF NOT AVAIL vend   OR
         (vend.active NE "A" ) THEN DO:
         IF AVAIL vend THEN
             cError =  TRIM(prmVendNo) + " not active, try help..."  .
         ELSE 
             cError = "Invalid " + TRIM(prmVendNo) + ", try help..." .
          RETURN .
     END.

     prmType = CAPS(prmType).

    IF INDEX("RDS",prmType) LE 0 THEN DO:
      cError = TRIM(prmType) +
              " is invalid, enter 'R'egular, 'D'rop ship, or 'S'heets from Roll"  .
         RETURN .
    END.

    IF prmCarrier NE "" THEN DO:
       FIND FIRST carrier WHERE carrier.company = cocode AND
                                carrier.carrier = prmCarrier
                   NO-LOCK NO-ERROR.
       IF NOT AVAIL carrier THEN DO:
          cError =  "Invalid Carrier. Try Help. " .
          RETURN .
       END.
    END.

    IF prmTaxGr NE "" AND
       NOT CAN-FIND(FIRST stax
                    WHERE stax.company   EQ cocode
                      AND stax.tax-group EQ prmTaxGr)
    THEN DO:
      cError =  TRIM(prmTaxGr) + " is invalid, try help..." .
      RETURN .
    END.
 
    IF prmTerms <> "" THEN DO:
       FIND FIRST terms WHERE terms.company = cocode AND
                              terms.t-code = prmTerms NO-LOCK NO-ERROR.
       IF NOT AVAIL terms THEN DO:
          cError = "Invalid Terms. Try Help. " .
          RETURN .
       END.
    END.
    
            
END.  /*** update*/  

IF prmAction = "Update" THEN DO:
    
    FIND FIRST po-ord WHERE 
      po-ord.company eq cocode and 
          po-ord.po-no EQ prmPoNo EXCLUSIVE-LOCK NO-ERROR. 

  
 ASSIGN po-ord.vend-no      = prmVendNo
        po-ord.TYPE         = prmType              
        po-ord.stat         = prmStat  
        po-ord.vend-no      = prmVendNo
       /* po-ord.ship-id      = prmShipId        
        po-ord.ship-name    = prmShipName
        po-ord.ship-addr[1]    = prmShipAddr    
        po-ord.ship-addr[2]    = prmVendAdd1
        po-ord.ship-city    = prmShipCity      
        po-ord.ship-state   = prmShipState    
        po-ord.ship-zip     = prmShipZip  */
        po-ord.contact      = prmContact        
        po-ord.due-date     = date(prmDueDate)
        po-ord.carrier      = prmCarrier       
        po-ord.tax-gr       = prmTaxGr        
        po-ord.terms        = prmTerms          
        po-ord.frt-pay      = prmFrtPay       
        po-ord.fob-code     = prmFobCode      
        po-ord.t-freight    = prmTFreight      
        po-ord.tax          = prmTax           
        po-ord.t-cost       = prmTCost
        po-ord.under-pct    = prmUnderPct
        po-ord.over-pct     = prmOverPct 
        po-ord.due-date     =  date(prmDueDate)
                                    
        po-ord.last-ship-date = date(prmLastShipDate)
        po-ord.po-date        = DATE(prmPoDate)
        .        

 FIND FIRST vend NO-LOCK
           WHERE vend.company EQ po-ord.company
             AND vend.vend-no EQ po-ord.vend-no
           NO-ERROR.
       IF AVAIL vend THEN 
           assign
           po-ord.tax-gr = vend.tax-gr .

       IF po-ord.rec_key = "" THEN DO:
       
        ASSIGN
        ls-key = string(today,"99999999") +
                  string(next-value(rec_key_seq,asi),"99999999")
        po-ord.rec_key = ls-key.               
        create rec_key.
        assign rec_key.rec_key = po-ord.rec_key
               rec_key.table_name = "PO".
        END.


        FOR EACH bx-notes WHERE bx-notes.rec_key = vend.rec_key
                         AND bx-notes.note_type = "G"
                         AND bx-notes.note_group = "PO" NO-LOCK:
         FIND FIRST notes WHERE notes.rec_key = po-ord.rec_key 
                            AND notes.note_title = bx-notes.note_title NO-ERROR.
         IF NOT AVAIL notes THEN CREATE notes.
          
         ASSIGN notes.rec_key = po-ord.rec_key
                notes.note_date = TODAY .
         BUFFER-COPY bx-notes EXCEPT bx-notes.rec_key bx-notes.note_date TO notes.
   
        END.  /* for each bx-notes  vendor notes */

        FOR EACH po-ordl WHERE
         po-ordl.company = po-ord.company AND
         po-ordl.po-no = po-ord.po-no:
        ASSIGN
             po-ordl.vend-no = po-ord.vend-no
             po-ordl.due-date = po-ord.due-date.
        END.
    
      RUN po/po-total.p (RECID(po-ord)).
  
  ASSIGN prmAction = "View" .
    


END.  /**** update  ***/ 

IF prmAction = "Addnewpo" THEN DO:
     
    DEF BUFFER b-po-ordl FOR po-ordl.
  
  find first po-ctrl where po-ctrl.company eq cocode
      exclusive-LOCK NO-ERROR.
  CREATE po-ord.

  ASSIGN po-ord.company        = cocode
         po-ord.po-no          = po-ctrl.next-po-no
         po-ctrl.next-po-no    = po-ord.po-no + 1
         po-ord.po-date        = today
         po-ord.loc            = locode
         po-ord.buyer          = prmUser  /*global-uid*/
         po-ord.under-pct      = 10
         po-ord.over-pct       = 10         
         po-ord.due-date       = po-ord.po-date + 
                                 if weekday(po-ord.po-date) eq 6 then 3 else 1
         po-ord.last-ship-date = po-ord.due-date
         fil_id                = recid (po-ord)
         po-ord.frt-pay        = "P"                                
         po-ord.fob-code       = "Orig" .

  {po/pouserid.i}
 
  ls-drop-custno = "".

  CREATE b-po-ordl.
  ASSIGN
   b-po-ordl.company  = po-ord.company
   b-po-ordl.po-no    = po-ord.po-no
   b-po-ordl.line     = 0
   b-po-ordl.due-date = po-ord.due-date.

  

  FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.
  IF AVAILABLE company THEN ASSIGN po-ord.ship-id = company.company
                                   po-ord.ship-name = company.NAME
                                   po-ord.ship-addr[1] = company.addr[1]
                                   po-ord.ship-addr[2] = company.addr[2]
                                   po-ord.ship-city = company.city
                                   po-ord.ship-state = company.state
                                   po-ord.ship-zip = company.zip.

 RELEASE po-ctrl.

 ASSIGN prmAction = "View"
        prmPoNo   = po-ord.po-no 
         .



END. /* add new po-ord*/

IF prmAction = "Addpo" THEN DO:

 FIND FIRST po-ord WHERE 
      po-ord.company eq cocode and 
          po-ord.po-no EQ prmPoNo EXCLUSIVE-LOCK NO-ERROR. 

 ASSIGN po-ord.vend-no      = prmVendNo
        po-ord.TYPE         = prmType              
        po-ord.stat         = prmStat  
        po-ord.vend-no      = prmVendNo
        /*po-ord.ship-id      = prmShipId        
        po-ord.ship-name    = prmShipName
        po-ord.ship-addr[1]    = prmShipAddr    
        po-ord.ship-addr[2]    = prmVendAdd1
        po-ord.ship-city    = prmShipCity      
        po-ord.ship-state   = prmShipState    
        po-ord.ship-zip     = prmShipZip  */
        po-ord.contact      = prmContact        
        po-ord.due-date     = date(prmDueDate)
        po-ord.carrier      = prmCarrier       
        po-ord.tax-gr       = prmTaxGr        
        po-ord.terms        = prmTerms          
        po-ord.frt-pay      = prmFrtPay       
        po-ord.fob-code     = prmFobCode      
        po-ord.t-freight    = prmTFreight      
        po-ord.tax          = prmTax           
        po-ord.t-cost       = prmTCost
        po-ord.under-pct    = prmUnderPct
        po-ord.over-pct     = prmOverPct 
        po-ord.due-date     =  date(prmDueDate)
                                    
        po-ord.last-ship-date = date(prmLastShipDate)
        po-ord.po-date        = DATE(prmPoDate)
        .        

 FIND FIRST vend NO-LOCK
           WHERE vend.company EQ po-ord.company
             AND vend.vend-no EQ po-ord.vend-no
           NO-ERROR.
       IF AVAIL vend THEN 
           assign
           po-ord.tax-gr = vend.tax-gr .

       IF po-ord.rec_key = "" THEN DO:
       
        ASSIGN
        ls-key = string(today,"99999999") +
                  string(next-value(rec_key_seq,asi),"99999999")
        po-ord.rec_key = ls-key.               
        create rec_key.
        assign rec_key.rec_key = po-ord.rec_key
               rec_key.table_name = "PO".
        END.


        FOR EACH bx-notes WHERE bx-notes.rec_key = vend.rec_key
                         AND bx-notes.note_type = "G"
                         AND bx-notes.note_group = "PO" NO-LOCK:
         FIND FIRST notes WHERE notes.rec_key = po-ord.rec_key 
                            AND notes.note_title = bx-notes.note_title NO-ERROR.
         IF NOT AVAIL notes THEN CREATE notes.
          
         ASSIGN notes.rec_key = po-ord.rec_key
                notes.note_date = TODAY .
         BUFFER-COPY bx-notes EXCEPT bx-notes.rec_key bx-notes.note_date TO notes.
   
        END.  /* for each bx-notes  vendor notes */

        FOR EACH po-ordl WHERE
         po-ordl.company = po-ord.company AND
         po-ordl.po-no = po-ord.po-no:
        ASSIGN
             po-ordl.vend-no = po-ord.vend-no
             po-ordl.due-date = po-ord.due-date.
        END.
    
      RUN po/po-total.p (RECID(po-ord)).
  
  ASSIGN prmAction = "View"
          .

END.   /* add*********/

IF prmAction = "Deletepo" THEN DO:
    DEF VAR ll AS LOG NO-UNDO.
     ll = NO.
    FIND FIRST po-ord WHERE po-ord.company = cocode AND
         po-ord.po-no = int(prmRecKey) EXCLUSIVE-LOCK NO-ERROR.

    FOR EACH po-ordl
    WHERE po-ordl.company EQ po-ord.company
      AND po-ordl.po-no   EQ po-ord.po-no
    NO-LOCK:

    IF po-ordl.item-type THEN
      ll = CAN-FIND(FIRST rm-rctd
                    WHERE rm-rctd.company    EQ po-ordl.company
                      AND rm-rctd.rita-code  EQ "R"
                      AND rm-rctd.i-no       EQ po-ordl.i-no
                      AND INT(rm-rctd.po-no) EQ po-ordl.po-no
                    USE-INDEX rita-code) NO-ERROR.
    ELSE
      ll = CAN-FIND(FIRST fg-rctd
                    WHERE fg-rctd.company    EQ po-ordl.company
                      AND fg-rctd.rita-code  EQ "R"
                      AND fg-rctd.i-no       EQ po-ordl.i-no
                      AND INT(fg-rctd.po-no) EQ po-ordl.po-no
                    USE-INDEX rita-code) NO-ERROR.

  IF ll THEN DO:
    cError =  "Receipts and/or invoices have been entered against an item, cannot delete..." .
    RETURN .
  END.
    END.


    IF AVAIL po-ord THEN 
        DELETE po-ord .

    FIND FIRST po-ord WHERE po-ord.company = cocode AND
         po-ord.po-no = int(prmPoNo) NO-LOCK NO-ERROR.
  IF NOT AVAIL po-ord THEN DO:
    FIND LAST po-ord WHERE 
        po-ord.company eq cocode NO-LOCK NO-ERROR.
  END.
    IF AVAIL po-ord  THEN
        ASSIGN
        prmAction = "View" 
        prmPoNo  = po-ord.po-no .
END.  /* end of delete*/ 
      
 IF prmAction = "View" THEN DO:
    
  
  FIND FIRST b-po-ord WHERE b-po-ord.company = cocode AND
        b-po-ord.po-no = prmPoNo NO-LOCK NO-ERROR.
    
   FIND FIRST vend
        WHERE vend.company EQ b-po-ord.company
          AND vend.vend-no EQ b-po-ord.vend-no NO-LOCK NO-ERROR.
   FIND FIRST cust WHERE 
       cust.company EQ cocode NO-LOCK NO-ERROR.
             
        IF AVAIL b-po-ord THEN DO:
       
            create ttViewPurOrd.
            assign
                ttViewPurOrd.vPoNo               = b-po-ord.po-no
                ttViewPurOrd.vPoDate             = string(b-po-ord.po-date)
                ttViewPurOrd.vType               = b-po-ord.type
                ttViewPurOrd.vStat               = b-po-ord.stat
                ttViewPurOrd.vVendNo             = b-po-ord.vend-no
                ttViewPurOrd.vShipId             = b-po-ord.ship-id
                ttViewPurOrd.vShipName           = b-po-ord.ship-name
                ttViewPurOrd.vShipAddr[1]        = b-po-ord.ship-addr[1]
                ttViewPurOrd.vShipAddr[2]        = b-po-ord.ship-addr[2]
                ttViewPurOrd.vShipCity           = b-po-ord.ship-city
                ttViewPurOrd.vShipState          = b-po-ord.ship-state
                ttViewPurOrd.vShipZip            = b-po-ord.ship-zip
                ttViewPurOrd.vShipAreaCode       = IF AVAIL cust THEN cust.area-code ELSE ""
                ttViewPurOrd.vShipPhone          = IF AVAIL cust THEN cust.phone ELSE ""  
                ttViewPurOrd.vBuyer              = b-po-ord.buyer
                ttViewPurOrd.vContact            = b-po-ord.contact 
                ttViewPurOrd.vDueDate            = string(b-po-ord.due-date)  
                ttViewPurOrd.vLastShipDate       = string(b-po-ord.last-ship-date)
                ttViewPurOrd.vUnderPct           = b-po-ord.under-pct
                ttViewPurOrd.vOverPct            = b-po-ord.over-pct
                ttViewPurOrd.vCarrier            = b-po-ord.carrier
                ttViewPurOrd.vTaxGr              = b-po-ord.tax-gr
                ttViewPurOrd.vTerms              = b-po-ord.terms
                ttViewPurOrd.vFrtPay             = b-po-ord.frt-pay 
                ttViewPurOrd.vFobCode            = b-po-ord.fob-code
                ttViewPurOrd.vTFreight           = b-po-ord.t-freight  
                ttViewPurOrd.vTax                = b-po-ord.tax 
                ttViewPurOrd.vTCost              = b-po-ord.t-cost
                ttViewPurOrd.vRecKey             = b-po-ord.rec_key .
            IF ttViewPurOrd.vFobCode = "DEST" THEN
                ASSIGN ttViewPurOrd.vFobCode = "Dest" .
            IF ttViewPurOrd.vFobCode = "ORIG" THEN
                ASSIGN ttViewPurOrd.vFobCode = "Orig" .
            IF ttViewPurOrd.vFobCode = "" THEN
              ASSIGN ttViewPurOrd.vFobCode = "Dest" .
            IF ttViewPurOrd.vFrtPay = "" THEN
              ASSIGN ttViewPurOrd.vFrtPay = "B" .
            
                IF AVAIL vend THEN
                    ASSIGN 
                    ttViewPurOrd.vVendName           = vend.name 
                ttViewPurOrd.vVendAdd1           = vend.add1 
                ttViewPurOrd.vVendAdd2           = vend.add2 
                ttViewPurOrd.vVendCity           = vend.city 
                ttViewPurOrd.vVendState          = vend.state
                ttViewPurOrd.vVendZip            = vend.zip
                ttViewPurOrd.vVendAreaCode       = vend.area-code
                ttViewPurOrd.vVendPhone          = vend.phone.
                
        END.

             
             
  END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/
