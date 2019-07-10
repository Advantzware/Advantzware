/*------------------------------------------------------------------------
    File      : VendorLookup.p
    Purpose   :  Corrugated Customer Lookup
    Syntax    :

    Description : Return a Dataset of Corrugated Box

    Author(s)   : 
    Created     : 2 Feb 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCorVendorLook NO-UNDO 
        FIELD vVendor      AS character
        FIELD vVendName        AS CHARACTER
        FIELD vAdd         AS CHAR
        FIELD vCity        AS CHAR
        FIELD vState       AS CHAR
        FIELD vZip         AS CHAR
        FIELD vType         AS CHAR
        FIELD vBuyer        AS CHAR
        FIELD vphone        AS CHAR
        FIELD areacode      AS CHAR
        FIELD carrier       AS CHAR
        FIELD contact       AS CHAR
        FIELD terms         AS CHAR
        FIELD fob-code      AS CHAR
        FIELD frt-pay       AS CHAR
        FIELD over-pct      AS DEC
        FIELD under-pct     AS DEC
        FIELD tax-gr        AS CHAR
        FIELD add2          AS CHAR
        FIELD retyw        AS CHAR
        .
    
DEFINE DATASET dsCorVendorLook FOR ttCorVendorLook .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmActive  AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorVendorLook.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".
IF prmField       = ? THEN ASSIGN prmField       = "".
IF prmActive    = ? THEN ASSIGN prmActive    = "".

DEFINE VAR v-count AS INT NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


if prmAction <> "search" then do:
   
    FOR EACH vend WHERE vend.company EQ prmComp  AND (vend.active EQ prmActive OR prmActive eq "") NO-LOCK :
        IF AVAIL vend THEN DO:
             create ttCorVendorLook.
             assign  

                 ttCorVendorLook.vVendor     = vend.vend-no 
                 ttCorVendorLook.vVendName   = vend.name    
                 ttCorVendorLook.vAdd        = vend.add1    
                 ttCorVendorLook.vCity       = vend.city    
                 ttCorVendorLook.vState      = vend.state   
                 ttCorVendorLook.vZip        = vend.zip     
                 ttCorVendorLook.vType       = vend.type    
                 ttCorVendorLook.vBuyer      = vend.buyer   
                 ttCorVendorLook.vphone      = vend.phone
                 ttCorVendorLook.areacode    = vend.area-code
                 ttCorVendorLook.carrier     = vend.carrier
                 ttCorVendorLook.contact     = vend.contact
                 ttCorVendorLook.terms       = vend.terms
                 ttCorVendorLook.fob-code    = IF vend.fob-code NE "" THEN vend.fob-code ELSE "DEST"   
                 ttCorVendorLook.frt-pay     = IF vend.frt-pay NE "" THEN vend.frt-pay ELSE "B"        
                 ttCorVendorLook.over-pct    = vend.over-pct   
                 ttCorVendorLook.under-pct   = vend.under-pct
                 ttCorVendorLook.tax-gr      = vend.tax-gr            
                 ttCorVendorLook.add2        = vend.add2 .
                                             
         END.	 /* IF AVAIL vend */         
    END. /* for each vend */                 
END.  /*if prmAction <> "search" then do*/   
                                             

IF prmAction = "Search" THEN DO:
  IF prmField = "Vend" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH vend WHERE vend.company EQ prmComp AND vend.vend-no = prmText AND (vend.active EQ prmActive OR prmActive eq "") NO-LOCK:
        IF AVAIL vend THEN DO:
             create ttCorVendorLook.
             assign  

                 ttCorVendorLook.vVendor     = vend.vend-no 
                 ttCorVendorLook.vVendName   = vend.name    
                 ttCorVendorLook.vAdd        = vend.add1    
                 ttCorVendorLook.vCity       = vend.city    
                 ttCorVendorLook.vState      = vend.state   
                 ttCorVendorLook.vZip        = vend.zip     
                 ttCorVendorLook.vType       = vend.type    
                 ttCorVendorLook.vBuyer      = vend.buyer   
                 ttCorVendorLook.vphone      = vend.phone
                 ttCorVendorLook.areacode    = vend.area-code
                 ttCorVendorLook.carrier     = vend.carrier
                 ttCorVendorLook.contact     = vend.contact
                 ttCorVendorLook.terms       = vend.terms
                 ttCorVendorLook.fob-code    = IF vend.fob-code NE "" THEN vend.fob-code ELSE "DEST"   
                 ttCorVendorLook.frt-pay     = IF vend.frt-pay NE "" THEN vend.frt-pay ELSE "B"        
                 ttCorVendorLook.over-pct    = vend.over-pct
                 ttCorVendorLook.under-pct   = vend.under-pct   
                 ttCorVendorLook.tax-gr      = vend.tax-gr            
                 ttCorVendorLook.add2        = vend.add2
                 .

         END.	 /* IF AVAIL vend */
    END. /* for each vend */
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH vend WHERE vend.company EQ prmComp AND vend.vend-no BEGINS prmText AND (vend.active EQ prmActive OR prmActive eq "") NO-LOCK:
        IF AVAIL vend THEN DO:
             create ttCorVendorLook.
             assign  

                 ttCorVendorLook.vVendor     = vend.vend-no 
                 ttCorVendorLook.vVendName   = vend.name    
                 ttCorVendorLook.vAdd        = vend.add1    
                 ttCorVendorLook.vCity       = vend.city    
                 ttCorVendorLook.vState      = vend.state   
                 ttCorVendorLook.vZip        = vend.zip     
                 ttCorVendorLook.vType       = vend.type    
                 ttCorVendorLook.vBuyer      = vend.buyer   
                 ttCorVendorLook.vphone      = vend.phone
                 ttCorVendorLook.areacode    = vend.area-code
                 ttCorVendorLook.carrier     = vend.carrier
                 ttCorVendorLook.contact     = vend.contact
                 ttCorVendorLook.terms       = vend.terms
                 ttCorVendorLook.fob-code    = IF vend.fob-code NE "" THEN vend.fob-code ELSE "DEST"   
                 ttCorVendorLook.frt-pay     = IF vend.frt-pay NE "" THEN vend.frt-pay ELSE "B"        
                 ttCorVendorLook.over-pct    = vend.over-pct    
                 ttCorVendorLook.under-pct   = vend.under-pct
                 ttCorVendorLook.tax-gr      = vend.tax-gr            
                 ttCorVendorLook.add2        = vend.add2
                 .

         END.	 /* IF AVAIL vend */
    END. /* for each vend */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */

IF prmField = "name" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH vend WHERE vend.company EQ prmComp AND vend.name = prmText AND (vend.active EQ prmActive OR prmActive eq "") NO-LOCK:
        IF AVAIL vend THEN DO:
             create ttCorVendorLook.
             assign  

                 ttCorVendorLook.vVendor     = vend.vend-no 
                 ttCorVendorLook.vVendName   = vend.name    
                 ttCorVendorLook.vAdd        = vend.add1    
                 ttCorVendorLook.vCity       = vend.city    
                 ttCorVendorLook.vState      = vend.state   
                 ttCorVendorLook.vZip        = vend.zip     
                 ttCorVendorLook.vType       = vend.type    
                 ttCorVendorLook.vBuyer      = vend.buyer   
                 ttCorVendorLook.vphone      = vend.phone
                 ttCorVendorLook.areacode    = vend.area-code
                 ttCorVendorLook.carrier     = vend.carrier
                 ttCorVendorLook.contact     = vend.contact
                 ttCorVendorLook.terms       = vend.terms
                 ttCorVendorLook.fob-code    = IF vend.fob-code NE "" THEN vend.fob-code ELSE "DEST"   
                 ttCorVendorLook.frt-pay     = IF vend.frt-pay NE "" THEN vend.frt-pay ELSE "B"        
                 ttCorVendorLook.over-pct    = vend.over-pct    
                 ttCorVendorLook.under-pct   = vend.under-pct
                 ttCorVendorLook.tax-gr      = vend.tax-gr            
                 ttCorVendorLook.add2        = vend.add2
                 .

         END.	 /* IF AVAIL vend */
    END. /* for each vend */
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH vend WHERE vend.company EQ prmComp AND vend.name BEGINS prmText AND (vend.active EQ prmActive OR prmActive eq "") NO-LOCK:
        IF AVAIL vend THEN DO:
             create ttCorVendorLook.
             assign  

                 ttCorVendorLook.vVendor     = vend.vend-no 
                 ttCorVendorLook.vVendName   = vend.name    
                 ttCorVendorLook.vAdd        = vend.add1    
                 ttCorVendorLook.vCity       = vend.city    
                 ttCorVendorLook.vState      = vend.state   
                 ttCorVendorLook.vZip        = vend.zip     
                 ttCorVendorLook.vType       = vend.type    
                 ttCorVendorLook.vBuyer      = vend.buyer   
                 ttCorVendorLook.vphone      = vend.phone
                 ttCorVendorLook.areacode    = vend.area-code
                 ttCorVendorLook.carrier     = vend.carrier
                 ttCorVendorLook.contact     = vend.contact
                 ttCorVendorLook.terms       = vend.terms
                 ttCorVendorLook.fob-code    = IF vend.fob-code NE "" THEN vend.fob-code ELSE "DEST"   
                 ttCorVendorLook.frt-pay     = IF vend.frt-pay NE "" THEN vend.frt-pay ELSE "B"        
                 ttCorVendorLook.over-pct    = vend.over-pct
                 ttCorVendorLook.under-pct   = vend.under-pct
                 ttCorVendorLook.tax-gr      = vend.tax-gr            
                 ttCorVendorLook.add2        = vend.add2
                 .

         END.	 /* IF AVAIL vend */
    END. /* for each vend */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */


END. /*ir prmAction = "search"*/


