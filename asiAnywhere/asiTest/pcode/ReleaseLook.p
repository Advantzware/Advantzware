


/*------------------------------------------------------------------------
    File        : ReleaseLook.p
    Purpose     : release

    Syntax      :

    Description : Return a Dataset of all release

    Author(s)   : Jyoti
    Created     : dec 05 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttReleaseLook NO-UNDO 
    FIELD vRelease   AS INTEGER
    FIELD vRelDate   AS DATE
    FIELD vOrdNo     AS INTEGER
    FIELD cust-no    AS CHARACTER
    FIELD ship-id    AS CHARACTER
    FIELD po-no      AS CHARACTER
    FIELD vRelQty    AS INTEGER    
    .

DEFINE DATASET dsReleaseLook FOR ttReleaseLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmRelease   AS INT NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsReleaseLook .

DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 
       
IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmRelease   = ? THEN ASSIGN prmRelease   = 0.

DEF VAR lv-do-leave-rel AS LOG NO-UNDO.
DEF VAR v-release# AS INT NO-UNDO.
DEFINE VARIABLE v-rel-qty AS INTEGER NO-UNDO.
 DEFINE VAR custcount AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

        FOR EACH usercust WHERE usercust.user_id = prmUser AND 
           usercust.company = prmComp  NO-LOCK:
           ASSIGN 
              custcount = custcount + "," + usercust.cust-no .
        END.

/*------------------------------------------------------------------------------*/
FUNCTION get-ordno RETURNS INTEGER
  ( /* parameter-definitions */ ) :

  IF AVAIL oe-relh THEN DO:
    FIND FIRST oe-rell
        WHERE oe-rell.company EQ oe-relh.company
          AND oe-rell.r-no    EQ oe-relh.r-no
        USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL oe-rell THEN RETURN oe-rell.ord-no.
    ELSE RETURN 0.
  END.
  

END FUNCTION.


/*------------------------------------------------------------------------------------------------------------------*/

IF prmAction = "ReleaseLeave" THEN DO:

    DEF VAR lv-num-item AS INT NO-UNDO.
    
    /*IF (LASTKEY = -1 OR LASTKEY = 27 /*ESC*/) AND NOT lv-do-leave-rel THEN RETURN . */

    lv-do-leave-rel = NO.
    IF NOT CAN-FIND(FIRST oe-relh WHERE oe-relh.company = prmComp
                                    AND oe-relh.release# = INT(prmRelease)
                                    and  oe-relh.deleted eq no
                                    AND oe-relh.printed AND NOT oe-relh.posted )
    THEN DO:
        /*IF NOT g-sharpshooter THEN MESSAGE "Invalid Release#. Not printed? or Already posted?   Try help..." VIEW-AS ALERT-BOX ERROR. 
        ELSE RUN custom/d-msg.w ("Error","","Invalid Release#.  Not printed? or Already posted? ","",1,"OK", OUTPUT v-msgreturn).         
        RETURN NO-APPLY.
        */
        
        cError = "Invalid Release#.  Not printed? or Already posted? ".
        RETURN.  
    END.

     FOR EACH oe-relh WHERE  oe-relh.company eq prmComp AND oe-relh.release# = INTEGER(prmRelease) and  oe-relh.posted  eq NO and  oe-relh.printed eq YES and  oe-relh.deleted eq no  NO-LOCK BY oe-relh.release#:
        create ttReleaseLook.
            assign                                         
                ttReleaseLook.vRelease      =  oe-relh.release#  
                ttReleaseLook.vRelDate      =  oe-relh.rel-date                   
                ttReleaseLook.vOrdNo        =  get-ordno() 
                ttReleaseLook.cust-no       =  oe-relh.cust-no
                ttReleaseLook.ship-id       =  oe-relh.ship-id
                ttReleaseLook.po-no         =  oe-relh.po-no   
                 .
        END. /*FOR EACH oe-relh where*/

    
    /*tt-relbol.release# = INT(prmRelease).
    v-release# = tt-relbol.release#.
    */

    v-release# = INT(prmRelease).

    lv-num-item = 0. 
    v-rel-qty = 0.
    FOR EACH oe-relh NO-LOCK WHERE oe-relh.company = prmComp and
                           oe-relh.release# = INT(prmRelease)
                       AND oe-relh.printed AND NOT oe-relh.posted ,
        EACH oe-rell
        WHERE oe-rell.company EQ oe-relh.company
          AND oe-rell.r-no    EQ oe-relh.r-no
        USE-INDEX r-no NO-LOCK BREAK BY oe-rell.i-no:
        IF FIRST-OF(oe-rell.i-no) THEN lv-num-item = lv-num-item + 1.
        v-rel-qty = v-rel-qty + oe-rell.qty.
    END.

    ASSIGN 
        ttReleaseLook.vRelQty = v-rel-qty.

    /*IF lv-num-item = 1 THEN DISPLAY v-rel-qty WITH FRAME {&FRAME-NAME}.
    RETURN .
    */
END.

/*------------------------------------------------------------------------------*/

    
if prmAction =  "showall" then do:
   
    FOR EACH oe-relh WHERE  oe-relh.company eq prmComp AND  oe-relh.posted  eq NO and  oe-relh.printed eq YES 
       and  oe-relh.deleted eq NO AND LOOKUP(oe-relh.cust-no ,custcount) <> 0 AND oe-relh.stat NE "W" use-index release# NO-LOCK :
       create ttReleaseLook.
            assign                                         
                ttReleaseLook.vRelease      =  oe-relh.release#  
                ttReleaseLook.vRelDate      =  oe-relh.rel-date                  
                ttReleaseLook.vOrdNo        =  get-ordno()               
                ttReleaseLook.cust-no       =  oe-relh.cust-no
                ttReleaseLook.ship-id       =  oe-relh.ship-id
                ttReleaseLook.po-no         =  oe-relh.po-no                                            
                .
    END.	 /* FOR EACH oe-relh */     
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
     
    if prmField = "release"  then do:
        if prmCondition = "EQUAL" then do:             
             FOR EACH oe-relh WHERE  oe-relh.company eq prmComp AND oe-relh.release# = INTEGER(prmText) and  oe-relh.posted  eq NO and  oe-relh.printed eq YES 
                and  oe-relh.deleted eq NO  AND LOOKUP(oe-relh.cust-no ,custcount) <> 0 AND oe-relh.stat NE "W" use-index release# NO-LOCK BY oe-relh.release#:
                create ttReleaseLook.
                assign                                         
                ttReleaseLook.vRelease      =  oe-relh.release#  
                ttReleaseLook.vRelDate      =  oe-relh.rel-date                   
                ttReleaseLook.vOrdNo        =  get-ordno() 
                ttReleaseLook.cust-no       =  oe-relh.cust-no
                ttReleaseLook.ship-id       =  oe-relh.ship-id
                ttReleaseLook.po-no         =  oe-relh.po-no   
                 .
            END. /*FOR EACH oe-relh where*/
        END. /*if prmCondition = EQUAL */     
     end.  /* if prmField = release  */
           
     if prmField = "customer"  then do:
        if prmCondition = "EQUAL" then do:             
             FOR EACH oe-relh WHERE  oe-relh.company eq prmComp AND oe-relh.cust-no = prmText and  oe-relh.posted  eq NO and  oe-relh.printed eq YES 
                and  oe-relh.deleted eq NO AND LOOKUP(oe-relh.cust-no ,custcount) <> 0 AND oe-relh.stat NE "W" use-index release# NO-LOCK BY oe-relh.release#:
                create ttReleaseLook.
                assign                                         
                ttReleaseLook.vRelease      =  oe-relh.release#  
                ttReleaseLook.vRelDate      =  oe-relh.rel-date                   
                ttReleaseLook.vOrdNo        =  get-ordno() 
                ttReleaseLook.cust-no       =  oe-relh.cust-no
                ttReleaseLook.ship-id       =  oe-relh.ship-id
                ttReleaseLook.po-no         =  oe-relh.po-no   
                 .
            END. /*FOR EACH oe-relh where*/
        END. /*if prmCondition = EQUAL */  

        IF prmCondition = "BEGIN" THEN DO:
            FOR EACH oe-relh WHERE  oe-relh.company eq prmComp AND oe-relh.cust-no BEGINS prmText and  oe-relh.posted  eq NO and  oe-relh.printed eq YES 
               and  oe-relh.deleted eq no AND LOOKUP(oe-relh.cust-no ,custcount) <> 0 AND oe-relh.stat NE "W" use-index release# NO-LOCK BY oe-relh.release#:
                create ttReleaseLook.
                assign                                         
                ttReleaseLook.vRelease      =  oe-relh.release#  
                ttReleaseLook.vRelDate      =  oe-relh.rel-date                   
                ttReleaseLook.vOrdNo        =  get-ordno() 
                ttReleaseLook.cust-no       =  oe-relh.cust-no
                ttReleaseLook.ship-id       =  oe-relh.ship-id
                ttReleaseLook.po-no         =  oe-relh.po-no   
                 .
            END. /*FOR EACH oe-relh where*/
        END.
     end.  /* if prmField = customer  */     
    
END.  /* IF prmAction = search then do: */

