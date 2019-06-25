
DEFINE TEMP-TABLE ttestitemupdate NO-UNDO
    FIELD vest AS CHAR
    FIELD vcustomer AS CHAR
    FIELD vino   AS CHAR
    FIELD pr-uom AS CHAR
    FIELD vitemdscr AS CHAR
    FIELD vcustpart AS CHAR.
DEFINE DATASET dsestitemupdate FOR ttestitemupdate.

DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmText AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOrder AS INT NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsestitemupdate.
DEF VAR ip-est-type like eb.est-type no-undo.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR ip-loc AS CHAR NO-UNDO.
IF prmAction      = ? THEN ASSIGN prmAction      = "".



FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ip-loc = usercomp.loc.
ASSIGN  ip-est-type  = 0 .

if prmAction <> "search" then do:
   FIND FIRST oe-ord WHERE oe-ord.ord-no = prmOrder AND oe-ord.company = prmComp NO-LOCK NO-ERROR.
   
  FOR EACH eb WHERE 
    eb.company = prmComp /*and eb.loc = ip-loc */ and eb.cust-no = oe-ord.cust-no and 
   (ip-est-type = 0 or 
   (ip-est-type = 100 and eb.est-type ge 1 and eb.est-type le 4) or 
   (ip-est-type = 200 and eb.est-type ge 5 and eb.est-type le 8))   NO-LOCK:
       
IF eb.stock-no NE "" THEN
  FOR EACH itemfg
    where itemfg.company eq eb.company
    and itemfg.i-no    eq eb.stock-no
    no-lock :
    create ttestitemupdate.
    assign                                     
        ttestitemupdate.vest        = eb.est-no
        ttestitemupdate.vcustomer   = eb.cust-no 
        ttestitemupdate.vitemdscr   = eb.part-dscr1
        ttestitemupdate.vcustpart   = eb.part-no
        ttestitemupdate.vino        = eb.stock-no
        ttestitemupdate.pr-uom      = itemfg.sell-uom
        .
   END.  /* FOR EACH eb */
  END. /* FOR EACH itemfg */
END.  /*if prmAction <> "search" */

/****************************************************Search***********************************************************************************/
IF prmAction = "search" then do:
    MESSAGE "testeb" prmAction prmField prmCondition prmText.  
    if prmField = "estno"  then do:
        if prmCondition = "EQUAL" then do:
            FIND FIRST oe-ord WHERE oe-ord.ord-no = prmOrder AND oe-ord.company = prmComp NO-LOCK NO-ERROR.
            FOR EACH eb WHERE 
                  eb.company = prmComp /*and eb.loc = ip-loc*/ and eb.cust-no = oe-ord.cust-no and eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText)   AND
                  (ip-est-type = 0 or 
                  (ip-est-type = 100 and eb.est-type ge 1 and eb.est-type le 4) or 
                  (ip-est-type = 200 and eb.est-type ge 5 and eb.est-type le 8)) NO-LOCK:
             
          FOR EACH itemfg
                where itemfg.company eq eb.company and itemfg.i-no eq eb.stock-no no-lock :
                create ttestitemupdate.
                assign                                     
                    ttestitemupdate.vest        = eb.est-no
                    ttestitemupdate.vcustomer   = eb.cust-no 
                    ttestitemupdate.vitemdscr   = eb.part-dscr1
                    ttestitemupdate.vcustpart   = eb.part-no
                    ttestitemupdate.vino        = eb.stock-no
                    ttestitemupdate.pr-uom      = itemfg.sell-uom
                    .
            END.  /* FOR EACH eb */
          END.  /* FOR EACH itemfg */
        END. /*if prmCondition = "EQUAL"*/
          IF prmCondition = "BEGIN" then do:
              FIND FIRST oe-ord WHERE oe-ord.ord-no = prmOrder AND oe-ord.company = prmComp NO-LOCK NO-ERROR.
              FOR EACH eb WHERE 
                  eb.company = prmComp /*and eb.loc = ip-loc*/ and eb.cust-no = oe-ord.cust-no AND eb.est-no BEGINS FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText)   AND 
                    (ip-est-type = 0 or 
                    (ip-est-type = 100 and eb.est-type ge 1 and eb.est-type le 4) or 
                    (ip-est-type = 200 and eb.est-type ge 5 and eb.est-type le 8)) NO-LOCK:
                 FOR EACH itemfg
    where itemfg.company eq eb.company
    and itemfg.i-no    eq eb.stock-no
    no-lock :
    create ttestitemupdate.
    assign                                     
        ttestitemupdate.vest        = eb.est-no
        ttestitemupdate.vcustomer   = eb.cust-no 
        ttestitemupdate.vitemdscr   = eb.part-dscr1
        ttestitemupdate.vcustpart   = eb.part-no
        ttestitemupdate.vino        = eb.stock-no
         ttestitemupdate.pr-uom     = itemfg.sell-uom
        .
    END.  /* FOR EACH eb */
   END.
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */

if prmField = "part-no"  then do:
        
         if prmCondition = "EQUAL" then do:
             FIND FIRST oe-ord WHERE oe-ord.ord-no = prmOrder AND oe-ord.company = prmComp NO-LOCK NO-ERROR.
             FOR EACH eb WHERE 
                 eb.company = prmComp /*and eb.loc = ip-loc*/ and eb.cust-no = oe-ord.cust-no and eb.part-no = prmText AND
                 ( ip-est-type = 0 or 
                   (ip-est-type = 100 and eb.est-type ge 1 and eb.est-type le 4) or 
                   (ip-est-type = 200 and eb.est-type ge 5 and eb.est-type le 8)) NO-LOCK:
                 FOR EACH itemfg
    where itemfg.company eq eb.company
    and itemfg.i-no    eq eb.stock-no
    no-lock :
    create ttestitemupdate.
    assign                                     
        ttestitemupdate.vest        = eb.est-no
        ttestitemupdate.vcustomer   = eb.cust-no 
        ttestitemupdate.vitemdscr   = eb.part-dscr1
        ttestitemupdate.vcustpart   = eb.part-no
        ttestitemupdate.vino        = eb.stock-no
         ttestitemupdate.pr-uom     = itemfg.sell-uom
        .
    END.  /* FOR EACH eb */
   END.
          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
              FIND FIRST oe-ord WHERE oe-ord.ord-no = prmOrder AND oe-ord.company = prmComp NO-LOCK NO-ERROR.
              FOR EACH eb WHERE 
                  eb.company = prmComp /*and eb.loc = ip-loc*/ and eb.cust-no = oe-ord.cust-no AND eb.part-no BEGINS prmText AND 
                  ( ip-est-type = 0 or 
                    (ip-est-type = 100 and eb.est-type ge 1 and eb.est-type le 4) or 
                    (ip-est-type = 200 and eb.est-type ge 5 and eb.est-type le 8)) NO-LOCK:
                 FOR EACH itemfg
    where itemfg.company eq eb.company
    and itemfg.i-no    eq eb.stock-no
    no-lock :
    create ttestitemupdate.
    assign                                     
        ttestitemupdate.vest        = eb.est-no
        ttestitemupdate.vcustomer   = eb.cust-no 
        ttestitemupdate.vitemdscr   = eb.part-dscr1
        ttestitemupdate.vcustpart   = eb.part-no
        ttestitemupdate.vino        = eb.stock-no
         ttestitemupdate.pr-uom     = itemfg.sell-uom
        .
    END.  /* FOR EACH eb */
   END.
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */


