/*------------------------------------------------------------------------
    File        : TestLookup.p
    Purpose     : Test 

    Syntax      :

    Description : Return a Dataset of Test

    Author(s)   : 
    Created     : 19 jan 2009
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttTestLook NO-UNDO 
    FIELD vTest         AS CHARACTER
    FIELD board          AS CHAR
    FIELD caliper       AS DECIMAL
    FIELD boarddesc     AS CHAR .

DEFINE DATASET dsTestLook FOR ttTestLook .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      LIKE cust.company NO-UNDO.
DEFINE INPUT PARAMETER prmLoc       LIKE est.loc NO-UNDO.
DEFINE INPUT PARAMETER prmFlute     LIKE ef.flute NO-UNDO.
DEFINE INPUT PARAMETER prmStyle     AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTestLook.


DEFINE VAR v-term AS CHAR.
def var i as int no-undo.
def var v-term-2 like v-term no-undo.
 DEF VAR lv-mat-types AS CHAR INIT "B" NO-UNDO.
 DEFINE VAR ls-board AS CHAR NO-UNDO.

IF prmUser = ? THEN prmUser = "".
IF prmField = ? THEN prmField = "".
IF prmText = ? THEN prmText = "".
/*IF prmFlute = ? THEN ASSIGN prmFlute = "".*/

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmAction <> "Search" THEN DO:
    
     FOR EACH stack-flute WHERE stack-flute.company = prmComp AND (stack-flute.code = prmFlute OR prmFlute EQ "" OR prmFlute EQ "all") NO-LOCK:
       DO i = 1 to 16:
           FIND FIRST ttTestLook WHERE ttTestLook.vTest = stack-flute.row-value[i] NO-LOCK NO-ERROR.
           IF AVAIL ttTestLook  THEN NEXT.
           IF stack-flute.row-value[i] ne "" THEN 
             CREATE ttTestLook.
                ttTestLook.vTest = stack-flute.row-value[i]
               .

                 find first reftable where reftable.reftable = "STYFLU"
                           and reftable.company = prmStyle
                           and (reftable.loc = prmFlute)
                           and reftable.code = "BOARD"
                        no-lock no-error.
                 if avail reftable and reftable.dscr <> "" then do:
                     find first item where item.company = prmComp and
                         item.i-no = reftable.dscr
                         no-lock no-error.
                     if avail item then
                         assign 
                         ttTestLook.board = item.i-no
                         /* ef.brd-dscr:screen-value = item.est-dscr 
                         eb.flute:screen-value = item.flute
                         eb.test:screen-value = item.reg-no */
                         ttTestLook.caliper =  (item.cal)
                         ls-board = item.i-no
                         ttTestLook.boarddesc =  i-name.
                     .                      
                    end.  
     IF ls-board = ""  THEN DO:                        
             FOR EACH ITEM  WHERE item.company   EQ prmComp
                 AND CAN-DO(lv-mat-types,item.mat-type)
                 AND item.industry  EQ "2"
                 AND item.i-code    EQ "E"
                 AND item.flute     EQ prmFlute
                 AND item.reg-no    EQ stack-flute.row-value[i]
                 USE-INDEX mat-type NO-LOCK
                 BY item.i-no:                 

                 ASSIGN
                 ttTestLook.board = item.i-no
                 ttTestLook.caliper =  (item.cal)  
                 ttTestLook.boarddesc =  i-name  .
    END.
  END.
        

       END. /*do i = 1 to 16:*/  
  END. /*for each stack-flute */
END. /*if prmAction <> search*/





IF prmAction = "Search" THEN DO:
    IF prmField = "Test"  then do:       
     
       DO i = 1 to 16:
           FOR EACH stack-flute WHERE stack-flute.company = prmComp AND (stack-flute.code = prmFlute OR prmFlute EQ "" OR prmFlute EQ "all") AND stack-flute.row-value[i] = prmText  NO-LOCK:
           FIND FIRST ttTestLook WHERE ttTestLook.vTest = stack-flute.row-value[i] NO-LOCK NO-ERROR.
           IF AVAIL ttTestLook  THEN NEXT.
           IF stack-flute.row-value[i] ne "" THEN 
             CREATE ttTestLook.
                ttTestLook.vTest = stack-flute.row-value[i]
               .            

                find first reftable where reftable.reftable = "STYFLU"
                          and reftable.company = prmStyle
                          and (reftable.loc = prmFlute)
                          and reftable.code = "BOARD"
                       no-lock no-error.
                if avail reftable and reftable.dscr <> "" then do:
                    find first item where item.company = prmComp and
                        item.i-no = reftable.dscr
                        no-lock no-error.
                    if avail item then
                        assign 
                        ttTestLook.board = item.i-no
                        /* ef.brd-dscr:screen-value = item.est-dscr 
                        eb.flute:screen-value = item.flute
                        eb.test:screen-value = item.reg-no */
                        ttTestLook.caliper =  (item.cal)
                        ls-board = item.i-no
                        ttTestLook.boarddesc =  i-name  .
                                          
                   end.  
                   
                   IF ls-board = ""  THEN DO:                                    
                        FOR EACH ITEM  WHERE item.company   EQ prmComp
                            AND  CAN-DO(lv-mat-types,item.mat-type)
                            AND item.industry  EQ "2"
                            AND item.i-code    EQ "E"
                            AND item.flute     EQ prmFlute
                            AND item.reg-no    EQ stack-flute.row-value[i]
                            USE-INDEX mat-type NO-LOCK
                            BY item.i-no:                            

                        ASSIGN
                            ttTestLook.board = item.i-no
                            ttTestLook.caliper =  (item.cal) 
                            ttTestLook.boarddesc =  i-name  .

                        LEAVE.
                   END.
                                  
                END.
            END.
       END. /*do i = 1 to 16:*/         
  END. /*for each stack-flute */
END. /*if prmAction <> search*/ 

IF prmAction = "HandTest" THEN DO:
    
    DO i = 1 to 16:
           FOR EACH stack-flute WHERE stack-flute.company = prmComp AND (stack-flute.code = prmFlute OR prmFlute EQ "") AND stack-flute.row-value[i] = prmText  NO-LOCK:
           FIND FIRST ttTestLook WHERE ttTestLook.vTest = stack-flute.row-value[i] NO-LOCK NO-ERROR.
           IF AVAIL ttTestLook  THEN NEXT.
           IF stack-flute.row-value[i] ne "" THEN 
             CREATE ttTestLook.
                ttTestLook.vTest = stack-flute.row-value[i]
               .

                 find first reftable where reftable.reftable = "STYFLU"
                           and reftable.company = prmStyle
                           and (reftable.loc = prmFlute)
                           and reftable.code = "BOARD"
                        no-lock no-error.
                 if avail reftable and reftable.dscr <> "" then do:
                     find first item where item.company = prmComp and
                         item.i-no = reftable.dscr
                         no-lock no-error.
                     if avail item then
                         assign 
                         ttTestLook.board = item.i-no
                         /* ef.brd-dscr:screen-value = item.est-dscr 
                         eb.flute:screen-value = item.flute
                         eb.test:screen-value = item.reg-no */
                         ttTestLook.caliper =  (item.cal)
                         ls-board = item.i-no.
                     .                      
                    end.  
     IF ls-board = ""  THEN DO:
             FOR EACH ITEM  WHERE item.company   EQ prmComp
                 AND CAN-DO(lv-mat-types,item.mat-type)
                 AND item.industry  EQ "2"
                 AND item.i-code    EQ "E"
                 AND item.flute     EQ prmFlute
                 AND item.reg-no    EQ stack-flute.row-value[i]
                 USE-INDEX mat-type NO-LOCK
                 BY item.i-no:
                 ASSIGN
                 ttTestLook.board = item.i-no
                 ttTestLook.caliper =  (item.cal) 
                 ttTestLook.boarddesc =  i-name .
    END.
  END.
        

       END. /*do i = 1 to 16:*/  
  END. /*for each stack-flute */

END.
