


/*------------------------------------------------------------------------
    File        : QFgItemLook.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all QFgItemLook

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttQFgItemLook NO-UNDO 
    FIELD QEstNum       AS CHARACTER
    FIELD QCustNum      AS CHAR
    FIELD QPart         AS  CHAR
    FIELD QItem         AS  CHAR
    FIELD style         AS CHARACTER
    FIELD QItemDscr     AS CHARACTER
    FIELD category      AS CHARACTER
    FIELD qcolor        AS INTEGER
    FIELD qcoat         AS INTEGER
    FIELD len           AS DECIMAL
    FIELD wid           AS DECIMAL
    FIELD dep           AS DECIMAL
    FIELD DieIn         AS DECIMAL
    FIELD board         AS CHARACTER
    FIELD cal AS DECIMAL
    FIELD qty AS INTEGER
    FIELD flute         AS CHARACTER
    FIELD test          AS CHARACTER
    FIELD ebrecid       AS CHAR 
        
    .


DEFINE DATASET dsQFgItemLook FOR ttQFgItemLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmType      AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQFgItemLook.

DEFINE VAR vEst AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR vtype AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmCust      = ? THEN ASSIGN prmCust      = "".
IF prmType      = ? THEN ASSIGN prmType      = "".
ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText).

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmType = "Corr" THEN DO:
    vtype = "5" + "," + "6" + "," + "8" .
END.

IF prmType = "fold" THEN DO:
    vtype = "1" + "," + "2" + "," + "4" .
END.

if prmAction <> "search" THEN
    FOR EACH cust WHERE cust.cust-no = prmCust OR cust.ACTIVE = "X" NO-LOCK:
    IF prmType <> "" THEN DO:   
        FOR EACH eb WHERE  eb.company = prmComp AND eb.cust-no = cust.cust-no AND LOOKUP(string(eb.est-type),vtype) <> 0 NO-LOCK BY eb.est-no DESC:   
        find ef of eb no-lock NO-ERROR. 
      
        create ttQFgItemLook.
        assign                                         
            ttQFgItemLook.QEstNum      =  eb.est-no  
            ttQFgItemLook.QItemDscr    =  eb.part-dscr1                         
            ttQFgItemLook.QCustNum     =  eb.cust-no
            ttQFgItemLook.QPart        =  eb.part-no  
            ttQFgItemLook.QItem        =  eb.stock-no
            ttQFgItemLook.style        =  eb.style 
            ttQFgItemLook.category     =  eb.procat
            ttQFgItemLook.qcolor       =  eb.i-col  
            ttQFgItemLook.qcoat        =  eb.i-coat
            ttQFgItemLook.len          =  eb.len 
            ttQFgItemLook.wid          =  eb.wid
            ttQFgItemLook.dep          =  eb.dep
            ttQFgItemLook.board        =  ef.board
            ttQFgItemLook.cal          =  ef.cal
            ttQFgItemLook.DieIn        =  eb.die-in
            ttQFgItemLook.flute        =  eb.flute
            ttQFgItemLook.test         =  eb.test  
            ttQFgItemLook.ebrecid      = string(eb.rec_key)
            .
        END.   /*FOR EACH eb*/
    END.
    ELSE DO:
        FOR EACH eb WHERE  eb.company = prmComp AND eb.cust-no = cust.cust-no NO-LOCK BY eb.est-no DESC:   
        find ef of eb no-lock NO-ERROR. 
      
        create ttQFgItemLook.
        assign                                         
            ttQFgItemLook.QEstNum      =  eb.est-no  
            ttQFgItemLook.QItemDscr    =  eb.part-dscr1                         
            ttQFgItemLook.QCustNum     =  eb.cust-no
            ttQFgItemLook.QPart        =  eb.part-no  
            ttQFgItemLook.QItem        =  eb.stock-no
            ttQFgItemLook.style        =  eb.style 
            ttQFgItemLook.category     =  eb.procat
            ttQFgItemLook.qcolor       =  eb.i-col  
            ttQFgItemLook.qcoat        =  eb.i-coat
            ttQFgItemLook.len          =  eb.len 
            ttQFgItemLook.wid          =  eb.wid
            ttQFgItemLook.dep          =  eb.dep
            ttQFgItemLook.board        =  ef.board
            ttQFgItemLook.cal          =  ef.cal
            ttQFgItemLook.DieIn        =  eb.die-in
            ttQFgItemLook.flute        =  eb.flute
            ttQFgItemLook.test         =  eb.test
            ttQFgItemLook.ebrecid      = string(eb.rec_key)
            .
        END.   /*FOR EACH eb*/
    END.

   END.	 /* FOR EACH eb */  
ELSE
/*IF prmAction = "search" then*/ do:
    if prmField = "est-no"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH cust WHERE cust.cust-no = prmCust OR cust.ACTIVE = "X" NO-LOCK:
                IF prmType <> "" THEN DO:
                    FOR EACH eb WHERE  eb.company = prmComp AND eb.cust-no = cust.cust-no AND eb.est-no = vEst AND LOOKUP(string(eb.est-type),vtype) <> 0 NO-LOCK BY eb.est-no DESC:
                        find ef of eb no-lock no-error.
                        create ttQFgItemLook.
                        assign
                            ttQFgItemLook.QEstNum      =  eb.est-no  
                            ttQFgItemLook.QItemDscr    =  eb.part-dscr1                         
                            ttQFgItemLook.QCustNum     =  eb.cust-no
                            ttQFgItemLook.QPart        =  eb.part-no  
                            ttQFgItemLook.QItem        =  eb.stock-no
                            ttQFgItemLook.style        =  eb.style 
                            ttQFgItemLook.category     =  eb.procat
                            ttQFgItemLook.qcolor       =  eb.i-col  
                            ttQFgItemLook.qcoat        =  eb.i-coat
                            ttQFgItemLook.len          =  eb.len 
                            ttQFgItemLook.wid          =  eb.wid
                            ttQFgItemLook.dep          =  eb.dep
                            ttQFgItemLook.board        = ef.board
                            ttQFgItemLook.cal          = ef.cal
                            ttQFgItemLook.DieIn        = eb.die-in 
                            ttQFgItemLook.flute        =  eb.flute
                            ttQFgItemLook.test         =  eb.test
                            ttQFgItemLook.ebrecid      = string(eb.rec_key)
                            .
                    END.
                END.
                ELSE DO:
                    FOR EACH eb WHERE  eb.company = prmComp AND eb.cust-no = cust.cust-no AND eb.est-no = vEst NO-LOCK BY eb.est-no DESC:
                        find ef of eb no-lock no-error.
                        create ttQFgItemLook.
                        assign
                            ttQFgItemLook.QEstNum      =  eb.est-no  
                            ttQFgItemLook.QItemDscr    =  eb.part-dscr1                         
                            ttQFgItemLook.QCustNum     =  eb.cust-no
                            ttQFgItemLook.QPart        =  eb.part-no  
                            ttQFgItemLook.QItem        =  eb.stock-no
                            ttQFgItemLook.style        =  eb.style 
                            ttQFgItemLook.category     =  eb.procat
                            ttQFgItemLook.qcolor       =  eb.i-col  
                            ttQFgItemLook.qcoat        =  eb.i-coat
                            ttQFgItemLook.len          =  eb.len 
                            ttQFgItemLook.wid          =  eb.wid
                            ttQFgItemLook.dep          =  eb.dep
                            ttQFgItemLook.board        = ef.board
                            ttQFgItemLook.cal          = ef.cal
                            ttQFgItemLook.DieIn        = eb.die-in 
                            ttQFgItemLook.flute        =  eb.flute
                            ttQFgItemLook.test         =  eb.test
                            ttQFgItemLook.ebrecid      = string(eb.rec_key)
                            .
                        END.
                END.
            END. /*FOR EACH eb where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:              
             FOR EACH cust WHERE cust.cust-no = prmCust OR cust.ACTIVE = "X" NO-LOCK:  
                 IF prmType <> "" THEN DO:
                    FOR EACH eb WHERE  eb.company = prmComp AND eb.cust-no = cust.cust-no  AND eb.est-no BEGINS vEst
                        AND LOOKUP(string(eb.est-type),vtype) <> 0  NO-LOCK BY eb.est-no DESC:
                        find ef of eb no-lock no-error.                   
                            create ttQFgItemLook.
                            assign
                            ttQFgItemLook.QEstNum      =  eb.est-no  
                            ttQFgItemLook.QItemDscr    =  eb.part-dscr1                         
                            ttQFgItemLook.QCustNum     =  eb.cust-no
                            ttQFgItemLook.QPart        =  eb.part-no  
                            ttQFgItemLook.QItem        =  eb.stock-no
                            ttQFgItemLook.style        =  eb.style 
                            ttQFgItemLook.category     =  eb.procat
                            ttQFgItemLook.qcolor       =  eb.i-col  
                            ttQFgItemLook.qcoat        =  eb.i-coat
                            ttQFgItemLook.len          =  eb.len 
                            ttQFgItemLook.wid          =  eb.wid
                            ttQFgItemLook.dep          =  eb.dep
                            ttQFgItemLook.board        = ef.board
                            ttQFgItemLook.cal          = ef.cal
                            ttQFgItemLook.DieIn        = eb.die-in
                            ttQFgItemLook.flute        =  eb.flute
                            ttQFgItemLook.test         =  eb.test 
                            ttQFgItemLook.ebrecid      = string(eb.rec_key)
                        .                    
                    END.
                END.
                ELSE DO:
                    FOR EACH eb WHERE  eb.company = prmComp AND eb.cust-no = cust.cust-no  AND eb.est-no BEGINS vEst
                        NO-LOCK BY eb.est-no DESC:
                        find ef of eb no-lock no-error.                   
                            create ttQFgItemLook.
                            assign
                            ttQFgItemLook.QEstNum      =  eb.est-no  
                            ttQFgItemLook.QItemDscr    =  eb.part-dscr1                         
                            ttQFgItemLook.QCustNum     =  eb.cust-no
                            ttQFgItemLook.QPart        =  eb.part-no  
                            ttQFgItemLook.QItem        =  eb.stock-no
                            ttQFgItemLook.style        =  eb.style 
                            ttQFgItemLook.category     =  eb.procat
                            ttQFgItemLook.qcolor       =  eb.i-col  
                            ttQFgItemLook.qcoat        =  eb.i-coat
                            ttQFgItemLook.len          =  eb.len 
                            ttQFgItemLook.wid          =  eb.wid
                            ttQFgItemLook.dep          =  eb.dep
                            ttQFgItemLook.board        = ef.board
                            ttQFgItemLook.cal          = ef.cal
                            ttQFgItemLook.DieIn        = eb.die-in
                            ttQFgItemLook.flute        =  eb.flute
                            ttQFgItemLook.test         =  eb.test
                            ttQFgItemLook.ebrecid      = string(eb.rec_key)
                        .                    
                    END.     
                END.
            end.   /*FOR EACH eb wher*/
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = est  */
       IF prmField = "part-no" then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH cust WHERE cust.cust-no = prmCust OR cust.ACTIVE = "X" NO-LOCK:
                 IF prmType <> "" THEN DO:
                    FOR EACH eb WHERE  eb.company = prmComp AND eb.cust-no = cust.cust-no  AND eb.part-no = prmText
                        AND LOOKUP(string(eb.est-type),vtype) <> 0 NO-LOCK BY eb.est-no DESC:
                        find ef of eb no-lock no-error.
                        create ttQFgItemLook.
                        assign
                            ttQFgItemLook.QEstNum      =  eb.est-no  
                            ttQFgItemLook.QItemDscr    =  eb.part-dscr1                         
                            ttQFgItemLook.QCustNum     =  eb.cust-no
                            ttQFgItemLook.QPart        =  eb.part-no  
                            ttQFgItemLook.QItem        =  eb.stock-no
                            ttQFgItemLook.style        =  eb.style 
                            ttQFgItemLook.category     =  eb.procat
                            ttQFgItemLook.qcolor       =  eb.i-col  
                            ttQFgItemLook.qcoat        =  eb.i-coat
                            ttQFgItemLook.len          =  eb.len 
                            ttQFgItemLook.wid          =  eb.wid
                            ttQFgItemLook.dep          =  eb.dep
                            ttQFgItemLook.board        = ef.board
                            ttQFgItemLook.cal          = ef.cal
                            ttQFgItemLook.DieIn        = eb.die-in
                            ttQFgItemLook.flute        =  eb.flute
                            ttQFgItemLook.test         =  eb.test
                            ttQFgItemLook.ebrecid      = string(eb.rec_key)
                            .
                    END.
                 END.
                 ELSE DO:
                    FOR EACH eb WHERE  eb.company = prmComp AND eb.cust-no = cust.cust-no  AND eb.part-no = prmText NO-LOCK BY eb.est-no DESC:
                        find ef of eb no-lock no-error.
                        create ttQFgItemLook.
                        assign
                            ttQFgItemLook.QEstNum      =  eb.est-no  
                            ttQFgItemLook.QItemDscr    =  eb.part-dscr1                         
                            ttQFgItemLook.QCustNum     =  eb.cust-no
                            ttQFgItemLook.QPart        =  eb.part-no  
                            ttQFgItemLook.QItem        =  eb.stock-no
                            ttQFgItemLook.style        =  eb.style 
                            ttQFgItemLook.category     =  eb.procat
                            ttQFgItemLook.qcolor       =  eb.i-col  
                            ttQFgItemLook.qcoat        =  eb.i-coat
                            ttQFgItemLook.len          =  eb.len 
                            ttQFgItemLook.wid          =  eb.wid
                            ttQFgItemLook.dep          =  eb.dep
                            ttQFgItemLook.board        = ef.board
                            ttQFgItemLook.cal          = ef.cal
                            ttQFgItemLook.DieIn        = eb.die-in
                            ttQFgItemLook.flute        =  eb.flute
                            ttQFgItemLook.test         =  eb.test
                            ttQFgItemLook.ebrecid      = string(eb.rec_key)
                            .
                    END.
                 END.
             END. /*FOR EACH eb where*/
         END. /*if prmCondition = EQUAL*/
         IF prmCondition = "BEGIN" then do:
             FOR EACH cust WHERE cust.cust-no = prmCust OR cust.ACTIVE = "X" NO-LOCK:
                IF prmType <> "" THEN DO:
                    FOR EACH eb WHERE  eb.company = prmComp AND eb.cust-no = cust.cust-no  AND eb.part-no BEGINS prmText
                        AND LOOKUP(string(eb.est-type),vtype) <> 0 NO-LOCK BY eb.est-no DESC:
                        find ef of eb no-lock no-error.
                        create ttQFgItemLook.
                        assign 
                            ttQFgItemLook.QEstNum      =  eb.est-no  
                            ttQFgItemLook.QItemDscr    =  eb.part-dscr1                         
                            ttQFgItemLook.QCustNum     =  eb.cust-no
                            ttQFgItemLook.QPart        =  eb.part-no  
                            ttQFgItemLook.QItem        =  eb.stock-no
                            ttQFgItemLook.style        =  eb.style 
                            ttQFgItemLook.category     =  eb.procat
                            ttQFgItemLook.qcolor       =  eb.i-col  
                            ttQFgItemLook.qcoat        =  eb.i-coat
                            ttQFgItemLook.len          =  eb.len 
                            ttQFgItemLook.wid          =  eb.wid
                            ttQFgItemLook.dep          =  eb.dep
                            ttQFgItemLook.board        = ef.board
                            ttQFgItemLook.cal          = ef.cal
                            ttQFgItemLook.DieIn        = eb.die-in
                            ttQFgItemLook.flute        =  eb.flute
                            ttQFgItemLook.test         =  eb.test
                            ttQFgItemLook.ebrecid      = string(eb.rec_key)
                        .
                    END.
                END.
                ELSE DO:
                    FOR EACH eb WHERE  eb.company = prmComp AND eb.cust-no = cust.cust-no  AND eb.part-no BEGINS prmText NO-LOCK BY eb.est-no DESC:
                        find ef of eb no-lock no-error.
                        create ttQFgItemLook.
                        assign 
                            ttQFgItemLook.QEstNum      =  eb.est-no  
                            ttQFgItemLook.QItemDscr    =  eb.part-dscr1                         
                            ttQFgItemLook.QCustNum     =  eb.cust-no
                            ttQFgItemLook.QPart        =  eb.part-no  
                            ttQFgItemLook.QItem        =  eb.stock-no
                            ttQFgItemLook.style        =  eb.style 
                            ttQFgItemLook.category     =  eb.procat
                            ttQFgItemLook.qcolor       =  eb.i-col  
                            ttQFgItemLook.qcoat        =  eb.i-coat
                            ttQFgItemLook.len          =  eb.len 
                            ttQFgItemLook.wid          =  eb.wid
                            ttQFgItemLook.dep          =  eb.dep
                            ttQFgItemLook.board        = ef.board
                            ttQFgItemLook.cal          = ef.cal
                            ttQFgItemLook.DieIn        = eb.die-in
                            ttQFgItemLook.flute        =  eb.flute
                            ttQFgItemLook.test         =  eb.test
                            ttQFgItemLook.ebrecid      = string(eb.rec_key)
                        .
                    END.
                END.
             END. /*FOR EACH eb where*/
         END.  /*if prmCondition = BEGIN*/
     END.  /*IF prmField = est-no */
     IF prmField = "stock-no" then do:
             if prmCondition = "EQUAL" then do:
                 FOR EACH cust WHERE cust.cust-no = prmCust OR cust.ACTIVE = "X" NO-LOCK:
                     IF prmType <> "" THEN DO:
                        FOR EACH eb WHERE  eb.company = prmComp AND eb.cust-no = cust.cust-no  AND eb.stock-no = prmText 
                            AND LOOKUP(string(eb.est-type),vtype) <> 0 NO-LOCK BY eb.est-no DESC:
                            find ef of eb no-lock no-error.
                            create ttQFgItemLook.
                            assign
                                ttQFgItemLook.QEstNum      =  eb.est-no  
                                ttQFgItemLook.QItemDscr    =  eb.part-dscr1                         
                                ttQFgItemLook.QCustNum     =  eb.cust-no
                                ttQFgItemLook.QPart        =  eb.part-no  
                                ttQFgItemLook.QItem        =  eb.stock-no
                                ttQFgItemLook.style        =  eb.style 
                                ttQFgItemLook.category     =  eb.procat
                                ttQFgItemLook.qcolor       =  eb.i-col  
                                ttQFgItemLook.qcoat        =  eb.i-coat
                                ttQFgItemLook.len          =  eb.len 
                                ttQFgItemLook.wid          =  eb.wid
                                ttQFgItemLook.dep          =  eb.dep
                                ttQFgItemLook.board        = ef.board
                                ttQFgItemLook.cal          = ef.cal
                                ttQFgItemLook.DieIn        = eb.die-in
                                ttQFgItemLook.flute        =  eb.flute
                                ttQFgItemLook.test         =  eb.test
                                ttQFgItemLook.ebrecid      = string(eb.rec_key)
                            .
                        END.
                     END.
                     ELSE DO:
                        FOR EACH eb WHERE  eb.company = prmComp AND eb.cust-no = cust.cust-no  AND eb.stock-no = prmText NO-LOCK BY eb.est-no DESC:
                            find ef of eb no-lock no-error.
                            create ttQFgItemLook.
                            assign
                                ttQFgItemLook.QEstNum      =  eb.est-no  
                                ttQFgItemLook.QItemDscr    =  eb.part-dscr1                         
                                ttQFgItemLook.QCustNum     =  eb.cust-no
                                ttQFgItemLook.QPart        =  eb.part-no  
                                ttQFgItemLook.QItem        =  eb.stock-no
                                ttQFgItemLook.style        =  eb.style 
                                ttQFgItemLook.category     =  eb.procat
                                ttQFgItemLook.qcolor       =  eb.i-col  
                                ttQFgItemLook.qcoat        =  eb.i-coat
                                ttQFgItemLook.len          =  eb.len 
                                ttQFgItemLook.wid          =  eb.wid
                                ttQFgItemLook.dep          =  eb.dep
                                ttQFgItemLook.board        = ef.board
                                ttQFgItemLook.cal          = ef.cal
                                ttQFgItemLook.DieIn        = eb.die-in
                                ttQFgItemLook.flute        =  eb.flute
                                ttQFgItemLook.test         =  eb.test
                                ttQFgItemLook.ebrecid      = string(eb.rec_key)
                            .
                        END.
                     END.
                 END. /*FOR EACH eb where*/
             END. /*if prmCondition = EQUAL*/
             IF prmCondition = "BEGIN" then do:
                 FOR EACH cust WHERE cust.cust-no = prmCust OR cust.ACTIVE = "X" NO-LOCK:
                     IF prmType <> "" THEN DO:
                        FOR EACH eb WHERE  eb.company = prmComp AND eb.cust-no = cust.cust-no  AND eb.stock-no BEGINS prmText
                            AND LOOKUP(string(eb.est-type),vtype) <> 0 NO-LOCK BY eb.est-no DESC:
                            find ef of eb no-lock no-error.
                            create ttQFgItemLook.
                            assign 
                                ttQFgItemLook.QEstNum      =  eb.est-no  
                                ttQFgItemLook.QItemDscr    =  eb.part-dscr1                         
                                ttQFgItemLook.QCustNum     =  eb.cust-no
                                ttQFgItemLook.QPart        =  eb.part-no  
                                ttQFgItemLook.QItem        =  eb.stock-no
                                ttQFgItemLook.style        =  eb.style 
                                ttQFgItemLook.category     =  eb.procat
                                ttQFgItemLook.qcolor       =  eb.i-col  
                                ttQFgItemLook.qcoat        =  eb.i-coat
                                ttQFgItemLook.len          =  eb.len 
                                ttQFgItemLook.wid          =  eb.wid
                                ttQFgItemLook.dep          =  eb.dep
                                ttQFgItemLook.board        = ef.board
                                ttQFgItemLook.cal          = ef.cal
                                ttQFgItemLook.DieIn        = eb.die-in
                                ttQFgItemLook.flute        =  eb.flute
                                ttQFgItemLook.test         =  eb.test
                                ttQFgItemLook.ebrecid      = string(eb.rec_key)
                            .
                        END.
                     END.
                     ELSE DO:
                        FOR EACH eb WHERE  eb.company = prmComp AND eb.cust-no = cust.cust-no  AND eb.stock-no BEGINS prmText NO-LOCK BY eb.est-no DESC:
                            find ef of eb no-lock no-error.
                            create ttQFgItemLook.
                            assign 
                                ttQFgItemLook.QEstNum      =  eb.est-no  
                                ttQFgItemLook.QItemDscr    =  eb.part-dscr1                         
                                ttQFgItemLook.QCustNum     =  eb.cust-no
                                ttQFgItemLook.QPart        =  eb.part-no  
                                ttQFgItemLook.QItem        =  eb.stock-no
                                ttQFgItemLook.style        =  eb.style 
                                ttQFgItemLook.category     =  eb.procat
                                ttQFgItemLook.qcolor       =  eb.i-col  
                                ttQFgItemLook.qcoat        =  eb.i-coat
                                ttQFgItemLook.len          =  eb.len 
                                ttQFgItemLook.wid          =  eb.wid
                                ttQFgItemLook.dep          =  eb.dep
                                ttQFgItemLook.board        = ef.board
                                ttQFgItemLook.cal          = ef.cal
                                ttQFgItemLook.DieIn        = eb.die-in
                                ttQFgItemLook.flute        =  eb.flute
                                ttQFgItemLook.test         =  eb.test
                                ttQFgItemLook.ebrecid      = string(eb.rec_key)
                            .
                        END.
                     END.
                 END. /*FOR EACH eb where*/
             END.  /*if prmCondition = BEGIN*/
         END.  /*IF prmField = stock-no */

    
END.  /* IF prmAction = search then do: */







