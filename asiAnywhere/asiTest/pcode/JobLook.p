
/*------------------------------------------------------------------------
    File         : Job Lookup
    Purpose     :  Job lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Oct 01 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{JobLook.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsJobLook.

DEFINE VAR vJob AS CHAR.
DEFINE VAR vEst AS CHAR.


ASSIGN vJob = FILL(" ",6 - LENGTH(TRIM(prmText))) + TRIM(prmText)
       vEst =  FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) .

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF vJob         = ? THEN ASSIGN vJob         = "" .

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
    
if prmAction <> "search" then do:
    FOR EACH job-hdr WHERE job-hdr.company = prmComp  NO-LOCK  :
        create ttJobLook.
        assign                                     
            ttJobLook.JOB       = job-hdr.job-no
            ttJobLook.JOB2       = job-hdr.job-no2 
            ttJobLook.ESTIMATE  = job-hdr.est-no
            ttJobLook.TICKET    = job-hdr.ftick-prnt
            ttJobLook.cust-no   = job-hdr.cust-no
            ttJobLook.ord-no    = string(job-hdr.ord-no)
            ttJobLook.item-no   = job-hdr.i-no .            

        IF AVAIL job-hdr THEN 
                   ttJobLook.stdcost = (job-hdr.std-mat-cost +
                                                          job-hdr.std-lab-cost +
                                                          job-hdr.std-fix-cost +
                                                          job-hdr.std-var-cost)  .

        FIND FIRST itemfg WHERE itemfg.company = prmComp
                           AND itemfg.i-no = job-hdr.i-no  NO-LOCK NO-ERROR.
             IF AVAIL ITEMfg THEN
                 ASSIGN ttJobLook.itemname = itemfg.i-name
                        ttJobLook.costuom  = itemfg.prod-uom  
                        ttJobLook.loc-no = itemfg.def-loc
                        ttJobLook.loc-bin = itemfg.def-loc-bin
                        ttJobLook.qty-case = itemfg.case-count .

    END.	 /* FOR EACH job-hdr */
    FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
    IF AVAILABLE users THEN DO:
        IF users.internal-user = NO THEN DO:
            FOR EACH ttJobLook:
                FIND FIRST usercust WHERE usercust.user_id = prmUser 
                    AND usercust.cust-no = ttJobLook.cust-no
                    AND usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE usercust THEN DELETE ttJobLook.
            END. /*FOR EACH ttJobLook*/
        END. /*IF users.internal-user = NO*/
        IF users.internal-user = YES THEN DO:
            FIND FIRST usercust WHERE usercust.user_id = prmUser 
                 AND usercust.company EQ prmComp
                NO-LOCK NO-ERROR.
            IF AVAILABLE usercust THEN DO:
                FOR EACH ttJobLook:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.cust-no = ttJobLook.cust-no
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttJobLook.
                END. /*FOR EACH ttJobLook*/
            END. /*IF AVAILABLE usercust*/
        END. /*IF users.internal-user = YES*/
    END. /*IF AVAILABLE users*/
    ELSE DO:
        FOR EACH ttJobLook:
            DELETE ttJobLook.
        END.
    END. /*IF NOT AVAILABLE users*/

END.  /*ifif prmAction <> "search" */
ELSE
do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:

            FOR EACH job-hdr where job-hdr.company = prmComp no-lock:
 
                IF job-hdr.job-no = vJob /*or
                   job-hdr.job-no2 = int(prmText) or 
                   job-hdr.est-no = vEst*/ THEN
                DO:
                   create ttJobLook.
                   assign                                     
                       ttJobLook.JOB       = job-hdr.job-no
                       ttJobLook.JOB2       = job-hdr.job-no2 
                       ttJobLook.ESTIMATE  = job-hdr.est-no
                       ttJobLook.TICKET    = job-hdr.ftick-prnt
                       ttJobLook.cust-no   = job-hdr.cust-no
                        ttJobLook.ord-no    = string(job-hdr.ord-no)
                         ttJobLook.item-no   = job-hdr.i-no .            

        IF AVAIL job-hdr THEN 
                   ttJobLook.stdcost = (job-hdr.std-mat-cost +
                                                          job-hdr.std-lab-cost +
                                                          job-hdr.std-fix-cost +
                                                          job-hdr.std-var-cost)  .

        FIND FIRST itemfg WHERE itemfg.company = prmComp
                           AND itemfg.i-no = job-hdr.i-no  NO-LOCK NO-ERROR.
             IF AVAIL ITEMfg THEN
                 ASSIGN ttJobLook.itemname = itemfg.i-name
                        ttJobLook.costuom  = itemfg.prod-uom  
                        ttJobLook.loc-no = itemfg.def-loc
                        ttJobLook.loc-bin = itemfg.def-loc-bin
                        ttJobLook.qty-case = itemfg.case-count .
                END.
            END.   /*FOR EACH job-hdr*/
        END.   /* IF prmCondition = "EQUAL"*/
        if prmCondition = "BEGIN" then do:
           FOR EACH job-hdr WHERE job-hdr.company = prmComp AND  job-hdr.job-no begins vJob no-lock:

                
                   create ttJobLook.
                   assign                                     
                       ttJobLook.JOB       = job-hdr.job-no
                       ttJobLook.JOB2       = job-hdr.job-no2 
                       ttJobLook.ESTIMATE  = job-hdr.est-no
                       ttJobLook.TICKET    = job-hdr.ftick-prnt
                       ttJobLook.cust-no   = job-hdr.cust-no
                        ttJobLook.ord-no    = string(job-hdr.ord-no)
                        ttJobLook.item-no   = job-hdr.i-no .            

        IF AVAIL job-hdr THEN 
                   ttJobLook.stdcost = (job-hdr.std-mat-cost +
                                                          job-hdr.std-lab-cost +
                                                          job-hdr.std-fix-cost +
                                                          job-hdr.std-var-cost)  .

        FIND FIRST itemfg WHERE itemfg.company = prmComp
                           AND itemfg.i-no = job-hdr.i-no  NO-LOCK NO-ERROR.
             IF AVAIL ITEMfg THEN
                 ASSIGN ttJobLook.itemname = itemfg.i-name
                        ttJobLook.costuom  = itemfg.prod-uom  
                        ttJobLook.loc-no = itemfg.def-loc
                        ttJobLook.loc-bin = itemfg.def-loc-bin
                        ttJobLook.qty-case = itemfg.case-count .
               

               
            END.  /*FOR EACH job-hdr*/
        END.   /*if prmCondition = "BEGIN" */     
    END.   /*IF prmField = "ANY" then do:*/
    if prmField = "job-no" then do:
        if prmCondition = "EQUAL" then do:        
            FOR EACH job-hdr WHERE job-hdr.company = prmComp AND job-hdr.job-no = vJob no-lock:           
                create ttJobLook.
                assign                                     
                    ttJobLook.JOB       = job-hdr.job-no
                    ttJobLook.JOB2       = job-hdr.job-no2 
                    ttJobLook.ESTIMATE  = job-hdr.est-no
                    ttJobLook.TICKET    = job-hdr.ftick-prnt
                    ttJobLook.cust-no   = job-hdr.cust-no
                     ttJobLook.ord-no    = string(job-hdr.ord-no)
                    ttJobLook.item-no   = job-hdr.i-no .            

        IF AVAIL job-hdr THEN 
                   ttJobLook.stdcost = (job-hdr.std-mat-cost +
                                                          job-hdr.std-lab-cost +
                                                          job-hdr.std-fix-cost +
                                                          job-hdr.std-var-cost)  .

        FIND FIRST itemfg WHERE itemfg.company = prmComp
                           AND itemfg.i-no = job-hdr.i-no  NO-LOCK NO-ERROR.
             IF AVAIL ITEMfg THEN
                 ASSIGN ttJobLook.itemname = itemfg.i-name
                        ttJobLook.costuom  = itemfg.prod-uom  
                        ttJobLook.loc-no = itemfg.def-loc
                        ttJobLook.loc-bin = itemfg.def-loc-bin
                        ttJobLook.qty-case = itemfg.case-count .                   
            END.  /*FOR EACH job-hdr*/
        END.   /*if prmCondition = "EQUAL"*/
        if prmCondition = "BEGIN" then do:    
            
            FOR EACH job-hdr WHERE job-hdr.company = prmComp AND job-hdr.job-no begins vJob no-lock:
                create ttJobLook.
                assign                                                                  
                    ttJobLook.JOB       = job-hdr.job-no
                    ttJobLook.JOB2       = job-hdr.job-no2 
                    ttJobLook.ESTIMATE  = job-hdr.est-no
                    ttJobLook.TICKET    = job-hdr.ftick-prnt
                    ttJobLook.cust-no   = job-hdr.cust-no
                    ttJobLook.ord-no    = string(job-hdr.ord-no)
                    ttJobLook.item-no   = job-hdr.i-no .    
                

        IF AVAIL job-hdr THEN 
                   ttJobLook.stdcost = (job-hdr.std-mat-cost +
                                                          job-hdr.std-lab-cost +
                                                          job-hdr.std-fix-cost +
                                                          job-hdr.std-var-cost)  .

        FIND FIRST itemfg WHERE itemfg.company = prmComp
                           AND itemfg.i-no = job-hdr.i-no  NO-LOCK NO-ERROR.
             IF AVAIL ITEMfg THEN
                 ASSIGN ttJobLook.itemname = itemfg.i-name
                        ttJobLook.costuom  = itemfg.prod-uom  
                        ttJobLook.loc-no = itemfg.def-loc
                        ttJobLook.loc-bin = itemfg.def-loc-bin
                        ttJobLook.qty-case = itemfg.case-count .                    
                    
            END.  /*FOR EACH job-hdr*/
        END.  /*if prmCondition = "BEGIN"*/       
    END.   /*  if prmField = "job-no" then do:*/
    

/*FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttJobLook:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.cust-no = ttJobLook.cust-no
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttJobLook.
                END. /*FOR EACH ttJobLook*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser
                     AND usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttJobLook:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                            AND usercust.cust-no = ttJobLook.cust-no
                            AND usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttJobLook.
                    END. /*FOR EACH ttJobLook*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttJobLook:
                DELETE ttJobLook.
            END.
        END. /*IF NOT AVAILABLE users*/*/

END. /* IF prmAction = search then do: */


