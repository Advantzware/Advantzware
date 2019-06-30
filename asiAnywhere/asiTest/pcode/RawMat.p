
/*------------------------------------------------------------------------
    File         : itemLookup
    Purpose     : item lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Oct 01 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{RawMat.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRawMat.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.

       
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


if prmAction <> "search" then do:
    FOR EACH ITEM WHERE ITEM.company EQ prmComp  NO-LOCK:
        create ttRawMat.
            assign 
                ttRawMat.RItem1 = item.i-no
                ttRawMat.RName1 = item.i-name 
                ttRawMat.RDscr1 = item.i-dscr
                ttRawMat.Rcust-no = item.cust-no

                .
    END.	 /* FOR EACH item */
    FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttRawMat:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.cust-no = ttRawMat.Rcust-no
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttRawMat.
                END. /*FOR EACH ttRawMat*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser
                     AND usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttRawMat:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                            AND usercust.cust-no = ttRawMat.Rcust-no
                            AND usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttRawMat.
                    END. /*FOR EACH ttRawMat*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttRawMat:
                DELETE ttRawMat.
            END.
        END. /*IF NOT AVAILABLE users*/



END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH item where item.i-no = prmText or item.i-name = prmText or item.i-dscr = prmText no-lock:
                create ttRawMat.
                assign 
                    ttRawMat.RItem1 = item.i-no
                ttRawMat.RName1 = item.i-name 
                ttRawMat.RDscr1 = item.i-dscr
                ttRawMat.Rcust-no = item.cust-no


                    .
            END. /*FOR EACH cust where cust.cust-no = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH item where item.i-no begins prmText or item.i-name begins prmText or item.i-dscr begins prmText no-lock:
                create ttRawMat.
                assign
                    ttRawMat.RItem1 = item.i-no
                ttRawMat.RName1 = item.i-name 
                ttRawMat.RDscr1 = item.i-dscr
                ttRawMat.Rcust-no = item.cust-no.


             END.  /*FOR EACH item where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/     
    IF prmField = "i-no" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH item where item.i-no = prmText no-lock:
                create ttRawMat.
                assign 
                     ttRawMat.RItem1 = item.i-no
                ttRawMat.RName1 = item.i-name 
                ttRawMat.RDscr1 = item.i-dscr
                ttRawMat.Rcust-no = item.cust-no


                    .
            END. /*FOR EACH item where*/
        END. /*if prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH item where item.i-no begins prmText no-lock:
                create ttRawMat.
                assign 
                     ttRawMat.RItem1 = item.i-no
                ttRawMat.RName1 = item.i-name 
                ttRawMat.RDscr1 = item.i-dscr
                ttRawMat.Rcust-no = item.cust-no


                    .
            END. /*FOR EACH item where*/
        END.  /*if prmCondition = BEGIN*/
    END.  /*IF prmField = i-no */
    if prmField = "i-name"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH item where item.i-name = prmText no-lock:
                create ttRawMat.
                assign
                    ttRawMat.RItem1 = item.i-no
                ttRawMat.RName1 = item.i-name 
                ttRawMat.RDscr1 = item.i-dscr
                ttRawMat.Rcust-no = item.cust-no


                    .
            end. /*FOR EACH item-fg where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH item where item.i-name begins prmText no-lock:
                create ttRawMat.
                assign 
                     ttRawMat.RItem1 = item.i-no
                ttRawMat.RName1 = item.i-name 
                ttRawMat.RDscr1 = item.i-dscr
                ttRawMat.Rcust-no = item.cust-no


                    .
            end.  /*FOR EACH item-fg wher*/
        end.    /*if prmCondition = BEGIN*/    
    end.  /* if prmField = name  */
    if prmField = "i-dscr"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH item where item.i-dscr = prmText no-lock:
                create ttRawMat.
                assign
                     ttRawMat.RItem1 = item.i-no
                ttRawMat.RName1 = item.i-name 
                ttRawMat.RDscr1 = item.i-dscr
                ttRawMat.Rcust-no = item.cust-no


                    .
            end. /*FOR EACH item where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH item where item.i-name begins prmText no-lock:
                create ttRawMat.
                assign
                     ttRawMat.RItem1 = item.i-no
                ttRawMat.RName1 = item.i-name 
                ttRawMat.RDscr1 = item.i-dscr
                ttRawMat.Rcust-no = item.cust-no
.

            end.  /*FOR EACH item wher*/
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = name  */
     FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
     IF AVAILABLE users THEN DO:
         IF users.internal-user = NO THEN DO:
             FOR EACH ttRawMat:
                 FIND FIRST usercust WHERE usercust.user_id = prmUser 
                     AND usercust.cust-no = ttRawMat.Rcust-no
                     AND usercust.company EQ prmComp
                     NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE usercust THEN DELETE ttRawMat.
             END. /*FOR EACH ttRawMat*/
             END. /*IF users.internal-user = NO*/
             IF users.internal-user = YES THEN DO:
                 FIND FIRST usercust WHERE usercust.user_id = prmUser
                      AND usercust.company EQ prmComp
                     NO-LOCK NO-ERROR.
                 IF AVAILABLE usercust THEN DO:
                     FOR EACH ttRawMat:
                         FIND FIRST usercust WHERE usercust.user_id = prmUser 
                             AND usercust.cust-no = ttRawMat.Rcust-no
                             AND usercust.company EQ prmComp
                             NO-LOCK NO-ERROR.
                         IF NOT AVAILABLE usercust THEN DELETE ttRawMat.
                     END. /*FOR EACH ttRawMat*/
                 END. /*IF AVAILABLE usercust*/
                 END. /*IF users.internal-user = YES*/
             END. /*IF AVAILABLE users*/
             ELSE DO:
                 FOR EACH ttRawMat:
                     DELETE ttRawMat.
                 END.
             END. /*IF NOT AVAILABLE users*/
END.  /* IF prmAction = search then do: */

