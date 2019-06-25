/*------------------------------------------------------------------------
    File        : invinfolook.p
    Purpose     : 
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttInvoiceInfoLookup NO-UNDO 
    FIELD vend            AS CHAR
    FIELD invno           AS CHAR
    FIELD invdate         AS CHAR
    FIELD net             AS DEC
    FIELD paid            AS DEC
    FIELD due             AS DEC
    FIELD amtpaid         AS DEC  
    FIELD amtdisc         AS DEC
    FIELD act             AS CHAR
    FIELD extra           AS CHAR
    .

DEFINE DATASET dsInvoiceInfoLookup FOR ttInvoiceInfoLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmvend      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsInvoiceInfoLookup.
 
DEF VAR prmComp AS CHAR NO-UNDO.


IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField     = "".
IF prmvend      = ? THEN ASSIGN prmvend      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

DEF VAR v-vend-act AS cha NO-UNDO.
DEF VAR v-vend-actdscr AS cha NO-UNDO.

IF prmField = "" THEN DO:

  if prmAction = "Select" then do:

    FOR EACH ap-inv WHERE ap-inv.company = prmComp
        and ap-inv.vend-no = prmvend
        AND ap-inv.posted NO-LOCK:

        IF AVAIL ap-inv THEN
              create ttInvoiceInfoLookup.
                      assign
                           ttInvoiceInfoLookup.vend       = ap-inv.vend-no
                           ttInvoiceInfoLookup.invno      = ap-inv.inv-no
                           ttInvoiceInfoLookup.invdate    = string(ap-inv.inv-date)
                           ttInvoiceInfoLookup.net        = ap-inv.net
                           ttInvoiceInfoLookup.paid       = ap-inv.paid
                           ttInvoiceInfoLookup.due        = ap-inv.due 
                            .


        FIND FIRST ap-invl WHERE ap-invl.company = ap-inv.company
                         AND ap-invl.i-no = ap-inv.i-no NO-LOCK NO-ERROR.    
        
        IF AVAIL ap-invl THEN
            ASSIGN v-vend-act = ap-invl.actnum.

        FIND FIRST vend NO-LOCK
            WHERE vend.company EQ prmComp
            AND vend.vend-no EQ prmvend
            NO-ERROR.

        IF AVAIL vend AND v-vend-act = "" THEN 
            ASSIGN v-vend-act = vend.actnum
                   v-vend-actdscr = vend.actdscr.
        
        
        IF v-vend-act EQ "" THEN DO:
            FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ prmComp NO-LOCK NO-ERROR.
            IF AVAIL ap-ctrl THEN 
                ASSIGN v-vend-act = ap-ctrl.purchases.
        END.

        
        IF AVAIL ap-inv THEN DO:
            ASSIGN
                ttInvoiceInfoLookup.invdate = STRING(ap-inv.due-date,"99/99/9999")
                ttInvoiceInfoLookup.due     = ap-inv.due
                ttInvoiceInfoLookup.invno   = ap-inv.inv-no
                ttInvoiceInfoLookup.act     = v-vend-act
                 .

            IF ap-inv.due LT 0 THEN
                ASSIGN
                ttInvoiceInfoLookup.amtpaid = 0
                ttInvoiceInfoLookup.amtdisc = (ap-inv.due * -1).
            ELSE
                ASSIGN
                    ttInvoiceInfoLookup.amtpaid = (ap-inv.due)
                    ttInvoiceInfoLookup.amtdisc = 0.           
        END.

        FIND FIRST account WHERE account.company = prmComp
            AND account.actnum = ttInvoiceInfoLookup.act NO-LOCK NO-ERROR.
        IF AVAIL account THEN
            ASSIGN
            ttInvoiceInfoLookup.extra  = account.dscr.
        ELSE 
            ASSIGN
            ttInvoiceInfoLookup.extra  = "".
    



       

    END.  /*FOR EACH ap-inv*/
END.  /*ifif prmAction <> "search" */

/******************Search***********************************/

IF prmAction = "Search" then do:
     if prmCondition = "EQUAL" then do:
              FOR EACH ap-inv WHERE ap-inv.company = prmComp
                  and ap-inv.vend-no = prmvend
                  AND ap-inv.inv-no = prmText
                  AND ap-inv.posted NO-LOCK:

                  IF AVAIL ap-inv THEN
                      create ttInvoiceInfoLookup.
                  assign
                      ttInvoiceInfoLookup.vend       = ap-inv.vend-no
                      ttInvoiceInfoLookup.invno      = ap-inv.inv-no
                      ttInvoiceInfoLookup.invdate    = string(ap-inv.inv-date)
                      ttInvoiceInfoLookup.net        = ap-inv.net
                      ttInvoiceInfoLookup.paid       = ap-inv.paid
                      ttInvoiceInfoLookup.due        = ap-inv.due .

                  FIND FIRST ap-invl WHERE ap-invl.company = ap-inv.company
                         AND ap-invl.i-no = ap-inv.i-no NO-LOCK NO-ERROR.    
        
        IF AVAIL ap-invl THEN
            ASSIGN v-vend-act = ap-invl.actnum.

        FIND FIRST vend NO-LOCK
            WHERE vend.company EQ prmComp
            AND vend.vend-no EQ prmvend
            NO-ERROR.

        IF AVAIL vend AND v-vend-act = "" THEN
            ASSIGN v-vend-act = vend.actnum.
        
        IF v-vend-act EQ "" THEN DO:
            FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ prmComp NO-LOCK NO-ERROR.
            IF AVAIL ap-ctrl THEN
                ASSIGN v-vend-act = ap-ctrl.purchases.
        END.

        
        IF AVAIL ap-inv THEN DO:
            ASSIGN
                ttInvoiceInfoLookup.invdate = STRING(ap-inv.due-date,"99/99/9999")
                ttInvoiceInfoLookup.due  = ap-inv.due
                ttInvoiceInfoLookup.invno   = ap-inv.inv-no
                ttInvoiceInfoLookup.act     = v-vend-act.

            IF ap-inv.due LT 0 THEN
                ASSIGN
                ttInvoiceInfoLookup.amtpaid = 0
                ttInvoiceInfoLookup.amtdisc = (ap-inv.due * -1).
            ELSE
                ASSIGN
                    ttInvoiceInfoLookup.amtpaid = (ap-inv.due)
                    ttInvoiceInfoLookup.amtdisc = 0.           
        END.

        FIND FIRST account WHERE account.company = prmComp
            AND account.actnum = ttInvoiceInfoLookup.act NO-LOCK NO-ERROR.
        IF AVAIL account THEN
            ASSIGN
            ttInvoiceInfoLookup.extra  = account.dscr.
        ELSE 
            ttInvoiceInfoLookup.extra  = "".


              END.  /*FOR EACH ap-inv*/
         END. 

        if prmCondition = "BEGIN" then do:
            FOR EACH ap-inv WHERE ap-inv.company = prmComp
                  and ap-inv.vend-no = prmvend
                  AND ap-inv.inv-no BEGINS prmText
                  AND ap-inv.posted NO-LOCK:

                  IF AVAIL ap-inv THEN
                      create ttInvoiceInfoLookup.
                  assign
                      ttInvoiceInfoLookup.vend       = ap-inv.vend-no
                      ttInvoiceInfoLookup.invno      = ap-inv.inv-no
                      ttInvoiceInfoLookup.invdate    = string(ap-inv.inv-date)
                      ttInvoiceInfoLookup.net        = ap-inv.net
                      ttInvoiceInfoLookup.paid       = ap-inv.paid
                      ttInvoiceInfoLookup.due        = ap-inv.due .


                  FIND FIRST ap-invl WHERE ap-invl.company = ap-inv.company
                         AND ap-invl.i-no = ap-inv.i-no NO-LOCK NO-ERROR.    
        
        IF AVAIL ap-invl THEN
            ASSIGN v-vend-act = ap-invl.actnum.

        FIND FIRST vend NO-LOCK
            WHERE vend.company EQ prmComp
            AND vend.vend-no EQ prmvend
            NO-ERROR.

        IF AVAIL vend AND v-vend-act = "" THEN
            ASSIGN v-vend-act = vend.actnum.
        
        IF v-vend-act EQ "" THEN DO:
            FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ prmComp NO-LOCK NO-ERROR.
            IF AVAIL ap-ctrl THEN
                ASSIGN v-vend-act = ap-ctrl.purchases.
        END.

        
        IF AVAIL ap-inv THEN DO:
            ASSIGN
                ttInvoiceInfoLookup.invdate = STRING(ap-inv.due-date,"99/99/9999")
                ttInvoiceInfoLookup.due  = ap-inv.due
                ttInvoiceInfoLookup.invno   = ap-inv.inv-no
                ttInvoiceInfoLookup.act     = v-vend-act.

            IF ap-inv.due LT 0 THEN
                ASSIGN
                ttInvoiceInfoLookup.amtpaid = 0
                ttInvoiceInfoLookup.amtdisc = (ap-inv.due * -1).
            ELSE
                ASSIGN
                    ttInvoiceInfoLookup.amtpaid = (ap-inv.due)
                    ttInvoiceInfoLookup.amtdisc = 0.           
        END.


        FIND FIRST account WHERE account.company = prmComp
            AND account.actnum = ttInvoiceInfoLookup.act NO-LOCK NO-ERROR.
        IF AVAIL account THEN
            ASSIGN
            ttInvoiceInfoLookup.extra  = account.dscr.
        ELSE 
            ttInvoiceInfoLookup.extra  = "".

              END.  /*FOR EACH ap-inv*/       
          
     END .  /* if prmField = state  */
    
  END.  /* IF prmAction = search then do: */

          
END. /*prmField = "" */

ELSE DO:  /******************   manual check ***************/

    if prmAction = "Select" then do:

        FOR EACH ap-inv WHERE ap-inv.company = prmComp
            and ap-inv.vend-no = prmvend
            AND ap-inv.posted AND ap-inv.due <> 0 NO-LOCK:

            
            create ttInvoiceInfoLookup.
            assign
                ttInvoiceInfoLookup.vend       = ap-inv.vend-no
                ttInvoiceInfoLookup.invno      = ap-inv.inv-no
                ttInvoiceInfoLookup.invdate    = string(ap-inv.inv-date)
                ttInvoiceInfoLookup.net        = ap-inv.net
                ttInvoiceInfoLookup.paid       = ap-inv.due
                ttInvoiceInfoLookup.due        = ap-inv.due 
                .

            IF (DATE(prmField) - ap-inv.inv-date) LE ap-inv.disc-days THEN DO:
                ttInvoiceInfoLookup.amtdisc = (round(ap-inv.disc-% * ap-inv.net / 100,2)).
            END.
        END.  /*FOR EACH ap-inv*/
    END.  /*ifif prmAction <> "search" */

    /******************Search***********************************/

    IF prmAction = "Search" then do:
         if prmCondition = "EQUAL" then do:
                  FOR EACH ap-inv WHERE ap-inv.company = prmComp
                      and ap-inv.vend-no = prmvend
                      AND ap-inv.inv-no = prmText
                      AND ap-inv.posted AND ap-inv.due <> 0  NO-LOCK:

                      create ttInvoiceInfoLookup.
            assign
                ttInvoiceInfoLookup.vend       = ap-inv.vend-no
                ttInvoiceInfoLookup.invno      = ap-inv.inv-no
                ttInvoiceInfoLookup.invdate    = string(ap-inv.inv-date)
                ttInvoiceInfoLookup.net        = ap-inv.net
                ttInvoiceInfoLookup.paid       = ap-inv.due
                ttInvoiceInfoLookup.due        = ap-inv.due 
                .

            IF (DATE(prmField) - ap-inv.inv-date) LE ap-inv.disc-days THEN DO:
                ttInvoiceInfoLookup.amtdisc = (round(ap-inv.disc-% * ap-inv.net / 100,2)).
            END.
           END.  /*FOR EACH ap-inv*/
        END. 

            if prmCondition = "BEGIN" then do:
                FOR EACH ap-inv WHERE ap-inv.company = prmComp
                      and ap-inv.vend-no = prmvend
                      AND ap-inv.inv-no BEGINS prmText
                      AND ap-inv.posted AND ap-inv.due <> 0  NO-LOCK:

                     create ttInvoiceInfoLookup.
            assign
                ttInvoiceInfoLookup.vend       = ap-inv.vend-no
                ttInvoiceInfoLookup.invno      = ap-inv.inv-no
                ttInvoiceInfoLookup.invdate    = string(ap-inv.inv-date)
                ttInvoiceInfoLookup.net        = ap-inv.net
                ttInvoiceInfoLookup.paid       = ap-inv.due
                ttInvoiceInfoLookup.due        = ap-inv.due 
                .

            IF (DATE(prmField) - ap-inv.inv-date) LE ap-inv.disc-days THEN DO:
                ttInvoiceInfoLookup.amtdisc = (round(ap-inv.disc-% * ap-inv.net / 100,2)).
            END.
        END.  /*FOR EACH ap-inv*/      

         END .  /* if prmField = state  */

      END.  /* IF prmAction = search then do: */



END.
