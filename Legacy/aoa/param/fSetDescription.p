/* fSetDescription.p */
/* used in aoaParam.w to return descriptions for parameter fields */

DEFINE INPUT  PARAMETER iphObject      AS HANDLE    NO-UNDO.
DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcDescription AS CHARACTER NO-UNDO.

DEFINE VARIABLE cRange AS CHARACTER NO-UNDO.

cRange = REPLACE(iphObject:NAME,"sv","").
/* add additional parameter fields alphabetically */
CASE iphObject:NAME:
    WHEN "svStartCompany" OR WHEN "svEndCompany" THEN DO:
        cRange = REPLACE(cRange,"Company","").
        FIND FIRST company NO-LOCK
             WHERE company.company EQ iphObject:SCREEN-VALUE
             NO-ERROR.
        IF AVAILABLE company THEN opcDescription = company.name.
    END.
    WHEN "svStartCustNo" OR WHEN "svEndCustNo" THEN DO:
        cRange = REPLACE(cRange,"CustNo","").
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ ipcCompany
               AND cust.cust-no EQ iphObject:SCREEN-VALUE
             NO-ERROR.
        IF AVAILABLE cust THEN opcDescription = cust.name.
    END.
    WHEN "svStartCustPart" OR WHEN "svEndCustPart" THEN DO:
        cRange = REPLACE(cRange,"CustPart","").
        FIND FIRST cust-part NO-LOCK
             WHERE cust-part.company EQ ipcCompany
               AND cust-part.part-no EQ iphObject:SCREEN-VALUE
             NO-ERROR.
        IF AVAILABLE cust THEN opcDescription = cust-part.i-name.
    END.
    WHEN "svStartCurrency" OR WHEN "svEndCurrency" THEN DO:
        cRange = REPLACE(cRange,"Currency","").
        FIND FIRST currency NO-LOCK
             WHERE currency.company EQ ipcCompany
               AND currency.c-code  EQ iphObject:SCREEN-VALUE
             NO-ERROR.
        IF AVAILABLE currency THEN opcDescription = currency.c-desc.
    END.
    WHEN "svStartDept" OR WHEN "svEndDept" THEN DO:
        cRange = REPLACE(cRange,"Dept","").
        FIND FIRST dept NO-LOCK
             WHERE dept.code EQ iphObject:SCREEN-VALUE
             NO-ERROR.
        IF AVAILABLE dept THEN opcDescription = dept.dscr.
    END.
    WHEN "svStartItemNo" OR WHEN "svEndItemNo" THEN DO:
        cRange = REPLACE(cRange,"ItemNo","").
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ ipcCompany
               AND itemfg.i-no    EQ iphObject:SCREEN-VALUE
             NO-ERROR.
        IF AVAILABLE itemfg THEN opcDescription = itemfg.i-name.
    END.
    WHEN "svStartLoc" OR WHEN "svEndLoc" THEN DO:
        cRange = REPLACE(cRange,"Loc","").
        FIND FIRST loc NO-LOCK
             WHERE loc.company EQ ipcCompany
               AND loc.loc     EQ iphObject:SCREEN-VALUE
             NO-ERROR.
        IF AVAILABLE loc THEN opcDescription = loc.dscr.
    END.
    WHEN "svStartMachine" OR WHEN "svEndMachine" THEN DO:
        cRange = REPLACE(cRange,"Machine","").
        FIND FIRST mach NO-LOCK
             WHERE mach.company EQ ipcCompany
               AND mach.m-code  EQ iphObject:SCREEN-VALUE
             NO-ERROR.
        IF AVAILABLE mach THEN opcDescription = mach.m-dscr.
    END.
    WHEN "svStartProdCategory" OR WHEN "svEndProdCategory" THEN DO:
        cRange = REPLACE(cRange,"ProdCategory","").
        FIND FIRST procat NO-LOCK
             WHERE procat.company EQ ipcCompany
               AND procat.procat  EQ iphObject:SCREEN-VALUE
             NO-ERROR.
        IF AVAILABLE procat THEN opcDescription = procat.dscr.
    END.
    WHEN "svStartSalesRep" OR WHEN "svEndSalesRep" THEN DO:
        cRange = REPLACE(cRange,"SalesRep","").
        FIND FIRST sman NO-LOCK
             WHERE sman.company EQ ipcCompany
               AND sman.sman    EQ iphObject:SCREEN-VALUE
             NO-ERROR.
        IF AVAILABLE sman THEN opcDescription = sman.sname.
    END.
    WHEN "svStartShift" OR WHEN "svEndShift" THEN DO:
        cRange = REPLACE(cRange,"Shift","").
        FIND FIRST shift NO-LOCK
             WHERE shift.company EQ ipcCompany
               AND shift.shift   EQ INTEGER(iphObject:SCREEN-VALUE)
             NO-ERROR.
        IF AVAILABLE shift THEN opcDescription = shift.descr.
    END.
    WHEN "svStartStyle" OR WHEN "svEndStyle" THEN DO:
        cRange = REPLACE(cRange,"Style","").
        FIND FIRST style NO-LOCK
             WHERE style.company EQ ipcCompany
               AND style.style   EQ iphObject:SCREEN-VALUE
             NO-ERROR.
        IF AVAILABLE style THEN opcDescription = style.dscr.
    END.
    WHEN "svStartTerms" OR WHEN "svEndTerms" THEN DO:
        cRange = REPLACE(cRange,"Terms","").
        FIND FIRST terms NO-LOCK
             WHERE terms.company EQ ipcCompany
               AND terms.t-code  EQ iphObject:SCREEN-VALUE
             NO-ERROR.
        IF AVAILABLE terms THEN opcDescription = terms.dscr.
    END.
    WHEN "svStartUserID" OR WHEN "svEndUserID" OR WHEN "svStartCSR" OR WHEN "svEndCSR" THEN DO:
        cRange = REPLACE(cRange,"UserID","").
        FIND FIRST users NO-LOCK
             WHERE users.user_id EQ iphObject:SCREEN-VALUE
             NO-ERROR.
        IF AVAILABLE users THEN opcDescription = users.user_name.
    END.
END CASE.

IF opcDescription EQ "" THEN
opcDescription = "<" + cRange + " Range Value>".
