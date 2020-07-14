/* spDynDescriptionProc.p - rstark - 3.1.2019 */

/* add dynamic description procedures in alphabetical order */
/* procedure has input of two iphWidget AS HANDLE           */
/* 1. handle of widget with key value for find              */
/* 2. handle of widget description value is displayed       */
/* use scoped-define defInputParam as 1st line in procedure */

&Scoped-define defInputParam ~
    DEFINE INPUT PARAMETER iphWidgetFrom AS HANDLE NO-UNDO.~
    DEFINE INPUT PARAMETER iphWidgetTo   AS HANDLE NO-UNDO.~
~
    RUN spGetSessionParam ("Company", OUTPUT cCompany).~
    iphWidgetTo:SCREEN-VALUE = fDefaultDescription(iphWidgetTo).

DEFINE VARIABLE cCompany      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSessionValue AS CHARACTER NO-UNDO.

/* **********************  Internal Functions  ************************ */

FUNCTION fDefaultDescription RETURNS CHARACTER
    (iphWidgetTo AS HANDLE):
    IF iphWidgetTo:DATA-TYPE EQ "Character" THEN
    RETURN IF iphWidgetTo:NAME BEGINS "start" THEN "<Start Range Value>"
      ELSE IF iphWidgetTo:NAME BEGINS "end"   THEN "<End Range Value>"
      ELSE "<Value Not Found>".
    ELSE IF iphWidgetTo:DATA-TYPE EQ "Logical" THEN
    RETURN "NO".
    ELSE
    RETURN "".
END FUNCTION.

/* **********************  Internal Procedures  *********************** */

PROCEDURE dynDescripBoxDesign:
    {&defInputParam}
    FIND FIRST box-design-hdr NO-LOCK
         WHERE box-design-hdr.company   EQ cCompany
           AND box-design-hdr.design-no GT 0 
           AND box-design-hdr.design-no EQ integer(iphWidgetFrom:SCREEN-VALUE)
         NO-ERROR.
    IF AVAILABLE box-design-hdr THEN
    iphWidgetTo:SCREEN-VALUE = box-design-hdr.description.
END PROCEDURE.

PROCEDURE dynDescripCarrier:
    {&defInputParam}
    FIND FIRST carrier NO-LOCK
         WHERE carrier.company EQ cCompany
           AND carrier.carrier EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE carrier THEN
    iphWidgetTo:SCREEN-VALUE = carrier.dscr.
END PROCEDURE.

PROCEDURE dynDescripCarrierZone:
    {&defInputParam}
    FIND FIRST carr-mtx NO-LOCK
         WHERE carr-mtx.company  EQ cCompany
           AND carr-mtx.del-zone EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE carr-mtx THEN
    iphWidgetTo:SCREEN-VALUE = carr-mtx.del-dscr.
END PROCEDURE.

PROCEDURE dynDescripCompany:
    {&defInputParam}
    FIND FIRST company NO-LOCK
         WHERE company.company EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE company THEN
    iphWidgetTo:SCREEN-VALUE = company.name.       
END PROCEDURE.

PROCEDURE dynDescripCust:
    {&defInputParam}
    FIND FIRST cust NO-LOCK
         WHERE cust.company EQ cCompany
           AND cust.cust-no EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE cust THEN
    iphWidgetTo:SCREEN-VALUE = cust.name.       
END PROCEDURE.

PROCEDURE dynDescripCustype:
    {&defInputParam}
    FIND FIRST custype NO-LOCK
         WHERE custype.company EQ cCompany
           AND custype.custype EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE custype THEN
    iphWidgetTo:SCREEN-VALUE = custype.dscr .
END PROCEDURE.

PROCEDURE dynDescripEmployee:
    {&defInputParam}
    FIND FIRST emp NO-LOCK
         WHERE emp.company EQ cCompany
           AND emp.emp-id  EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE emp THEN
    iphWidgetTo:SCREEN-VALUE = emp.l-name + ", " + emp.f-name.       
END PROCEDURE.

PROCEDURE dynDescripFGItem:
    {&defInputParam}
    FIND FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ cCompany
           AND itemfg.i-no    EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE itemfg THEN
    iphWidgetTo:SCREEN-VALUE = itemfg.i-name.       
END PROCEDURE.

PROCEDURE dynDescripFlute:
    {&defInputParam}
    FIND FIRST flute NO-LOCK
         WHERE flute.company EQ cCompany
           AND flute.code    EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE flute THEN
    iphWidgetTo:SCREEN-VALUE = flute.dscr .
END PROCEDURE.

PROCEDURE dynDescripJobCode:
    {&defInputParam}
    FIND FIRST job-code NO-LOCK
         WHERE job-code.code EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE job-code THEN
    iphWidgetTo:SCREEN-VALUE = job-code.dscr .
END PROCEDURE.

PROCEDURE dynDescripLoc:
    {&defInputParam}
    FIND FIRST loc NO-LOCK
         WHERE loc.company EQ cCompany
           AND loc.loc     EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE loc THEN
    iphWidgetTo:SCREEN-VALUE = loc.dscr.       
END PROCEDURE.

PROCEDURE dynDescripMachine:
    {&defInputParam}
    FIND FIRST mach NO-LOCK
         WHERE mach.company EQ cCompany
           AND mach.m-code  EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE mach THEN
    iphWidgetTo:SCREEN-VALUE = mach.m-dscr.       
END PROCEDURE.

PROCEDURE dynDescripMat:
    {&defInputParam}
    FIND FIRST mat NO-LOCK
         WHERE mat.mat EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE mat THEN
    iphWidgetTo:SCREEN-VALUE = mat.dscr.
END PROCEDURE.

PROCEDURE dynDescripPeriod:
    {&defInputParam}    
    DEFINE VARIABLE iPeriod AS INTEGER NO-UNDO.    
    RUN spGetSessionParam ("Period", OUTPUT cSessionValue).
    iphWidgetTo:SCREEN-VALUE = cSessionValue.
END PROCEDURE.

PROCEDURE dynDescripPerp:
    {&defInputParam}
    FIND FIRST prep NO-LOCK
         WHERE prep.company EQ cCompany
           AND prep.code    EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE prep THEN
    iphWidgetTo:SCREEN-VALUE = prep.dscr.
END PROCEDURE.

PROCEDURE dynDescripProCat:
    {&defInputParam}
    FIND FIRST procat NO-LOCK
         WHERE procat.company EQ cCompany
           AND procat.procat  EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE procat THEN
    iphWidgetTo:SCREEN-VALUE = procat.dscr.       
END PROCEDURE.


PROCEDURE dynDescripRouting:
    {&defInputParam}
    FIND FIRST routing NO-LOCK
         WHERE routing.company EQ cCompany
           AND routing.r-code  EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE routing THEN
    iphWidgetTo:SCREEN-VALUE = routing.dscr .
END PROCEDURE.

PROCEDURE dynDescripRMItem:
    {&defInputParam}
    FIND FIRST item NO-LOCK
         WHERE item.company EQ cCompany
           AND item.i-no    EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE item THEN
    iphWidgetTo:SCREEN-VALUE = item.i-name.       
END PROCEDURE.

PROCEDURE dynDescripSalesGroup:
    {&defInputParam}
    FIND FIRST usergrps NO-LOCK
         WHERE usergrps.usergrps EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE usergrps THEN
    iphWidgetTo:SCREEN-VALUE = usergrps.dscr.
END PROCEDURE.

PROCEDURE dynDescripSalesRep:
    {&defInputParam}
    FIND FIRST sman NO-LOCK
         WHERE sman.company EQ cCompany
           AND sman.sman    EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE sman THEN
    iphWidgetTo:SCREEN-VALUE = sman.sman.
END PROCEDURE.

PROCEDURE dynDescripScoreType:
    {&defInputParam}
    FIND FIRST scoreType NO-LOCK
         WHERE scoreType.company   EQ cCompany
           AND scoreType.scoreType EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE scoreType THEN
    iphWidgetTo:SCREEN-VALUE = scoreType.description .
END PROCEDURE.

PROCEDURE dynDescripSecure:
    {&defInputParam}
    RUN spGetSessionParam ("Secure", OUTPUT cSessionValue).
    IF cSessionValue EQ "" THEN
    cSessionValue = "NO".
    iphWidgetTo:SCREEN-VALUE = cSessionValue.
END PROCEDURE.

PROCEDURE dynDescripShift:
    {&defInputParam}
    FIND FIRST shifts NO-LOCK
         WHERE shifts.company EQ cCompany
           AND shifts.shift   EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE shifts THEN
    iphWidgetTo:SCREEN-VALUE = shifts.description.
END PROCEDURE.

PROCEDURE dynDescripStackPatterns:
    {&defInputParam}
    FIND FIRST stackPattern NO-LOCK
         WHERE  stackPattern.stackCode EQ iphWidgetFrom:SCREEN-VALUE 
         NO-ERROR.
    IF AVAILABLE stackPattern THEN
    iphWidgetTo:SCREEN-VALUE = stackPattern.stackDescription .
END PROCEDURE.

PROCEDURE dynDescripStyle:
    {&defInputParam}
    FIND FIRST style NO-LOCK
         WHERE style.company EQ cCompany
           AND style.style   EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE style THEN
    iphWidgetTo:SCREEN-VALUE = style.dscr .
END PROCEDURE.

PROCEDURE dynDescripUser:
    {&defInputParam}
    FIND FIRST users NO-LOCK
         WHERE users.user_id EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE users THEN
    iphWidgetTo:SCREEN-VALUE = users.user_name.
END PROCEDURE.

PROCEDURE dynDescripVendor:
    {&defInputParam}
    FIND FIRST vend NO-LOCK
         WHERE vend.company EQ cCompany
           AND vend.vend-no EQ iphWidgetFrom:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE vend THEN
    iphWidgetTo:SCREEN-VALUE = vend.name.
END PROCEDURE.
