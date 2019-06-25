
                                 
/*------------------------------------------------------------------------
    File        : validatecustomer.p
    Purpose     : validation

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : 
    Created     : monday may 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.

DEFINE INPUT PARAMETER prmstate                 AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmzip                   AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmcity                  AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmtype                  AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmtypedscr              AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmsman                  AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmsmandscr              AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmterms                 AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmtermsdscr             AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmcurrency              AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmloc                   AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmlocdscr               AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmcarrier               AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmcarrierdscr           AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmdelzone               AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmdelzonedscr           AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmterr                  AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmterrdscr              AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmtaxcode               AS CHARACTER NO-UNDO. 
                                    
DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 

IF prmComp   = ?   THEN ASSIGN prmComp = "".
IF prmUser   = ?   THEN ASSIGN prmUser = "".
IF prmAction = ?   THEN ASSIGN prmAction = "".

IF prmstate         = ? THEN ASSIGN   prmstate         = "".
IF prmzip           = ? THEN ASSIGN   prmzip           = "".
IF prmcity          = ? THEN ASSIGN   prmcity          = "".
IF prmtype          = ? THEN ASSIGN   prmtype          = "".
IF prmtypedscr      = ? THEN ASSIGN   prmtypedscr      = "".
IF prmsman          = ? THEN ASSIGN   prmsman          = "".
IF prmsmandscr      = ? THEN ASSIGN   prmsmandscr      = "".
IF prmterms         = ? THEN ASSIGN   prmterms         = "".
IF prmtermsdscr     = ? THEN ASSIGN   prmtermsdscr     = "".
IF prmcurrency      = ? THEN ASSIGN   prmcurrency      = "".
IF prmloc           = ? THEN ASSIGN   prmloc           = "".
IF prmlocdscr       = ? THEN ASSIGN   prmlocdscr       = "".
IF prmcarrier       = ? THEN ASSIGN   prmcarrier       = "".
IF prmcarrierdscr   = ? THEN ASSIGN   prmcarrierdscr   = "".
IF prmdelzone       = ? THEN ASSIGN   prmdelzone       = "".
IF prmdelzonedscr   = ? THEN ASSIGN   prmdelzonedscr   = "".
IF prmterr          = ? THEN ASSIGN   prmterr          = "".
IF prmterrdscr      = ? THEN ASSIGN   prmterrdscr      = "".
IF prmtaxcode       = ? THEN ASSIGN   prmtaxcode       = "".
                                                 
IF prmAction = ""  THEN ASSIGN prmAction = "Select".



IF prmComp EQ "" THEN
DO:
   FIND FIRST usercomp WHERE
        usercomp.user_id = prmUser AND
        usercomp.loc = '' AND
        usercomp.company_default = YES
        NO-LOCK NO-ERROR.

   prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
END.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.company = prmComp AND
     usercomp.loc NE "" AND
     usercomp.loc_default = yes
     NO-LOCK NO-ERROR.

prmLoc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".

IF prmAction = "Validate" THEN DO:
   
    
    FIND FIRST statecod WHERE statecod.statecod = prmstate NO-LOCK NO-ERROR.
    IF NOT AVAILABLE statecod THEN DO:
        ASSIGN cError  = "Invalid State".
        RETURN. 
    END.
    FIND FIRST zipcode WHERE zipcode.zipcode = prmzip NO-LOCK NO-ERROR.
    IF NOT AVAILABLE zipcode THEN DO:
        ASSIGN cError  = "Invalid zip".
        RETURN. 
    END.
     FIND FIRST zipcode WHERE zipcode.city = prmcity NO-LOCK NO-ERROR.
    IF NOT AVAILABLE zipcode THEN DO:
        ASSIGN cError  = "Invalid City".
        RETURN. 
    END.
    FIND FIRST cust WHERE cust.TYPE = prmtype NO-LOCK NO-ERROR.
   IF NOT AVAILABLE cust THEN DO:
       ASSIGN cError  = "Invalid Customer Type".
       RETURN. 
   END.
     FIND FIRST custype WHERE custype.dscr = prmtypedscr  NO-LOCK NO-ERROR.
     IF NOT AVAILABLE custype THEN  DO:
         ASSIGN
             cError  = "Invalid Customer Type Description".
     END.
      FIND FIRST sman WHERE sman.sman = prmsman NO-LOCK NO-ERROR.
     IF NOT AVAILABLE sman THEN  DO:
         ASSIGN
             cError  = "Invalid Salesman".
     END.
      FIND FIRST sman WHERE sman.sname = prmsmandscr NO-LOCK NO-ERROR.
     IF  NOT  AVAILABLE sman  THEN DO:
         ASSIGN
            cError  = "Invalid Salesman Description".
     END.

     FIND FIRST terms WHERE terms.t-code = prmterms NO-LOCK NO-ERROR.
     IF  NOT AVAILABLE terms  THEN DO:
         ASSIGN
            cError  = "Invalid Terms Code".
     END.

     FIND FIRST terms WHERE terms.dscr = prmtermsdscr NO-LOCK NO-ERROR.
     IF NOT  AVAILABLE terms  THEN DO:
         ASSIGN
            cError  = "Invalid Terms Description".
     END.
      FIND FIRST currency WHERE currency.c-code = prmcurrency NO-LOCK NO-ERROR.
     IF NOT  AVAILABLE currency  THEN DO:
         ASSIGN
            cError  = "Invalid Currency".
     END.
      FIND FIRST loc WHERE loc.loc = prmloc NO-LOCK NO-ERROR.
     IF NOT  AVAILABLE loc  THEN DO:
         ASSIGN
            cError  = "Invalid Location".
     END.
      FIND FIRST loc WHERE loc.dscr = prmlocdscr NO-LOCK NO-ERROR.
     IF NOT  AVAILABLE loc  THEN DO:
         ASSIGN
            cError  = "Invalid Location Description".
     END.
     FIND FIRST carrier WHERE carrier.carrier = prmcarrier NO-LOCK NO-ERROR.
     IF  NOT AVAILABLE carrier  THEN DO:
         ASSIGN
            cError  = "Invalid carrier".
     END.
     FIND FIRST carrier WHERE carrier.dscr = prmcarrierdscr NO-LOCK NO-ERROR.
     IF NOT  AVAILABLE carrier  THEN DO:
         ASSIGN
            cError  = "Invalid Carrier Description".
     END.
     FIND FIRST carr-mtx WHERE carr-mtx.del-zone = prmdelzone NO-LOCK NO-ERROR.
     IF  NOT AVAILABLE carr-mtx  THEN DO:
         ASSIGN
            cError  = "Invalid Delivery Zone".
     END.
     FIND FIRST carr-mtx WHERE carr-mtx.del-dscr = prmdelzonedscr NO-LOCK NO-ERROR.
     IF  NOT AVAILABLE carr-mtx  THEN DO:
         ASSIGN
            cError  = "Invalid Delivery Zone Description".
     END.

     FIND FIRST terr WHERE terr.terr = prmterr NO-LOCK NO-ERROR.
    IF  NOT AVAILABLE terr  THEN DO:
        ASSIGN
           cError  = "Invalid Territory".
    END.

    FIND FIRST terr WHERE terr.dscr = prmterrdscr NO-LOCK NO-ERROR.
   IF  NOT AVAILABLE terr  THEN DO:
       ASSIGN
          cError  = "Invalid Territory Description".
   END.

    FIND FIRST stax WHERE stax.tax-group = prmtaxcode NO-LOCK NO-ERROR.
   IF NOT  AVAILABLE stax  THEN DO:
       ASSIGN
          cError  = "Invalid Tax Code".
   END.








    
     
END.

/*************************************/
/*****************************************PROCEDURE assign-RfqShipping******************************/



