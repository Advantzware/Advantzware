/*------------------------------------------------------------------------
    File         : BolLook.p
    Purpose     : Bol lookup

    Syntax      :

    Description : Return a Dataset of all Browse Invoice
    Author(s)   : Jyoti Bajaj
    Created     : Feb 07, 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{BolLook.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum  AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBolLook.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmOrderNum      = ? THEN ASSIGN prmOrderNum      = "".

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR qh AS WIDGET-HANDLE. 
DEF VAR v-q-string AS CHAR NO-UNDO.
DEF VAR v-usercust-log AS LOG NO-UNDO.

CREATE QUERY qh.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST users WHERE
     users.user_id = prmUser
     NO-LOCK NO-ERROR.

IF AVAIL users THEN
DO:
   IF users.internal-user = NO THEN DO:

      qh:ADD-BUFFER(BUFFER usercust:HANDLE).

      ASSIGN
        v-usercust-log = YES
        v-q-string = "FOR EACH usercust WHERE usercust.user_id = " + QUOTER(prmUser)
                   + " AND usercust.company EQ " + QUOTER(prmComp) + " NO-LOCK,".
   END.
   ELSE /*internal-user = yes*/
   DO:
      IF CAN-FIND(FIRST usercust WHERE usercust.user_id = prmUser AND
         usercust.company EQ prmComp) THEN
      DO:
      
         qh:ADD-BUFFER(BUFFER usercust:HANDLE).
        
         ASSIGN
           v-usercust-log = YES
           v-q-string = "FOR EACH usercust WHERE usercust.user_id = " + QUOTER(prmUser)
                      + " AND usercust.company EQ " + QUOTER(prmComp) + " NO-LOCK,".
      END.
   END.

   if prmAction <> "search" then do:

   qh:ADD-BUFFER(BUFFER oe-boll:HANDLE).
   qh:ADD-BUFFER(BUFFER oe-bolh:HANDLE).
   
   IF v-usercust-log = NO THEN
      v-q-string = "FOR ".

   v-q-string = v-q-string
              + " EACH oe-boll where oe-boll.company eq " + QUOTER(prmComp)
              + " AND oe-boll.ord-no = " + STRING(prmOrderNum)
              + " NO-LOCK, FIRST oe-bolh of oe-boll NO-LOCK ".

   IF v-usercust-log THEN
      v-q-string = v-q-string + " WHERE oe-bolh.cust-no eq usercust.cust-no ".
       
   qh:QUERY-PREPARE(v-q-string).
   qh:QUERY-OPEN.
   
   REPEAT:
      qh:GET-NEXT().
      IF qh:QUERY-OFF-END THEN LEAVE.
   
      create ttBolLook.
      assign                                         
          ttBolLook.BolNum        = oe-boll.bol-no
          ttBolLook.BolItem       = oe-boll.i-no
          ttBolLook.Cust-no       = oe-bolh.cust-no.
   END.
      
   END.  /* if avail users*/
   END.  /*if prmAction <> "search" then do*/ 

   IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:

           qh:ADD-BUFFER(BUFFER oe-boll:HANDLE).
           qh:ADD-BUFFER(BUFFER oe-bolh:HANDLE).
           
           v-q-string = v-q-string
                      + " EACH oe-boll where oe-boll.company eq " + QUOTER(prmComp)
                      + " AND oe-boll.ord-no = " + STRING(prmOrderNum)
                      + " NO-LOCK, FIRST oe-bolh of oe-boll NO-LOCK ".
           
           IF v-usercust-log THEN
              v-q-string = v-q-string + " WHERE oe-bolh.cust-no eq usercust.cust-no ".
               
           qh:QUERY-PREPARE(v-q-string).
           
           qh:QUERY-OPEN.
           
           REPEAT:
              qh:GET-NEXT().
              IF qh:QUERY-OFF-END THEN LEAVE.
           
              IF oe-boll.bol-no = int(prmText) OR oe-boll.i-no = prmText THEN
              DO:
                 create ttBolLook.
                 assign                                         
                    ttBolLook.BolNum        = oe-boll.bol-no
                    ttBolLook.BolItem       = oe-boll.i-no
                    ttBolLook.Cust-no       = oe-bolh.cust-no.
              END.
           END.
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:

           qh:ADD-BUFFER(BUFFER oe-boll:HANDLE).
           qh:ADD-BUFFER(BUFFER oe-bolh:HANDLE).
           
           v-q-string = v-q-string
                      + " EACH oe-boll where oe-boll.company eq " + QUOTER(prmComp)
                      + " AND oe-boll.ord-no = " + STRING(prmOrderNum)
                      + " NO-LOCK, FIRST oe-bolh of oe-boll NO-LOCK ".
           
           IF v-usercust-log THEN
              v-q-string = v-q-string + " WHERE oe-bolh.cust-no eq usercust.cust-no ".
               
           qh:QUERY-PREPARE(v-q-string).
           qh:QUERY-OPEN.
           
           REPEAT:
              qh:GET-NEXT().
              IF qh:QUERY-OFF-END THEN LEAVE.
           
              IF oe-boll.bol-no = int(prmText) OR oe-boll.i-no BEGINS prmText THEN
              DO:
                 create ttBolLook.
                 assign                                         
                    ttBolLook.BolNum        = oe-boll.bol-no
                    ttBolLook.BolItem       = oe-boll.i-no
                    ttBolLook.Cust-no       = oe-bolh.cust-no.
              END.
           END.
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/  
    if prmField = "bol-no"  then do:
        if prmCondition = "EQUAL" then do:

           qh:ADD-BUFFER(BUFFER oe-boll:HANDLE).
           qh:ADD-BUFFER(BUFFER oe-bolh:HANDLE).
           
           v-q-string = v-q-string
                      + " EACH oe-boll where oe-boll.company eq " + QUOTER(prmComp)
                      + " AND oe-boll.ord-no = " + STRING(prmOrderNum)
                      + " AND oe-boll.bol-no = " + STRING(prmText)
                      + " NO-LOCK, FIRST oe-bolh of oe-boll NO-LOCK ".
           
           IF v-usercust-log THEN
              v-q-string = v-q-string + " WHERE oe-bolh.cust-no eq usercust.cust-no ".
               
           qh:QUERY-PREPARE(v-q-string).
           qh:QUERY-OPEN.
           
           REPEAT:
              qh:GET-NEXT().
              IF qh:QUERY-OFF-END THEN LEAVE.
           
              IF oe-boll.bol-no = int(prmText) OR oe-boll.i-no BEGINS prmText THEN
              DO:
                 create ttBolLook.
                 assign                                         
                    ttBolLook.BolNum        = oe-boll.bol-no
                    ttBolLook.BolItem       = oe-boll.i-no
                    ttBolLook.Cust-no       = oe-bolh.cust-no.
              END.
           END.
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:

           qh:ADD-BUFFER(BUFFER oe-boll:HANDLE).
           qh:ADD-BUFFER(BUFFER oe-bolh:HANDLE).
           
           v-q-string = v-q-string
                      + " EACH oe-boll where oe-boll.company eq " + QUOTER(prmComp)
                      + " AND oe-boll.ord-no = " + STRING(prmOrderNum)
                      + " AND oe-boll.bol-no = " + STRING(prmText)
                      + " NO-LOCK, FIRST oe-bolh of oe-boll NO-LOCK ".
           
           IF v-usercust-log THEN
              v-q-string = v-q-string + " WHERE oe-bolh.cust-no eq usercust.cust-no ".
               
           qh:QUERY-PREPARE(v-q-string).
           qh:QUERY-OPEN.
           
           REPEAT:
              qh:GET-NEXT().
              IF qh:QUERY-OFF-END THEN LEAVE.
           
              IF oe-boll.bol-no = int(prmText) OR oe-boll.i-no BEGINS prmText THEN
              DO:
                 create ttBolLook.
                 assign                                         
                    ttBolLook.BolNum        = oe-boll.bol-no
                    ttBolLook.BolItem       = oe-boll.i-no
                    ttBolLook.Cust-no       = oe-bolh.cust-no.
              END.
           END.
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = name  */
    IF prmField = "i-no" then do:

        if prmCondition = "EQUAL" then do:
           qh:ADD-BUFFER(BUFFER oe-boll:HANDLE).
           qh:ADD-BUFFER(BUFFER oe-bolh:HANDLE).
           
           v-q-string = v-q-string
                      + " EACH oe-boll where oe-boll.company eq " + QUOTER(prmComp)
                      + " AND oe-boll.ord-no = " + STRING(prmOrderNum)
                      + " AND oe-boll.i-no = " + QUOTER(prmText)
                      + " NO-LOCK, FIRST oe-bolh of oe-boll NO-LOCK ".
           
           IF v-usercust-log THEN
              v-q-string = v-q-string + " WHERE oe-bolh.cust-no eq usercust.cust-no ".
               
           qh:QUERY-PREPARE(v-q-string).
           qh:QUERY-OPEN.
           
           REPEAT:
              qh:GET-NEXT().
              IF qh:QUERY-OFF-END THEN LEAVE.
           
              create ttBolLook.
              assign                                         
                 ttBolLook.BolNum        = oe-boll.bol-no
                 ttBolLook.BolItem       = oe-boll.i-no
                 ttBolLook.Cust-no       = oe-bolh.cust-no.
           END.
        END. /*if prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
           qh:ADD-BUFFER(BUFFER oe-boll:HANDLE).
           qh:ADD-BUFFER(BUFFER oe-bolh:HANDLE).
           
           v-q-string = v-q-string
                      + " EACH oe-boll where oe-boll.company eq " + QUOTER(prmComp)
                      + " AND oe-boll.ord-no = " + STRING(prmOrderNum)
                      + " AND oe-boll.i-no BEGINS " + QUOTER(prmText)
                      + " NO-LOCK, FIRST oe-bolh of oe-boll NO-LOCK ".
           
           IF v-usercust-log THEN
              v-q-string = v-q-string + " WHERE oe-bolh.cust-no eq usercust.cust-no ".
               
           qh:QUERY-PREPARE(v-q-string).
           qh:QUERY-OPEN.
           
           REPEAT:
              qh:GET-NEXT().
              IF qh:QUERY-OFF-END THEN LEAVE.
           
              create ttBolLook.
              assign                                         
                 ttBolLook.BolNum        = oe-boll.bol-no
                 ttBolLook.BolItem       = oe-boll.i-no
                 ttBolLook.Cust-no       = oe-bolh.cust-no.
           END. 
        END.  /*if prmCondition = BEGIN*/
    END.  /*IF prmField = i-no */
   END.  /* IF prmAction = search then do: */

qh:QUERY-CLOSE().
DELETE OBJECT qh.
