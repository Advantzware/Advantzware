/* fg/autofg.p 03/01/2016 */
/* Finished Goods - FG# auto generation system                                */
/* -------------------------------------------------------------------------- */
                                        
DEF INPUT  PARAM ip-rowid AS   ROWID NO-UNDO.     
DEF INPUT PARAM ip-fgitem-cha AS cha NO-UNDO.
DEF INPUT PARAM ip-category AS cha NO-UNDO.
DEF INPUT PARAM ip-industry AS cha NO-UNDO.
DEF INPUT PARAM ip-cust-no AS cha NO-UNDO.
DEF OUTPUT PARAM op-i-no  AS cha NO-UNDO INIT "".

{sys/inc/var.i SHARED}

DEF BUFFER b-eb FOR eb.
DEF BUFFER bb   FOR eb.

DEF VAR li AS INT NO-UNDO.
DEF VAR li1 AS INT NO-UNDO.
DEF VAR v-len AS INT NO-UNDO.
DEF VAR v-FirstChar AS cha NO-UNDO.
DEF VAR v-catRule AS cha NO-UNDO.
DEF VAR v-indRule AS cha NO-UNDO.
DEF VAR v-custRule AS cha NO-UNDO.
DEF VAR v-numRule AS cha NO-UNDO.
DEF VAR v-sufRule AS cha NO-UNDO.
DEF VAR v-asisRule AS cha NO-UNDO.  /* literal character */
DEF VAR v-asisPos AS INT NO-UNDO.  /* literal character position */
DEF VAR v-catRuleLen AS INT NO-UNDO.
DEF VAR v-indRuleLen AS INT NO-UNDO.
DEF VAR v-custRuleLen AS INT NO-UNDO.
DEF VAR v-numRuleLen AS INT NO-UNDO.
DEF VAR v-sufRuleLen AS INT NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEF VAR v-numMin AS cha NO-UNDO INIT "0000000".
DEF VAR v-numMax AS cha NO-UNDO INIT "9999999".
DEF VAR v-licha AS cha NO-UNDO.
DEF VAR v-sufChar AS cha NO-UNDO.

IF LENGTH(ip-fgitem-cha) > 15 THEN DO:  /* error: over maximum length */
   op-i-no = "".
   RETURN.
END.

v-count = 0.
DO WHILE true:
   v-count = v-count + 1.
   case SUBSTRING(ip-fgitem-cha,v-count,1) :
       WHEN "%" THEN v-catRule = v-catRule + SUBSTRING(ip-fgitem-cha,v-count,1).
       WHEN "@" THEN v-indRule = v-indRule + SUBSTRING(ip-fgitem-cha,v-count,1).
       WHEN "$" THEN v-custRule = v-custRule + SUBSTRING(ip-fgitem-cha,v-count,1).
       WHEN "#" THEN v-numRule = v-numRule + SUBSTRING(ip-fgitem-cha,v-count,1).
       WHEN "&" OR WHEN ">" THEN v-sufRule = v-sufRule + SUBSTRING(ip-fgitem-cha,v-count,1).
       OTHERWISE ASSIGN v-asisRule = v-asisRule + SUBSTRING(ip-fgitem-cha,v-count,1)
                        v-asisPos = v-count /* hold last position of literal characters */ 
                        .
   END.
   IF v-count >= LENGTH(ip-fgitem-cha) THEN LEAVE.
END.

ASSIGN v-catRuleLen = LENGTH(v-catRule)
       v-indRuleLen = LENGTH(v-indRule)
       v-custRuleLen = LENGTH(v-custRule)
       v-numRuleLen = LENGTH(v-numRule)
       v-sufRuleLen = LENGTH(v-sufRule)
       .

IF v-catRule <> "" THEN op-i-no = SUBSTRING(caps(ip-category),1,v-catRuleLen) .
IF v-indRule <> "" THEN op-i-no = op-i-no + SUBSTRING(caps(ip-industry),1,v-indRuleLen).
IF v-custRule <> "" THEN op-i-no = op-i-no + SUBSTRING(ip-cust-no,1,v-custRuleLen).

FOR FIRST b-eb FIELDS(company est-no form-no) WHERE
    ROWID(b-eb) EQ ip-rowid NO-LOCK,
    FIRST eb FIELDS(company procat cust-no)
    WHERE eb.company EQ b-eb.company
      AND eb.est-no  EQ b-eb.est-no
      AND eb.form-no NE 0
    NO-LOCK,
    
    FIRST est FIELDS(est-type)
    WHERE est.company EQ b-eb.company
      AND est.est-no  EQ b-eb.est-no
    NO-LOCK:

  ASSIGN
    /* op-i-no = SUBSTR(eb.procat,1,1) + SUBSTR(eb.cust-no,1,6)  */
       v-len   = LENGTH(op-i-no) + 1.

  FOR EACH itemfg
      WHERE itemfg.company          EQ eb.company
        AND itemfg.i-no             BEGINS op-i-no
        AND SUBSTR(itemfg.i-no,v-len,v-numRuleLen) GE SUBSTRING(v-numMin,1,v-numRuleLen)
        AND SUBSTR(itemfg.i-no,v-len,v-numRuleLen) LE SUBSTRING(v-numMax,1,v-numRuleLen)
      NO-LOCK
      BY SUBSTR(itemfg.i-no,1,v-catRuleLen + v-indRuleLen + v-custRuleLen + v-numRuleLen) DESC:
    li = INT(SUBSTR(itemfg.i-no,v-len,v-numRuleLen)) NO-ERROR. 
    IF ERROR-STATUS:ERROR THEN li = 0.
    ELSE LEAVE.
  END.
  li = li + 1. /* increment */
  v-licha = IF v-numRuleLen = 1 THEN STRING(li,"9")
            ELSE IF v-numRuleLen = 2 THEN STRING(li,"99")
            ELSE IF v-numRuleLen = 3 THEN STRING(li,"999")
            ELSE IF v-numRuleLen = 4 THEN STRING(li,"9999")
            ELSE IF v-numRuleLen = 5 THEN STRING(li,"99999")
            ELSE IF v-numRuleLen = 6 THEN STRING(li,"999999")
            ELSE STRING(li,"9999999")
            .
            
  FOR EACH oe-ordl
      WHERE oe-ordl.company          EQ eb.company
        AND oe-ordl.i-no             BEGINS TRIM(op-i-no)
        AND SUBSTR(oe-ordl.i-no,v-len,v-numRuleLen) GE v-licha /*STRING(li,"9999")*/
        AND SUBSTR(oe-ordl.i-no,v-len,v-numRuleLen) LE SUBSTRING(v-numMax,1,v-numRuleLen) /* "9999" */
      NO-LOCK
      BY SUBSTR(oe-ordl.i-no,1,v-catRuleLen + v-indRuleLen + v-custRuleLen + v-numRuleLen) DESC:
    li1 = INT(SUBSTR(oe-ordl.i-no,v-len,v-numRuleLen)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN li1 = 0.
    ELSE LEAVE.
  END.  
  li1 = li1 + 1.
  
  ASSIGN
     li = MAX(li,li1)
     li1 = 0.

  v-licha = IF v-numRuleLen = 1 THEN STRING(li,"9")
            ELSE IF v-numRuleLen = 2 THEN STRING(li,"99")
            ELSE IF v-numRuleLen = 3 THEN STRING(li,"999")
            ELSE IF v-numRuleLen = 4 THEN STRING(li,"9999")
            ELSE IF v-numRuleLen = 5 THEN STRING(li,"99999")
            ELSE IF v-numRuleLen = 6 THEN STRING(li,"999999")
            ELSE STRING(li,"9999999")
            .
  IF b-eb.form-no NE 0 THEN
  FOR EACH bb OF est
      WHERE bb.form-no  NE 0
        AND bb.blank-no NE 0
      NO-LOCK
      BY bb.form-no BY bb.blank-no:
    li1 = li1 + 1.
    IF ROWID(bb) EQ ROWID(b-eb) THEN LEAVE.
  END.
  
  IF est.est-type EQ 2 OR est.est-type EQ 6 THEN DO:
     v-sufChar = IF v-sufRule = "&&" THEN STRING(li1,"99")
                 ELSE IF v-sufRule = ">"  THEN STRING(li1,"9")
                 ELSE STRING(li1,"9").
  END.
  else if v-sufRule <> "" then v-sufChar = if length(v-sufRule) = 1 then string("9")
                                           else if length(v-sufRule) = 2 then string("99")
                                           else if length(v-sufRule) = 3 then string("99")
                                           else "".    
  ELSE li1 = 0.

  op-i-no = op-i-no + 
            v-licha +
            v-asisRule +
            v-sufChar
            .
    /*  STRING(li + 1,"9999") + "A" +
            (IF est.est-type EQ 2 OR est.est-type EQ 6 THEN
               STRING(li1,"99")
             ELSE "99")  */

/*   MESSAGE "Autofg.p op-i-no:" op-i-no " , li: " Li SKIP                                      */
/*         "v-licha: " v-licha ",  asirule:" v-asisrule " ,sufchar:" v-sufchar SKIP             */
/*         "numlen: " v-numRuleLen SKIP                                                         */
/*         "rules: " v-catRule ",  " v-indRule ",  " v-custRule ",  " v-indRule ",  "  v-sufrule*/
/*       SKIP                                                                                   */
/*       PROGRAM-NAME(1) SKIP                                                                   */
/*       PROGRAM-NAME(2) SKIP                                                                   */
/*       PROGRAM-NAME(3) SKIP                                                                   */
/*       PROGRAM-NAME(4) SKIP                                                                   */
/*       PROGRAM-NAME(5) SKIP                                                                   */
/*       PROGRAM-NAME(6) SKIP                                                                   */
/*       PROGRAM-NAME(7) SKIP                                                                   */
/*       PROGRAM-NAME(8) SKIP                                                                   */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                       */

END.

