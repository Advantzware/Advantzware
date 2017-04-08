/******  Addon/bol/bolemail.p *******/  
  
  FUNCTION comma RETURNS CHARACTER (ipValue AS CHARACTER) :
     RETURN IF ipValue NE '' THEN ',' ELSE ''.
  END FUNCTION.

  PROCEDURE mail EXTERNAL "xpMail.dll" :
    DEF INPUT PARAM mailTo AS CHAR.
    DEF INPUT PARAM mailsubject AS CHAR.
    DEF INPUT PARAM mailText AS CHAR.
    DEF INPUT PARAM mailFiles AS CHAR.
    DEF INPUT PARAM mailDialog AS LONG.
    DEF OUTPUT PARAM retCode AS LONG.
  END.

{addon/bol/tt-email.i}
/*   DEFINE SHARED TEMP-TABLE tt-email NO-UNDO    */
/*      FIELD release# AS INT FORMAT "->,>>>,>>9" */
/*      FIELD ord-no AS INT FORMAT ">>>>>9"       */
/*      FIELD i-no AS CHAR FORMAT "X(15)"         */
/*      FIELD part-no AS CHAR FORMAT "X(15)"      */
/*      FIELD i-name AS CHAR FORMAT "X(30)"       */
/*      FIELD done-what AS cha                    */
/*      FIELD ord-no2 AS INT FORMAT ">>>>>9"      */
/*      FIELD job-no AS CHAR FORMAT "X(6)"        */
/*      FIELD job-no2 AS INT FORMAT ">9".         */
  
  DEFINE INPUT PARAMETER ip-release# AS INT NO-UNDO.
  DEFINE INPUT PARAMETER cocode AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER v-prgmname AS CHAR NO-UNDO.

  DEF VAR retcode AS INT NO-UNDO.
  DEF VAR ls-to-list AS cha NO-UNDO.
  DEF VAR lv-mailto AS cha NO-UNDO.
  DEF VAR lv-mailsubject AS cha NO-UNDO.
  DEF VAR lv-mailbody AS cha NO-UNDO.
  DEF VAR lv-mailattach AS cha NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.
  DEF VAR lv-job AS CHAR NO-UNDO.
  DEF VAR cRtnChar AS CHAR NO-UNDO.
  DEF VAR lRecFound AS LOG NO-UNDO.
  DEF VAR SSBolEmail-log AS LOG NO-UNDO.
  DEF VAR SSBolEmail-char AS CHAR NO-UNDO.
  DEF VAR lSSBolEmailPassed AS LOG NO-UNDO.
RUN sys/ref/nk1look.p (INPUT cocode, "SSBolEmail", "L" /* Logical */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT cRtnChar, OUTPUT lRecFound).
SSBolEmail-log = LOGICAL(cRtnChar) NO-ERROR.
RUN sys/ref/nk1look.p (INPUT cocode, "SSBolEmail", "C" /* Character */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT cRtnChar, OUTPUT lRecFound).
SSBolEmail-char = cRtnChar NO-ERROR.

lSSBolEmailPassed = FALSE.
  FOR EACH tt-email WHERE tt-email.release# EQ ip-release# BREAK BY tt-email.done-what:
      FIND FIRST oe-ord NO-LOCK WHERE oe-ord.company EQ cocode AND oe-ord.ord-no EQ tt-email.ord-no NO-ERROR.
      FIND FIRST users NO-LOCK WHERE users.user_id EQ oe-ord.user-id NO-ERROR.
      IF AVAIL users AND users.image_filename NE '' AND
         NOT CAN-DO(ls-to-list,users.image_filename) THEN 
         ls-to-list = ls-to-list + comma(ls-to-list) + users.image_filename.

      FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode AND cust.cust-no EQ oe-ord.cust-no NO-ERROR.
      IF cust.active EQ 'E' THEN
         FOR EACH phone NO-LOCK
             WHERE phone.table_rec_key EQ cust.rec_key
             BREAK BY phone.e_mail:
             IF CAN-FIND(FIRST emaildtl
                         WHERE (emaildtl.emailcod EQ 'ERELEASE'
                            OR emaildtl.emailcod EQ v-prgmname)
                           AND emaildtl.table_rec_key EQ phone.rec_key)
                            OR phone.titlcode EQ 'ERELEASE'
                           AND phone.e_mail NE ''
                           AND NOT CAN-DO(ls-to-list,phone.e_mail) THEN
             ls-to-list = ls-to-list + comma(ls-to-list) + phone.e_mail.
         END. /* each phone */
      ELSE
         IF cust.active EQ 'X' THEN
            ls-to-list = ls-to-list + comma(ls-to-list) + cust.email.

      IF FIRST-OF(tt-email.done-what) THEN DO:
         IF CAN-DO('Added,Deleted',tt-email.done-what) THEN 
            lv-mailbody = lv-mailbody + 'Release# ' + STRING(tt-email.release#) +
                       ' has changed.  ITEMS have been ' +
                       tt-email.done-what + '.' + CHR(10).
         ELSE IF tt-email.done-what EQ 'OverUnder' THEN
             lv-mailbody = lv-mailbody + 'Release# ' + STRING(tt-email.release#) +
                       ' has a mismatch in quantity.' + CHR(10).
      END.

      /* If user has selected only certain types of emails, filter them */
      IF ssBolEmail-log AND (ssBolEmail-char EQ "" OR LOOKUP(tt-email.done-what, ssBolEmail-char) GT 0) THEN DO:

        /* mail from at least one record will be sent */
        lSSBolEmailPassed = TRUE.

        IF CAN-DO('Added,Deleted',tt-email.done-what) THEN
           ASSIGN
              lv-job = IF tt-email.job-no EQ '' THEN ''
                       ELSE ' / Job: ' + tt-email.job-no + '-' + STRING(tt-email.job-no2)
              lv-mailbody = lv-mailbody + '     Item: ' + tt-email.i-no + ', ' +
                            tt-email.part-no + ' ' + tt-email.i-name + lv-job +
                            ' / Order: ' + STRING(tt-email.ord-no2) + CHR(10).
        ELSE IF tt-email.done-what EQ 'OverUnder' THEN
            ASSIGN
                lv-mailbody = lv-mailbody + ' Order: ' + STRING(tt-email.ord-no)
                           + ' Item: ' + tt-email.i-no + ', ' + tt-email.part-no + ' ' + tt-email.i-name + CHR(10) 
                           + '    Qty Released: ' + STRING(tt-email.qty-rel) + CHR(10)
                           + '    Qty Scanned: ' + STRING(tt-email.qty-scan) + CHR(10)
                           + '    Delta: ' + STRING((tt-email.qty-scan - tt-email.qty-rel)  / tt-email.qty-rel * 100,"->>,>>9.99%")
                           + CHR(10).
      END.
       
  END. /* each tt-email */

  /* If none of the tt-email records have selected message types, then */
  /* don't send any email */
  IF ssBolEmail-log EQ NO OR NOT lSSBolEmailPassed THEN
    lv-mailbody = "".

  IF lv-mailbody NE '' 
     THEN DO:
     ASSIGN
        lv-mailto = 'To:' + ls-to-list
        lv-mailsubject = 'Change for Release# ' + STRING(ip-release#)
        lv-mailattach = v-fgemail-file.      
     RUN mail(lv-mailto,lv-mailsubject,lv-mailbody,lv-mailattach,0,OUTPUT retcode).
  END. /* if lv-mailbody */
  
  FOR EACH tt-email WHERE tt-email.release# EQ ip-release#:
      DELETE tt-email.
  END. /* each tt-email */

  
