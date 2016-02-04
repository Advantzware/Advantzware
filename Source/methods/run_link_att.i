/* run_link_att.i */

&IF "{&IAMWHAT}" = "" &THEN
IF VALID-HANDLE(adm-broker-hdl) THEN DO:
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'{1}':U,OUTPUT char-hdl) NO-ERROR.
  phandle = WIDGET-HANDLE(char-hdl).
  IF VALID-HANDLE(phandle) THEN DO:
     RUN {2} IN phandle {3} NO-ERROR.
     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attach-source':U, OUTPUT char-hdl) NO-ERROR.
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
      DEF VAR v-att AS LOG NO-UNDO.
      DEF VAR v-name AS CHAR NO-UNDO.
      DEF VAR ip-rec_key AS CHAR NO-UNDO.
      DEF VAR cocode AS CHAR NO-UNDO.
      DEF VAR v-est-no AS CHAR NO-UNDO.
      DEF VAR v-i-no AS CHAR NO-UNDO.

      RUN program-name-proc IN WIDGET-HANDLE(char-hdl) (OUTPUT v-name,OUTPUT ip-rec_key,OUTPUT cocode) NO-ERROR.
      IF v-name NE "" THEN DO:
         IF v-name EQ "oe/b-ordinq" THEN DO:
            {sys/ref/attachlogic.i}
            v-att = CAN-FIND(FIRST attach WHERE
                    attach.company = cocode AND
                    (v-est-no <> "" AND TRIM(attach.est-no) = trim(v-est-no)) OR
                    (v-i-no <> "" AND INDEX(v-i-no,attach.i-no) > 0)).
            RUN paper-clip-image (INPUT v-att) NO-ERROR.
         END.
      END.
    END.
  END.
END.
&ENDIF
