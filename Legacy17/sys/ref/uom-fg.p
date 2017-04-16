/*sys/ref/uom-fg.p */
DEF INPUT  PARAM ip-po AS LOG NO-UNDO.
DEF OUTPUT PARAM op-uom-list AS CHAR NO-UNDO.

DEF VAR v-ea-list AS cha NO-UNDO.


RUN sys/ref/uom-ea.p (OUTPUT v-ea-list).

op-uom-list = (IF NOT ip-po THEN "C,CS,L," ELSE "") +
              (IF ip-po NE ? THEN "MSF,M," ELSE "") +
              v-ea-list.
                                       
