
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-die-no LIKE eb.die-no NO-UNDO.
DEF INPUT PARAM ip-cad-no LIKE eb.cad-no NO-UNDO.
DEF INPUT PARAM ip-cimage LIKE ef.cad-image NO-UNDO.
DEF INPUT PARAM ip-pgm-name AS CHAR NO-UNDO.
DEF INPUT PARAM ip-upd-die AS LOG NO-UNDO.

RUN est/d-diecad.w (ip-rowid, ip-die-no, ip-cad-no, ip-cimage, ip-pgm-name,
                    ip-upd-die).
