/* util/chk-mod.p  Check module license */
DEF INPUT PARAM ip-db-name AS cha NO-UNDO.
DEFINE INPUT PARAMETER ip-module-code AS CHARACTER NO-UNDO. /* button label*/

IF INDEX(ip-module-code,"&") > 0 THEN
   ip-module-code = REPLACE(ip-module-code,"&","").

FIND FIRST asi.module WHERE asi.module.db-name = ip-db-name AND
                            asi.module.module = ip-module-code NO-LOCK NO-ERROR.

IF AVAIL asi.module AND (NOT asi.module.is-used OR asi.module.expire-date < TODAY)
THEN do:
    MESSAGE "System License Error!" SKIP
            "Contact Advance Software Inc. to purchase this module." VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
