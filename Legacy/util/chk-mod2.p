/* util/chk-mod2.p  Check modlue liscense */

DEFINE INPUT PARAMETER ip-module-code AS CHARACTER NO-UNDO. /* button label*/
DEF INPUT PARAM ip-module-dscr AS cha NO-UNDO.
DEF INPUT PARAM ip-message AS cha NO-UNDO.

IF INDEX(ip-module-code,"&") > 0 THEN
   ip-module-code = REPLACE(ip-module-code,"&","").

FIND FIRST asi.module WHERE asi.module.module = ip-module-code
                        AND asi.module.dscr = ip-module-dscr NO-LOCK NO-ERROR.

IF AVAIL asi.module AND (NOT asi.module.is-used OR asi.module.expire-date < TODAY)
THEN do:
    MESSAGE "System Liscense Error!" SKIP
            "Contact Advance Software Inc. to purchase" ip-message VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
