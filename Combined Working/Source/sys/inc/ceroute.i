
DEF VAR ceroute-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR ceroute-chr LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR ceroute-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR ceroute-dec LIKE sys-ctrl.dec-fld NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "CEROUTE" + "{1}"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CEROUTE" + "{1}".

  IF "{1}" EQ "" THEN
    ASSIGN
     sys-ctrl.log-fld = YES
     sys-ctrl.descrip = "Show 'Machines Excluded from Routing' box when building machine stds?".

  ELSE DO:
    ASSIGN
     sys-ctrl.log-fld = NO
     sys-ctrl.descrip = "Build machine routing from page 1 of Estimate (" +
                        (IF "{1}" EQ "F" THEN "Fold" ELSE "Corr") + "ware)".

    /*IF "{1}" EQ "C" THEN
      MESSAGE sys-ctrl.descrip
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE sys-ctrl.log-fld.  */

    MESSAGE "Please enter the default layout machine (" +
            (IF "{1}" EQ "F" THEN "Fold" ELSE "Corr") + "ware)"
            UPDATE sys-ctrl.char-fld.
  END.
END.

ASSIGN
 ceroute-log = sys-ctrl.log-fld
 ceroute-chr = sys-ctrl.char-fld
 ceroute-int = sys-ctrl.int-fld
 ceroute-dec = sys-ctrl.dec-fld.
