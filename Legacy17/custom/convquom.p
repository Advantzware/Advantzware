/*custom/convquom.p to get around browser's shared var error if it has more than 
                      one page records */
DEF INPUT  PARAM ip-cocode  AS   CHAR NO-UNDO.
DEF INPUT  PARAM ip-fr-uom  LIKE job-mat.sc-uom NO-UNDO.
DEF INPUT  PARAM ip-to-uom  LIKE job-mat.sc-uom NO-UNDO.
DEF INPUT  PARAM ip-basis-w LIKE job-mat.basis-w NO-UNDO.
DEF INPUT  PARAM ip-len     LIKE job-mat.len NO-UNDO.
DEF INPUT  PARAM ip-wid     LIKE job-mat.wid NO-UNDO.
DEF INPUT  PARAM ip-dep     LIKE job-mat.dep NO-UNDO.
DEF INPUT  PARAM ip-in-qty  AS   DEC NO-UNDO.
DEF OUTPUT PARAM op-out-qty AS   DEC NO-UNDO.

{sys/inc/var.i NEW SHARED}

cocode = ip-cocode.

RUN sys/ref/convquom.p(ip-fr-uom,
                       ip-to-uom ,
                       ip-basis-w,
                       ip-len,
                       ip-wid,
                       ip-dep,
                       ip-in-qty,
                       OUTPUT op-out-qty).
