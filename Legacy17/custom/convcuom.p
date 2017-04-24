/* custom/convcuom.p  to get around browser's shared var error if it has more than 
                      one page records */
                      
DEF INPUT  PARAM ip-cocode  AS   CHAR NO-UNDO.
DEF INPUT  PARAM ip-fr-uom  LIKE job-mat.sc-uom NO-UNDO.
DEF INPUT  PARAM ip-to-uom  LIKE job-mat.sc-uom NO-UNDO.
DEF INPUT  PARAM ip-basis-w LIKE job-mat.basis-w NO-UNDO.
DEF INPUT  PARAM ip-len     LIKE job-mat.len NO-UNDO.
DEF INPUT  PARAM ip-wid     LIKE job-mat.wid NO-UNDO.
DEF INPUT  PARAM ip-dep     LIKE job-mat.dep NO-UNDO.
DEF INPUT  PARAM ip-in-cst  as   dec decimals 10 no-undo.
DEF OUTPUT PARAM op-out-cst as   dec decimals 10 no-undo.

{sys/inc/var.i NEW SHARED}

cocode = ip-cocode.

RUN sys/ref/convcuom.p(ip-fr-uom,
                  ip-to-uom ,
                  ip-basis-w,
                  ip-len,
                  ip-wid,
                  ip-dep,
                  ip-in-cst,
                  OUTPUT op-out-cst).
