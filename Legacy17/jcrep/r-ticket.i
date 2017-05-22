
DEF {1} VAR fjob-no         LIKE job-hdr.job-no NO-UNDO.
DEF {1} VAR tjob-no         LIKE fjob-no INIT "zzzzzz" NO-UNDO.
DEF {1} VAR fjob-no2        LIKE job-hdr.job-no2 NO-UNDO.
DEF {1} VAR tjob-no2        LIKE fjob-no2 INIT 99 NO-UNDO.
DEF {1} VAR fmach           LIKE job-mch.m-code NO-UNDO.                  /* dd 03/30/2007 */
DEF {1} VAR tmach           LIKE job-mch.m-code   INIT 'zzzzzz' NO-UNDO.  /* dd 03/30/2007 */
DEF {1} VAR fform           LIKE job-mch.frm      INIT 0 NO-UNDO.         /* dd 03/30/2007 */
DEF {1} VAR tform           LIKE job-mch.frm      INIT 99 NO-UNDO.        /* dd 03/30/2007 */
DEF {1} VAR fblnk           LIKE job-mch.blank-no INIT 0 NO-UNDO.         /* dd 03/30/2007 */
DEF {1} VAR tblnk           LIKE job-mch.blank-no INIT 99 NO-UNDO.        /* dd 03/30/2007 */
DEF {1} VAR reprint         AS   LOG FORMAT "Y/N" NO-UNDO.
DEF {1} VAR print-box       AS   LOG FORMAT "Y/N" NO-UNDO.
DEF {1} VAR spec-list       AS   CHAR NO-UNDO.
DEF {1} VAR approve         AS   LOG NO-UNDO.
DEF {1} VAR production      AS   LOG NO-UNDO.
DEF {1} VAR lDraft          AS   LOG NO-UNDO.
