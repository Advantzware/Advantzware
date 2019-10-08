DEF {1} var ws_rec_code AS INT NO-UNDO LABEL "Seq#" format "99".
DEF {1} var dtl_rec_code LIKE ws_rec_code NO-UNDO.
DEF {1} var last_rec_code LIKE ws_rec_code NO-UNDO.
DEF {1} var rms_sep_code          AS CHAR FORMAT 'X(05)' NO-UNDO
  LABEL "Separator-Code" INITIAL "*****".
DEF {1} var rms_header_partner    AS CHAR FORMAT 'X(05)' NO-UNDO
  LABEL "Prtnr-ID".
DEF {1} var rms_header_setid     AS CHAR FORMAT 'X(03)' NO-UNDO
  LABEL "SetID".
DEF {1} var rms_header_std-ver    AS CHAR FORMAT 'X(12)' NO-UNDO
  LABEL "Std-Ver".
DEF {1} var rms_header_int-cd     AS CHAR FORMAT 'X(30)' NO-UNDO.
DEF {1} var rms_header_company-id AS CHAR FORMAT 'X(05)' NO-UNDO.
