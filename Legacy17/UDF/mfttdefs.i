/* mfttDefs.i */

&SCOPED-DEFINE table-fields ~
  FIELD attr_id AS CHARACTER FORMAT 'X(20)' ~
  FIELD attr_mfgroup AS CHARACTER FORMAT 'X(20)' ~
  FIELD attr_tab AS INTEGER FORMAT 'z9' ~
  FIELD attr_type AS CHARACTER FORMAT 'X(14)' ~
  FIELD attr_label AS CHARACTER FORMAT 'X(30)' ~
  FIELD attr_name AS CHARACTER FORMAT 'X(30)' ~
  FIELD attr_values AS CHARACTER FORMAT 'X(80)' ~
  FIELD attr_default AS CHARACTER FORMAT 'X(30)' ~
  FIELD attr_x AS INTEGER FORMAT 'zzzz9' ~
  FIELD attr_y AS INTEGER FORMAT 'zzzz9' ~
  FIELD attr_height AS INTEGER FORMAT 'zzzz9' ~
  FIELD attr_width AS INTEGER FORMAT 'zzzz9' ~
  FIELD attr_settings AS CHARACTER FORMAT 'X(30)' ~
  FIELD attr_datatype AS CHARACTER FORMAT 'X(10)' ~
  FIELD attr_order AS INTEGER FORMAT 'z9' ~
  FIELD attr_enabled AS LOGICAL ~
  FIELD attr_proc AS CHARACTER FORMAT 'X(40)' ~
  FIELD attr_colLabel AS CHARACTER FORMAT 'X(20)' ~
  FIELD attr_sbField AS INTEGER FORMAT '>9' ~
  FIELD attr_esko AS LOGICAL

&IF "{&NEW}" EQ "NEW SHARED" &THEN
DEFINE TEMP-TABLE ttMFGroup NO-UNDO LIKE {&dbnm}mfgroup.

DEFINE WORK-TABLE wtClipboard NO-UNDO
  {&table-fields}.
&ENDIF

DEFINE {&NEW} VARIABLE hMFPersist AS HANDLE NO-UNDO.

DEFINE {&NEW} TEMP-TABLE ttAttrb NO-UNDO
  {&table-fields}
    INDEX pi-attrb IS PRIMARY
      attr_mfgroup
      attr_tab
      attr_order
    INDEX si-attrb IS UNIQUE
      attr_id
    INDEX si-name
      attr_name.

DEFINE {&NEW} TEMP-TABLE ttMFPrgrms NO-UNDO
  FIELD mfgroup    AS CHARACTER
  FIELD prgmname LIKE prgrms.prgmname
  FIELD prgtitle LIKE prgrms.prgtitle
    INDEX ttMFPrgrms IS PRIMARY prgmname.
