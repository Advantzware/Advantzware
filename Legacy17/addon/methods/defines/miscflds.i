/* miscflds.i */

&scoped-define table-fields ~
  FIELD attr_id AS CHARACTER FORMAT 'X(20)' ~
  FIELD attr_mfgroup AS CHARACTER FORMAT 'X(20)' ~
  FIELD attr_tab AS INTEGER FORMAT 'z9' ~
  FIELD attr_type AS CHARACTER FORMAT 'X(14)' ~
  FIELD attr_label AS CHARACTER FORMAT 'X(30)' ~
  FIELD attr_values AS CHARACTER FORMAT 'X(80)' ~
  FIELD attr_default AS CHARACTER FORMAT 'X(30)' ~
  FIELD attr_x AS INTEGER FORMAT 'zzzz9' ~
  FIELD attr_y AS INTEGER FORMAT 'zzzz9' ~
  FIELD attr_height AS INTEGER FORMAT 'zzzz9' ~
  FIELD attr_width AS INTEGER FORMAT 'zzzz9' ~
  FIELD attr_settings AS CHARACTER FORMAT 'X(30)' ~
  FIELD attr_datatype AS CHARACTER FORMAT 'X(10)'

&IF "{&NEW}" = "NEW" &THEN
DEFINE TEMP-TABLE mfgroup NO-UNDO
  FIELD mfgroup AS CHARACTER
  FIELD tab-labels AS CHARACTER
    INDEX pi-mfgroup IS PRIMARY
      mfgroup.

DEFINE WORK-TABLE wtbl-clipboard NO-UNDO
  {&table-fields}.
&ENDIF

DEFINE {&NEW} SHARED TEMP-TABLE attrb NO-UNDO
  {&table-fields}
    INDEX pi-attrb IS PRIMARY
      attr_mfgroup
      attr_tab
      attr_y
      attr_x
    INDEX si-attrb IS UNIQUE
      attr_id.
