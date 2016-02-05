/* custom/compflds.i  Compare all fill-in fields screen-value and db values
                     to see any field changed 
                     Need procedure get-query-handle in record-source procedure
                          with output param for query handle */
                     
DEF VAR lv-table-hd AS HANDLE NO-UNDO.
DEF VAR lv-cnt AS INT NO-UNDO.
DEF VAR lv-fillin-hd AS HANDLE NO-UNDO.
DEF VAR lv-query-hd AS HANDLE NO-UNDO.
DEF VAR lv-record-val AS cha NO-UNDO.
DEF VAR lv-tmp-hd AS HANDLE NO-UNDO.

RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT lv-record-val).
RUN get-query-handle IN WIDGET-HANDLE(lv-record-val) (OUTPUT lv-query-hd).

DO lv-cnt = 1 TO lv-query-hd:NUM-BUFFERS:
   lv-tmp-hd = lv-query-hd:GET-BUFFER-HANDLE(lv-cnt).
   CREATE BUFFER lv-table-hd FOR TABLE lv-tmp-hd:NAME.
   lv-table-hd:FIND-BY-ROWID(lv-tmp-hd:ROWID, NO-LOCK).
   ASSIGN lv-fillin-hd = FRAME {&FRAME-NAME}:FIRST-CHILD
          lv-fillin-hd = lv-fillin-hd:FIRST-CHILD.

   DO WHILE VALID-HANDLE(lv-fillin-hd):
      IF /*lv-fillin-hd:TYPE = "Fill-In" AND*/ lv-table-hd:TABLE = lv-fillin-hd:TABLE THEN DO:
          /* compare screen-value and db value */
         IF lv-table-hd:BUFFER-FIELD(lv-fillin-hd:NAME):BUFFER-VALUE <> lv-fillin-hd:SCREEN-VALUE 
         THEN MESSAGE "Table: " lv-table-hd:NAME "   Field: " lv-fillin-hd:NAME SKIP(1)
                      "Saved Database: " lv-table-hd:BUFFER-FIELD(lv-fillin-hd:NAME):BUFFER-VALUE SKIP
                      "New Screen Value: " lv-fillin-hd:SCREEN-VALUE VIEW-AS ALERT-BOX.
      END.
      ASSIGN lv-fillin-hd = lv-fillin-hd:NEXT-SIBLING.
   END.
    
END.



