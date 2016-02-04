
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
     po-ord.stat:SCREEN-VALUE = "N"
     lv-copy-from-po-num = INT(po-ord.po-no:SCREEN-VALUE).
END.
