/* sys-ctrl-shipto.i */

IF adm-new-record THEN DO:
  {methods/run_link.i "RECORD-SOURCE" "Get-Values" "(OUTPUT opName,OUTPUT opModule)"}
  ASSIGN
    sys-ctrl-shipto.company = gcompany
    sys-ctrl-shipto.name = opName
    sys-ctrl-shipto.module = opModule.
END.
