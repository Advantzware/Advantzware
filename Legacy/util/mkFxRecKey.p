/* mkfxreckey.p */

OUTPUT TO 'util/fxRecKey.i'.

PUT UNFORMATTED 'PROCEDURE fixTables:' SKIP.
FOR EACH _file NO-LOCK,
    FIRST _field OF _file NO-LOCK WHERE _field-name EQ 'rec_key':
  PUT UNFORMATTED '  RUN fix' CAPS(_file-name) '.' SKIP.
END.
PUT UNFORMATTED 'END PROCEDURE.' SKIP(1).

FOR EACH _file NO-LOCK,
    FIRST _field OF _file NO-LOCK WHERE _field-name EQ 'rec_key':
  PUT UNFORMATTED
    'PROCEDURE fix' CAPS(_file-name) ':' SKIP
    '  DEFINE VARIABLE i AS INTEGER NO-UNDO.' SKIP(1)
    '  IF NOT CAN-FIND(FIRST ' _file-name ' WHERE ' _file-name
    '.rec_key EQ ~'~') THEN RETURN.' SKIP
    '  RUN msg (~'Processing ' _file-name ' ... ~').' SKIP
    '  FOR EACH ' _file-name ' EXCLUSIVE-LOCK WHERE ' _file-name '.rec_key EQ ~'~':' SKIP
    '    ASSIGN' SKIP
    '      ' _file-name '.rec_key = nextRecKey()' SKIP
    '      i = i + 1.' SKIP
    '    RUN createRecKey (' _file-name '.rec_key,~'' _file-name '~').' SKIP
    '  END.' SKIP
    '  RUN msg (~'Fixed ~' + STRING(i) + ~' Records~' + CHR(10)).' SKIP
    'END PROCEDURE.' SKIP(1).
END.

OUTPUT CLOSE.
