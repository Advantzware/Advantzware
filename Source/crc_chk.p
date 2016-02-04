PAUSE 0 before-hide.
    
FOR EACH _file WHERE _file-name LT "_" AND _crc NE ?:
  DISPLAY _crc (TOTAL).
END.
