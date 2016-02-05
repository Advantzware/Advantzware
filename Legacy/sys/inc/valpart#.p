
DEF INPUT PARAM ip-part-no LIKE itemfg.part-no NO-UNDO.
DEF INPUT PARAM ip-i-no    LIKE itemfg.i-no    NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}


ASSIGN
 cocode = g_company
 locode = g_loc.

{sys/inc/fgpart#.i}

IF fgpart#-log THEN DO:
  FOR EACH itemfg
      WHERE itemfg.company  EQ cocode
        AND itemfg.part-no  EQ ip-part-no
        AND itemfg.i-no     NE ip-i-no
      USE-INDEX cust-part NO-LOCK:
    MESSAGE "Sorry, you may not create/update FG, Part#: " +
            TRIM(ip-part-no)                               +
            " is already used for Cust#/FG Item#: "        +
            TRIM(itemfg.cust-no) + "/"                     +
            TRIM(itemfg.i-no).
    RETURN ERROR.
  END.

  IF CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part") THEN DO:
    RUN sys/inc/valprt#2.p (ip-part-no, ip-i-no) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  END.
END.
