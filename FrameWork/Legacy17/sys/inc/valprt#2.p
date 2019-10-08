
DEF INPUT PARAM ip-part-no LIKE itemfg.part-no NO-UNDO.
DEF INPUT PARAM ip-i-no    LIKE itemfg.i-no    NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}


ASSIGN
 cocode = g_company
 locode = g_loc.

FOR EACH cust-part
    WHERE cust-part.company EQ cocode
      AND cust-part.part-no EQ ip-part-no
      AND cust-part.i-no    NE ip-i-no
      AND cust-part.cust-no NE FILL("*",20)
    USE-INDEX part-no NO-LOCK:
  MESSAGE "Sorry, you may not create/update FG, Part#: " +
          TRIM(ip-part-no)                               +
          " is already used for Cust#/FG Item#: "        +
          TRIM(cust-part.cust-no) + "/"                  +
          TRIM(cust-part.i-no).
  RETURN ERROR.
END.
