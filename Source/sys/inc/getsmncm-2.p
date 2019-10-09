
DEF INPUT         PARAM ip-cust-no LIKE cust.cust-no   NO-UNDO.
DEF INPUT-OUTPUT  PARAM io-sman    LIKE sman.sman      NO-UNDO.
DEF INPUT         PARAM ip-procat  LIKE procat.procat  NO-UNDO.
DEF INPUT         PARAM ip-net-p   AS   DEC            NO-UNDO.
DEF OUTPUT        PARAM op-comm    AS DEC DECIMALS 10  NO-UNDO.
DEF INPUT        PARAM ip-shipto    AS CHAR  NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR li AS INT NO-UNDO.

FIND FIRST cust
    WHERE cust.company EQ cocode
      AND cust.cust-no EQ ip-cust-no
    NO-LOCK NO-ERROR.

IF AVAIL cust THEN
    FIND FIRST shipto WHERE shipto.company = cocode 
                            AND shipto.cust-no = cust.cust-no
                            AND shipto.ship-id = ip-shipto NO-LOCK NO-ERROR.

IF AVAIL shipto AND shipto.spare-char-1 NE "" THEN
     io-sman = shipto.spare-char-1 .

IF io-sman EQ "" THEN io-sman = cust.sman.

FIND FIRST sman
    WHERE sman.company EQ cocode
      AND sman.sman    EQ io-sman
    NO-LOCK NO-ERROR.

IF AVAIL cust THEN
FIND FIRST custype WHERE custype.custype EQ cust.type NO-LOCK NO-ERROR.

IF AVAIL sman THEN
  RUN custom/com-comm.p (cocode, sman.sman, custype.custype, ip-procat, ip-net-p,
                         IF AVAIL cust THEN cust.cust-no ELSE "",
                         OUTPUT op-comm).

ELSE DO:
  IF AVAIL custype AND custype.custype NE "" THEN op-comm = custype.commrate.

  ELSE DO:
    FIND FIRST ce-ctrl
        WHERE ce-ctrl.company EQ cocode
          AND ce-ctrl.loc     EQ locode
        NO-LOCK NO-ERROR.
    IF AVAIL ce-ctrl THEN op-comm = ce-ctrl.comm-mrkup.
  END.
END.
