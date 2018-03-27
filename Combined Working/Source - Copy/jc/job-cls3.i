
DEF {1} SHARED WORKFILE w-qty
  FIELD s-num  LIKE job-hdr.frm
  FIELD b-num  LIKE job-hdr.blank-no
  FIELD p-type AS   INT
  FIELD seq    LIKE dept.fc
  FIELD dept   LIKE mch-act.dept
  FIELD pass   LIKE mch-act.pass
  FIELD m-code LIKE mch-act.m-code
  FIELD n-on   AS   INT
  FIELD fed    AS   DEC
  FIELD fin    AS   DEC
  FIELD wst    AS   DEC
  FIELD out    AS   INT.
