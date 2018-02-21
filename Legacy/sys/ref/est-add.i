/* ---------------------------------------------- sys/ref/est-add.i 10/97 JLF */
/*                                                                            */
/* create est - recalc & override field defaults                              */
/*                                                                            */
/* -------------------------------------------------------------------------- */

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ {1}.company
      AND sys-ctrl.name    EQ "CERUN" + "{2}"
    NO-ERROR.
IF NOT AVAIL sys-ctrl THEN
FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ {1}.company
      AND sys-ctrl.name    EQ "CERUN"
    NO-ERROR.
{1}.recalc = AVAIL sys-ctrl AND sys-ctrl.log-fld.




FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ {1}.company
      AND sys-ctrl.name    EQ "CEGSA"
    NO-ERROR.
{1}.override = AVAIL sys-ctrl AND sys-ctrl.log-fld.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
