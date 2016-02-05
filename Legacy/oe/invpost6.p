/* -------------------------------------------------- oe/invpost6.p 02/99 JLF */
/* Bill Of Lading and Invoice Posting - relieve bins                          */
/* -------------------------------------------------------------------------- */
DEF VAR lv-header-cust-no LIKE oe-bolh.cust-no NO-UNDO.
DEF BUFFER b-fg-bin FOR fg-bin.
DEF VAR lv-header-fnd-bin AS LOG NO-UNDO.
DEF VAR lv-header-orig-qty AS INT NO-UNDO.
{oe/invpost6.i "inv-line"}

/* end ---------------------------------- copr. 1999  advanced software, inc. */
