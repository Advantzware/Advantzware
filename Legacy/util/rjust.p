/* -------------------------------------------------- util/rjust.p  11/95 BSM */
/* Right justify a character string                                           */
/* -------------------------------------------------------------------------- */
define input-output parameter v-char as char no-undo.
define input        parameter v-size as inte no-undo.

define var v-format as char no-undo.

v-format = fill(">", v-size - 1) + "9".

v-char = string(integer(v-char), v-format).
/* end ---------------------------------- copr. 1995  Advanced Software, Inc. */
