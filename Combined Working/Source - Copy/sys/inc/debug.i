/* --------------------------------------------------sys/inc/debug.i 1/95 cts */
/*                                                                            */
/* debugging include file                                                     */
/*                                                                            */
/* usage:                                                                     */
/* {sys/inc/debug.i "Text message #1" "vars/fields #1"                        */
/*                  "Text message #2" "vars/fields #2"}                       */
/*                                                                            */
/* Each parameter is optional.                                                */
/*                                                                            */
/* -------------------------------------------------------------------------- */

IF debug-mode-on THEN
DO:
  MESSAGE "DEBUG1:" "{1}" {2}.
  MESSAGE "DEBUG2:" "{3}" {4}.
  PAUSE MESSAGE "Press any key to continue processing...".
END.

/* end ---------------------------------- copr. 1995  advanced software, inc. */
