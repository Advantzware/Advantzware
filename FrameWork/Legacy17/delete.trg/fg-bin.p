&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME fg-bin

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

/* IF fg-bin.i-no = "" THEN DO:                                   */
/*    MESSAGE "F/G Bin can not be deleted. " SKIP                 */
/*            "Contact System Administrator. " VIEW-AS ALERT-BOX. */
/*    RETURN ERROR.                                               */
/* END. /* see task 04210911 */                                   */

{methods/triggers/delete.i}


IF TRIM({&TABLENAME}.tag) NE "" THEN
FOR EACH loadtag
    WHERE loadtag.company      EQ {&TABLENAME}.company
      AND loadtag.item-type    EQ NO
      AND loadtag.tag-no       EQ {&TABLENAME}.tag
      AND loadtag.i-no         EQ {&TABLENAME}.i-no
      AND loadtag.loc          EQ {&TABLENAME}.loc      /* task# 06130519 */
      AND loadtag.loc-bin      EQ {&TABLENAME}.loc-bin  /* Fg Transfer changes */
      AND loadtag.is-case-tag  EQ NO                    /* loadtags's values*/
    USE-INDEX tag:
  ASSIGN
   loadtag.qty          = 0
   loadtag.pallet-count = 0
   loadtag.partial      = 0
   loadtag.tot-cases    = 0.
END.
