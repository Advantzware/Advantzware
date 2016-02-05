&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME loadtag

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.
{custom/globdefs.i}

IF NOT loadtag.item-type  THEN DO:
   FIND FIRST fg-bin WHERE fg-bin.company EQ g_company 
                    AND fg-bin.i-no    EQ loadtag.i-no
                    /*AND fg-bin.loc     EQ loadtag.loc
                      AND fg-bin.loc-bin EQ loadtag.loc-bin*/
                      AND fg-bin.tag     EQ loadtag.tag-no
                      AND loadtag.tag-no <> ""
                      /*AND fg-bin.job-no = loadtag.job-no
                      AND fg-bin.job-no2 = loadtag.job-no2*/
                      NO-LOCK NO-ERROR.
   IF AVAIL fg-bin THEN do:
      MESSAGE "Can't Delete. Inventory exists for the tag." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
   END.
   FIND FIRST fg-rctd WHERE fg-rctd.company = g_company
                        AND fg-rctd.tag = loadtag.tag-no
                        AND loadtag.tag-no <> ""
                        AND fg-rctd.i-no = loadtag.i-no
                        AND fg-rctd.rita-code <> "P"
                        NO-LOCK NO-ERROR.
   IF AVAIL fg-rctd THEN do:
      MESSAGE "Can't Delete. Not Posted FG Warehouse Transactions exist." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
   END.
END.

{methods/triggers/delete.i}
