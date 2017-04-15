/* footer.i */

FORMAT HEADER SKIP(1) "Continued on Page:" STRING(PAGE-NUMBER + 1)
      WITH FRAME f-continued PAGE-BOTTOM NO-BOX NO-LABELS WIDTH 132.

FORMAT HEADER SKIP(1) "End of Listing"
      WITH FRAME f-end-footer NO-BOX NO-LABELS WIDTH 132.
