/* byReopenBrowse.i - used in procedure reopenBrowse in viewersInclude.i */

    WHEN '{1}Field{2}' THEN
    IF ascendingSort THEN RUN By{1}Field{2}Ascending.
    ELSE RUN By{1}Field{2}Descending.
