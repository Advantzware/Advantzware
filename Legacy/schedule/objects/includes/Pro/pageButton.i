/* pageButton.i - used in page button triggers in detail.w */

  RUN select-page ({1}).
  RUN passRowID IN h_jobDetail-{1} (jobRowID).
