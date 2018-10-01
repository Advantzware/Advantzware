/* oe-ord.i - window local-view - rstark 9.30.2018 */

IF lv-initial AND NOT AVAILABLE oe-ord THEN DO:
    lv-initial = NO.
    RUN select-page (2).
    RUN select-page (1).  
END.
