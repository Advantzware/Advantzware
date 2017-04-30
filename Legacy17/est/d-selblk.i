
DEF {1} SHARED TEMP-TABLE tt-select FIELD tt-rowid AS ROWID
                                    FIELD tt-selected AS LOG
                                    FIELD tt-blank-no LIKE eb.blank-no
                                    INDEX tt-select tt-selected.
