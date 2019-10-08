
DEF {1} SHARED TEMP-TABLE inks NO-UNDO 
                    FIELD ps LIKE eb.i-ps   EXTENT 1
                    FIELD cd LIKE eb.i-code EXTENT 1
                    FIELD ds LIKE eb.i-dscr EXTENT 1
                    FIELD pc LIKE eb.i-%    EXTENT 1
                    FIELD side AS CHAR EXTENT 1
                    FIELD iv LIKE item.mat-type.

