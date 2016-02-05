
DEF {1} SHARED TEMP-TABLE tt-mach-exc FIELD form-no LIKE eb.form-no
                                      FIELD blank-no LIKE eb.blank-no
                                      FIELD m-code LIKE mach.m-code
                                      FIELD reason AS CHAR FORMAT "x(50)"
                                      FIELD dept AS CHAR
                                      FIELD defr AS LOG.
