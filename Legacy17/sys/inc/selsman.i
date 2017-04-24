/*sys/inc/selsman.i*/

DEFINE {1} SHARED TEMP-TABLE tt-sman
    FIELD company AS CHAR
    FIELD sman AS CHAR
    INDEX idx company ASC sman ASC.
