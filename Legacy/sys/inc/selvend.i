/*sys/inc/selcust.i*/

DEFINE {1} SHARED TEMP-TABLE tt-vend
    FIELD company AS CHAR
    FIELD vend-no AS CHAR
    INDEX idx company ASC vend-no ASC.
