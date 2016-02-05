/*sys/inc/selcust.i*/

DEFINE {1} SHARED TEMP-TABLE tt-cust
    FIELD company AS CHAR
    FIELD cust-no AS CHAR
    INDEX idx company ASC cust-no ASC.
