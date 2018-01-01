/***************************************************************************\
*****************************************************************************
**  Program: E:\robj8\patch\ed\tdf\share
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/

DEF {1} var ws_rec_code AS INT NO-UNDO LABEL 'Seq#' FORMAT '999'.
DEF {1} var dtl_rec_code LIKE ws_rec_code NO-UNDO.
DEF {1} var last_rec_code LIKE ws_rec_code NO-UNDO.

DEF {1} var header_sep-code          AS CHAR FORMAT 'X(11)' NO-UNDO
                LABEL 'Separator-Code' INITIAL '00000000000'.
DEF {1} var header_partner    AS CHAR FORMAT 'X(05)' NO-UNDO
                LABEL 'Prtnr-ID'.
DEF {1} var header_setid     AS CHAR FORMAT 'X(06)' NO-UNDO
                LABEL 'SetID'.
DEF {1} var header_std-ver    AS CHAR FORMAT 'X(12)' NO-UNDO
                LABEL 'Std-Ver'.
DEF {1} var header_int-cd     AS CHAR FORMAT 'X(30)' NO-UNDO.
DEF {1} var header_company-id AS CHAR FORMAT 'X(15)' NO-UNDO.

/* 9807 CAH */
DEF {1} var header_std-rcvd     AS char FORMAT 'x(02)' NO-UNDO.
DEF {1} var header_std-used     AS char FORMAT 'x(01)' NO-UNDO.
DEF {1} var header_rcvd-test-prod AS char FORMAT 'x(01)' NO-UNDO.
DEF {1} var header_part-test-prod AS char FORMAT 'x(01)' NO-UNDO.
DEF {1} var header_fgid                     AS char NO-UNDO FORMAT 'x(02)'.
DEF {1} var header_isa                          AS int NO-UNDO FORMAT "999999999".
DEF {1} var header_gs                           AS int NO-UNDO FORMAT "999999999".
DEF {1} var header_st                           AS int NO-UNDO FORMAT "999999999".

def {1} var ws_section                          as int no-undo.

DEF {1} var skip_doc                        AS logical NO-UNDO.

def {1} var ws_version   like eddoc.version  no-undo
    label "Vers".
DEF {1} var ws_segment          AS char FORMAT 'x(03)' NO-UNDO
    label "Seg".
DEF {1} var ws_element          AS char FORMAT 'x(06)' NO-UNDO
    label "Element".
DEF {1} var ws_filetype          AS char FORMAT 'x(06)' NO-UNDO
    label "Output File Type".
DEF {1} var ws_elem_delim          AS char FORMAT 'x(06)' NO-UNDO
    label "Element Delimiter" init "*".	
/* 9805 CP */
DEF {1} var tdf_eof AS logical NO-UNDO initial FALSE LABEL "EOF".

def {1} var str_bigint      as integer  no-undo format "-9999999999999999999".
def {1} var str_bigchar     as char     no-undo format "x(20)".
/* during outbound processing if there is an error set this field */
def {1} var erc_field       as char     no-undo format "x(30)".
