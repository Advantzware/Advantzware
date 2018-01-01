/* RUN rc/writelog.p ("BOJ", ''). */

/* rc/loginv.i - standard login shared VARs
11.19.96 by CAH on ricky@812<rprodev> Log#0000:
1.  Added ws_custpo.

12.22.95 by CAH on CDS972@812<rprodemo> Log#0000:
1.  Added ws_char,int,date,recid,decimal,logical
    so that all procs will have available a scratch variable of each type.

11.18.94 by CAH on B28@812 Log#0000:
1.  Added hyperhelp_key and keycap for differentiating hypertext help calls
    from normal help calls.

10.05.94 by CAH on B28@812 Log#0000:
1.  Added ws_local for tax jd pick return.

05.04.94 by CAH on B28@111 Log#0000:
1.  Included rc/palette.i here so that all procedures will have it.  When
    better (file-based) mechanism is available then it should be
    easier to propogate to all procedures and include files.

05.01.94 by CAH on B28@111 Log#0000:
1.  Made column-label on ws_dept 'Dept' as spelled out was wider than field.

11.07.92 by CH:
1.  Added beginning of job logging call.

09.23.92 by CH:
1.  Added ws_calendar, required for report heading include files.  Assigned
    during login.

04.03.92 by CH:
1.  Added ws_dept, required for FS system, GL, IC, etc.

11.22.91 by CH:
1.  Added ws_style, required for ic/mastpick.

05.22.91 by CH:
1.  Removed all like file.fld reference to eliminate
    scoping of these files to any procedures.  Was causing
    qualification problems.

05.20.91 by ch: Added ws_per and ws_mo, required for app/login.p's.

02.22.91 by ch: save_ VARs for preserving printer selection

02.16.91 by ch:
1.  added lo_stat and hi_stat for use in transaction report extracts.

01.24.91 by CH:
1.  corrected labels of ws_vars to clean up displays.

12.21.90 BY JZ:  added shared vars for applhelp.p.  See EOF for explanation.
12.17.90 BY CH:  added ws_banknum, used in both AP and AR
12.17.90 by jz:  added ws_app for application signature.  This made it
                 significantly easier to implement smart jsc picks.
12.07.90 by jz:  included contents of lib\sharedv.i, necessary for HELP and
                 scroll picks.

10.29.90 by ch:  initial create
*/

/* Shared VARs for system includes: */
/*   for pick lists: */
DEF {1} SHARED VAR   lib_recid_ret   as recid no-undo.

def {1} shared var scr_firstkey as INT format "999" no-undo.
def {1} shared var scr_lastkey  as INT format "999" no-undo.
def {1} shared var scr_firstkeycap as CHAR format "x(20)" no-undo.
def {1} shared var scr_lastkeycap as CHAR  format "x(20)" no-undo.
def {1} shared var hyperhelp_key as INT format "999" no-undo.
def {1} shared var hyperhelp_keycap as CHAR format "x(20)" no-undo.

/*   for printer support: */
def {1} shared var save_printstring as CHAR format "x(78)" NO-UNDO.
def {1} shared var save_setupdescription as CHAR format "x(30)" NO-UNDO.
def {1} shared var save_printfid as CHAR format "x(10)" NO-UNDO.
def {1} shared var save_emulation as char no-undo.

/*   for extraction ranges and related purposes: */
def {1} shared var lo_stat as INT format "9" label "From Status" NO-UNDO.
def {1} shared var hi_stat as INT format "9" label "Thru Status" NO-UNDO.
DEF {1} SHARED VAR ws_co         as INT format ">9"
    LABEL "Company#" COLUMN-LABEL "Co" initial 1 NO-UNDO.
DEF {1} SHARED VAR lo_co LIKE ws_co label "From Co#" NO-UNDO.
DEF {1} SHARED VAR hi_co LIKE ws_co label "Thru Co#" NO-UNDO.
DEF {1} SHARED VAR ws_co_name    as CHAR format "x(30)"
    LABEL "Company name" COLUMN-LABEL "Company Name" NO-UNDO.
DEF {1} SHARED VAR ws_perdate AS DATE INITIAL TODAY
    LABEL "P/E Date" COLUMN-LABEL "P/E Date" NO-UNDO.
DEF {1} SHARED VAR ws_per        AS INT FORMAT ">>>9"
    LABEL "Period (YYMM)" column-label "Per" NO-UNDO.
DEF {1} SHARED VAR ws_mo   AS INT FORMAT ">9"     /* bucket index */
    LABEL "Month" COLUMN-LABEL "Month" NO-UNDO.
def {1} shared var ws_year  as int format "9999" /* year pointer */
    label "Year" column-label "Year" no-undo.    
DEF {1} SHARED VAR ws_userid     AS CHAR FORMAT "X(03)"
    LABEL "Operator" COLUMN-LABEL "Op" NO-UNDO.
DEF {1} SHARED VAR ws_password   AS CHAR FORMAT "X(12)"
    LABEL "Password" NO-UNDO.
DEF {1} SHARED VAR ws_app        AS CHAR FORMAT "X(02)" INITIAL "RC".
/* added 12.17.90 by ch: */
DEF {1} SHARED VAR ws_banknum    as INT format ">9" initial 1
    label "Bank#" NO-UNDO.

/* The following VAR are needed by applhelp.p to qualify scroll picks.
   Previously, they were declared as NEW SHARED in applhelp.p, but this caused
   them to be masked if they previously existed.  PROGRESS's scoping rules
   allow no easy way around this other than putting them in a high-level file,
   and propagating them downward.
*/

DEF {1} shared var ws_vendor   as INT format ">>>>>9" label "Vendor#" NO-UNDO.
DEF {1} shared var ws_cust     as INT format ">>>>>9" label "Cust#" NO-UNDO.
DEF {1} shared var ws_emp      as INT format ">>>>>9" label "Emp#" NO-UNDO.
DEF {1} shared var ws_job      as INT format ">>>>" label "Job#" NO-UNDO.
DEF {1} shared var ws_jsc      as INT format ">9" label "Jsc#" NO-UNDO.
DEF {1} shared var ws_acct     as CHAR format "X(15)" label "GL Acct#" NO-UNDO.
DEF {1} shared var ws_svc      as INT format ">>9" label "Svc#" NO-UNDO.
DEF {1} shared var ws_terms    as CHAR format "X(02)" label "Terms" NO-UNDO.
DEF {1} shared var ws_shipvia  as CHAR format "X(03)" label "ShipVia" NO-UNDO.
DEF {1} shared var ws_style    as CHAR format "x(15)"
    label "Style" column-label "Style" no-undo.

def {1} shared var ws_div  as char format "x(02)" no-undo
    label "Div"  column-label "Div".
def {1} shared var ws_unit as char format "x(05)" no-undo    
    label "Unit" column-label "Unit".
def {1} SHARED VAR ws_dept AS CHAR FORMAT "X(10)" no-undo
    LABEL "Dept" COLUMN-LABEL "Dept".

def {1} shared var ws_calendar as int no-undo label "Calendar#".
def {1} shared var ws_state as char format 'x(02)' no-undo label "State".
def {1} shared var ws_city  as char format 'x(23)' no-undo label "City".
def {1} shared var ws_zip   as char format 'x(10)' no-undo label "Zip".
def {1} shared var ws_fax   as char format 'x(15)' no-undo label "Fax#".
def {1} shared var ws_local as int  format '>>>>'  no-undo label "TaxJD".

def {1} shared var ws_int       as integer  no-undo label "Number".
def {1} shared var ws_char      as char     no-undo label "Char".
def {1} shared var ws_recid     as recid    no-undo label "Recid".
def {1} shared var ws_date      as date     no-undo label "Date".
def {1} shared var ws_logical   as logical  no-undo label "Logical".
def {1} shared var ws_decimal   as decimal  no-undo label "Decimal".

{rc/palette.i}

def {1} shared var spool_recid  as recid no-undo.
def {1} shared var ws_sic       as char format "x(04)"
    label "SIC Code" Column-label "SIC".
def {1} shared var ws_custpo   as char format "x(16)" no-undo
    Label "Cust PO#".
def {1} shared var printer_sel_proc as char no-undo initial "rc/getprint.p".


DEFINE {1} SHARED VAR ws_ref  as integer format ">>>>>>>>>9"
    label "Invoice#" column-label "Inv#" NO-UNDO.

DEFINE {1} SHARED VAR ws_seq  as integer format ">>9"
    label "Seq#"  NO-UNDO.

def {1} shared var ws_textid as char no-undo
    label "Text ID".

def {1} shared var ws_paytype as char format "9" no-undo
    label "Pay Type" column-label "Pay!Typ".

def {1} shared var ws_shipto as integer format ">>>>>>" no-undo
    label "Ship#".

DEF {1} SHARED VAR WS_ORDERNUM AS INTEGER FORMAT ">>>>>>>>>9" NO-UNDO
    LABEL "Order#".

/* 9704 CAH: Added to support calling and error returns */
def {1} shared var called as logical no-undo.
def {1} shared var caller as char format "x(15)" no-undo.
def {1} shared var next_program as char format "x(20)" no-undo.
def {1} shared var ws_process_rec   as recid no-undo.
DEF {1} shared var ws_erc AS INT format "->>>>>" NO-UNDO label "Erc".
def {1} shared var ws_erc_desc as char no-undo.

def {1} shared var dirsep       as char no-undo format 'x(01)' initial "/".


/*
06.23.96 by CAH on Ricky@812<rprodemo> Log#0000:
1.  Changed default page_len back from 63 to 60.  Was overflowing on
    customer laser printers set for 60.
06.09.96 by CAH on Ricky@812<rprodemo> Log#0000:
1.  Added support for export stream.
2.  Inserted variables from std headings.  Putting them here allows them
to be initialized or tested between inclusion of hdg-xxx.i and getprint.i.
*/

def {1} shared var printfid AS char FORMAT 'x(50)' NO-UNDO.
def {1} shared var printdest AS char NO-UNDO.
def {1} shared var printstring as char format 'x(78)' NO-UNDO.

def {1} shared var print-width AS int INITIAL 80 NO-UNDO.
def {1} shared var dos_command AS char FORMAT 'x(60)' NO-UNDO.
def {1} shared var page_len AS int NO-UNDO INITIAL 60.

def {1} shared var hdg_name     as char format 'x(40)' NO-UNDO.
def {1} shared var hdg_text     as char format 'X(40)' NO-UNDO.
DEF {1} shared VAR hdg_left     AS CHAR FORMAT 'X(40)' NO-UNDO.
DEF {1} shared VAR hdg_right    AS CHAR FORMAT 'X(40)' NO-UNDO.
def {1} shared var hdg_rpt_code as char format 'x(18)' /* 9704 CAH was 15 */
    no-undo.
def {1} shared var hdg_desc as char format 'x(30)'
    NO-UNDO.
def {1} shared var hdg_perdate as date NO-UNDO.
def {1} shared var hdg_widerpt as logical initial false NO-UNDO.

def stream s-export.
def {1} shared var export_opt as logical no-undo
    format 'Y/N' label 'Create Export File?'.
def {1} shared var export_capable as logical no-undo initial false.
def {1} shared var export_fid as char no-undo initial 'export.txt'
    format 'x(30)' label 'Export Filename'.
def {1} shared var export_open as logical no-undo initial false.
def {1} shared var export_action as char no-undo initial 'R' format '!'.

def {1} shared var screen-name     as char format "x(40)"   no-undo.

/* MESSAGE AND LOG VARIABLES */
def {1} shared var msg1            as char format "x(76)"   no-undo.
def {1} shared var msg2            as char format "x(76)"   no-undo.
def {1} shared var msg-is-err      as logi                  no-undo.
def {1} shared var msg-is-warn     as logi                  no-undo.
def {1} shared var msg-sec         as int                   no-undo.
def {1} shared var msg-sec-dflt    as int init 3            no-undo.
if "{1}" <> "" then msg-sec = msg-sec-dflt.

/* FOLLOWING VARIABLE INDICATES BATCH MODE */
def {1} shared var x-batch-mode as logi no-undo.

/* FOLLOWING VARIABLES INDICATES ERROR STATUS IN BATCH MODE */
def {1} shared var x-batch-err  as logi no-undo.
def {1} shared var x-batch-msg1 as char no-undo.
def {1} shared var x-batch-msg2 as char no-undo.

/* FOLLOWING VARIABLES INDICATE ERROR OR ENDKEY STAT IN ONLINE OR BATCH MODE */
def {1} shared var x-have-err   as logi no-undo.
def {1} shared var x-have-F4    as logi no-undo.

def {1} shared var login_userid   as char format "x(12)"    NO-UNDO
    label "Login Userid".
def {1} shared var login_co       like ws_co                NO-UNDO
    label "Login Company".
def {1} shared var login_dept     like ws_dept              NO-UNDO
    label "Login Department".
def {1} shared var login_group    as char format "x(12)"    NO-UNDO
    label "Login Group".
def {1} shared var single_company AS LOGICAL                NO-UNDO.
def {1} shared var company_count  AS INTEGER                NO-UNDO.
def {1} shared var single_dept    AS LOGICAL                NO-UNDO.
def {1} shared var dept_count     AS INTEGER                NO-UNDO.
def {1} shared var ws_dept_name   as char format 'x(30)'    NO-UNDO.

def {1} shared var valid_co       as char no-undo.
def {1} shared var valid_dept     as char no-undo.


def {1} shared var batch_capable   as logical initial false no-undo.
def {1} shared var batch_job       as logical initial false no-undo.
def {1} shared var ws_progid       as character no-undo.
    /* name of this program */
def {1} shared var batch_qrec      as recid no-undo.

/* 970728 CAH: Required in AR for Comm Buildup */
DEF {1} SHARED VAR WS_PO        as int format ">>>>>>>>>9"    NO-UNDO
    label "Our PO#".
DEF {1} SHARED VAR WS_POLINE    as int format ">>9"        no-undo
    label "PO Line#" column-label "L#".

/* 9708 CAH: Inserted to programs can determine how they were on menu */
def {1} shared var v-funct-name     as char NO-UNDO FORMAT 'X(30)'.
def {1} shared var v-run-funct      as char NO-UNDO FORMAT 'X(30)'.
def {1} shared var v-hdr            as char FORMAT "x(60)"  NO-UNDO.
def {1} shared var v-init-menu      as char initial "rcmenu" no-undo
    FORMAT 'X(30)'.
DEF {1} shared VAR v-hold-entry     as int NO-UNDO.

/* 9709 CAH: Moved to top level from FS module */
def {1} shared var ws_lastday as int no-undo.
def {1} shared var ws_daynames as char no-undo
    initial "Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday".
    
/* message "Div:" ws_div "Cal:" ws_calendar "Cut:" ws_lastday. */

def {1} shared var ws_context as char no-undo  /* 9712 CAH: for rc/helper */
    label "Help Context".
    
def {1} shared var top-debug as logical no-undo initial false.
if top-debug then do:
    MESSAGE "Debug In:" program-name(1) 
        "Debug Msg:" update ws_context format "x(30)".
    run rc/debugmsg.p (input ws_context).    
end.

    
