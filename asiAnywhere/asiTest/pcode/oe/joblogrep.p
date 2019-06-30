
/*------------------------------------------------------------------------
    File        : jobblogrep.p
    Purpose     :  Job Log Report

    Syntax      :

    Description : Return a Dataset of Request For job

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/gcompany.i}
{custom/gloc.i}
DEFINE TEMP-TABLE ttjobblogrep NO-UNDO
    FIELD vjobfilename AS CHAR.
DEFINE DATASET dsjobblog FOR ttjobblogrep.
    DEFINE INPUT PARAMETER prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginOrder    AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmEndOrder      AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginItem     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndItem       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginOrderDate AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmEndOrderDate  AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintPart     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSort          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintDueDate  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCustName      AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsjobblog.
    DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.

    IF prmBeginCust = ?  THEN ASSIGN prmBeginCust = "".
    IF prmEndCust = ?    THEN ASSIGN prmEndCust   = "".
    IF prmBeginOrder = ? THEN ASSIGN prmBeginOrder = 0.
    IF prmEndOrder = ?   THEN ASSIGN prmEndOrder = 0.
    IF prmBeginItem = ?  THEN ASSIGN prmBeginItem = "".
    IF prmEndItem = ?    THEN ASSIGN prmEndItem = "".
    
DEFINE VARIABLE prmComp AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
    

    DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U  NO-UNDO.
    DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01  NO-UNDO.
    DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0  NO-UNDO.
    DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
    DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz"  NO-UNDO.
    DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999  NO-UNDO.
    DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999  NO-UNDO.
    DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\Inetpub\wwwroot\joblog.pdf" NO-UNDO.
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99  NO-UNDO.
    DEFINE VARIABLE tb_cust-name AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE tb_due-date AS LOGICAL INITIAL no  NO-UNDO.
    DEFINE VARIABLE tb_prt AS LOGICAL INITIAL no  NO-UNDO.
    DEFINE VARIABLE tb_sort-by-cust AS LOGICAL INITIAL no  NO-UNDO.
    DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.



def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
{custom/xprint.i}
{sys/inc/var.i new shared}          
    assign
    cocode         = prmComp
    locode         = usercomp.loc
    begin_cust-no  = prmBeginCust
    end_cust-no    = prmEndCust 
    begin_i-no     = prmBeginItem
    end_i-no       = prmEndItem
    begin_ord-date = prmBeginOrderDate
    end_ord-date   = prmEndOrderDate
    begin_ord-no   = prmBeginOrder
    end_ord-no     = prmEndOrder
    tb_cust-name   = prmCustName .
    tb_due-date     = IF prmPrintDueDate = "Yes" THEN TRUE ELSE FALSE.
    tb_prt          = IF prmPrintPart = "Yes" THEN TRUE ELSE FALSE.
    tb_sort-by-cust = IF prmSort = "Yes" THEN TRUE ELSE FALSE.

    init-dir    = "C:\Inetpub\wwwroot\pdfs\".
    lv-pdf-file = init-dir + 'JOBBLOG' .
    lv-pdf-file = lv-pdf-file + prmBeginCust.
    
    run run-report.

    RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
    vPdfFile   = 'JOBBLOG' + prmBeginCust + '.pdf'.
    CREATE ttjobblogrep.
    ASSIGN ttjobblogrep.vjobfilename = vPdfFile.   

/***********************************PROCEDURE run-report :***********************************/
    PROCEDURE run-report :
        {sys/form/r-topw.f}
            def var v-fcust as char extent 2 init ["", "zzzzzzzz"].
        def var v-ford-no as int format ">>>>>9" extent 2 init [0, 999999].
        def var v-fdate as date format "99/99/9999" extent 2 init [01/01/0001, today].
        def var v-fitem as char format "x(15)" extent 2 init ["", "zzzzzzzzzzzzzzz"].
        def var v-frst as log init no.
        def var changed as log init no.
        def var job-num as char format "x(9)".
        def var v-i-no like oe-ordl.i-no no-undo.
        def var v-i-name like oe-ordl.i-name no-undo.
        DEF VAR v-cust AS CHAR FORMAT "X(17)" NO-UNDO.
        DEFINE VARIABLE ExcelHeader AS CHARACTER  NO-UNDO.
        
        FORMAT job-num              COLUMN-LABEL "Job!Number"
            job-hdr.ord-no       COLUMN-LABEL "Order!Number"
            v-cust               COLUMN-LABEL "!Customer" FORMAT "X(17)"
            v-i-no               COLUMN-LABEL "!Item Number"
            v-i-name             COLUMN-LABEL "!Item Name"
            oe-ordl.po-no        COLUMN-LABEL "!Cust PO#"
            job-hdr.qty          COLUMN-LABEL "Quantity!Ordered"
            oe-ord.ord-date      COLUMN-LABEL "Order!Date"
            FORMAT "99/99/99"
            oe-ord.due-date      COLUMN-LABEL "Due!Date"
            FORMAT "99/99/99"
            SKIP
            WITH FRAME detail STREAM-IO WIDTH 156 DOWN. /*132*/
        
        FORMAT job-num              COLUMN-LABEL "Job!Number"
            job-hdr.ord-no       COLUMN-LABEL "Order!Number"
            v-cust               COLUMN-LABEL "!Customer" FORMAT "X(17)"
            v-i-no               COLUMN-LABEL "!Item Number"
            v-i-name             COLUMN-LABEL "!Item Name"
            oe-ordl.po-no        COLUMN-LABEL "!Cust PO#"
            job-hdr.qty          COLUMN-LABEL "Quantity!Ordered"
            oe-ord.ord-date      COLUMN-LABEL "Order!Date"
            FORMAT "99/99/99"
            oe-ord.due-date      COLUMN-LABEL "Due!Date"
            FORMAT "99/99/99"
            oe-ordl.part-no      COLUMN-LABEL "!Cust Part#"
            eb.est-no            COLUMN-LABEL "!    Est#"
            eb.die-no            COLUMN-LABEL "!Die#"
            eb.cad-no            COLUMN-LABEL "!Cad#"                         
            SKIP
            WITH FRAME detail-w STREAM-IO WIDTH 204 DOWN. /*180*/
        
        FORMAT job-num              COLUMN-LABEL "Job!Number"
            job-hdr.ord-no       COLUMN-LABEL "Order!Number"
            v-cust               COLUMN-LABEL "!Customer" FORMAT "X(17)"
            v-i-no               COLUMN-LABEL "!Item Number"
            v-i-name             COLUMN-LABEL "!Item Name"
            oe-ordl.po-no        COLUMN-LABEL "!Cust PO#"
            job-hdr.qty          COLUMN-LABEL "Quantity!Ordered"
            oe-ord.ord-date      COLUMN-LABEL "Order!Date"
            FORMAT "99/99/99"
            SKIP
            WITH FRAME detail-y STREAM-IO WIDTH 156 DOWN. /*132*/
        FORMAT job-num              COLUMN-LABEL "Job!Number"
            job-hdr.ord-no       COLUMN-LABEL "Order!Number"
            v-cust               COLUMN-LABEL "!Customer" FORMAT "X(17)"
            v-i-no               COLUMN-LABEL "!Item Number"
            v-i-name             COLUMN-LABEL "!Item Name"
            oe-ordl.po-no        COLUMN-LABEL "!Cust PO#"
            job-hdr.qty          COLUMN-LABEL "Quantity!Ordered"
            oe-ord.ord-date      COLUMN-LABEL "Order!Date"
            FORMAT "99/99/99"
            oe-ordl.part-no      COLUMN-LABEL "!Cust Part#"
            eb.est-no            COLUMN-LABEL "!    Est#"
            eb.die-no            COLUMN-LABEL "!Die#"
            eb.cad-no            COLUMN-LABEL "!Cad#"                         
            SKIP
            WITH FRAME detail-z STREAM-IO WIDTH 204 DOWN. /*180*/
        
        ASSIGN
            str-tit2 =  " Blog Job report"
            {sys/inc/ctrtext.i str-tit2 112}
            
            v-fcust[1]   = begin_cust-no
            v-fcust[2]   = end_cust-no
            v-ford-no[1] = begin_ord-no
            v-ford-no[2] = end_ord-no
            v-fitem[1]   = begin_i-no
            v-fitem[2]   = end_i-no
            v-fdate[1]   = begin_ord-date
            v-fdate[2]   = end_ord-date.
        
        changed = v-fdate[1] ne date(01,01,year(today)) or v-fdate[2] ne today.
        
        {sys/inc/print1.i}
            {sys/inc/outprint.i value(lines-per-page)}
            DISPLAY "" WITH FRAME r-top.
        
        for each job-hdr
            where job-hdr.company eq cocode
            and job-hdr.opened  eq yes
            and job-hdr.ord-no  ge v-ford-no[1]
            and job-hdr.ord-no  le v-ford-no[2]
            and job-hdr.cust-no ge v-fcust[1]
            and job-hdr.cust-no le v-fcust[2]
            and job-hdr.i-no    ge v-fitem[1]
            and job-hdr.i-no    le v-fitem[2]
            use-index opened no-lock,
            
            first job
            where job.company eq cocode
            and job.job     eq job-hdr.job
            and job.job-no  eq job-hdr.job-no
            and job.job-no2 eq job-hdr.job-no2
            no-lock,
            FIRST cust FIELDS(cust-no NAME) WHERE
            cust.company EQ cocode AND
            cust.cust-no EQ job-hdr.cust-no
            NO-LOCK
            break by (if tb_sort-by-cust AND
                     tb_cust-name EQ "#" then job-hdr.cust-no
                  ELSE IF tb_sort-by-cust AND
                       tb_cust-name EQ "name" THEN cust.NAME
                  else "")
              by job-hdr.job-no
              by job-hdr.job-no2:
            
        release oe-ord.
        if job-hdr.ord-no ne 0 then
            find first oe-ord
            where oe-ord.company eq cocode
            and oe-ord.ord-no  eq job-hdr.ord-no
            no-lock no-error.
        if avail oe-ord then
            if oe-ord.ord-date lt v-fdate[1] or
            oe-ord.ord-date gt v-fdate[2] or
             index("CZ",oe-ord.stat) ne 0  then next.
          else.
        else     
          if job.start-date lt v-fdate[1] or
             job.start-date gt v-fdate[2] or
             job.start-date eq ?          then next.

        ASSIGN
          v-i-name = ""
          v-i-no = "".

        RELEASE oe-ordl.

        if avail oe-ord then
          find first oe-ordl
         where oe-ordl.company eq cocode
           and oe-ordl.ord-no  eq job-hdr.ord-no
           and oe-ordl.i-no    eq job-hdr.i-no
          no-lock no-error.

        if avail oe-ordl then 
          v-i-name = oe-ordl.i-name.
        else
        do:
            find first itemfg
                 where itemfg.company eq cocode
                   and itemfg.i-no eq job-hdr.i-no
                no-lock no-error.
            if avail itemfg then 
              v-i-name = itemfg.i-name.
            else 
              v-i-name = "".
        end.

        assign
          v-i-no  = job-hdr.i-no
          job-num = trim(job-hdr.job-no) + "-" + string(job-hdr.job-no2,"99").

        IF tb_cust-name EQ "#" THEN
           v-cust = job-hdr.cust-no.
        ELSE
           v-cust = cust.NAME.

        RELEASE eb.
        IF job.est-no NE "" THEN
        FOR EACH eb
           WHERE eb.company EQ cocode
             AND eb.est-no  EQ job.est-no
            NO-LOCK
           BREAK BY eb.form-no DESC
                BY eb.blank-no DESC:
            IF LAST(eb.form-no) OR
               (eb.form-no EQ job-hdr.frm AND
                eb.blank-no EQ job-hdr.blank-no) THEN LEAVE.
        END.

        IF tb_prt THEN 
        DO:
           IF tb_due-date THEN
           DO:
              display 
                   job-num
                   job-hdr.ord-no
                   v-cust
                   job-hdr.i-no @ v-i-no
                   v-i-name
                   oe-ordl.po-no     when avail oe-ordl
                   job-hdr.qty
                   oe-ord.ord-date   when avail oe-ord
                     job.start       when not avail oe-ord @ oe-ord.ord-date
                   oe-ord.due-date   WHEN tb_due-date AND AVAIL oe-ord
                   job.due-date      WHEN tb_due-date AND NOT AVAIL oe-ord
                                     @ oe-ord.due-date
                   oe-ordl.part-no   when avail oe-ordl
                   eb.est-no         when avail eb
                   eb.die-no         when avail eb
                   eb.cad-no         when avail eb
                 with FRAME detail-w.
              down with FRAME detail-w.
           END.
           ELSE
              display 
                   job-num
                   job-hdr.ord-no
                   v-cust
                   job-hdr.i-no @ v-i-no
                   v-i-name
                   oe-ordl.po-no     when avail oe-ordl
                   job-hdr.qty
                   oe-ord.ord-date   when avail oe-ord
                     job.start       when not avail oe-ord @ oe-ord.ord-date
                   oe-ord.due-date   WHEN tb_due-date AND AVAIL oe-ord
                   oe-ordl.part-no   when avail oe-ordl
                   eb.est-no         when avail eb
                   eb.die-no         when avail eb
                   eb.cad-no         when avail eb
                 with FRAME detail-z.
              down with FRAME detail-z.
                    END.
                    
    end.  /* for each */


end procedure.
 

