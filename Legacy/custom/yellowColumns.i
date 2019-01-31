/* yellowColumns.i */

DEFINE VARIABLE sortBy AS LOGICAL NO-UNDO.
DEFINE VARIABLE sortColumn AS CHARACTER NO-UNDO.
DEFINE VARIABLE colLabels AS CHARACTER NO-UNDO.
DEFINE VARIABLE browserTitle AS CHARACTER NO-UNDO.

FUNCTION comma RETURNS CHARACTER (ipValue AS CHARACTER) :
  RETURN IF ipValue NE '' THEN ',' ELSE ''.
END FUNCTION.

&IF DEFINED(localOpenQuery) EQ 0 &THEN
PROCEDURE openQuery:

&IF DEFINED(yellowColumnsName) EQ 0 &THEN
  MESSAGE 'Scop-Def yellowColumnsName is not Defined' SKIP(1)
    '1. Create Scop-Def yellowColumnsName in Definitions Section.' SKIP
    '2. Modify Procedure "openQuery" in include "custom/yellowColumns.i" accordingly.' VIEW-AS ALERT-BOX.
&ENDIF

/* ****************************************************************** */  
/* custom code placed here for each unique yellowColumnsName Scop-Def */
/* entries should be done alphabetically by yellowColumnsName         */
/* add "&SCOPED-DEFINE localOpenQuery" line to not utilize this proc  */
/* in definitions section                                             */
/* ****************************************************************** */  


&IF '{&yellowColumnsName}' EQ 'account' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Invoice#' THEN account.dscr ELSE ~
  IF sortColumn EQ 'Check#' THEN account.type ELSE ~
  account.actnum ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'b-reconc' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Check/Journal#' THEN tt-number ELSE ~
  IF sortColumn EQ 'Trans Date' THEN STRING(YEAR(tt-date),'9999') + ~
  STRING(MONTH(tt-date),'99') + ~
  STRING(DAY(tt-date),'99') ELSE ~
  IF sortColumn EQ 'Amount' THEN STRING( 1000000 / (9999999.99 - tt-amt),'-9.999999999') ELSE ~
  IF sortColumn EQ 'Bank Code' THEN tt-bank ELSE ~
  IF sortColumn EQ 'Vendor#' THEN tt-vend ELSE ~
  IF sortColumn EQ 'Name' THEN tt-name ELSE ~
  STRING(tt-cleared) ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'w-seldue' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Vendor' THEN tt-sel.vend-no ELSE ~
  IF sortColumn EQ 'Invoice Num' THEN tt-sel.inv-no ELSE ~
  IF sortColumn EQ 'Invoice Date' THEN STRING(YEAR(tt-sel.inv-date),'9999') + ~
  STRING(MONTH(tt-sel.inv-date),'99') + ~
  STRING(DAY(tt-sel.inv-date),'99') ELSE ~
  IF sortColumn EQ 'Due Date' THEN STRING(YEAR(tt-sel.due-date),'9999') + ~
  STRING(MONTH(tt-sel.due-date),'99') + ~
  STRING(DAY(tt-sel.due-date),'99') ELSE ~
  IF sortColumn EQ 'Disc Date' THEN STRING(YEAR(tt-sel.dsc-date),'9999') + ~
  STRING(MONTH(tt-sel.dsc-date),'99') + ~
  STRING(DAY(tt-sel.dsc-date),'99') ELSE ~
  IF sortColumn EQ 'Invoice Amt' THEN STRING( 1000000 / (9999999.99 - tt-sel.inv-bal),'-9.999999999') ELSE ~
  IF sortColumn EQ 'Balance Due' THEN STRING( 1000000 / (9999999.99 - tt-sel.amt-due),'-9.999999999') ELSE ~
  IF sortColumn EQ 'Discount' THEN STRING( 1000000 / (9999999.99 - tt-sel.disc-amt),'-9.999999999') ELSE ~
  STRING( 1000000 / (9999999.99 - tt-sel.amt-paid),'-9.999999999') ~{&SORTED}
  
&ELSEIF '{&yellowColumnsName}' EQ 'account2' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Account No' THEN account.actnum ELSE ~
  IF sortColumn EQ 'Description' THEN account.dscr ELSE ~
  account.TYPE ~{&SORTED}

&ELSEIF  '{&yellowColumnsName}' EQ 'b-apibal' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Invoice#' THEN tt-report.key-02 ELSE ~
  IF sortColumn EQ 'Check#' THEN tt-report.check-no ELSE ~
  IF sortColumn EQ 'Date' THEN STRING(YEAR(tt-report.trans-date),'9999') + ~
  STRING(MONTH(tt-report.trans-date),'99') + ~
  STRING(DAY(tt-report.trans-date),'99') ELSE ~
  IF sortColumn EQ 'Description' THEN tt-report.dscr ELSE ~
  IF sortColumn EQ 'Credits' THEN STRING(tt-report.credits,'-9999999.99') ELSE ~
  IF sortColumn EQ 'Debits' THEN STRING(tt-report.debits,'-9999999.99') ELSE ~
  IF sortColumn EQ 'Balance' THEN STRING(tt-report.balance,'-999999999.99') ELSE ~
  IF sortColumn EQ 'PO#s' THEN tt-report.po-no ELSE ~
  tt-report.key-01 ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'b-apichk' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Chk Date' THEN STRING(YEAR(tt-report.check-date),'9999') + ~
  STRING(MONTH(tt-report.check-date),'99') + ~
  STRING(DAY(tt-report.check-date),'99') ELSE ~
  IF sortColumn EQ 'Invoice#' THEN tt-report.inv-no ELSE ~
  IF sortColumn EQ 'Due Date' THEN STRING(YEAR(tt-report.due-date),'9999') + ~
  STRING(MONTH(tt-report.due-date),'99') + ~
  STRING(DAY(tt-report.due-date),'99') ELSE ~
  IF sortColumn EQ 'Gross Amt' THEN STRING(tt-report.gross-amt,'-99999999.99') ELSE ~
  IF sortColumn EQ 'Discount' THEN STRING(tt-report.amt-disc,'-99999.99') ELSE ~
  IF sortColumn EQ 'Net Amt' THEN STRING(tt-report.amt-paid,'-99999999.99') ELSE ~
  STRING(tt-report.check-no,'99999999') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'b-cusinq' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Ck/Cr/Dr/PO#' THEN tt-arinq.ref-num ELSE ~
  IF sortColumn EQ 'Invoice#' THEN STRING(tt-arinq.inv-no,'999999') ELSE ~
  IF sortColumn EQ 'Date' THEN STRING(tt-arinq.seq,'9999999999') ELSE ~
  IF sortColumn EQ 'Description' THEN tt-arinq.tr-dscr ELSE ~
  IF sortColumn EQ 'Age App' THEN tt-arinq.ageapp ELSE ~
  IF sortColumn EQ 'Appl' THEN tt-arinq.ageapp ELSE ~
  IF sortColumn EQ 'Debits' THEN STRING(tt-arinq.tr-damt,'-9999999.99') ELSE ~
  IF sortColumn EQ 'Credits' THEN STRING(tt-arinq.tr-camt,'-9999999.99') ELSE ~
  IF sortColumn EQ 'Balance' THEN STRING(tt-arinq.balance,'-999999999.99') ELSE ~
  STRING(tt-arinq.applied) ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'b-estop' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'S' THEN STRING(est-op.s-num,'>>>') ELSE ~
  IF sortColumn EQ 'Desc' THEN est-op.m-dscr ELSE ~
  est-op.m-code ~{&SORTED} BY STRING(est-op.s-num,'>>>') BY STRING(est-op.b-num,'>>>')

&ELSEIF '{&yellowColumnsName}' EQ 'b-jobmch' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'S' THEN STRING(job-mch.frm,'>>>') ELSE ~
  job-mch.m-code ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'b-jhdrin' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Blank' THEN STRING(job-hdr.blank-no,'999') ELSE ~
  IF sortColumn EQ 'Customer#' THEN job-hdr.cust-no ELSE ~
  IF sortColumn EQ 'FG Item#' THEN job-hdr.i-no ELSE ~
  IF sortColumn EQ 'Quantity' THEN STRING(job-hdr.qty,'999999') ELSE ~
  IF sortColumn EQ 'SqInch%' THEN STRING(job-hdr.sq-in,'999.99') ELSE ~
  IF sortColumn EQ 'Order#' THEN STRING(job-hdr.ord-no,'999999') ELSE ~
  IF sortColumn EQ "Mat'l" THEN STRING(job-hdr.std-mat-cost,'-99999.99') ELSE ~
  IF sortColumn EQ 'D.L.' THEN STRING(job-hdr.std-lab-cost,'-99999.99') ELSE ~
  IF sortColumn EQ 'Var OH' THEN STRING(job-hdr.std-var-cost,'-99999.99') ELSE ~
  IF sortColumn EQ 'Fixed OH' THEN STRING(job-hdr.std-fix-cost,'-99999.99') ELSE ~
  STRING(job-hdr.frm,'999') ~
  ~{&SORTED} BY STRING(job-hdr.frm,'999') BY STRING(job-hdr.blank-no,'999')

&ELSEIF '{&yellowColumnsName}' EQ 'b-jmatin' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Blank' THEN STRING(tt-mat.blank-no,'999') ELSE ~
  IF sortColumn EQ 'RM Item#' THEN tt-mat.rm-i-no ELSE ~
  IF sortColumn EQ 'Standard Qty' THEN STRING(tt-mat.qty-std,'999999.9999') ELSE ~
  IF sortColumn EQ 'Actual Qty' THEN STRING(tt-mat.qty-act,'999999.9999') ELSE ~
  IF sortColumn EQ 'Qty Variance' THEN STRING(tt-mat.qty-var,'-999999.9999') ELSE ~
  IF sortColumn EQ 'Standard Cost' THEN STRING(tt-mat.cst-std,'9999999.99') ELSE ~
  IF sortColumn EQ 'Actual Cost' THEN STRING(tt-mat.cst-act,'9999999.99') ELSE ~
  IF sortColumn EQ 'Cost Variance' THEN STRING(tt-mat.cst-var,'-9999999.99') ELSE ~
  STRING(tt-mat.form-no,'999') ~
  ~{&SORTED} BY STRING(tt-mat.form-no,'999') BY STRING(tt-mat.blank-no,'999')

&ELSEIF '{&yellowColumnsName}' EQ 'b-jmchci' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Blank' THEN STRING(mch.blank-no,'999') ELSE ~
  IF sortColumn EQ 'P#' THEN STRING(mch.pass,'99') ELSE ~
  IF sortColumn EQ 'Machine' THEN mch.m-code ELSE ~
  IF sortColumn EQ 'FG Item#' THEN mch.i-no ELSE ~
  IF sortColumn EQ 'Std Run Cost' THEN STRING(mch.run-std,'9999999.99') ELSE ~
  IF sortColumn EQ 'Act Run Cost' THEN STRING(mch.run-act,'9999999.99') ELSE ~
  IF sortColumn EQ 'Run Cost Var' THEN STRING(mch.run-var,'-9999999.99') ELSE ~
  IF sortColumn EQ 'Std MR Cost' THEN STRING(mch.mr-std,'9999999.99') ELSE ~
  IF sortColumn EQ 'Act MR Cost' THEN STRING(mch.mr-act,'9999999.99') ELSE ~
  IF sortColumn EQ 'MR Cost Var' THEN STRING(mch.mr-var,'-9999999.99') ELSE ~
  STRING(mch.form-no,'999') ~
  ~{&SORTED} BY STRING(mch.form-no,'999') BY STRING(mch.blank-no,'999') BY STRING(mch.line,'999')

&ELSEIF '{&yellowColumnsName}' EQ 'b-jmchin' &THEN
&SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Blank' THEN STRING(mch.blank-no,'999') ELSE ~
  IF sortColumn EQ 'P#' THEN STRING(mch.pass,'99') ELSE ~
  IF sortColumn EQ 'Machine' THEN mch.m-code ELSE ~
  IF sortColumn EQ 'FG Item#' THEN mch.i-no ELSE ~
  IF sortColumn EQ 'Std Run Hrs' THEN STRING(mch.run-hr,'999999.99') ELSE ~
  IF sortColumn EQ 'Act Run Hrs' THEN STRING(mch.run-act,'999999.99') ELSE ~
  IF sortColumn EQ 'Run Hrs Var' THEN STRING(mch.run-var,'-999999.99') ELSE ~
  IF sortColumn EQ 'Std MR Hrs' THEN STRING(mch.mr-hr,'999999.99') ELSE ~
  IF sortColumn EQ 'Act MR Hrs' THEN STRING(mch.mr-act,'999999.99') ELSE ~
  IF sortColumn EQ 'MR Hrs Var' THEN STRING(mch.mr-var,'-999999.99') ELSE ~
  STRING(mch.form-no,'999') ~
  ~{&SORTED} BY STRING(mch.form-no,'999') BY STRING(mch.blank-no,'999') BY STRING(mch.line,'999')

&ELSEIF '{&yellowColumnsName}' EQ 'b-jmchli' &THEN
&SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Blank' THEN STRING(mch.blank-no,'999') ELSE ~
  IF sortColumn EQ 'P#' THEN STRING(mch.pass,'99') ELSE ~
  IF sortColumn EQ 'Machine' THEN mch.m-code ELSE ~
  IF sortColumn EQ 'FG Item#' THEN mch.i-no ELSE ~
  IF sortColumn EQ 'Std Run D.L.' THEN STRING(mch.run-std,'9999999.99') ELSE ~
  IF sortColumn EQ 'Act Run D.L.' THEN STRING(mch.run-act,'9999999.99') ELSE ~
  IF sortColumn EQ 'Run D.L. Var' THEN STRING(mch.run-var,'-9999999.99') ELSE ~
  IF sortColumn EQ 'Std MR D.L.' THEN STRING(mch.mr-std,'9999999.99') ELSE ~
  IF sortColumn EQ 'Act MR D.L.' THEN STRING(mch.mr-act,'9999999.99') ELSE ~
  IF sortColumn EQ 'MR D.L. Var' THEN STRING(mch.mr-var,'-9999999.99') ELSE ~
  STRING(mch.form-no,'999') ~
  ~{&SORTED} BY STRING(mch.form-no,'999') BY STRING(mch.blank-no,'999') BY STRING(mch.line,'999')

&ELSEIF '{&yellowColumnsName}' EQ 'b-jmchqi' &THEN
&SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Blank' THEN STRING(mch.blank-no,'999') ELSE ~
  IF sortColumn EQ 'P#' THEN STRING(mch.pass,'99') ELSE ~
  IF sortColumn EQ 'Machine' THEN mch.m-code ELSE ~
  IF sortColumn EQ 'FG Item#' THEN mch.i-no ELSE ~
  IF sortColumn EQ 'Std Run Qty' THEN STRING(mch.run-std,'99999999') ELSE ~
  IF sortColumn EQ 'Act Run Qty' THEN STRING(mch.run-act,'99999999') ELSE ~
  IF sortColumn EQ 'Run Qty Var' THEN STRING(mch.run-var,'-99999999') ELSE ~
  IF sortColumn EQ 'Std MR Qty' THEN STRING(mch.mr-std,'99999999') ELSE ~
  IF sortColumn EQ 'Act MR Qty' THEN STRING(mch.mr-act,'99999999') ELSE ~
  IF sortColumn EQ 'MR Qty Var' THEN STRING(mch.mr-var,'-99999999') ELSE ~
  STRING(mch.form-no,'999') ~
  ~{&SORTED} BY STRING(mch.form-no,'999') BY STRING(mch.blank-no,'999') BY STRING(mch.line,'999')

&ELSEIF '{&yellowColumnsName}' EQ 'b-jmchvi' &THEN
&SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Blank' THEN STRING(mch.blank-no,'999') ELSE ~
  IF sortColumn EQ 'P#' THEN STRING(mch.pass,'99') ELSE ~
  IF sortColumn EQ 'Machine' THEN mch.m-code ELSE ~
  IF sortColumn EQ 'FG Item#' THEN mch.i-no ELSE ~
  IF sortColumn EQ 'Std Run VO' THEN STRING(mch.run-std,'9999999.99') ELSE ~
  IF sortColumn EQ 'Act Run VO' THEN STRING(mch.run-act,'9999999.99') ELSE ~
  IF sortColumn EQ 'Run VO Var' THEN STRING(mch.run-var,'-9999999.99') ELSE ~
  IF sortColumn EQ 'Std MR VO' THEN STRING(mch.mr-std,'9999999.99') ELSE ~
  IF sortColumn EQ 'Act MR VO' THEN STRING(mch.mr-act,'9999999.99') ELSE ~
  IF sortColumn EQ 'MR VO Var' THEN STRING(mch.mr-var,'-9999999.99') ELSE ~
  STRING(mch.form-no,'999') ~
  ~{&SORTED} BY STRING(mch.form-no,'999') BY STRING(mch.blank-no,'999') BY STRING(mch.line,'999')

&ELSEIF '{&yellowColumnsName}' EQ 'b-jmchwi' &THEN
&SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Blank' THEN STRING(mch.blank-no,'999') ELSE ~
  IF sortColumn EQ 'P#' THEN STRING(mch.pass,'99') ELSE ~
  IF sortColumn EQ 'Machine' THEN STRING(mch.m-code,'999') ELSE ~
  IF sortColumn EQ 'FG Item#' THEN mch.i-no ELSE ~
  IF sortColumn EQ 'Std Run Waste' THEN STRING(mch.run-std,'99999999') ELSE ~
  IF sortColumn EQ 'Act Run Waste' THEN STRING(mch.run-act,'99999999') ELSE ~
  IF sortColumn EQ 'Run Waste Var' THEN STRING(mch.run-var,'-99999999') ELSE ~
  IF sortColumn EQ 'Std MR Waste' THEN STRING(mch.mr-std,'99999999') ELSE ~
  IF sortColumn EQ 'Act MR Waste' THEN STRING(mch.mr-act,'99999999') ELSE ~
  IF sortColumn EQ 'MR Waste Var' THEN STRING(mch.mr-var,'-99999999') ELSE ~
  STRING(mch.form-no,'999') ~
  ~{&SORTED} BY STRING(mch.form-no,'999') BY STRING(mch.blank-no,'999') BY STRING(mch.line,'999')

&ELSEIF '{&yellowColumnsName}' EQ 'b-jmchfi' &THEN
&SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Blank' THEN STRING(mch.blank-no,'999') ELSE ~
  IF sortColumn EQ 'P#' THEN STRING(mch.pass,'99') ELSE ~
  IF sortColumn EQ 'Machine' THEN mch.m-code ELSE ~
  IF sortColumn EQ 'FG Item#' THEN mch.i-no ELSE ~
  IF sortColumn EQ 'Std Run FO' THEN STRING(mch.run-std,'9999999.99') ELSE ~
  IF sortColumn EQ 'Act Run FO' THEN STRING(mch.run-act,'9999999.99') ELSE ~
  IF sortColumn EQ 'Run FO Var' THEN STRING(mch.run-var,'-9999999.99') ELSE ~
  IF sortColumn EQ 'Std MR FO' THEN STRING(mch.mr-std,'9999999.99') ELSE ~
  IF sortColumn EQ 'Act MR FO' THEN STRING(mch.mr-act,'9999999.99') ELSE ~
  IF sortColumn EQ 'MR FO Var' THEN STRING(mch.mr-var,'-9999999.99') ELSE ~
  STRING(mch.form-no,'999') ~
  ~{&SORTED} BY STRING(mch.form-no,'999') BY STRING(mch.blank-no,'999') BY STRING(mch.line,'999')

&ELSEIF '{&yellowColumnsName}' EQ 'b-ldtag' OR
        '{&yellowColumnsName}' EQ 'b-lgtag' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Warehouse' THEN loadtag.loc ELSE ~
  IF sortColumn EQ 'Bin' THEN loadtag.loc-bin ELSE ~
  IF sortColumn EQ 'Job' THEN STRING(loadtag.job-no,'x(6)') ELSE ~
  IF sortColumn EQ 'PO' THEN STRING(loadtag.po-no,'999999') ELSE ~
  IF sortColumn EQ 'Order' THEN STRING(loadtag.ord-no,'999999') ELSE ~
  IF sortColumn EQ 'Item' THEN loadtag.i-no ELSE ~
  IF sortColumn EQ 'Name' THEN loadtag.i-name ELSE ~
  IF sortColumn EQ 'Vendor Tag#' THEN loadtag.misc-char[1] ELSE ~
   loadtag.tag-no ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'loadtag' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Primary Bin Loc.'  THEN loadtag.loc-bin ELSE ~
  IF sortColumn EQ 'Warehouse'         THEN loadtag.loc     ELSE ~
  IF sortColumn EQ 'Run #'             THEN STRING(loadtag.job-no2,'>9')   ELSE ~
  IF sortColumn EQ 'Job Number'        THEN STRING(loadtag.job-no,'x(6)')  ELSE ~
  IF sortColumn EQ 'Purchase Order#'   THEN STRING(loadtag.po-no,'>>>>>9') ELSE ~
  IF sortColumn EQ 'Order#'            THEN STRING(loadtag.ord-no,'>>>>>9') ELSE ~
  IF sortColumn EQ 'Name'              THEN loadtag.i-name  ELSE ~
  IF sortColumn EQ 'Item'              THEN loadtag.i-no    ELSE ~
   loadtag.tag-no ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'b-wiptag' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'RM Whs' THEN wiptag.rm-whs ELSE ~
  IF sortColumn EQ 'RM Bin' THEN wiptag.rm-bin ELSE ~
  IF sortColumn EQ 'WIP Whs' THEN wiptag.wip-warehouse ELSE ~
  IF sortColumn EQ 'WIP Bin' THEN wiptag.wip-rm-bin ELSE ~
  IF sortColumn EQ 'Job' THEN STRING(wiptag.job-no,'x(6)') ELSE ~
  IF sortColumn EQ 'RM Item#' THEN wiptag.rm-i-no ELSE ~
  IF sortColumn EQ 'RM Name' THEN wiptag.rm-i-name ELSE ~
  IF sortColumn EQ 'FG Item#' THEN wiptag.fg-i-no ELSE ~
  IF sortColumn EQ 'FG Name' THEN wiptag.fg-i-name ELSE ~
  IF sortColumn EQ 'Tag Qty.' THEN STRING(wiptag.pallet-count,"->,>>>,>>9") ELSE ~
  wiptag.tag-no ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'b-oeboll' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'FG Item#' THEN oe-boll.i-no ELSE ~
  IF sortColumn EQ 'Customer PO' THEN oe-boll.po-no ELSE ~
  IF sortColumn EQ 'Tag' THEN oe-boll.tag ELSE ~
  IF sortColumn EQ 'Whse' THEN oe-boll.loc ELSE ~
  IF sortColumn EQ 'Bin' THEN oe-boll.loc-bin ELSE ~
  IF sortColumn EQ 'Job #' THEN oe-boll.job-no ELSE ~
  STRING(oe-boll.ord-no,'999999') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'b-phys' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Count Date' THEN STRING(YEAR(fg-rctd.rct-date),'9999') + ~
  STRING(MONTH(fg-rctd.rct-date),'99') + ~
  STRING(DAY(fg-rctd.rct-date),'99') ELSE ~
  IF sortColumn EQ 'Tag#' THEN fg-rctd.tag ELSE ~
  IF sortColumn EQ 'Job#' THEN fg-rctd.job-no ELSE ~
  IF sortColumn EQ 'Item' THEN fg-rctd.i-no ELSE ~
  IF sortColumn EQ 'Name/Desc' THEN fg-rctd.i-name ELSE ~
  IF sortColumn EQ 'Whse' THEN fg-rctd.loc ELSE ~
  IF sortColumn EQ 'Bin' THEN fg-rctd.loc-bin ELSE ~
  IF sortColumn EQ 'Units' THEN STRING(fg-rctd.cases,'-999999') ELSE ~
  IF sortColumn EQ 'Unit Count' THEN STRING(fg-rctd.qty-case,'999999') ELSE ~
  IF sortColumn EQ 'Unit!Pallet' THEN STRING(fg-rctd.cases-unit,'999') ELSE ~
  IF sortColumn EQ 'Partial' THEN STRING(fg-rctd.partial,'-999999') ELSE ~
  IF sortColumn EQ 'Quantity' THEN STRING(fg-rctd.t-qty,'-999999999.99') ELSE ~
  IF sortColumn EQ 'Created by' THEN reftable.code ELSE ~
  IF sortColumn EQ 'Last Updated by' THEN reftable.code2 ELSE ~
  STRING(fg-rctd.r-no,'99999999') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'b-rcptd' OR
        '{&yellowColumnsName}' EQ 'b-issued' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF CAN-DO('Issue Date,Receipt Date',sortColumn) THEN STRING(YEAR(rm-rctd.rct-date),'9999') + ~
  STRING(MONTH(rm-rctd.rct-date),'99') + ~
  STRING(DAY(rm-rctd.rct-date),'99') ELSE ~
  IF sortColumn EQ 'PO#' THEN rm-rctd.po-no ELSE ~
  IF sortColumn EQ 'Job#' THEN rm-rctd.job-no ELSE ~
  IF sortColumn EQ 'Item' THEN rm-rctd.i-no ELSE ~
  IF sortColumn EQ 'Name/Desc' THEN rm-rctd.i-name ELSE ~
  IF sortColumn EQ 'Whse' THEN rm-rctd.loc ELSE ~
  IF sortColumn EQ 'Bin' THEN rm-rctd.loc-bin ELSE ~
  IF sortColumn EQ 'Tag#' THEN rm-rctd.tag ELSE ~
  IF sortColumn EQ 'Qty' THEN STRING(rm-rctd.qty,'-9999999.999') ELSE ~
  IF sortColumn EQ 'PUOM' THEN rm-rctd.pur-uom ELSE ~
  IF sortColumn EQ 'Cost' THEN STRING(rm-rctd.cost,'-999999.999999') ELSE ~
  IF sortColumn EQ 'CUOM' THEN rm-rctd.cost-uom ELSE ~
  IF sortColumn EQ 'Vendor Tag #' THEN loadtag.misc-char[1] ELSE ~
  STRING(rm-rctd.r-no,'99999999') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'b-rcptd-fg' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Receipt!Date' THEN STRING(YEAR(fg-rctd.rct-date),'9999') + ~
  STRING(MONTH(fg-rctd.rct-date),'99') + ~
  STRING(DAY(fg-rctd.rct-date),'99') ELSE ~
  IF sortColumn EQ 'Tag#' THEN fg-rctd.tag ELSE ~
  IF sortColumn EQ 'PO#' THEN fg-rctd.po-no ELSE ~
  IF sortColumn EQ 'Job#' THEN fg-rctd.job-no ELSE ~
  IF sortColumn EQ 'Item' THEN fg-rctd.i-no ELSE ~
  IF sortColumn EQ 'Name/Desc' THEN fg-rctd.i-name ELSE ~
  IF sortColumn EQ 'Whse' THEN fg-rctd.loc ELSE ~
  IF sortColumn EQ 'Bin' THEN fg-rctd.loc-bin ELSE ~
  IF sortColumn EQ 'Units' THEN STRING(fg-rctd.cases,'-999999') ELSE ~
  IF sortColumn EQ 'Unit!Count' THEN STRING(fg-rctd.qty-case,'999999') ELSE ~
  IF sortColumn EQ 'Unit!per Pallet' THEN STRING(fg-rctd.cases-unit,'999') ELSE ~
  IF sortColumn EQ 'Partial' THEN STRING(fg-rctd.partial,'-999999') ELSE ~
  IF sortColumn EQ 'Cost/UOM' THEN STRING(fg-rctd.std-cost,'-9999999.999999') ELSE ~
  IF sortColumn EQ 'UOM' THEN fg-rctd.cost-uom ELSE ~
  IF sortColumn EQ 'Total!Qty' THEN STRING(fg-rctd.t-qty,'-999999999.99') ELSE ~
  IF sortColumn EQ 'Freight Cost' THEN STRING(fg-rctd.frt-cost,'999999.9999') ELSE ~
  IF sortColumn EQ 'Extended Cost' THEN STRING(fg-rctd.ext-cost,'-9999999.99') ELSE ~
  IF sortColumn EQ 'FG Lot#' THEN fg-rctd.stack-code ELSE ~
  IF sortColumn EQ 'Created by' THEN reftable.code ELSE ~
  IF sortColumn EQ 'Last Updated by' THEN reftable.code2 ELSE ~
  STRING(fg-rctd.r-no,'99999999') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'b-trans' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Transfer Date' THEN STRING(YEAR(fg-rctd.rct-date),'9999') + ~
  STRING(MONTH(fg-rctd.rct-date),'99') + ~
  STRING(DAY(fg-rctd.rct-date),'99') ELSE ~
  IF sortColumn EQ 'From!Tag' THEN fg-rctd.tag ELSE ~
  IF sortColumn EQ 'To!Tag' THEN fg-rctd.tag2 ELSE ~
  IF sortColumn EQ 'Job#' THEN fg-rctd.job-no ELSE ~
  IF sortColumn EQ 'Item' THEN fg-rctd.i-no ELSE ~
  IF sortColumn EQ 'Name/Desc' THEN fg-rctd.i-name ELSE ~
  IF sortColumn EQ 'From!Whse' THEN fg-rctd.loc ELSE ~
  IF sortColumn EQ 'To!Whse' THEN fg-rctd.loc2 ELSE ~
  IF sortColumn EQ 'From!Bin' THEN fg-rctd.loc-bin ELSE ~
  IF sortColumn EQ 'To!Bin' THEN fg-rctd.loc-bin2 ELSE ~
  IF sortColumn EQ 'Units' THEN STRING(fg-rctd.cases,'999999') ELSE ~
  IF sortColumn EQ 'Qty/Unit' THEN STRING(fg-rctd.qty-case,'999999') ELSE ~
  IF sortColumn EQ 'Unit!Count' THEN STRING(fg-rctd.qty-case,'999999') ELSE ~
  IF sortColumn EQ 'Partial' THEN STRING(fg-rctd.partial,'-999999') ELSE ~
  IF sortColumn EQ 'Created by' THEN fg-rctd.created-by ELSE ~
  IF sortColumn EQ 'Last Updated by' THEN fg-rctd.updated-by ELSE ~
  STRING(fg-rctd.r-no,'99999999') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'cust' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Name' THEN cust.name ELSE ~
  IF sortColumn EQ 'City' THEN cust.city ELSE ~
  IF sortColumn EQ 'State' THEN cust.state ELSE ~
  IF sortColumn EQ 'Zip' THEN cust.zip ELSE ~
  IF sortColumn EQ 'Type' THEN cust.type ELSE ~
  IF sortColumn EQ 'SalesRep' THEN cust.sman ELSE ~
  IF sortColumn EQ 'Territory' THEN cust.terr ELSE ~
  cust.cust-no ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'l-rmibn2' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Warehouse' THEN rm-bin.loc ELSE ~
  IF sortColumn EQ 'Bin' THEN rm-bin.loc-bin ELSE ~
  IF sortColumn EQ 'Tag#' THEN rm-bin.tag ELSE ~
  string(rm-bin.qty,'->>>,>>9.9<<') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'd-invprc' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'FG Name' THEN ar-invl.i-name ELSE ~
  IF sortColumn EQ 'Qty Invoiced' THEN STRING(ar-invl.inv-qty,'-99999999.99') ELSE ~
  IF sortColumn EQ 'Last Price' THEN STRING(ar-invl.unit-pr,'99999999.999999') ELSE ~
  IF sortColumn EQ 'UOM' THEN ar-invl.pr-qty-uom ELSE ~
  ar-invl.i-no ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'item' OR
        '{&yellowColumnsName}' EQ 'b-item' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Name' THEN item.i-name ELSE ~
  IF sortColumn EQ 'Matl!Type' THEN item.mat-type ELSE ~
  IF sortColumn EQ 'Catgy' THEN item.procat ELSE ~
  IF sortColumn EQ 'Description' THEN item.i-dscr ELSE ~
  IF sortColumn EQ 'CType' THEN item.cost-type ELSE ~
  IF sortColumn EQ 'Qty On Hand' THEN string(item.q-onh,'->>>,>>>,>>9.99') ELSE ~
  IF sortColumn EQ 'Qty On Order' THEN string(item.q-ono,'->>>,>>>,>>9.99') ELSE ~
  IF sortColumn EQ 'Qty Committed' THEN string(item.q-comm,'->>>,>>>,>>9.99') ELSE ~
  IF sortColumn EQ 'C-UOM' THEN string(item.cons-uom,'x(3)') ELSE ~
  IF sortColumn EQ 'Roll Width' THEN string(item.r-wid,'>>,>>9.99') ELSE ~
  IF sortColumn EQ 'Width' THEN string(item.s-wid,'>>,>>9.99') ELSE ~
  IF sortColumn EQ 'Depth' THEN string(item.s-dep,'>>,>>9.99') ELSE ~
  IF sortColumn EQ 'Length' THEN string(item.s-len,'>>,>>9.99') ELSE ~
  item.i-no ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'itemfg' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Name' THEN itemfg.i-name ELSE ~
  IF sortColumn EQ 'Description' THEN itemfg.Part-dscr1 ELSE ~
  IF sortColumn EQ 'Cust. #' THEN itemfg.Cust-no ELSE ~
  IF sortColumn EQ 'style' THEN itemfg.style ELSE ~
  IF sortColumn EQ 'category' THEN itemfg.procat ELSE ~
  IF sortColumn EQ 'Part' THEN itemfg.part-no ELSE ~
  IF sortColumn EQ 'stock/!Custom' THEN itemfg.i-code ELSE ~
  IF sortColumn EQ 'estimate #' THEN itemfg.est-no ELSE ~
  IF sortColumn EQ 'cad #' THEN itemfg.cad-no ELSE ~
  IF sortColumn EQ 'Quality/SPC #' THEN itemfg.spc-no ELSE ~
  IF sortColumn EQ 'Stocked?' THEN STRING(itemfg.stocked,'Y/N') ELSE ~
  IF sortColumn EQ 'Qty On-hand' THEN STRING(itemfg.q-onh,'-99999999.999') ELSE ~
  itemfg.i-no ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'item-pos' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Due Date' THEN STRING(YEAR(po-ordl.due-date),'9999') + ~
  STRING(MONTH(po-ordl.due-date),'99') + ~
  STRING(DAY(po-ordl.due-date),'99') ELSE ~
  IF sortColumn EQ 'Ship To' THEN po-ord.ship-id ELSE ~
  STRING(po-ordl.po-no,'>>>>>9') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'l-jobno' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Item No' THEN  job-hdr.i-no ELSE ~
  IF sortColumn EQ 'Estimate #' THEN job-hdr.est-no ELSE ~
  IF sortColumn EQ 'Order#' THEN STRING(job-hdr.ord-no,'999999') ELSE ~
  IF sortColumn EQ 'Cust. #' THEN job-hdr.cust-no ELSE ~
  job-hdr.job-no ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'l-pofg' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Item#' THEN  po-ordl.i-no ELSE ~
  IF sortColumn EQ 'PO Qty' THEN  string(po-ordl.ord-qty,'->>>>>>9.9<<') ELSE ~
  IF sortColumn EQ 'Name' THEN po-ordl.i-name ELSE ~
  IF sortColumn EQ 'Job #' THEN po-ordl.job-no ELSE ~
  IF sortColumn EQ 'Vendor Item #' THEN po-ordl.vend-i-no ELSE ~
  IF sortColumn EQ 'Vendor' THEN po-ord.vend-no ELSE ~
  IF sortColumn EQ 'Sheet!Wid' THEN STRING(po-ordl.s-wid,'999.9999') ELSE ~
  IF sortColumn EQ 'Sheet!len' THEN STRING(po-ordl.s-len,'999.9999') ELSE ~
  STRING(po-ordl.po-no,'999999') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'l-poordl' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Item#' THEN  po-ordl.i-no ELSE ~
  IF sortColumn EQ 'PO Qty' THEN  string(po-ordl.ord-qty,'->>>>>>9.9<<') ELSE ~
  IF sortColumn EQ 'Name' THEN po-ordl.i-name ELSE ~
  IF sortColumn EQ 'Job Number' THEN po-ordl.job-no ELSE ~
  IF sortColumn EQ 'Vendor' THEN po-ord.vend-no ELSE ~
  IF sortColumn EQ 'Sheet Wid' THEN STRING(po-ordl.s-wid,'999.9999') ELSE ~
  IF sortColumn EQ 'Sheet len' THEN STRING(po-ordl.s-len,'99999.9999') ELSE ~
  STRING(po-ordl.po-no,'999999') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'bi-poord' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'RM/FG Item#' THEN  po-ordl.i-no ELSE ~
  IF sortColumn EQ 'Line' THEN  STRING(po-ordl.LINE) ELSE ~
  IF sortColumn EQ 'PO Qty' THEN  string(po-ordl.ord-qty,'->>>>>>9.9<<') ELSE ~
  IF sortColumn EQ 'RM/FG Item Name' THEN po-ordl.i-name ELSE ~
  IF sortColumn EQ 'Job Number' THEN po-ordl.job-no ELSE ~
  IF sortColumn EQ 'Vendor' THEN po-ord.vend-no ELSE ~
  IF sortColumn EQ 'Sheet Wid' THEN STRING(po-ordl.s-wid,'999.9999') ELSE ~
  IF sortColumn EQ 'Sheet len' THEN STRING(po-ordl.s-len,'99999.9999') ELSE ~
  STRING(po-ordl.po-no,'999999') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'oe-prmtx' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Type' THEN oe-prmtx.custype ELSE ~
  IF sortColumn EQ 'Item No' THEN oe-prmtx.i-no ELSE ~
  IF sortColumn EQ 'Category' THEN oe-prmtx.procat ELSE ~
  oe-prmtx.cust-no ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'poprint' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'PO Date' THEN STRING(YEAR(po-ord.po-date),'9999') + ~
  STRING(MONTH(po-ord.po-date),'99') + ~
  STRING(DAY(po-ord.po-date),'99') ELSE ~
  IF sortColumn EQ 'Vendor' THEN po-ord.vend-no ELSE ~
  IF sortColumn EQ 'Printed?' THEN STRING(po-ord.printed,'Y/N') ELSE ~
  IF sortColumn EQ 'Status' THEN po-ord.stat ELSE ~
  STRING(po-ord.po-no,'>>>>>9') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'rm-ibin' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Vendor PO#' THEN STRING(INT(rm-bin.po-no),"9999999999") ELSE ~
  IF sortColumn EQ 'Bin Loc' THEN rm-bin.loc-bin ELSE ~
  IF sortColumn EQ 'Tag #' THEN rm-bin.tag ELSE ~
  IF sortColumn EQ 'Quantity' THEN STRING(rm-bin.qty,'-999999.999') ELSE ~
  rm-bin.loc ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'shipto' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Name' THEN shipto.ship-name ELSE ~
  IF sortColumn EQ 'City' THEN shipto.ship-city ELSE ~
  IF sortColumn EQ 'State' THEN shipto.ship-state ELSE ~
  IF sortColumn EQ 'Zip' THEN shipto.ship-zip ELSE ~
  STRING(shipto.ship-no,">>9") ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'vend' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Vendor' THEN vend.vend-no ELSE ~
  IF sortColumn EQ 'Name' THEN vend.name ELSE ~
  IF sortColumn EQ 'Type' THEN vend.type ELSE ~
  IF sortColumn EQ 'Status' THEN vend.active ELSE ~
  IF sortColumn EQ 'Area Code' THEN vend.area-code ELSE ~
  IF sortColumn EQ 'Phone #' THEN vend.phone ELSE ~
  IF sortColumn EQ 'Fax Area Code' THEN vend.fax-area ELSE ~
  IF sortColumn EQ 'Fax #' THEN vend.fax ELSE ~
  vend.buyer ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'inv-head' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Customer #' THEN inv-head.cust-no ELSE ~
  IF sortColumn EQ 'Customer Name' THEN inv-head.cust-name ELSE ~
  IF sortColumn EQ 'Invoice Date' THEN STRING(YEAR(inv-head.inv-date),'9999') + ~
     STRING(MONTH(inv-head.inv-date),'99') + ~
     STRING(DAY(inv-head.inv-date),'99') ELSE ~
  IF sortColumn EQ 'BOL #' THEN STRING(inv-head.bol-no,'>>>>>>>9') ELSE ~
  IF sortColumn EQ 'Printed?' THEN STRING(inv-head.printed,'Y/N') ELSE ~
  IF sortColumn EQ 'Status' THEN ls-status ELSE ~
  IF sortColumn EQ 'Order#' THEN string(f-ordno()) ELSE ~
  IF sortColumn EQ 'Invoiced Total' THEN STRING(inv-head.t-inv-rev,'->>,>>>,>>9.99') ELSE ~
  IF sortColumn EQ 'Unique Internal Number' THEN STRING(inv-head.r-no,'>>>>>>>9') ELSE ~
  STRING(inv-head.inv-no,'>>>>>9') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'truck-run' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Carrier' THEN tt-report.carrier ELSE ~
  IF sortColumn EQ 'Trailer#' THEN tt-report.truck-code ELSE ~
  IF sortColumn EQ 'Descrp' THEN tt-report.truck-dscr ELSE ~
  IF sortColumn EQ 'Stop' THEN STRING(tt-report.stop-no,"ZZZ9") ELSE ~
  IF sortColumn EQ 'Load #' THEN tt-report.load-no ELSE ~
  IF sortColumn EQ 'Ship Date' THEN STRING(YEAR(tt-report.ship-date),'9999') + ~
     STRING(MONTH(tt-report.ship-date),'99') + ~
     STRING(DAY(tt-report.ship-date),'99') ELSE ~
  IF sortColumn EQ 'Cust Name' THEN tt-report.cust-name ELSE ~
  IF sortColumn EQ 'Cust #' THEN tt-report.cust-no ELSE ~
  IF sortColumn EQ 'Order #' THEN STRING(tt-report.order-no) ELSE ~
  IF sortColumn EQ 'FG Item #' THEN tt-report.item-no ELSE ~
  IF sortColumn EQ 'Rel #' THEN STRING(tt-report.rel-no) ELSE ~
  IF sortColumn EQ 'BOL #' THEN STRING(tt-report.bol-no) ELSE ~
  IF sortColumn EQ 'City' THEN tt-report.city ELSE ~
  IF sortColumn EQ 'St' THEN tt-report.state ELSE ~
  IF sortColumn EQ 'Zip' THEN tt-report.zip ELSE ~
  IF sortColumn EQ 'Ship To' THEN tt-report.ship-no ELSE ~
  IF sortColumn EQ 'Zone' THEN tt-report.deliv-zone ELSE ~
  IF sortColumn EQ 'Units' THEN STRING(tt-report.no-units,"->>>>>>>>9") ELSE ~
  tt-report.release-type ~{&SORTED} BY tt-report.carrier BY tt-report.truck-code BY STRING(YEAR(tt-report.ship-date),'9999') + ~
  STRING(MONTH(tt-report.ship-date),'99') + ~
  STRING(DAY(tt-report.ship-date),'99') BY tt-report.load-no BY tt-report.stop-no

&ELSEIF '{&yellowColumnsName}' EQ 'b-hrms-x' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Vendor RM Item#' THEN reftable.CODE ELSE ~
  reftable.code2 ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'probeit' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  probeit.part-no ~{&SORTED}

/* rtc */
&ELSEIF '{&yellowColumnsName}' EQ 'fgcat' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Category' THEN fgcat.procat ELSE ~
  IF sortColumn EQ 'Description' THEN fgcat.dscr ELSE ~
  IF sortColumn EQ 'GL Account' THEN fgcat.glacc ELSE ~
  STRING(fgcat.commrate,">>9.99%") ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'surcharge' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Charge' THEN surcharge.charge ELSE ~
  IF sortColumn EQ 'Description' THEN surcharge.dscr ELSE ~
  IF sortColumn EQ 'Customer' THEN surcharge.cust-no ELSE ~
  IF sortColumn EQ 'Amount' THEN STRING(surcharge.amt,"->>>,>>9.99") ELSE ~
  surcharge.calc-on ~{&SORTED}

/*rb 9/17/08 - test */

&ELSEIF '{&yellowColumnsName}' EQ 'procat' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Category' THEN procat.procat ELSE ~
  procat.dscr ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'oe-hist' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'S'    THEN tt-ordl.IS-SELECTED ELSE ~
    IF sortColumn EQ 'Order#'         THEN oe-ordl.ord-no      ELSE ~
    IF sortColumn EQ 'Quantity'          THEN tt-ordl.e-qty       ELSE ~
    IF sortColumn EQ 'Item#'          THEN oe-ordl.i-no       ELSE ~
    IF sortColumn EQ 'Item Description'          THEN oe-ordl.part-dscr1       ELSE ~
        oe-ordl.i-no  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'oe-ordl' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
  IF sortColumn EQ 'Line' THEN STRING(oe-ordl.line,">>9") ELSE ~
  IF sortColumn EQ 'Item#' THEN oe-ordl.i-no ELSE ~
  IF sortColumn EQ 'Quantity' THEN STRING(oe-ordl.qty,"->>,>>>,>>9.9<<") ELSE ~
  IF sortColumn EQ 'Name' THEN oe-ordl.i-name ELSE ~
  IF sortColumn EQ 'Part #' THEN oe-ordl.part-no ELSE ~
  IF sortColumn EQ 'Sell Price' THEN STRING(oe-ordl.price,"->>,>>>,>>9.99<<<<") ELSE ~
  IF sortColumn EQ 'UOM' THEN oe-ordl.pr-uom ELSE ~
  IF sortColumn EQ 'Tax' THEN STRING(oe-ordl.tax,'Y/N') ELSE ~
  IF sortColumn EQ 'Cust PO#' THEN oe-ordl.po-no ELSE ~
  IF sortColumn EQ 'Estimate #' THEN oe-ordl.est-no ELSE ~
  IF sortColumn EQ 'Due Date' THEN STRING(YEAR(oe-ordl.req-date),'9999') + ~
     STRING(MONTH(oe-ordl.req-date),'99') + ~
     STRING(DAY(oe-ordl.req-date),'99') ELSE ~
  IF sortColumn EQ 'Job Number' THEN STRING(oe-ordl.job-no,'x(6)') ELSE ~
  IF sortColumn EQ 'Vendor' THEN oe-ordl.vend-no ELSE ~
  IF sortColumn EQ 'Discount' THEN STRING(oe-ordl.disc,"(>>>,>>9.99)") ELSE ~
  STRING(oe-ordl.t-price,"->>>,>>9.99") ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'fgbin4' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
   IF sortColumn EQ 'Job#' THEN fg-bin.job-no ELSE ~
   IF sortColumn EQ 'Whs' THEN fg-bin.loc ELSE ~
   IF sortColumn EQ 'Bin' THEN fg-bin.loc-bin ELSE ~
   IF sortColumn EQ 'Tag#' THEN fg-bin.tag ELSE ~
   IF sortColumn EQ 'Customer#' THEN fg-bin.cust-no ELSE ~
   STRING(fg-bin.qty,'-9999999.999') ~{&SORTED}

/* gdm - 0915080*/
&ELSEIF '{&yellowColumnsName}' EQ 'sys-ctrl' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
   IF sortColumn EQ 'Name'		      THEN asi.sys-ctrl.name     ELSE ~
   IF sortColumn EQ 'Description'	  THEN asi.sys-ctrl.descrip  ELSE ~
   IF sortColumn EQ 'Sys!Mod'   THEN asi.sys-ctrl.module   ELSE ~
   IF sortColumn EQ 'Int!Val'   THEN STRING(asi.sys-ctrl.int-fld,'->,>>>,>>9')  ELSE ~
   IF sortColumn EQ 'Dec!val'   THEN STRING(asi.sys-ctrl.dec-fld,'->>,>>9.99')  ELSE ~
   IF sortColumn EQ 'Character!Value' THEN asi.sys-ctrl.char-fld ELSE ~
   IF sortColumn EQ 'Date!Value'	  THEN STRING(YEAR(asi.sys-ctrl.date-fld),'9999') + ~
      STRING(MONTH(asi.sys-ctrl.date-fld),'99') + ~
      STRING(DAY(asi.sys-ctrl.date-fld),'99') ELSE ~
   STRING(asi.sys-ctrl.log-fld)  ~{&SORTED}

/*task# 12271301*/
&ELSEIF '{&yellowColumnsName}' EQ 'sys-ctrl-shipto' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
   IF sortColumn EQ 'Type'		      THEN STRING(asi.sys-ctrl-shipto.cust-vend)     ELSE ~
   IF sortColumn EQ 'Cust/Vend'	  THEN STRING(asi.sys-ctrl-shipto.cust-vend-no)  ELSE ~
   IF sortColumn EQ 'ShipTo'   THEN STRING(asi.sys-ctrl-shipto.ship-id)   ELSE ~
   IF sortColumn EQ 'Character'   THEN STRING(asi.sys-ctrl-shipto.char-fld)  ELSE ~
   IF sortColumn EQ 'Decimal'   THEN STRING(asi.sys-ctrl-shipto.dec-fld,'->>,>>9.99')  ELSE ~
   IF sortColumn EQ 'Integer' THEN string(asi.sys-ctrl-shipto.int-fld,'->,>>>,>>9') ELSE ~
   IF sortColumn EQ 'Date'	  THEN STRING(YEAR(asi.sys-ctrl-shipto.date-fld),'9999') + ~
      STRING(MONTH(asi.sys-ctrl-shipto.date-fld),'99') + ~
      STRING(DAY(asi.sys-ctrl-shipto.date-fld),'99') ELSE ~
   STRING(asi.sys-ctrl-shipto.log-fld)  ~{&SORTED}

/* gdm - 09150806 */
&ELSEIF '{&yellowColumnsName}' EQ 'sman' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
   IF sortColumn EQ 'Sales Rep' 	      THEN asi.sman.sman   	  ELSE ~
   IF sortColumn EQ 'Sales Rep Name'   THEN asi.sman.sname  	  ELSE ~
   IF sortColumn EQ 'Sales Territory' THEN asi.sman.territory ELSE ~
   IF sortColumn EQ 'Net%'		      THEN STRING(asi.sman.netpct,'>>9.99') ELSE ~
       STRING(asi.sman.scomm,'>>9.99') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'smanmtrx' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
   IF sortColumn EQ 'Category'    THEN smanmtrx.procat ELSE ~
   IF sortColumn EQ 'Basis'       THEN smanmtrx.commbasis  ELSE ~
   IF sortColumn EQ 'Margin%'     THEN STRING(smanmtrx.netpct,'>>9.99') ELSE ~
   STRING(smanmtrx.comm,'>>9.99') ~{&SORTED}

/* gdm - 09160802 */
&ELSEIF '{&yellowColumnsName}' EQ 'e-itemfg' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
   IF sortColumn EQ 'FG Item#'       THEN e-itemfg.i-no ELSE ~
   IF sortColumn EQ 'Vendor'         THEN e-itemfg-vend.vend-no ELSE ~
   IF sortColumn EQ 'Cust. #'        THEN e-itemfg-vend.cust-no ELSE ~
       STRING(e-itemfg-vend.setups[1],'->>,>>9.99') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'vend-whse-trans' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
   IF sortColumn EQ 'Seq#'                THEN STRING(vend-whse-trans.r-no, ">>>>>>>9") ELSE ~
   IF sortColumn EQ 'Customers!Usage Date' THEN STRING(YEAR(vend-whse-trans.trans-date),'9999') + ~
    STRING(MONTH(vend-whse-trans.trans-date),'99') + ~
    STRING(DAY(vend-whse-trans.trans-date),'99') ELSE ~
   IF sortColumn EQ 'Customers!Receipt Date' THEN STRING(YEAR(vend-whse-trans.trans-date),'9999') + ~
    STRING(MONTH(vend-whse-trans.trans-date),'99') + ~
    STRING(DAY(vend-whse-trans.trans-date),'99') ELSE ~
   IF sortColumn EQ 'Suppliers!BOL No'    THEN STRING(vend-whse-trans.vend-bol-no, ">>>>>>>9") ELSE ~
   IF sortColumn EQ 'Customers!A/P Code'  THEN vend-whse-trans.vendor-code ELSE ~
   IF sortColumn EQ 'Customers!Plant ID'  THEN vend-whse-trans.vendor-plant-code ELSE ~
   IF sortColumn EQ 'Customers!Dept Code' THEN vend-whse-trans.vendor-dept-code ELSE ~
   IF sortColumn EQ 'Customers!PO#'       THEN vend-whse-trans.item-po-no ELSE ~
   IF sortColumn EQ 'Suppliers!FG Item'   THEN vend-whse-trans.fg-item-no ELSE ~
   IF sortColumn EQ 'Customers!Part#'     THEN vend-whse-trans.cust-part-no ELSE ~
   IF sortColumn EQ 'Suppliers!Order#'    THEN STRING(vend-whse-trans.vend-ord-no, ">>>>>9") ELSE ~
   IF sortColumn EQ 'Suppliers!Job#'      THEN STRING(vend-whse-trans.vend-job-no,'x(6)') ELSE ~
   IF sortColumn EQ 'Suppliers Items!Sell Price' THEN STRING(vend-whse-trans.sell-price, ">,>>>,>>9.99<<<<") ELSE ~
   IF sortColumn EQ 'Quantity!Used'       THEN STRING(vend-whse-trans.trans-qty, "->>,>>>,>>9.9<<") ELSE ~
   IF sortColumn EQ 'Receipt!Quantity'    THEN STRING(vend-whse-trans.trans-qty, "->>,>>>,>>9.9<<") ELSE ~
       STRING(vend-whse-trans.plant-tot-oh-qty,'->>,>>9.99') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'vend-whse-trans-hist' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
   IF sortColumn EQ 'Seq#'                THEN STRING(vend-whse-trans-hist.r-no, ">>>>>>>9") ELSE ~
   IF sortColumn EQ 'Trans!Date' THEN STRING(YEAR(vend-whse-trans-hist.trans-date),'9999') + ~
    STRING(MONTH(vend-whse-trans-hist.trans-date),'99') + ~
    STRING(DAY(vend-whse-trans-hist.trans-date),'99') ELSE ~
   IF sortColumn EQ 'Customers!A/P Code'  THEN vend-whse-trans-hist.vendor-code ELSE ~
   IF sortColumn EQ 'Customers!Plant ID'  THEN vend-whse-trans-hist.vendor-plant-code ELSE ~
   IF sortColumn EQ 'Customers!Dept Code' THEN vend-whse-trans-hist.vendor-dept-code ELSE ~
   IF sortColumn EQ 'Customers!PO#'       THEN vend-whse-trans-hist.item-po-no ELSE ~
   IF sortColumn EQ 'Suppliers!FG Item'   THEN vend-whse-trans-hist.fg-item-no ELSE ~
   IF sortColumn EQ 'Customers!Part#'     THEN vend-whse-trans-hist.cust-part-no ELSE ~
   IF sortColumn EQ 'Suppliers!Order#'    THEN STRING(vend-whse-trans-hist.vend-ord-no, ">>>>>9") ELSE ~
   IF sortColumn EQ 'Suppliers!Job#'      THEN STRING(vend-whse-trans-hist.vend-job-no,'x(6)') ELSE ~
   IF sortColumn EQ 'Suppliers Items!Sell Price' THEN STRING(vend-whse-trans-hist.sell-price, ">,>>>,>>9.99<<<<") ELSE ~
   IF sortColumn EQ 'Quantity!Used'       THEN STRING(vend-whse-trans-hist.trans-qty, "->>,>>>,>>9.9<<") ELSE ~
   IF sortColumn EQ 'Trans!Quantity'    THEN STRING(vend-whse-trans-hist.trans-qty, "->>,>>>,>>9.9<<") ELSE ~
       STRING(vend-whse-trans-hist.plant-tot-oh-qty,'->>,>>9.99') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'vend-whse-item' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
   IF sortColumn EQ 'Suppliers!A/R Code'      THEN vend-whse-item.vendor-code ELSE ~
   IF sortColumn EQ 'Customers!Plant ID'      THEN vend-whse-item.vendor-plant-code ELSE ~
   IF sortColumn EQ 'Customers!A/P Code'      THEN vend-whse-item.cust-no ELSE ~
   IF sortColumn EQ 'Suppliers!FG Item'       THEN vend-whse-item.fg-item-no ELSE ~
   IF sortColumn EQ 'Revision'                THEN vend-whse-item.revision ELSE ~
   IF sortColumn EQ 'Customers!Dept Code'     THEN vend-whse-item.vendor-dept-code ELSE ~
   IF sortColumn EQ 'Obsoloete'               THEN STRING(vend-whse-item.obsolete, 'Y/N') ELSE ~
   IF sortColumn EQ 'Obsolete!Date'           THEN STRING(YEAR(vend-whse-item.obsolete-date),'9999') + ~
                                                   STRING(MONTH(vend-whse-item.obsolete-date),'99') + ~
                                                   STRING(DAY(vend-whse-item.obsolete-date),'99') ELSE ~
      vend-whse-item.cust-part-no ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'vend-plant' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
   IF sortColumn EQ 'Suppliers!A/R Code'      THEN vend-plant.cust-no ELSE ~
   IF sortColumn EQ 'Suppliers!Ship ID'       THEN vend-plant.ship-id ELSE ~
   IF sortColumn EQ 'Customers!A/P Code'      THEN vend-plant.vendor-code ELSE ~
   IF sortColumn EQ 'Customers!Plant ID'      THEN vend-plant.plant-id ELSE ~
   IF sortColumn EQ 'Customers!Dept Code'     THEN vend-plant.vendor-dept-code ELSE ~
      vend-plant.plant-name ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'vend-code-cust-xref' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
   IF sortColumn EQ 'Suppliers!A/R Code'  THEN vend-code-cust-xref.cust-no ELSE ~
   IF sortColumn EQ 'Customers!A/P Code'  THEN vend-code-cust-xref.vendor-code ELSE ~
      vend-code-cust-xref.cust-name ~{&SORTED}
           
&ELSEIF '{&yellowColumnsName}' EQ 'item-comm' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
   IF sortColumn EQ 'Cust. #' THEN item-comm.cust-no ELSE ~
   IF sortColumn EQ 'Item No' THEN item-comm.i-no ELSE ~
      item-comm.part-no ~{&SORTED}

/* gdm - 11210805 */
&ELSEIF '{&yellowColumnsName}' EQ 'machtran' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Machine'    THEN machtran.machine               ELSE ~
    IF sortColumn EQ 'Job'        THEN machtran.job_number            ELSE ~
    IF sortColumn EQ 'Sub'        THEN STRING(machtran.job_sub,'>9')        ELSE ~
    IF sortColumn EQ 'Form'       THEN STRING(machtran.form_number,'>>9')   ELSE ~
    IF sortColumn EQ 'Blk'        THEN STRING(machtran.blank_number,'>9')   ELSE ~
    IF sortColumn EQ 'Pass'       THEN STRING(machtran.pass_sequence,'>>9') ELSE ~
    IF sortColumn EQ 'Charge'     THEN machtran.charge_code           ELSE ~
    IF sortColumn EQ 'Start Date' THEN STRING(YEAR(machtran.start_date),'9999') + ~
       STRING(MONTH(machtran.start_date),'99') + ~
       STRING(DAY(machtran.start_date),'99')                          ELSE ~
    IF sortColumn EQ 'Log In'     THEN STRING(machtran.start_time)    ELSE ~
    IF sortColumn EQ 'End Date'   THEN STRING(YEAR(machtran.end_date),'9999') + ~
       STRING(MONTH(machtran.end_date),'99') + ~
       STRING(DAY(machtran.end_date),'99')                            ELSE ~
    IF sortColumn EQ 'Shift'      THEN machtran.shift                 ELSE ~
    IF sortColumn EQ 'Log Out'    THEN STRING(machtran.end_time)      ELSE ~
    IF sortColumn EQ 'Run'        THEN STRING(machtran.run_qty)       ELSE ~
       STRING(machtran.total_time)  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'machemp' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Emp ID'     THEN machemp.employee ELSE ~
    IF sortColumn EQ 'Name'       THEN STRING(employee.last_name) + ',' + STRING(employee.first_name) ELSE ~
    IF sortColumn EQ 'Machine'    THEN machtran.machine ELSE ~
    IF sortColumn EQ 'Start Date' THEN STRING(YEAR(machemp.start_date),'9999') + ~
                                       STRING(MONTH(machemp.start_date),'99') + ~
                                       STRING(DAY(machemp.start_date),'99') ELSE ~
    IF sortColumn EQ 'Started'    THEN STRING(machemp.start_time) ELSE ~
    IF sortColumn EQ 'End Date'   THEN STRING(YEAR(machemp.end_date),'9999') + ~
                                       STRING(MONTH(machemp.end_date),'99') + ~
                                       STRING(DAY(machemp.end_date),'99') ELSE ~
    IF sortColumn EQ 'Ended'      THEN STRING(machemp.end_time) ELSE ~
    IF sortColumn EQ 'Shift'      THEN machemp.Shift ELSE ~
    IF sortColumn EQ 'Total'      THEN STRING(machemp.total_time) ELSE ~
    IF sortColumn EQ 'Type'       THEN machemp.ratetype ELSE ~
    IF sortColumn EQ 'Rate'       THEN STRING(machemp.Rate) ELSE ~
       STRING(machemp.posted) ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'machtran2' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Machine'    THEN machtran.machine               ELSE ~
    IF sortColumn EQ 'Job'        THEN machtran.job_number            ELSE ~
    IF sortColumn EQ 'Sub'        THEN STRING(machtran.job_sub,'>9')        ELSE ~
    IF sortColumn EQ 'Form'       THEN STRING(machtran.form_number,'>>9')   ELSE ~
    IF sortColumn EQ 'Blk'        THEN STRING(machtran.blank_number,'>9')   ELSE ~
    IF sortColumn EQ 'Pass'       THEN STRING(machtran.pass_sequence,'>>9') ELSE ~
    IF sortColumn EQ 'Charge'     THEN machtran.charge_code           ELSE ~
    IF sortColumn EQ 'Start Date' THEN STRING(YEAR(machtran.start_date),'9999') + ~
                                       STRING(MONTH(machtran.start_date),'99') + ~
                                       STRING(DAY(machtran.start_date),'99') ELSE ~
    IF sortColumn EQ 'Log In'     THEN STRING(machtran.start_time)    ELSE ~
    IF sortColumn EQ 'End Date'   THEN STRING(YEAR(machtran.end_date),'9999') + ~
                                       STRING(MONTH(machtran.end_date),'99') + ~
                                       STRING(DAY(machtran.end_date),'99') ELSE ~
    IF sortColumn EQ 'Shift'      THEN machtran.shift                 ELSE ~
    IF sortColumn EQ 'Log Out'    THEN STRING(machtran.end_time)      ELSE ~
    IF sortColumn EQ 'Run'        THEN STRING(machtran.run_qty)       ELSE ~
       STRING(machtran.total_time)  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'pc-prdh' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Trans Date' THEN STRING(YEAR(pc-prdh.trans-date),'9999') + ~
                                       STRING(MONTH(pc-prdh.trans-date),'99') + ~
                                       STRING(DAY(pc-prdh.trans-date),'99') ELSE ~
    IF sortColumn EQ 'Shift'      THEN STRING(pc-prdh.shift, '>>9')            ELSE ~
       pc-prdh.m-code  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'pc-prdd' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ '  Job#'       THEN pc-prdd.job-no                    ELSE ~
    IF sortColumn EQ 'Sheet'        THEN STRING(pc-prdd.frm, '>>>')        ELSE ~
    IF sortColumn EQ 'Blank'        THEN STRING(pc-prdd.blank-no, '>>>')   ELSE ~
    IF sortColumn EQ 'Pass'         THEN STRING(pc-prdd.pass, '>>>')       ELSE ~
    IF sortColumn EQ 'Item#'        THEN pc-prdd.i-no                      ELSE ~
    IF sortColumn EQ 'Name'         THEN pc-prdd.i-name                    ELSE ~
    IF sortColumn EQ 'Code'         THEN pc-prdd.CODE                      ELSE ~
    IF sortColumn EQ 'Start Time'   THEN STRING(pc-prdd.startx, '99.99')   ELSE ~
    IF sortColumn EQ 'Stop Time'    THEN STRING(pc-prdd.stopx, '99.99')    ELSE ~
    IF sortColumn EQ 'Run Hours'    THEN STRING(pc-prdd.hours, '>>9.99-')  ELSE ~
    IF sortColumn EQ 'Crew Size'    THEN STRING(pc-prdd.crew, '>9.9')      ELSE ~
    IF sortColumn EQ 'Quantity'     THEN STRING(pc-prdd.qty, '>>>>>>>9-')  ELSE ~
    IF sortColumn EQ 'Waste'        THEN STRING(pc-prdd.waste, '>>>>9-')   ELSE ~
       STRING(pc-prdd.COMPLETE, 'Y/N')  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'pc-prdd2' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Job Number'   THEN pc-prdd.job-no                 ELSE ~
    IF sortColumn EQ 'S'            THEN STRING(pc-prdd.frm, '>>9')     ELSE ~
    IF sortColumn EQ 'B'            THEN STRING(pc-prdd.blank-no, '>9') ELSE ~
    IF sortColumn EQ 'P'            THEN STRING(pc-prdd.pass, '>>9')    ELSE ~
    IF sortColumn EQ 'Item#'        THEN pc-prdd.i-no                   ELSE ~
    IF sortColumn EQ 'Name'         THEN pc-prdd.i-name                 ELSE ~
       pc-prdd.m-code  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'tt-prdd' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Machine'      THEN tt-prdd.m-code                 ELSE ~
    IF sortColumn EQ 'S'            THEN STRING(tt-prdd.frm, '>>9')     ELSE ~
    IF sortColumn EQ 'B'            THEN STRING(tt-prdd.blank-no, '>9') ELSE ~
    IF sortColumn EQ 'P'            THEN STRING(tt-prdd.pass, '>>9')    ELSE ~
    IF sortColumn EQ 'Item#'        THEN tt-prdd.i-no                   ELSE ~
    IF sortColumn EQ 'Name'         THEN tt-prdd.i-name                 ELSE ~
    IF sortColumn EQ 'Code'         THEN tt-prdd.CODE                      ELSE ~
    IF sortColumn EQ 'Start Time'   THEN STRING(tt-prdd.startx, '99.99')   ELSE ~
    IF sortColumn EQ 'Stop Time'    THEN STRING(tt-prdd.stopx, '99.99')    ELSE ~
    IF sortColumn EQ 'Run Hours'    THEN STRING(tt-prdd.hours, '>>9.99-')  ELSE ~
    IF sortColumn EQ 'Crew Size'    THEN STRING(tt-prdd.crew, '>9.9')      ELSE ~
    IF sortColumn EQ 'Quantity'     THEN STRING(tt-prdd.qty, '>>>>>>>9-')  ELSE ~
    IF sortColumn EQ 'Waste'        THEN STRING(tt-prdd.waste, '>>>>9-')   ELSE ~
       STRING(tt-prdd.COMPLETE, 'Y/N')  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'stax' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Tax Group'             THEN stax.tax-group     ELSE ~
    IF sortColumn EQ 'Tax Code[1]'           THEN stax.tax-code1[1]  ELSE ~
    IF sortColumn EQ 'Description[1]'        THEN stax.tax-dscr1[1]  ELSE ~
    IF sortColumn EQ 'Tax Rate[1]'           THEN STRING(stax.tax-rate1[1], '>>>9.99<<<') ELSE ~
    IF sortColumn EQ 'Sales Tax Account[1]'  THEN stax.tax-acc1[1]   ELSE ~
    IF sortColumn EQ 'Tax!Frt?[1]'           THEN STRING(stax.tax-frt1[1], "Y/N") ELSE ~
    IF sortColumn EQ 'Tax Code[2]'           THEN stax.tax-code1[2]  ELSE ~
    IF sortColumn EQ 'Description[2]'        THEN stax.tax-dscr1[2]  ELSE ~
    IF sortColumn EQ 'Tax Rate[2]'           THEN STRING(stax.tax-rate1[2], '>>>9.99<<<') ELSE ~
    IF sortColumn EQ 'Sales Tax Account[2]'  THEN stax.tax-acc1[2]   ELSE ~
    IF sortColumn EQ 'Tax!Frt?[2]'           THEN STRING(stax.tax-frt1[2], "Y/N") ELSE ~
    IF sortColumn EQ 'Tax Code[3]'           THEN stax.tax-code1[3]  ELSE ~
    IF sortColumn EQ 'Description[3]'        THEN stax.tax-dscr1[3]  ELSE ~
    IF sortColumn EQ 'Tax Rate[3]'           THEN STRING(stax.tax-rate1[3], '>>>9.99<<<') ELSE ~
    IF sortColumn EQ 'Sales Tax Account[3]'  THEN stax.tax-acc1[3]   ELSE ~
    IF sortColumn EQ 'Tax!Frt?[3]'           THEN STRING(stax.tax-frt1[3], "Y/N") ELSE ~
    IF sortColumn EQ 'Tax Code[4]'           THEN stax.tax-code1[4]  ELSE ~
    IF sortColumn EQ 'Description[4]'        THEN stax.tax-dscr1[4]  ELSE ~
    IF sortColumn EQ 'Tax Rate[4]'           THEN STRING(stax.tax-rate1[4], '>>>9.99<<<') ELSE ~
    IF sortColumn EQ 'Sales Tax Account[4]'  THEN stax.tax-acc1[4]   ELSE ~
    IF sortColumn EQ 'Tax!Frt?[4]'           THEN STRING(stax.tax-frt1[4], "Y/N") ELSE ~
    IF sortColumn EQ 'Tax Code[5]'           THEN stax.tax-code1[5]  ELSE ~
    IF sortColumn EQ 'Description[5]'        THEN stax.tax-dscr1[5]  ELSE ~
    IF sortColumn EQ 'Tax Rate[5]'           THEN STRING(stax.tax-rate1[5], '>>>9.99<<<') ELSE ~
    IF sortColumn EQ 'Sales Tax Account[5]'  THEN stax.tax-acc1[5]   ELSE ~
       STRING(stax.tax-frt1[5], "Y/N")  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'carrier' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Carrier'   THEN carrier.carrier ELSE ~
    IF sortColumn EQ 'Desc.'     THEN carrier.dscr    ELSE ~
    IF sortColumn EQ 'Location'  THEN carrier.loc     ELSE ~
       carrier.chg-method  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'carr-mtx' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Zone'         THEN carr-mtx.del-zone ELSE ~
    IF sortColumn EQ 'Description'  THEN carr-mtx.del-dscr ELSE ~
    IF sortColumn EQ 'Zip Code'     THEN carr-mtx.del-zip  ELSE ~
       STRING(carr-mtx.min-rate, '>>9.999')  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'truck' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Trailer#' THEN truck.truck-code ELSE ~
       truck.truck-desc  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'emp_type' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Employee Type' THEN emp_type.emp_type ELSE ~
       emp_type.DESCRIPTION  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'rate' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Rate Usage'   THEN STRING(rate.rate_usage, "Shift/Machine") ELSE ~
    IF sortColumn EQ 'Shift'        THEN rate.shift ELSE ~
    IF sortColumn EQ 'Machine'      THEN rate.mach  ELSE ~
    IF sortColumn EQ 'Type'         THEN rate.ratetype ELSE ~
    IF sortColumn EQ 'Rate'         THEN STRING(rate.rate, '>>>>9.99<<<') ELSE ~
       rate.factortype  ~{&SORTED}                       

&ELSEIF '{&yellowColumnsName}' EQ 'mach' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Machine'    THEN mach.m-code ELSE ~
       mach.m-dscr  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'machshft' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Shift'        THEN machshft.shift              ELSE ~
    IF sortColumn EQ 'Start Time'   THEN STRING(machshft.start_time) ELSE ~
       STRING(machshft.end_time)  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'machchrg' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Charge'        THEN machchrg.charge            ELSE ~
       machchrg.charge  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'machseq' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Seq'        THEN STRING(machseq.machseq, '>9')   ELSE ~
       machseq.charge_code  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'shifts' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Shift'      THEN shifts.shift                    ELSE ~
       shifts.DESCRIPTION  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'shift_break' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Charge'      THEN shift_break.charge_code        ELSE ~
       shift_break.charge_code  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'jobseq' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Seq'        THEN STRING(jobseq.jobseq, '>9')   ELSE ~
       jobseq.charge_code  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'cmpltjob' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Machine'      THEN cmpltjob.machine                     ELSE ~
    IF sortColumn EQ 'Job'          THEN cmpltjob.job_number                  ELSE ~
    IF sortColumn EQ 'Sub'          THEN STRING(cmpltjob.job_sub, '>9')       ELSE ~
    IF sortColumn EQ 'Form'         THEN STRING(cmpltjob.form_number, '>>9')  ELSE ~
       STRING(cmpltjob.blank_number, '>9')  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'emplogin' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Start Date' THEN STRING(YEAR(emplogin.start_date),'9999') + ~
                                       STRING(MONTH(emplogin.start_date),'99') + ~
                                       STRING(DAY(emplogin.start_date),'99') ELSE ~
    IF sortColumn EQ 'Machine'    THEN emplogin.machine ELSE ~
    IF sortColumn EQ 'End Date'   THEN STRING(YEAR(emplogin.end_date),'9999') + ~
                                       STRING(MONTH(emplogin.end_date),'99') + ~
                                       STRING(DAY(emplogin.end_date),'99') ELSE ~
       emplogin.Shift ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'tt-note' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Note Title' THEN tt-note.note_title                     ELSE ~
    IF sortColumn EQ 'Note Date'  THEN STRING(YEAR(tt-note.note_date),'9999') + ~
                                       STRING(MONTH(tt-note.note_date),'99') + ~
                                       STRING(DAY(tt-note.note_date),'99') ELSE ~
       tt-note.note-src  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'employee' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Employee'        THEN employee.employee     ELSE ~
    IF sortColumn EQ 'Last Name'       THEN employee.last_name    ELSE ~
    IF sortColumn EQ 'First Name'      THEN employee.first_name   ELSE ~
    IF sortColumn EQ 'Soc Sec #'       THEN employee.soc_sec      ELSE ~
    IF sortColumn EQ 'Start Date'      THEN STRING(YEAR(employee.start_date),'9999') + ~
                                            STRING(MONTH(employee.start_date),'99') + ~
                                            STRING(DAY(employee.start_date),'99') ELSE ~
    IF sortColumn EQ 'GL Acct'         THEN employee.actnum       ELSE ~
    IF sortColumn EQ 'Rate Use'        THEN STRING(employee.rate_usage, "Y/N")   ELSE ~
       employee.emp_type  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'employee2' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Emp ID'          THEN employee.employee   ELSE ~
    IF sortColumn EQ 'Last Name'       THEN employee.last_name  ELSE ~
    IF sortColumn EQ 'FIrst Name'      THEN employee.first_name  ELSE ~
       employee.FIRST_name  ~{&SORTED}


&ELSEIF '{&yellowColumnsName}' EQ 'tt-login' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Start Date' THEN STRING(YEAR(tt-login.s-dt),'9999') + ~
                                       STRING(MONTH(tt-login.s-dt),'99') + ~
                                       STRING(DAY(tt-login.s-dt),'99') ELSE ~
    IF sortColumn EQ 'Start Time' THEN tt-login.s-time                        ELSE ~
    IF sortColumn EQ 'End Date'   THEN STRING(YEAR(tt-login.e-dt),'9999') + ~
                                       STRING(MONTH(tt-login.e-dt),'99') + ~
                                       STRING(DAY(tt-login.e-dt),'99') ELSE ~
    IF sortColumn EQ 'End Time'   THEN tt-login.e-time ELSE ~
       tt-login.t-time ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'empmach' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Machine'        THEN empmach.machine     ELSE ~
       empmach.gl_account  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'routing' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Routing Code'    THEN routing.r-code   ELSE ~
    IF sortColumn EQ 'Description'     THEN routing.dscr     ELSE ~
    IF sortColumn EQ 'Machine Code[1]' THEN routing.m-code[1]  ELSE ~
    IF sortColumn EQ 'Machine Code[2]' THEN routing.m-code[2]  ELSE ~
    IF sortColumn EQ 'Machine Code[3]' THEN routing.m-code[3]  ELSE ~
    IF sortColumn EQ 'Machine Code[4]' THEN routing.m-code[4]  ELSE ~
    IF sortColumn EQ 'Machine Code[5]' THEN routing.m-code[5]  ELSE ~
    IF sortColumn EQ 'Machine Code[6]' THEN routing.m-code[6]  ELSE ~
    IF sortColumn EQ 'Machine Code[7]' THEN routing.m-code[7]  ELSE ~
    IF sortColumn EQ 'Machine Code[8]' THEN routing.m-code[8]  ELSE ~
    IF sortColumn EQ 'Machine Code[9]' THEN routing.m-code[9]  ELSE ~
       routing.m-code[10]  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'ar-cash' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Customer Number'    THEN cust-no   ELSE ~
    IF sortColumn EQ 'Memo Date' THEN STRING(YEAR(ar-cash.check-date),'9999') + ~
                                       STRING(MONTH(ar-cash.check-date),'99') + ~
                                       STRING(DAY(ar-cash.check-date),'99') ELSE ~
       STRING(ar-cash.check-no, '9999999999') 

&ELSEIF '{&yellowColumnsName}' EQ 'ar-cashl' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Invoice#'    THEN STRING(ar-cashl.inv-no, '>>>>>9')   ELSE ~
    IF sortColumn EQ 'Invoice Date' THEN STRING(YEAR(ar-cashl.inv-date),'9999') + ~
                                         STRING(MONTH(ar-cashl.inv-date),'99') + ~
                                         STRING(DAY(ar-cashl.inv-date),'99') ELSE ~
    IF sortColumn EQ 'Balance Due'   THEN STRING(ar-cashl.amt-due,'->>,>>>,>>9.99')  ELSE ~
    IF sortColumn EQ 'Discount'   THEN STRING(ar-cashl.amt-disc,'->>,>>9.99')  ELSE ~
    IF sortColumn EQ 'Total Applied'   THEN STRING(ar-cashl.amt-paid,'->>,>>>,>>9.99')  ELSE ~
    IF sortColumn EQ 'Account Number'   THEN ar-cashl.actnum  ELSE ~
       STRING(ar-cashl.dscr, '9999999999')  ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'ar-inv' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Invoice#'    THEN STRING(ar-inv.inv-no, '>>>>>9')   ELSE ~
    IF sortColumn EQ 'Cust.#'      THEN ar-inv.cust-no                    ELSE ~
    IF sortColumn EQ 'Customer Name'   THEN ar-inv.cust-name              ELSE ~
    IF sortColumn EQ 'Inv Date' THEN STRING(YEAR(ar-inv.inv-date),'9999') + ~
                                     STRING(MONTH(ar-inv.inv-date),'99') + ~
                                     STRING(DAY(ar-inv.inv-date),'99') ELSE ~
    IF sortColumn EQ 'Inv Amount'   THEN STRING(ar-inv.gross,'->,>>>,>>9.99')  ELSE ~
    IF sortColumn EQ 'Amount Paid'  THEN STRING(ar-inv.paid,'->,>>>,>>9.99')  ELSE ~
       STRING(ar-inv.due,'->>,>>>,>>9.99')

&ELSEIF '{&yellowColumnsName}' EQ 'b-ar-inv1' &THEN
   &SCOPED-DEFINE SORTBY-PHRASE BY ~
    IF sortColumn EQ 'Invoice Date' THEN STRING(YEAR(ar-inv.inv-date),'9999') + ~
                                     STRING(MONTH(ar-inv.inv-date),'99') + ~
                                     STRING(DAY(ar-inv.inv-date),'99') ELSE ~
    IF sortColumn EQ 'Invoice#'    THEN STRING(ar-inv.inv-no, '>>>>>>>>') ELSE ~
    IF sortColumn EQ 'Invoice Amt' THEN STRING(ar-inv.paid + ar-inv.due, '->>,>>>,>>9.99')   ELSE ~
    IF sortColumn EQ 'Days Old'    THEN STRING((TODAY - ar-inv.inv-date),'->>,>>>') ELSE ~
       STRING(ar-inv.due,'->>,>>>,>>9.99') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'ap-invl' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
     IF sortColumn EQ 'PO Number'   THEN STRING(ap-invl.po-no) ELSE ~
     IF sortColumn EQ 'Quantity'    THEN STRING(ap-invl.qty,'->>>,>>9.9<<<<<') ELSE ~
     IF sortColumn EQ 'UOM'         THEN ap-invl.cons-uom ELSE ~
     IF sortColumn EQ 'Price'       THEN STRING(ap-invl.unit-pr, '->,>>>,>>9.99<<<<') ELSE ~
     IF sortColumn EQ 'UOM Price'   THEN ap-invl.pr-qty-uom ELSE ~
     IF sortColumn EQ 'Tax'         THEN STRING(ap-invl.tax, "Y/N") ELSE ~
     IF sortColumn EQ 'SqFt'        THEN STRING(ap-invl.unit-pr,'>>,>>9.9<<<') ELSE ~
     IF sortColumn EQ 'Amount'      THEN STRING(ap-invl.amt,'->>>>,>>9.99') ELSE ~
     IF sortColumn EQ 'Amt MSF'     THEN STRING(ap-invl.amt-msf,'->>,>>9.99') ELSE ~
     IF sortColumn EQ 'Description' THEN ap-invl.dscr ELSE ~
        ap-invl.actnum ~{&SORTED}

/* gdm - 07310904 */
&ELSEIF '{&yellowColumnsName}' EQ 'b-multbl' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
     IF sortColumn EQ 'Form#'          THEN STRING(reftable.val[1],'>>>>>9') ELSE ~
     IF sortColumn EQ 'Blank#'         THEN STRING(reftable.val[2],'>>>>>9') ELSE ~
     IF sortColumn EQ 'Customer Part#' THEN eb.part-no ELSE ~
     IF sortColumn EQ 'Priced By'      THEN STRING(eb.yrprice, 'Yield/Request') ELSE ~
        STRING(eb.num-up, '>>>>>9') ~{&SORTED}

/* gdm - 07310903 */
&ELSEIF '{&yellowColumnsName}' EQ 'b-est' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
     IF sortColumn EQ 'Estimate #'     THEN est.est-no ELSE ~
     IF sortColumn EQ 'Cust. #'        THEN STRING(eb.cust-no) ELSE ~
     IF sortColumn EQ 'Cust Part #'    THEN STRING(eb.part-no)   ELSE ~
     IF sortColumn EQ 'Ship To'        THEN STRING(eb.ship-id)   ELSE ~
     IF sortColumn EQ 'Item Name'      THEN STRING(eb.part-dscr1) ELSE ~
     IF sortColumn EQ 'FG Item#'       THEN STRING(eb.stock-no)  ELSE ~
     IF sortColumn EQ 'Est Qty'        THEN STRING(est-qty.eqty, '>>>>>>>9') ELSE ~
     IF sortColumn EQ 'Style'          THEN STRING(eb.style)     ELSE ~
     IF sortColumn EQ 'Flute'          THEN STRING(eb.flute)     ELSE ~
     IF sortColumn EQ 'Test'           THEN STRING(eb.test)      ELSE ~
     IF sortColumn EQ 'Board'          THEN STRING(ef.board)     ELSE ~
     IF sortColumn EQ 'Caliper'        THEN STRING(ef.cal, '>9.99999<') ELSE ~
     IF sortColumn EQ 'Category'       THEN STRING(eb.procat)                   ELSE ~
     IF sortColumn EQ 'Length'         THEN STRING(eb.len, '>>9.99')    ELSE ~
     IF sortColumn EQ 'Width'          THEN STRING(eb.wid, '>>9.99')    ELSE ~
     IF sortColumn EQ 'Depth'          THEN STRING(eb.dep, '>>9.99')    ELSE ~
     IF sortColumn EQ 'FORM'           THEN STRING(eb.form-no, '>9')    ELSE ~
     IF sortColumn EQ 'BLANK'          THEN STRING(eb.blank-no, '>9')   ELSE ~
     IF sortColumn EQ 'Tab'            THEN STRING(eb.tab-in, 'In/Out') ELSE ~
     IF sortColumn EQ 'Colors'         THEN STRING(eb.i-col, '>9')      ELSE ~
     IF sortColumn EQ 'Passes'         THEN STRING(eb.i-pass, '>9')     ELSE ~
     IF sortColumn EQ 'Coating'        THEN STRING(eb.i-coat, '>9')     ELSE ~
     IF sortColumn EQ 'Coat Passes'    THEN STRING(eb.i-coat-p, '>9')   ELSE ~
     IF sortColumn EQ 'Yield Qty       THEN STRING(eb.yld-qty, '->>>>>>>9')   ELSE ~
     IF sortColumn EQ 'Qty/Set'        THEN STRING(eb.quantityPerSet, '->>>>>>>9.99')   ELSE ~
     IF sortColumn EQ 'Inks/Form'      THEN STRING(ef.f-col, '>9')      ELSE ~
     IF sortColumn EQ 'Passes/Form'    THEN STRING(ef.f-pass, '>9')     ELSE ~
     IF sortColumn EQ 'Coatings/Form'  THEN STRING(ef.f-coat, '>9')     ELSE ~
     IF sortColumn EQ 'Coat Passes/Form' THEN STRING(ef.f-coat-p, '>9') ELSE ~
     IF sortColumn EQ 'Purch/Manuf'    THEN STRING(eb.pur-man, 'P/M')   ELSE ~
        STRING(est.est-date) ~{&SORTED}

/* gdm - 07310902 */
&ELSEIF '{&yellowColumnsName}' EQ 'b-estitm1' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
     IF sortColumn EQ 'Estimate #'     THEN est.est-no                   ELSE ~
     IF sortColumn EQ 'Est Date'       THEN STRING(YEAR(est.est-date),'9999') + ~
                                            STRING(MONTH(est.est-date),'99') + ~
                                            STRING(DAY(est.est-date),'99') ELSE ~
     IF sortColumn EQ 'Cust. #'        THEN STRING(eb.cust-no)           ELSE ~
     IF sortColumn EQ 'Ship To'        THEN STRING(eb.ship-id)           ELSE ~
     IF sortColumn EQ 'Cust Part #'    THEN STRING(eb.part-no)           ELSE ~
     IF sortColumn EQ 'Ship To'        THEN STRING(eb.ship-id)           ELSE ~
     IF sortColumn EQ 'Item Description'      THEN STRING(eb.part-dscr1) ELSE ~
     IF sortColumn EQ 'FG Item#'       THEN STRING(eb.stock-no)          ELSE ~
     IF sortColumn EQ 'Qty'            THEN STRING(eb.bl-qty, '>>>,>>>,>>>') ELSE ~
     IF sortColumn EQ 'Style'          THEN STRING(eb.style)             ELSE ~
     IF sortColumn EQ 'Board'          THEN STRING(ef.board)             ELSE ~
     IF sortColumn EQ 'Paper 1'        THEN STRING(ef.medium)            ELSE ~
     IF sortColumn EQ 'Paper 2'        THEN STRING(ef.flute)             ELSE ~
     IF sortColumn EQ 'Caliper'        THEN STRING(ef.cal, '>9.99999<')  ELSE ~
     IF sortColumn EQ 'Category'       THEN STRING(eb.procat)            ELSE ~
     IF sortColumn EQ 'Length'         THEN STRING(eb.len, '>9.99999')   ELSE ~
     IF sortColumn EQ 'Width'          THEN STRING(eb.wid, '>9.99999')   ELSE ~
     IF sortColumn EQ 'Depth'          THEN STRING(eb.dep, '>9.99999')   ELSE ~
     IF sortColumn EQ 'Qty/Set'        THEN STRING(eb.cust-%, '->>,>>>') ELSE ~
     IF sortColumn EQ 'Colors'         THEN STRING(eb.i-col, '>9')       ELSE ~
     IF sortColumn EQ 'Coating'        THEN STRING(eb.i-coat, '>9')      ELSE ~
     IF sortColumn EQ 'S'              THEN STRING(eb.form-no, '>>>')    ELSE ~
     IF sortColumn EQ 'B'              THEN STRING(eb.blank-no, '>>>')   ELSE ~
     IF sortColumn EQ '# on Width'     THEN STRING(eb.num-wid, '>9')     ELSE ~
     IF sortColumn EQ '# on Length'    THEN STRING(eb.num-len, '>9')     ELSE ~
     IF sortColumn EQ '# Up'           THEN STRING(eb.num-up, '>>>,>>9') ELSE ~
     IF sortColumn EQ 'Die Inches'     THEN STRING(eb.die-in, '>>>>9')   ELSE ~
     IF sortColumn EQ 'Inks/Form'      THEN STRING(ef.f-col, '>>')       ELSE ~
     IF sortColumn EQ 'Passes/Form'    THEN STRING(ef.f-pass, '>>')      ELSE ~
     IF sortColumn EQ 'Coatings/Form'  THEN STRING(ef.f-coat, '>>')      ELSE ~
     IF sortColumn EQ 'Coat Passes/Form' THEN STRING(ef.f-coat-p, '>>')  ELSE ~
        STRING(eb.pur-man, 'P/M') ~{&SORTED}

/* gdm - 10260911 */
&ELSEIF '{&yellowColumnsName}' EQ 'l-est' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
     IF sortColumn EQ 'Estimate #'      THEN tt-eb.est-no                   ELSE ~
     IF sortColumn EQ 'Customer#'       THEN STRING(tt-eb.cust-no)          ELSE ~
     IF sortColumn EQ 'Cust Name'      THEN STRING(tt-eb.cust-name)         ELSE ~
     IF sortColumn EQ 'Item Description'THEN STRING(tt-eb.part-dscr1)       ELSE ~
     IF sortColumn EQ 'Item Name'       THEN STRING(tt-eb.part-dscr1)       ELSE ~
     IF sortColumn EQ 'Cust Part #'     THEN STRING(tt-eb.part-no)          ELSE ~
     IF sortColumn EQ 'Style'           THEN STRING(tt-eb.style)            ELSE ~
     IF sortColumn EQ 'Length'          THEN STRING(tt-eb.len, '>>9.99')    ELSE ~
     IF sortColumn EQ 'Width'           THEN STRING(tt-eb.wid, '>>9.99')    ELSE ~
        STRING(tt-eb.dep, '>>9.99') ~{&SORTED}

&ELSEIF '{&yellowColumnsName}' EQ 'l-estcst' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
     IF sortColumn EQ 'Estimate #'      THEN eb.est-no                   ELSE ~
     IF sortColumn EQ 'Cust. Name'      THEN STRING(eb.ship-name)        ELSE ~
     IF sortColumn EQ 'Item Description'THEN STRING(eb.part-dscr1)       ELSE ~
     IF sortColumn EQ 'Item Name'       THEN STRING(eb.part-dscr1)       ELSE ~
     IF sortColumn EQ 'Cust Part #'     THEN STRING(eb.part-no)          ELSE ~
     IF sortColumn EQ 'Style'           THEN STRING(eb.style)            ELSE ~
     IF sortColumn EQ 'Length'          THEN STRING(eb.len, '>>9.99')    ELSE ~
     IF sortColumn EQ 'Width'           THEN STRING(eb.wid, '>>9.99')    ELSE ~
        STRING(eb.dep, '>>9.99') ~{&SORTED}
/* Ticket 20737 */
&ELSEIF '{&yellowColumnsName}' EQ 'w-bin' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
     IF sortColumn EQ "Job#"    THEN w-bin.job-no + STRING(w-bin.job-no2, "99") ELSE ~
    IF sortColumn EQ "WHS"       THEN w-bin.loc                                  ELSE ~
    IF sortColumn EQ "Receipt Date" THEN STRING(w-bin.last-rct-date)            ELSE ~
    IF sortColumn EQ "Bin"   THEN w-bin.loc-bin                              ELSE ~
    IF sortColumn EQ "tag"       THEN w-bin.tag                                  ELSE ~
    IF sortColumn EQ "rfid"       THEN w-bin.rfid                                ELSE ~
    IF sortColumn EQ "FG Lot" THEN w-bin.stack-code                          ELSE ~
    IF sortColumn EQ "Customer#"   THEN w-bin.cust-no                              ELSE ~
    IF sortColumn EQ "Bol Qty"    THEN STRING(w-bin.to-bol, "9999999999")         ELSE ~
    IF sortColumn EQ "Released Qty"    THEN STRING(w-bin.to-rel, "9999999999")         ELSE ~
    IF sortColumn EQ "Quantity"       THEN STRING(9999999999.9999999999 + w-bin.qty, "-9999999999.9999999999") ELSE ~
    IF sortColumn EQ "units"     THEN STRING(9999999999 + w-bin.units, "-9999999999")                     ELSE ~
    IF sortColumn EQ "Unit Count" THEN STRING(w-bin.case-count, "9999999999")                             ELSE ~
                                       STRING(w-bin.partial-count, "-9999999999")  ~{&SORTED}

/* btr - 02/15/2011  */
&ELSEIF '{&yellowColumnsName}' EQ 'b-wipmach' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
     IF sortColumn EQ 'Machine Department'            THEN STRING(dept.fc)                    ELSE ~
        STRING(dept.seq) ~{&SORTED}


&ELSEIF '{&yellowColumnsName}' EQ 'module' &THEN
  &SCOPED-DEFINE SORTBY-PHRASE BY ~
     IF sortColumn EQ 'Module Description'     THEN module.dscr             ELSE ~
     IF sortColumn EQ 'Module Code'            THEN module.module           ELSE ~
     IF sortColumn EQ 'Expiration Date'        THEN STRING(YEAR(module.expire-date),'9999') + ~
                                                    STRING(MONTH(module.expire-date),'99')  + ~
                                                    STRING(DAY(module.expire-date),'99')  ELSE ~
        STRING(module.is-used, 'Y/N') ~{&SORTED}
&ENDIF

/* ****************************************************************** */  

/* ****************************************************************** */  

  &IF LOOKUP('{&yellowColumnsName}','inv-head,ar-inv,ar-cash') EQ 0 &THEN
     IF sortBy THEN DO:
        {&OPEN-QUERY-{&BROWSE-NAME}}
     END.
     ELSE DO:
        &SCOPED-DEFINE SORTED DESCENDING
        {&OPEN-QUERY-{&BROWSE-NAME}}
     END.
        APPLY 'value-changed' TO BROWSE {&browse-name}.
  &ELSEIF '{&yellowColumnsName}' EQ 'inv-head' &THEN
     IF sortBy THEN
     DO:
        {oe/j-oeinv-a.i}
     END.
     ELSE DO:
        &SCOPED-DEFINE SORTED DESCENDING
        {oe/j-oeinv-a.i}
     END.
     
     APPLY 'value-changed' TO BROWSE {&browse-name}. 
  &ELSEIF '{&yellowColumnsName}' EQ 'ar-cash' &THEN
     IF sortBy THEN
     DO:
        {ar/j-dbcr.i}
     END.
     ELSE DO:
        
        {ar/j-dbcr-d.i}
     END.
     
     APPLY 'value-changed' TO BROWSE {&browse-name}. 
  &ELSEIF '{&yellowColumnsName}' EQ 'ar-inv' &THEN
     IF sortBy THEN
     DO:
        {ar/j-inv.i}
     END.
     ELSE DO:
        
        {ar/j-inv-d.i}
     END.
     
     APPLY 'value-changed' TO BROWSE {&browse-name}.

  &ENDIF

END PROCEDURE.
&ENDIF

PROCEDURE getColLabels:
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = 1 TO BROWSE {&BROWSE-NAME}:NUM-COLUMNS:
    IF BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(i):LABEL-BGCOLOR EQ 14 THEN
    colLabels = colLabels + comma(colLabels) +
                BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(i):LABEL.
  END. /* do i */
  ASSIGN
    sortColumn = ENTRY(1,colLabels)
    browserTitle = BROWSE {&BROWSE-NAME}:TITLE.
END PROCEDURE.

PROCEDURE startSearch:
  DEFINE VARIABLE sortDisplay AS CHARACTER NO-UNDO.

  IF colLabels EQ '' THEN RUN getColLabels.

  IF NOT CAN-DO(colLabels,BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:LABEL) THEN
  RETURN NO-APPLY.

  IF sortColumn EQ BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:LABEL THEN
     sortBy = NOT sortBy.

  ASSIGN
    sortColumn = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:LABEL
    sortDisplay = TRIM(sortColumn + ' - ' + STRING(sortBy,'Ascending/Descending'))
    &IF DEFINED(noSortByField) EQ 0 &THEN
    fi_sortBy:SCREEN-VALUE IN FRAME {&FRAME-NAME} = sortDisplay
    &ENDIF
    &IF DEFINED(autoFind) NE 0 &THEN
    auto_find:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
    auto_find
    &ENDIF
    &IF DEFINED(lvSearch) NE 0 &THEN
    lv-search:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
    lv-search
    &ENDIF
    .
 

  IF browserTitle NE '' THEN
  BROWSE {&BROWSE-NAME}:TITLE = browserTitle + ' (sorted by: ' + sortDisplay + ')'.
  RUN openQuery.
END PROCEDURE.

/* Sort removed from b-cusinq 'Date' */
/* STRING((YEAR(tt-arinq.tr-date) * 10000000) + ~
           (MONTH(tt-arinq.tr-date) * 1000)    + ~
           DAY(tt-arinq.tr-date) + ~
           (IF tt-arinq.inv-no EQ 0 THEN 99999999 ELSE 0),'99999999') ~
  ELSE ~*/
