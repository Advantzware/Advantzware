0 "Dynamic Parameter Lookup" yes "System" "NS" "_default" 9999 "Custom" "Grid" "" "" "FOR EACH dynParamValue WHERE dynParamValue.subjectID GT 0 AND INDEX( dynParamValue.paramTitle , ""Lookup"" ) NE 0" "201904059999999999999" 0
1 "Machine Transaction Query" yes "System" "TS" "_default" 0 "Custom" "Grid" "" "" "FOR EACH machtran WHERE machtran.company EQ [[company]] AND machtran.machine GE [[startMachine]] AND machtran.machine LE [[endMachine]] AND machtran.shift GE [[startShift]] AND machtran.shift LE [[endShift]] AND DATETIME ( machtran.start_date , machtran.start_time * 1000 ) GE DATETIME ( [[startTransDate]] , INT ( [|CalcShiftStartTime|] ) ) AND DATETIME ( machtran.end_date , machtran.end_time ) LE DATETIME ( [[endTransDate]] , INT ( [|CalcShiftEndTime|] ) ),  FIRST mach WHERE mach.company EQ machtran.company,  FIRST job WHERE job.company EQ machtran.company AND job.job-no EQ machtran.job_number AND job.job-no2 EQ machtran.job_sub,  FIRST est OUTER-JOIN WHERE est.company EQ job.company AND est.est-no EQ job.est-no,  FIRST ef OUTER-JOIN WHERE ef.company EQ est.company AND ef.est-no EQ est.est-no AND ef.form-no EQ machtran.form_number,  FIRST eb OUTER-JOIN WHERE eb.company EQ ef.company AND eb.est-no EQ ef.est-no AND eb.form-no EQ ef.form-no AND ( eb.blank-no EQ machtran.blank_number OR machtran.blank_number EQ 0 )" "201901230000000000001" 0
2 "Audit History by Audit ID" yes "System" "NS" "_default" 0 "Custom" "Grid" "" "" "FOR EACH AuditHdr WHERE AuditHdr.AuditType EQ ""LOG"" AND AuditHdr.AuditTable EQ ""job-mch"" AND AuditHdr.AuditID EQ [[auditID]],  EACH AuditDtl OF AuditHdr WHERE AuditDtl.AuditAfterValue BEGINS ""Job Recalc""" "201903286321503436850" 0
3 "Job Lookup" yes "System" "JC" "_default" 0 "Custom" "Grid" "" "" "FOR EACH job-hdr WHERE job-hdr.company EQ [[company]]" "201904046356503445516" 0
5 "Inventory by Bin and Tag" yes "System" "FG" "_default" 0 "Custom" "Grid" "" "AOA\dynBL\r-ibtag.p" "FOR" "201906097228703524082" 0
7 "EDI 810 Exception" yes "System" "AR" "_default" 0 "Custom" "Grid" "" "AOA\dynBL\edi810Xcp.p" "FOR" "201906137801903524352" 0
8 "Machine Transaction" yes "System" "TS" "_default" 0 "Custom" "Grid" "" "AOA\dynBL\r-mchtrn.p" "FOR EACH ttMachineTransactions" "201904085548603445644" 0
9 "Audit Field Query" yes "System" "NS" "_default" 0 "Custom" "Grid" "" "" "FOR EACH AuditHdr WHERE AuditHdr.AuditDB EQ [[AuditDB]] AND AuditHdr.AuditTable EQ [[AuditTable]] AND AuditHdr.AuditKey EQ [[AuditKey]],  EACH AuditDtl OF AuditHdr WHERE AuditDtl.AuditField EQ [[AuditField]]" "201904237192203445836" 0
10 "Customer Inventory" yes "System" "FG" "_default" 0 "Custom" "Grid" "" "AOA\dynBL\r-cusinv.p" "FOR" "201906145351403524390" 0
11 "Audit History BL" yes "System" "NS" "_default" 0 "Custom" "Grid" "" "AOA\dynBL\AuditHist.p" "FOR" "201906175565903524469" 0
12 "Cash Receipt Salesrep Name" yes "System" "AR" "_default" 0 "Custom" "Grid" "" "AOA\dynBL\r-cashs2.p" "FOR" "201906177825003524525" 0
13 "IF1 Bin View Allocated" yes "System" "FG" "_default" 0 "Custom" "Grid" "" "" "FOR EACH oe-ordl WHERE oe-ordl.company EQ [[company]] AND oe-ordl.i-no EQ [[fgItem]] AND oe-ordl.opened EQ YES,  FIRST oe-ord WHERE oe-ord.company EQ oe-ordl.company AND oe-ord.ord-no EQ oe-ordl.ord-no" "201905010337103446073" 0
14 "IF1 Bin View POs" yes "System" "FG" "_default" 0 "Custom" "Grid" "" "" "FOR EACH po-ordl WHERE po-ordl.company EQ [[company]] AND po-ordl.i-no EQ [[fgItem]] AND po-ordl.opened EQ YES,  FIRST po-ord WHERE po-ord.company EQ po-ordl.company AND po-ord.po-no EQ po-ordl.po-no" "201905014284103446163" 0
15 "IF1 Bin View Jobs" yes "System" "FG" "_default" 0 "Custom" "Grid" "" "" "FOR EACH job-hdr WHERE job-hdr.company EQ [[company]] AND job-hdr.i-no BEGINS [[fgItem]] AND job-hdr.opened EQ YES USE-INDEX i-no,  FIRST job WHERE job.company EQ job-hdr.company AND job.job EQ job-hdr.job AND job.job-no EQ job-hdr.job-no AND job.job-no2 EQ job-hdr.job-no2" "201905014289203446164" 0
16 "AP Invoice Posting" yes "System" "AP" "_default" 0 "Custom" "Grid" "" "AOA\dynBL\r-apve&p.p" "FOR" "201906186399403524606" 0
17 "Invoice Post Update GL" yes "System" "OE" "_default" 0 "Custom" "Grid" "" "AOA\dynBL\r-inve&pb.p" "FOR" "201906198191803524856" 0
18 "Orders Booked by Order Number" yes "System" "OE" "_default" 0 "Custom" "Grid" "" "AOA\dynBL\r-booko#.p" "FOR" "201906205697603525394" 0
19 "Post BOL Create Invoice" yes "System" "OE" "_default" 0 "Custom" "Grid" "" "AOA\dynBL\r-bolpst.p" "FOR" "201906206506703526054" 0
20 "Open Orders" yes "System" "OE" "_default" 0 "Custom" "Grid" "" "AOA\dynBL\r-ordopn.p" "FOR" "201906206507803526055" 0
21 "Customer Credit Hold List" yes "System" "AR" "_default" 0 "Custom" "Grid" "" "" "FOR EACH cust WHERE cust.company EQ [[company]] AND cust.cr-hold EQ YES" "201907165679203621844" 0
22 "Online Status" yes "System" "AR" "_default" 0 "Custom" "Grid" "" "" "FOR EACH oe-prmtx WHERE oe-prmtx.company EQ [[company]] AND oe-prmtx.cust-no GE [[startCustNo]] AND oe-prmtx.cust-no LE [[endCustNo]]" "201907208300103627319" 0
23 "Customer Lookup" yes "System" "AR" "_default" 0 "Custom" "Grid" "" "" "FOR EACH cust WHERE cust.company EQ [[company]]" "201906236720303527716" 0
24 "Warehouse Lookup" yes "System" "FG" "_default" 0 "Custom" "Grid" "" "" "FOR EACH loc WHERE loc.company EQ [[company]]" "201906244759403527798" 0
25 "FG item Lookup" yes "System" "FG" "_default" 0 "Custom" "Grid" "" "" "FOR EACH itemfg WHERE itemfg.company EQ [[company]]" "201906244847103527849" 0
26 "RM Item Lookup" yes "System" "RM" "_default" 0 "Custom" "Grid" "" "" "FOR EACH item WHERE item.company EQ [[company]]" "201906244857503527868" 0
27 "Order Lookup" yes "System" "OE" "_default" 0 "Custom" "Grid" "" "" "FOR EACH oe-ord WHERE oe-ord.company EQ [[company]]" "201906244876403527898" 0
28 "User Lookup" yes "System" "NS" "_default" 0 "Custom" "Grid" "" "" "FOR EACH users" "201906244886603527920" 0
29 "Sales Rep Lookup" yes "System" "AR" "_default" 0 "Custom" "Grid" "" "" "FOR EACH sman WHERE sman.company EQ [[company]]" "201906245040603527931" 0
30 "Machine Lookup" yes "System" "EQ" "_default" 0 "Custom" "Grid" "" "" "FOR EACH mach WHERE mach.company EQ [[company]]" "201906245056203527951" 0
31 "Shifts Lookup" yes "System" "TS" "_default" 0 "Custom" "Grid" "" "" "FOR EACH shifts WHERE shifts.company EQ [[company]]" "201906245126103527971" 0
32 "Vendor Lookup" yes "System" "AP" "_default" 0 "Custom" "Grid" "" "" "FOR EACH vend WHERE vend.company EQ [[company]]" "201906245913403528046" 0
33 "Material Type Lookup" yes "System" "NS" "_default" 0 "Custom" "Grid" "" "" "FOR EACH mat" "201906245924203528066" 0
34 "Product Category Lookup" yes "System" "RM" "_default" 0 "Custom" "Grid" "" "" "FOR EACH procat WHERE procat.company EQ [[company]]" "201906245938903528079" 0
36 "Purchase Order Lookup" yes "System" "PO" "_default" 0 "Custom" "Grid" "" "" "FOR EACH po-ordl WHERE po-ordl.company EQ [[company]]" "201906245991403528161" 0
37 "CAD Lookup" yes "System" "FG" "_default" 0 "Custom" "Grid" "" "" "FOR EACH itemfg WHERE itemfg.company EQ [[company]] AND itemfg.cad-no GT """" USE-INDEX cad-no" "201906246090703528216" 0
38 "BOL Lookup" yes "System" "OE" "_default" 0 "Custom" "Grid" "" "" "FOR EACH oe-bolh WHERE oe-bolh.company EQ [[company]]" "201906246130703528251" 0
39 "Invoice Lookup" yes "System" "OE" "_default" 0 "Custom" "Grid" "" "" "FOR EACH inv-head WHERE inv-head.company EQ [[company]] USE-INDEX inv-no" "201906246250503528319" 0
40 "Web Order Hold" yes "System" "OE" "_default" 0 "Custom" "Grid" "" "AOA\dynBL\webOrderHold.p" "FOR" "201906247471203528450" 0
