1 "Company" "System" "Company" "Character" "x(3)" "Fill-In" "" "" 1 6 5 "dynInitCompany" "" "" ? "" "201901400000000000001"
2 "Date" "System" "Date" "Date" "99/99/9999" "Fill-In" "" "" 1 16 0 "" "" "" "CALENDAR,DATEPICKLIST" "" "201902265315702670007"
3 "Time" "System" "Time" "Character" "99:99" "Fill-In" "" "0000" 1 8 0 "" "dynValTime" "" "NO:DISABLE,YES:ENABLE" "useTimes" "201902274861502670099"
4 "Description" "System" "Description" "Character" "x(256)" "Fill-In" "" "<Start Range Value>" 1 40 0 "" "" "" ? "" "201903016395202670251"
5 "allRange" "System" "All Range" "Logical" "yes/no" "Toggle-Box" "" "yes" 1 14 0 "" "" "" ? "" "201902265340002670008"
6 "Machine" "System" "Machine" "Character" "x(6)" "Fill-In" "" "" 1 10 0 "" "dynValMachine" "" "NO:ENABLE,YES:DISABLE" "allMachines" "201902266057502670026"
7 "Shift" "System" "Shift" "Character" "X" "Fill-In" "" "" 1 4 0 "" "" "" "NO:ENABLE,YES:DISABLE" "allShifts" "201902266178502670067"
8 "AuditID" "System" "Audit ID" "Integer" ">>>>>>9" "Fill-In" "" "" 1 16 0 "" "" "" ? "" "201903286306903436840"
9 "Fill-In Character" "System" "Generic Fill In" "Character" "x(256)" "Fill-In" "" "" 1 40 0 "" "" "" ? "" "201904237166903445831"
11 "Radio-Set Character" "System" "Radio Set" "Character" "x(8)" "Radio-Set" "First,1,Second,2,Third,3" "1" 1 31 0 "" "" "" "HORIZONTAL" "" "201906100102103524205"
12 "Toggle-Box" "System" "Toggle Box" "Logical" "yes/no" "Toggle-Box" "" "no" 1 40 0 "" "" "" ? "" "201906100159803524218"
13 "Combo-Box Character" "System" "Combo Box" "Character" "x(256)" "Combo-Box" "" "" 1 20 10 "" "" "" ? "" "201906175536603524466"
14 "Fill-In Integer" "System" "Fill-In Integer" "Integer" "->,>>>,>>9" "Fill-In" "" "0" 1 14 0 "" "" "" ? "" "201906178051203524548"
15 "Period" "System" "Period" "Integer" ">9" "Fill-In" "" "" 1 5 0 "" "" "" ? "" "201906186685903524713"
16 "Secure" "System" "Secure" "Logical" "yes/no" "Toggle-Box" "" "yes" 1 11 0 "" "" "" ? "" "201906186848203524734"
17 "InvNo" "System" "Invoice" "Integer" ">>>>>>9" "Fill-In" "" "0" 1 10 0 "" "" "" ? "" "201906198226703524899"
18 "ItemNo" "System" "Item" "Character" "x(15)" "Fill-In" "" "" 1 20 0 "" "" "" ? "" "201906205796403525415"
19 "OrderNo" "System" "Order" "Integer" ">>>>>>>9" "Fill-In" "" "" 1 10 0 "" "" "" ? "" "201906205902303525454"
20 "BOL" "System" "BOL" "Integer" ">>>>>>>9" "Fill-In" "" "" 1 13 0 "" "" "" ? "" "201906206628303526063"
21 "LocBin" "System" "Bin" "Character" "x(8)" "Fill-In" "" "" 1 11 0 "" "" "" ? "" "201906206664303526067"
22 "PONumber" "System" "PO Number" "Character" "x(8)" "Fill-In" "" "" 1 11 0 "" "" "" ? "" "201906206886003526359"
23 "CAD" "System" "CAD" "Character" "x(15)" "Fill-In" "" "" 1 20 0 "" "" "" ? "" "201906206890803526360"
24 "JobNo" "System" "Job" "Character" "x(9)" "Fill-In" "" "" 1 9 0 "" "" "" ? "" "201906206895803526361"
25 "JobNo2" "System" ":" "Integer" ">>9" "Fill-In" "" "99" 1 5 0 "" "" "" ? "" "201906206914003526362"
10 "InventoryStockID" "System" "Inventory Stock ID" "Character" "x(14)" "Fill-In" "" "" 1 14 0 "" "" "" ? "" "201910106089504351450"
26 "EstimateNo" "System" "Estimate Number" "Character" "x(8)" "Fill-In" "" "" 1 11 0 "" "" "" ? "" "201910255766204353519"
27 "ItemID" "System" "Item ID" "Character" "x(20)" "Fill-In" "" "" 1 24 0 "" "" "" ? "" "201910255798504353524"
28 "CorrectData" "System" "Correct Data" "Logical" "yes/no" "Toggle-Box" "" "yes" 1 16 0 "" "" "" ? "" "201911064450004355512"
29 "Location" "System" "Location" "Character" "x(5)" "Fill-In" "" "" 1 11 0 "" "dynValLoc" "dynDescripLoc" ? "" "201911223481304359160"
31 "APIStatus" "System" "API Status" "Logical" "yes/no" "Radio-Set" "All,?,Success,Yes,Failure,No" "yes" 1 34 0 "" "" "" "HORIZONTAL" "" "202001234577904610545"
32 "APIInboundRoute" "System" "API Inbound Route" "Character" "x(32)" "Fill-In" "" "" 1 50 0 "" "" "" ? "" "202002044850904639774"
33 "ShowHistory" "System" "Show History" "Logical" "yes/no" "Toggle-Box" "" "no" 1 17 0 "" "" "" ? "" "202002066526704654728"
34 "AsOfDateOption" "System" "As Of Date Option" "Character" "x(8)" "Radio-Set" "Current,Current,Prior Month,Prior Month,Prior Year,Prior Year" "Current" 1 41 0 "" "" "" "HORIZONTAL" "" "202003095686704702280"
35 "IncludeZeroPricePer" "System" "Include Zero Price Per" "Logical" "yes/no" "Toggle-Box" "" "yes" 1 24 0 "" "" "" ? "" "202003257004304706946"
36 "EstTypeID" "System" "Estimate Type ID" "Character" "x(30)" "Combo-Box" "" "" 1 30 0 "dynInitEstTypeID" "" "" "LIST-ITEM-PAIRS" "" "202004095310904708491"
37 "EstType" "System" "Estimate Type" "Integer" "9" "Combo-Box" "," "5" 1 30 5 "dynInitEstTypeCorr" "" "" "LIST-ITEM-PAIRS" "" "202004095318404708492"
38 "release" "System" "Release" "Integer" ">>>>>>9" "Fill-In" "" "" 1 14 0 "" "" "" ? "" "202004236061005124137"
39 "UseImportFile" "System" "Use Import File" "Logical" "yes/no" "Toggle-Box" "" "no" 1 18 0 "" "" "" ? "" "202004236099005124147"
40 "ImportType" "System" "Import Type" "Character" "x(8)" "Radio-Set" "Orders,Orders,Releases,Releases" "Orders" 1 24 0 "" "" "" "HORIZONTAL" "" "202004236108705124148"
41 "ImportFile" "System" "Import File" "Character" "x(40)" "Fill-In" "" "" 1 50 0 "" "dynValFileName" "" ? "" "202004236114305124149"
42 "SalesGroup" "System" "Sales Group" "Character" "x(3)" "Fill-In" "" "" 1 7 0 "" "" "" ? "" "202004287504805172008"
43 "Currency" "System" "Currency" "Character" "x(3)" "Fill-In" "" "" 1 8 0 "" "" "" ? "" "202007228613905465165"
44 "Terms" "System" "Terms" "Character" "x(5)" "Fill-In" "" "" 1 11 0 "" "" "" ? "" "202007228620105465166"
45 "ARClass" "System" "AR Class" "Integer" "99" "Fill-In" "" "99" 1 4 0 "" "dynValARClass" "dynDescripARClass" ? "" "202007230013705465167"
46 "RecentTrendDays" "System" "Recent Trend Days" "Integer" ">>9" "Fill-In" "" "" 1 6 0 "" "" "" ? "" "202007230097305465168"
47 "PeriodDays" "System" "Period Days" "Integer" ">,>>9" "Fill-In" "" "" 1 9 0 "" "" "" ? "" "202007230103805465169"
48 "sbID" "System" "Schedule Board" "Character" "x(40)" "Selection-List" "" "" 2.95 30 0 "" "" "" ? "" "202008157947805472999"
50 "HowToUse" "System" "Uses?" "Character" "" "Fill-In" "" "" 1 12 0 "" "" "" ? "" "202101265903006058375"
51 "SpecNote" "System" "Spec Note" "Character" "x(3)" "Fill-In" "" "" 1 14 0 "" "" "" ? "" "202102154570706046399"
52 "UnderValue" "System" "Under Value" "Integer" ">9" "Fill-In" "" "" 1 4 0 "" "" "" ? "" "202102185497006061947"
53 "OverValue" "System" "Over Value" "Integer" ">9" "Fill-In" "" "" 1 4 0 "" "" "" ? "" "202102185501706061948"
54 "VendorType" "System" "Vendor Type" "Character" "x(8)" "Fill-In" "" "" 1 14 0 "" "" "" ? "" "202103097312606073722"
55 "OnlyShowQOHOlderThanDays" "System" "Only Show QOH Older Than Days" "Integer" ">>9" "Fill-In" "" "0" 1 14 0 "" "" "" ? "" "202107074574107541370"
56 "InvoiceID" "System" "Invoice ID" "Integer" ">>>>>>>9" "Fill-In" "" "" 1 14 0 "" "" "" ? "" "202107163546507787476"
58 "Company" "System" "Company" "Character" "x(3)" "Combo-Box" "" "" 1 10 5 "dynInitCompanyList" "" "" ? "" "202107285207907825775"
59 "Selection-List" "System" "Selection List" "Character" "x(30)" "Selection-List" "" "" 1 14 10 "dynInitNK1AutoPDC" "" "" ? "" "202108207471607910289"
60 "Button" "System" "Button" "Character" "" "Button" "" "" 1 14 0 "" "" "" ? "" "202111108044708194139"
61 "dmiID" "System" "DMI ID" "Integer" ">>9" "Fill-In" "" "" 1 14 0 "" "" "" ? "" "202203114563809079479"
62 "ActiveStatus" "System" "Status" "Character" "x(8)" "Radio-Set" "Active,A,Inactive,I,Inhouse,X,E-Service,E,All,All" "All" 1 57 0 "" "" "" "HORIZONTAL" "" "20221017_16:09:37_10234098"
