define input parameter appId   as character no-undo.
define input parameter setToolbar as logical no-undo.
define variable answer as logical no-undo.
define variable handle as widget no-undo.





run ip0.
/* ============================================================== */
procedure ip0.
run adeshar/_maddm.p("Results", "&Query", "", "aderes/_mstate.p", output answer).


run adeshar/_maddm.p("Results", "&Table", "", "aderes/_mstate.p", output answer).


run adeshar/_maddm.p("Results", "&Field", "", "aderes/_mstate.p", output answer).


run adeshar/_maddm.p("Results", "&Data", "", "aderes/_mstate.p", output answer).


run adeshar/_maddm.p("Results", "&Options", "", "aderes/_mstate.p", output answer).


run adeshar/_maddm.p("Results", "&View", "", "aderes/_mstate.p", output answer).


run adeshar/_maddm.p("Results", "&Help", "", "aderes/_mstate.p", output answer).


run adeshar/_maddi.p("Results", "NewSubMenu", "&Query", "&New", "SubMenu", no, "", output answer).


run adeshar/_maddi.p("Results", "NewBrowseView", "&New", "&Browse...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "NewReportView", "&New", "&Report...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "NewFormView", "&New", "&Form...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "NewLabelView", "&New", "&Label...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "NewExportView", "&New", "&Export...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "FileOpen", "&Query", "&Open...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "FileSave", "&Query", "&Save", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "FileSaveAs", "&Query", "Save &As...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "FileClose", "&Query", "&Close", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "FileDelete", "&Query", "&Delete...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "Generate", "&Query", "&Generate...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "----------------", "&Query", "- ---------------", "MenuSeperator", no, "", output answer).


run adeshar/_maddi.p("Results", "PrintSubMenu", "&Query", "&Print", "SubMenu", no, "", output answer).


run adeshar/_maddi.p("Results", "PrintPrinter", "&Print", "To &Printer...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "PrintClip", "&Print", "To &Clipboard", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "PrintFile", "&Print", "To &File...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "PrintPreview", "&Query", "Print Pre&view", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "----------------", "&Query", "--  --------------", "MenuSeperator", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminSubMenu", "&Query", "Si&te Admin", "SubMenu", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminTableRelations", "Si&te Admin", "Table &Relationships...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminWhere", "Si&te Admin", "Ta&ble Data Selection...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminTableAlias", "Si&te Admin", "Table Alia&ses...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "----------------", "Si&te Admin", "---   -------------", "MenuSeperator", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminRebuild", "Si&te Admin", "&Application Rebuild...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminSecurity", "Si&te Admin", "Feature Securit&y...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "----------------", "Si&te Admin", "----    ------------", "MenuSeperator", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminReportType", "Si&te Admin", "S&tandard Page...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminExportType", "Si&te Admin", "Standard E&xport...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminLabelType", "Si&te Admin", "Standard &Label...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminLabelField", "Si&te Admin", "Label &Field Selection...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "ProgrammingSubMenu", "&Query", "C&ustomize", "SubMenu", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminFeatures", "C&ustomize", "&Features...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminMenuEdit", "C&ustomize", "&Menu Layout...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminToolbarEdit", "C&ustomize", "&Tool Bar Layout...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "----------------", "C&ustomize", "-----     -----------", "MenuSeperator", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminIntegration", "C&ustomize", "&Integration Procedures...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminDeployment", "C&ustomize", "&Deployment...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminReset", "C&ustomize", "&Reset...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "----------------", "C&ustomize", "------      ----------", "MenuSeperator", no, "", output answer).


run adeshar/_maddi.p("Results", "AdminCustomizeInterface", "C&ustomize", "&Preferences...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "----------------", "&Query", "-------       ---------", "MenuSeperator", no, "", output answer).


run adeshar/_maddi.p("Results", "Exit", "&Query", "E&xit", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "Tables", "&Table", "Add/Remove &Tables...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "Joins", "&Table", "&Relationship Types...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "Fields", "&Field", "Add/Remove &Fields...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "----------------", "&Field", "--------        --------", "MenuSeperator", no, "", output answer).


run adeshar/_maddi.p("Results", "CalcSubMenu", "&Field", "Add &Calculated Field...", "SubMenu", no, "", output answer).


run adeshar/_maddi.p("Results", "PercentTotal", "Add &Calculated Field...", "&Percent of Total...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "RunningTotal", "Add &Calculated Field...", "&Running Total...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "Counter", "Add &Calculated Field...", "&Counter...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "Array", "Add &Calculated Field...", "Stacked &Array...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "Lookup", "Add &Calculated Field...", "Loo&kup...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "Math", "Add &Calculated Field...", "&Math...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "StringFunc", "Add &Calculated Field...", "&String Function...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "NumericFunc", "Add &Calculated Field...", "&Numeric Function...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "DateFunc", "Add &Calculated Field...", "&Date Function...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "LogicalFunc", "Add &Calculated Field...", "&Logical Function...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "Totals", "&Field", "&Aggregates...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "FieldProperties", "&Field", "&Properties...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "Selection", "&Data", "&Selection...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "ReAsk", "&Data", "&Re-ask Questions...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "SortOrdering", "&Data", "Sort &Ordering...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "Governor", "&Data", "&Governor...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "Information", "&Options", "&Query Information...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "HeadersAndFooters", "&Options", "&Headers and Footers...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "MasterDetail", "&Options", "&Master-Detail...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "FrameProperties", "&Options", "&Frame Properties...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "TotalsOnly", "&Options", "&Totals Only Summary...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "----------------", "&Options", "---------         -------", "MenuSeperator", no, "", output answer).


run adeshar/_maddi.p("Results", "StandardReport", "&Options", "Standard &Page...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "CustomReport", "&Options", "&Custom Page...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "PageBreak", "&Options", "Page &Break...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "----------------", "&Options", "----------          ------", "MenuSeperator", no, "", output answer).


run adeshar/_maddi.p("Results", "StandardLabel", "&Options", "&Standard Label...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "CustomLabel", "&Options", "Custom &Label...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "----------------", "&Options", "-----------           -----", "MenuSeperator", no, "", output answer).


run adeshar/_maddi.p("Results", "StandardExport", "&Options", "Standard &Export...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "ExportSubMenu", "&Options", "Custom E&xport", "SubMenu", no, "", output answer).


run adeshar/_maddi.p("Results", "OutputHeaderRecord", "Custom E&xport", "&Output Header Record", "MenuTogg", no, "", output answer).


run adeshar/_maddi.p("Results", "FixedWidthFields", "Custom E&xport", "&Fixed-width Fields", "MenuTogg", no, "", output answer).


run adeshar/_maddi.p("Results", "----------------", "Custom E&xport", "------------            ----", "MenuSeperator", no, "", output answer).


run adeshar/_maddi.p("Results", "RecordStart", "Custom E&xport", "Record &Start...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "RecordEnd", "Custom E&xport", "Record &End...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "FieldDelimiters", "Custom E&xport", "Field De&limiters...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "FieldSeparators", "Custom E&xport", "Field Se&parators...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "BrowseView", "&View", "As &Browse", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "ReportView", "&View", "As &Report", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "FormView", "&View", "As &Form", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "LabelView", "&View", "As &Label", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "ExportView", "&View", "As &Export", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "----------------", "&View", "-------------             ---", "MenuSeperator", no, "", output answer).


run adeshar/_maddi.p("Results", "ToolBar", "&View", "&Tool Bar", "MenuTogg", no, "", output answer).


end procedure.



run ip1.
/* ============================================================== */
procedure ip1.
run adeshar/_maddi.p("Results", "StatusLine", "&View", "&Status Line", "MenuTogg", no, "", output answer).


run adeshar/_maddi.p("Results", "HelpTopics", "&Help", "&Help Topics", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "----------------", "&Help", "--------------              --", "MenuSeperator", no, "", output answer).


run adeshar/_maddi.p("Results", "Messages", "&Help", "M&essages...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "RecentMessages", "&Help", "&Recent Messages...", "MenuItem", no, "", output answer).


run adeshar/_maddi.p("Results", "----------------", "&Help", "---------------               -", "MenuSeperator", no, "", output answer).


run adeshar/_maddi.p("Results", "AboutResults", "&Help", "&About RESULTS...", "MenuItem", no, "", output answer).


run adeshar/_maddt.p("Results", "NewDuplicateView", "ResultsToolbar", "adeicon/new-u", "","", 1, 1, 25, 25, "TbItem", no, "", output answer).


run adeshar/_maddt.p("Results", "FileOpen", "ResultsToolbar", "adeicon/open-u", "","", 26, 1, 25, 25, "TbItem", no, "", output answer).


run adeshar/_maddt.p("Results", "FileSave", "ResultsToolbar", "adeicon/save-u", "","", 51, 1, 25, 25, "TbItem", no, "", output answer).


run adeshar/_maddt.p("Results", "PrintPrinterNoBox", "ResultsToolbar", "adeicon/print-u", "","", 76, 1, 25, 25, "TbItem", no, "", output answer).


run adeshar/_maddt.p("Results", "PrintPreview", "ResultsToolbar", "adeicon/prevw-u", "","", 101, 1, 25, 25, "TbItem", no, "", output answer).


run adeshar/_maddt.p("Results", "Selection", "ResultsToolbar", "adeicon/select-u", "","", 151, 1, 25, 25, "TbItem", no, "", output answer).


run adeshar/_maddt.p("Results", "SortOrdering", "ResultsToolbar", "adeicon/sort-u", "","", 176, 1, 25, 25, "TbItem", no, "", output answer).


run adeshar/_maddt.p("Results", "BrowseView", "ResultsToolbar", "adeicon/browse-u", "","", 226, 1, 25, 25, "TbItem", no, "", output answer).


run adeshar/_maddt.p("Results", "ReportView", "ResultsToolbar", "adeicon/rpt-u", "","", 251, 1, 25, 25, "TbItem", no, "", output answer).


run adeshar/_maddt.p("Results", "FormView", "ResultsToolbar", "adeicon/form-u", "","", 276, 1, 25, 25, "TbItem", no, "", output answer).


run adeshar/_maddt.p("Results", "LabelView", "ResultsToolbar", "adeicon/labels-u", "","", 301, 1, 25, 25, "TbItem", no, "", output answer).


run adeshar/_maddt.p("Results", "ExportView", "ResultsToolbar", "adeicon/export-u", "","", 326, 1, 25, 25, "TbItem", no, "", output answer).


end procedure.
run adeshar/_mupdatm.p(appId) .
if setToolbar then run adeshar/_mupdatt.p(appId) .
