define input parameter appId   as character no-undo.
define variable answer as logical no-undo.
define variable handle as widget no-undo.





run ip0.
/* ============================================================== */
procedure ip0.
run adeshar/_maddf.p("Results", "AboutResults", "MenuItem", "aderes/_dspfunc.p", "RunHelp,aboutres", "&About RESULTS...", "","","  ","Information About This Product", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminCustomizeInterface", "MenuItem", "aderes/_dspfunc.p", "RunDirty,_acust.p", "&Preferences...", "","","  ","Customize User Interface Components", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminDeployment", "MenuItem", "aderes/_dspfunc.p", "RunSingle,_adeploy.p", "&Preferences...", "","","  ","Manage Deployment Files", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminExportType", "MenuItem", "aderes/_dspfunc.p", "RunDirty,e-type.p,1", "Standard E&xport...", "","","  ","Choose a Standard Export Format", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminFeatures", "MenuItem", "aderes/_dspfunc.p", "RunSingle,_afeat.p", "&Features...", "","","  ","Create and Modify VAR defined Features", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminIntegration", "MenuItem", "aderes/_dspfunc.p", "RunSingle,_aint.p", "&Preferences...", "","","  ","Set Integration Procedures", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminLabelField", "MenuItem", "aderes/_dspfunc.p", "RunSingle,_alabel.p", "Label &Field Selection...", "","","  ","Select Fields for Label Layout", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminLabelType", "MenuItem", "aderes/_dspfunc.p", "RunDirty,l-type.p,1", "Standard &Label...", "","","  ","Choose a Standard Label Size", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminMenuEdit", "MenuItem", "aderes/_dspfunc.p", "RunSingle,_amenu.p", "&Menu Layout...", "","","  ","Create and Modify Menu Layout", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminProgInitialize", "MenuItem", "aderes/_dspfunc.p", "RunLogicalInput,s-zap.p,false", "AP Initialize", "","","  ","Initialize the Variables", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminProgInstantiate", "MenuItem", "aderes/_dspfunc.p", "RunSingle,s-level.p", "AP Instantiate", "","","  ","Instantiate the Query", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminProgWrite4GL", "MenuItem", "aderes/_dspfunc.p", "RunTriple,_a4gl.p,test.p,g", "AP Write4GL", "","","  ","Create a .p Based on the Query", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminRebuild", "MenuItem", "aderes/_dspfunc.p", "RunSingle,_abuild.p", "&Application Rebuild...", "","","  ","Rebuild Configuration, Directory, Query Files", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminReportType", "MenuItem", "aderes/_dspfunc.p", "RunDirty,p-type.p,1", "S&tandard Page...", "","","  ","Choose a Standard Page Size", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminReset", "MenuItem", "aderes/_dspfunc.p", "RunSingle,_areset.p", "&Reset...", "","","  ","Delete Existing Features, Menus, Tool Bars", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminSecurity", "MenuItem", "aderes/_dspfunc.p", "RunSingle,_asecure.p", "Feature Securit&y...", "","","  ","Set Security for Features", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminSubMenu", "SubMenu", "", "", "Si&te Admin", "","","  ","Submenu to Organize Site Adminstration Options", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminTableAlias", "MenuItem", "aderes/_dspfunc.p", "RunSingle,_atalias.p", "Table Alia&ses...", "","","  ","Create and Delete Table Aliases", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminTableRelations", "MenuItem", "aderes/_dspfunc.p", "RunSingle,_arships.p", "Table &Relationships...", "","","  ","Edit Relationships Between Tables", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminToolbarEdit", "MenuItem", "aderes/_dspfunc.p", "RunSingle,_atool.p", "&Tool Bar Layout...", "","","  ","Create and Modify Tool Bar Layout", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "AdminWhere", "MenuItem", "aderes/_dspfunc.p", "RunDirty,_awhere.p", "Ta&ble Data Selection...", "","","  ","Create and Edit Site Restrictions", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "Array", "MenuItem", "aderes/_dspfunc.p", "ipFields,e", "Stacked &Array...", "","","  ","Add an Array Field As a Stacked Array", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "BrowseView", "MenuItem", "aderes/_dspfunc.p", "ViewAs,b", "As &Browse", "adeicon/browse-u","","  ","Switch to Browse View", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "CalcSubMenu", "SubMenu", "", "", "Add &Calculated Field...", "","","  ","Submenu to Organize Calculated Fields", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "Counter", "MenuItem", "aderes/_dspfunc.p", "ipFields,c", "&Counter...", "","","  ","Create a Counter Calculated Field", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "CustomLabel", "MenuItem", "aderes/_dspfunc.p", "RunDirty,l-layout.p", "Custom &Label...", "","","  ","Create a Customized Label", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "CustomReport", "MenuItem", "aderes/_dspfunc.p", "RunDirty,r-layout.p", "&Custom Page...", "","","  ","Define Custom Page Sizes", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "DateFunc", "MenuItem", "aderes/_dspfunc.p", "ipFields,d", "&Date Function...", "","","  ","Create a Date Based Calculated Field", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "Exit", "MenuItem", "aderes/_dspfunc.p", "Exit,", "E&xit", "","","  ","Exit the Product", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "ExportSubMenu", "SubMenu", "", "", "Custom E&xport", "","","  ","Submenu to Organize Custom Exports", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "ExportView", "MenuItem", "aderes/_dspfunc.p", "ViewAs,e", "As &Export", "adeicon/export-u","","  ","Switch to Export View", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "FieldDelimiters", "MenuItem", "aderes/_dspfunc.p", "Custom,f", "Field De&limiters...", "","","  ","Choose the Export Field Delimiters", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "FieldProperties", "MenuItem", "aderes/_dspfunc.p", "RunDirty,y-format.p,qbf-index", "&Properties...", "","","  ","Define Field Properties", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "Fields", "MenuItem", "aderes/_dspfunc.p", "RunDirty,y-field.p", "Add/Remove &Fields...", "","","  ","Modify Fields in the Query", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "FieldSeparators", "MenuItem", "aderes/_dspfunc.p", "Custom,s", "Field Se&parators...", "","","  ","Choose the Export Field Separators", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "FileClose", "MenuItem", "aderes/_dspfunc.p", "ChooseModule,?", "&Close", "","","  ","Close the Query", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "FileDelete", "MenuItem", "aderes/_dspfunc.p", "RunSingle,i-zap.p,", "&Delete...", "","","  ","Delete Queries from a Query Directory", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "FileOpen", "MenuItem", "aderes/_dspfunc.p", "FileOpen,", "&Open...", "adeicon/open-u","","  ","Open an Existing Query", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "FileSave", "MenuItem", "aderes/_dspfunc.p", "RunDirty,i-opnsav.p,1", "&Save", "adeicon/save-u","","  ","Save the Query", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "FileSaveAs", "MenuItem", "aderes/_dspfunc.p", "RunDirty,i-opnsav.p,2", "Save &As...", "","","  ","Save the Query with a Different Name", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "FixedWidthFields", "MenuItem", "aderes/_dspfunc.p", "Headers,f", "&Fixed-width Fields", "","","  ","Toggle Fixed Width Columns for Exports", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "FormView", "MenuItem", "aderes/_dspfunc.p", "ViewAs,f", "As &Form", "adeicon/form-u","","  ","Switch to Form View", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "FrameProperties", "MenuItem", "aderes/_dspfunc.p", "RunDirty,_fbprop.p", "&Frame Properties...", "","","  ","Form and Browse View Frame Properties", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "Generate", "MenuItem", "aderes/_dspfunc.p", "ChooseModule,%", "&Generate...", "","","  ","Create 4GL Code Based on the View", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "Governor", "MenuItem", "aderes/_dspfunc.p", "RunDirty,_governr.p", "&Governor...", "","","  ","Limit Number of Records Selected", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "HeadersAndFooters", "MenuItem", "aderes/_dspfunc.p", "RunDirty,r-header.p", "&Headers and Footers...", "","","  ","Define Report Headers and Footers", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "HelpContents", "MenuItem", "aderes/_dspfunc.p", "RunHelp,contents", "&Contents", "","","  ","Display the Table of Contents for Help", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "HelpSearch", "MenuItem", "aderes/_dspfunc.p", "RunHelp,search", "&Search for Help On...", "","","  ","Search Help for a Specific Topic", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "HelpTopics", "MenuItem", "aderes/_dspfunc.p", "RunHelp,topics", "&Help Topics", "","","  ","Access Help Contents, Index, and Search Options", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "HowTo", "MenuItem", "aderes/_dspfunc.p", "RunHelp,howto", "&How To", "","","  ","Information On How To Use This Product", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "Includes", "MenuItem", "aderes/_dspfunc.p", "RunDirty,s-inc.p", "&Include Files...", "","","  ","Include Files", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "Information", "MenuItem", "aderes/_dspfunc.p", "RunSingle,s-info.p", "&Query Information...", "","","  ","View Information About the Current Query", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "Joins", "MenuItem", "aderes/_dspfunc.p", "RunDirty,s-join.p", "&Relationship Types...", "","","  ","Set Record Inclusion Behavior", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "LabelView", "MenuItem", "aderes/_dspfunc.p", "ViewAs,l", "As &Label", "adeicon/labels-u","","  ","Switch to Label View", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "LogicalFunc", "MenuItem", "aderes/_dspfunc.p", "ipFields,l", "&Logical Function...", "","","  ","Create a Logical Variable Calculated Field", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "Lookup", "MenuItem", "aderes/_dspfunc.p", "ipFields,x", "Loo&kup...", "","","  ","Create a Lookup Calculated Field", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "MasterDetail", "MenuItem", "aderes/_dspfunc.p", "RunDirty,r-level.p", "&Master-Detail...", "","","  ","Set the Master-Detail Relationship", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "Math", "MenuItem", "aderes/_dspfunc.p", "ipFields,m", "&Math...", "","","  ","Create an Arithmetic Calculated Field", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "Messages", "MenuItem", "aderes/_dspfunc.p", "RunHelp,messages", "M&essages...", "","","  ","Lookup Standard Error Message", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "NewBrowseView", "MenuItem", "aderes/_dspfunc.p", "ChooseModule,b", "&Browse...", "","","  ","Create a New Browse View", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "NewDuplicateView", "MenuItem", "aderes/_dspfunc.p", "NewDupModule,", "&New (Same As Current)", "adeicon/new-u","","  ","Create a New View, Same Type as Current", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "NewExportView", "MenuItem", "aderes/_dspfunc.p", "ChooseModule,e", "&Export...", "","","  ","Create a New Export View", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "NewFormView", "MenuItem", "aderes/_dspfunc.p", "ChooseModule,f", "&Form...", "","","  ","Create a New Form View", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "NewLabelView", "MenuItem", "aderes/_dspfunc.p", "ChooseModule,l", "&Label...", "","","  ","Create a New Label View", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "NewReportView", "MenuItem", "aderes/_dspfunc.p", "ChooseModule,r", "&Report...", "","","  ","Create a New Report View", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "NewSubMenu", "SubMenu", "", "", "&New", "","","  ","Submenu to Organize New Views", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "NumericFunc", "MenuItem", "aderes/_dspfunc.p", "ipFields,n", "&Numeric Function...", "","","  ","Create a Numeric Calculated Field", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "OutputHeaderRecord", "MenuItem", "aderes/_dspfunc.p", "Headers,h", "&Output Header Record", "","","  ","Create Header Description for Custom Export", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "PageBreak", "MenuItem", "aderes/_dspfunc.p", "NewPage,", "Page &Break...", "","","  ","Set a Page Break in a Report", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "PercentTotal", "MenuItem", "aderes/_dspfunc.p", "ipFields,p", "&Percent of Total...", "","","  ","Create a Percent of Total Calculated Field", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "PrintClip", "MenuItem", "aderes/_dspfunc.p", "PrintClip,", "To &Clipboard", "","","  ","Send Results of Query to the Clipboard", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "PrintFile", "MenuItem", "aderes/_dspfunc.p", "PrintToFile,", "To &File...", "","","  ","Send Results of Query to a File", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "PrintPreview", "MenuItem", "aderes/_dspfunc.p", "PrintPreview,", "Print Pre&view", "adeicon/prevw-u","","  ","Check the Results of the Query", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "PrintPrinter", "MenuItem", "aderes/_dspfunc.p", "PrintToPrinter,1", "To &Printer...", "","","  ","Send Results of Query to the Default Printer", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "PrintPrinterNoBox", "MenuItem", "aderes/_dspfunc.p", "PrintToPrinter,0", "To &Printer", "adeicon/print-u","","  ","Send Results of Query to the Default Printer", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "PrintSubMenu", "SubMenu", "", "", "&Print", "","","  ","Submenu to Organize Print Options", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "ProgrammingSubMenu", "SubMenu", "", "", "C&ustomize", "","","  ","Submenu to Organize Programming Options", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "ReadOtherDirectory", "MenuItem", "", "", "", "","","  ","Read Another Directory File", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "ReadPublicDirectory", "MenuItem", "", "", "", "","","  ","Read Public Directory File", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "ReAsk", "MenuItem", "aderes/_dspfunc.p", "RunDirty,_reask.p", "&Re-ask Questions...", "","","  ","Provide Answers for variables", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "RecentMessages", "MenuItem", "aderes/_dspfunc.p", "RunHelp,recentmsgs", "&Recent Messages...", "","","  ","Display Recent Error Messages", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "RecordAdd", "MenuItem", "", "", "", "","","  ","Add a Record in a Form View", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "RecordDelete", "MenuItem", "", "", "", "","","  ","Delete a Record in a Form View", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "RecordEnd", "MenuItem", "aderes/_dspfunc.p", "Custom,l", "Record &End...", "","","  ","Choose the Export Record End Codes", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "RecordStart", "MenuItem", "aderes/_dspfunc.p", "Custom,b", "Record &Start...", "","","  ","Choose the Export Record Start Codes", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "RecordUpdate", "MenuItem", "", "", "", "","","  ","Update a Record in a Form View", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "ReportView", "MenuItem", "aderes/_dspfunc.p", "ViewAs,r", "As &Report", "adeicon/rpt-u","","  ","Switch to Report View", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "RunningTotal", "MenuItem", "aderes/_dspfunc.p", "ipFields,r", "&Running Total...", "","","  ","Create a Running Total Calculated Field", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "Selection", "MenuItem", "aderes/_dspfunc.p", "RunDirty,s-where.p", "&Selection...", "adeicon/select-u","","  ","Add Restrictions to a Query", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "SortOrdering", "MenuItem", "aderes/_dspfunc.p", "RunDirty,y-sort.p", "Sort &Ordering...", "adeicon/sort-u","","  ","Create and Modify the Ordering of a Query", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "StandardExport", "MenuItem", "aderes/_dspfunc.p", "RunDirty,e-type.p,0", "Standard &Export...", "","","  ","Choose a Standard Export Format", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "StandardLabel", "MenuItem", "aderes/_dspfunc.p", "RunDirty,l-type.p,0", "&Standard Label...", "","","  ","Choose a Standard Label Size", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "StandardReport", "MenuItem", "aderes/_dspfunc.p", "RunDirty,p-type.p,0", "Standard &Page...", "","","  ","Choose a Standard Page Size", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "StatusLine", "MenuItem", "aderes/_dspfunc.p", "ToolView,s", "&Status Line", "","","  ","Turn the Status Line On or Off", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "StringFunc", "MenuItem", "aderes/_dspfunc.p", "ipFields,s", "&String Function...", "","","  ","Create a String Based Calculated Field", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "Tables", "MenuItem", "aderes/_dspfunc.p", "RunDirty,y-table.p", "Add/Remove &Tables...", "","","  ","Modify Tables Active in the Query", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "ToolBar", "MenuItem", "aderes/_dspfunc.p", "ToolView,t", "&Tool Bar", "","","  ","Turn the Tool Bar On or Off", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "Totals", "MenuItem", "aderes/_dspfunc.p", "RunDirty,y-total.p", "&Aggregates...", "","","  ","Define Aggregates for the Query", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "TotalsOnly", "MenuItem", "aderes/_dspfunc.p", "RunDirty,r-short.p", "&Totals Only Summary...", "","","  ","Specify Totals for Fields in a Query", no, "", "*", output answer).


run adeshar/_maddf.p("Results", "WriteOtherDirectory", "MenuItem", "", "", "", "","","  ","Write Another Directory File", no, "", "*", output answer).


end procedure.



run ip1.
/* ============================================================== */
procedure ip1.
run adeshar/_maddf.p("Results", "WritePublicDirectory", "MenuItem", "", "", "", "","","  ","Write Public Directory File", no, "", "*", output answer).


end procedure.
run adeshar/_mupdatm.p(appId) .
