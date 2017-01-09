/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/ 
 /*------------------------------------------------------------------------
    File        : SmartKendoWebGrid
    Purpose     : Wrapper to the Telerik Kendo UI Grid
    Syntax      : 
    Description : Executes data requests in the seperate WebSpeed call
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 12 18:26:03 CEST 2014
    Notes       : The Kendo UI JavaScript Libraries are expected to be 
                  deployed on the same web server that also hosts the 
                  WebSpeed messenger executable under the following URI:
                  /KendoUI/js/kendo.all.min.js or /KendoUI/styles/kendo.common.min.css
                  etc.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*           FROM PROPATH . 
USING Consultingwerk.Web.*            FROM PROPATH .
USING Consultingwerk.Web.Kendo.Data.* FROM PROPATH .
USING Consultingwerk.Web.Kendo.Grid.* FROM PROPATH .
USING Consultingwerk.Web.Widgets.*    FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

&SCOPED-DEFINE UrlBase /KendoUI/
&SCOPED-DEFINE JsLibraries {&UrlBase}js/jquery.min.js,{&UrlBase}js/angular.min.js,{&UrlBase}js/kendo.all.min.js
&SCOPED-DEFINE CssLibraries {&UrlBase}styles/kendo.common.min.css,{&UrlBase}styles/kendo.rtl.min.css,{&UrlBase}styles/kendo.default.min.css,{&UrlBase}styles/kendo.dataviz.min.css,{&UrlBase}styles/kendo.dataviz.default.min.css

CLASS Consultingwerk.Web.Widgets.SmartKendoWebGrid 
    INHERITS Control
    IMPLEMENTS IControlWithCustomJavaScript, IControlWithCustomStyleSheet: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of BrowserColumns for this Browser                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Columns AS Consultingwerk.Web.Widgets.ListGridColumn NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the comma delimited list of JavaScript libraries that 
                 need to be outputted for this Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CustomJavaScriptLibraries AS CHARACTER NO-UNDO INIT "{&JsLibraries}":U
    GET.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the comma delimited list of stlye sheet libraries that 
                 need to be outputted with the Page 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CustomStyleLibraries AS CHARACTER NO-UNDO INIT "{&CssLibraries}":U
    GET.
        
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the Data Source for the SmartKendoWebGrid
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY DataSource AS Consultingwerk.Web.Data.JsonDataSource NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the handle of the Query used by the SmartWebGrid                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Query AS HANDLE NO-UNDO 
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the title of the Grid                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY GridTitle AS CHARACTER NO-UNDO 
    GET.
    SET.  

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartWebGrid class                                                                       
        Notes:                             
        @param pcName The name of the Control                                           
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartKendoWebGrid (pcName AS CHARACTER):
        SUPER (pcName).
        
        THIS-OBJECT:Columns = NEW ListGridColumn () . 
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Performs the Output for the Web Widget                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID PerformOutput ():
        
        DEFINE VARIABLE lAlternate        AS LOGICAL   NO-UNDO INIT FALSE.
        DEFINE VARIABLE pcColumnName      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcInitColumnWidth AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcColSorting      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcColType         AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcColAlign        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE plcDataXML        AS LONGCHAR  NO-UNDO.
        DEFINE VARIABLE lRowTrigger       AS LOGICAL   NO-UNDO INIT FALSE. 
        
        DEFINE VARIABLE iColumn           AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cColumnName       AS CHARACTER NO-UNDO INIT "":U.
        DEFINE VARIABLE oColumn           AS Column    NO-UNDO .
            
        DEFINE VARIABLE oKendoGrid        AS KendoGrid NO-UNDO . 
        DEFINE VARIABLE lcJson            AS LONGCHAR  NO-UNDO .
        
        oKendoGrid = NEW Consultingwerk.Web.Kendo.Grid.KendoGrid () . 
                
        oKendoGrid:height = 500 .
        oKendoGrid:filterable = NEW Filterable ("menu":U) . 
        oKendoGrid:sortable = TRUE .
        oKendoGrid:resizable = TRUE . 
        oKendoGrid:reorderable = TRUE .                    
                
        IF VALID-OBJECT (THIS-OBJECT:DataSource) THEN DO:
            oKendoGrid:dataSource:type = "json":U .            
            oKendoGrid:dataSource:pageSize = 20 .      
            
            oKendoGrid:dataSource:serverPaging = TRUE . 
            oKendoGrid:dataSource:serverFiltering = TRUE . 
            oKendoGrid:dataSource:serverSorting = TRUE .
            
            oKendoGrid:dataSource:transport = NEW Transport () . 
            
            oKendoGrid:dataSource:transport:read = WebUtilities:UrlToDataObject (THIS-OBJECT:DataSource:GetClass()) .
            oKendoGrid:dataSource:transport:dataType = "json":U .
            
            oKendoGrid:dataSource:schema = NEW Schema (THIS-OBJECT:DataSource:EntityTable) .
        END.
    
        {Consultingwerk/foreachABL.i GridColumn oGridColumn in THIS-OBJECT:Columns}        
            
            oColumn = oKendoGrid:Columns:Add (oGridColumn:BufferField:Name) .
            oColumn:filterable = TRUE . 
            oColumn:sortable = TRUE .
            oColumn:width = "100px":U .            
            
            IF oGridColumn:ColumnLabel > "":U THEN  
                ASSIGN oColumn:title = oGridColumn:ColumnLabel . 
            ELSE 
                ASSIGN oColumn:title = oGridColumn:BufferField:Name . 
                
            oColumn:filterable = oGridColumn:Filterable .
            oColumn:sortable = oGridColumn:Sortable . 
                
            IF oGridColumn:ColumnAlign > "":U THEN DO:
                ASSIGN oColumn:attributes = NEW Attributes () .
                
                oColumn:attributes:style = SUBSTITUTE ("text-align:&1;":U, LC (oGridColumn:ColumnAlign)) .
            END.
            
            IF oGridColumn:Width > "":U THEN  
                oColumn:width = oGridColumn:Width .
                
            IF TYPE-OF (oGridColumn, IGridColumnWithCellTemplate) THEN 
                oColumn:template = CAST (oGridColumn, IGridColumnWithCellTemplate):ColumnTemplate . 
            ELSE 
                oColumn:template = ? .
            
        END. 
    
        IF NOT THIS-OBJECT:GridTitle = "":U THEN 
            THIS-OBJECT:OUTPUT (SUBSTITUTE ('<div id="&1_Title" class="SmartGridTitle"><a>&2</a></div>':U, THIS-OBJECT:Name, THIS-OBJECT:GridTitle)) .
        
        lcJson = oKendoGrid:Serialize () . 
    
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('   <div id="&1"></div>~n':U +
                                        '            <script>~n':U +
                                        '                $(document).ready(function() ~{~n':U +
                                        '                    $("#&1").kendoGrid(~n':U, THIS-OBJECT:Name), FALSE) .

        THIS-OBJECT:Output (STRING(lcJson)) .

        THIS-OBJECT:Output (');~n  });~n </script>~n </div>':U)  .

    END METHOD.

END CLASS.
