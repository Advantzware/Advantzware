/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : LinkGridColumn
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Apr 10 23:04:16 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web.Widgets.*          FROM PROPATH . 
USING Consultingwerk.Web.Widgets.Grid.*     FROM PROPATH . 
USING Progress.Lang.*                       FROM PROPATH .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Web.Widgets.LinkGridColumn 
    INHERITS GridColumn
    IMPLEMENTS IGridColumnWithCellTemplate: 

    /*------------------------------------------------------------------------------
        Purpose: Raised 
        Notes:   
        @param sender The object that raised the BuildImageUrl event
        @param e The BuildUrlEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BuildImageUrl SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                      e AS BuildUrlEventArgs).
    
    /*------------------------------------------------------------------------------
        Purpose: Raised to allow callbacks to modify the generated hyperlink url
        Notes:   
        @param sender The object that raised the BuildUrl event
        @param e The BuildUrlEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BuildUrl SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                 e AS BuildUrlEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the alternative image text                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AltText AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the column display type
        Notes:   Use Text, Image or ImageAndText  
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ColumnDisplayType AS ColumnDisplayTypeEnum NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the hyperlink used by this LinkGridColumn                                                                      
        Notes:   Use &1 for the current cell's screenvalue                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Hyperlink AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name (href) of the Image displayed by the LinkGridColumn                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Image AS CHARACTER NO-UNDO 
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets additional link attributes used by this LinkGridColumn           
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY LinkAttributes AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the href display text                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UrlDisplayText AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets additional image attributes used by this LinkGridColumn                                                                      
        Notes:                                                                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ImageAttributes AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Column Template (Kendo.UI) for Cells of the Column
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ColumnTemplate AS CHARACTER NO-UNDO 
    GET:
        DEFINE VARIABLE cOutput      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cDisplayText AS CHARACTER NO-UNDO.
        
        ASSIGN cDisplayText = SUBSTITUTE ("#: data.&1 #":U, THIS-OBJECT:Name) .
        
        CASE THIS-OBJECT:ColumnDisplayType:
            WHEN ColumnDisplayTypeEnum:Image THEN DO:
                ASSIGN cOutput = '<a href="&1" &5><img src="&2" alt="&3" &4 /></a>':U.
            END.
            WHEN ColumnDisplayTypeEnum:Text THEN DO:
                ASSIGN cOutput = '<a href="&1" &5>&6</a>':U.
                
            END.
            WHEN ColumnDisplayTypeEnum:ImageAndText THEN DO:
                ASSIGN cOutput = '<a href="&1" &5><img src="&2" alt="&3" &4 />&5</a>':U.
            END.
        END CASE.
        
        IF URLDisplayText <> "":U THEN 
            ASSIGN cDisplayText = URLDisplayText.
        
        RETURN SUBSTITUTE (cOutput,
                           SUBSTITUTE (THIS-OBJECT:Hyperlink, SUBSTITUTE ("#: data.&1 #":U, 
                                                                          THIS-OBJECT:Name)),
                           THIS-OBJECT:Image,
                           THIS-OBJECT:AltText,
                           THIS-OBJECT:ImageAttributes,
                           THIS-OBJECT:LinkAttributes,
                           cDisplayText) .
        
    END GET . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the LinkGridColumn class                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LinkGridColumn ():
        SUPER ().
        
        ASSIGN THIS-OBJECT:ColumnDisplayType = ColumnDisplayTypeEnum:Text.
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the LinkGridColumn class                                                                         
        Notes:                                                                        
        @param phBufferField The handle of the BufferField to bind to                           
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LinkGridColumn (phBufferField AS HANDLE):
        SUPER (INPUT phBufferField).
        
        ASSIGN ColumnDisplayType = ColumnDisplayTypeEnum:Text.
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Builds the URL for the Image
        Notes:   
        @return The Url used for the Image location
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER BuildImageUrl ():
        
        DEFINE VARIABLE cUrl AS CHARACTER         NO-UNDO.
        DEFINE VARIABLE e    AS BuildUrlEventArgs NO-UNDO . 
        
        ASSIGN cUrl = THIS-OBJECT:Image .

        e = NEW BuildUrlEventArgs (cUrl) .

        THIS-OBJECT:OnBuildImageUrl (e) .
        
        IF e:Url > "":U THEN 
            ASSIGN cUrl = e:Url .

        RETURN cUrl .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Build the hyperlink on the Image
        Notes:   
        @return The Url used for the Hyperlink on the Image
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER BuildUrl ():
        
        DEFINE VARIABLE cUrl AS CHARACTER         NO-UNDO.
        DEFINE VARIABLE e    AS BuildUrlEventArgs NO-UNDO . 
        
        ASSIGN cUrl = SUBSTITUTE (THIS-OBJECT:Hyperlink,
                                  THIS-OBJECT:ScreenValue) .

        e = NEW BuildUrlEventArgs (cUrl) .

        THIS-OBJECT:OnBuildUrl (e) .
        
        IF e:Url > "":U THEN 
            ASSIGN cUrl = e:Url .

        RETURN cUrl .

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Raises the BuildImageUrl
        Notes:   
        @param e The BuildUrlEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBuildImageUrl (e AS BuildUrlEventArgs):
            
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "BuildImageUrl":U) .
     
        THIS-OBJECT:BuildImageUrl:Publish (THIS-OBJECT, e) .
    
    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Raises the BuildUrl event
        Notes:   
        @param e The BuildUrlEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnBuildUrl (e AS BuildUrlEventArgs):
            
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "BuildUrl":U) .
     
        THIS-OBJECT:BuildUrl:Publish (THIS-OBJECT, e) .
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Outputs a cell for this column.                                                            
        Notes:   If the ColumnDisplayType equal 'Text'and if the URLDisplayText empty 
                 displays the Link the URL.                                                                        
        @return The output value for the cell                                                           
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE LONGCHAR OutputCell ():
        
        DEFINE VARIABLE cOutput      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cDisplayText AS CHARACTER NO-UNDO.
        
        CASE THIS-OBJECT:ColumnDisplayType:
            WHEN ColumnDisplayTypeEnum:Image THEN DO:
                ASSIGN cOutput = '<a href="&1" &5><img src="&2" alt="&3" &4 /></a>':U.
            END.
            WHEN ColumnDisplayTypeEnum:Text THEN DO:
                ASSIGN cOutput = '<a href="&1" &5>&6</a>':U.
                
            END.
            WHEN ColumnDisplayTypeEnum:ImageAndText THEN DO:
                ASSIGN cOutput = '<a href="&1" &5><img src="&2" alt="&3" &4 />&5</a>':U.
            END.
        END CASE.
        
        ASSIGN cDisplayText = THIS-OBJECT:BufferField:BUFFER-HANDLE:BUFFER-FIELD (THIS-OBJECT:NAME):BUFFER-VALUE.
        
        IF URLDisplayText <> "":U THEN 
            ASSIGN cDisplayText = URLDisplayText.
        
        RETURN SUBSTITUTE (cOutput,
                           THIS-OBJECT:BuildUrl (),
                           THIS-OBJECT:BuildImageUrl (),
                           THIS-OBJECT:AltText,
                           THIS-OBJECT:ImageAttributes,
                           THIS-OBJECT:LinkAttributes,
                           cDisplayText) .
    END METHOD.
    
END CLASS.
