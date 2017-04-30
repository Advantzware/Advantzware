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
    File        : IEmbeddedWindowTabFolderForm
    Purpose     : An Interface describing the methods required in WinKit
                  Forms that realize a TabFolder
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 15 13:11:13 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.WindowIntegrationKit.Forms.* FROM PROPATH .  
USING Progress.Lang.*                             FROM PROPATH .

INTERFACE Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowTabFolderForm
    &IF NOT PROVERSION BEGINS "10.2":U &THEN 
    INHERITS IEmbeddedWindowForm
    &ENDIF
    :  
  
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Splitter's distance
        Notes:   Should be set to match the position of the original ABL tabfolder                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SplitterDistance AS INTEGER NO-UNDO 
    GET.
    SET.  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the UltraTabControl used in this Form
        Notes:                                                                              
    ------------------------------------------------------------------------------*/ 
    DEFINE PUBLIC PROPERTY TabFolder AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO 
    GET. 
  
    /*------------------------------------------------------------------------------
        Purpose: Assigns the Labels of the Tabfolder based on a Character List                                                                        
        Notes:                                                  
        @param pcLabels A delimited list of tab labels
        @param pcDelimiter The list delimited, like the | (pipe) for ADM2 folder                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AssignTabLabels (pcLabels AS CHARACTER,
                                        pcDelimiter AS CHARACTER) .   
  
    /*------------------------------------------------------------------------------
        Purpose: Closes the dynamically created windows and WindowContainer objects                                                                       
        Notes:   Used by closewindow.i                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CloseTabs () .   
  
    /*------------------------------------------------------------------------------
        Purpose: Creates a Tab on the TabFolder with the given Label and key                                                                       
        Notes:                                                              
        @param pcTabText The label to use for the new tab
        @param piTabKey The 1 based number for the tab to create          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateTab (pcTabText AS CHARACTER,
                                  piTabKey AS INTEGER) .   
  
    /*------------------------------------------------------------------------------
        Purpose: Creates the Window and Frame used for folder pages
        Notes:   Creates ttFolderPages record                                                                     
        @param piPage The page number to create a window and frame for
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateTabPageFrame (piPage AS INTEGER) . 
    
    /*------------------------------------------------------------------------------
        Purpose: Creates Tabs for a list of Tabs 
        Notes:                                                                        
        @param pcLabels A delimited character list of tab labels
        @param pcDelimiter The character used as the delimiter of the list
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateTabs (pcLabels AS CHARACTER,
                                   pcDelimiter AS CHARACTER) .    
    
    /*------------------------------------------------------------------------------
        Purpose: Creates Tabs for a list of Tabs 
        Notes:   Uses the comma as the default delimiter    
        @param pcLabels A comma-charcater delimited list of tab labels
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateTabs (pcLabels AS CHARACTER) . 

     /*------------------------------------------------------------------------------
        Purpose: Deletes (removes) a Tab Page 
        Notes:   
        @param piPage The tab page to remove
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeleteTabPage (piPage AS INTEGER) .    
    
    /*------------------------------------------------------------------------------
        Purpose: Disables a folder Page                                                                     
        Notes:                          
        @param piPage The page number to disable (based on 1)                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisableFolderPage (piPage AS INTEGER) .     
    
    /*------------------------------------------------------------------------------
        Purpose: Enables a folder Page                                                                     
        Notes:                                                                        
        @param piPage The page number to enable (based on 1)                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnableFolderPage (piPage AS INTEGER) .      
    
    /*------------------------------------------------------------------------------
        Purpose: Fianlizes the Embedding of a single Tab Page                                                                     
        Notes:                                               
        @param piPage The Tab Page Number                         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FinalizeTabPage (piPage AS INTEGER) .     
      
    /*------------------------------------------------------------------------------
        Purpose: Returns the HANDLE of the ABL Frame that was created as the default 
                 frame for a tab page                                                                      
        Notes:                                                                        
        @param piPage The 1 based index of the tab page
        @return The HANDLE of the ABL Frame that was created as the default 
                frame for a tab page 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetTabPageFrame (piPage AS INTEGER) . 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the HANDLE of the ABL Window that was created for a tab page                                                                      
        Notes:                                                                        
        @param piPage The 1 based index of the tab page
        @return The HANDLE of the ABL Window that was created for a tab page 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetTabPageWindow (piPage AS INTEGER) . 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the UltraTab Control that was created for a tab page                                                                      
        Notes:                                                                        
        @param piPage The 1 based index of the tab page
        @return The reference to the UltraTab Control that was created for a tab page
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Windows.Forms.Control GetTabPageControl (piPage AS INTEGER) .     
        
    /*------------------------------------------------------------------------------
        Purpose: Returns the WindowContainer Control that was created for a tab page                                                                      
        Notes:                                                                        
        @param piPage The 1 based index of the tab page
        @return The reference to the WindowContainer Control that was created for a tab page
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Progress.Windows.WindowContainer GetTabPageWindowContainer (piPage AS INTEGER) .     
        
    /*------------------------------------------------------------------------------
        Purpose: Selects a TabPage                                                                     
        Notes:                    
        @param piPage The tab page number                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SelectTab (piPage AS INTEGER) .         
        
      
END INTERFACE.
