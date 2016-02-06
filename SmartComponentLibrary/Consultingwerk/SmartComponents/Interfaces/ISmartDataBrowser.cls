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
    File        : ISmartDataBrowser
    Purpose     : Interface to seperate calls from SmartDataAdpater to 
                  SmartDataBrowser
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 14 20:58:50 CET 2014
    Notes       : Preparation for providing alternative UI classes based
                  on Telerik RAD Controls
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .  
USING Progress.Lang.*                             FROM PROPATH.

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartDataBrowser:  
  
	/*------------------------------------------------------------------------------
	    Purpose: Activates the current grid row during Update/Delete/Copy in a Viewer
	    Notes:
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ActivateCurrentRow ().

    /*------------------------------------------------------------------------------
        Purpose: Resume row syncronization
        Notes:   SuspendRowSynchronization and ResumeRowSynchronization methods can 
                 be used to temporarily suspend UltraGrid from responding to data 
                 source change notifications. When row syncrhonization is suspended, 
                 the UltraGrid will still mark the rows dirty so it will re-create 
                 the rows next time it gets painted.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ResumeRowSynchronizationAfterReposition ().

    /*------------------------------------------------------------------------------
        Purpose: Scrolls to the BindingSource's position                                                                        
        Notes:   
        @param plAllRegions Logical value indicating if all RowScrollRegions will be positioned to the current row of just the first one.                                                                         
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ScrollToCurrentRow (plAllRegions AS LOGICAL) . 
  
      /*------------------------------------------------------------------------------
        Purpose: Scrolls to the BindingSource's position                                                                        
        Notes:   
        @param plAllRegions Logical value indicating if all RowScrollRegions will be positioned to the current row of just the first one.
        @param plEnforeScroll Logical set to false to indicate that we should only scroll when the row would otherwise not be visible, FALSE is the ABL Browse widget's SET-REPOSITIONED-ROW CONDITIONAL behaviour                                                                          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ScrollToCurrentRow (plAllRegions AS LOGICAL,
                                           plEnforeScroll AS LOGICAL) .
                           
    /*------------------------------------------------------------------------------
        Purpose: Scrolls the Grid to the First Row 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ScrollToFirstRow () . 
                                               
    /*------------------------------------------------------------------------------
        Purpose: Scrolls the Grid to the Last Row 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ScrollToLastRow () . 
    
	/*------------------------------------------------------------------------------
	    Purpose: Suspends row syncronization
	    Notes:   SuspendRowSynchronization and ResumeRowSynchronization methods can 
	             be used to temporarily suspend UltraGrid from responding to data 
	             source change notifications. When row syncrhonization is suspended, 
	             the UltraGrid will still mark the rows dirty so it will re-create 
	             the rows next time it gets painted.
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SuspendRowSynchronizationForReposition ().

    /*------------------------------------------------------------------------------
        Purpose: Updates the Sort Indicators of the SmartDataBrowser columns 
        Notes:   Typically invoked from a SmartDataAdapter after setting the QuerySort
                 property. Does not call back into QuerySort 
        @param pcQuerySort The QuerySort to visualize
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateSortIndicator (pcQuerySort AS CHARACTER) .  
  
END INTERFACE.
