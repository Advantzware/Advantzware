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
    File        : SmartFrameworkToolbarCustomizer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Aug 16 21:26:51 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.*                    FROM PROPATH .  
USING Consultingwerk.Windows.Framework.*          FROM PROPATH .  
USING Consultingwerk.Windows.Framework.Controls.* FROM PROPATH . 
USING Infragistics.Win.UltraWinToolbars.*         FROM ASSEMBLY  . 
USING Progress.Lang.*                             FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.SmartFrameworkToolbarCustomizer 
    IMPLEMENTS IFormToolbarCustomizer: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartFrameworkToolbarCustomizer class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartFrameworkToolbarCustomizer ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Customizes a UltraToolbarsManager instance during OnLoad of a BaseForm
                 instance 
        Notes:   
        @param poToolbarsManager The UltraToolbarsManager instance to customizer
        @param poOwningForm The owning Form 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CustomizeToolbar (poToolbarsManager AS UltraToolbarsManager,
                                         poOwningForm AS System.Windows.Forms.Form):

        DEFINE VARIABLE oController AS MenuStructureRibbonTabController NO-UNDO . 

        /* Mike Fechner, Consultingwerk Ltd. 16.08.2013
           All SmartFramework Maintenance Windows should use the standard SmartFramework Ribbon Tab */
        IF NOT VALID-OBJECT (poOwningForm:MdiParent) AND 
           poOwningForm:GetClass():TypeName BEGINS "Consultingwerk.Windows.Framework.":U THEN 

            oController = NEW MenuStructureRibbonTabController (poToolbarsManager,
                                                                poOwningForm, 
                                                                "fafd8128-fc6d-8eaf-e311-cd051a3c1fa6":U) .

        /* Mike Fechner, Consultingwerk Ltd. 01.05.2014
           Form MDI Children, avoid "Start" as the Title of the first Ribbon Tab */
        IF VALID-OBJECT (poOwningForm:MdiParent) AND 
            poToolbarsManager:Ribbon:Tabs:Count > 0 AND 
            poToolbarsManager:Ribbon:Tabs[0]:Caption = "Start":U THEN
            
            poToolbarsManager:Ribbon:Tabs[0]:Caption = poOwningForm:Text . 
             

    END METHOD . 

END CLASS.
