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
    File        : DesignerHelper
    Purpose     : General purpose support routines for Visual Designer
                  enhancements
    Syntax      : Static methods only, private constructor to disallow 
                  instance creation
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jun 01 15:51:45 CEST 2010
    Notes       : This file is intended for GUI for .NET only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING Consultingwerk.Util.*          FROM ASSEMBLY . 
USING Progress.Lang.*                FROM ASSEMBLY .
USING System.ComponentModel.*        FROM ASSEMBLY .
USING System.ComponentModel.Design.* FROM ASSEMBLY . 
&ENDIF

CLASS Consultingwerk.Util.DesignerHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Protected default constructor. 
        Notes:   There's no need to create instances of the helper classes                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED DesignerHelper (  ):
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a name (for a control or component) that compilies with the 
                 supported component names in the Visual Designer 
        Notes:   Replaces all characters outside of A-Z, a-z and 0-9 with _
                 First character needs to be a alpha character, no _
        @param pcName The name to turn into a valid component name
        @return The fixed component name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER DesignerSafeName (pcName AS CHARACTER):
        
        DEFINE VARIABLE i    AS INTEGER NO-UNDO.
        DEFINE VARIABLE iAsc AS INTEGER NO-UNDO.
        
        DO i = 1 TO LENGTH (pcName):
            
            ASSIGN iAsc = ASC (SUBSTRING (pcName, i, 1)) .
            
            IF (iAsc >= 65 AND iAsc <= 90)  /* A..Z */      OR  
               (iAsc >= 97 AND iAsc <= 122) /* a..z */      OR
               (iAsc >= 48 AND iAsc <= 57) THEN 
               NEXT . 
            
            SUBSTRING (pcName, i, 1) = "_":U . 
        END.

        RETURN pcName .

    END METHOD .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Locates the IDesigner for the component                                                                        
        Notes:    
        @param poComponent The reference to an IComponent
        @return The IDesigner associated with the Component                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC IDesigner FindDesigner (poComponent AS IComponent):
        
        DEFINE VARIABLE oHost     AS System.ComponentModel.Design.IDesignerHost NO-UNDO .
        DEFINE VARIABLE oDesigner AS System.ComponentModel.Design.IDesigner     NO-UNDO . 

        ASSIGN oHost = CAST (poComponent:Site:GetService (Progress.Util.TypeHelper:GetType ("System.ComponentModel.Design.IDesignerHost":U)),
                            System.ComponentModel.Design.IDesignerHost).
        
        IF NOT VALID-OBJECT (oHost) THEN 
            UNDO, THROW NEW AppError ("Unable to locate the DesignerHost.":U, 0) .
            
        oDesigner = oHost:GetDesigner (poComponent) . 
            
        RETURN oDesigner .
            
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the ComponentChanged Event                                                                       
        Notes:
        @param poComponent The reference to the IComponent
        @param pcPropertyName The name of the property the changed the value
        @param poOldValue The old property value
        @param poNewValue The new property value                                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID RaiseComponentChanged (poComponent AS IComponent,
                                                     pcPropertyName AS CHARACTER,
                                                     poOldValue AS System.Object,
                                                     poNewValue AS System.Object):
        
        DEFINE VARIABLE oComponentChangeService AS System.ComponentModel.Design.IComponentChangeService NO-UNDO . 
        DEFINE VARIABLE oComponentCollection    AS "System.Collections.Generic.List<Component>":U       NO-UNDO . 

        DEFINE VARIABLE oMember                 AS MemberDescriptor                                     NO-UNDO .
        DEFINE VARIABLE oProperties             AS PropertyDescriptorCollection                         NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 26.10.2011
           Component is probably not running in the Visual Designer */                
        IF NOT VALID-OBJECT (poComponent:Site) THEN 
            RETURN .                 
                
        ASSIGN oComponentChangeService = CAST (poComponent:Site:GetService (Progress.Util.TypeHelper:GetType ("System.ComponentModel.Design.IComponentChangeService":U)),
                                               IComponentChangeService) .

        IF TYPE-OF (poComponent,
                    ICustomTypeDescriptor) THEN
            oProperties = CAST (poComponent,
                                ICustomTypeDescriptor):GetProperties () .
        ELSE
            oProperties = TypeDescriptor:GetProperties (poComponent) .    

        oMember = oProperties [pcPropertyName] .

        oComponentChangeService:OnComponentChanging (poComponent,
                                                     oMember).
                                                    
        oComponentChangeService:OnComponentChanged (poComponent,
                                                    oMember,
                                                    poOldValue,
                                                    poNewValue).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the PropertyGrid of a Component in the Visual Designer                                                                        
        Notes:   Actually de-selects and selects the property using the 
                 ISelectionService
        @param poComponent The reference to the Component for which the property grid should be refreshed                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID RefreshPropertyGrid (poComponent AS Component):
        
        DEFINE VARIABLE oSelectionService    AS System.ComponentModel.Design.ISelectionService NO-UNDO . 
        DEFINE VARIABLE oComponentCollection AS "System.Collections.Generic.List<Component>"   NO-UNDO . 
        
        /* Mike Fechner, Consultingwerk Ltd. 26.10.2011
           Component is probably not running in the Visual Designer */                
        IF NOT VALID-OBJECT (poComponent:Site) THEN 
            RETURN .                 
        
        ASSIGN oSelectionService = CAST (poComponent:Site:GetService (Progress.Util.TypeHelper:GetType ("System.ComponentModel.Design.ISelectionService":U)),
                                         ISelectionService) .
                       
        IF NOT VALID-OBJECT (oSelectionService) THEN 
            UNDO, THROW NEW AppError ("Unable to get current ISelectionService. Cannot refresh PropertyGrid."{&TRAN}, 0) .                       
                                   
        oComponentCollection = NEW "System.Collections.Generic.List<Component>" () .

        oSelectionService:SetSelectedComponents (oComponentCollection) .

        oComponentCollection:Add (poComponent) .

        oSelectionService:SetSelectedComponents (oComponentCollection) .

        FINALLY:
            IF VALID-OBJECT (oComponentCollection) THEN 
                DELETE OBJECT oComponentCollection .
        END FINALLY.

    END METHOD.
&ENDIF
END CLASS.
