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
    File        : GarbageCollectorHelper
    Purpose     : 
    Syntax      : Static methods only, private constructor to disallow 
                  instance creation
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 13 23:36:38 CET 2010
    Notes       : This file is intended for GUI for .NET only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Util.*           FROM PROPATH . 
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING Infragistics.Win.UltraWinTree.* FROM ASSEMBLY .
USING Progress.Lang.*                 FROM ASSEMBLY .
&ENDIF

CLASS Consultingwerk.Util.GarbageCollectorHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Protected default constructor. 
        Notes:   There's no need to create instances of the helper classes                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED GarbageCollectorHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 AND DEFINED (Infragistics) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Deletes all nodes objects in the UltraTree's Nodes Collection                                                                        
        Notes:            
        @param poUltraTree The reference to the UltraTree object                                                            
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC VOID ClearUltraTreeNodes (poUltraTree AS UltraTree):
        
        GarbageCollectorHelper:ClearUltraTreeNodes (poUltraTree:Nodes) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deletes nodes objects in a TreeNodes Collections                                                                      
        Notes:                                                  
        @param poNodesCollection The reference to the TreeNodesCollection                       
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC STATIC VOID ClearUltraTreeNodes (poNodesCollection AS TreeNodesCollection):
       
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        
        IF NOT VALID-OBJECT (poNodesCollection) THEN 
            RETURN .
            
        DO i = poNodesCollection:Count - 1 TO 0 BY -1:
            
            IF VALID-OBJECT (poNodesCollection[i]:Nodes) THEN 
                GarbageCollectorHelper:ClearUltraTreeNodes(poNodesCollection[i]:Nodes) .
            
            DELETE OBJECT poNodesCollection[i] .
        END.    

        poNodesCollection:Clear () . 

    END METHOD.
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Deletes an Widget handle, if valid                                                                     
        Notes:                 
        @param phWidget The handle of the widget to delete                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DeleteObject (phWidget AS HANDLE):
        
        IF VALID-HANDLE (phWidget) THEN 
            DELETE OBJECT phWidget NO-ERROR .

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Deletes an Object instance, if valid                                                                        
        Notes:  
        @param poObject The reference to the object to delete                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DeleteObject (poObject AS Progress.Lang.Object):
        
        IF VALID-OBJECT (poObject) THEN 
            DELETE OBJECT poObject NO-ERROR .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deletes Object instances from an Array                                                                       
        Notes:   We have noticed a potential regression in OE11.4 CQC documented in 
                 SCL-392. It appears that passing an initialized Object[] to this method 
                 raises the runtime error: Uninitialized array used as source of assignment. 
                 (14906). We will investigate the issue in probably have to log a ticket 
                 with PSC when the issue is still present in 11.4 FCS. At this time 
                 we do not recomment to use this method.
        @param poObjects The ABL array of objects to delete                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DeleteObject (poObjects AS Progress.Lang.Object EXTENT):
        
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        
        DO i = 1 TO EXTENT (poObjects):
            GarbageCollectorHelper:DeleteObject (poObjects [i]) . 
        END .

    END METHOD.
    
END CLASS.
