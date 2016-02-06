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
    File        : AfterInitializeBusinessEntityAdapterEventArgs
    Purpose     : Event argument for the AfterInitializeBusinessEntityAdapter 
                  event of the SmartBusinessEntityLookup Controls
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Apr 11 21:12:05 CEST 2011
    Notes       : Event argument class for the SmartBusinessEntityLookup 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .
USING System.EventArgs                                FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Implementation.AfterInitializeBusinessEntityAdapterEventArgs 
    INHERITS System.EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the origin of the initailized SmartBusinessEntity Adapter
        Notes:   
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY AdapterOrigin AS AdapterOriginEnum NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the reference to the SmartBusinessEntity Adapter initialized
        Notes:   
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY BusinessEntityAdapter AS SmartBusinessEntityAdapter NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AfterInitializeBusinessEntityAdapterEventArgs class
        Notes:   
        @param poAdapter The reference to the business entity adapter
        @param poOrigin The reference to the AdapterOrigin 
    ------------------------------------------------------------------------------*/  
    CONSTRUCTOR PUBLIC AfterInitializeBusinessEntityAdapterEventArgs (poAdapter AS SmartBusinessEntityAdapter,
                                                                      poOrigin AS AdapterOriginEnum):
        SUPER ().
        
        ASSIGN THIS-OBJECT:BusinessEntityAdapter = poAdapter
               THIS-OBJECT:AdapterOrigin         = poOrigin 
               .
                
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AfterInitializeBusinessEntityAdapterEventArgs class
        Notes:   
    ------------------------------------------------------------------------------*/  
    CONSTRUCTOR PUBLIC AfterInitializeBusinessEntityAdapterEventArgs ():
        SUPER ().
        
    END CONSTRUCTOR.

END CLASS.
