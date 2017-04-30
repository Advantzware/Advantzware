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
    File        : RepositoryTaskParameter
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jan 28 21:13:00 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

USING Consultingwerk.*                              FROM PROPATH .
USING Consultingwerk.Framework.RepositoryServices.* FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.Framework.RepositoryServices.RepositoryTaskParameter
    INHERITS Serializable:

    {Consultingwerk/SerializableProperty.i ClassChildren CHARACTER} .
    {Consultingwerk/SerializableProperty.i New LOGICAL} .
    {Consultingwerk/SerializableProperty.i ObjectName CHARACTER} .
    {Consultingwerk/SerializableProperty.i ObjectInstanceName CHARACTER} .
    {Consultingwerk/SerializableProperty.i ObjectType CHARACTER} .

END CLASS.