
/*------------------------------------------------------------------------
    File        : notes.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Sep 14 18:40:34 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/
&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME notes

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
