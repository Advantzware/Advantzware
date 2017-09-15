
/*------------------------------------------------------------------------
    File        : notes.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Sep 14 18:38:45 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

&Scoped-define TABLENAME notes

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

ASSIGN notes.createDate = TODAY
       notes.createTime = TIME
       notes.createUser = USERID("asi")
       .
