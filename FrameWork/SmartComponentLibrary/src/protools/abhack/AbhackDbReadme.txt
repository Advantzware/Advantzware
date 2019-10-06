    AbhackDbReadme.txt   


Steps to create the database (one minute):
=========================================
a) Use Start => Progress => proenv
b) cd <yourDrive>/<thePathYouWant>
b) Prodb abhack empty
c) Proserve abhack  (use -S only if you are sure you need to host it on another 
machine, or even use the Admin Service)
d) Connect to it in a development session, then use Database Administration tool 
to load the abhack.df file provided in \protools\abhack\sampleCatDbs
e) In the misc page of the main AbhackWindow, set the connection phrase to the 
database,then hit return of press the test button to test it.  Again, on a local 
machine, try to use "<path>/<databaseFile>" rather than using a Client Server 
connection with -S.  Then you can select "Abhack database" in the "global
Resource Catalog" combo to use it.  A good start is to feed it with the global 
Dump Utility.

I do not know if I should include a few empty databases with ready schema in the 
package itself, because I do not want to have to maintain it for V9 + 10.0 10.1 
and 10.1B.



Notes:
======
1)	You may not need to backup this database.  Indeed, it takes just a few 
minutes to through it away, make a new one and Bulk Load it with the Bulk Dump 
utility available in the Misc Page.
2)	No need to host it on your usual *NIX database server, but rather try to 
host it on the development machine itself.
3)	It is not a bad idea to have a private database on your own machine as 
space is not an issue and you can connect to it in shared memory mode (without 
the ?S parameter).  By the way, it is definitely the way to go if you work with a 
laptop.
4)	If you are working on a Windows Terminal Server Machine, then you should 
definitely share your database by proserving it, but again, try to use Shared 
Memory connections, which goes about 5-10 times faster than involving the local 
TCP loopback
5)	The code that works with abhack.db is hold in one single library 
protools/abhack/abhackDbLib.p.  It has optimized transaction scoping for multi 
user cases (The Lock Table water mark remains low).
6)	At startup, if you have filled the connection phrase in the Misc page of 
the abhack main window, and if you have the ?Global Resource Catalog? combo set 
to ?ABHack Database?, then abhack tries to connect to it and checks that its 
schema is up to date.  If success, then it starts the abhackDbLib.p 
automatically.  If not, then it sets the catalog combo to ?disabled?.  Note you 
have a button to test the connection.


    


03-SEP-2007  by slacroix   
ABHack has reached phase 3 with a new catalog in a abhack.db database to improve 
two points:
1]  In phase 2, ABHack could use xml to dump and retrieve global description of 
source file in a fast way.  This way you do not need to do a global load each 
time you open a source file, as abhack reuses a previous dump.  It is also 
especially important to build a list of resource defined in other source files 
(think of Super Classes, or RUN something IN someLibHandle).  
   => But this was limited to OE 10.

2]  XML files are great, but, this is somehow a step back to the OS files days.  
The main drawback of using a XML file for each source file is that I have to 
parse it entirely just to build a subset list of resources (like just the 
internal procedures of another external procedure).
  => this can take up to 200 ms for a class with 5 super classes.  Not that bad, 
but I thought we could do something faster.


Progress has a so good database that it took me only one evening to build a 
catalog db with 14 tables and restructure the main parsers with a new database 
maintenance library.  The result is very fast (15ms instead of 200ms to load 
large resources) and works fine on Progress 9, so you should all enable a little 
abhack database somewhere to get the best from abhack.  I will keep the support 
of XML Dump for a while, but keep in mind the abhack db is just more flexible to 
provide many points in the future.



14-SEP-2007 by slacroix:
abhack.df is always the df file of the latest greatest schema 

When the schema is updated the previous schema definition is left as abhack<YYMMDD>.df 

It is certainly possible to do an incremental DF file and update the schema of an existing database, but some feature require to refresh the data themself (with the bulk Dump).  Do it if you prefer to apply a new version as soon as possible and refresh the abhack DB at night.  But be aware that a few features will not find the data it is expecting (like completion of OS files or external procedure parameters).  This is not fatal because the point is 'just' to provide completion, but what a completion...

So in short, it is just better to make a new database and bulk refresh it...


26-SEP-2007 
The last update on the schema is rather minor, so it is possible to do it manually in ordr to keep an existing database, although one should take the time to refresh it with the bulk dump utility to repair temp-table field completion
  => read the 26-SEP-2007 notes in protools/abhack/ABHackWin.releaseNotes.txt



13-JAN-2007
Added new table ahpreproc to store preprocessor definitions.
Note that you can keep your existing abhack database and load the incremental delta070926-to-080113.df.  Just refresh the database with the Bulk Dump utility in order to manage the preprocessor definitions.
