SET DLClocn=C:\Progress\OpenEdge
set DUMP_INC_CODEPAGE=1252 
set DUMP_INC_INDEXMODE=active 
set DUMP_INC_RENAMEFILE=.\wrk\renamefile.rf 
set DUMP_INC_DEBUG=0 
set PROPATH=p:\asi16ship\stdpatch,p:\asi16ship\stdpatch\prodict

set DUMP_INC_DFFILE=.\wrk\asidelta.df 
%DLClocn/bin/_progres -b  -db asi -S 3800 -db asi -S 2803 -ld slave -p prodict/dump_inc.p

set DUMP_INC_DFFILE=.\wrk\nosweatdelta.df 
%DLClocn/bin/_progres -b  -db nosweat -S 3802 -db nosweat -S 2800 -ld slave -p prodict/dump_inc.p

set DUMP_INC_DFFILE=.\wrk\asihelpdelta.df 
%DLClocn/bin/_progres -b  -db asihelp -S 3801 -db asihelp -S 2801 -ld slave -p prodict/dump_inc.p

set DUMP_INC_DFFILE=.\wrk\addonnosweatdelta.df 
%DLClocn/bin/_progres -b  -db nosweat -S 3810 -db nosweat -S 2806 -ld slave -p prodict/dump_inc.p

set DUMP_INC_DFFILE=.\wrk\emptrackdelta.df 
%DLClocn/bin/_progres -b  -db emptrack -S 3808 -db emptrack -S 2804 -ld slave -p prodict/dump_inc.p

set DUMP_INC_DFFILE=.\wrk\jobsdelta.df 
%DLClocn/bin/_progres -b  -db jobs -S 3809 -db jobs -S 2805 -ld slave -p prodict/dump_inc.p

set DUMP_INC_DFFILE=.\wrk\rfqdelta.df 
%DLClocn/bin/_progres -b  -db rfq -S 3811 -db rfq -S 2807 -ld slave -p prodict/dump_inc.p
