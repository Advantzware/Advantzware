// Title: Tigra Calendar
// URL: http://www.softcomplex.com/products/tigra_calendar/
// Version: 3.3 (European date format)
// Date: 09/01/2005 (mm/dd/yyyy)
// Note: Permission given to use this script in ANY kind of applications if
//    header lines are left unchanged.
// Note: Script consists of two files: calendar?.js and calendar.html

// if two digit year input dates after this year considered 20 century.
var NUM_CENTYEAR = 30;
// is time input control required by default
var BUL_TIMECOMPONENT = false;
// are year scrolling buttons required by default
var BUL_YEARSCROLL = true;
// 
var BUL_SUNDAYFIRST = true;

var calendars = [];
var RE_NUM = /^\-?\d+$/;

function calendar(value) {

  // assigning methods
  this.gen_date = cal_gen_date;
  this.gen_time = cal_gen_time;
  this.gen_tsmp = cal_gen_tsmp;
  this.prs_date = cal_prs_date;
  this.prs_time = cal_prs_time;
  this.prs_tsmp = cal_prs_tsmp;
  this.popup    = cal_popup;

  // validate input parameters 
  this.targetvalue = value;

  //txt
  this.target = null;
  //dll
  this.targetD = null;
  this.targetM = null;
  this.targetY = null;
  //date format
  this.targetformat = "dd-mm-yyyy";
  
  this.time_comp = BUL_TIMECOMPONENT;
  this.sunday_first = BUL_SUNDAYFIRST;
  this.year_scroll = BUL_YEARSCROLL;
  
  // register in global collections
  this.id = calendars.length;
  calendars[this.id] = this;
}

function cal_popup(str_datetime) {
  if (str_datetime) {
    this.dt_current = this.prs_tsmp(str_datetime);
  }
  else {
    this.dt_current = this.prs_tsmp(this.targetvalue);
    this.dt_selected = this.dt_current;
  } 
  
  if (!this.dt_current)
  {
    this.dt_current=new Date();
    this.dt_selected = this.dt_current;
  };

  var obj_calwindow = window.open(
    'calendar.html?datetime=' + this.dt_current.valueOf()+ '&id=' + this.id,
    'Calendar', 'width=215,height='+(this.time_comp ? 230 : 205)+
    ',status=no,resizable=no,top=200,left=200,dependent=yes,alwaysRaised=yes'
  );
  obj_calwindow.opener = window;
  obj_calwindow.focus();
}

// timestamp generating function
function cal_gen_tsmp(dt_datetime) {
  return(this.gen_date(dt_datetime) + ' ' + this.gen_time(dt_datetime));
}

// date generating function
function cal_gen_date(dt_datetime) {
  return (
    (dt_datetime.getDate() < 10 ? '0' : '') + dt_datetime.getDate() + "-"
    + (dt_datetime.getMonth() < 9 ? '0' : '') + (dt_datetime.getMonth() + 1) + "-"
    + dt_datetime.getFullYear()
  );
}
// time generating function
function cal_gen_time(dt_datetime) {
  return (
    (dt_datetime.getHours() < 10 ? '0' : '') + dt_datetime.getHours() + ":"
    + (dt_datetime.getMinutes() < 10 ? '0' : '') + (dt_datetime.getMinutes()) + ":"
    + (dt_datetime.getSeconds() < 10 ? '0' : '') + (dt_datetime.getSeconds())
  );
}

// timestamp parsing function
function cal_prs_tsmp(str_datetime) {
  // if no parameter specified return current timestamp
  if (!str_datetime)
    return (new Date());

  // if positive integer treat as milliseconds from epoch
  if (RE_NUM.exec(str_datetime))
    return new Date(str_datetime);
    
  // else treat as date in string format
  var arr_datetime = str_datetime.split(' ');
  return this.prs_time(arr_datetime[1], this.prs_date(arr_datetime[0]));
}

// date parsing function
function cal_prs_date(str_date) {

  var arr_date = str_date.split('-');

  if (arr_date.length != 3) return cal_error ("Invalid date format: '" + str_date + "'.\nFormat accepted is dd-mm-yyyy.");
  if (!arr_date[0]) return cal_error ("Invalid date format: '" + str_date + "'.\nNo day of month value can be found.");
  if (!RE_NUM.exec(arr_date[0])) return cal_error ("Invalid day of month value: '" + arr_date[0] + "'.\nAllowed values are unsigned integers.");
  if (!arr_date[1]) return cal_error ("Invalid date format: '" + str_date + "'.\nNo month value can be found.");
  if (!RE_NUM.exec(arr_date[1])) return cal_error ("Invalid month value: '" + arr_date[1] + "'.\nAllowed values are unsigned integers.");
  if (!arr_date[2]) return cal_error ("Invalid date format: '" + str_date + "'.\nNo year value can be found.");
  if (!RE_NUM.exec(arr_date[2])) return cal_error ("Invalid year value: '" + arr_date[2] + "'.\nAllowed values are unsigned integers.");

  var dt_date = new Date();
  dt_date.setDate(1);

  if (arr_date[1] < 1 || arr_date[1] > 12) return cal_error ("Invalid month value: '" + arr_date[1] + "'.\nAllowed range is 01-12.");
  dt_date.setMonth(arr_date[1]-1);
   
  if (arr_date[2] < 100) arr_date[2] = Number(arr_date[2]) + (arr_date[2] < NUM_CENTYEAR ? 2000 : 1900);
  dt_date.setFullYear(arr_date[2]);

  var dt_numdays = new Date(arr_date[2], arr_date[1], 0);
  dt_date.setDate(arr_date[0]);
  if (dt_date.getMonth() != (arr_date[1]-1)) return cal_error ("Invalid day of month value: '" + arr_date[0] + "'.\nAllowed range is 01-"+dt_numdays.getDate()+".");

  return (dt_date)
}

// time parsing function
function cal_prs_time(str_time, dt_date) {

  if (!dt_date) return null;
  var arr_time = String(str_time ? str_time : '').split(':');

  if (!arr_time[0]) dt_date.setHours(0);
  else if (RE_NUM.exec(arr_time[0]))
    if (arr_time[0] < 24) dt_date.setHours(arr_time[0]);
    else return cal_error ("Invalid hours value: '" + arr_time[0] + "'.\nAllowed range is 00-23.");
  else return cal_error ("Invalid hours value: '" + arr_time[0] + "'.\nAllowed values are unsigned integers.");
  
  if (!arr_time[1]) dt_date.setMinutes(0);
  else if (RE_NUM.exec(arr_time[1]))
    if (arr_time[1] < 60) dt_date.setMinutes(arr_time[1]);
    else return cal_error ("Invalid minutes value: '" + arr_time[1] + "'.\nAllowed range is 00-59.");
  else return cal_error ("Invalid minutes value: '" + arr_time[1] + "'.\nAllowed values are unsigned integers.");

  if (!arr_time[2]) dt_date.setSeconds(0);
  else if (RE_NUM.exec(arr_time[2]))
    if (arr_time[2] < 60) dt_date.setSeconds(arr_time[2]);
    else return cal_error ("Invalid seconds value: '" + arr_time[2] + "'.\nAllowed range is 00-59.");
  else return cal_error ("Invalid seconds value: '" + arr_time[2] + "'.\nAllowed values are unsigned integers.");

  dt_date.setMilliseconds(0);
  return dt_date;
}

function cal_error (str_message) {
//  alert (str_message);
  return null;
}

function show_mycalendar(sFormat, bShowTime, bSundayFirst, sTarget)
{

  show_mycalendar(sFormat, bShowTime, bSundayFirst, sTarget, null, null)

}

function show_mycalendar(sFormat, bShowTime, bSundayFirst, sTargetD, sTargetM, sTargetY)
{
  if (sTargetM == null || sTargetY == null)
  {
	  var sVal = eval(sTargetD + '.value');
	  var cal = new calendar(dt2str(str2dt(sVal, sFormat), "dd-mm-yyyy", bShowTime), sFormat);

	  cal.target = sTargetD;
	  cal.targetD = null;
	  cal.targetM = null;
	  cal.targetY = null;
  }
  else
  {
	  var sVal = eval(sTargetD + '.value + "-"+' + sTargetM + '.value + "-"+' + sTargetY + '.value');
	  var cal = new calendar(sVal);

	  cal.target = null;
	  cal.targetD = sTargetD;
	  cal.targetM = sTargetM;
	  cal.targetY = sTargetY;
  }
  
  cal.targetformat = sFormat;
  cal.time_comp = bShowTime; 
  cal.sunday_first = bSundayFirst;
  cal.year_scroll = false;
  cal.popup();
}

function str2dt(str,format)
{

 if (str == "" || str == null) return null;
  var sFormat= "" + format
  sFormat = sFormat.toLowerCase();

  var re=/\d+/g;
  var arr=str.match(re);
  var dt;
  
  if(arr==null || arr.length<3) return null;
  while(arr.length<6) arr[arr.length]=0;

  if (sFormat.charAt(0) == "y")
  { //yyyy_?_?
    if (sFormat.charAt(5) == "d")
      //yyyy_dd_mm
    dt = new Date(arr[0],arr[2]-1,arr[1],arr[3],arr[4],arr[5]);
    else
    //yyyy_mm_dd
      dt = new Date(arr[0],arr[1]-1,arr[2],arr[3],arr[4],arr[5]);
  }
  else
  {
  if (sFormat.charAt(0) == "d")
      //dd_mm_yyyy
      dt = new Date(arr[2],arr[1]-1,arr[0],arr[3],arr[4],arr[5]);
    else
      //mm_dd_yyyy
    dt = new Date(arr[2],arr[0]-1,arr[1],arr[3],arr[4],arr[5]);
  }

//alert(dt);

  if(isNaN(dt))
    return null;
//  check date and month
  if(sFormat.charAt(0) == "d" && (dt.getMonth()!=arr[1]-1 || dt.getDate()!=arr[0]))
    return null;
  if(sFormat.charAt(0) == "m" && (dt.getMonth()!=arr[0]-1 || dt.getDate()!=arr[1]))
    return null;
  if(sFormat.charAt(0) == "y")
  {
  if (sFormat.charAt(5) == "d" && (dt.getMonth()!=arr[2]-1 || dt.getDate()!=arr[1]))  return null;
  else if (dt.getMonth()!=arr[1]-1 || dt.getDate()!=arr[2])  return null;
  }
  return dt;
}

function dt2str(value, format, showtime)
{
//  format:  dd?mm?yyyy || mm?dd?yyyy || yyyy?dd?mm || yyyy?mm?dd

  if (value == null || value=="") return "";
  var date='';
  var datedelimiter='/';
  
  var str = "" + format
  str = str.toLowerCase();
  if (str.charAt(0) == "y")
  { //yyyy_?_?
    datedelimiter = str.charAt(4);
    date+=value.getFullYear() + datedelimiter;
    if (str.charAt(5) == "d")
      //yyyy_dd_mm
      date+= (value.getDate()<10?'0'+value.getDate():value.getDate()) + datedelimiter + (value.getMonth()<9?'0'+(value.getMonth()+1):value.getMonth()+1);
    else
      //yyyy_mm_dd
      date+=  (value.getMonth()<9?'0'+(value.getMonth()+1):value.getMonth()+1) + datedelimiter + (value.getDate()<10?'0'+value.getDate():value.getDate());    
  }
  else
  { //?_?_yyyy
    datedelimiter =  str.charAt(2);
    if (str.charAt(0) == "d")
      //dd_mm_yyyy
      date+= (value.getDate()<10?'0'+value.getDate():value.getDate()) + datedelimiter + (value.getMonth()<9?'0'+(value.getMonth()+1):value.getMonth()+1) + datedelimiter + value.getFullYear();
    else
      //mm_dd_yyyy
      date+=  (value.getMonth()<9?'0'+(value.getMonth()+1):value.getMonth()+1) + datedelimiter + (value.getDate()<10?'0'+value.getDate():value.getDate()) + datedelimiter + value.getFullYear();
  }
  
  //alert(date);
  
  if(!showtime)
    return date;

  var time='';
  if(value.getHours()>0 || value.getMinutes()>0 || value.getSeconds()>0)
  {
    time+=(value.getHours()<10?'0'+value.getHours():value.getHours());
    time+=':'+(value.getMinutes()<10?'0'+value.getMinutes():value.getMinutes())
  }
  if(value.getSeconds()>0)
    time+=':'+(value.getSeconds()<10?'0'+value.getSeconds():value.getSeconds());
  
  return date+' '+time;
}

function SetOutputValue(cal, dtValue)
{
  
  if (cal.target != null)
  {
    var sDt = dt2str(dtValue, cal.targetformat, cal.time_comp)
    eval(cal.target+'.value="'+sDt+'";');
  }
  else
  {    
    eval(cal.targetD+'.value="'+dtValue.getDate()+'";');  
	eval(cal.targetM+'.value="'+(dtValue.getMonth()+1)+'";');  
	eval(cal.targetY+'.value="'+dtValue.getFullYear()+'";');  
  }
}