<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="ticket_report" Codebehind="ticket_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Ticket Report</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = JavaScript>
    
    var bSelected=false;
    function ChSel()
    {
        var theForm = document.forms['frmList'];
        if (!theForm) theForm = document.frmList;
        bSelected = !bSelected; 
        var i;
        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
    } 
    
    function OnKeyDown()
    {
        e = window.event;
        if (e.keyCode == 13)
        {
            e.cancel = true;
            var theForm = document.forms['frmList'];
            if (!theForm) theForm = document.frmList;                
            theForm.btnSearch.click();              
        }
    }
    
 function samevalue()
    {
    var beginc=document.getElementById("TextBox1");
    var endc=document.getElementById("TextBox2");
    endc.value=beginc.value;
    }
    
    function samevalue2()
    {
    var beginc=document.getElementById("TextBox1");
    var endc=document.getElementById("TextBox2");
    if(endc.value!=beginc.value)
    {
    alert("Begin and End Customer Value must be same");
    endc.value=beginc.value;
    endc.focus();
    }
}


var count = "";
function job1look(obj) {
    count = obj;
    var NewWindow = window.open("job1_lookup.aspx", "Job1LookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1Lookup(ReturnObj1) {
    if (count == "1") {
        document.forms[0].TextBox1.value = ReturnObj1;
    }
    else
        document.forms[0].TextBox2.value = ReturnObj1;
}

    
     function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].TextBox1.value = ReturnObj1;
  
}
function contactcustomerlook2(){ 
  var NewWindow = window.open("contact_customer_copylookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerCopyLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  
  document.forms[0].TextBox2.value = ReturnObj1;
  }

  function Relook() {
      var item1 = document.getElementById("TextBox1").value;
  var NewWindow = window.open("reorder_item_lookup.aspx?item="+ item1 +"","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup(ReturnObj1){ 
  document.forms[0].TextBox3.value = ReturnObj1;
}
function Relook2() {
    var item2 = document.getElementById("TextBox2").value;
  var NewWindow = window.open("reorder_item_lookup2.aspx?item1="+ item2 +"","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup2(ReturnObj1){ 
  document.forms[0].TextBox4.value = ReturnObj1;
}
function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].BeginorderTextBox.value=obj;
}

function Datelook1()
{
  document.forms[0].BeginorderTextBox.value="";
  Datelook();
}
function Date2look(){ 
  var NewWindow = window.open("date_lookup2.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup2(obj)
{
  document.forms[0].endingorderTextBox.value=obj;
}

function Datelook2()
{
  document.forms[0].endingorderTextBox.value="";
  Date2look();
}
function Date3look(){ 
  var NewWindow = window.open("date3_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup3(obj)
{
  document.forms[0].beDueTextBox.value=obj;
}

function Datelook3()
{
  document.forms[0].beDueTextBox.value="";
  Date3look();
}
function Date4look(){ 
  var NewWindow = window.open("date4_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup4(obj)
{
  document.forms[0].enddueTextBox.value=obj;
}

function Datelook4()
{
  document.forms[0].enddueTextBox.value="";
  Date4look();
}
  
  
  function salesreplook(){ 
  var NewWindow = window.open("salesrep_lookup.aspx","SalesRepLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function SalesRepLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].besmanTextBox.value = ReturnObj1;
  }


function smancopylook1(){ 
  var NewWindow = window.open("sman_copylookup.aspx","smancopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function smancopyLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].endsmanTextBox.value = ReturnObj1;
  }
 
 function jobReplook(){ 
  var NewWindow = window.open("jobRep_lookup.aspx","JobLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function JobRepLookup(ReturnObj1){ 
  document.forms[0].endjobTextBox.value = ReturnObj1;
}

 function custpolook(){ 
  var NewWindow = window.open("cust_po_lookup.aspx","CustPoLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustPoLookup(ReturnObj1){ 
  document.forms[0].becustpoTextBox.value = ReturnObj1;
}  
function custpo2look(){ 
  var NewWindow = window.open("cust_po_lookup2.aspx","CustPoLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustPoLookup2(ReturnObj1){ 
  document.forms[0].endcustpoTextBox.value = ReturnObj1;
}  
function userlook(){ 
  var NewWindow = window.open("user_lookup.aspx","UserLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function UserLookup(ReturnObj1){ 
  document.forms[0].beuserTextBox.value = ReturnObj1;
}  
function user2look(){ 
  var NewWindow = window.open("user2_lookup.aspx","UserLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function User2Lookup(ReturnObj1){ 
  document.forms[0].enduserTextBox.value = ReturnObj1;
}  
function datevalidate()
{
    var date=document.getElementById("BeginorderTextBox").value;
    
    if(date.length>1 && date.length<3 && date.indexOf('/')!=1)
    {
        document.getElementById("BeginorderTextBox").value = date + "/";
    }
    if(date.length>4 && date.length<6 && date.indexOf('/')!=3)
    {
        document.getElementById("BeginorderTextBox").value = date + "/";
    }
    var date2=document.getElementById("endingorderTextBox").value;
    
    if(date2.length>1 && date2.length<3 && date2.indexOf('/')!=1)
    {
        document.getElementById("endingorderTextBox").value = date2 + "/";
    }
    if(date2.length>4 && date2.length<6 && date2.indexOf('/')!=3)
    {
        document.getElementById("endingorderTextBox").value = date2 + "/";
    }
    
    var date3=document.getElementById("beDueTextBox").value;
    
    if(date3.length>1 && date3.length<3 && date3.indexOf('/')!=1)
    {
        document.getElementById("beDueTextBox").value = date3 + "/";
    }
    if(date3.length>4 && date3.length<6 && date3.indexOf('/')!=3)
    {
        document.getElementById("beDueTextBox").value = date3 + "/";
    }
    var date4=document.getElementById("enddueTextBox").value;
    
    if(date4.length>1 && date4.length<3 && date4.indexOf('/')!=1)
    {
        document.getElementById("enddueTextBox").value = date4 + "/";
    }
    if(date4.length>4 && date4.length<6 && date4.indexOf('/')!=3)
    {
        document.getElementById("enddueTextBox").value = date4 + "/";
    }
}
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>        
      <div>
          <asp:HiddenField ID="HiddenField1" runat="server" />
                   
          <asp:Label ID="Label1" ForeColor="red" Font-Bold="true" runat="server" ></asp:Label>        
          
                     
      <fieldset style="width:490px">
      <table class="shade" width="490px">
     
      <tr><td align="right" style="padding-right: 5px"><b>Begining Job#:</b></td><td>
          <asp:TextBox ID="TextBox1" OnTextChanged="TextBox1_textchanged" onkeyup="samevalue()" MaxLength="6"  width="80px" runat="server"></asp:TextBox>
          <asp:TextBox ID="TextBox3" MaxLength="2" width="20px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="job1look(1); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          <td align="right" style="padding-right: 5px"><b>Ending Job#:</b></td><td>
          <asp:TextBox ID="TextBox2" OnTextChanged="TextBox1_textchanged" MaxLength="6"  width="80px" runat="server"></asp:TextBox>
          <asp:TextBox ID="TextBox4" MaxLength="2" width="20px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="job1look(2); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>      
        </tr>   </table><table class="shade" width="490px">         
        
          <tr><td align="left" style="padding-left: 70px">
          <b><asp:CheckBox ID="CheckBox1" Text="Folding Carton" runat="server" ></asp:CheckBox></b></td>
          <td align="left" style="padding-left: 30px">
          <b><asp:CheckBox ID="CheckBox8" Text="Print Sheeter Card" runat="server" ></asp:CheckBox></b></td>          
          </tr>
          
          <tr><td align="left" style="padding-left: 70px">
          <b><asp:CheckBox ID="CheckBox2" Text="Corrugated" runat="server" ></asp:CheckBox></b></td>
          <td align="left" style="padding-left: 30px">
          <b><asp:CheckBox ID="CheckBox9" Text="Print Printer Card" runat="server" ></asp:CheckBox></b></td>
          </tr>
          
          <tr><td align="left" style="padding-left: 70px">
          <b><asp:CheckBox ID="CheckBox3" Text="Reprint Ticket" runat="server" ></asp:CheckBox></b></td>
          <td align="left" style="padding-left: 30px">
          <b><asp:CheckBox ID="CheckBox10" Text="Print Die Cutter Card" runat="server" ></asp:CheckBox></b></td>
          </tr>
          
          <tr><td align="left" style="padding-left: 70px">
          <b><asp:CheckBox ID="CheckBox4" Text="Print Box Design" runat="server" ></asp:CheckBox></b></td>
          <td align="left" style="padding-left: 30px">
          <b><asp:CheckBox ID="CheckBox11" Text="Print Glure/Window Card" runat="server" ></asp:CheckBox></b></td>
          </tr>
          
          <tr><td align="left" style="padding-left: 70px">
          <b><asp:CheckBox ID="CheckBox5" Text="Print FG Item Image" runat="server" ></asp:CheckBox></b></td>
          <td align="left" style="padding-left: 30px">
          <b><asp:CheckBox ID="CheckBox12" Text="Print Shrink Warp Card" runat="server" ></asp:CheckBox></b></td>
          </tr>
          
          <tr><td align="left" style="padding-left: 70px">
          <b><asp:CheckBox ID="CheckBox6" Text="Approve Job(s)" runat="server" ></asp:CheckBox></b></td>
          <td align="left" style="padding-left: 30px">
          <b><asp:CheckBox ID="CheckBox13" Text="Copy 2and3 intrag 2(Artios)" runat="server" ></asp:CheckBox></b></td>
          </tr>
          
          <tr><td nowrap align="left" style="padding-left: 70px">
          <b><asp:CheckBox ID="CheckBox7" Text="Print All Unprinted App. Ticket" runat="server" ></asp:CheckBox></b></td>
          <td align="left" style="padding-left: 30px">
          <b><asp:CheckBox ID="CheckBox14" Text="Make Hold" runat="server" ></asp:CheckBox></b></td>
          </tr>
          
          
          <tr><td colspan="2" align="left" style="padding-left: 60px"><b>Spec Codes:</b>
          <asp:TextBox ID="TextBox5" width="280px" runat="server"></asp:TextBox></td></tr>
                             
          <tr><td colspan="2" align="left" style="padding-left: 70px">
          <b><asp:CheckBox ID="CheckBox15" Text="Print Revision Number" runat="server" ></asp:CheckBox></b>
          <asp:TextBox ID="TextBox6" runat="server"></asp:TextBox></td></tr>
          
          <tr><td align="left" style="padding-left: 70px">
          <b><asp:CheckBox ID="CheckBox16" Text="Print Machine Standerd" runat="server" ></asp:CheckBox></b></td>
          <td><asp:TextBox ID="TextBox7" runat="server"></asp:TextBox></td>
          </tr>
          <tr><td colspan="2" align="left" style="padding-left: 60px"><b>Print Machine Speed on Run Hours:</b>
          <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="2" Font-Bold ="true" runat="server">
                              
                 <asp:ListItem     Text="Speed" />
                 <asp:ListItem     Text="Run Hour" />
                 </asp:RadioButtonList></td></tr>
          
          <tr><td align="left" style="padding-left: 70px">
          <b><asp:CheckBox ID="CheckBox17" Text="Print shipto" runat="server" ></asp:CheckBox></b></td>
          
          <td align="left" style="padding-left: 30px">
          <b><asp:CheckBox ID="CheckBox20" Text="Print Sell Price in Place of UPC#" runat="server" ></asp:CheckBox></b></td></tr>
          
          <tr><td align="left" style="padding-left: 70px">
          <b><asp:CheckBox ID="CheckBox18" Text="Print Label info" runat="server" ></asp:CheckBox></b></td>          
          <td align="left" style="padding-left: 30px">
          <b><asp:CheckBox ID="CheckBox21" Text="Print Only committed board" runat="server" ></asp:CheckBox></b></td></tr>
          
          <tr><td nowrap align="left" style="padding-left: 70px">
          <b><asp:CheckBox ID="CheckBox19" Text="Print Set Utilization Page" runat="server" ></asp:CheckBox></b></td>
          
          <td align="left" style="padding-left: 30px">
          <b><asp:CheckBox ID="CheckBox22" Text="Prompt Split Shipment or Split order" runat="server" ></asp:CheckBox></b></td></tr>
          
          <tr><td align="left" style="padding-left: 70px">
          <b><asp:CheckBox ID="CheckBox23" Text="Freeze job Notes" runat="server" ></asp:CheckBox></b></td>
          
          <td nowrap align="left" style="padding-left: 30px">
          <b><asp:CheckBox ID="CheckBox24" Text="Sample(s) Required" runat="server" ></asp:CheckBox></b>
          <asp:TextBox ID="TextBox8" Width="100px" runat="server"></asp:TextBox></td></tr>
         
          <tr><td align="left" style="padding-left: 70px">
          <b><asp:CheckBox ID="CheckBox25" Text="Departments" runat="server" ></asp:CheckBox></b></td></tr>         
          
             <tr><td colspan="3">
              <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" />
              &nbsp;&nbsp;&nbsp;&nbsp;
              <asp:Label ID="OutputLabel" runat="server" Text="Print:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              &nbsp;
              <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
              </td>
          </tr>  
                                
              
             
             
           </table></fieldset>
       <div style="display:none">
          <asp:FormView ID="FormView1"  OnDataBound="Formview1_databound" runat="server" 
              DataSourceID="ObjectDataSource1">
                           
              <ItemTemplate>
                  ticketfile:
                  <asp:Label ID="ticketfileLabel" runat="server" Text='<%# Bind("ticketfile") %>'></asp:Label><br />
                  vCorr:
                  <asp:Label ID="vCorrLabel" runat="server" Text='<%# Bind("vCorr") %>' />
                  <br />
                  vFold:
                  <asp:Label ID="vFoldLabel" runat="server" Text='<%# Bind("vFold") %>' />
                  <br />
                  vForcor:
                  <asp:Label ID="vForcorLabel" runat="server" Text='<%# Bind("vForcor") %>' />
                  <br />
                  vForfold:
                  <asp:Label ID="vForfoldLabel" runat="server" Text='<%# Bind("vForfold") %>' />
                  <br />
                  vTicket:
                  <asp:Label ID="vTicketLabel" runat="server" Text='<%# Bind("vTicket") %>' />
                  <br />
              </ItemTemplate>
          </asp:FormView>
          </div>
             
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="ReRticket" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmbeginJob1" Type="String" />
                  <asp:Parameter Name="prmbeginJob2" Type="Int32" />
                  <asp:Parameter Name="prmendJob1" Type="string" />
                  <asp:Parameter Name="prmendJob2" Type="Int32" />
                  <asp:Parameter Name="prmtbFold" Type="String" />
                  <asp:Parameter Name="prmtbRS" Type="String" />
                  <asp:Parameter Name="prmtbCorr" Type="String" />
                  <asp:Parameter Name="prmtbPR" Type="String" />
                  <asp:Parameter Name="prmtbReprint" Type="String" />
                  <asp:Parameter Name="prmtbDC" Type="String" />
                  <asp:Parameter Name="prmtbBox" Type="String" />
                  <asp:Parameter Name="prmtbGL" Type="String" />
                  <asp:Parameter Name="prmtbSW" Type="String" />
                  <asp:Parameter Name="prmtbApprove" Type="String" />
                  <asp:Parameter Name="prmspecCodes" Type="String" />
                  <asp:Parameter Name="prmrevsnNo" Type="Int32" />
                  <asp:Parameter Name="prmtbPrtLabel" Type="String" />
                  <asp:Parameter Name="prmtbCommitted" Type="String" />
                  <asp:Parameter Name="prmtbPrtSetHeader" Type="String" />
                  <asp:Parameter Name="prmtbPromptShip" Type="String" />
                  <asp:Parameter Name="prmdeptCodes" Type="String" />
                  <asp:Parameter Name="prmtbFreezeNote" Type="String" />
                  <asp:Parameter Name="prmtbDeptNote" Type="String" />
                  <asp:Parameter Name="prmTBSampleReq" Type="String" />
                  <asp:Parameter Name="prmflJobord" Type="Int32" />
                  <asp:Parameter Name="prmrdPrintSpeed" Type="String" />
                  <asp:Parameter Name="prmtbFgimage" Type="String" />
                  <asp:Parameter Name="prmtbMakeHold" Type="String" />
                  <asp:Parameter Name="prmtbPrtMch" Type="String" />
                  <asp:Parameter Name="prmtbPrtSellprc" Type="String" />
                  <asp:Parameter Name="prmtbTray2" Type="String" />
                  <asp:Parameter Name="prmtbAppUunprinted" Type="String" />
                  <asp:Parameter Name="prmtbPrtRev" Type="String" />
                  <asp:Parameter Name="rmtbPrtShipto" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
   
    </form>
  </body>
</HTML>

