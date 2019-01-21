<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="order_bal_by_job" Codebehind="~/order_bal_by_job.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Order Balance By PO#/Job</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="jaorder_bal_by_jobvascript" src="include/insert.js"></script>
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

function salesreplook(){ 
  var NewWindow = window.open("salesrep_lookup.aspx","SalesRepLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function SalesRepLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].BeginSalesmanTextBox.value = ReturnObj1;
  }


function smancopylook1(){ 
  var NewWindow = window.open("sman_copylookup.aspx","smancopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function smancopyLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].EndingSalesmanTextBox.value = ReturnObj1;
 }

 function Relook() {
     var item1 = document.getElementById("TextBox1").value;
  var NewWindow = window.open("reorder_item_lookup.aspx?item="+ item1+"","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup(ReturnObj1){ 
  document.forms[0].TextBox3.value = ReturnObj1;
}
function Relook2() {
    var item2 = document.getElementById("TextBox2").value;
  var NewWindow = window.open("reorder_item_lookup2.aspx?item2="+ item2+"","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup2(ReturnObj1){ 
  document.forms[0].TextBox4.value = ReturnObj1;
}
function job1look(){ 
  var NewWindow = window.open("job1_lookup.aspx","JobLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1Lookup(ReturnObj1){ 
  document.forms[0].BeginjobTextBox.value = ReturnObj1;
} 
 function jobReplook(){ 
  var NewWindow = window.open("jobRep_lookup.aspx","JobLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function JobRepLookup(ReturnObj1){ 
  document.forms[0].EndjobTextBox.value = ReturnObj1;
}
function custpolook(){ 
  var NewWindow = window.open("cust_po_lookup.aspx","CustPoLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustPoLookup(ReturnObj1){ 
  document.forms[0].BeginCustpoTextBox.value = ReturnObj1;
}  
function custpo2look(){ 
  var NewWindow = window.open("cust_po_lookup2.aspx","CustPoLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustPoLookup2(ReturnObj1){ 
  document.forms[0].EndCustpoTextBox.value = ReturnObj1;
}  



function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].TextBox5.value=obj;
}

function Datelook1()
{
  document.forms[0].TextBox5.value="";
  Datelook();
}
function Date2look(){ 
  var NewWindow = window.open("date_lookup2.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup2(obj)
{
  document.forms[0].TextBox6.value=obj;
}

function Datelook2()
{
  document.forms[0].TextBox6.value="";
  Date2look();
}
function Date3look(){ 
  var NewWindow = window.open("date3_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup3(obj)
{
  document.forms[0].asofTextBox.value=obj;
}

function Datelook3()
{
  document.forms[0].asofTextBox.value="";
  Date3look();
}
function datevalidate()
{
    var date=document.getElementById("TextBox5").value;
    
    if(date.length>1 && date.length<3 && date.indexOf('/')!=1)
    {
        document.getElementById("TextBox5").value = date + "/";
    }
    if(date.length>4 && date.length<6 && date.indexOf('/')!=3)
    {
        document.getElementById("TextBox5").value = date + "/";
    }
    var date2=document.getElementById("TextBox6").value;
    
    if(date2.length>1 && date2.length<3 && date2.indexOf('/')!=1)
    {
        document.getElementById("TextBox6").value = date2 + "/";
    }
    if(date2.length>4 && date2.length<6 && date2.indexOf('/')!=3)
    {
        document.getElementById("TextBox6").value = date2 + "/";
    }
}
function asofdate()
{
    var date=document.getElementById("asofTextBox").value;
    
    if(date.length>1 && date.length<3 && date.indexOf('/')!=1)
    {
        document.getElementById("asofTextBox").value = date + "/";
    }
    if(date.length>4 && date.length<6 && date.indexOf('/')!=3)
    {
        document.getElementById("asofTextBox").value = date + "/";
    }
}
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
         <hd:header id="Header1" runat="server"></hd:header>
      <div>
         <asp:HiddenField ID="HiddenField1" runat="server" />
         <asp:HiddenField ID="HiddenField2" runat="server" />
         <asp:HiddenField ID="HiddenField3" runat="server" />
         <asp:HiddenField ID="HiddenField4" runat="server" />
         <asp:HiddenField ID="HiddenField5" runat="server" />
         <asp:HiddenField ID="HiddenField6" runat="server" />
         <asp:HiddenField ID="HiddenField7" runat="server" />
         <asp:HiddenField ID="HiddenField8" runat="server" />
         <asp:HiddenField ID="HiddenField9" runat="server" />
         <asp:HiddenField ID="HiddenField10" runat="server" />
         <asp:HiddenField ID="HiddenField11" runat="server" />
         <asp:HiddenField ID="HiddenField12" runat="server" />
         <asp:HiddenField ID="HiddenField13" runat="server" />
       
         
         
          <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
          
                    
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Order Balance By PO# / Job &nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      
      <table class="shade" width="700px">
       
      <tr><td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
          <asp:TextBox ID="TextBox1"  onkeyup="samevalue()"  width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td>
      <td>
            <asp:TextBox ID="TextBox2" width="100px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
      </td>
        </tr>
        
        <tr><td align="right" style="padding-right:5px"><b>Begining Order Date:</b></td>
          <td><asp:TextBox ID="TextBox5" MaxLength="12" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"   Width="100px" ToolTip="MM/DD/YYYY" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onblur="document.getElementById('TextBox5').focus()"  onClick="showCalendarControl(TextBox5); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
          </td>
         
          <td align="right" style="padding-right:5px"><b>Ending Order Date</b></td>
           <td>
              <asp:TextBox ID="TextBox6" MaxLength="12" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"   Width="100px" ToolTip="MM/DD/YYYY" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onblur="document.getElementById('TextBox6').focus()"  onClick="showCalendarControl(TextBox6); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
              
           </td>
          </tr>
          
          <tr><td align="right" style="padding-right: 5px"><b>Begining Customer PO#:</b></td>
          <td nowrap><asp:TextBox ID="BeginCustpoTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="custpolook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Customer PO#:</b></td>
          <td nowrap><asp:TextBox ID="EndCustpoTextBox"  Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="custpo2look(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
          
          <tr><td align="right" style="padding-right: 5px"><b>Begining Job#:</b></td>
          <td nowrap><asp:TextBox ID="BeginjobTextBox" Width="67px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="job1look(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
           <asp:TextBox ID="beginjob2TextBox" Width="30px" runat="server"></asp:TextBox>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Job#:</b></td>
          <td nowrap><asp:TextBox ID="EndjobTextBox"  Width="67px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="jobReplook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          <asp:TextBox ID="endjob2TextBox" Width="30px" runat="server"></asp:TextBox>
          </td>
          </tr>
          
          
        <tr><td align="right" style="padding-right: 5px"><b>Begining Item #:</b></td>
          <td nowrap><asp:TextBox ID="TextBox3" Width="100px" runat="server"></asp:TextBox>
           <a href="#" tabindex="1" onClick="Relook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Item#:</b></td>
          <td nowrap><asp:TextBox ID="TextBox4"  Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="Relook2(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
          
        <tr><td align="right" style="padding-right: 5px"><b>Beginning Sales Rep#:</b></td>
        <td><asp:TextBox ID="BeginSalesmanTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          
        <td align="right" style="padding-right: 5px"><b>Ending Sales Rep#:</b></td>
          <td><asp:TextBox ID="EndingSalesmanTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="smancopylook1(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          </tr>
        
          
        
        </table>
          
        
        <table class="shade" width="700px">
           <tr><td nowrap align="left" style="padding-left:10px"><b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
           Sort By? &nbsp;
           <asp:RadioButtonList ID="RadioButtonList1" RepeatColumns="8" RepeatLayout="flow" runat="server">
           <asp:ListItem Text="PO#"></asp:ListItem>
           <asp:ListItem Text="Item"></asp:ListItem>
           <asp:ListItem Text="Cust Part"></asp:ListItem>
           <asp:ListItem Text="Fg Item Name"></asp:ListItem>
           <asp:ListItem Text="Order#"></asp:ListItem>
           <asp:ListItem Text="Due Date"></asp:ListItem>
           </asp:RadioButtonList>
           </b></td></tr>
           <tr><td nowrap align="left" style="padding-left:10px"><b>&nbsp;&nbsp;&nbsp;&nbsp;Job Status &nbsp;
           <asp:RadioButtonList ID="RadioButtonList2" RepeatColumns="3" RepeatLayout="flow" runat="server">
           <asp:ListItem Text="Open"></asp:ListItem>
           <asp:ListItem Text="Closed"></asp:ListItem>
           <asp:ListItem Text="All"></asp:ListItem>
          </asp:RadioButtonList>
           </b></td></tr>
           
           <tr><td nowrap align="left" style="padding-left:10px"><b>Order Status &nbsp;
           <asp:RadioButtonList ID="RadioButtonList3" RepeatColumns="3" RepeatLayout="flow" runat="server">
           <asp:ListItem Text="Open"></asp:ListItem>
           <asp:ListItem Text="Closed"></asp:ListItem>
           <asp:ListItem Text="All"></asp:ListItem>
          </asp:RadioButtonList>
           </b></td></tr>
           
           <tr><td nowrap align="left" style="padding-left:10px"><b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
           Print &nbsp;
           <asp:RadioButtonList ID="RadioButtonList4" RepeatColumns="2" RepeatLayout="flow" runat="server">
           <asp:ListItem Text="Invoice Amount"></asp:ListItem>
           <asp:ListItem Text="Balance Due"></asp:ListItem>
           
          </asp:RadioButtonList>
           </b></td>
           <td><b>
           <asp:CheckBox ID="PageBreakCheckbox" runat="server" Text="Page Break By Sales Rep?" />
           </b></td>
           </tr>
           <tr>
           <td><b>
           <asp:CheckBox ID="CheckBox4" runat="server" Text="Drop Order Underrun%" /></b>
           <br />
           <b><asp:CheckBox ID="CheckBox5" runat="server" Text="Print Job Qty Details?" /></b>
           <br />
           <b><asp:CheckBox ID="CheckBox6" runat="server" Text="Include Zero Order Balance Items?" />
           </b><br/><b><asp:CheckBox ID="CheckBox_SchRel" runat="server" Text="Show Scheduled Releases?" /></b>
           </td>
           <td>
           <fieldset>
           <table>
           <tr>
           <td nowrap><b>Only Show QOH that is:</b></td></tr>
          <tr> <td nowrap><b>Older Than <asp:TextBox ID="olderTextbox" runat="server" Width="50px"></asp:TextBox>Days</b></td>
          </tr>
           <tr>
           <td nowrap><b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
           As of <asp:TextBox ID="asofTextBox" ToolTip="MM/DD/YYYY" MaxLength="12" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  runat="server" Width="60px"></asp:TextBox></b>
           <a href="#" tabindex="1" onblur="document.getElementById('asofTextBox').focus()"   onClick="showCalendarControl(asofTextBox); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
           
           </td>
           </tr>
           <tr>
           <td nowrap><b><asp:CheckBox ID="Checkbox8" runat="server" Text="Include Items with Zero QOH?" /></b></td>
           </tr>
           
           </table>
           </fieldset>
           </td>
           </tr>
           
           <tr><td nowrap align="left" style="padding-left:10px"><b>PO# From? &nbsp;
           <asp:RadioButtonList ID="RadioButtonList5" RepeatColumns="2" RepeatLayout="flow" runat="server">
           <asp:ListItem Text="Header"></asp:ListItem>
           <asp:ListItem Text="Line"></asp:ListItem>
           
          </asp:RadioButtonList>
           </b></td></tr>
           
         
         <tr><td align="left" style="padding-left:10px"><b>
         
             &nbsp;&nbsp;&nbsp;
            
             &nbsp;&nbsp;&nbsp;
             </td>         
         </tr>
         
         
         
         
        <tr><td colspan="2">
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" />
             &nbsp;&nbsp;&nbsp;&nbsp;
              <asp:Label ID="OutputLabel" runat="server" Text="Print:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              &nbsp;
              <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
             </td>
          </tr>     
         </table>
       
          
        <asp:FormView ID="FormView1" Visible="false"   runat="server" DataSourceID="ObjectDataSource1">
             
              
              <ItemTemplate>
                  vBalFile:
                  <asp:Label ID="vBalFileLabel" runat="server" Text='<%# Bind("[vBalFile-job]") %>'></asp:Label><br />
                                  
              </ItemTemplate>
            
           
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
               SelectMethod="BalancePoJob" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmActBal" Type="String" />
                  <asp:Parameter Name="prmBegcust" Type="String" />
                  <asp:Parameter Name="prmEndCust" Type="String" />         
                  <asp:Parameter Name="prmBegOrdDate" Type="DateTime" />
                  <asp:Parameter Name="prmEndOrdDate" Type="DateTime" />
                  <asp:Parameter Name="prmBegCustPo" Type="String" />
                  <asp:Parameter Name="prmEndCustPo" Type="String" />
                  <asp:Parameter Name="prmBegJob" Type="String" />
                  <asp:Parameter Name="prmEndJob" Type="String" />
                  <asp:Parameter Name="prmBegJob2" Type="String" />
                  <asp:Parameter Name="prmEndJob2" Type="String" />
                  <asp:Parameter Name="prmBegItem" Type="String" />
                  <asp:Parameter Name="prmEndItem" Type="String" />
                  <asp:Parameter Name="prmBegSman" Type="String" />
                  <asp:Parameter Name="prmEndSman" Type="String" />
                  <asp:Parameter Name="prmSort" Type="String" />
                  <asp:Parameter Name="prmJobStat" Type="String" />
                  <asp:Parameter Name="prmOrdStat" Type="String" />
                  <asp:Parameter Name="prmPrint" Type="String" />
                  <asp:Parameter Name="prmUnderrun" Type="String" />
                  <asp:Parameter Name="prmJobqty" Type="String" />
                  <asp:Parameter Name="prmZerobal" Type="String" />
                  <asp:Parameter Name="prmPofrom" Type="String" />
                  <asp:Parameter Name="prmDays" Type="Int32" />
                  <asp:Parameter Name="prmDate" Type="DateTime" />
                  <asp:Parameter Name="prmZeroqoh" Type="String" />
                  <asp:Parameter Name="prmPageBreak" Type="String" />
                  <asp:Parameter Name="prmSchRel" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
                  
              </SelectParameters>
          </asp:ObjectDataSource>
          
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

