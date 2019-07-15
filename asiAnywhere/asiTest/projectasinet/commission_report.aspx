<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="commission_report" Codebehind="commission_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Commission Report</title>
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
    var beginc=document.getElementById("TextBox5");
    var endc=document.getElementById("TextBox6");
    endc.value=beginc.value;
    }
    
    function samevalue2()
    {
    var beginc=document.getElementById("TextBox5");
    var endc=document.getElementById("TextBox6");
    if(endc.value!=beginc.value)
    {
    /*alert("Begin and End Customer Value must be same");
    endc.value=beginc.value;
    endc.focus();
    */
    }
    }

    var cval = "";

    function contactcustomerlook(val) {
        cval = val;
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
    if (cval == 1) {
        document.forms[0].TextBox5.value = ReturnObj1;
        document.forms[0].TextBox6.value = ReturnObj1;
    }
    else if (cval == 2) {
        document.forms[0].TextBox6.value = ReturnObj1;
    }    
}

function Relook(){ 
  var NewWindow = window.open("reorder_item_lookup.aspx","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup(ReturnObj1){ 
  document.forms[0].TextBox3.value = ReturnObj1;
}
function Relook2(){ 
  var NewWindow = window.open("reorder_item_lookup2.aspx","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup2(ReturnObj1){ 
  document.forms[0].TextBox4.value = ReturnObj1;
}
function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].TextBox1.value=obj;
}
function Datelook1()
{
  document.forms[0].TextBox1.value="";
  Datelook();
}

function Datelook2(){ 
  var NewWindow = window.open("date3_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup3(obj)
{
  document.forms[0].TextBox2.value=obj;
}
function Datelook3()
{
  document.forms[0].TextBox2.value="";
  Datelook2();
}
function salesreplook(){ 
  var NewWindow = window.open("salesrep_lookup.aspx","SalesRepLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function SalesRepLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].TextBox3.value = ReturnObj1;
  }
  function smancopylook1(){ 
  var NewWindow = window.open("sman_copylookup.aspx","smancopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function smancopyLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].TextBox4.value = ReturnObj1;
 }
 function procatlook(){ 
  var NewWindow = window.open("procat_lookup.aspx","ProcatLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function procatLookup(ReturnObj1){ 
  document.forms[0].TextBox7.value = ReturnObj1;
}

function checkEnabled() {
    if (document.getElementById("CheckBox1").checked || document.getElementById("CheckBox2").checked) {
        document.getElementById("RadioButtonList3").childNodes[0].disabled = false;
        document.getElementById("RadioButtonList3").childNodes[1].disabled = false;
        document.getElementById("RadioButtonList3").childNodes[2].disabled = false;
        document.getElementById("RadioButtonList3").childNodes[3].disabled = false;
    }
    else {
        document.getElementById("RadioButtonList3").childNodes[0].disabled = true;
        document.getElementById("RadioButtonList3").childNodes[1].disabled = true;
        document.getElementById("RadioButtonList3").childNodes[2].disabled = true;
        document.getElementById("RadioButtonList3").childNodes[3].disabled = true;
    } 
}
function changemonth() {

    var mon = document.getElementById("TextBox_period").value;
    var bedate = document.getElementById("TextBox1");
    var endate = document.getElementById("TextBox2");
    if (document.getElementById("RadioButtonList2").childNodes[0].checked == true) {
       
        if (parseInt(mon) <= 12 && parseInt(mon) > 0 ) {
            var today = new Date;
            today = mon + "/" + "01" + "/" + today.getFullYear();
            var year = new Date;
            year = year.getFullYear();
            bedate.value = today;
            var dayofmonth = "";
            var m = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
           
            if (mon != 2)
                dayofmonth = m[mon - 1];
            if (mon == 2) {
                if (year % 4 != 0)
                    dayofmonth = m[1];
                else if (year % 100 == 0 && mon % 400 != 0)
                    dayofmonth = m[1];
                else
                    dayofmonth = m[1] + 1;
            }

            endate.value = mon + "/" + dayofmonth + "/" + year;

        }
        else {
            alert(mon + "  is not a valid period. ");
            document.getElementById("TextBox_period").focus();
        }
    }
    
   
}

    </script> 

      <link href="include/tree.css" rel="stylesheet" type="text/css" />
      <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox_period'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div >       
            
                       
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Commission Report&nbsp;</b></font></TD>
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
      </TABLE></div><div>
      <asp:HiddenField ID="HiddenField1" runat="server" />
         <asp:HiddenField ID="HiddenField2" runat="server" />
         <asp:HiddenField ID="HiddenField3" runat="server" />
         <asp:HiddenField ID="HiddenField4" runat="server" />
         <asp:HiddenField ID="HiddenField5" runat="server" />
         <asp:HiddenField ID="HiddenField6" runat="server" />
         <asp:HiddenField ID="HiddenField7" runat="server" />
                  
          <asp:Label ID="Label1" Font-Bold="true" ForeColor="red" runat="server" ></asp:Label>&nbsp;
          
          <asp:FormView ID="FormView2" Visible="false" runat="server" DataSourceID="ObjectDataSource2">
                            
              <ItemTemplate>                  
                  <asp:Label ID="CustLabel" Visible="false" runat="server" Text='<%# Bind("Cust") %>'></asp:Label><br />
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="FillAlphabeticList" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
           <table class="shade" width="520px">
           
         <tr><td nowrap colspan="2" style="width: 617px" ><b>PTD/YTD?</b>
             <asp:RadioButtonList ID="RadioButtonList2" onclick="changemonth()" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="2" Font-Bold ="true" runat="server" Width="233px">
                 <asp:ListItem      Text="PTD" />
                  <asp:ListItem     Text="YTD" />
         </asp:RadioButtonList> 
         </td></tr></table>
      <table class="shade" style="width: 620px">
     <tr><td align="right" style="padding-right: 5px; "><b>For Period:</b></td>
          <td><asp:TextBox ID="TextBox_period" onblur="changemonth()"  Width="40px" runat="server"></asp:TextBox></td>
        </tr>
      <tr><td align="right" style="padding-right: 5px; " nowrap><b>Begining Date:</b></td>
          <td>
            <asp:TextBox ID="TextBox1" onfocus="this.select();javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  runat="server" Width="95px"></asp:TextBox>
            <a href="#" onblur="document.getElementById('TextBox1').focus()"  tabindex="1" onClick="showCalendarControl(TextBox1); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
          </td>
      <td nowrap align="right" style="padding-right: 5px"><b>Ending Date:</b></td>
       <td  style="padding-right: 5px">
            <asp:TextBox ID="TextBox2" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  runat="server" Width="95px"></asp:TextBox>
              <a href="#" onblur="document.getElementById('TextBox2').focus()"  tabindex="1" onClick="showCalendarControl(TextBox2); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
      </tr>
      <tr><td align="right" style="padding-right: 5px; " nowrap><b>Begining Salesman#:</b></td>
          <td nowrap><asp:TextBox ID="TextBox3" Width="95px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px" nowrap><b>Ending salesman#:</b></td>
          <td nowrap style="width: 187px"><asp:TextBox ID="TextBox4" Width="95px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="smancopylook1(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
          
            <tr> <td align="right" style="padding-right: 5px; " nowrap><b>Begining Customer:</b></td>
          <td><asp:TextBox ID="TextBox5" Width="95px" onkeyup="samevalue()"  runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(1); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          
          
        <td align="right" style="padding-right: 5px" nowrap><b>Ending Customer:</b></td>
          <td style="width: 187px">
            <asp:TextBox ID="TextBox6" Width="95px"  onkeyup="samevalue2()" runat="server"></asp:TextBox>            
            <a href="#" tabindex="1" onClick="contactcustomerlook(2); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td></tr>
        
        
        <tr><td align="right" style="padding-right: 5px; " nowrap><b>For Category:</b></td>
          <td colspan="3"><asp:TextBox ID="TextBox7" Width="95px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="procatlook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          <font color="#000084">(Leave Blank for all Categories)</font>
          </td>
        </tr>
        </table>
          <table class="shade" width="620px">
          <tr>
            <td align="center" style="padding-right: 5px; " nowrap >
                <asp:CheckBox ID="CheckBox1" runat="server" onclick="checkEnabled();" /><b>Detailed?</b>                            
                <asp:CheckBox ID="CheckBox2" runat="server" onclick="checkEnabled();"  /><b>Show Prep Charges?</b>
                <asp:CheckBox ID="CheckBox3" runat="server"  /><b>Print Cost and Profit?</b>
            </td>
          </tr>
          </table>
        
        <table class="shade" width="620px">
           
         <tr><td nowrap colspan="2" align="center" style="width: 605px"><b>Cost?</b>
             <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                              
                 <asp:ListItem      Text="FG" />
                  <asp:ListItem     Text="Estimated Board" />
                 <asp:ListItem      Text="Order" />                   
         </asp:RadioButtonList> </td></tr>
         
         <tr><td nowrap colspan="2" align="center" style="width: 605px"><b>Print?</b>
             <asp:RadioButtonList ID="RadioButtonList3" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                              
                 <asp:ListItem      Text="Customer Part#" />
                  <asp:ListItem     Text="FG Item#" />                                  
         </asp:RadioButtonList> </td></tr>
         
         <tr><td colspan="2" align="left" style="padding-left:10px">
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         <b>Output to?  <asp:RadioButtonList ID="RadioButtonList_out"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                 <asp:ListItem   Value="No"   Text="Text File" />
                 <asp:ListItem  Value="Yes"  Text="Excel" />                 
         </asp:RadioButtonList></b></td></tr>
         
         <tr><td style="width: 615px" colspan="2">
              <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" />
               &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
              </td>
          </tr>     
         </table>
           <asp:FormView ID="FormView1" Visible="False" runat="server" 
              DataSourceID="ObjectDataSource1" >                                           
                            
              <ItemTemplate>
                  commrepfile:
                  <asp:Label ID="commrepfileLabel" runat="server" 
                      Text='<%# Bind("commrepfile") %>'></asp:Label><br />
                  
              </ItemTemplate>
               
               
          </asp:FormView>
      
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="Commission" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="CommRep" Type="String"  />
                  <asp:Parameter Name="prmPtdYtd" Type="String" />
                  <asp:Parameter Name="prmPeriod" Type="int32" />
                  <asp:Parameter Name="prmBegDate" Type="datetime" />
                  <asp:Parameter Name="prmEndDate" Type="datetime" />
                  <asp:Parameter Name="prmBegSales" Type="String" />
                  <asp:Parameter Name="prmEndSales" Type="String" />
                  <asp:Parameter Name="prmBegCust" Type="String" />
                  <asp:Parameter Name="prmEndCust" Type="string" />
                  <asp:Parameter Name="prmCategory" Type="string" />
                  <asp:Parameter Name="prmDetailed" Type="string" />
                  <asp:Parameter Name="prmPrepCharg" Type="string" />
                  <asp:Parameter Name="prmCostProfit" Type="String" />
                  <asp:Parameter Name="prmCost" Type="string" />
                  
                  <asp:Parameter Name="prmPrintCustPart" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
                  
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>
<script language="javascript" type="text/javascript">
    if (document.getElementById("CheckBox1").checked || document.getElementById("CheckBox2").checked) {
        document.getElementById("RadioButtonList3").childNodes[0].disabled = false;
        document.getElementById("RadioButtonList3").childNodes[1].disabled = false;
        document.getElementById("RadioButtonList3").childNodes[2].disabled = false;
        document.getElementById("RadioButtonList3").childNodes[3].disabled = false;
    }
    else {
        document.getElementById("RadioButtonList3").childNodes[0].disabled = true;
        document.getElementById("RadioButtonList3").childNodes[1].disabled = true;
        document.getElementById("RadioButtonList3").childNodes[2].disabled = true;
        document.getElementById("RadioButtonList3").childNodes[3].disabled = true;
    }
</script>
