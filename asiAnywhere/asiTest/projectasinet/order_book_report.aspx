<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="order_book_report_list" Codebehind="~/order_book_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Order Booked</title>
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
        document.forms[0].TextBox1.value = ReturnObj1;
        document.forms[0].TextBox2.value = ReturnObj1;
    }
    else if (cval == 2) {
        document.forms[0].TextBox2.value = ReturnObj1;
    }
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
 


function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].bedateTextBox.value=obj;
}

function Datelook1()
{
  document.forms[0].bedateTextBox.value="";
  Datelook();
}
function Date2look(){ 
  var NewWindow = window.open("date_lookup2.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup2(obj)
{
  document.forms[0].enddateTextBox.value=obj;
}

function Datelook2()
{
  document.forms[0].enddateTextBox.value="";
  Date2look();
}

function procatlook() {
    var NewWindow = window.open("procat_lookup.aspx", "ProcatLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function procatLookup(ReturnObj1) {
    document.forms[0].beproTextBox.value = ReturnObj1;
}
function procat2look() {
    var NewWindow = window.open("procat_lookup2.aspx", "ProcatLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function procat2Lookup(ReturnObj1) {
    document.forms[0].endproTextBox.value = ReturnObj1;
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
         
          <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
          
           <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2">
                            
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
         
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Orders Booked Report &nbsp;</b></font></TD>
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
      <table class="shade" width="630px">
       
      <tr><td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
          <asp:TextBox ID="TextBox1"  onkeyup="samevalue()"  width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(1); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td><td><asp:TextBox ID="TextBox2" onkeyup="samevalue2()" width="100px" runat="server"></asp:TextBox>
      <a href="#" tabindex="1" onClick="contactcustomerlook(2); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
      </td>
        </tr>
        <tr><td align="right" style="padding-right: 5px"><b>Begining Date:</b></td>
          <td><asp:TextBox ID="bedateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Width="100px" runat="server"></asp:TextBox>
          <a href="#" onblur="document.getElementById('bedateTextBox').focus()" tabindex="1" onClick="showCalendarControl(bedateTextBox); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        <td align="right" style="padding-right: 5px"><b>Ending Date:</b></td>
          <td><asp:TextBox ID="enddateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Width="100px" runat="server"></asp:TextBox>
          <a href="#" onblur="document.getElementById('enddateTextBox').focus()" tabindex="1" onClick="showCalendarControl(enddateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
      <tr><td align="right" style="padding-right: 5px"><b>Begining Sales Rep#:</b></td>
          <td nowrap><asp:TextBox ID="besmanTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Sale Rep#:</b></td>
          <td nowrap><asp:TextBox ID="endsmanTextBox"  Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="smancopylook1(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
          <tr><td align="right" style="padding-right:5px"><b>Begining Product Category</b></td>
          <td>
              <asp:TextBox ID="beproTextBox" Width="100px" runat="server"></asp:TextBox>
              <a href="#" tabindex="1"  onClick="procatlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          <td align="right" style="padding-right:5px"><b>Ending Product Category</b></td>
          <td>
              <asp:TextBox ID="endproTextBox" Width="100px" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="procat2look(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          </tr>
          
        
        </table>
          
        
        <table class="shade" width="630px">
           
         <tr><td colspan="2" align="left" style="padding-left:10px" ><b>Print SqFt or Part#?:</b>
             <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                              
                 <asp:ListItem      Text="Square Ft" />
                  <asp:ListItem     Text="Part#" />                 
         </asp:RadioButtonList> &nbsp; &nbsp;</td></tr>
         <tr><td align="left" style="padding-left:10px"><b><asp:CheckBox ID="CheckBox1" Text="Page By Sales Rep?" runat="server"></asp:CheckBox></b></td><td>
         <b><asp:CheckBox ID="CheckBox2" Text="Sort By Order#?" runat="server"></asp:CheckBox></b>
         </td></tr>
         
         <tr><td align="left" style="padding-left:10px"><b><asp:CheckBox ID="CheckBox3" Text="Display Item Description?" runat="server"></asp:CheckBox></b></td><td>
         <b><asp:CheckBox ID="CheckBox4" Text="Print $/Ton ?" runat="server"></asp:CheckBox></b>
         </td></tr>
         
         <tr><td align="left" style="padding-left:10px"><b><asp:CheckBox ID="CheckBox5" Text="Print Profit?" runat="server"></asp:CheckBox></b></td>
         <td><b><asp:CheckBox ID="CheckBox6" Text="Include Prep/Misc Charge" runat="server"></asp:CheckBox></b>
         </td></tr>
         <tr>
            <td align="left" style="padding-left:10px"><b><asp:CheckBox ID="CheckBox7" Text="Exclude Commission?" runat="server"></asp:CheckBox></b></td>
            <td><b><asp:CheckBox ID="CheckBox8" Text="Print Avail Margin?" runat="server"></asp:CheckBox></b></td>
         </tr>
         
         <tr><td colspan="2" align="left" style="padding-left:10px">
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                <b>Output to?  
                    <asp:RadioButtonList ID="RadioButtonList_out"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                              
                        <asp:ListItem   Value="No"   Text="Text File" />
                        <asp:ListItem  Value="Yes"  Text="Excel" />                 
                    </asp:RadioButtonList>
                </b>
         </td></tr>
         
        <tr><td colspan="3">
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" />
             &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
             </td>
          </tr>     
         </table>
       
          
          <asp:FormView ID="FormView1" Visible="false"  runat="server" DataSourceID="ObjectDataSource1">
                           
              <ItemTemplate>
                  datFile:
                  <asp:Label ID="datFileLabel" runat="server" Text='<%# Bind("datFile") %>'></asp:Label><br />
                  abc:
                  <asp:Label ID="abcLabel" runat="server" Text='<%# Bind("abc") %>'></asp:Label><br />
              </ItemTemplate>
          </asp:FormView>   
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SedsOrderBookRep" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmBookAct" Type="String" />
                  <asp:Parameter Name="prmBeginCust" Type="String" />
                  <asp:Parameter Name="prmEndCust" Type="String" />
                  <asp:Parameter Name="prmBegdate" Type="DateTime" />
                  <asp:Parameter Name="prmEndDate" Type="DateTime" />
                  <asp:Parameter Name="prmBegsman" Type="String" />
                  <asp:Parameter Name="prmEndsman" Type="String" />
                  <asp:Parameter Name="prmBegPro" Type="String" />
                  <asp:Parameter Name="prmEndPro" Type="String" />
                  <asp:Parameter Name="prmSquare" Type="String" />
                  <asp:Parameter Name="prmPsman" Type="String" />
                  <asp:Parameter Name="prmSortOrd" Type="String" />
                  <asp:Parameter Name="prmIdesc" Type="String" />
                  <asp:Parameter Name="prmTon" Type="String" />
                  <asp:Parameter Name="prmPro" Type="String" />
                  <asp:Parameter Name="prmMis" Type="String" />
                  <asp:Parameter Name="prmComm" Type="String" />
                  <asp:Parameter Name="prmAvailMargin" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

