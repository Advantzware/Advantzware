<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="estmarg_report" Codebehind="estmarg_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Estimates List w/Margins</title>
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
        var beginc = document.getElementById("BeCustTextBox");
        var endc = document.getElementById("EndcustTextBox");
            endc.value=beginc.value;
    }
    
    
    var cval = "";

    function contactcustomerlook(val) {
        cval = val;
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
    if (cval == 1) {
        document.forms[0].BeCustTextBox.value = ReturnObj1;
        document.forms[0].EndcustTextBox.value = ReturnObj1;
        document.forms[0].EndcustTextBox.focus();
    }
    else if (cval == 2) {
    document.forms[0].EndcustTextBox.value = ReturnObj1;
    document.forms[0].EndcustTextBox.focus();
    }    
}

function Relook(){ 
  var NewWindow = window.open("reorder_item_lookup.aspx","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup(ReturnObj1){
    document.forms[0].TextBox3.value = ReturnObj1;
    document.forms[0].TextBox3.focus();
}
function Relook2(){ 
  var NewWindow = window.open("reorder_item_lookup2.aspx","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup2(ReturnObj1){
    document.forms[0].TextBox4.value = ReturnObj1;
    document.forms[0].TextBox4.focus();
}
function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
    document.forms[0].TextBox1.value = obj;    
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
    document.forms[0].BeSmanTextBox.value = ReturnObj1;
    document.forms[0].BeSmanTextBox.focus();
  }
  function smancopylook1(){ 
  var NewWindow = window.open("sman_copylookup.aspx","smancopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function smancopyLookup(ReturnObj1,ReturnObj2){
    document.forms[0].EndSmanTextBox.value = ReturnObj1;
    document.forms[0].EndSmanTextBox.focus();
}
var eval = "";
 function estinfolook(val) {
     eval = val;
     var NewWindow = window.open("est_info_lookup.aspx", "EstimateInformation", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
 function EstimateInfoLookup(ReturnObj1) {
     if (eval == "1") {
         document.forms[0].BeEstTextBox.value = ReturnObj1;
         document.forms[0].BeEstTextBox.focus();
     }
     else if (eval == "2") {
         document.forms[0].EndEstTextBox.value = ReturnObj1;
         document.forms[0].EndEstTextBox.focus();
     }
 }    


    
   


    </script> 

      <link href="include/tree.css" rel="stylesheet" type="text/css" />
      <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='BeCustTextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div >       
            
                       
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Estimates List w/Margins &nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
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
           
      <table class="shade" style="width: 620px">
      <tr> <td align="right" style="padding-right: 5px; " nowrap><b>Begining Customer:</b></td>
      <td><asp:TextBox ID="BeCustTextBox" Width="95px" onkeyup="samevalue()"  runat="server"></asp:TextBox>
      <a href="#" tabindex="1" onClick="contactcustomerlook(1); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
      <td align="right" style="padding-right: 5px" nowrap><b>Ending Customer:</b></td>
          <td style="width: 187px">
            <asp:TextBox ID="EndcustTextBox" Width="95px"   runat="server"></asp:TextBox>            
            <a href="#" tabindex="1" onClick="contactcustomerlook(2); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td></tr>
            <tr><td align="right" style="padding-right: 5px; " nowrap><b>Begining Sales Rep#:</b></td>
          <td nowrap><asp:TextBox ID="BeSmanTextBox" Width="95px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px" nowrap><b>Ending sales Rep#:</b></td>
          <td nowrap style="width: 187px"><asp:TextBox ID="EndSmanTextBox" Width="95px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="smancopylook1(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
          <tr><td align="right" style="padding-right: 5px; " nowrap><b>Begining Estimate#:</b></td>
          <td nowrap><asp:TextBox ID="BeEstTextBox" Width="95px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="estinfolook(1); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px" nowrap><b>Ending Estimate#:</b></td>
          <td nowrap style="width: 187px"><asp:TextBox ID="EndEstTextBox" Width="95px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="estinfolook(2); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
    
      <tr><td align="right" style="padding-right: 5px; " nowrap><b>Begining Add Date:</b></td>
          <td>
            <asp:TextBox ID="BeAddTextBox" onfocus="this.select();javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  runat="server" Width="95px"></asp:TextBox>
            <a href="#" onblur="document.getElementById('BeAddTextBox').focus()"  tabindex="1" onClick="showCalendarControl(BeAddTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
          </td>
      <td nowrap align="right" style="padding-right: 5px"><b>Ending Add Date:</b></td>
       <td  style="padding-right: 5px">
            <asp:TextBox ID="EndAddTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  runat="server" Width="95px"></asp:TextBox>
              <a href="#" onblur="document.getElementById('EndAddTextBox').focus()"  tabindex="1" onClick="showCalendarControl(EndAddTextBox); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
      </tr>
      <tr><td align="right" style="padding-right: 5px; " nowrap><b>Begining Mod Date:</b></td>
          <td>
            <asp:TextBox ID="BeModTextBox" onfocus="this.select();javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  runat="server" Width="95px"></asp:TextBox>
            <a href="#" onblur="document.getElementById('BeModTextBox').focus()"  tabindex="1" onClick="showCalendarControl(BeModTextBox); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
          </td>
      <td nowrap align="right" style="padding-right: 5px"><b>Ending Mod Date:</b></td>
       <td  style="padding-right: 5px">
            <asp:TextBox ID="EndModTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  runat="server" Width="95px"></asp:TextBox>
              <a href="#" onblur="document.getElementById('EndModTextBox').focus()"  tabindex="1" onClick="showCalendarControl(EndModTextBox); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
      </tr>
            
        
       
        </table>
         
        
        <table class="shade" width="620px">
                  
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
                  wmarginsfile:
                  <asp:Label ID="wmarginsfileLabel" runat="server" 
                      Text='<%# Bind("wmarginsfile") %>'></asp:Label><br />
                  
              </ItemTemplate>
               
               
          </asp:FormView>
      
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectEstimatemargRep" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String"  />
                  <asp:Parameter Name="prmBegCust" Type="String" />
                  <asp:Parameter Name="prmEndCust" Type="string" />
                  <asp:Parameter Name="prmBegSman" Type="string" />
                  <asp:Parameter Name="prmEndSman" Type="string" />
                  <asp:Parameter Name="prmBegEst" Type="string" />
                  <asp:Parameter Name="prmEndEst" Type="String" />
                  <asp:Parameter Name="prmAddDate" Type="string" />
                  
                  <asp:Parameter Name="prmEndAddDate" Type="String" />
                  <asp:Parameter Name="prmModDate" Type="String" />
                  <asp:Parameter Name="prmEndModDate" Type="String" />
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
