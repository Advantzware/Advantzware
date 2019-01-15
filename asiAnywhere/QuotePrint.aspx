<%@ Page Language="c#" MasterPageFile="~/MasterPage6.master" AutoEventWireup="true" Debug="false" Inherits="QuotePrint" Title="Quote Print" Codebehind="QuotePrint.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">

<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>

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


    </script> 
       
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
          <asp:Label ID="Label1" Font-Bold="true" ForeColor="red" runat="server" ></asp:Label>
          
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
          
        <table class="shade" width="520px">     
            <tr>
                <td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
                    <asp:TextBox ID="TextBox1" OnTextChanged="TextBox1_textchanged" onkeyup="samevalue()" onblur="samevalue()" width="100px" runat="server"></asp:TextBox>
                    <a href="#" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
                <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td><td><asp:TextBox ID="TextBox2"  width="100px" runat="server"></asp:TextBox>
                    <a href="#" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
            </tr>
            <tr>
                <td align="right" style="padding-right: 5px"><b>Begining Quote#:</b></td>
                <td nowrap><asp:TextBox ID="TextBox3" Width="100px" runat="server"></asp:TextBox></td>
                <td align="right" style="padding-right: 5px"><b>Ending Quote#:</b></td>
                <td nowrap><asp:TextBox ID="TextBox4" Width="100px" runat="server"></asp:TextBox></td>
            </tr>    
            <tr>
                <td align="right" style="padding-right: 5px"><b>Begining Department:</b></td>
                <td nowrap><asp:TextBox ID="TextBox5" Width="100px" runat="server"></asp:TextBox></td>
                <td align="right" style="padding-right: 5px"><b>Ending Department:</b></td>
                <td nowrap><asp:TextBox ID="TextBox6" Width="100px" runat="server"></asp:TextBox></td>
            </tr>    
        </table>
        <table class="shade" width="520px">
            <tr>
                <td><asp:CheckBox ID="CheckBox1" runat="server" /></td>  
                <td align="left" style="padding-right: 5px" ><b>Print Department Manufacturing Instructions?</b></td>                           
                <td><asp:CheckBox ID="CheckBox2" runat="server" /></td>
                <td align="left" style="padding-right: 5px"><b>Instructions Span Multiple Pages</b></td>                
            </tr>
            <tr>
                <td><asp:CheckBox ID="CheckBox3" runat="server" /></td>  
                <td align="left" style="padding-right: 5px" ><b>Print Notes per Item or Form?</b></td>                           
                <td><asp:CheckBox ID="CheckBox4" runat="server" /></td>
                <td align="left" style="padding-right: 5px"><b>Print Box Design?</b></td>                
            </tr>
            <tr>
                <td><asp:CheckBox ID="CheckBox5" runat="server" /></td>  
                <td align="left" style="padding-right: 5px" ><b>Print SalesRep Commission?</b></td>                           
                <td><asp:CheckBox ID="CheckBox6" runat="server" /></td>
                <td align="left" style="padding-right: 5px"><b>Print Components?</b></td>                
            </tr>
            <tr>
                <td><asp:CheckBox ID="CheckBox7" runat="server" /></td>  
                <td align="left" style="padding-right: 5px" ><b>Print 2nd Item Description Line?</b></td>                                                       
            </tr>
        </table>                
        <table class="shade" width="520px">                                
            <tr>
                <td nowrap colspan="2">
                    <asp:RadioButtonList ID="RadioButtonList2" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="4" Font-Bold ="true" runat="server">
                        <asp:ListItem      Text="Corr Note" />
                        <asp:ListItem     Text="Fold Note" />
                        <asp:ListItem      Text="Corr/Fold Note" />                  
                        <asp:ListItem      Text="No Note" /> 
                    </asp:RadioButtonList> &nbsp; &nbsp;
                </td>
            </tr>                          
        
        <tr><td >
           <b>Print To:</b><b><asp:RadioButtonList ID="RadioButtonList8" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Pdf" />
                  <asp:ListItem      Text="Excel" />
                  
                 </asp:RadioButtonList></b>
           </td></tr>
        <tr><td>
              <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" />
              <%--<asp:Button ID="submitbutton" runat="server" class="buttonM" Text="Submit" />--%>
              &nbsp;&nbsp;<asp:Label ID="OutputLabel" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
          &nbsp;
          <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink> 
              
              </td>
          </tr>     
         </table>
           <%--<asp:FormView ID="FormView1" runat="server" 
              DataSourceID="ObjectDataSource1" >
                            
               <EditItemTemplate>
                   vFile:
                   <asp:TextBox ID="vFileTextBox" runat="server" Text='<%# Bind("vFile") %>' />
                   <br />
                   vhhhtyh:
                   <asp:TextBox ID="vhhhtyhTextBox" runat="server" Text='<%# Bind("vhhhtyh") %>' />
                   <br />
                   vhhhfds:
                   <asp:TextBox ID="vhhhfdsTextBox" runat="server" Text='<%# Bind("vhhhfds") %>' />
                   <br />
                   <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" 
                       CommandName="Update" Text="Update" />
                   &nbsp;<asp:LinkButton ID="UpdateCancelButton" runat="server" 
                       CausesValidation="False" CommandName="Cancel" Text="Cancel" />
               </EditItemTemplate>
               <InsertItemTemplate>
                   vFile:
                   <asp:TextBox ID="vFileTextBox" runat="server" Text='<%# Bind("vFile") %>' />
                   <br />
                   vhhhtyh:
                   <asp:TextBox ID="vhhhtyhTextBox" runat="server" Text='<%# Bind("vhhhtyh") %>' />
                   <br />
                   vhhhfds:
                   <asp:TextBox ID="vhhhfdsTextBox" runat="server" Text='<%# Bind("vhhhfds") %>' />
                   <br />
                   <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" 
                       CommandName="Insert" Text="Insert" />
                   &nbsp;<asp:LinkButton ID="InsertCancelButton" runat="server" 
                       CausesValidation="False" CommandName="Cancel" Text="Cancel" />
               </InsertItemTemplate>
                            
              <ItemTemplate>
                  vFile:
                  <asp:Label ID="vFileLabel" runat="server" Text='<%# Bind("vFile") %>'></asp:Label><br />
                  vhhhtyh:
                  <asp:Label ID="vhhhtyhLabel" runat="server" Text='<%# Bind("vhhhtyh") %>'></asp:Label><br />
                  vhhhfds:
                  <asp:Label ID="vhhhfdsLabel" runat="server" Text='<%# Bind("vhhhfds") %>'></asp:Label><br />
                 
              </ItemTemplate>
               
          </asp:FormView>--%>
      
          <%--<asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectQuotePrint" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
                  <asp:Parameter Name="prmBeginCust" Type="String" />
                  <asp:Parameter Name="prmEndCust" Type="String" />
                  <asp:Parameter Name="prmBeginDept" Type="String" />
                  <asp:Parameter Name="prmEndDept" Type="String" />
                  
                  <asp:Parameter Name="prmBeginQuote" Type="Int32" />
                  <asp:Parameter Name="prmEndQuote" Type="Int32" />
                  <asp:Parameter Name="prmQuoteList" Type="String" />
                  <asp:Parameter Name="prmInst" Type="String" />
                  <asp:Parameter Name="prmNotesSpanPage" Type="String" />
                  
                  <asp:Parameter Name="prmNote" Type="String" />
                  <asp:Parameter Name="prmPrtBox" Type="String" />
                  <asp:Parameter Name="prmComm" Type="String" />
                  <asp:Parameter Name="prmPrtComp" Type="String" />
                  <asp:Parameter Name="prmPrint2ndDscr" Type="String" />
                  <asp:Parameter Name="prmRsNote" Type="String" />
                  
                  <asp:Parameter Name="prmOut" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>--%>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>          

</asp:Content>