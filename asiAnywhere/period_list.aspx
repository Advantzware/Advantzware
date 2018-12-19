<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="period_list" Codebehind="period_list.aspx.cs" %>

<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Period</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
   <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = "JavaScript" src="include/CalendarControl.js">
    </script>
  
    
<script language="javascript" >


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
   
window.onload=setfocus;
function setfocus()
{
    if(document.getElementById("FormView1_vcustnoTextBox"))
    {
        var cust=document.getElementById("FormView1_vcustnoTextBox");
        cust.focus();
    }
    else if(document.getElementById("FormView1_vcustnameTextBox"))
        {
            var name=document.getElementById("FormView1_vcustnameTextBox");
            name.focus();
        }  
}

function preLeave( fieldObj, fieldType, fieldFormat ){
fieldObj.style.backgroundColor='Window';
    fieldObj.style.color='WindowText';
  fieldType = fieldType.toLowerCase();
 
  if((fieldType == "") || (fieldType == "text")){
     leaveField( fieldObj );
  }



if(fieldType == "number"){
     if(fieldFormat == ""){ var numFormat = "(>>>>9)";
     }else{ var numFormat = fieldFormat; }
     checkNum(numFormat,fieldObj,'?','?',0);
  }      
}

function preEnter( fieldObj, canEdit ){
fieldObj.style.backgroundColor='blue';
    fieldObj.style.color = 'white';
  if(canEdit == "no"){
     fieldObj.blur();
     leaveField( fieldObj );      
  }
 
  enterField( fieldObj );
  return;
}
    

 
 
function focusval(obj)
{
    obj.style.backgroundColor='blue';
    obj.style.color = 'white';
}
function blurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
}
function expblurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText'; 
    
    if(document.getElementById("FormView1_vcustnoTextBox"))
    {
        var cust=document.getElementById("FormView1_vcustnoTextBox");
        cust.focus();
    }
    else if(document.getElementById("FormView1_vcustnameTextBox"))
    {
        var name=document.getElementById("FormView1_vcustnameTextBox");
        name.focus();
    }
}


function datevalcalender()
{
    var adddate = document.getElementById("FormView1_vdate1TextBox");
    //obj.style.backgroundColor='blue';
    //obj.style.color = 'white';
    adddate.style.backgroundColor = 'blue';
    adddate.style.color= 'white';
    showCalendarControl(adddate);
}
function espdatecal()
{
    var adddate = document.getElementById("FormView1_vdatefield2TextBox");   
    adddate.style.backgroundColor = 'blue';
    adddate.style.color= 'white';
    showCalendarControl(adddate);

}

   </script>

 </head>    

   <body>
        <form id="frmList" runat="server"  defaultfocus='comp_TextBox' defaultbutton="btnSearch">   
        <hd:header id="Header1" runat="server"></hd:header>
        <table><tr><td><div>
            <table align="left" border="1" width="95%">
                <tr class="topheadcolor">                                      
                 
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" ToolTip="Add" OnClick="img_btn_add_click" />
                        </td>                       
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" Width="35px" ImageUrl="~/Images/exit-au.bmp" ToolTip="LogOut" OnClick="hlnkLogOut_Click" />
                        </td>
                        <td nowrap> &nbsp;</td>
                </tr>
            </table></div></td></tr>
            <tr><td>
            
                
      <div>
          <asp:ScriptManager ID="ScriptManager1" runat="server">
          </asp:ScriptManager>
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>Periods&nbsp;</b></font></TD>
          <TD >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          
            &nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
            &nbsp;<b>Company:</b>&nbsp;  <asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <table><tr bgcolor="gray"><td><div  id="navigation" style="width:100%">
	  <ul nowrap> <li >
      <asp:LinkButton ID="lnk_Listcompany" runat="server"  OnClick="lnk_listcompany_Click" >Brws Company</asp:LinkButton></li>
      <li ><asp:LinkButton ID="lnk_viewcompany" runat="server" OnClick="lnk_viewcompany_Click" > View Company</asp:LinkButton></li>
      <li class="selected" ><asp:LinkButton ID="lnk_listperiod" runat="server" OnClick="lnk_listperiod_Click" >Open Periods </asp:LinkButton></li>
      <li ><asp:LinkButton ID="lnk_viewperiod" runat="server" OnClick="lnk_viewperiod_Click" > View Period</asp:LinkButton></li></ul></div>
      
      
      
      </td>
      </tr></table>
     <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource3" >
                    <ItemTemplate>
                    <fieldset style="width:400px" class="shade"><legend>Reference Information</legend>
                    <table><tr><td>
                    <b>Company:</b></td>
                    <td><asp:Label ID="companyTextBox" Width="70px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("company") %>' /></td>
                    <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Name:</b></td>
                    <td><asp:Label ID="cnameTextBox" Width="190px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("cname") %>' /></td></tr></table>
                    </fieldset>
                    </ItemTemplate>
                    </asp:FormView>
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" width='400px' border="0" >
        <TR >
          <TD class="shade">
            <fieldset>
            <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"   >
              <TR>
                <TD align="center" width="50" class="shade" bgcolor="gray"><nobr>
                  <%--<asp:linkbutton id="btnAdd" runat="server" OnClick="btnAdd_Click">Add new</asp:linkbutton></nobr>--%>
                <asp:button id="btnSearch" runat="server" OnClick=" btnSearch_Click" Width="40px" CssClass="button" Text="Go" ></asp:button>
                <br />
                <br />
                  <asp:button id="btnShowAll" runat="server" OnClick=" btnShowAll_Click" Width="40px" CssClass="button" Text="All" ></asp:button>
                </TD>               
                          
                <TD id="tdSearch" runat="server" class="shade" vAlign="middle" align="left" >&nbsp;                
                <table >
                <tr><td ><b>Year#</b></td><td><b> Records/Page</b></td></tr>
                <tr><td>
                    <asp:TextBox ID="comp_TextBox" Width="100px" runat="server"></asp:TextBox>
                     </td>
                    
                    <TD id="tdPageCount" runat="server" class="shade"  >
          <table><tr><td align="left">
           
            <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2">
                          <EditItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                                  Text="Update">
                              </asp:LinkButton>
                              <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </EditItemTemplate>
                          <InsertItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                                  Text="Insert">
                              </asp:LinkButton>
                              <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </InsertItemTemplate>
                          <ItemTemplate>
                             
                              <asp:TextBox ID="aLineLabel" runat="server" Width="40px" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                               <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="aLineLabel" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
                          </ItemTemplate>
                      </asp:FormView>
                      <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
                          SelectMethod="SelectRows" TypeName="Order">
                          <SelectParameters>
                              <asp:SessionParameter Name="prmUser" SessionField="Rowuser" Type="String" />
                              <asp:SessionParameter Name="vLine" Type="Int32" SessionField="gridsize" />
                          </SelectParameters>
                      </asp:ObjectDataSource>
          </td></tr></table>  
                </TD>                    
                    </tr></table>
                  
                </TD>                 
                
              </TR>
            </TABLE>
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
          </fieldset></td>
        </tr>
        <tr>
          <td>

    
           
            
          </TD>
        </TR>
        <tr><td> 
            
            <asp:GridView ID="GridView1" runat="server" DataSourceID="ObjectDataSource1" EmptyDataText=" Records not  Found" AllowPaging="True" AllowSorting="True" Width="400px" AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndex">
                <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                        HorizontalAlign="Center" VerticalAlign="Middle" />
                        
                <Columns>
                    <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="images\sel.gif" SelectText="" >
                    <ItemStyle Width="10px" />                    
                    </asp:CommandField>
                   
                    <asp:BoundField DataField="peryr" HeaderText="Year" SortExpression="peryr" />                    
                    <asp:BoundField DataField="perpnum" HeaderText="Period" 
                        SortExpression="perpnum" />                    
                  
                    <asp:BoundField DataField="perpst" HeaderText="Start Date" 
                        SortExpression="perpst" />
                    <asp:BoundField DataField="perpend" HeaderText="End Date" 
                        SortExpression="perpend" />
                    <asp:BoundField DataField="perpstat" HeaderText="Status" 
                        SortExpression="perpstat" />
                   
                    <asp:TemplateField Visible="false"  HeaderText="Reckey"  >
                    <ItemTemplate>      
                    <asp:Label id="reckeyLabel" runat="server" Text='<%# Bind("preckey") %>'></asp:Label>
                    </ItemTemplate>
                    </asp:TemplateField>
                                       
                </Columns>
                    <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle  ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    <AlternatingRowStyle CssClass="GridItemOdd" />
                    <RowStyle  CssClass="shade" />
            </asp:GridView>
            <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="PeriodList" 
                TypeName="ledger">
                <SelectParameters>
                    <asp:Parameter DefaultValue="search" Name="prmAction" Type="String" />                    
                    <asp:Parameter Name="prmComp"  Type="string" />
                    <asp:Parameter Name="prmUser"  Type="string" />                     
                    <asp:SessionParameter SessionField="company_list_company" Name="prmCompany" Type="String" />
                    <asp:SessionParameter SessionField="period_list_year"  Name="prmyr" Type="String" />
                    <asp:Parameter Name="prmpnum" Type="String" />
                    <asp:Parameter Name="prmpst" Type="String" />
                    <asp:Parameter Name="prmpend" Type="String" />
                    <asp:Parameter Name="prmpstat" Type="String" />
                    <asp:Parameter Name="prmReckey" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>
            
             <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="PeriodList" TypeName="ledger">
              <SelectParameters>
                  <asp:Parameter DefaultValue="CompanyDetail" Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />                  
                  <asp:Parameter Name="prmUser" Type="String"  DefaultValue="" />
                  <asp:SessionParameter SessionField="company_list_company" Name="prmCompany" Type="String" />
                  <asp:Parameter Name="prmyr" Type="String" />
                  <asp:Parameter Name="prmpnum" Type="String" />                 
                  <asp:Parameter Name="prmpst" Type="String" />
                  <asp:Parameter Name="prmpend" Type="String" />
                  <asp:Parameter Name="prmpstat" Type="String" />
                  <asp:SessionParameter SessionField="period_list_reckey_name" Name="prmReckey" 
                      Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
          
        </td></tr>
      </TABLE>      
    </div>
    </td></tr></table>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

