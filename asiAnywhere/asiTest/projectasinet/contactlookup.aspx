<%@ Page Language="C#" AutoEventWireup="true" Inherits="contactlookup" Codebehind="contactlookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">



<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Contacts</title>
    <script language = JavaScript>
   
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
    
    </script>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue">    
    <asp:Panel ID="Panel1" runat="server" DefaultButton="Button1">
    <div>          
        <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
            <tr>
                <td class="shade"><asp:button id="Button1" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>&nbsp;<br /><br />
                    <asp:button id="Button2" runat="server" Width="40px" CssClass="button" Text=" All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;
                </td>
                <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                    <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                    <asp:dropdownlist id="ddlSearchField" runat="server"> 
                        <asp:ListItem Value="firstname">First Name</asp:ListItem>  
                        <asp:ListItem Value="lastname">Last Name</asp:ListItem>                       
                    </asp:dropdownlist>&nbsp;&nbsp;
                    <asp:dropdownlist id="ddlSearchOperation" runat="server">                                      
                        <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                        <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>                 
                    </asp:dropdownlist>
                    <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>                  
                </td>
            </tr>
        </table>
  </div>
  </asp:Panel>
  <div>
            
        <asp:GridView ID="GridView1" runat="server" CssClass="Grid" Width="450px"  AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No records found" AutoGenerateColumns="False" >
         <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="teal" ForeColor="White" />
            <Columns>
              <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.contactlookup('<%#DataBinder.Eval(Container,"DataItem.first_name")%>', '<%#DataBinder.Eval(Container,"DataItem.last_name")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="first_name" HeaderText="First Name" SortExpression="first_name" />
                <asp:BoundField DataField="last_name" HeaderText="Last Name" SortExpression="last_name" />
                <asp:BoundField DataField="addr1" HeaderText="Address" SortExpression="addr1" />
                <asp:BoundField DataField="addr2" HeaderText="Address" SortExpression="addr2" />
                <asp:BoundField DataField="city" HeaderText="City" SortExpression="city" />
                <asp:BoundField DataField="state" HeaderText="State" SortExpression="state" />
                <asp:BoundField DataField="zip" HeaderText="Zip" SortExpression="zip" />
                <asp:BoundField DataField="country" HeaderText="Country" SortExpression="country" />
            </Columns>
        </asp:GridView>
        
        <asp:SqlDataSource ID="SqlDataSource1" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT [first_name], [last_name], [addr1], [addr2], [city], [state], [zip], [country] FROM [contact]">
        </asp:SqlDataSource>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
    </div>
    </form>
</body>


</html>
