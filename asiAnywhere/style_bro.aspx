<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="style_bro" Title="Style LookUp" Codebehind="style_bro.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Style </title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js">    
</script>
<script type="text/javascript">
    function stylebox() {
        var NewWindow = window.open("style_box_design.aspx", "StyleBoxDesignWindow", "width=450,height=350,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    
</script>
<body>
    <form id="frmList" runat="server">
    
    <div>
    
     <table><tr bgcolor="gray"><td> <asp:LinkButton ID="lnk_Listcustomers" runat="server" ><img src="Images/browse style 1.jpg" border="0" alt="List Style " /></asp:LinkButton>
      <%--<asp:LinkButton ID="lnk_boxdesign" runat="server" OnClick="lnk_boxdesign_Click"  > <img src="Images/box design 0.jpg"  border="0" alt="Box Design" /></asp:LinkButton>--%>
         <img alt="" onclick="stylebox()" src="Images/box design 0.jpg" />
      <asp:LinkButton ID="lnk_3boxdesign" runat="server" OnClick="lnk_3boxdesign_Click" ><img src="Images/box 3d image 0.jpg" border="0" alt="3D Box Design" /></asp:LinkButton>
            
      </td>
      </tr></table>
      <asp:Panel ID="searchpanel" runat="server" DefaultButton="btnSearch">
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade">
  <asp:button id="btnSearch" runat="server" width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="Button2" runat="server" width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <asp:ListItem Value="ANY">ANY</asp:ListItem>
                      <asp:ListItem Value="style">Style</asp:ListItem>
                      <asp:ListItem Value="dscr">Description</asp:ListItem>  
                      <asp:ListItem Value="design_no">Design#</asp:ListItem>
                      
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                    </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                 
 </td>
  </tr>
  </table>
  </asp:Panel>
  </div>
    
    <div>
        &nbsp;&nbsp;
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
            DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True"  
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle BackColor="yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            <Columns>
            <asp:CommandField ShowSelectButton="true" ButtonType="image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
            
             <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.styleLookUp('<%#DataBinder.Eval(Container,"DataItem.style")%>','<%#DataBinder.Eval(Container,"DataItem.StyleDscr")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                 
                <asp:BoundField DataField="style" HeaderText="Style" SortExpression="style" />
                <asp:BoundField DataField="StyleDscr" HeaderText="Style Dscr" SortExpression="StyleDscr" />
                <asp:BoundField DataField="design_no" HeaderText="Design No" SortExpression="design_no" />
                <asp:BoundField DataField="BoxDscr" HeaderText="Box Dscr" SortExpression="BoxDscr" />
                
                <asp:TemplateField  Visible="false">
                <EditItemTemplate>
                        <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("box_3d_image") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <%--<asp:Label ID="Label1" runat="server" Text='<%# Bind("box_3d_image") %>'></asp:Label>--%>
                        <%--<asp:HyperLink ID="Label1" runat="server" NavigateUrl="viewimage.aspx" Text='<%# Bind("box_3d_image") %>'></asp:HyperLink>--%>
                        <asp:Label ID="Label11" runat="server" Text='<%# Bind("box_3d_image") %>' ></asp:Label> <br />
                        <asp:Label ID="Labelboxdesign" runat="server" Text='<%# Bind("box_design") %>' ></asp:Label>
                        <%--<a href="Images/RFQ item1.jpg">open</a>--%>
                        
                    </ItemTemplate>
                </asp:TemplateField>
                                             
                
            </Columns>
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="StyleLook" TypeName="LookUp">
            <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
                    
                <asp:SessionParameter SessionField="style_bro_prmaction" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:SessionParameter SessionField="style_bro_prmtext" Name="prmText" Type="String" />
                
                 
            </SelectParameters>
        </asp:ObjectDataSource>
        
    </div>
   
    </form>
</body>
</html>
