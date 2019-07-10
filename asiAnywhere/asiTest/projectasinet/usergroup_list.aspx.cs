
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class Cusergroup_list : System.Web.UI.Page 
{
  
   

    private bool bSort = true;

protected void Page_Load( object sender,  System.EventArgs e)  
{

    UserClass.CheckLogin(Page);
     UserClass UserLogin = (UserClass)Session["User"];
   

    if (! func.CheckUserPermissions("[dbo].[usergroup]", "SA") )
    {
      Response.Write("<p>" + "You don't have permissions to access this table" + "<a href=\"login.aspx\">&nbsp;" + "Back to login page" + "</a></p>");
      Response.End();
    }  

    lblMessage.Text = "";
    dbGrid_usergroup.Visible = true;

    string sCulture = ConfigurationManager.AppSettings["LCID"];
    if (!String.IsNullOrEmpty(sCulture)) 
    {
        int nCulture = int.Parse(sCulture);
        System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
    } 
       
    if (! Page.IsPostBack )
    {    

        if (Session["User"] != null) 
        {
            
            lblUser.Text = UserLogin.UserName;

            hlnkChangePwd.Visible = (UserLogin.UserName != "Guest");

        }
          
        tdSearch.Visible = tdInfo.Visible;                
       if (func.CheckUserPermissions("[dbo].[usergroup]", "s"))
      
        BuildDataSource();
    }
    dbGrid_usergroup.SelectedIndex = Convert.ToInt32(Session["user_group_index"]);
    try
    {
        if (Session["user_group_index"] == null)
        {
           dbGrid_usergroup.SelectedIndex = 0;
           Session["user_group_code"] = dbGrid_usergroup.SelectedRow.Cells[3].Text;            
        }
    }
    catch
    {
        return;
    }
    Session["Rowuser"] = UserLogin.UserName;
    try
    {
        TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        Session["size"] = Convert.ToInt32(ddl_display.Text);
        dbGrid_usergroup.PageSize = Convert.ToInt32(Session["size"]);
    }
    catch
    {
        return;
    }
    
}

private void BuildDataSource()
{
    try
    {
        string sSqlWhere = "";
        usergroupSqlDataSource.SelectParameters.Clear();
        
        if (Session["dbGrid_usergroup_SearchSQL"] != null) 
            sSqlWhere =  func.WhereBuilder(sSqlWhere, "(" + Session["dbGrid_usergroup_SearchSQL"].ToString() + ")","And");
        if (Session["dbGrid_usergroup_SearchParams"] != null) 
        {
            ParameterCollection Params = (ParameterCollection)Session["dbGrid_usergroup_SearchParams"];
            foreach (Parameter p in Params) 
            {
                txtSearchValue.Text = p.DefaultValue;
                usergroupSqlDataSource.SelectParameters.Add(p);
            }
        }               

        if (Session["dbGrid_usergroup_AdvSearch"] != null)
            sSqlWhere = func.WhereBuilder( sSqlWhere , Session["dbGrid_usergroup_AdvSearch"].ToString());
        if (Session["dbGrid_usergroup_AdvParam"] != null)
        {
            ParameterCollection Params = (ParameterCollection)Session["dbGrid_usergroup_AdvParam"];
            foreach (Parameter p in Params) usergroupSqlDataSource.SelectParameters.Add(p);
        }    

        if ( func.GetsAdvSecurityMethod("[dbo].[usergroup]") == "1" && (! func.IsAdminUser()))
        {
            string sOwnerIDField = func.GetOwnerIDField("[dbo].[usergroup]");
            string userId = ((UserClass)Session["User"]).UserID;
            
            if (sOwnerIDField != string.Empty && userId != string.Empty)
            {
                sSqlWhere = func.WhereBuilder(sSqlWhere, "[" + sOwnerIDField + "]=@" + func.BuildParameterName(sOwnerIDField));            
                usergroupSqlDataSource.SelectParameters.Add(func.BuildParameterName(sOwnerIDField), GetFieldType(sOwnerIDField), userId);
            }
            else
            {
                return;
            }  
         }

        if(sSqlWhere != string.Empty) usergroupSqlDataSource.SelectCommand = func.SqlBuilder(usergroupSqlDataSource.SelectCommand, sSqlWhere);
        
        dbGrid_usergroup.DataBind();
       
        if (Session["dbGrid_usergroup_SortExpression"] != null)
        {
            bSort = false;
            dbGrid_usergroup.Sort((string)Session["dbGrid_usergroup_SortExpression"], (SortDirection)Session["dbGrid_usergroup_SortDirection"]);
            bSort = true;
        }
        
        if (Session["dbGrid_usergroup_CurrentPageCount"] != null )
        {
            //ddlPagerCount.SelectedValue = (string)Session["dbGrid_usergroup_CurrentPageCount"];
            //dbGrid_usergroup.PageSize = Convert.ToInt32(ddlPagerCount.SelectedValue);
        } 
        else
        {
           // if (ddlPagerCount.Items.FindByValue(iDefaultRecordsPerPage.ToString()) == null) ddlPagerCount.Items.Add(iDefaultRecordsPerPage.ToString());
            //ddlPagerCount.SelectedValue = iDefaultRecordsPerPage.ToString();
           // dbGrid_usergroup.PageSize = iDefaultRecordsPerPage;
        }
        
        int iCurrentPageIndex = (Session["dbGrid_usergroup_CurrentPageIndex"] == null)?0:Convert.ToInt32(Session["dbGrid_usergroup_CurrentPageIndex"]);
        if (dbGrid_usergroup.PageCount >= iCurrentPageIndex)
            dbGrid_usergroup.PageIndex = iCurrentPageIndex;
        else 
        {
            dbGrid_usergroup.PageIndex = 0;
            Session["dbGrid_usergroup_CurrentPageIndex"] = 0;
        }
    }
    catch (Exception ex)
    {
        lblMessage.Text += "Error description" + ": " + ex.Message + "<p>";
        dbGrid_usergroup.Visible = false;
    }  
    finally
    {
/*#if(DEBUG)
{

    lblMessage.Text += "<p>SQL = " + usergroupSqlDataSource.SelectCommand  + "<p>";
    foreach (Parameter p in usergroupSqlDataSource.SelectParameters)
        lblMessage.Text += p.Name + "(" + p.Type.ToString()+")="+p.DefaultValue+"<br>";

}
#endif*/
    }
}

protected void dbGrid_usergroup_RowCreated(object sender,  GridViewRowEventArgs e)
{

    if (e.Row.RowType == DataControlRowType.Header) func.AddGlyph(dbGrid_usergroup, e.Row);

    if (e.Row.RowType == DataControlRowType.DataRow)
    { 

        bool bCheckSecurityD = func.CheckUserPermissions("[dbo].[usergroup]", "d");

        bool bCheckSecurityE = func.CheckUserPermissions("[dbo].[usergroup]", "e");

        try 
        {
            DataRowView rowData;
            rowData = (DataRowView)e.Row.DataItem;
            string  sOwnerIDField = func.GetOwnerIDField("[dbo].[usergroup]");
            if (! (sOwnerIDField == "" || (rowData == null)) ) 
            {
                string  sData = (rowData.Row[sOwnerIDField] == System.DBNull.Value)?"":Convert.ToString(rowData.Row[sOwnerIDField]);

                bCheckSecurityD = func.CheckSecurity("[dbo].[usergroup]", "d", sData);

                bCheckSecurityE = func.CheckSecurity("[dbo].[usergroup]", "e", sData);

            }
        } 
        catch
        {

                bCheckSecurityD = false;

                bCheckSecurityE = false;

        }
              
        if (!bCheckSecurityD) e.Row.Cells[0].Text = "";
                     
        //TableCell myTableCellE;
        //myTableCellE = (TableCell)e.Row.Cells[1];
        //LinkButton myEditButton;
        //myEditButton = (LinkButton)myTableCellE.Controls[0];

        //myEditButton.Visible = bCheckSecurityE;

       }

   }
 
protected void dbGrid_usergroup_RowDataBound(object sender,  GridViewRowEventArgs e)
{
    if (e.Row.RowType == DataControlRowType.DataRow)
    {
        DataRowView rowData;
        rowData = (DataRowView)e.Row.DataItem;

    }

}    

protected void dbGrid_usergroup_RowCommand(object source,  GridViewCommandEventArgs e)
{        
    if (e.CommandName == "Page") return;
    if (e.CommandName == "Sort")  return;
    
    int index = Convert.ToInt32(e.CommandArgument);
    DataKey dkKeys = dbGrid_usergroup.DataKeys[index];    
    
    string sKeysArg = "";   
    foreach (string s in dkKeys.Values.Keys)    
        sKeysArg += s + "=" + Convert.ToString(dkKeys[s])+"&";    
    if (sKeysArg == String.Empty) return;

  if ( e.CommandName == "cmdEdit" )  Response.Redirect("usergroup_Edit.aspx?" + sKeysArg);

}

protected void dbGrid_usergroup_RowDeleted(object sender, GridViewDeletedEventArgs e)
{
    lblMessage.Text = "<b>Record has been deleted!</b><br>";
}

protected void dbGrid_usergroup_PageIndexChanged(object source, EventArgs e)
{
    Session["dbGrid_usergroup_CurrentPageIndex"] = dbGrid_usergroup.PageIndex;
    BuildDataSource();

}

protected void dbGrid_usergroup_Sorted(object sender, EventArgs e)
{
    Session["dbGrid_usergroup_SortExpression"] = null;
    if (bSort) BuildDataSource();
    Session["dbGrid_usergroup_SortExpression"] = dbGrid_usergroup.SortExpression;
    Session["dbGrid_usergroup_SortDirection"] = dbGrid_usergroup.SortDirection;
}

protected void hlnkLogOut_Click(object sender,  EventArgs e) 
{

    string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
    if ( sLoginURL == "" ) 
    {
        Response.Write("<script language=javascript>alert('"+"Login page isn’t set"+"!');</script>");
        return;
    }

    Page.Session.Clear();
    Response.Redirect(sLoginURL);
}
 
protected void ddlPagerCount_SelectedIndexChanged(object sender,  EventArgs e)  
{
    //Session["dbGrid_usergroup_CurrentPageCount"] = ddlPagerCount.SelectedValue;
    Session["dbGrid_usergroup_CurrentPageIndex"] = 0;
    BuildDataSource();
}

protected void btnShowAll_Click( object sender,  System.EventArgs e) 
{
    
    ViewState["bNoRecords"] = false;
    txtSearchValue.Text = "";
    ddlSearchOperation.SelectedIndex = 0;
    ddlSearchField.SelectedIndex = 0;

    Session["dbGrid_usergroup_CurrentPageIndex"] = 0;
    Session["dbGrid_usergroup_SearchSQL"]= null;
    Session["dbGrid_usergroup_SearchParams"]= null;           
    Session["dbGrid_usergroup_AdvSearch"] = null;
    Session["dbGrid_usergroup_AdvParam"] = null;
    Session["htPeramusergroup"] = null;

    Session["htPeramusergroup"] = null;
    BuildDataSource();
}

protected void btnSearch_Click( object sender,  System.EventArgs e) 
{
  if (txtSearchValue.Text.Trim() == String.Empty && ddlSearchOperation.SelectedValue.ToString() != "IsNull") return;

  string  sSqlWhere = "";
  ParameterCollection Params = new ParameterCollection();

if (ddlSearchField.SelectedValue.ToString() == "Any Field")
  {

        sSqlWhere =  func.WhereBuilder(sSqlWhere, WhereOneField("groupID", "SearchgroupID", Params), "Or");

        sSqlWhere =  func.WhereBuilder(sSqlWhere, WhereOneField("group", "Searchgroup", Params), "Or");

  }
  else
  {
      sSqlWhere = WhereOneField(ddlSearchField.SelectedValue.ToString(), func.BuildParameterName("Search" + ddlSearchField.SelectedValue.ToString()), Params);
  }
  
  if ( sSqlWhere.Trim() != "" )
  {
    Session["dbGrid_usergroup_SearchSQL"]= sSqlWhere;
    Session["dbGrid_usergroup_SearchParams"]= Params;   
  }
  else
  {
    Session["dbGrid_usergroup_SearchSQL"] = "2<>2";
    Session["dbGrid_usergroup_SearchParams"] = null;
  }  
  Session["dbGrid_usergroup_CurrentPageIndex"] = 0;
  BuildDataSource();

}

private string WhereOneField(string  sSearchField, string sParamName, ParameterCollection Params) 
{
    string  sSearchOperation = ddlSearchOperation.SelectedValue;
    string  sValue = txtSearchValue.Text.TrimEnd();
    string  sReturn = "";
    string  sSearchType = "";
    TypeCode  fieldType = GetFieldType(sSearchField);
    
    sSearchField = "[" + sSearchField + "]";
    
    if (sSearchOperation == "IsNull") return sSearchField + " is null";
    
    try{object o = Convert.ChangeType(sValue, fieldType);}
    catch { return String.Empty; }

    if (!(func.IsDate(sValue) || func.IsNumeric(sValue))) 
    {

    sSearchType = "upper";

    sValue = sValue.ToUpper();
  }
    
  switch (sSearchOperation)
    { 
            case "Contains":
                sReturn = sSearchType + "(" + sSearchField + ") like '%" + sValue + "%'";break;
            case "Starts with ...":              
                sReturn = sSearchType + "(" + sSearchField + ") like '" + sValue + "%'";break;            
            case "Equals":
                sReturn = sSearchType + "(" + sSearchField + ") = @" + sParamName;break;
            case "More than ...":
                sReturn = sSearchField + ">@" + sParamName;break;
            case "Less than ...":
                sReturn = sSearchField + "<@" + sParamName;break;
            case "Equal or more than ...":
                sReturn = sSearchField + ">=@" + sParamName;break;
            case "Equal or less than ...":
                sReturn = sSearchField + "<=@" + sParamName;break;
            default:
            sReturn = String.Empty; break;
  }

  if (sReturn != string.Empty && (sSearchOperation != "Contains" && sSearchOperation != "Starts with ..."))
  {  
      if (func.IsNumeric(sValue)) Params.Add(sParamName, sValue);
      else Params.Add(sParamName, fieldType, sValue);
  }
  return sReturn;
}

protected TypeCode GetFieldType(string  sField) 
{
    switch (sField)
    {
  
        case "groupID": return TypeCode.Int32;
  
        case "group": return TypeCode.String;
  
        default: return TypeCode.String;
    }    
}

protected void btnDelete_Click(object sender,  EventArgs e)  
{
    string sWhere = Request.Form["chDelete"];
    if (!string.IsNullOrEmpty(sWhere))
    {
    try
    {
        foreach (string s in sWhere.Split(','))
        {
            int iRowIndex = Convert.ToInt32(s);
     
            dbGrid_usergroup.DeleteRow(iRowIndex);
        }
        
    }
    catch (Exception ex)
    {
      lblMessage.Text += "Error description" + ": " + ex.Message + "<p>";     
    }  
    }
    BuildDataSource();
}
protected void usergroupSqlDataSource_Deleting(object sender, SqlDataSourceCommandEventArgs e)
{
    
}

protected void btnAdd_Click(object sender,  EventArgs e)  
{

    Response.Redirect("usergroup_add.aspx");

}
 
protected void hlkBackToMenu_Click(object sender,  EventArgs e)
{
    string sMenuURL = ConfigurationManager.AppSettings["MenuFile"];
    if ( sMenuURL == String.Empty) 
    {
        Response.Write("<script language=javascript>alert('Menu page isn't set');</script>");
        return;
    }
        
    ClearSession();
    Response.Redirect(sMenuURL);
}

protected void ddlQuickJump_SelectedIndexChanged(object sender, EventArgs e)
{
    ClearSession();
    //Response.Redirect(ddlQuickJump.SelectedItem.Value);
}

private void ClearSession() 
{
    Session["dbGrid_usergroup_Sort"] = null;

    Session["dbGrid_usergroup_SearchSQL"]= null;
    Session["dbGrid_usergroup_SearchParams"]= null;   

    Session["dbGrid_usergroup_AdvSearch"] = null;
    Session["dbGrid_usergroup_AdvParam"] = null;
    Session["htPeramusergroup"] = null;   

    Session["htPeramusergroup"] = null;
    Session["dbGrid_usergroup_CurrentPageIndex"] = null;
    Session["dbGrid_usergroup_CurrentPageCount"] = null;

    Session["dbGrid_usergroup_SortExpression"] = null;
    Session["dbGrid_usergroup_SortDirection"] = null;
}

protected void ShowWait() 
{
    Response.Write("<div id='mydiv' align=center>&nbsp;</div>");
    Response.Write("<script>mydiv.innerText = '';</script>");
    Response.Write("<script language=javascript>;");
    Response.Write("var dots = 0;var dotmax = 10;function ShowWait()");
    Response.Write("{var output; output = '"+"Please wait"+"';dots++;if(dots>=dotmax)dots=1;");
    Response.Write("for(var x = 0;x < dots;x++){output += '.';}mydiv.innerText =  output;}");
    Response.Write("function StartShowWait(){mydiv.style.visibility = 'visible'; window.setInterval('ShowWait()',500);}");
    Response.Write("function HideWait(){mydiv.style.visibility = 'hidden';window.clearInterval();}");
    Response.Write("StartShowWait();</script>");
    Response.Flush();
}

protected void usergroupSqlDataSource_Selected(object sender, SqlDataSourceStatusEventArgs e)
{
    lblCount.Text = "Details found" +": " + e.AffectedRows.ToString();
}

    protected void dbGrid_usergroup_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["user_group_index"] = dbGrid_usergroup.SelectedIndex;
        Session["user_group_code"] = dbGrid_usergroup.SelectedRow.Cells[3].Text;
        Session["group_user"] = dbGrid_usergroup.SelectedRow.Cells[4].Text;        
    }
    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);

    }
    protected void lnk_listgroup_Click(object sender, EventArgs e)
    {
        Response.Redirect("usergroup_list.aspx");
    }
    protected void lnk_viewgroup_Click(object sender, EventArgs e)
    {
        Response.Redirect("usergroup_viewlist.aspx");
    }
    protected void lnk_groupuser_Click(object sender, EventArgs e)
    {
        Response.Redirect("group_user.aspx");
    }
}
