
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class user_groupidlookup : System.Web.UI.Page
{

    

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        

        //lblMessage.Text = "";
        dbGrid_user_master.Visible = true;

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                UserClass UserLogin = (UserClass)Session["User"];
                //lblUser.Text = UserLogin.UserName;

                

            }

            

            //tdInfo.Visible = tdPageCount.Visible = func.CheckUserPermissions("[dbo].[user_master]", "s");

            //tdSearch.Visible = tdInfo.Visible;
            

            if (func.CheckUserPermissions("[dbo].[user_master]", "s"))

                BuildDataSource();
        }
        dbGrid_user_master.SelectedIndex = Convert.ToInt32(Session["user_master_index"]);
        try
        {
            if (Session["user_master_index"] == null)
            {
                dbGrid_user_master.SelectedIndex = 0;
                Session["user_master_code"] = dbGrid_user_master.SelectedRow.Cells[3].Text;
            }
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
            user_masterSqlDataSource.SelectParameters.Clear();

            if (Session["dbGrid_user_master_SearchSQL"] != null)
                sSqlWhere = func.WhereBuilder(sSqlWhere, "(" + Session["dbGrid_user_master_SearchSQL"].ToString() + ")", "And");
            if (Session["dbGrid_user_master_SearchParams"] != null)
            {
                ParameterCollection Params = (ParameterCollection)Session["dbGrid_user_master_SearchParams"];
                foreach (Parameter p in Params)
                {
                    txtSearchValue.Text = p.DefaultValue;
                    user_masterSqlDataSource.SelectParameters.Add(p);
                }
            }

            if (Session["dbGrid_user_master_AdvSearch"] != null)
                sSqlWhere = func.WhereBuilder(sSqlWhere, Session["dbGrid_user_master_AdvSearch"].ToString());
            if (Session["dbGrid_user_master_AdvParam"] != null)
            {
                ParameterCollection Params = (ParameterCollection)Session["dbGrid_user_master_AdvParam"];
                foreach (Parameter p in Params) user_masterSqlDataSource.SelectParameters.Add(p);
            }

            if (func.GetsAdvSecurityMethod("[dbo].[usergroup]") == "1" && (!func.IsAdminUser()))
            {
                string sOwnerIDField = func.GetOwnerIDField("[dbo].[usergroup]");
                string userId = ((UserClass)Session["User"]).UserID;

                if (sOwnerIDField != string.Empty && userId != string.Empty)
                {
                    sSqlWhere = func.WhereBuilder(sSqlWhere, "[" + sOwnerIDField + "]=@" + func.BuildParameterName(sOwnerIDField));
                    user_masterSqlDataSource.SelectParameters.Add(func.BuildParameterName(sOwnerIDField), GetFieldType(sOwnerIDField), userId);
                }
                else
                {
                    return;
                }
            }

            if (sSqlWhere != string.Empty) user_masterSqlDataSource.SelectCommand = func.SqlBuilder(user_masterSqlDataSource.SelectCommand, sSqlWhere);

            dbGrid_user_master.DataBind();

            if (Session["dbGrid_user_master_SortExpression"] != null)
            {
                bSort = false;
                dbGrid_user_master.Sort((string)Session["dbGrid_user_master_SortExpression"], (SortDirection)Session["dbGrid_user_master_SortDirection"]);
                bSort = true;
            }

            if (Session["dbGrid_user_master_CurrentPageCount"] != null)
            {
                // ddlPagerCount.SelectedValue = (string)Session["dbGrid_user_master_CurrentPageCount"];
                //dbGrid_user_master.PageSize = Convert.ToInt32(ddlPagerCount.SelectedValue);
            }
            else
            {
                //if (ddlPagerCount.Items.FindByValue(iDefaultRecordsPerPage.ToString()) == null) ddlPagerCount.Items.Add(iDefaultRecordsPerPage.ToString());
                //ddlPagerCount.SelectedValue = iDefaultRecordsPerPage.ToString();
                //dbGrid_user_master.PageSize = iDefaultRecordsPerPage;
            }

            int iCurrentPageIndex = (Session["dbGrid_user_master_CurrentPageIndex"] == null) ? 0 : Convert.ToInt32(Session["dbGrid_user_master_CurrentPageIndex"]);
            if (dbGrid_user_master.PageCount >= iCurrentPageIndex)
                dbGrid_user_master.PageIndex = iCurrentPageIndex;
            else
            {
                dbGrid_user_master.PageIndex = 0;
                Session["dbGrid_user_master_CurrentPageIndex"] = 0;
            }
        }
        catch (Exception ex)
        {
            //lblMessage.Text += "Error description" + ": " + ex.Message + "<p>";
            dbGrid_user_master.Visible = false;
        }
        finally
        {
/*#if(DEBUG)
{

    //lblMessage.Text += "<p>SQL = " + user_masterSqlDataSource.SelectCommand  + "<p>";
    foreach (Parameter p in user_masterSqlDataSource.SelectParameters)
        //lblMessage.Text += p.Name + "(" + p.Type.ToString()+")="+p.DefaultValue+"<br>";

}
#endif*/
        }
    }

    protected void dbGrid_user_master_RowCreated(object sender, GridViewRowEventArgs e)
    {

        if (e.Row.RowType == DataControlRowType.Header) func.AddGlyph(dbGrid_user_master, e.Row);

        if (e.Row.RowType == DataControlRowType.DataRow)
        {

            bool bCheckSecurityD = func.CheckUserPermissions("[dbo].[usergroup]", "d");

            bool bCheckSecurityE = func.CheckUserPermissions("[dbo].[usergroup]", "e");

            try
            {
                DataRowView rowData;
                rowData = (DataRowView)e.Row.DataItem;
                string sOwnerIDField = func.GetOwnerIDField("[dbo].[usergroup]");
                if (!(sOwnerIDField == "" || (rowData == null)))
                {
                    string sData = (rowData.Row[sOwnerIDField] == System.DBNull.Value) ? "" : Convert.ToString(rowData.Row[sOwnerIDField]);

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


        }

    }

    protected void dbGrid_user_master_RowDataBound(object sender, GridViewRowEventArgs e)
    {
        if (e.Row.RowType == DataControlRowType.DataRow)
        {
            DataRowView rowData;
            rowData = (DataRowView)e.Row.DataItem;

        }

    }

    protected void dbGrid_user_master_RowCommand(object source, GridViewCommandEventArgs e)
    {
        if (e.CommandName == "Page") return;
        if (e.CommandName == "Sort") return;

        int index = Convert.ToInt32(e.CommandArgument);
        DataKey dkKeys = dbGrid_user_master.DataKeys[index];

        string sKeysArg = "";
        foreach (string s in dkKeys.Values.Keys)
            sKeysArg += s + "=" + Convert.ToString(dkKeys[s]) + "&";
        if (sKeysArg == String.Empty) return;

        if (e.CommandName == "cmdEdit") Response.Redirect("user_master_Edit.aspx?" + sKeysArg);

    }

    protected void dbGrid_user_master_RowDeleted(object sender, GridViewDeletedEventArgs e)
    {
        //lblMessage.Text = "<b>Record has been deleted!</b><br>";
    }

    protected void dbGrid_user_master_PageIndexChanged(object source, EventArgs e)
    {
        Session["dbGrid_user_master_CurrentPageIndex"] = dbGrid_user_master.PageIndex;
        BuildDataSource();

    }

    protected void dbGrid_user_master_Sorted(object sender, EventArgs e)
    {
        Session["dbGrid_user_master_SortExpression"] = null;
        if (bSort) BuildDataSource();
        Session["dbGrid_user_master_SortExpression"] = dbGrid_user_master.SortExpression;
        Session["dbGrid_user_master_SortDirection"] = dbGrid_user_master.SortDirection;
    }

   

    protected void ddlPagerCount_SelectedIndexChanged(object sender, EventArgs e)
    {
        //Session["dbGrid_user_master_CurrentPageCount"] = ddlPagerCount.SelectedValue;
        Session["dbGrid_user_master_CurrentPageIndex"] = 0;
        BuildDataSource();
    }

    protected void btnShowAll_Click(object sender, System.EventArgs e)
    {
        ViewState["bNoRecords"] = false;
        txtSearchValue.Text = "";
        ddlSearchOperation.SelectedIndex = 0;
        ddlSearchField.SelectedIndex = 0;

        Session["dbGrid_user_master_CurrentPageIndex"] = 0;
        Session["dbGrid_user_master_SearchSQL"] = null;
        Session["dbGrid_user_master_SearchParams"] = null;
        Session["dbGrid_user_master_AdvSearch"] = null;
        Session["dbGrid_user_master_AdvParam"] = null;
        Session["htPeramuser_master"] = null;

        Session["htPeramuser_master"] = null;
        BuildDataSource();
    }

    protected void btnSearch_Click(object sender, System.EventArgs e)
    {
        if (txtSearchValue.Text.Trim() == String.Empty && ddlSearchOperation.SelectedValue.ToString() != "IsNull") return;

        string sSqlWhere = "";
        ParameterCollection Params = new ParameterCollection();

        if (ddlSearchField.SelectedValue.ToString() == "Any Field")
        {

            sSqlWhere = func.WhereBuilder(sSqlWhere, WhereOneField("groupID", "SearchUserID", Params), "Or");

            sSqlWhere = func.WhereBuilder(sSqlWhere, WhereOneField("group", "Searchgroup", Params), "Or");

            

        }
        else
        {
            sSqlWhere = WhereOneField(ddlSearchField.SelectedValue.ToString(), func.BuildParameterName("Search" + ddlSearchField.SelectedValue.ToString()), Params);
        }

        if (sSqlWhere.Trim() != "")
        {
            Session["dbGrid_user_master_SearchSQL"] = sSqlWhere;
            Session["dbGrid_user_master_SearchParams"] = Params;
        }
        else
        {
            Session["dbGrid_user_master_SearchSQL"] = "2<>2";
            Session["dbGrid_user_master_SearchParams"] = null;
        }
        Session["dbGrid_user_master_CurrentPageIndex"] = 0;
        BuildDataSource();

    }

    private string WhereOneField(string sSearchField, string sParamName, ParameterCollection Params)
    {
        string sSearchOperation = ddlSearchOperation.SelectedValue;
        string sValue = txtSearchValue.Text.TrimEnd();
        string sReturn = "";
        string sSearchType = "";
        TypeCode fieldType = GetFieldType(sSearchField);

        sSearchField = "[" + sSearchField + "]";

        if (sSearchOperation == "IsNull") return sSearchField + " is null";

        try { object o = Convert.ChangeType(sValue, fieldType); }
        catch { return String.Empty; }

        if (!(func.IsDate(sValue) || func.IsNumeric(sValue)))
        {

            sSearchType = "upper";

            sValue = sValue.ToUpper();
        }

        switch (sSearchOperation)
        {
            case "Contains":
                sReturn = sSearchType + "(" + sSearchField + ") like '%" + sValue + "%'"; break;
            case "Starts with ...":
                sReturn = sSearchType + "(" + sSearchField + ") like '" + sValue + "%'"; break;
            case "Equals":
                sReturn = sSearchType + "(" + sSearchField + ") = @" + sParamName; break;
            case "More than ...":
                sReturn = sSearchField + ">@" + sParamName; break;
            case "Less than ...":
                sReturn = sSearchField + "<@" + sParamName; break;
            case "Equal or more than ...":
                sReturn = sSearchField + ">=@" + sParamName; break;
            case "Equal or less than ...":
                sReturn = sSearchField + "<=@" + sParamName; break;
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

    protected TypeCode GetFieldType(string sField)
    {
        switch (sField)
        {

            case "groupID": return TypeCode.Int32;

            case "group": return TypeCode.String;

            

            default: return TypeCode.String;
        }
    }

    
    protected void user_masterSqlDataSource_Deleting(object sender, SqlDataSourceCommandEventArgs e)
    {

    }

    
    private void ClearSession()
    {
        Session["dbGrid_user_master_Sort"] = null;

        Session["dbGrid_user_master_SearchSQL"] = null;
        Session["dbGrid_user_master_SearchParams"] = null;

        Session["dbGrid_user_master_AdvSearch"] = null;
        Session["dbGrid_user_master_AdvParam"] = null;
        Session["htPeramuser_master"] = null;

        Session["htPeramuser_master"] = null;
        Session["dbGrid_user_master_CurrentPageIndex"] = null;
        Session["dbGrid_user_master_CurrentPageCount"] = null;

        Session["dbGrid_user_master_SortExpression"] = null;
        Session["dbGrid_user_master_SortDirection"] = null;
    }

    protected void ShowWait()
    {
        Response.Write("<div id='mydiv' align=center>&nbsp;</div>");
        Response.Write("<script>mydiv.innerText = '';</script>");
        Response.Write("<script language=javascript>;");
        Response.Write("var dots = 0;var dotmax = 10;function ShowWait()");
        Response.Write("{var output; output = '" + "Please wait" + "';dots++;if(dots>=dotmax)dots=1;");
        Response.Write("for(var x = 0;x < dots;x++){output += '.';}mydiv.innerText =  output;}");
        Response.Write("function StartShowWait(){mydiv.style.visibility = 'visible'; window.setInterval('ShowWait()',500);}");
        Response.Write("function HideWait(){mydiv.style.visibility = 'hidden';window.clearInterval();}");
        Response.Write("StartShowWait();</script>");
        Response.Flush();
    }

    
    protected void dbGrid_user_master_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["user_master_index"] = dbGrid_user_master.SelectedIndex;
        Session["user_master_code"] = dbGrid_user_master.SelectedRow.Cells[2].Text;
        Session["user_master_id"] = dbGrid_user_master.SelectedRow.Cells[2].Text;
        Session["user_master_name"] = dbGrid_user_master.SelectedRow.Cells[3].Text;
        Session["user_master_email"] = dbGrid_user_master.SelectedRow.Cells[4].Text;
        Session["user_master_groupid"] = dbGrid_user_master.SelectedRow.Cells[5].Text;

    }
   
}




