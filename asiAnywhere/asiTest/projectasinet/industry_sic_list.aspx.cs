
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class Cindustry_sic_list : System.Web.UI.Page
{

    private const string ASCENDING = " ASC";
    private const string DESCENDING = " DESC";


    protected void Page_Load(object sender, System.EventArgs e)
    {

        //UserClass.CheckLogin(Page);

        if (!func.CheckUserPermissions("[dbo].[industry_sic]", "SA"))
        {
            Response.Write("<p>" + "You don't have permissions to access this table" + "<a href=\"login.aspx\">&nbsp;" + "Back to login page" + "</a></p>");
            Response.End();
        }
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "industry_sic_list.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            lblComp.Text = PrmComp;
            if (aUsers == "external")
            {
                //txt_customer.Visible = false;
                //CustomerLook.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        // NewAddButton.Visible =false;



        if (dbGrid_industry_sic.SelectedIndex >= 0)
        {
            //NewAddButton.Visible = false;
        }

        lblMessage.Text = "";
        dbGrid_industry_sic.Visible = true;


        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;

            }


            BuildDataSource();
        }
        Session["Rowuser"] = UserLogin.UserName;
        dbGrid_industry_sic.SelectedIndex = Convert.ToInt32(Session["industry_sic_list_index"]);
        try
        {
            if (Session["industry_sic_list_index"] == null)
            {
                dbGrid_industry_sic.SelectedIndex = 0;
                Session["industry_sic_list_code"] = dbGrid_industry_sic.SelectedRow.Cells[1].Text;
            }
        }
        catch
        {
            return;
        }
        try
        {
            TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            dbGrid_industry_sic.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            return;
        }

    }

    private void BuildDataSource()
    {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select industry_sic_code as 'Industry Sic Code', description as 'Description' from industry_sic";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            dbGrid_industry_sic.DataSource = ds;
            dbGrid_industry_sic.DataBind();

            conn.Close();
        }
        catch
        { return; }
        finally
        {
            conn.Close();
        }
    }



    protected void dbGrid_industry_sic_RowDeleted(object sender, GridViewDeletedEventArgs e)
    {
        //lblMessage.Text = "<b>Record has been deleted!</b><br>";
    }

    protected void dbGrid_industry_sic_PageIndexChanging(object source, GridViewPageEventArgs e)
    {
        dbGrid_industry_sic.PageIndex = e.NewPageIndex;
        BuildDataSource();

    }

    protected void dbGrid_industry_sic_Sorted(object sender, EventArgs e)
    {
        //Session["dbGrid_industry_sic_SortExpression"] = null;
        //if (bSort) BuildDataSource();
        //Session["dbGrid_industry_sic_SortExpression"] = dbGrid_industry_sic.SortExpression;
        //Session["dbGrid_industry_sic_SortDirection"] = dbGrid_industry_sic.SortDirection;
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void hlkBackToMenu_Click(object sender, EventArgs e)
    {
        string sMenuURL = ConfigurationManager.AppSettings["MenuFile"];
        if (sMenuURL == String.Empty)
        {
            Response.Write("<script language=javascript>alert('Menu page isn't set');</script>");
            return;
        }

        ClearSession();
        Response.Redirect(sMenuURL);
    }
     

    protected void btnShowAll_Click(object sender, System.EventArgs e)
    {
        ViewState["bNoRecords"] = false;
        txt_industrycode.Text = "";
        txt_description.Text = "";
        //ddlSearchOperation.SelectedIndex = 0;
        //ddlSearchField.SelectedIndex = 0;

        Session["dbGrid_industry_sic_CurrentPageIndex"] = 0;
        Session["dbGrid_industry_sic_SearchSQL"] = null;
        Session["dbGrid_industry_sic_SearchParams"] = null;
        Session["dbGrid_industry_sic_AdvSearch"] = null;
        Session["dbGrid_industry_sic_AdvParam"] = null;
        Session["htPeramindustry_sic"] = null;

        Session["htPeramindustry_sic"] = null;
        Session["industry_sic_list_index"] = null;
        BuildDataSource();
    }

    protected void btnSearch_Click(object sender, System.EventArgs e)
    {

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "";
            if (txt_industrycode.Text != "")
            {
                cmd = "select industry_sic_code as 'Industry Sic Code', description as 'Description' from industry_sic where industry_sic_code LIKE '" + txt_industrycode.Text + "%'";
            }
            if (txt_description.Text != "")
            {
                cmd = "select industry_sic_code as 'Industry Sic Code', description as 'Description' from industry_sic where description LIKE '" + txt_description.Text + "%'";
            }

            if (txt_description.Text != "" && txt_industrycode.Text != "")
            {
                cmd = "select industry_sic_code as 'Industry Sic Code', description as 'Description' from industry_sic where description LIKE '" + txt_description.Text + "%' and industry_sic_code LIKE '" + txt_industrycode.Text + "%'";
            }

            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            dbGrid_industry_sic.DataSource = ds;
            dbGrid_industry_sic.DataBind();

            conn.Close();
        }
        catch
        { return; }
        finally
        {
            dbGrid_industry_sic.PageSize = Convert.ToInt32(Session["size"]);
            conn.Close();
        }



    }



    protected void dbGrid_contact_RowCommand(object source, GridViewCommandEventArgs e)
    {

    }




    private void ClearSession()
    {
        Session["dbGrid_industry_sic_Sort"] = null;

        Session["dbGrid_industry_sic_SearchSQL"] = null;
        Session["dbGrid_industry_sic_SearchParams"] = null;

        Session["htPeramindustry_sic"] = null;
        Session["dbGrid_industry_sic_CurrentPageIndex"] = null;
        Session["dbGrid_industry_sic_CurrentPageCount"] = null;

        Session["dbGrid_industry_sic_SortExpression"] = null;
        Session["dbGrid_industry_sic_SortDirection"] = null;
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


    protected void dbGrid_industry_sic_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["industry_sic_list_index"] = dbGrid_industry_sic.SelectedIndex;
        //Session["industry_sic_list_code"] = Session["industry_sic_list_code"];
        Session["industry_sic_list_code"] = dbGrid_industry_sic.SelectedRow.Cells[1].Text;
        //Response.Write(Session["industry_sic_list_code"]);
        //Response.Write(Session["industry_sic_list_code"]);
    }



    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);
        BuildDataSource();
    }

    protected void lnk_listsic_Click(object sender, EventArgs e)
    {
        Response.Redirect("industry_sic_list.aspx");
    }
    protected void lnk_viewsic_Click(object sender, EventArgs e)
    {
        Response.Redirect("industry_sic_listview.aspx");
    }


    public SortDirection GridViewSortDirection
    {

        get
        {

            if (ViewState["sortDirection"] == null)

                ViewState["sortDirection"] = SortDirection.Ascending;

            return (SortDirection)ViewState["sortDirection"];

        }

        set { ViewState["sortDirection"] = value; }

    }
    protected void GridView1_Sorting(object sender, GridViewSortEventArgs e)
    {

        string sortExpression = e.SortExpression;

        if (GridViewSortDirection == SortDirection.Ascending)
        {

            GridViewSortDirection = SortDirection.Descending;

            SortGridView(sortExpression, " DESC");

        }

        else
        {

            GridViewSortDirection = SortDirection.Ascending;

            SortGridView(sortExpression, " ASC");

        }

    }

    private void SortGridView(string sortExpression, string direction)
    {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select industry_sic_code as 'Industry Sic Code', description as 'Description' from industry_sic";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            dbGrid_industry_sic.DataSource = ds;
            dbGrid_industry_sic.DataBind();
            DataTable dt = ds.Tables[0];

            DataView dv = new DataView(dt);

            dv.Sort = sortExpression + direction;

            dbGrid_industry_sic.DataSource = dv;

            dbGrid_industry_sic.DataBind();


            conn.Close();

        }



        catch { return; }

    }


}
