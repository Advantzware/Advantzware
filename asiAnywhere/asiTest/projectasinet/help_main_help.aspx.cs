
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

public partial class help_main_help : System.Web.UI.Page
{

    private const string ASCENDING = " ASC";
    private const string DESCENDING = " DESC";


    protected void Page_Load(object sender, System.EventArgs e)
    {

        //UserClass.CheckLogin(Page);

        //if (!func.CheckUserPermissions("[dbo].[industry_sic]", "SA"))
        //{
        //    Response.Write("<p>" + "You don't have permissions to access this table" + "<a href=\"login.aspx\">&nbsp;" + "Back to login page" + "</a></p>");
        //    Response.End();
        //}
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Session["Rowuser"] = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "help_main.aspx";
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



        if (dbGrid_help_main.SelectedIndex >= 0)
        {
            //NewAddButton.Visible = false;
        }

        lblMessage.Text = "";
        dbGrid_help_main.Visible = true;


        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;
                txt_fldname.Text = Convert.ToString(Session["help_main_list_index_help_field"]);
                txt_title.Text = Convert.ToString(Session["help_main_list_index_help_title"]);
                try
                {
                    TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
                    Session["size"] = Convert.ToInt32(ddl_display.Text);
                    dbGrid_help_main.PageSize = Convert.ToInt32(Session["size"]);
                }
                catch
                {
                    return;
                }
            }
                                    
        }
        
        dbGrid_help_main.SelectedIndex = Convert.ToInt32(Session["help_main_list_index_help"]);
        try
        {
            if (Session["help_main_list_index_help"] == null)
            {
                dbGrid_help_main.SelectedIndex = 0;
                Session["help_main_list_code_help"] = dbGrid_help_main.SelectedRow.Cells[1].Text;
            }
        }
        catch
        {
            return;
        }
        try
        {
            TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            dbGrid_help_main.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            return;
        }
        
       

    }
    protected void dbGrid_help_main_PreRender(object sender, EventArgs e)
    {
        BuildDataSource();
    }

    private void BuildDataSource()
    {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "";
            cmd = "select msg_num as 'Message#', fld_name as 'Field Name', frm_title as 'Title', fil_name as 'Table', frm_name as 'Frame Name', substring(help_txt,1,80) as 'Contents' from hlp_head";
            if (txt_fldname.Text != "")
            {
                cmd = "select msg_num as 'Message#', fld_name as 'Field Name', frm_title as 'Title', fil_name as 'Table', frm_name as 'Frame Name', substring(help_txt,1,80) as 'Contents' from hlp_head where fld_name LIKE '" + txt_fldname.Text + "%'";

            }
            if (txt_title.Text != "")
            {
                cmd = "select msg_num as 'Message#', fld_name as 'Field Name', frm_title as 'Title', fil_name as 'Table', frm_name as 'Frame Name', substring(help_txt,1,80) as 'Contents' from hlp_head where frm_title LIKE '" + txt_title.Text + "%'";

            }
            if (txt_fldname.Text != "" && txt_title.Text != "")
            {
                cmd = "select msg_num as 'Message#', fld_name as 'Field Name', frm_title as 'Title', fil_name as 'Table', frm_name as 'Frame Name', substring(help_txt,1,80) as 'Contents' from hlp_head where fld_name LIKE '" + txt_fldname.Text + "%' and frm_title LIKE '" + txt_title.Text + "%'";

            }
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            dbGrid_help_main.DataSource = ds;
            dbGrid_help_main.DataBind();

            conn.Close();
            //dbGrid_help_main.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        { return; }
        finally
        {
         conn.Close();
        }
        

    }



    protected void dbGrid_help_main_RowDeleted(object sender, GridViewDeletedEventArgs e)
    {
        //lblMessage.Text = "<b>Record has been deleted!</b><br>";
    }

    protected void dbGrid_help_main_PageIndexChanging(object source, GridViewPageEventArgs e)
    {
        dbGrid_help_main.PageIndex = e.NewPageIndex;
      

    }

    protected void dbGrid_help_main_Sorted(object sender, EventArgs e)
    {
        //Session["dbGrid_help_main_SortExpression"] = null;
        //if (bSort) BuildDataSource();
        //Session["dbGrid_help_main_SortExpression"] = dbGrid_help_main.SortExpression;
        //Session["dbGrid_help_main_SortDirection"] = dbGrid_help_main.SortDirection;
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
        txt_fldname.Text = "";
        txt_title.Text="";
        
        Session["dbGrid_help_main_CurrentPageIndex"] = 0;
        Session["dbGrid_help_main_SearchSQL"] = null;
        Session["dbGrid_help_main_SearchParams"] = null;
        Session["dbGrid_help_main_AdvSearch"] = null;
        Session["dbGrid_help_main_AdvParam"] = null;
        Session["htPeramhelp_main"] = null;

        Session["help_main_list_index_help_field"] = null;
        Session["help_main_list_index_help_title"] = null;
        Session["help_main_list_index_help"] = null;
        
    }

    protected void btnSearch_Click(object sender, System.EventArgs e)
    {
        Session["help_main_list_index_help_field"] = txt_fldname.Text.Trim();
        Session["help_main_list_index_help_title"] = txt_title.Text.Trim();
        //SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        //try
        //{
        //    conn.Open();
        //    string cmd = "";
            
        //    if (txt_fldname.Text != "")
        //    {
        //        cmd = "select msg_num as 'Message#', fld_name as 'Field Name', frm_title as 'Title', fil_name as 'Table', frm_name as 'Frame Name', substring(help_txt,1,80) as 'Contents' from hlp_head where fld_name LIKE '" + txt_fldname.Text + "%'";
                
        //    }
        //    if (txt_title.Text != "")
        //    {
        //        cmd = "select msg_num as 'Message#', fld_name as 'Field Name', frm_title as 'Title', fil_name as 'Table', frm_name as 'Frame Name', substring(help_txt,1,80) as 'Contents' from hlp_head where frm_title LIKE '" + txt_title.Text + "%'";

        //    }
        //    if (txt_fldname.Text != "" && txt_title.Text != "")
        //    {
        //        cmd = "select msg_num as 'Message#', fld_name as 'Field Name', frm_title as 'Title', fil_name as 'Table', frm_name as 'Frame Name', substring(help_txt,1,80) as 'Contents' from hlp_head where fld_name LIKE '" + txt_fldname.Text + "%' and frm_title LIKE '" + txt_title.Text + "%'";

        //    }
            
        //    SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
        //    DataSet ds = new DataSet();
        //    da.Fill(ds);

        //    dbGrid_help_main.DataSource = ds;
        //    dbGrid_help_main.DataBind();

        //    conn.Close();
        //}
        //catch
        //{ return; }
        //finally
        //{
        //    dbGrid_help_main.PageSize = Convert.ToInt32(Session["size"]);
        //    conn.Close();
        //}



    }



    protected void dbGrid_contact_RowCommand(object source, GridViewCommandEventArgs e)
    {

    }




    private void ClearSession()
    {
        Session["dbGrid_help_main_Sort"] = null;

        Session["dbGrid_help_main_SearchSQL"] = null;
        Session["dbGrid_help_main_SearchParams"] = null;

        Session["htPeramhelp_main"] = null;
        Session["dbGrid_help_main_CurrentPageIndex"] = null;
        Session["dbGrid_help_main_CurrentPageCount"] = null;

        Session["dbGrid_help_main_SortExpression"] = null;
        Session["dbGrid_help_main_SortDirection"] = null;
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


    protected void dbGrid_help_main_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["help_main_list_index_help"] = dbGrid_help_main.SelectedIndex;
        //Session["help_main_list_code"] = Session["help_main_list_code"];
        Session["help_main_list_code_help"] = dbGrid_help_main.SelectedRow.Cells[1].Text;
        
    }



    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);
       
    }

    protected void lnk_listsic_Click(object sender, EventArgs e)
    {
        Response.Redirect("help_main_help.aspx");
    }
    protected void lnk_viewsic_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_main_help.aspx");
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
            string cmd = "";
            //string cmd = "select msg_num as 'Message#', fld_name as 'Field Name', frm_title as 'Title', fil_name as 'Table', frm_name as 'Frame Name', substring(help_txt,1,80) as 'Contents' from hlp_head";
            if (txt_fldname.Text != "")
            {
                cmd = "select msg_num as 'Message#', fld_name as 'Field Name', frm_title as 'Title', fil_name as 'Table', frm_name as 'Frame Name', substring(help_txt,1,80) as 'Contents' from hlp_head where fld_name LIKE '" + txt_fldname.Text + "%'";

            }
            if (txt_title.Text != "")
            {
                cmd = "select msg_num as 'Message#', fld_name as 'Field Name', frm_title as 'Title', fil_name as 'Table', frm_name as 'Frame Name', substring(help_txt,1,80) as 'Contents' from hlp_head where frm_title LIKE '" + txt_title.Text + "%'";

            }
            if (txt_fldname.Text != "" && txt_title.Text != "")
            {
                cmd = "select msg_num as 'Message#', fld_name as 'Field Name', frm_title as 'Title', fil_name as 'Table', frm_name as 'Frame Name', substring(help_txt,1,80) as 'Contents' from hlp_head where fld_name LIKE '" + txt_fldname.Text + "%' and frm_title LIKE '" + txt_title.Text + "%'";

            }
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            dbGrid_help_main.DataSource = ds;
            dbGrid_help_main.DataBind();
            DataTable dt = ds.Tables[0];

            DataView dv = new DataView(dt);

            dv.Sort = sortExpression + direction;

            dbGrid_help_main.DataSource = dv;

            dbGrid_help_main.DataBind();


            conn.Close();

        }



        catch { return; }

    }


}
