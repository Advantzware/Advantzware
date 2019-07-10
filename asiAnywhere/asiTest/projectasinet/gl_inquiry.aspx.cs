
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

public partial class gl_inquiry : System.Web.UI.Page
{

   protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (!Page.IsPostBack)
        {


            if (actno_TextBox.Text == "")
                actno_TextBox.Text = Convert.ToString(Session["gl_inquiry_account"]);
            if (year_TextBox.Text == "")
                year_TextBox.Text = Convert.ToString(Session["gl_inquiry_year"]);
            if (opbal_TextBox.Text == "")
                opbal_TextBox.Text = Convert.ToString(Session["gl_inquiry_opbal"]);
            if (clbal_TextBox.Text == "")
                clbal_TextBox.Text = Convert.ToString(Session["gl_inquiry_clsbal"]);
            if (bepdrn_TextBox.Text == "")
                bepdrn_TextBox.Text = Convert.ToString(Session["gl_inquiry_frmprd"]);
            if (endpdrn_TextBox.Text == "")
                endpdrn_TextBox.Text = Convert.ToString(Session["gl_inquiry_toprd"]);

            if (year_TextBox.Text == "")
                year_TextBox.Text = "0";
            if (opbal_TextBox.Text == "")
                opbal_TextBox.Text = "0.00";
            if (clbal_TextBox.Text == "")
                clbal_TextBox.Text = "0.00";
            if (bepdrn_TextBox.Text == "")
                bepdrn_TextBox.Text = "0";
            if (endpdrn_TextBox.Text == "")
            {
                endpdrn_TextBox.Text = "12";
                Session["gl_inquiry_toprd"] = endpdrn_TextBox.Text;

            }



            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;
            }
        }
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "gl_inquiry.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;

            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }




        try
        {
            if (Session["gl_inquiry_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["top_gl_inquiry_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reckeyLabel")).Text;
                clbal_TextBox.Text = ((Label)GridView1.SelectedRow.FindControl("extraLabel")).Text;
                Session["top_attach_gl_inquiry"] = GridView1.SelectedRow.Cells[1].Text;
                Session["top_list_notes_rec_key"] = "";
            }
        }
        catch
        {
            
        }
        try
        {
            if (Session["gl_inquiry_index"] != null)
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["apinv_bal_index"]);
                Session["top_gl_inquiry_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reckeyLabel")).Text;
                clbal_TextBox.Text = ((Label)GridView1.SelectedRow.FindControl("extraLabel")).Text;
                Session["top_attach_gl_inquiry"] = GridView1.SelectedRow.Cells[1].Text;
                Session["top_list_notes_rec_key"] = "";

            }
        }
        catch
        {

        }
        Session["Rowuser"] = UserLogin.UserName;

        try
        {
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            
        }
               


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

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }



    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);

    }


    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["gl_inquiry_index"] = GridView1.SelectedIndex;
        /*Session["top_gl_inquiry_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reckeyLabel")).Text;*/
        Session["top_gl_inquiry_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reckeyLabel")).Text;        
        clbal_TextBox.Text = ((Label)GridView1.SelectedRow.FindControl("extraLabel")).Text;
        Response.Write(((Label)GridView1.SelectedRow.FindControl("extraLabel")).Text);
        Session["top_attach_gl_inquiry"] = GridView1.SelectedRow.Cells[1].Text;
        Session["top_list_notes_rec_key"] = "";
        
    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
               
       
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmbegact"].DefaultValue = actno_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmglyear"].DefaultValue = year_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmperiodfr"].DefaultValue = bepdrn_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmperiodto"].DefaultValue = endpdrn_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmopnbal"].DefaultValue = opbal_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmclsbal"].DefaultValue = clbal_TextBox.Text.Trim();
        
        Session["gl_inquiry_account"] = actno_TextBox.Text.Trim();
        Session["gl_inquiry_year"] = year_TextBox.Text.Trim();
        Session["gl_inquiry_frmprd"] = bepdrn_TextBox.Text.Trim();
        Session["gl_inquiry_toprd"] = endpdrn_TextBox.Text.Trim();
        Session["gl_inquiry_opbal"] = opbal_TextBox.Text.Trim();
        Session["gl_inquiry_clsbal"] = clbal_TextBox.Text.Trim();
        
        Session["gl_inquiry_index"] = null;
        Response.Write("<script>window.location.href='gl_inquiry.aspx'</script>");
    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        actno_TextBox.Text = "";
        year_TextBox.Text = "";              
        clbal_TextBox.Text = "";


        Session["gl_inq_list"] = actno_TextBox.Text;
        //Session["disbur_list_checkno"] = checkno_TextBox.Text.Trim();
        

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Showall";

        ObjectDataSource1.SelectParameters["prmbegact"].DefaultValue = actno_TextBox.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmcheckno"].DefaultValue = checkno_TextBox.Text.Trim();
        Session["gl_inquiry_account"] = actno_TextBox.Text.Trim();
        Session["gl_inquiry_year"] = year_TextBox.Text.Trim();
        Session["gl_inquiry_frmprd"] = bepdrn_TextBox.Text.Trim();
        Session["gl_inquiry_toprd"] = endpdrn_TextBox.Text.Trim();
        Session["gl_inquiry_opbal"] = opbal_TextBox.Text.Trim();
        Session["gl_inquiry_clsbal"] = clbal_TextBox.Text.Trim();
        Session["gl_inquiry_index"] = null;
    }

    protected void lnk_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_gl_inquiry.aspx");
    }
    protected void load_viewjournals_Click(object sender, EventArgs e)
    {
        Response.Redirect("load_recr_journal.aspx");
    }
    

     protected void img_btn_exit_click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }

     protected void print_Click(object sender, EventArgs e)
     {
         UserClass UserLogin = (UserClass)Session["User"];

         ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
         ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "PrintReport";
         ObjectDataSource3.SelectParameters["prmbegact"].DefaultValue = actno_TextBox.Text.Trim();
         ObjectDataSource3.SelectParameters["prmglyear"].DefaultValue = year_TextBox.Text.Trim();
         ObjectDataSource3.SelectParameters["prmperiodfr"].DefaultValue = bepdrn_TextBox.Text.Trim();
         ObjectDataSource3.SelectParameters["prmperiodto"].DefaultValue = endpdrn_TextBox.Text.Trim();
         ObjectDataSource3.SelectParameters["prmopnbal"].DefaultValue = opbal_TextBox.Text.Trim();
         ObjectDataSource3.SelectParameters["prmclsbal"].DefaultValue = clbal_TextBox.Text.Trim();
         



         



         Label vpath = (Label)FormView2.FindControl("extraLabel");

         try
         {
             if (vpath.Text != "")
             {
                 string path = vpath.Text;
                 string path2 = @"/pdfs/" + path;
                 Session["open_tranorder_list"] = path2;
                 if (path2 != "")
                 {
                     if (!Request.Browser.Browser.Contains("Safari"))
                         Response.Write("<script>window.open('print_tranorder_list.aspx'); target='_blank'</script>");

                 }
             }


         }
         catch { }
     }

     protected void FormView2_PreRender(object sender, EventArgs e)
     {
         
     }

    
    

}
